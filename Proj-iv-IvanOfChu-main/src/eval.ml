open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
  | Int int -> Int int
  | Bool bool -> Bool bool
  | String string -> String string
  | ID id -> lookup env (id)
  | Not x -> 
    (let find_not = eval_expr env x in 
    match find_not with 
    | Bool bool -> Bool (not bool)
    | _ -> raise (TypeError "Not Error"))
  | Binop (op, e1, e2) ->
    (let eval_e1 = eval_expr env e1 in 
    let eval_e2 = eval_expr env e2 in 
    match (op, eval_e1, eval_e2) with 
    | Add, Int x, Int y -> Int (x + y)
    | Add, _, _ -> raise (TypeError "Add")
    | Sub, Int x, Int y -> Int (x - y)
    | Sub, _, _ -> raise (TypeError "Add")
    | Mult, Int x, Int y -> Int (x * y)
    | Mult, _, _ -> raise (TypeError "Add")
    | Div, Int x, Int 0 -> raise (DivByZeroError)
    | Div, Int x, Int y -> Int (x / y)
    | Div, _, _ -> raise (TypeError "Add")
    | Greater, Int x, Int y -> Bool (x > y)
    | Greater, _, _ -> raise (TypeError "Greater")
    | Less, Int x, Int y -> Bool (x < y)
    | Less, _, _ -> raise (TypeError "Greater")
    | GreaterEqual, Int x, Int y -> Bool (x >= y)
    | GreaterEqual, _, _ -> raise (TypeError "Greater")
    | LessEqual, Int x, Int y -> Bool (x >= y)
    | LessEqual, _, _ -> raise (TypeError "Greater")
    | Concat, String x, String y -> String (x ^ y) 
    | Concat, _, _ -> raise (TypeError "Concat")
    | Equal, Int x, Int y -> Bool (x = y)
    | Equal, Bool x, Bool y -> Bool (x = y)
    | Equal, String x, String y -> Bool (x = y)
    | Equal, _, _ -> raise (TypeError "Equal")
    | NotEqual, Int x, Int y -> Bool (x <> y)
    | NotEqual, Bool x, Bool y -> Bool (x <> y)
    | NotEqual, String x, String y -> Bool (x <> y)
    | NotEqual, _, _ -> raise (TypeError "Equal")
    | Or, Bool x, Bool y -> Bool (x || y)
    | And, Bool x, Bool y -> Bool (x && y)
    | Or, _ , _ -> raise (TypeError "Or")
    | And, _ , _ -> raise (TypeError "Or"))
  | If (w, t, f) -> 
    let guard = eval_expr env w in 
    (match guard with 
    | Bool bool -> 
      if (bool) then 
        let t_branch = eval_expr env t in 
      t_branch else
        let f_branch = eval_expr env f in 
        f_branch
    | _ -> raise (TypeError "guard not bool"))
  | Let (v, b, e1, e2) ->
    (if (b = false) then 
      let e1_eval = eval_expr env e1 in 
      let expansion = extend env v e1_eval in 
      let e2_eval = eval_expr expansion e2 in 
      e2_eval
    else 
      let temp_expansion = extend_tmp env v in 
      let e1_eval = eval_expr temp_expansion e1 in 
      let _ = update temp_expansion v e1_eval in
      let e2_eval = eval_expr temp_expansion e2 in 
      e2_eval)
  | Fun (v, e1) -> Closure(env, v, e1)
  | App (e1, e2) -> 
    let e1_eval = eval_expr env e1 in 
    (match e1_eval with 
    | Closure(a, x, e) -> 
      let e2_eval = eval_expr env e2 in 
      let expansion = extend a x e2_eval in 
      let final_eval = eval_expr expansion e in 
      final_eval
    | _ -> raise (TypeError "App"))
  | Record record -> Record record
  | Select (label, e1) -> 
    let e1_eval = eval_expr env e1 in 
    (match e1_eval with 
    | Record record -> 
      let rec finder record label = 
        (match record with 
        | [] -> raise (SelectError "not found")
        | h :: t -> if fst h = label then snd h else finder t label)
      in eval_expr env (finder record label) 
    | _ -> raise (TypeError "Select"))
  | _ -> raise (TypeError "ur cooked buddy")


(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with 
  | Def (v, e1) -> 
    let temp = extend_tmp env v in 
    let e1_eval = eval_expr temp e1 in 
    let _ = update temp v e1_eval in 
    (temp,Some e1_eval)
  | Expr e1 -> 
    let e1_eval = eval_expr env e1 in 
    (env, Some e1_eval)
  | NoOp -> (env, None)