open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  let a1 = lookahead toks in 
  match a1 with 
  | None -> raise (InvalidInputException "None parse_expr")
  | Some input -> 
    match input with 
    | Tok_Let ->
      let next = match_token toks Tok_Let in
      parse_let next
    | Tok_Fun -> 
      let next = match_token toks Tok_Fun in 
      parse_fun next
    | Tok_If ->
      let next = match_token toks Tok_If in 
      parse_if next
    | _ ->
      parse_or toks
    
    and parse_let toks = 
      let parse_let_helper tok_lst = 
        let a1 = lookahead tok_lst in 
        match a1 with 
        | Some Tok_Rec ->
          let new_lst = match_token tok_lst Tok_Rec in 
          (new_lst, true)
        | _-> (tok_lst, false) in

      let a2 = parse_let_helper toks in 
      let a3 = lookahead (fst a2) in 
      (match a3 with
      | Some (Tok_ID id) -> 
        let after_id_token = match_token (fst a2) (Tok_ID id) in 
        let look_equals = lookahead after_id_token in
        (match look_equals with
        | Some Tok_Equal ->
          let after_equal_token = match_token after_id_token Tok_Equal in
          let after_expr1 = parse_expr after_equal_token in
          let look_in = lookahead (fst after_expr1) in
          (match look_in with
          | Some Tok_In ->
            let after_in_token = match_token (fst after_expr1) Tok_In in
            let after_expr2 = parse_expr after_in_token in
            (fst after_expr2, Let (id, (snd a2), snd after_expr1, snd after_expr2 )) 
          | _ -> raise (InvalidInputException "In token parse_let"))
        | _ -> raise (InvalidInputException "Equal token parse_let"))
      | _ -> raise (InvalidInputException "ID token parse_let"))  

      
      and parse_fun toks = 
        let a1 = lookahead toks in 
        (match a1 with 
        | Some (Tok_ID id) ->
          let after_id_token = match_token toks (Tok_ID id) in 
          let look_arrow = lookahead after_id_token in
          (match look_arrow with
          | Some Tok_Arrow ->
            let after_arrow_token = match_token after_id_token Tok_Arrow in 
            let after_expr = parse_expr after_arrow_token in 
            (fst after_expr, Fun(id, snd after_expr))
          | _ -> raise (InvalidInputException "Arrow token parse_fun"))
         | _ -> raise (InvalidInputException "ID token parse_fun"))
         

      and parse_if toks = 
        let after_expr1 = parse_expr toks in 
        let look_then = lookahead (fst after_expr1) in 
        (match look_then with
        | Some Tok_Then ->
          let after_then_token = match_token (fst after_expr1) Tok_Then in
          let after_expr2 = parse_expr after_then_token in
          let look_else = lookahead (fst after_expr2) in 
          (match look_else with
          | Some Tok_Else ->
            let after_else_token = match_token (fst after_expr2) Tok_Else in
            let after_expr3 = parse_expr after_else_token in 
            (fst after_expr3, If(snd after_expr1, snd after_expr2, snd after_expr3))
          | _ -> raise (InvalidInputException "Else token parse_if")) 
        | _ -> raise (InvalidInputException "Then token parse_if"))



        and parse_or toks = 
          let after_parse_and = parse_and toks in
          let look_or = lookahead (fst after_parse_and) in 
          (match look_or with
          | Some Tok_Or ->
            let after_or_token = match_token (fst after_parse_and) Tok_Or in
            let after_parse_or = parse_or after_or_token in
            (fst after_parse_or, Binop(Or, snd after_parse_and, snd after_parse_or))
          | _ -> after_parse_and
          )

        
        and parse_and toks = 
          let after_parse_equal = parse_equal toks in
          let look_and = lookahead (fst after_parse_equal) in
          (match look_and with
          | Some Tok_And ->
            let after_and_token = match_token (fst after_parse_equal) Tok_And in
            let after_parse_and = parse_and after_and_token in 
            (fst after_parse_and, Binop(And, snd after_parse_equal, snd after_parse_and))
          | _ -> after_parse_equal
          )


        and parse_equal toks = 
          let after_parse_relexpr = parse_relexpr toks in
          let look_equal = lookahead (fst after_parse_relexpr) in 
          (match look_equal with
          | Some Tok_Equal ->
            let after_equal_token = match_token (fst after_parse_relexpr) Tok_Equal in
            let after_parse_equal = parse_equal after_equal_token in
            (fst after_parse_equal, (Binop(Equal, snd after_parse_relexpr, snd after_parse_equal)))
          | Some Tok_NotEqual ->
            let after_notequal_token = match_token (fst after_parse_relexpr) Tok_NotEqual in
            let after_parse_equal = parse_equal after_notequal_token in
            (fst after_parse_equal, (Binop(NotEqual, snd after_parse_relexpr, snd after_parse_equal)))
          | _ -> after_parse_relexpr
          )
          


        and parse_relexpr toks = 
          let after_parse_add = parse_add toks in 
          let look_rel = lookahead (fst after_parse_add) in 
          (match look_rel with
          | Some Tok_Less ->
            let after_less = match_token (fst after_parse_add) Tok_Less in
            let after_parse_relexpr = parse_relexpr after_less in
            (fst after_parse_relexpr, Binop(Less, snd after_parse_add, snd after_parse_relexpr))
          | Some Tok_Greater ->
            let after_greater = match_token (fst after_parse_add) Tok_Greater in
            let after_parse_relexpr = parse_relexpr after_greater in
            (fst after_parse_relexpr, Binop(Greater, snd after_parse_add, snd after_parse_relexpr))
          | Some Tok_LessEqual ->
            let after_lessequal = match_token (fst after_parse_add) Tok_LessEqual in
            let after_parse_relexpr = parse_relexpr after_lessequal in
            (fst after_parse_relexpr, Binop(LessEqual, snd after_parse_add, snd after_parse_relexpr)) 
          | Some Tok_GreaterEqual ->
            let after_greaterequal = match_token (fst after_parse_add) Tok_GreaterEqual in
            let after_parse_relexpr = parse_relexpr after_greaterequal in
            (fst after_parse_relexpr, Binop(GreaterEqual, snd after_parse_add, snd after_parse_relexpr))
          | _ -> after_parse_add
          )


        and parse_add toks = 
          let after_parse_mult = parse_multi toks in 
          let look_add = lookahead (fst after_parse_mult) in 
          (match look_add with
          | Some Tok_Add ->
            let after_add = match_token (fst after_parse_mult) Tok_Add in
            let after_parse_add = parse_add after_add in
            (fst after_parse_add, Binop(Add, snd after_parse_mult, snd after_parse_add))
          | Some Tok_Sub -> 
            let after_sub = match_token (fst after_parse_mult) Tok_Sub in
            let after_parse_add = parse_add after_sub in
            (fst after_parse_add, Binop(Sub, snd after_parse_mult, snd after_parse_add))
          | _ -> after_parse_mult
          )



        and parse_multi toks = 
          let after_parse_concat = parse_concat toks in 
          let look_op = lookahead (fst after_parse_concat) in
          (match look_op with
          | Some Tok_Mult -> 
            let after_mult = match_token (fst after_parse_concat) Tok_Mult in 
            let after_parse_multi = parse_multi after_mult in
            (fst after_parse_multi, Binop(Mult, snd after_parse_concat, snd after_parse_multi))
          | Some Tok_Div -> 
            let after_div = match_token (fst after_parse_concat) Tok_Div in
            let after_parse_multi = parse_multi after_div in 
            (fst after_parse_multi, Binop(Div, snd after_parse_concat, snd after_parse_multi))
          | _ -> after_parse_concat
          )

        and parse_concat toks = 
          let after_parse_unary = parse_unary toks in 
          let look_concat = lookahead (fst after_parse_unary) in
          (match look_concat with
          | Some Tok_Concat ->
            let after_concat = match_token (fst after_parse_unary) Tok_Concat in 
            let after_parse_concat = parse_concat after_concat in 
            (fst after_parse_concat, Binop (Concat, snd after_parse_unary, snd after_parse_concat))
          | _ -> after_parse_unary
          )


        and parse_unary toks = 
          let look_not = lookahead toks in
          (match look_not with
          | Some Tok_Not ->
            let after_not_token = match_token toks Tok_Not in
            let after_parser = parse_unary after_not_token in
            (fst after_parser, Not (snd after_parser))
          | _ -> parse_appexpr toks
          )



        and parse_appexpr toks = 
          let after_parse_select1 = parse_selectexpr toks in
          let look_primary = lookahead (fst after_parse_select1) in
          (match look_primary with
           | Some (Tok_Int _ | Tok_Bool _ | Tok_String _ | Tok_ID _ | Tok_LParen | Tok_LCurly)-> 
            let after_parse_primary1 = parse_primaryexpr (fst after_parse_select1) in
            (fst after_parse_primary1, App (snd after_parse_select1, snd after_parse_primary1))
           | _ -> after_parse_select1)



        and parse_selectexpr toks = 
          let after_parser = parse_primaryexpr toks in 
          let look_dot = lookahead (fst after_parser) in 
          (match look_dot with 
          | Some Tok_Dot -> 
            let after_dot = match_token (fst after_parser) Tok_Dot in 
            let look_id = lookahead after_dot in 
            (match look_id with 
            | Some (Tok_ID id) ->
              let after_id = match_token after_dot (Tok_ID id) in 
              (after_id, Select(Lab id, snd after_parser))
            | _ -> raise (InvalidInputException "ID token selectexpr"))
          | _ -> after_parser)

        and parse_primaryexpr toks = 
          let a1 = lookahead toks in 
          (match a1 with 
          | Some (Tok_Int int) -> 
            let after_int = match_token toks (Tok_Int int) in 
            (after_int, Int int)
          | Some (Tok_Bool bool) -> 
            let after_bool = match_token toks (Tok_Bool bool) in 
            (after_bool, Bool bool)
          | Some (Tok_String string) ->
            let after_string = match_token toks (Tok_String string) in 
            (after_string, String string)
          | Some (Tok_ID id) ->
            let after_id = match_token toks (Tok_ID id) in 
            (after_id, ID id)
          | Some Tok_LParen -> 
            let after_lparen = match_token toks Tok_LParen in 
            let after_parser = parse_expr after_lparen in 
            let look_rparen = lookahead (fst after_parser) in 
            (match look_rparen with
            | Some Tok_RParen ->
              let after_rparen = match_token (fst after_parser) Tok_RParen in 
              (after_rparen, snd after_parser)
            | _ -> raise (InvalidInputException "RParen token primaryexpr"))
          | Some Tok_LCurly -> parse_recordexpr toks
          | _ -> raise (InvalidInputException "general match token primaryexpr"))



        and parse_recordexpr toks = 
          let look_lcurly = lookahead toks in 
          (match look_lcurly with
          | Some Tok_LCurly -> 
            let after_lcurly = match_token toks Tok_LCurly in 
            let check_next = lookahead after_lcurly in 
            (match check_next with
            | Some Tok_RCurly -> 
              let after_rcurly = match_token after_lcurly Tok_RCurly in 
              (after_rcurly, Record([]))
            | _ -> 
              let after_parser = parse_recordbodyexpr after_lcurly in 
              let look_rcurly = lookahead (fst after_parser) in 
              (match look_rcurly with
              | Some Tok_RCurly -> 
                let after_rcurly = match_token (fst after_parser) Tok_RCurly in
                (after_rcurly, Record(snd after_parser))
              | _ -> raise (InvalidInputException "RCurly token recordexpr")))
          | _ -> raise (InvalidInputException "LCurly token recordexpr"))
        
        and parse_recordbodyexpr toks = 
          let look_id = lookahead toks in 
          (match look_id with 
          | Some (Tok_ID id) -> 
            let after_id_token = match_token toks (Tok_ID id) in 
            let look_equal = lookahead after_id_token in 
            (match look_equal with
            | Some Tok_Equal -> 
              let after_equal_token = match_token after_id_token Tok_Equal in 
              let after_expr1 = parse_expr after_equal_token in 
              let look_semi = lookahead (fst after_expr1) in 
              (match look_semi with
              | Some Tok_Semi ->
                let after_semi_token = match_token (fst after_expr1) Tok_Semi in 
                let after_parse_recordbody = parse_recordbodyexpr after_semi_token in 
                (fst after_parse_recordbody,(Lab id, snd after_expr1) :: snd after_parse_recordbody)
              | _ -> (fst after_expr1, [(Lab id, snd after_expr1)]))
            | _ -> raise (InvalidInputException "Equal token recordbodyexpr"))
          | _ -> (toks, []))



(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  let look_def = lookahead toks in 
  (match look_def with
  | Some Tok_Def ->
    let after_parse_def = parse_defmutop toks in
    after_parse_def
  | Some Tok_DoubleSemi ->
    let after_double = match_token toks Tok_DoubleSemi in 
    ([], NoOp) 
  | _ -> let after_parse_expr = parse_exprmutop toks in 
    after_parse_expr
  )



  and parse_defmutop toks = 
    let look_def = lookahead toks in 
    (match look_def with
    | Some Tok_Def -> 
      let after_def_token = match_token toks Tok_Def in 
      let look_id = lookahead after_def_token in 
      (match look_id with
      | Some (Tok_ID id) ->
        let after_id = match_token after_def_token (Tok_ID id) in 
        let look_equal = lookahead after_id in 
        (match look_equal with
        | Some Tok_Equal ->
          let after_equal = match_token after_id Tok_Equal in 
          let after_parse = parse_expr after_equal in 
          let look_double = lookahead (fst after_parse) in 
          (match look_double with
          | Some Tok_DoubleSemi ->
            let after_double = match_token (fst after_parse) Tok_DoubleSemi in 
            (after_double, Def(id, snd after_parse))
          | _ -> raise (InvalidInputException "DoubleSemi token defmutop"))
        | _ -> raise (InvalidInputException "Equal token defmutop"))
      | _ -> raise (InvalidInputException "ID token defmutop"))
    | _ -> raise (InvalidInputException "Def token defmutop"))



  and parse_exprmutop toks = 
    let after_parseexpr = parse_expr toks in 
    let look_doublesemi = lookahead (fst after_parseexpr) in 
    (match look_doublesemi with
    | Some Tok_DoubleSemi ->
      let after_double = match_token (fst after_parseexpr) Tok_DoubleSemi in 
      (after_double, Expr (snd after_parseexpr))
    | _ -> raise (InvalidInputException "DoubleSemi Token in ExprMutop")
    )
