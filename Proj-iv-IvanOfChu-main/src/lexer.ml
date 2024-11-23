open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let re_rightparen = Str.regexp ")"
let re_leftparen = Str.regexp "("
let re_rightcurly = Str.regexp "}"
let re_leftcurly = Str.regexp "{"
let re_dot = Str.regexp "\\."
let re_equal = Str.regexp "="
let re_notequal = Str.regexp "<>"
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greaterequal = Str.regexp ">="
let re_lessequal = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "\\bnot\\b"
let re_if = Str.regexp "\\bif\\b"
let re_then = Str.regexp "\\bthen\\b"
let re_else = Str.regexp "\\belse\\b"
let re_add = Str.regexp "+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "\\blet\\b"
let re_def = Str.regexp "\\bdef\\b"
let re_fun = Str.regexp "\\bfun\\b"
let re_in = Str.regexp "\\bin\\b"
let re_rec = Str.regexp "\\brec\\b"
let re_arrow = Str.regexp "->"
let re_doublesemi = Str.regexp ";;"
let re_semi = Str.regexp ";"
let re_integer = Str.regexp "[0-9]+"
let re_neg_integer = Str.regexp "(\\(-[0-9]+\\))"
let re_bool = Str.regexp "\\b\\(true\\|false\\)\\b"
let re_string = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_space = Str.regexp " "





let tokenize input = 
  let rec tok pos s = 
    if pos >= String.length s then []
    else if Str.string_match re_integer s pos then
      let token = Str.matched_string s in 
      let len = String.length token in 
      let int_value = int_of_string token in
      Tok_Int int_value :: tok (pos + len) s
    else if Str.string_match re_neg_integer s pos then 
      let token = Str.matched_string s in 
      let len = String.length token in
      let int_value = int_of_string (String.sub token 1 (len - 2)) in
      Tok_Int int_value :: tok (pos + len) s  
else if Str.string_match re_bool s pos then 
  let token = Str.matched_string s in 
  let len = String.length token in 
  let value = bool_of_string token in 
  Tok_Bool value :: tok (pos + len) s
else if Str.string_match re_string s pos then
  let token = Str.matched_string s in 
  let len = String.length token in 
  Tok_String (String.sub token 1 (len - 2)) :: tok (pos + len) s
    else if Str.string_match re_space s pos then tok (pos + 1) s
    else if Str.string_match re_rightparen s pos then Tok_RParen :: tok (pos + 1) s
    else if Str.string_match re_leftparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rightcurly s pos then Tok_RCurly :: tok (pos + 1) s
    else if Str.string_match re_leftcurly s pos then Tok_LCurly :: tok (pos + 1) s
    else if Str.string_match re_dot s pos then Tok_Dot :: tok (pos + 1) s
    else if Str.string_match re_equal s pos then Tok_Equal :: tok (pos + 1) s
    else if Str.string_match re_notequal s pos then Tok_NotEqual :: tok (pos + 2) s
    else if Str.string_match re_arrow s pos then Tok_Arrow :: tok (pos + 2) s
    else if Str.string_match re_greater s pos then Tok_Greater :: tok (pos + 1) s
    else if Str.string_match re_less s pos then Tok_Less :: tok (pos + 1) s
    else if Str.string_match re_greaterequal s pos then Tok_GreaterEqual :: tok (pos + 2) s
    else if Str.string_match re_lessequal s pos then Tok_LessEqual :: tok (pos + 2) s
    else if Str.string_match re_or s pos then Tok_Or :: tok (pos + 2) s  
    else if Str.string_match re_and s pos then Tok_And :: tok (pos + 3) s
    else if Str.string_match re_not s pos then Tok_Not :: tok (pos + 3) s
    else if Str.string_match re_if s pos then Tok_If :: tok (pos + 2) s
    else if Str.string_match re_then s pos then Tok_Then :: tok (pos + 4) s
    else if Str.string_match re_else s pos then Tok_Else :: tok (pos + 4) s
    else if Str.string_match re_add s pos then Tok_Add :: tok (pos + 1) s
    else if Str.string_match re_sub s pos then Tok_Sub :: tok (pos + 1) s
    else if Str.string_match re_mult s pos then Tok_Mult :: tok (pos + 1) s
    else if Str.string_match re_div s pos then Tok_Div :: tok (pos + 1) s
    else if Str.string_match re_concat s pos then Tok_Concat :: tok (pos + 1) s
    else if Str.string_match re_let s pos then Tok_Let :: tok (pos + 3) s
    else if Str.string_match re_rec s pos then Tok_Rec :: tok (pos + 3) s
    else if Str.string_match re_in s pos then Tok_In :: tok (pos + 2) s
    else if Str.string_match re_def s pos then Tok_Def :: tok (pos + 3) s
    else if Str.string_match re_fun s pos then Tok_Fun :: tok (pos + 3) s
    else if Str.string_match re_doublesemi s pos then Tok_DoubleSemi :: tok (pos + 2) s
    else if Str.string_match re_semi s pos then Tok_Semi :: tok (pos + 1) s
    else if Str.string_match re_id s pos then 
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_ID token :: tok (pos + len) s
    else raise (InvalidInputException "Tokenize error")
  in 
tok 0 input
