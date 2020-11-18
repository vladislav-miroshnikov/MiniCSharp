open Ast
open Opal

let reserved =
  [ "true"; "false"; "if"; "else"; "while"; "public"; "static"; "const"
  ; "override"; "try"; "catch"; "finally"; "when"; "void"; "string"; "char"
  ; "Console"; "namespace"; "using"; "int"; "bool"; "for"; "Null"; "new"
  ; "return"; "break"; "continue"; "class" (*check*) ]

let parens = between (token "(") (token ")")

(* let braces = between (token "{") (token "}") *)
let get_modifier =
  choice
    [ token "public" >> return Public; token "static" >> return Static
    ; token "const" >> return Const; token "override" >> return Override ]

let digits = spaces >> many1 digit => implode
let convert_to_int = digits => int_of_string
let get_value_list list = match list with Some x -> x | None -> []

module Expr = struct
  open Ast

  let add_op = token "+" >> return (fun x y -> Add (x, y))
  let sub_op = token "-" >> return (fun x y -> Sub (x, y))
  let mul_op = token "*" >> return (fun x y -> Mul (x, y))
  let div_op = token "/" >> return (fun x y -> Div (x, y))
  let mod_op = token "%" >> return (fun x y -> Mod (x, y))
  let or_op = token "||" >> return (fun x y -> Or (x, y))
  let and_op = token "&&" >> return (fun x y -> And (x, y))
  let l_op = token "<" >> return (fun x y -> Less (x, y))
  let m_op = token ">" >> return (fun x y -> More (x, y))
  let le_op = token "<=" >> return (fun x y -> LessOrEqual (x, y))
  let me_op = token ">=" >> return (fun x y -> MoreOrEqual (x, y))
  let eq_op = token "==" >> return (fun x y -> Equal (x, y))
  let neq_op = token "!=" >> return (fun x y -> NotEqual (x, y))
  let null = token "Null" >> return Null

  let ident_obj =
    spaces >> letter <~> many alpha_num => implode
    >>= function x when List.mem x reserved -> mzero | x -> return x

  let get_variable = ident_obj => fun x -> IdentObj x

  let parse_string =
    let string_of_chars chars =
      let buf = Buffer.create 16 in
      List.iter (Buffer.add_char buf) chars ;
      Buffer.contents buf in
    token "\""
    >> many (satisfy (fun x -> x <> '\"'))
    >>= fun list ->
    token "\"" >> return (ConstExpr (CsVString (string_of_chars list)))

  let atomic =
    get_variable
    <|> (convert_to_int >>= fun n -> return (ConstExpr (CsVInt n)))
    <|> parse_string
    <|> (token "false" >> return (ConstExpr (CsVBool false)))
    <|> (token "true" >> return (ConstExpr (CsVBool true)))
    <|> null

  let define_type =
    choice
      [ token "int" >> return CsInt; token "String" >> return CsString
      ; token "void" >> return CsVoid
      ; (ident_obj >>= fun class_name -> return (CsClass class_name)) ]

  let%test _ =
    parse define_type (LazyStream.of_string "Car") = Some (CsClass "Car")

  let rec expression input = num_expr input
  and num_expr input = (chainl1 and_expr or_op) input
  and and_expr input = (chainl1 comp_expr and_op) input

  and comp_expr input =
    (chainl1 add_expr (le_op <|> me_op <|> l_op <|> m_op <|> eq_op <|> neq_op))
      input

  and add_expr input = (chainl1 mul_expr (add_op <|> sub_op)) input
  and mul_expr input = (chainl1 unar_expr (mul_op <|> div_op <|> mod_op)) input

  and unar_expr input =
    choice
      [ (token "!" >> lexeme primar_expr >>= fun x -> return (Not x))
      ; ( token "-" >> lexeme primar_expr
        >>= fun x -> return (Sub (ConstExpr (CsVInt 0), x)) )
      ; (token "++" >> lexeme primar_expr >>= fun x -> return (PrefInc x))
      ; (token "--" >> lexeme primar_expr >>= fun x -> return (PrefDec x))
      ; (lexeme primar_expr >>= fun x -> token "++" >> return (PostInc x))
      ; (lexeme primar_expr >>= fun x -> token "--" >> return (PostDec x))
      ; primar_expr ]
      input

  and primar_expr input =
    ( (*init_obj <|> assign <|> field_access <|> method_call <|>*)
      parens expression
    <|> atomic )
      input
end
