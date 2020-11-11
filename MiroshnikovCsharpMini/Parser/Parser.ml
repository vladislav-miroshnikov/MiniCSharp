open Opal
open AST

let parens = between (token "(") (token ")")

(* 
let brackets = between (token "[") (token "]") *)

(* let braces = between (token "{") (token "}") *)

let digits = spaces >> many1 digit => implode

let convert_to_int = digits => int_of_string

let get_value_list list = match list with Some x -> x | None -> []

module Expr = struct
  open AST

  let reserved =
    [
      "true";
      "false";
      "if";
      "else";
      "while";
      "public";
      "static";
      "const";
      "override";
      "try";
      "catch";
      "finally";
      "void";
      "string";
      "char";
      "Console";
      (*?*)
      "namespace";
      "using";
      "int";
      "bool";
      "for";
      "Null";
      "new";
      (*check*)
    ]

  let identity =
    spaces >> letter <~> many alpha_num => implode >>= function
    | s when List.mem s reserved -> mzero
    | s -> return s

  let get_variable = identity => fun s -> Variable s

  let null = token "Null" >> return Null

  let add_op = token "+" >> return (fun x y -> NumExpr (Add (x, y)))

  let sub_op = token "-" >> return (fun x y -> NumExpr (Sub (x, y)))

  let mul_op = token "*" >> return (fun x y -> NumExpr (Mul (x, y)))

  let div_op = token "/" >> return (fun x y -> NumExpr (Div (x, y)))

  let mod_op = token "%" >> return (fun x y -> NumExpr (Mod (x, y)))

  let or_op = token "||" >> return (fun x y -> LogicExpr (Or (x, y)))

  let and_op = token "&&" >> return (fun x y -> LogicExpr (And (x, y)))

  let lt_op = token "<" >> return (fun x y -> CompareExpr (Less (x, y)))

  let mt_op = token ">" >> return (fun x y -> CompareExpr (More (x, y)))

  let loet_op =
    token "<=" >> return (fun x y -> CompareExpr (LessOrEqual (x, y)))

  let moet_op =
    token ">=" >> return (fun x y -> CompareExpr (MoreOrEqual (x, y)))

  let eq_op = token "==" >> return (fun x y -> CompareExpr (Equal (x, y)))

  let neq_op = token "!=" >> return (fun x y -> CompareExpr (NotEqual (x, y)))

  let atomaric =
    get_variable
    <|> (convert_to_int => fun x -> ConstExp (CsVInt x))
    <|> (token "false" >> return (ConstExp (CsVBool false)))
    <|> (token "true" >> return (ConstExp (CsVBool true)))
    <|> null

  (*maybe more?*)
  let rec expr input = num_expr input

  and num_expr input = (chainl1 and_expr or_op) input

  and and_expr input = (chainl1 comp_expr and_op) input

  and comp_expr input =
    (chainl1 add_expr
       (lt_op <|> mt_op <|> loet_op <|> moet_op <|> eq_op <|> neq_op))
      input

  and add_expr input = (chainl1 mul_expr (add_op <|> sub_op)) input

  and mul_expr input = (chainl1 unar_expr (mul_op <|> div_op <|> mod_op)) input

  and unar_expr input =
    choice
      [
        ( token "!" >> lexeme primary_expr >>= fun s ->
          return (LogicExpr (Not s)) );
        ( token "-" >> lexeme primary_expr >>= fun x ->
          return (NumExpr (Sub (ConstExp (CsVInt 0), x))) );
        primary_expr;
      ]
      input

  (*inc and dec*)
  and primary_expr input =
    (parens expr <|> access_expr <|> method_call <|> atomaric) input

  and access_expr input =
    ( method_call <|> get_variable >>= fun name ->
      token "." >> lexeme expr >>= fun ob -> return (Access (name, ob)) )
      input

  and sep_by_comm_expr input = sep_by expr (token ",") input

  and method_call input =
    ( get_variable >>= fun name ->
      token "(" >> sep_by_comm_expr >>= fun args ->
      token ")" >> return (CallMethod (name, args)) )
      input
end
