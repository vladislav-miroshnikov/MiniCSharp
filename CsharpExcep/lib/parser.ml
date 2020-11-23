open Ast
open Opal

let reserved =
  [ "true"; "false"; "if"; "else"; "while"; "public"; "static"; "const"
  ; "override"; "try"; "catch"; "finally"; "when"; "void"; "string"; "char"
  ; "Console"; "namespace"; "using"; "int"; "bool"; "for"; "null"; "new"
  ; "return"; "break"; "continue"; "class" (*check*) ]

let parens = between (token "(") (token ")")
let braces = between (token "{") (token "}")

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
  let null = token "null" >> return Null

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

  let rec expr input = num_expr input
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
    ( init_instance <|> assign <|> field_access <|> call_method <|> parens expr
    <|> atomic )
      input

  and separate_comma input = sep_by expr (token ",") input

  and call_method input =
    ( get_variable
    >>= fun name ->
    token "(" >> separate_comma
    >>= fun args_list -> token ")" >> return (CallMethod (name, args_list)) )
      input

  and init_instance input =
    ( token "new" >> ident_obj
    >>= fun name ->
    token "(" >> separate_comma
    >>= fun args_list -> token ")" >> return (ClassCreate (name, args_list)) )
      input

  and field_access input =
    let helper = parens init_instance <|> call_method <|> get_variable in
    ( helper
    >>= fun head ->
    many1 (token "." >> helper)
    => fun tl -> List.fold_left (fun head tl -> Access (head, tl)) head tl )
      input

  and assign input =
    let parse_left = field_access <|> call_method <|> get_variable in
    ( parse_left
    >>= fun left ->
    token "=" >> expr >>= fun right -> return (Assign (left, right)) )
      input
end

module Stat = struct
  open Expr

  let parse_continue = token "continue" >> token ";" >> return Continue
  let parse_break = token "break" >> token ";" >> return Break

  let parse_return =
    token "return"
    >> choice
         [ (expr >>= fun result -> token ";" >> return (Return (Some result)))
         ; token ";" >> return (Return None) ]

  let parse_expr =
    expr >>= fun express -> token ";" >> return (Expression express)

  let rec parse_statement input =
    choice
      [ parse_continue; parse_break; parse_expr; parse_return; parse_if
      ; parse_while; parse_var_declare; (*parse_for;*) parse_throw
      ; parse_stat_block; parse_print (*parse_try*) ]
      input

  and parse_if input =
    ( token "if" >> parens expr
    >>= fun condition ->
    parse_statement
    >>= fun then_stat ->
    choice
      [ ( token "else" >> parse_statement
        >>= fun else_stat -> return (If (condition, then_stat, Some else_stat))
        ); return (If (condition, then_stat, None)) ] )
      input

  and parse_while input =
    ( token "while" >> parens expr
    >>= fun condition ->
    parse_statement >>= fun stat -> return (While (condition, stat)) )
      input

  and parse_var_declare input =
    let helper =
      get_variable
      >>= fun var_name ->
      token "=" >> expr
      >>= (fun var_value -> return (var_name, Some var_value))
      <|> return (var_name, None) in
    ( define_type
    >>= fun var_type ->
    sep_by1 helper (token ",")
    >>= fun var_pair -> token ";" >> return (VarDeclare (var_type, var_pair)) )
      input

  and parse_stat_block input =
    ( braces (sep_by parse_statement spaces)
    >>= fun stats -> return (StatementBlock stats) )
      input

  (* and parse_for input =  *)
  and parse_print input =
    ( token "Console.WriteLine(" >> expr
    >>= fun print_expression -> token ");" >> return (Print print_expression) )
      input

  and parse_throw input =
    ( token "throw" >> expr
    >>= fun throw_expr -> token ";" >> return (Throw throw_expr) )
      input

  (* and parse_try input = (token "try" >> parse_statement >>= fun try_stat -> ) *)
end
