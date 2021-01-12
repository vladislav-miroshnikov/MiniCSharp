open Ast
open Parser
open Pretty_printer
open Format

let pp_get_diff pp_old pp_new ppf = function
  | elem -> fprintf ppf "-- %a\n++ %a\n\n" pp_old elem pp_new elem

let rec check_on_name name = function
  | Add (l, r)
   |Sub (l, r)
   |Div (l, r)
   |Mod (l, r)
   |Mul (l, r)
   |Or (l, r)
   |And (l, r)
   |Equal (l, r)
   |NotEqual (l, r)
   |Less (l, r)
   |More (l, r)
   |LessOrEqual (l, r)
   |MoreOrEqual (l, r)
   |Assign (l, r)
   |Access (l, r) ->
      check_on_name name l || check_on_name name r
  | CallMethod (_, args) | ClassCreate (_, args) ->
      List.exists (fun e -> check_on_name name e) args
  | IdentVar id_name -> name = id_name
  | Null | ConstExpr _ -> false
  | PrefInc e | PrefDec e | PostInc e | PostDec e | Not e ->
      check_on_name name e

let rec expr_rename old_name new_name ppf = function
  | Add (left, right) ->
      fprintf ppf "%a + %a"
        (expr_rename old_name new_name)
        left
        (expr_rename old_name new_name)
        right
  | Sub (left, right) ->
      fprintf ppf "%a - %a"
        (expr_rename old_name new_name)
        left
        (expr_rename old_name new_name)
        right
  | Mul (left, right) ->
      (*аналогично логике pretty-printer*)
      if check_add_or_sub left then (
        fprintf ppf "%a * " (expr_parens old_name new_name) left ;
        if check_add_or_sub right then
          fprintf ppf "%a" (expr_parens old_name new_name) right
        else fprintf ppf "%a" (expr_rename old_name new_name) right )
      else (
        fprintf ppf "%a * " (expr_rename old_name new_name) left ;
        if check_add_or_sub right then
          fprintf ppf "%a" (expr_parens old_name new_name) right
        else fprintf ppf "%a" (expr_rename old_name new_name) right )
  | Div (left, right) ->
      if check_add_or_sub left then (
        fprintf ppf "%a / " (expr_parens old_name new_name) left ;
        if check_add_or_sub right then
          fprintf ppf "%a" (expr_parens old_name new_name) right
        else fprintf ppf "%a" (expr_rename old_name new_name) right )
      else (
        fprintf ppf "%a / " (expr_rename old_name new_name) left ;
        if check_add_or_sub right then
          fprintf ppf "%a" (expr_parens old_name new_name) right
        else fprintf ppf "%a" (expr_rename old_name new_name) right )
  | Mod (left, right) ->
      if check_add_or_sub left then (
        fprintf ppf "%a %% " (expr_parens old_name new_name) left ;
        if check_add_or_sub right then
          fprintf ppf "%a" (expr_parens old_name new_name) right
        else fprintf ppf "%a" (expr_rename old_name new_name) right )
      else (
        fprintf ppf "%a %% " (expr_rename old_name new_name) left ;
        if check_add_or_sub right then
          fprintf ppf "%a" (expr_parens old_name new_name) right
        else fprintf ppf "%a" (expr_rename old_name new_name) right )
  | PostInc el -> fprintf ppf "%a++" (expr_rename old_name new_name) el
  | PrefInc el -> fprintf ppf "++%a" (expr_rename old_name new_name) el
  | PostDec el -> fprintf ppf "%a--" (expr_rename old_name new_name) el
  | PrefDec el -> fprintf ppf "--%a" (expr_rename old_name new_name) el
  | Null -> fprintf ppf "null"
  | ConstExpr value -> fprintf ppf "%a" pp_value value
  | And (left, right) ->
      if check_or left then (
        fprintf ppf "%a && " (expr_parens old_name new_name) left ;
        if check_or right then
          fprintf ppf "%a" (expr_parens old_name new_name) right
        else fprintf ppf "%a" (expr_rename old_name new_name) right )
      else (
        fprintf ppf "%a && " (expr_rename old_name new_name) left ;
        if check_or right then
          fprintf ppf "%a" (expr_parens old_name new_name) right
        else fprintf ppf "%a" (expr_rename old_name new_name) right )
  | Or (left, right) ->
      fprintf ppf "%a || %a"
        (expr_rename old_name new_name)
        left
        (expr_rename old_name new_name)
        right
  | Not be ->
      if check_const_or_id be then
        fprintf ppf "!%a" (expr_rename old_name new_name) be
      else fprintf ppf "!%a" (expr_parens old_name new_name) be
  | Less (left, right) ->
      fprintf ppf "%a < %a"
        (expr_rename old_name new_name)
        left
        (expr_rename old_name new_name)
        right
  | More (left, right) ->
      fprintf ppf "%a > %a"
        (expr_rename old_name new_name)
        left
        (expr_rename old_name new_name)
        right
  | LessOrEqual (left, right) ->
      fprintf ppf "%a <= %a"
        (expr_rename old_name new_name)
        left
        (expr_rename old_name new_name)
        right
  | MoreOrEqual (left, right) ->
      fprintf ppf "%a >= %a"
        (expr_rename old_name new_name)
        left
        (expr_rename old_name new_name)
        right
  | Equal (left, right) ->
      fprintf ppf "%a == %a"
        (expr_rename old_name new_name)
        left
        (expr_rename old_name new_name)
        right
  | NotEqual (left, right) ->
      fprintf ppf "%a != %a"
        (expr_rename old_name new_name)
        left
        (expr_rename old_name new_name)
        right
  | IdentVar curr_name ->
      if curr_name = old_name then fprintf ppf "%s" new_name
      else fprintf ppf "%s" curr_name
  | ClassCreate (name, expr_list) ->
      fprintf ppf "new %a(%a)" pp_name name
        (expr_list_rename old_name new_name)
        expr_list
  | CallMethod (m_expr, expr_list) ->
      fprintf ppf "%a(%a)" pp_name m_expr
        (expr_list_rename old_name new_name)
        expr_list
  | Assign (left, right) ->
      fprintf ppf "%a = %a"
        (expr_rename old_name new_name)
        left
        (expr_rename old_name new_name)
        right
  | Access (left, right) ->
      if check_create left then
        fprintf ppf "%a.%a"
          (expr_parens old_name new_name)
          left
          (expr_rename old_name new_name)
          right
      else
        fprintf ppf "%a.%a"
          (expr_rename old_name new_name)
          left
          (expr_rename old_name new_name)
          right

and expr_parens old_name new_name ppf =
  fprintf ppf "(%a)" (expr_rename old_name new_name)

and expr_list_rename old_name new_name ppf =
  pp_print_list ~pp_sep:pp_comma (expr_rename old_name new_name) ppf

let check_on_pairs old_name =
  List.exists (function name, expr_o ->
      ( match expr_o with
      | Some expr -> name = old_name || check_on_name old_name expr
      | None -> name = old_name ))

let pair_rename old_name new_name ppf = function
  | name, expr_o -> (
    match expr_o with
    | Some expr ->
        if name = old_name then
          fprintf ppf "%s = %a" new_name (expr_rename old_name new_name) expr
        else fprintf ppf "%s = %a" name (expr_rename old_name new_name) expr
    | None ->
        if name = old_name then fprintf ppf "%s" new_name
        else fprintf ppf "%s" name )

let pairs_rename old_name new_name ppf =
  pp_print_list ~pp_sep:pp_comma (pair_rename old_name new_name) ppf

let rec statement_rename old_name new_name ppf = function
  | If (condit, then_stat, else_stat_o) -> (
      let pp_condit pp_e ppf cond = fprintf ppf "if (%a)" pp_e cond in
      if check_on_name old_name condit then
        pp_get_diff (pp_condit pp_expression)
          (pp_condit (expr_rename old_name new_name))
          ppf condit
      else () ;
      statement_rename old_name new_name ppf then_stat ;
      match else_stat_o with
      | Some else_stat -> statement_rename old_name new_name ppf else_stat
      | None -> () )
  | While (condit, body) ->
      let pp_condit pp_e ppf cond = fprintf ppf "while (%a)" pp_e cond in
      if check_on_name old_name condit then
        pp_get_diff (pp_condit pp_expression)
          (pp_condit (expr_rename old_name new_name))
          ppf condit
      else () ;
      statement_rename old_name new_name ppf body
  | For (declare_o, condition_o, after_list, body) ->
      let rename_vars old_name new_name ppf = function
        | VarDeclare (modifier_o, var_type, pairs_list) -> (
          match modifier_o with
          | Some modifier ->
              fprintf ppf "%a %a %a;" pp_modifier modifier pp_data_type var_type
                (pairs_rename old_name new_name)
                pairs_list
          | None ->
              fprintf ppf "%a %a;" pp_data_type var_type
                (pairs_rename old_name new_name)
                pairs_list )
        | _ -> () in
      ( match (declare_o, condition_o) with
      | Some decs, Some condition ->
          let pp_var pp_s pp_e pp_aft ppf = function
            | de, be, aft ->
                fprintf ppf "for (%a %a; %a)" pp_s de pp_e be pp_aft aft in
          (match decs with VarDeclare (_, _, plist) -> plist | _ -> [])
          |> fun pair_list ->
          if
            check_on_name old_name condition
            || check_on_pairs old_name pair_list
            || List.exists (check_on_name old_name) after_list
          then
            pp_get_diff
              (pp_var pp_statement pp_expression pp_expression_list)
              (pp_var
                 (rename_vars old_name new_name)
                 (expr_rename old_name new_name)
                 (expr_list_rename old_name new_name))
              ppf
              (decs, condition, after_list)
      | None, Some condition ->
          let pp_var pp_e pp_aft ppf = function
            | be, aft -> fprintf ppf "for (; %a; %a)" pp_e be pp_aft aft in
          if
            check_on_name old_name condition
            || List.exists (check_on_name old_name) after_list
          then
            pp_get_diff
              (pp_var pp_expression pp_expression_list)
              (pp_var
                 (expr_rename old_name new_name)
                 (expr_list_rename old_name new_name))
              ppf (condition, after_list)
      | Some decs, None ->
          let pp_var pp_st pp_aft ppf = function
            | de, aft -> fprintf ppf "for (%a; %a)" pp_st de pp_aft aft in
          (match decs with VarDeclare (_, _, plist) -> plist | _ -> [])
          |> fun pair_list ->
          if
            check_on_pairs old_name pair_list
            || List.exists (check_on_name old_name) after_list
          then
            pp_get_diff
              (pp_var pp_statement pp_expression_list)
              (pp_var
                 (rename_vars old_name new_name)
                 (expr_list_rename old_name new_name))
              ppf (decs, after_list)
      | None, None ->
          let pp_var pp_aft ppf = function
            | aft -> fprintf ppf "for (;; %a)" pp_aft aft in
          if List.exists (check_on_name old_name) after_list then
            pp_get_diff
              (pp_var pp_expression_list)
              (pp_var (expr_list_rename old_name new_name))
              ppf after_list ) ;
      statement_rename old_name new_name ppf body
  | Return expr_o -> (
    match expr_o with
    | None -> ()
    | Some expr ->
        let pp_return pp_e ppf exp = fprintf ppf "return %a;" pp_e exp in
        if check_on_name old_name expr then
          pp_get_diff (pp_return pp_expression)
            (pp_return (expr_rename old_name new_name))
            ppf expr
        else () )
  | VarDeclare (modifier_o, var_type, pairs_list) -> (
      if check_on_pairs old_name pairs_list then
        match modifier_o with
        | Some modifier ->
            let vardec_rename pp_e ppf = function
              | md, v_type, pl ->
                  fprintf ppf "%a %a %a;" pp_modifier md pp_data_type v_type
                    pp_e pl in
            pp_get_diff
              (vardec_rename pp_declaration_list)
              (vardec_rename (pairs_rename old_name new_name))
              ppf
              (modifier, var_type, pairs_list)
        | None ->
            let vardec_rename pp_e ppf = function
              | vt, pl -> fprintf ppf "%a %a;" pp_data_type vt pp_e pl in
            pp_get_diff
              (vardec_rename pp_declaration_list)
              (vardec_rename (pairs_rename old_name new_name))
              ppf (var_type, pairs_list) )
  | Break | Continue -> ()
  | Expression expr ->
      if check_on_name old_name expr then
        let pp_ex pp_e ppf = function e -> fprintf ppf "%a;" pp_e e in
        pp_get_diff (pp_ex pp_expression)
          (pp_ex (expr_parens old_name new_name))
          ppf expr
  | StatementBlock stat_list ->
      List.iter (statement_rename old_name new_name ppf) stat_list
  | Print expr ->
      if check_on_name old_name expr then
        let pp_print pp_e ppf = function
          | e -> fprintf ppf "Console.WriteLine(%a);" pp_e e in
        pp_get_diff (pp_print pp_expression)
          (pp_print (expr_rename old_name new_name))
          ppf expr
  | Throw expr -> (
    match expr with
    | IdentVar _ ->
        if check_on_name old_name expr then
          let pp_throw pp_e ppf = function
            | e -> fprintf ppf "throw %a;" pp_e e in
          pp_get_diff (pp_throw pp_expression)
            (pp_throw (expr_rename old_name new_name))
            ppf expr
    | _ -> () )
  | Try (try_body, catch_list, finally_o) -> (
      statement_rename old_name new_name ppf try_body ;
      catch_list_rename old_name new_name ppf catch_list ;
      match finally_o with
      | Some finally_stat -> statement_rename old_name new_name ppf finally_stat
      | None -> () )

and catch_list_rename old_name new_name ppf =
  let catch_rename ppf = function
    | None, None, catch_stat ->
        statement_rename old_name new_name ppf catch_stat
    | None, Some condit, catch_stat ->
        let helper pp_e ppf = function
          | e -> fprintf ppf "catch when (%a)" pp_e e in
        pp_get_diff (helper pp_expression)
          (helper (expr_rename old_name new_name))
          ppf condit ;
        statement_rename old_name new_name ppf catch_stat
    | Some (CsClass _, None), None, catch_stat ->
        statement_rename old_name new_name ppf catch_stat
    | Some (CsClass cl_name, None), Some condit, catch_stat ->
        let helper pp_e ppf = function
          | e -> fprintf ppf "catch (%a) when (%a)" pp_name cl_name pp_e e in
        pp_get_diff (helper pp_expression)
          (helper (expr_rename old_name new_name))
          ppf condit ;
        statement_rename old_name new_name ppf catch_stat
    | Some (CsClass cl_name, Some (IdentVar th_ex_name)), None, catch_stat ->
        let helper in_name ppf = function
          | _ -> fprintf ppf "catch (%a %a)" pp_name cl_name pp_name in_name
        in
        pp_get_diff (helper th_ex_name) (helper new_name) ppf () ;
        statement_rename old_name new_name ppf catch_stat
    | ( Some (CsClass cl_name, Some (IdentVar th_ex_name))
      , Some condit
      , catch_stat ) ->
        let helper pp_e in_name ppf = function
          | e ->
              fprintf ppf "catch (%a %a) when (%a)" pp_name cl_name pp_name
                in_name pp_e e in
        pp_get_diff
          (helper pp_expression th_ex_name)
          (helper (expr_rename old_name new_name) new_name)
          ppf condit ;
        statement_rename old_name new_name ppf catch_stat
    | _ -> () in
  pp_print_list ~pp_sep:pp_end_l catch_rename ppf

let pp_field ppf = function
  | mod_list, VariableField (f_type, field_list) ->
      fprintf ppf "%a %a %a;" pp_modifier_list mod_list pp_data_type f_type
        pp_declaration_list field_list
  | _ -> ()

let rename_field old_name new_name ppf = function
  | mod_list, VariableField (f_type, field_list) ->
      fprintf ppf "%a %a %a;" pp_modifier_list mod_list pp_data_type f_type
        (pairs_rename old_name new_name)
        field_list
  | _ -> ()

let check_on_arg_name old_name =
  let helper old_n = function _, name -> name = old_n in
  List.exists (helper old_name)

let args_vars_rename old_name new_name ppf =
  let pp_var_pair old_name new_name ppf = function
    | v_type, name ->
        if name = old_name then fprintf ppf "%a %s" pp_data_type v_type new_name
        else fprintf ppf "%a %s" pp_data_type v_type name in
  pp_print_list ~pp_sep:pp_comma (pp_var_pair old_name new_name) ppf

let fields_rename old_name new_name ppf elem =
  match elem with
  | _, VariableField (_, pair_list) ->
      if check_on_pairs old_name pair_list then
        pp_get_diff pp_field (rename_field old_name new_name) ppf elem
  | mod_list, Method (meth_type, meth_name, args, body) ->
      ( if check_on_arg_name old_name args then
        let pp_m pp_args ppf = function
          | arg ->
              fprintf ppf "%a %a %a(%a)" pp_modifier_list mod_list pp_data_type
                meth_type pp_name meth_name pp_args arg in
        pp_get_diff
          (pp_m pp_type_name_pairs_list)
          (pp_m (args_vars_rename old_name new_name))
          ppf args ) ;
      statement_rename old_name new_name ppf body
  | mod_list, Constructor (c_name, args, body) ->
      ( if check_on_arg_name old_name args then
        let pp_constructor pp_args ppf = function
          | arg ->
              fprintf ppf "%a %a(%a)" pp_modifier_list mod_list pp_name c_name
                pp_args arg in
        pp_get_diff
          (pp_constructor pp_type_name_pairs_list)
          (pp_constructor (args_vars_rename old_name new_name))
          ppf args ) ;
      statement_rename old_name new_name ppf body

let class_rename old_name new_name ppf = function
  | Class (_, _, _, pairs_list) ->
      List.iter (fields_rename old_name new_name ppf) pairs_list

let class_list_rename old_name new_name ppf =
  List.iter (class_rename old_name new_name ppf)

let start_transform_rename input old_name new_name ppf =
  try
    let tree =
      match apply_parser parser input with
      | None | Some [] ->
          raise
            (Invalid_argument
               "No class found, you may have submitted an empty file or it's \
                syntax error in input")
      | Some cl_l -> cl_l in
    class_list_rename old_name new_name ppf tree
  with Invalid_argument message -> fprintf ppf "%s" message
