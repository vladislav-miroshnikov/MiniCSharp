open Hashtbl
open Ast
open Parser
open Stack
open Operators
open Extractors
open Printf
open Printexc

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val get : 'a t -> 'a
  val error : string -> 'a t
end

module Result = struct
  (*может реализовать интерфейс?*)
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let error = Result.error
  let ( >> ) x f = x >>= fun _ -> f
  let get = Result.get_ok
end

type table_key = string [@@deriving show]

type table_constructor = {args: (data_type * expr) list; body: statement}
[@@deriving show {with_path= false}]

type table_field =
  { field_type: data_type
  ; field_key: table_key
  ; is_mutable: bool
  ; sub_tree: expr option }
[@@deriving show {with_path= false}]

type table_method =
  { method_type: data_type
  ; has_override: bool
  ; has_static_mod: bool
  ; method_key: table_key
  ; args: (data_type * expr) list
  ; body: statement }
[@@deriving show {with_path= false}]

type table_class =
  { class_key: table_key
  ; field_table: (table_key, table_field) Hashtbl.t
  ; method_table: (table_key, table_method) Hashtbl.t
  ; constructor_table: (table_key, table_constructor) Hashtbl.t
  ; parent_key: table_key option
  ; children_keys: table_key list
  ; dec_tree: cs_class }

let class_table : (table_key, table_class) Hashtbl.t = Hashtbl.create 1024

let startswith test_str sub_str =
  let sub = String.sub test_str 0 (String.length sub_str) in
  String.equal sub sub_str

let convert_table_to_list hashtable =
  Hashtbl.fold (fun _ v acc -> v :: acc) hashtable []

let get_value_option = Hashtbl.find_opt

module ClassLoader (M : MONADERROR) = struct
  open M

  let is_override = List.mem Override
  let is_static = List.mem Static
  let is_public = List.mem Public
  let is_const = List.mem Const

  let replace_elem_hash_table hashtable key value =
    Hashtbl.replace hashtable key value ;
    return hashtable

  let rec monadic_list_iter list action base =
    match list with
    | [] -> return base
    | x :: xs -> action x >> monadic_list_iter xs action base

  let system_exception_init hashtable =
    let constructor_table = Hashtbl.create 16 in
    let field_table = Hashtbl.create 16 in
    let method_table = Hashtbl.create 16 in
    let to_string : table_method =
      { method_type= String
      ; has_override= true
      ; has_static_mod= false
      ; method_key= "ToString"
      ; args= []
      ; body=
          Option.get
            (apply_parser Stat.stat_block
               {| 
              {
                return Message + StackTrace;
              }


          |})
      } in
    let message : table_field =
      { field_type= String
      ; field_key= "Message"
      ; is_mutable= true
      ; sub_tree= None } in
    let stack_trace : table_field =
      { field_type= String
      ; field_key= "StackTrace"
      ; is_mutable= true
      ; sub_tree= None } in
    Hashtbl.add method_table "ToString" to_string ;
    Hashtbl.add field_table "Message" message ;
    Hashtbl.add field_table "StackTrace" stack_trace ;
    Hashtbl.add hashtable "Exception"
      { class_key= "Exception"
      ; field_table
      ; method_table
      ; constructor_table
      ; parent_key= None
      ; children_keys= []
      ; dec_tree=
          Option.get
            (apply_parser parse_class
               {|
          public class Exception 
          {
            public string Message;
            public string StackTrace;
            public string ToString()
            {
                return Message + StackTrace;
            }
          }
      |})
      } ;
    return hashtable

  let field_modifiers_check pair =
    match pair with
    | l, f -> (
      match f with
      | Method (Void, "Main", [], _)
        when is_static l && (not (is_const l)) && not (is_override l) ->
          return ()
      | Method (_, "Main", _, _) ->
          error "Only one main method can be in program!"
      | Method (_, _, _, _) when is_const l -> error "Method can not be const"
      | Method (_, _, _, _) -> return ()
      | VariableField (_, _) when (not (is_static l)) && not (is_override l) ->
          return ()
      | VariableField (_, _) -> error "Wrong modifiers"
      | Constructor (_, _, _)
        when (not (is_static l))
             && (not (is_const l))
             && (not (is_override l))
             && is_public l ->
          return ()
      | Constructor (_, _, _) -> error "Wrong constructor modifiers" )

  let class_modifiers_check = function
    | Class (ml, _, _, _)
      when (not (is_static ml)) && (not (is_override ml)) && not (is_const ml)
      ->
        return ()
    | Class (_, _, _, _) -> error "Wrong class modifiers"

  let type_of_list = List.map fst

  let add_default_constructor hashtable =
    Hashtbl.iter
      (fun key some_class ->
        if Hashtbl.length some_class.constructor_table = 0 then
          Hashtbl.add some_class.constructor_table key
            {args= []; body= StatementBlock []})
      hashtable ;
    return hashtable

  let class_adding class_list hastable =
    let add_to_table hashtable key value message =
      match get_value_option hashtable key with
      | None ->
          Hashtbl.add hashtable key value ;
          return hashtable
      | _ -> error message in
    let add_class_table hashtable adding_class =
      match adding_class with
      | Class (_, class_key, parent, fields) ->
          let method_table = Hashtbl.create 1024 in
          let field_table = Hashtbl.create 1024 in
          let constructor_table = Hashtbl.create 1024 in
          class_modifiers_check adding_class
          >>
          let add_class_elem : modifier list * field -> unit M.t =
           fun field_elem ->
            match field_elem with
            | mod_list, VariableField (field_type, arg_list) ->
                let rec add_var_field = function
                  | [] -> return ()
                  | (field_key, sub_tree) :: ps ->
                      let is_mutable = not (is_const mod_list) in
                      add_to_table field_table field_key
                        {field_type; field_key; is_mutable; sub_tree}
                        "Similar fields"
                      >> add_var_field ps in
                field_modifiers_check field_elem >> add_var_field arg_list
            | mod_list, Method (method_type, m_name, args, body) ->
                let method_key =
                  String.concat ""
                    (m_name :: List.map show_data_type (type_of_list args))
                in
                let has_override = is_override mod_list in
                let has_static_mod = is_static mod_list in
                field_modifiers_check field_elem
                >> add_to_table method_table method_key
                     { method_type
                     ; has_override
                     ; has_static_mod
                     ; method_key
                     ; args
                     ; body }
                     "Method with this type exists"
                >> return ()
            | _, Constructor (name, args, body) ->
                let make_key =
                  String.concat ""
                    (name :: List.map show_data_type (type_of_list args)) in
                let check_name =
                  if name = class_key then return ()
                  else error "Constructor name error" in
                check_name
                >> field_modifiers_check field_elem
                >> add_to_table constructor_table make_key {args; body}
                     "Constructor with this type exists"
                >> return () in
          let add_parent p = match p with None -> None | _ -> p in
          (*CHECK!!!!!!!*)
          let parent_key = add_parent parent in
          monadic_list_iter fields add_class_elem ()
          >> add_to_table hashtable class_key
               { class_key
               ; field_table
               ; method_table
               ; constructor_table
               ; parent_key
               ; children_keys= []
               ; dec_tree= adding_class }
               "Similar Classes" in
    monadic_list_iter class_list (add_class_table hastable) hastable

  let update_exception_class_childs hashtable =
    let helper : table_class -> table_class M.t =
     fun some_class ->
      match some_class.parent_key with
      | None -> return some_class
      | Some key -> (
          let parent = get_value_option hashtable key in
          let exception_is_parent = String.compare "Exception" in
          match parent with
          | None -> error "No parent class found"
          | Some parent_o when exception_is_parent parent_o.class_key = 0 ->
              (*CHECK THIS!*)
              let new_val =
                { parent_o with
                  children_keys= some_class.class_key :: parent_o.children_keys
                } in
              replace_elem_hash_table hashtable key new_val >> return new_val
          | Some _ ->
              error "The class can only be inherited from the Exception class!"
          ) in
    monadic_list_iter (convert_table_to_list hashtable) helper hashtable

  let transfer_fields parent children =
    let exception_transfer_field : table_class -> table_field -> unit t =
     fun child_class p_field ->
      match get_value_option child_class.field_table p_field.field_key with
      | None ->
          return (Hashtbl.add child_class.field_table p_field.field_key p_field)
      | _ -> return () in
    monadic_list_iter
      (convert_table_to_list parent.field_table)
      (exception_transfer_field children)
      ()

  let transfer_methods parent children =
    let exception_transfer_method : table_class -> table_method -> unit t =
     fun child_class p_method ->
      match get_value_option child_class.method_table p_method.method_key with
      | None ->
          return
            (Hashtbl.add child_class.method_table p_method.method_key p_method)
      | _ -> return () in
    monadic_list_iter
      (convert_table_to_list parent.method_table)
      (exception_transfer_method children)
      ()

  let check_override_mod parent children =
    let check : table_class -> table_method -> unit t =
     fun parent child_method ->
      match child_method.has_override with
      | false -> return ()
      | true -> (
        match get_value_option parent.method_table child_method.method_key with
        | None ->
            error "Not overriden method or parent does not exist this method!"
        | _ -> return () ) in
    monadic_list_iter
      (convert_table_to_list children.method_table)
      (check parent) ()

  let transfer_to_child : table_class -> table_class -> unit t =
   fun parent children ->
    transfer_fields parent children
    >> transfer_methods parent children
    >> check_override_mod parent children

  let begin_inheritance_from_exception hashtable =
    let exception_class = Option.get (get_value_option hashtable "Exception") in
    let helper child_key =
      transfer_to_child exception_class
        (Option.get (get_value_option hashtable child_key)) in
    monadic_list_iter exception_class.children_keys helper hashtable

  let load_classes class_list =
    match class_list with
    | [] ->
        error
          "No class found, you may have submitted an empty file or it's syntax \
           error in input"
    | _ ->
        system_exception_init class_table
        >>= fun table ->
        class_adding class_list table
        >>= fun table_with_classes ->
        add_default_constructor table_with_classes
        >>= fun table_update ->
        update_exception_class_childs table_update
        >>= fun new_table -> begin_inheritance_from_exception new_table
end

module Interpreter (M : MONADERROR) = struct
  open M
  open Operators

  type variable =
    { var_key: table_key
    ; var_type: data_type
    ; var_value: value
    ; is_mutable: bool
    ; assignment_count: int
    ; visibility_level: int }
  [@@deriving show {with_path= false}]

  type context =
    { current_o: obj_ref
    ; variable_table: (table_key, variable) Hashtbl_der.t
    ; current_meth_type: data_type
    ; last_expr_result: value option
    ; was_break: bool
    ; was_return: bool
    ; was_continue: bool
    ; is_main: bool
    ; is_constructor: bool
    ; count_of_cycle: int
    ; visibility_level: int
    ; prev_ctx: context option
    ; count_of_obj: int
    ; is_creation: bool
    ; constr_affilation: table_key option
    ; exception_class: table_class option }

  let sharp_stack = Stack.create

  let context_init current_o variable_table =
    return
      { current_o
      ; variable_table
      ; current_meth_type= Void
      ; last_expr_result= None
      ; was_break= false
      ; was_return= false
      ; was_continue= false
      ; is_main= true
      ; is_constructor= false
      ; count_of_cycle= 0
      ; visibility_level= 0
      ; prev_ctx= None
      ; count_of_obj= 0
      ; is_creation= false
      ; constr_affilation= None
      ; exception_class= None }

  let find_main_class hashtable =
    List.find
      (fun elem -> Hashtbl.mem elem.method_table "Main")
      (convert_table_to_list hashtable)
    |> fun main_class -> return main_class

  let rec expression_check : expr -> context -> data_type M.t =
   fun cur_expr ctx ->
    match cur_expr with
    | Add (left, right) -> (
        expression_check left ctx
        >>= fun left_type ->
        match left_type with
        | Int -> (
            expression_check right ctx
            >>= fun right_type ->
            match right_type with
            | Int -> return Int
            | String ->
                return String (*because we can write this: string a = 3 + "b";*)
            | _ -> error "Incorrect type: it must be Int or String!" )
        | String -> (
            expression_check right ctx
            >>= fun right_type ->
            match right_type with
            | Int | String -> return String
            | _ -> error "Incorrect type: it must be Int or String!" )
        | _ -> error "Incorrect type: it must be Int or String!" )
    | Sub (left, right)
     |Div (left, right)
     |Mod (left, right)
     |Mul (left, right) -> (
        expression_check left ctx
        >>= fun left_type ->
        match left_type with
        | Int -> (
            expression_check right ctx
            >>= fun right_type ->
            match right_type with
            | Int -> return Int
            | _ -> error "Incorrect type: it must be Int!" )
        | _ -> error "Incorrect type: it must be Int!" )
    | PostDec value | PostInc value | PrefDec value | PrefInc value -> (
        expression_check value ctx
        >>= fun value_type ->
        match value_type with
        | Int -> return Int
        | _ -> error "Incorrect type: it must be Int!" )
    | And (left, right) | Or (left, right) -> (
        expression_check left ctx
        >>= fun left_type ->
        match left_type with
        | Bool -> (
            expression_check right ctx
            >>= fun right_type ->
            match right_type with
            | Bool -> return Bool
            | _ -> error "Incorrect type: it must be Bool!" )
        | _ -> error "Incorrect type: it must be Bool!" )
    | Not value -> (
        expression_check value ctx
        >>= fun value_type ->
        match value_type with
        | Bool -> return Bool
        | _ -> error "Incorrect type: it must be Bool!" )
    | Less (left, right)
     |More (left, right)
     |LessOrEqual (left, right)
     |MoreOrEqual (left, right) -> (
        expression_check left ctx
        >>= fun left_type ->
        match left_type with
        | Int -> (
            expression_check right ctx
            >>= fun right_type ->
            match right_type with
            | Int -> return Bool
            | _ -> error "Incorrect type: it must be Int!" )
        | _ -> error "Incorrect type: it must be Int!" )
    | Equal (left, right) | NotEqual (left, right) -> (
        expression_check left ctx
        >>= fun left_type ->
        match left_type with
        | Int -> (
            expression_check right ctx
            >>= fun right_type ->
            match right_type with
            | Int -> return Bool
            | _ -> error "Incorrect type: it must be Int!" )
        | String -> (
            expression_check right ctx
            >>= fun right_type ->
            match right_type with
            | String -> return Bool
            | _ -> error "Incorrect type: it must be String!" )
        | Bool -> (
            expression_check right ctx
            >>= fun right_type ->
            match right_type with
            | Bool -> return Bool
            | _ -> error "Incorrect type: it must be Bool!" )
        | CsClass left_name -> (
            expression_check right ctx
            >>= fun right_type ->
            match right_type with
            | CsClass right_name when left_name = right_name -> return Bool
            | CsClass "null" -> return Bool
            | _ -> error "Incorrect class type!" )
        | _ -> error "Incorrect type in equal-expression!" )
    | Null -> return (CsClass "null")
    | CallMethod (method_name, args) ->
        let obj_key =
          match ctx.current_o with
          | ObjNull -> "null"
          | ObjRef {class_key= key; class_table= _; number= _} -> key in
        method_verify
          (Option.get (get_value_option class_table obj_key))
          method_name args ctx
        >>= fun t_mth -> return t_mth.method_type
    | Access (calling_obj, IdentVar var_name) -> (
        expression_check calling_obj ctx
        >>= fun obj_type ->
        match obj_type with
        | CsClass "null" -> error "NullReferenceException"
        | CsClass obj_key -> (
            let find_field =
              get_value_option
                (Option.get (get_value_option class_table obj_key)).field_table
                var_name in
            match find_field with
            | None -> error "No matching field found"
            | Some found_field -> return found_field.field_type )
        | _ -> error "Invalid type: type must be reference" )
    | Access (calling_obj, CallMethod (m_name, args)) -> (
        expression_check calling_obj ctx
        >>= fun obj_type ->
        match obj_type with
        | CsClass "null" -> error "NullReferenceException"
        | CsClass obj_key ->
            method_verify
              (Option.get (get_value_option class_table obj_key))
              m_name args ctx
            >>= fun found_mth -> return found_mth.method_type
        | _ -> error "Invalid type: type must be reference" )
    | ClassCreate (class_name, args) -> (
      match get_value_option class_table class_name with
      | None -> error "Class not found"
      | Some t_class -> (
        match args with
        | [] -> return (CsClass class_name)
        | _ ->
            constructor_verify t_class args ctx >> return (CsClass class_name) )
      )
    | IdentVar var_key -> (
        let find_variable = get_value_option ctx.variable_table var_key in
        match find_variable with
        | None -> (
          match ctx.current_o with
          | ObjRef {class_key= _; class_table= table; number= _} -> (
            match get_value_option table var_key with
            | None -> error "There is no such variable or field with that name!"
            | Some field -> return field.f_type )
          | ObjNull -> error "NullReferenceException" )
        | Some variable -> return variable.var_type )
    | ConstExpr value -> (
      match value with
      | VInt _ -> return Int
      | VBool _ -> return Bool
      | VString _ -> return String
      | VClass ObjNull -> return (CsClass "null")
      | VClass (ObjRef {class_key= key; _}) -> return (CsClass key)
      | _ -> error "Incorrect const expression type!" )
    | Assign (left, right) -> (
        expression_check left ctx
        >>= fun left_type ->
        match left_type with
        | Void -> error "Can not assign to void"
        | CsClass left_key -> (
            expression_check right ctx
            >>= fun right_type ->
            match right_type with
            | CsClass "null" -> return (CsClass left_key)
            | CsClass right_key -> assign_verify_polymorphic left_key right_key
            | _ -> error "Incorrect type assigning!" )
        | _ ->
            expression_check right ctx
            >>= fun right_type ->
            if left_type = right_type then return right_type
            else error "Incorrect type assigning!" )
    | _ -> error "Incorrect expression"

  and assign_verify_polymorphic left_key right_key =
    (*For subtype polymorphism (inclusion polymorphism)*)
    match classname_verify_polymorphic left_key right_key with
    | false -> error "Incorrect assign type"
    | true -> return (CsClass right_key)

  (*copy-paste*)
  (* let rec check_parent key =
       let find_class = Option.get (get_value_option class_table key) in
       if find_class.class_key = left_key then return (CsClass right_key)
       else
         match find_class.parent_key with
         | None -> error "Incorrect assign type"
         | Some parent_key -> check_parent parent_key in
     check_parent right_key *)
  and constructor_verify t_class args g_ctx =
    let check_type : int -> data_type -> table_key -> table_constructor -> bool
        =
     fun position curr_type _ value ->
      match List.nth_opt value.args position with
      | None -> false
      | Some (f_type, _) -> (
        match curr_type with
        | CsClass "null" -> (
          match f_type with CsClass _ -> true | _ -> false )
        | CsClass cl_key -> (
          match f_type with
          | CsClass this_key -> classname_verify_polymorphic this_key cl_key
          | _ -> false )
        | _ -> f_type = curr_type ) in
    let rec helper hashtable pos expr_list ctx =
      match Hashtbl.length hashtable with
      | 0 -> error "Constructor not found"
      | other -> (
        match expr_list with
        | [] -> (
          match other with
          | 1 -> return (List.hd (convert_table_to_list hashtable))
          | _ -> error "Constructor not recognized" )
        | x :: xs ->
            expression_check x ctx
            >>= fun x_type ->
            helper
              (Hashtbl_der.filter hashtable (check_type pos x_type))
              (pos + 1) xs ctx ) in
    helper
      (Hashtbl_der.filter t_class.constructor_table (fun _ cr ->
           List.length cr.args = List.length args))
      0 args g_ctx

  and classname_verify_polymorphic left_key right_key =
    (*For subtype polymorphism (inclusion polymorphism) while passing parameters*)
    let rec check_par key =
      let get_p = Option.get (get_value_option class_table key) in
      if get_p.class_key = left_key then true
      else match get_p.parent_key with None -> false | Some pk -> check_par pk
    in
    check_par right_key

  and method_verify :
      table_class -> table_key -> expr list -> context -> table_method M.t =
   fun t_class m_name args g_ctx ->
    let check_type : int -> data_type -> table_key -> table_method -> bool =
     fun position curr_type _ value ->
      match List.nth_opt value.args position with
      | None -> false
      | Some (f_type, _) -> (
        match curr_type with
        | CsClass "null" -> (
          match f_type with CsClass _ -> true | _ -> false )
        | CsClass cl_key -> (
          match f_type with
          | CsClass this_key -> classname_verify_polymorphic this_key cl_key
          | _ -> false )
        | _ -> f_type = curr_type ) in
    let rec helper hashtable pos expr_list ctx =
      match Hashtbl.length hashtable with
      | 0 -> error "Method not found"
      | other -> (
        match expr_list with
        | [] -> (
          match other with
          | 1 -> return (List.hd (convert_table_to_list hashtable))
          | _ -> (
              Hashtbl_der.filter hashtable (fun _ mt ->
                  startswith mt.method_key m_name)
              |> fun filter_table ->
              match Hashtbl.length filter_table with
              | 1 -> return (List.hd (convert_table_to_list filter_table))
              | _ -> error "Method not recognized" ) )
        | x :: xs ->
            expression_check x ctx
            >>= fun x_type ->
            helper
              (Hashtbl_der.filter hashtable (check_type pos x_type))
              (pos + 1) xs ctx ) in
    helper
      (Hashtbl_der.filter t_class.method_table (fun _ mr ->
           List.length mr.args = List.length args))
      0 args g_ctx

  let make_list_of_elem el size =
    let rec helper acc curr =
      match curr with 0 -> acc | x -> helper (el :: acc) (x - 1) in
    helper [] size

  let inc_visibility_level ctx =
    {ctx with visibility_level= ctx.visibility_level + 1}

  let dec_visibility_level ctx =
    {ctx with visibility_level= ctx.visibility_level - 1}

  let delete_var_visibility : context -> context M.t =
   fun ctx ->
    let delete : table_key -> variable -> unit =
     fun key element ->
      if element.visibility_level = ctx.visibility_level then
        Hashtbl.remove ctx.variable_table key in
    Hashtbl.iter delete ctx.variable_table ;
    return ctx

  let rec interprete_stat : statement -> context -> context M.t =
   fun stat input_ctx ->
    match stat with
    | StatementBlock stat_list ->
        let rec eval_stat : statement list -> context -> context M.t =
         fun es_list e_ctx ->
          match es_list with
          | [] -> return e_ctx
          | st :: tail -> (
            match st with
            | (Break | Continue | Return _) when tail <> [] ->
                error "Unreachable code"
            | _ when e_ctx.count_of_cycle >= 1 && e_ctx.was_break ->
                return e_ctx
            | _ when e_ctx.count_of_cycle >= 1 && e_ctx.was_continue ->
                return e_ctx
            | _ when e_ctx.was_return -> return e_ctx
            | _ ->
                interprete_stat st e_ctx
                >>= fun head_ctx -> eval_stat tail head_ctx ) in
        eval_stat stat_list input_ctx
        >>= fun new_ctx ->
        if new_ctx.is_main then return new_ctx
        else delete_var_visibility new_ctx
    | While (smexpr, stat) -> (
        let was_main = input_ctx.is_main in
        let rec loop l_stat ctx =
          if ctx.was_break then
            match l_stat with
            (*decrement visibility level if StatementBlock*)
            | StatementBlock _ ->
                return
                  (dec_visibility_level
                     { ctx with
                       was_break= false
                     ; count_of_cycle= ctx.count_of_cycle - 1 })
            | _ ->
                return
                  { ctx with
                    was_break= false
                  ; count_of_cycle= ctx.count_of_cycle - 1 }
          else
            interprete_expr smexpr ctx
            >>= fun new_ctx ->
            match new_ctx.last_expr_result with
            | Some (VBool false) -> (
              match l_stat with
              | StatementBlock _ ->
                  return
                    (dec_visibility_level
                       { new_ctx with
                         count_of_cycle= ctx.count_of_cycle - 1
                       ; is_main= was_main })
                  (*additional flag for context *)
              | _ -> return {new_ctx with count_of_cycle= ctx.count_of_cycle - 1}
              )
            | Some (VBool true) ->
                interprete_stat l_stat new_ctx
                >>= fun g_ctx ->
                if g_ctx.was_return then return g_ctx
                else if g_ctx.was_continue then
                  loop l_stat {g_ctx with was_continue= false}
                else loop l_stat g_ctx
            | _ -> error "Incorrect expression type for while stametent" in
        match stat with
        | StatementBlock _ ->
            loop stat
              (inc_visibility_level
                 { input_ctx with
                   count_of_cycle= input_ctx.count_of_cycle + 1
                 ; is_main= false })
        | _ ->
            loop stat
              {input_ctx with count_of_cycle= input_ctx.count_of_cycle + 1} )
    | Break ->
        if input_ctx.count_of_cycle <= 0 then
          error "There is no loop to do break"
        else return {input_ctx with was_break= true}
    | Continue ->
        if input_ctx.count_of_cycle <= 0 then
          error "There is no loop to do continue"
        else return {input_ctx with was_continue= true}
    | Throw s_expr ->
        interprete_expr s_expr input_ctx >>= fun new_ctx -> return new_ctx
    | Print print_expr ->
        interprete_expr print_expr input_ctx
        >>= fun new_ctx ->
        let printer = function
          | VInt value -> return (printf "%d\n" value)
          | VBool value -> return (printf "%b\n" value)
          | VChar value -> return (printf "%c\n" value)
          | VString value -> return (printf "%s\n" value)
          | VClass value -> (
            match value with
            | ObjNull -> error "NullReferenceException"
            | ObjRef ob -> return (printf "%s" ob.class_key) )
          | VVoid -> error "Impossible to print void"
          | VNull -> error "Impossible to print null" in
        printer (Option.get new_ctx.last_expr_result)
        >> (* with ex ->
              let print_ex = Printexc.to_string ex in
              Printf.eprintf "There was an error: %s" print_ex ) ; *)
        return new_ctx
    | If (condit, body, else_body) -> (
        interprete_expr condit input_ctx
        >>= fun new_ctx ->
        let was_main = new_ctx.is_main in
        match new_ctx.last_expr_result with
        | Some (VBool true) -> (
          match body with
          | StatementBlock _ ->
              interprete_stat body
                (inc_visibility_level {new_ctx with is_main= false})
              >>= fun in_ctx ->
              return (dec_visibility_level {in_ctx with is_main= was_main})
          | _ -> interprete_stat body new_ctx )
        | Some (VBool false) -> (
          match else_body with
          | Some else_st -> (
            match else_st with
            | StatementBlock _ ->
                interprete_stat else_st
                  (inc_visibility_level {new_ctx with is_main= false})
                >>= fun in_ctx ->
                return (dec_visibility_level {in_ctx with is_main= was_main})
            | _ -> interprete_stat else_st new_ctx )
          | None -> return input_ctx )
        | _ -> error "Incorrect type for condition statement" )
    | For (dec_stat_o, expr_o, after_list, body) ->
        let was_main = input_ctx.is_main in
        ( match dec_stat_o with
        | None -> return (inc_visibility_level {input_ctx with is_main= false})
        | Some dec_stat ->
            interprete_stat dec_stat
              (inc_visibility_level {input_ctx with is_main= false}) )
        >>= fun new_ctx ->
        let rec loop body_stat af_list ctx =
          if ctx.was_break then
            delete_var_visibility
              { ctx with
                was_break= false
              ; count_of_cycle= ctx.count_of_cycle - 1
              ; visibility_level= ctx.visibility_level - 1
              ; is_main= was_main }
          else
            ( match expr_o with
            | None -> return {ctx with last_expr_result= Some (VBool true)}
            | Some expr_t -> interprete_expr expr_t ctx )
            >>= fun cond_ctx ->
            match cond_ctx.last_expr_result with
            (*проверка не прошла, значит с циклом все*)
            | Some (VBool false) ->
                delete_var_visibility
                  { cond_ctx with
                    count_of_cycle= cond_ctx.count_of_cycle - 1
                  ; visibility_level= cond_ctx.visibility_level - 1
                  ; is_main= was_main }
            | Some (VBool true) ->
                let rec inter_expr_list e_list as_ctx =
                  match e_list with
                  | [] -> return as_ctx
                  | x :: xs -> (
                    match x with
                    | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
                     |Assign (_, _)
                     |CallMethod (_, _)
                     |Access (_, CallMethod (_, _)) ->
                        interprete_expr x as_ctx
                        >>= fun z_ctx -> inter_expr_list xs z_ctx
                    | _ -> error "Incorrect expression for after body list" )
                in
                interprete_stat body_stat
                  {cond_ctx with visibility_level= new_ctx.visibility_level + 1}
                >>= fun body_ctx ->
                if body_ctx.was_return then
                  return {body_ctx with is_main= was_main}
                else if body_ctx.was_continue then
                  loop body_stat af_list {body_ctx with was_continue= false}
                else
                  inter_expr_list af_list body_ctx
                  >>= fun after_ctx -> loop body_stat af_list after_ctx
            | _ -> error "Incorrect condition type in for statement" in
        loop body after_list new_ctx
    | Return result_op -> (
      match result_op with
      | None when input_ctx.current_meth_type = Void ->
          return {input_ctx with last_expr_result= Some VVoid; was_return= true}
      | None -> error "Return value error"
      | Some result ->
          expression_check result input_ctx
          >>= fun ret_type ->
          if ret_type <> input_ctx.current_meth_type then
            error "Return value error"
          else
            interprete_expr result input_ctx
            >>= fun new_ctx -> return {new_ctx with was_return= true} )
    | Expression s_expr -> (
      match s_expr with
      | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
       |CallMethod (_, _)
       |Access (_, CallMethod (_, _))
       |Assign (_, _) ->
          interprete_expr s_expr input_ctx >>= fun new_ctx -> return new_ctx
      | _ -> error "Incorrect expression for statement" )
    | VarDeclare (modifier, vars_type, var_list) ->
        let is_const : modifier option -> bool = function
          | Some Const -> true
          | _ -> false in
        let rec var_declarator v_list var_ctx =
          match v_list with
          | [] -> return var_ctx
          | (var_name, var_expr_o) :: tail -> (
            match var_ctx.current_o with
            | ObjNull -> error "Impossible to execute in null object"
            | ObjRef {class_key= _; class_table= table; number= _} ->
                ( if
                  Hashtbl.mem var_ctx.variable_table var_name
                  || Hashtbl.mem table var_name
                then error "Variable with this name is already defined"
                else
                  match var_expr_o with
                  | None ->
                      Hashtbl.add var_ctx.variable_table var_name
                        { var_key= var_name
                        ; var_type= vars_type
                        ; var_value= get_default_value vars_type
                        ; is_mutable= is_const modifier
                        ; assignment_count= 0
                        ; visibility_level= var_ctx.visibility_level } ;
                      return var_ctx
                  | Some var_expr_e -> (
                      expression_check var_expr_e var_ctx
                      >>= fun var_expr_type ->
                      let add_helper new_var =
                        interprete_expr new_var var_ctx
                        >>= fun ctx_aft_ad ->
                        Hashtbl.add ctx_aft_ad.variable_table var_name
                          { var_key= var_name
                          ; var_type= var_expr_type
                          ; var_value= Option.get ctx_aft_ad.last_expr_result
                          ; is_mutable= is_const modifier
                          ; assignment_count= 1
                          ; visibility_level= ctx_aft_ad.visibility_level } ;
                        return ctx_aft_ad in
                      match var_expr_type with
                      | CsClass "null" -> (
                        match vars_type with
                        | CsClass _ -> add_helper var_expr_e
                        | _ ->
                            error
                              "Incorrect assign type in variable declaration" )
                      | CsClass class_right -> (
                        match vars_type with
                        | CsClass class_left ->
                            assign_verify_polymorphic class_left class_right
                            >>= fun _ -> add_helper var_expr_e
                        | _ ->
                            error
                              "Incorrect assign type in variable declaration" )
                      | _ when var_expr_type = vars_type ->
                          add_helper var_expr_e
                      | _ ->
                          error
                            ( "Incorrect value type for declared variable:"
                            ^ show_data_type var_expr_type ) ) )
                >>= fun head_ctx -> var_declarator tail head_ctx ) in
        var_declarator var_list input_ctx
    | _ -> raise Not_found

  and interprete_expr : expr -> context -> context M.t =
   fun in_expr in_ctx ->
    let rec eval_expr e_expr ctx =
      let eval_bin left_e right_e operator =
        interprete_expr left_e ctx
        >>= fun left_ctx ->
        interprete_expr right_e left_ctx
        >>= fun right_ctx ->
        let get_left_value = Option.get left_ctx.last_expr_result in
        let get_right_value = Option.get right_ctx.last_expr_result in
        let cal_value = operator get_left_value get_right_value in
        try return {right_ctx with last_expr_result= Some cal_value} with
        | Invalid_argument m -> error m
        | Division_by_zero -> error "Division by zero!" in
      let eval_unar ex_operand operator =
        interprete_expr ex_operand ctx
        >>= fun new_ctx ->
        let get_value = Option.get new_ctx.last_expr_result in
        let cal_unar_v = operator get_value in
        try return {new_ctx with last_expr_result= Some cal_unar_v}
        with Invalid_argument m -> error m in
      match e_expr with
      | Add (left, right) -> eval_bin left right ( ++ )
      | Sub (left, right) -> eval_bin left right ( -- )
      | Mul (left, right) -> eval_bin left right ( ** )
      | Div (left, right) -> eval_bin left right ( // )
      | Mod (left, right) -> eval_bin left right ( %% )
      | And (left, right) -> eval_bin left right ( &&& )
      | Or (left, right) -> eval_bin left right ( ||| )
      | Not not_exp -> eval_unar not_exp not_op
      | Less (left, right) -> eval_bin left right ( <<< )
      | More (left, right) -> eval_bin left right ( >>> )
      | LessOrEqual (left, right) -> eval_bin left right ( <<== )
      | MoreOrEqual (left, right) -> eval_bin left right ( >>== )
      | Equal (left, right) -> eval_bin left right ( === )
      | NotEqual (left, right) -> eval_bin left right ( !=! )
      | ConstExpr value -> return {ctx with last_expr_result= Some value}
      | IdentVar var_id -> (
        match get_value_option ctx.variable_table var_id with
        | Some id -> return {ctx with last_expr_result= Some id.var_value}
        | None -> (
          try
            get_obj_info ctx.current_o
            |> fun (_, table, _) ->
            match get_value_option table var_id with
            | Some field ->
                return {ctx with last_expr_result= Some field.f_value}
            | None -> error "Field not found"
          with Failure m | Invalid_argument m -> error m ) )
      | Null -> return {ctx with last_expr_result= Some (VClass ObjNull)}
      | _ -> raise Not_found in
    raise Not_found
end
