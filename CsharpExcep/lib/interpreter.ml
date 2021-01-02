open Hashtbl
open Ast
open Parser
open Stack

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
      (* | Method (_, _, _, _) when is_static l ->
          error "Method can not be static" CHECK!!!!! *)
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
    ; main_ctx: context option
    ; count_of_obj: int }

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
      ; main_ctx= None
      ; count_of_obj= 0 }

  let get_main_ctx ctx =
    match ctx.main_ctx with None -> ctx | Some main_ctx -> main_ctx

  let get_obj_num = function
    | ObjNull -> error "NullPointerException"
    | ObjRef {class_key= _; class_table= _; number= n} -> return n

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
            (*constructor_verify t_class args ctx >>*)
            return (CsClass class_name) ) )
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
    | _ -> raise Not_found

  and method_verify :
      table_class -> table_key -> expr list -> context -> table_method M.t =
   fun t_class method_name args g_ctx ->
    let check_argument_type :
        int -> data_type -> table_key -> table_method -> bool =
     fun position curr_type _ value ->
      match List.nth_opt value.args position with
      | None -> false
      | Some (t_type, _) -> (
        match curr_type with
        | CsClass "null" -> (
          match t_type with CsClass _ -> true | _ -> false )
        | _ -> t_type = curr_type ) in
    let rec helper hashtable position expr_list ctx acc =
      match Hashtbl.length hashtable with
      | 0 -> error "The required method was not found"
      | other -> (
        match expr_list with
        | [] -> (
          match other with
          | 1 -> (
              let m_key = method_name ^ acc in
              match get_value_option t_class.method_table m_key with
              | None -> error "The required method was not found"
              | Some t_method -> return t_method )
          | _ -> error "Couldn't recognize the method" )
        | el :: ot ->
            expression_check el ctx
            >>= fun el_type ->
            helper
              (Hashtbl_der.filter hashtable
                 (check_argument_type position el_type))
              (position + 1) ot ctx
              (acc ^ show_data_type el_type) ) in
    helper
      (Hashtbl_der.filter t_class.method_table (fun _ t_method ->
           List.length t_method.args = List.length args))
      0 args g_ctx ""

  and assign_verify left_key right_key =
    (*For subtype polymorphism (inclusion polymorphism)*)
    let rec check_parent key =
      let find_class = Option.get (get_value_option class_table key) in
      if find_class.class_key = left_key then return (CsClass right_key)
      else
        match find_class.parent_key with
        | None -> error "Incorrect assign type"
        | Some parent_key -> check_parent parent_key in
    check_parent right_key
end
