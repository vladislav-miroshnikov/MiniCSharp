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
        when is_static l && is_public l
             && (not (is_const l))
             && not (is_override l) ->
          return ()
      | Method (_, "Main", _, _) ->
          error "Only one main method can be in program!"
      | Method (_, _, _, _) when is_static l -> error "Method can not be static"
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

  let add_to_table hashtable key value message =
    match get_value_option hashtable key with
    | None ->
        Hashtbl.add hashtable key value ;
        return hashtable
    | _ -> error message

  let type_of_list = List.map fst

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
                    let is_mutable = is_const mod_list in
                    add_to_table field_table field_key
                      {field_type; field_key; is_mutable; sub_tree}
                      "Similar fields"
                    >> add_var_field ps in
              field_modifiers_check field_elem >> add_var_field arg_list
          | mod_list, Method (method_type, m_name, args, body) ->
              let method_key =
                String.concat ""
                  (m_name :: List.map show_data_type (type_of_list args)) in
              let has_override = is_override mod_list in
              field_modifiers_check field_elem
              >> add_to_table method_table method_key
                   {method_type; has_override; method_key; args; body}
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
             "Similar Classes"

  let add_default_constructor hashtable =
    Hashtbl.iter
      (fun key some_class ->
        if Hashtbl.length some_class.constructor_table = 0 then
          Hashtbl.add some_class.constructor_table key
            {args= []; body= StatementBlock []})
      hashtable ;
    return hashtable

  let class_adding class_list hastable =
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

  let exception_transfer_field : table_class -> table_field -> unit t =
   fun child_class p_field ->
    match get_value_option child_class.field_table p_field.field_key with
    | None ->
        return (Hashtbl.add child_class.field_table p_field.field_key p_field)
    | _ -> return ()

  let transfer_fields parent children =
    monadic_list_iter
      (convert_table_to_list parent.field_table)
      (exception_transfer_field children)
      ()

  let exception_transfer_method : table_class -> table_method -> unit t =
   fun child_class p_method ->
    match get_value_option child_class.method_table p_method.method_key with
    | None ->
        return
          (Hashtbl.add child_class.method_table p_method.method_key p_method)
    | _ -> return ()

  let transfer_methods parent children =
    monadic_list_iter
      (convert_table_to_list parent.method_table)
      (exception_transfer_method children)
      ()
end
