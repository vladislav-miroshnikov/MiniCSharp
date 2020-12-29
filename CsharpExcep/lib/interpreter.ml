open Hashtbl
open Ast
open Parser

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

type table_constructor = {args: (data_type * string) list; body: statement}
[@@deriving show {with_path= false}]

type table_field =
  { field_type: data_type
  ; key: table_key
  ; is_mutable: bool
  ; sub_tree: expr option }
[@@deriving show {with_path= false}]

type table_method =
  { method_type: data_type
  ; is_override: bool
  ; method_key: table_key
  ; args: (data_type * string) list
  ; body: statement }
[@@deriving show {with_path= false}]

type table_class =
  { class_key: table_key
  ; field_table: (table_key, table_field) Hashtbl.t
  ; method_table: (table_key, table_method) Hashtbl.t
  ; constructor_table: (table_key, table_constructor) Hashtbl.t
  ; parent_key: table_key option }

let class_table : (table_key, table_class) Hashtbl.t = Hashtbl.create 1024
let convert_table_to_seq = Hashtbl.to_seq_values

(*Hashtbl.fold (fun _ v acc -> v :: acc) hashtable []*)

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
      ; is_override= true
      ; method_key= "ToString"
      ; args= []
      ; body=
          Option.get
            (apply_parser Stat.stat_block (*чекнуть функцию!!!!!*)
               {| 
              {
                Console.WriteLine();  
                Console.WriteLine(Message);
                Console.WriteLine(StackTrace);
              }


          |})
      } in
    let message : table_field =
      {field_type= String; key= "Message"; is_mutable= true; sub_tree= None}
    in
    let stack_trace : table_field =
      {field_type= String; key= "StackTrace"; is_mutable= true; sub_tree= None}
    in
    Hashtbl.add method_table "ToString" to_string ;
    Hashtbl.add field_table "Message" message ;
    Hashtbl.add field_table "StackTrace" stack_trace ;
    Hashtbl.add hashtable "Exception"
      { class_key= "Exception"
      ; field_table
      ; method_table
      ; constructor_table
      ; parent_key= None } ;
    return hashtable
end
