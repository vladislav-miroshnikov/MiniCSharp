open Ast

let get_obj_value = function VClass o -> o | _ -> ObjNull

let get_default_value = function
  | Int -> VInt 0
  | String -> VString ""
  | CsClass _ -> VClass ObjNull
  | Bool -> VBool false
  | Void -> VVoid

let get_obj_num = function
  | ObjNull -> raise (Invalid_argument "NullPointerException")
  | ObjRef {class_key= _; class_table= _; number= n} -> n

let get_obj_info = function
  | ObjNull -> raise (Invalid_argument "NullPointerException")
  | ObjRef {class_key= key; class_table= table; number= n} -> (key, table, n)
