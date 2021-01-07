type modifier = Public | Static | Final | Abstract | Override
[@@deriving show { with_path = false }]

type type_t =
  | Int
  | Void
  | ClassName of string
  | String
  | Array of type_t
  | Bool
[@@deriving show { with_path = false }]

type value =
  | VBool of bool
  | VInt of int
  | VArray of array_ref
  | VVoid
  | VString of string
  | VObjectRef of obj_ref
[@@deriving show { with_path = false }]

(* Поле внутри созданного объекта *)
and field_ref = {
  key : string;
  f_type : type_t;
  f_value : value;
  is_not_mutable : bool;
  assignment_count : int;
}

(* Ссылка на объект - может быть null или запись именем класса (он же ключ) и ссылкой на поля *)
and obj_ref =
  | RNull
  | RObj of {
      class_key : string;
      field_ref_table : (string, field_ref) Hashtbl_p.t;
      number : int;
    }

and array_ref =
  | ANull
  | Arr of { a_type : type_t; values : value list; number : int }

let get_arr_info_exn = function
  | ANull -> raise (Invalid_argument "NullPointerException")
  | Arr { a_type = at; values = av; number = an } -> (at, av, an)

let get_obj_fields_exn = function
  | RNull -> raise (Invalid_argument "NullPointerException")
  | RObj { class_key = _; field_ref_table = frt; number = _ } -> frt

let get_obj_number_exn = function
  | RNull -> raise (Invalid_argument "NullPointerException")
  | RObj { class_key = _; field_ref_table = _; number = n } -> n

let get_obj_info_exn = function
  | RNull -> raise (Invalid_argument "NullPointerException")
  | RObj { class_key = k; field_ref_table = t; number = n } -> (k, t, n)

let get_type_by_value = function
  | VInt _ -> Int
  | VBool _ -> Bool
  | VString _ -> String
  | VVoid -> Void
  | VObjectRef RNull -> ClassName "null"
  | VObjectRef (RObj { class_key = ck; field_ref_table = _; number = _ }) ->
      ClassName ck
  | VArray (Arr { a_type = t; values = _; number = _ }) -> Array t
  | VArray ANull -> Array Void

let update_array_val_exn v_arr index new_val =
  match v_arr with
  | VArray (Arr { a_type = at; values = v_list; number = an }) ->
      let update_list_on_pos pos list new_v =
        List.mapi (fun i old_v -> if i = pos then new_v else old_v) list
      in
      let check_value_type a_type new_val =
        a_type = get_type_by_value new_val
      in
      if check_value_type at new_val then
        update_list_on_pos index v_list new_val |> fun new_list ->
        VArray (Arr { a_type = at; values = new_list; number = an })
        (* То самое исключение, из-за которого Java/C# - отстой *)
      else raise (Failure "ArrayStoreException")
  | _ -> raise (Invalid_argument "Wrong value for array update!")

let ( ++ ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VInt (x + y)
  | VString x, VString y -> VString (x ^ y)
  | VInt x, VString y -> VString (string_of_int x ^ y)
  | VString x, VInt y -> VString (x ^ string_of_int y)
  | _, _ -> raise (Invalid_argument "Wrong argument types for adding!")

let ( -- ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VInt (x - y)
  | _, _ -> raise (Invalid_argument "Wrong argument types for subtraction!")

let ( ** ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VInt (x * y)
  | _, _ -> raise (Invalid_argument "Wrong argument types for multiplication!")

let ( // ) v1 v2 =
  match (v1, v2) with
  | VInt _, VInt y when y = 0 -> raise Division_by_zero
  | VInt x, VInt y -> VInt (x / y)
  | _, _ -> raise (Invalid_argument "Wrong argument types for division!")

let ( %% ) v1 v2 =
  match (v1, v2) with
  | VInt _, VInt y when y = 0 -> raise Division_by_zero
  | VInt x, VInt y -> VInt (x mod y)
  | _, _ -> raise (Invalid_argument "Wrong argument types for mod operator!")

let ( >>> ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VBool (x > y)
  | _ -> raise (Invalid_argument "Wrong type for ordering!")

let ( <<< ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VBool (x < y)
  | _ -> raise (Invalid_argument "Wrong type for ordering!")

let ( <<== ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VBool (x <= y)
  | _ -> raise (Invalid_argument "Wrong type for ordering!")

let ( >>== ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VBool (x >= y)
  | _ -> raise (Invalid_argument "Wrong type for ordering!")

let ( &&& ) v1 v2 =
  match (v1, v2) with
  | VBool x, VBool y -> VBool (x && y)
  | _, _ -> raise (Invalid_argument "Wrong types for && operator!")

let ( ||| ) v1 v2 =
  match (v1, v2) with
  | VBool x, VBool y -> VBool (x || y)
  | _, _ -> raise (Invalid_argument "Wrong types for || operator!")

let not_v = function
  | VBool x -> VBool (not x)
  | _ -> raise (Invalid_argument "Wrong types for NOT operator!")

let ( === ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VBool (x = y)
  | VBool x, VBool y -> VBool (x = y)
  | VVoid, VVoid -> VBool true
  | VString s, VString t -> VBool (s = t)
  | VObjectRef x, VObjectRef y -> (
      match (x, y) with
      | RNull, RNull -> VBool true
      | RNull, _ | _, RNull -> VBool false
      | ( RObj { class_key = _; field_ref_table = _; number = xn },
          RObj { class_key = _; field_ref_table = _; number = yn } ) ->
          VBool (xn = yn) )
  | VArray x, VArray y -> VBool (x = y)
  | _ -> raise (Invalid_argument "Wrong types for equality!")

let ( !=! ) v1 v2 = not_v (v1 === v2)

let get_init_value_of_type = function
  | Int -> VInt 0
  | String -> VString ""
  | ClassName _ -> VObjectRef RNull
  | Bool -> VBool false
  | Void -> VVoid
  | Array _ -> VArray ANull

type name = Name of string [@@deriving show { with_path = false }]

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | PrefInc of expr
  | PrefDec of expr
  | PostInc of expr
  | PostDec of expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Less of expr * expr
  | More of expr * expr
  | LessOrEqual of expr * expr
  | MoreOrEqual of expr * expr
  | ClassCreate of name * expr list (*new clName(argList)*)
  | ArrayCreateSized of type_t * expr (*new arrType[cntExpr]*)
  | ArrayCreateElements of type_t * expr list (*new arrType[] {expr, ... , expr}*)
  | CallMethod of expr * expr list (*this(...), super(...) ident(...)*)
  | Identifier of string
  | Const of value
  | This
  | Super
  | Null
  | FieldAccess of expr * expr
  | ArrayAccess of expr * expr (*arr_name[index]*)
  | Assign of expr * expr
[@@deriving show { with_path = false }]

and stmt =
  | If of expr * stmt * stmt option (*cond * thenStat * elseStat*)
  | While of expr * stmt (* cond * body *)
  | For of stmt option * expr option * expr list * stmt (* varDec * expr * afterBody * body *)
  | Break
  | Continue
  | Return of expr option (* result *)
  | StmtBlock of stmt list
  | VarDec of modifier option * type_t * (name * expr option) list
  | Expression of expr
[@@deriving show { with_path = false }]

and field =
  | Method of
      type_t
      * name
      * (type_t * name) list
      (*List of pairs (type, identificator)*)
      * stmt option (*Statement block*)
  | VarField of type_t * (name * expr option) list
  | Constructor of name * (type_t * name) list * stmt
[@@deriving show { with_path = false }]

and class_dec =
  | Class of
      modifier list
      * name (*class name*)
      * name option
      (*Parent class_name*)
      * (modifier list * field) list
(* class body *) [@@deriving show { with_path = false }]

let get_field_list = function Class (_, _, _, f_list) -> List.map snd f_list

let convert_elem_pair_list = function
  | t, p_list -> List.map (fun p -> match p with s, f -> (t, s, f)) p_list

let get_var_field_pairs_list_typed cd =
  List.concat
    (List.map convert_elem_pair_list
       (List.filter_map
          (fun f ->
            match f with
            | VarField (t, pair_list) -> Some (t, pair_list)
            | _ -> None)
          (get_field_list cd)))
