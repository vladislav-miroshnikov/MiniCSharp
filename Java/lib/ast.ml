type modifier = Public | Static | Final | Abstract [@@deriving show]

type type_t =
  | JInt
  | JVoid
  | JClassName of string
  | JString
  | JArray of type_t
  | JObject
[@@deriving show]

type value =
  | JVBool of bool
  | JVInt of int
  | JVNull
  | JVChar of char
  | JVArray of value list
  | JVVoid
  | JVString of string
  | JVClassName
  | JVObject
[@@deriving show]

(* type jException = {jName: type_t ; message : string} 

type jVariable = {vModifiers : modifier list; isMutable : bool; varType : type_t; varName : string; value : value option}  *)

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
  | ClassCreate of string * expr list (*new clName(argList)*)
  | ArrayCreate of type_t * expr option (*new arrType[cntExpr]*)
  | CallMethod of expr * expr list
  | Identifier of string
  | Const of value
  | This
  | Super
  | Null
  | FieldAccess of expr * expr
  | ArrayAccess of expr * expr (*arr_name * index*)
  | Assign of expr * expr
[@@deriving show]

and stmt =
  | If of expr * stmt * stmt option (*cond * thenStat * elseStat*)
  | While of expr * stmt (* cond * body *)
  | For of stmt option * expr option * expr list * stmt (* varDec * expr * afterBody * body *)
  | Break
  | Continue
  | Return of expr option (* result *)
  | StatBlock of stmt list
  | VarDec of type_t * (expr * expr option) list
  | Expression of expr
  | Throw of expr
[@@deriving show]

and field =
  | Method of
      modifier list
      * type_t
      * expr
      * (type_t * expr) list
      * (*List of pairs (type, identificator)*)
      stmt option (*Statement block*)
  | VarField of modifier list * type_t * (expr * expr option) list
  | Constructor of modifier list * expr * (type_t * expr) list * stmt
[@@deriving show]

and class_dec =
  | Class of
      modifier list
      * expr (*Identifier class name*)
      * expr option
      (*Parent class_name*)
      * field list
(* class body *) [@@deriving show]

(* type jMethod = {mModifiers: modifier list; mName : string; mRetType : type_t; mArgs : expr list; mBody : stmt list;}  

type jClass = {cModifiers: modifier list; cName : string; cFields : jVariable list; cMethods : jMethod list; cClasses : jClass list; cParent : jClass option}   *)
