type data_type = Int | CsClass of string | Void | String [@@deriving show]

type value =
  | VInt of int
  | VBool of bool
  | VVoid
  | VNull
  | VChar of char
  | VString of string
  | VClass
[@@deriving show]

type modifier = Public | Static | Override | Const [@@deriving show]

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | PostInc of expr (*x++*)
  | PostDec of expr (*x--*)
  | PrefInc of expr (*++x*)
  | PrefDec of expr (*--x*)
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Less of expr * expr
  | More of expr * expr
  | LessOrEqual of expr * expr
  | MoreOrEqual of expr * expr
  | Null
  | Access of expr * expr
  | ConstExpr of value
  | IdentObj of string
  | ClassCreate of string * expr list (*first - name, other - args list*)
  | CallMethod of string * expr list
  | Assign of expr * expr
[@@deriving show]

and statement =
  | For of statement option * expr option * expr list * statement
  | If of expr * statement * statement option (*statement option is "else"*)
  | While of expr * statement
  | Break
  | Continue
  | Return of expr option
  | VarDeclare of data_type * (expr * expr option) list
  | Expression of expr
  | Throw of expr
  | StatementBlock of statement list
  | Try of
      statement
      * ((data_type * expr option) option * expr option * statement) list
      * statement option (*WARN!*)
  | Print of expr
[@@deriving show]

and field =
  | VariableField of modifier list * data_type * (string * expr option) list (*example: static int a = 3, b*)
  | Method of
      modifier list
      * data_type
      * string
      * (data_type * string) list
      * statement option
  | Constructor of
      modifier list * string * (data_type * string) list * statement
[@@deriving show]

and csClass =
  | Class of
      modifier list
      * string (*name*)
      * string option
      (*parent class*)
      * field list
[@@deriving show]
