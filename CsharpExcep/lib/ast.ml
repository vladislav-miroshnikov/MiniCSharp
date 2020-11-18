type data_type = CsInt | CsVoid | CsString | CsClass of string
[@@deriving show]

type value =
  | CsVInt of int
  | CsVBool of bool
  | CsVVoid
  | CsVNull
  | CsVChar of char
  | CsVString of string
  | CsVClass
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
  | Const of value
  | IdentObj of string
  | ClassCreate of string * expr list (*first - name, other - args list*)
  | CallMethod of expr * expr list
  | Assign of expr * expr
[@@deriving show]

and statement =
  | For of statement option * expr option * expr list * statement
  | If of expr * statement * statement option (*statement option is "else"*)
  | While of expr * statement (*option?*)
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
[@@deriving show]

and field =
  | VariableField of modifier list * data_type * (expr * expr option) list (*example: static int a = 3, b*)
  | Method of
      modifier list
      * data_type
      * expr
      * (data_type * expr) list
      * statement option
  | Constructor of modifier list * expr * (data_type * expr) list * statement
[@@deriving show]

and csClass =
  | Class of
      modifier list * expr (*name*) * expr option (*parent class*) * field list
[@@deriving show]

type csPrint = PrintF of expr [@@deriving show]
