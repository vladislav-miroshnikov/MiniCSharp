module AST = struct
  type csType = CsInt | CsBool | CsVoid | CsChar | CsString | CsRef of string

  type csValue =
    | CsVInt of int
    | CsVBool of bool
    | CsVVoid
    | CsVNull
    | CsVChar of char
    | CsVString of string (*?*)
    | CsVRef

  type csModifier = Public | Static | Override | Const

  type csVariable = {
    varModifiers : csModifier list;
    varType : csType;
    varName : string;
    varValue : csValue option;
  }

  type csException = { excep : csType; excepMessage : string }

  type csExpr =
    | DeclareExpr of csDeclareExpr
    | NumExpr of csNumExpr
    | LogicExpr of csLogicExpr
    | CompareExpr of csCompareExpr
    | CallMethod of csExpr * csExpr list
    | Null
    | ConstExp of csValue
    | Variable of string
    | Access of csExpr * csExpr

  and csNumExpr =
    | Add of csExpr * csExpr
    | Sub of csExpr * csExpr
    | Mul of csExpr * csExpr
    | Div of csExpr * csExpr
    | Mod of csExpr * csExpr
    | PostInc of csExpr
    | PostDec of csExpr
    | PrefInc of csExpr
    | PrefDec of csExpr

  and csDeclareExpr =
    | ClassDeclare of { className : string; classArgs : csExpr list }

  and csLogicExpr =
    | And of csExpr * csExpr
    | Or of csExpr * csExpr
    | Not of csExpr

  and csCompareExpr =
    | Equal of csExpr * csExpr
    | NotEqual of csExpr * csExpr
    | Less of csExpr * csExpr
    | More of csExpr * csExpr
    | LessOrEqual of csExpr * csExpr
    | MoreOrEqual of csExpr * csExpr

  and csStatement =
    | Expr of csExpr
    | Break
    | Continue
    | If of {
        condition : csExpr;
        thenStatement : csStatement list;
        elseStatement : csStatement list;
      }
    | While of { condition : csExpr; body : csStatement list }
    | For of {
        varDec : csStatement;
        condition : csExpr;
        afterBody : csStatement list;
        body : csStatement list;
      }
    | Return of { result : csExpr }
    | VarDec of { vType : csType; vName : string; vValue : csValue option }
    | Throw of { exc : csException }
    | Try of {
        (*?*)
        catches : csStatement list;
        filters : csStatement list;
        finally : csStatement option;
      }
    | Catch of { excepName : csException option }
    | Filter of { excepName : csException; condition : csExpr }
    | Finally

  type csMethod = {
    mModifiers : csModifier list;
    mType : csType;
    mName : string;
    mArgs : csExpr list;
    mBody : csStatement list;
  }

  type csClass = {
    cModifiers : csModifier list;
    cName : string;
    cFields : csVariable list;
    cMethods : csMethod list;
    cClasses : csClass list;
  }

  type csPrint = PrintF of { exp : csExpr }

  (*?*)
end
