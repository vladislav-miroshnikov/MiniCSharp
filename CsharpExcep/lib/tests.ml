open Parser
open Parser.Expr
open Parser.Stat
open Ast

let%test _ =
  apply_parser get_modifier_list "public static const"
  = Some [Public; Static; Const]

let%test _ = apply_parser convert_to_int "4" = Some 4
let%test _ = apply_parser null "  null" = Some Null
let%test _ = apply_parser ident_obj "   car" = Some "car"
let%test _ = apply_parser ident_obj "  Cat1" = Some "Cat1"
let%test _ = apply_parser get_variable "   Cat" = Some (IdentObj "Cat")

let%test _ =
  apply_parser parse_string "\"Parse\"" = Some (ConstExpr (VString "Parse"))

let%test _ = apply_parser atomic "      123123" = Some (ConstExpr (VInt 123123))
let%test _ = apply_parser ident_obj "  123Ret" = None

let%test _ =
  apply_parser atomic "\"Parse\"" = Some (ConstExpr (VString "Parse"))

let%test _ = apply_parser atomic "   true" = Some (ConstExpr (VBool true))
let%test _ = apply_parser atomic "   false" = Some (ConstExpr (VBool false))
let%test _ = apply_parser atomic "   null" = Some Null
let%test _ = apply_parser define_type "  int" = Some Int
let%test _ = apply_parser define_type "  int[]" = None
let%test _ = apply_parser define_type "  void" = Some Void
let%test _ = apply_parser define_type "  string" = Some String
let%test _ = apply_parser define_type "  Excep" = Some (CsClass "Excep")

(* ------------------------  EXPRESSIONS -------------------------- *)
let%test _ =
  apply_parser expr "a = b = 1"
  = Some (Assign (IdentObj "a", Assign (IdentObj "b", ConstExpr (VInt 1))))

let%test _ =
  apply_parser expr "4 + 5"
  = Some (Add (ConstExpr (VInt 4), ConstExpr (VInt 5)))

let%test _ =
  apply_parser expr "2/5 + 3 * (5 % 3)"
  = Some
      (Add
         ( Div (ConstExpr (VInt 2), ConstExpr (VInt 5))
         , Mul (ConstExpr (VInt 3), Mod (ConstExpr (VInt 5), ConstExpr (VInt 3)))
         ))

let%test _ =
  apply_parser expr "x = true"
  = Some (Assign (IdentObj "x", ConstExpr (VBool true)))

let%test _ =
  apply_parser expr "a.b.c"
  = Some (Access (Access (IdentObj "a", IdentObj "b"), IdentObj "c"))

let%test _ =
  apply_parser expr "obj.Sum(5, arg2, arg3 * 3)"
  = Some
      (Access
         ( IdentObj "obj"
         , CallMethod
             ( IdentObj "Sum"
             , [ ConstExpr (VInt 5); IdentObj "arg2"
               ; Mul (IdentObj "arg3", ConstExpr (VInt 3)) ] ) ))

let%test _ =
  apply_parser expr "Sum(obj.a, 3)"
  = Some
      (CallMethod
         ( IdentObj "Sum"
         , [Access (IdentObj "obj", IdentObj "a"); ConstExpr (VInt 3)] ))

let%test _ =
  apply_parser expr "new Shop(5,\"MVideo\")"
  = Some
      (ClassCreate
         (IdentObj "Shop", [ConstExpr (VInt 5); ConstExpr (VString "MVideo")]))

let%test _ =
  apply_parser expr "Fork(new Child())"
  = Some (CallMethod (IdentObj "Fork", [ClassCreate (IdentObj "Child", [])]))

(* -----------------------  STATEMENTS ------------------------*)

let%test _ =
  apply_parser parse_statement "int a = 0, b = 1, c = 2;"
  = Some
      (VarDeclare
         ( Int
         , [ (IdentObj "a", Some (ConstExpr (VInt 0)))
           ; (IdentObj "b", Some (ConstExpr (VInt 1)))
           ; (IdentObj "c", Some (ConstExpr (VInt 2))) ] ))

let%test _ = apply_parser parse_break "break;" = Some Break
let%test _ = apply_parser parse_continue "continue;" = Some Continue

let%test _ =
  apply_parser parse_return "return 3;"
  = Some (Return (Some (ConstExpr (VInt 3))))

let%test _ =
  apply_parser parse_statement {|  if(num1 > num2)
{
}   |}
  = Some (If (More (IdentObj "num1", IdentObj "num2"), StatementBlock [], None))

let%test _ =
  apply_parser parse_statement
    {|  if(num1 > num2)
{        Console.WriteLine("help");
}   |}
  = Some
      (If
         ( More (IdentObj "num1", IdentObj "num2")
         , StatementBlock [Print (ConstExpr (VString "help"))]
         , None ))

let%test _ =
  apply_parser parse_statement
    {|  if(num1 > num2)
{
    Console.WriteLine(a.Sum());
}
else if (num1 < num2)
{
    Console.WriteLine(a.b);
}
else
{
    Console.WriteLine("2");
} |}
  = Some
      (If
         ( More (IdentObj "num1", IdentObj "num2")
         , StatementBlock
             [Print (Access (IdentObj "a", CallMethod (IdentObj "Sum", [])))]
         , Some
             (If
                ( Less (IdentObj "num1", IdentObj "num2")
                , StatementBlock [Print (Access (IdentObj "a", IdentObj "b"))]
                , Some (StatementBlock [Print (ConstExpr (VString "2"))]) )) ))

let%test _ =
  apply_parser parse_statement
    {| while (i > 0)
{
    Console.WriteLine(i);
    i--;
} |}
  = Some
      (While
         ( More (IdentObj "i", ConstExpr (VInt 0))
         , StatementBlock
             [Print (IdentObj "i"); Expression (PostDec (IdentObj "i"))] ))

let%test _ =
  apply_parser parse_statement
    {| for (int i = 0; i < 9; i++)
{
    Console.WriteLine(1);
}|}
  = Some
      (For
         ( Some (VarDeclare (Int, [(IdentObj "i", Some (ConstExpr (VInt 0)))]))
         , Some (Less (IdentObj "i", ConstExpr (VInt 9)))
         , [PostInc (IdentObj "i")]
         , StatementBlock [Print (ConstExpr (VInt 1))] ))

let%test _ =
  apply_parser parse_statement
    {| { if(a<b) {}
for (; ;)
{
    Console.WriteLine("1");
} }
|}
  = Some
      (StatementBlock
         [ If (Less (IdentObj "a", IdentObj "b"), StatementBlock [], None)
         ; For (None, None, [], StatementBlock [Print (ConstExpr (VString "1"))])
         ])

let%test _ =
  apply_parser parse_statement
    {|  for (; i<9;)
{
    Console.WriteLine(i*i);
} |}
  = Some
      (For
         ( None
         , Some (Less (IdentObj "i", ConstExpr (VInt 9)))
         , []
         , StatementBlock [Print (Mul (IdentObj "i", IdentObj "i"))] ))

let%test _ =
  apply_parser parse_statement
    {|  for (int i = 0; i < 9; i++)
{
    if (i == 5)
        break;
    Console.WriteLine(i);
}|}
  = Some
      (For
         ( Some (VarDeclare (Int, [(IdentObj "i", Some (ConstExpr (VInt 0)))]))
         , Some (Less (IdentObj "i", ConstExpr (VInt 9)))
         , [PostInc (IdentObj "i")]
         , StatementBlock
             [ If (Equal (IdentObj "i", ConstExpr (VInt 5)), Break, None)
             ; Print (IdentObj "i") ] ))

let%test _ =
  apply_parser parse_statement
    {| try
        {
            int x = 5;
            int y = x / 0;
            Console.WriteLine(y);
        }
        catch
        {
            Console.WriteLine("excep");
        }
        finally
        {
            Console.WriteLine("finally");
        }|}
  = Some
      (Try
         ( StatementBlock
             [ VarDeclare (Int, [(IdentObj "x", Some (ConstExpr (VInt 5)))])
             ; VarDeclare
                 ( Int
                 , [ ( IdentObj "y"
                     , Some (Div (IdentObj "x", ConstExpr (VInt 0))) ) ] )
             ; Print (IdentObj "y") ]
         , [(None, None, StatementBlock [Print (ConstExpr (VString "excep"))])]
         , Some (StatementBlock [Print (ConstExpr (VString "finally"))]) ))

let%test _ =
  apply_parser parse_statement
    {|  try
{
    int x = 5;
    int y = x / 0;
    Console.WriteLine(x);
}
catch
{
    Console.WriteLine("3");
}
       |}
  = Some
      (Try
         ( StatementBlock
             [ VarDeclare (Int, [(IdentObj "x", Some (ConstExpr (VInt 5)))])
             ; VarDeclare
                 ( Int
                 , [ ( IdentObj "y"
                     , Some (Div (IdentObj "x", ConstExpr (VInt 0))) ) ] )
             ; Print (IdentObj "x") ]
         , [(None, None, StatementBlock [Print (ConstExpr (VString "3"))])]
         , None ))

let%test _ =
  apply_parser parse_statement
    {|  try
{
    int x = 5;
    int y = x / 0;
    
}
finally
{
 
} 
 |}
  = Some
      (Try
         ( StatementBlock
             [ VarDeclare (Int, [(IdentObj "x", Some (ConstExpr (VInt 5)))])
             ; VarDeclare
                 ( Int
                 , [ ( IdentObj "y"
                     , Some (Div (IdentObj "x", ConstExpr (VInt 0))) ) ] ) ]
         , []
         , Some (StatementBlock []) ))

let%test _ =
  apply_parser parse_statement {|  try {} catch when (a==2) {}|}
  = Some
      (Try
         ( StatementBlock []
         , [ ( None
             , Some (Equal (IdentObj "a", ConstExpr (VInt 2)))
             , StatementBlock [] ) ]
         , None ))

let%test _ =
  apply_parser parse_statement {| try {} catch {} |}
  = Some (Try (StatementBlock [], [(None, None, StatementBlock [])], None))

let%test _ =
  apply_parser parse_statement {| try {} catch (Exception) when (a==2) {}|}
  = Some
      (Try
         ( StatementBlock []
         , [ ( Some (CsClass "Exception", None)
             , Some (Equal (IdentObj "a", ConstExpr (VInt 2)))
             , StatementBlock [] ) ]
         , None ))

let%test _ =
  apply_parser parse_statement {| try {} catch (Exception) {}|}
  = Some
      (Try
         ( StatementBlock []
         , [(Some (CsClass "Exception", None), None, StatementBlock [])]
         , None ))

let%test _ =
  apply_parser parse_statement {|  try {} catch (Exception e) when (a==2) {}|}
  = Some
      (Try
         ( StatementBlock []
         , [ ( Some (CsClass "Exception", Some (IdentObj "e"))
             , Some (Equal (IdentObj "a", ConstExpr (VInt 2)))
             , StatementBlock [] ) ]
         , None ))

let%test _ =
  apply_parser parse_statement {|   try {} catch (Exception e) {} |}
  = Some
      (Try
         ( StatementBlock []
         , [ ( Some (CsClass "Exception", Some (IdentObj "e"))
             , None
             , StatementBlock [] ) ]
         , None ))

let%test _ =
  apply_parser parse_field {|  public int sum;|}
  = Some (VariableField ([Public], Int, [(IdentObj "sum", None)]))

let%test _ =
  apply_parser parse_method
    {| static void SayHello()
{
    int hour = 23;
    if(hour > 22)
    {
        return;
    }
    else
    {
        Console.WriteLine("Hello");
    }
}
|}
  = Some
      (Method
         ( [Static]
         , Void
         , IdentObj "SayHello"
         , []
         , Some
             (StatementBlock
                [ VarDeclare
                    (Int, [(IdentObj "hour", Some (ConstExpr (VInt 23)))])
                ; If
                    ( More (IdentObj "hour", ConstExpr (VInt 22))
                    , StatementBlock [Return None]
                    , Some
                        (StatementBlock [Print (ConstExpr (VString "Hello"))])
                    ) ]) ))

let%test _ =
  apply_parser parse_constructor
    {| public Person(string n) { name = n; age = 18; }|}
  = Some
      (Constructor
         ( [Public]
         , IdentObj "Person"
         , [(String, IdentObj "n")]
         , StatementBlock
             [ Expression (Assign (IdentObj "name", IdentObj "n"))
             ; Expression (Assign (IdentObj "age", ConstExpr (VInt 18))) ] ))
