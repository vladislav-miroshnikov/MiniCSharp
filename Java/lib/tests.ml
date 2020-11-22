open Parser
open Parser.Stat
open Parser.Expr
open Opal

(* -------------------  EXPRESSIONS ------------------- *)

let%test _ =
  parse expression (LazyStream.of_string "a = 2")
  = Some (Assign (Identifier "a", Const (JVInt 2)))

let%test _ =
  parse expression (LazyStream.of_string "a[i] = 2")
  = Some
      (Assign (ArrayAccess (Identifier "a", Identifier "i"), Const (JVInt 2)))

let%test _ =
  parse expression (LazyStream.of_string "a = b = 3")
  = Some (Assign (Identifier "a", Assign (Identifier "b", Const (JVInt 3))))

let%test _ =
  parse expression (LazyStream.of_string "1 + 2")
  = Some (Add (Const (JVInt 1), Const (JVInt 2)))

let%test _ =
  parse expression (LazyStream.of_string "1 + 2 * 3 / 4 % 5 - 6")
  = Some
      (Sub
         ( Add
             ( Const (JVInt 1),
               Mod
                 ( Div (Mult (Const (JVInt 2), Const (JVInt 3)), Const (JVInt 4)),
                   Const (JVInt 5) ) ),
           Const (JVInt 6) ))

let%test _ =
  parse expression
    (LazyStream.of_string "(x + y <= 10) && (a % 2 == 0) || !(c / 2 > 3)")
  = Some
      (Or
         ( And
             ( LessOrEqual
                 (Add (Identifier "x", Identifier "y"), Const (JVInt 10)),
               Equal (Mod (Identifier "a", Const (JVInt 2)), Const (JVInt 0)) ),
           Not (More (Div (Identifier "c", Const (JVInt 2)), Const (JVInt 3)))
         ))

let%test _ =
  parse expression (LazyStream.of_string "2 + 3 * (5 - 3)")
  = Some
      (Add
         ( Const (JVInt 2),
           Mult (Const (JVInt 3), Sub (Const (JVInt 5), Const (JVInt 3))) ))

let%test _ =
  parse expression (LazyStream.of_string "a[i]")
  = Some (ArrayAccess (Identifier "a", Identifier "i"))

let%test _ =
  parse expression (LazyStream.of_string "someObj.method(arg1, arg2, arg3)")
  = Some
      (FieldAccess
         ( Identifier "someObj",
           CallMethod
             ( Identifier "method",
               [ Identifier "arg1"; Identifier "arg2"; Identifier "arg3" ] ) ))

let%test _ =
  parse expression (LazyStream.of_string "arr[i].get()")
  = Some
      (FieldAccess
         ( ArrayAccess (Identifier "arr", Identifier "i"),
           CallMethod (Identifier "get", []) ))

let%test _ =
  parse expression (LazyStream.of_string "a[i].b[j]")
  = Some
      (ArrayAccess
         ( FieldAccess
             (ArrayAccess (Identifier "a", Identifier "i"), Identifier "b"),
           Identifier "j" ))

let%test _ =
  parse expression (LazyStream.of_string "this.getArray()[i]")
  = Some
      (ArrayAccess
         ( FieldAccess (This, CallMethod (Identifier "getArray", [])),
           Identifier "i" ))

let%test _ =
  parse expression (LazyStream.of_string "this.getCar().wheels[2].rad")
  = Some
      (FieldAccess
         ( ArrayAccess
             ( FieldAccess
                 ( FieldAccess (This, CallMethod (Identifier "getCar", [])),
                   Identifier "wheels" ),
               Const (JVInt 2) ),
           Identifier "rad" ))

let%test _ =
  parse expression (LazyStream.of_string "a.b[i].c[j]")
  = Some
      (ArrayAccess
         ( FieldAccess
             ( ArrayAccess
                 (FieldAccess (Identifier "a", Identifier "b"), Identifier "i"),
               Identifier "c" ),
           Identifier "j" ))

let%test _ =
  parse expression (LazyStream.of_string "a.b.c")
  = Some
      (FieldAccess (FieldAccess (Identifier "a", Identifier "b"), Identifier "c"))

let%test _ =
  parse expression (LazyStream.of_string "call(1 + 2, 40)")
  = Some
      (CallMethod
         ( Identifier "call",
           [ Add (Const (JVInt 1), Const (JVInt 2)); Const (JVInt 40) ] ))

let%test _ =
  parse expression (LazyStream.of_string "new int[]")
  = Some (ArrayCreate (JInt, None))

let%test _ =
  parse expression (LazyStream.of_string "new int[4]")
  = Some (ArrayCreate (JInt, Some (Const (JVInt 4))))

let%test _ =
  parse expression (LazyStream.of_string "new arr[i]")
  = Some (ArrayCreate (JClassName "arr", Some (Identifier "i")))

let%test _ =
  parse expression (LazyStream.of_string "new Car(2,\"Ford\")")
  = Some (ClassCreate ("Car", [ Const (JVInt 2); Const (JVString "Ford") ]))

let%test _ =
  parse expression (LazyStream.of_string "get(new Sth(), new String[10])")
  = Some
      (CallMethod
         ( Identifier "get",
           [
             ClassCreate ("Sth", []);
             ArrayCreate (JString, Some (Const (JVInt 10)));
           ] ))

let%test _ =
  parse expression (LazyStream.of_string "(new Man(3,\"John\")).scream())")
  = Some
      (FieldAccess
         ( ClassCreate ("Man", [ Const (JVInt 3); Const (JVString "John") ]),
           CallMethod (Identifier "scream", []) ))

let%test _ =
  parse expression (LazyStream.of_string "--(obj.f + (x + y)++)")
  = Some
      (PrefDec
         (Add
            ( FieldAccess (Identifier "obj", Identifier "f"),
              PostInc (Add (Identifier "x", Identifier "y")) )))

(* -------------------  STATEMENTS ---------------------*)

let%test _ =
  parse statement (LazyStream.of_string "int a = 0, b, c, d = 5;")
  = Some
      (VarDec
         ( JInt,
           [
             (Identifier "a", Some (Const (JVInt 0)));
             (Identifier "b", None);
             (Identifier "c", None);
             (Identifier "d", Some (Const (JVInt 5)));
           ] ))

let%test _ =
  parse statement (LazyStream.of_string "int[] a = new int[6];")
  = Some
      (VarDec
         ( JArray JInt,
           [
             (Identifier "a", Some (ArrayCreate (JInt, Some (Const (JVInt 6)))));
           ] ))

let%test _ =
  parse statement (LazyStream.of_string "int a = 0, b = 1, c = 2;")
  = Some
      (VarDec
         ( JInt,
           [
             (Identifier "a", Some (Const (JVInt 0)));
             (Identifier "b", Some (Const (JVInt 1)));
             (Identifier "c", Some (Const (JVInt 2)));
           ] ))

let%test _ =
  parse statement (LazyStream.of_string "if (x < 10) x++;")
  = Some
      (If
         ( Less (Identifier "x", Const (JVInt 10)),
           Expression (PostInc (Identifier "x")),
           None ))

let%test _ =
  parse statement
    (LazyStream.of_string
       "if (a < b) {\n return b - a; \n } else { \n return a - b; \n }")
  = Some
      (If
         ( Less (Identifier "a", Identifier "b"),
           StatBlock [ Return (Some (Sub (Identifier "b", Identifier "a"))) ],
           Some
             (StatBlock [ Return (Some (Sub (Identifier "a", Identifier "b"))) ])
         ))

let%test _ =
  parse statement (LazyStream.of_string "array = new int[3];")
  = Some
      (Expression
         (Assign (Identifier "array", ArrayCreate (JInt, Some (Const (JVInt 3))))))

let%test _ =
  parse statement
    (LazyStream.of_string
       "if (a % 2 == 0 && b < 2) {\n\
       \ a++;\n\
       \ b--;\n\
       \ return a * b; \n\
       \ } else if (!(b / 2 != 5)) { \n\
       \ --b; \n\
       \  return (a + b)*3; \n\
       \ } else continue;")
  = Some
      (If
         ( And
             ( Equal (Mod (Identifier "a", Const (JVInt 2)), Const (JVInt 0)),
               Less (Identifier "b", Const (JVInt 2)) ),
           StatBlock
             [
               Expression (PostInc (Identifier "a"));
               Expression (PostDec (Identifier "b"));
               Return (Some (Mult (Identifier "a", Identifier "b")));
             ],
           Some
             (If
                ( Not
                    (NotEqual
                       (Div (Identifier "b", Const (JVInt 2)), Const (JVInt 5))),
                  StatBlock
                    [
                      Expression (PrefDec (Identifier "b"));
                      Return
                        (Some
                           (Mult
                              ( Add (Identifier "a", Identifier "b"),
                                Const (JVInt 3) )));
                    ],
                  Some Continue )) ))

let%test _ =
  parse statement
    (LazyStream.of_string
       "while (d * d <= n) { if (n % d == 0) { return true; } d++; }")
  = Some
      (While
         ( LessOrEqual (Mult (Identifier "d", Identifier "d"), Identifier "n"),
           StatBlock
             [
               If
                 ( Equal (Mod (Identifier "n", Identifier "d"), Const (JVInt 0)),
                   StatBlock [ Return (Some (Const (JVBool true))) ],
                   None );
               Expression (PostInc (Identifier "d"));
             ] ))

let%test _ =
  parse statement
    (LazyStream.of_string
       "for (int i = 0, j = n - 1; i < j; i++, j--) { \
        System.out.println(\"test\"); }")
  = Some
      (For
         ( Some
             (VarDec
                ( JInt,
                  [
                    (Identifier "i", Some (Const (JVInt 0)));
                    ( Identifier "j",
                      Some (Sub (Identifier "n", Const (JVInt 1))) );
                  ] )),
           Some (Less (Identifier "i", Identifier "j")),
           [ PostInc (Identifier "i"); PostDec (Identifier "j") ],
           StatBlock
             [
               Expression
                 (FieldAccess
                    ( FieldAccess (Identifier "System", Identifier "out"),
                      CallMethod
                        (Identifier "println", [ Const (JVString "test") ]) ));
             ] ))

let%test _ =
  parse statement
    (LazyStream.of_string "if (somethingWrong()) throw new Exception();")
  = Some
      (If
         ( CallMethod (Identifier "somethingWrong", []),
           Throw (ClassCreate ("Exception", [])),
           None ))

let%test _ =
  parse statement (LazyStream.of_string "for(public int i = 0;;) {i++;}") = None

(*---------------- IN CLASSES ---------------*)

let%test _ =
  parse field_declaration (LazyStream.of_string "public int wheel;")
  = Some (VarField ([ Public ], JInt, [ (Identifier "wheel", None) ]))

let%test _ =
  parse method_declaration
    (LazyStream.of_string
       "public int arraySum (int[] a) { int sum = 0; for (int i = 0; i < \
        a.length(); i++) {sum = sum + a[i];} return sum; }")
  = Some
      (Method
         ( [ Public ],
           JInt,
           Identifier "arraySum",
           [ (JArray JInt, Identifier "a") ],
           Some
             (StatBlock
                [
                  VarDec (JInt, [ (Identifier "sum", Some (Const (JVInt 0))) ]);
                  For
                    ( Some
                        (VarDec
                           (JInt, [ (Identifier "i", Some (Const (JVInt 0))) ])),
                      Some
                        (Less
                           ( Identifier "i",
                             FieldAccess
                               ( Identifier "a",
                                 CallMethod (Identifier "length", []) ) )),
                      [ PostInc (Identifier "i") ],
                      StatBlock
                        [
                          Expression
                            (Assign
                               ( Identifier "sum",
                                 Add
                                   ( Identifier "sum",
                                     ArrayAccess (Identifier "a", Identifier "i")
                                   ) ));
                        ] );
                  Return (Some (Identifier "sum"));
                ]) ))

let%test _ =
  parse constructor_declaration
    (LazyStream.of_string
       "public Car(int speed, int[] wheels) {this.speed = speed; this.wheels = \
        wheels;}")
  = Some
      (Constructor
         ( [ Public ],
           Identifier "Car",
           [ (JInt, Identifier "speed"); (JArray JInt, Identifier "wheels") ],
           StatBlock
             [
               Expression
                 (Assign
                    (FieldAccess (This, Identifier "speed"), Identifier "speed"));
               Expression
                 (Assign
                    ( FieldAccess (This, Identifier "wheels"),
                      Identifier "wheels" ));
             ] ))
