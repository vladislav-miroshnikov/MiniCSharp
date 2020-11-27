  $ (cd ../../../default && demos/demoFirst.exe)
  Ast.Class
  ([], "Program", None,
   [Ast.Method
    ([Ast.Static], Ast.Void, "Main", [],
     Some (Ast.StatementBlock ([Ast.Expression (Ast.CallMethod
                                                ("TrickyTest21", []))])));
    Ast.Method
    ([Ast.Public; Ast.Static], Ast.Bool, "Filter",
     [(Ast.String, Ast.IdentVar ("msg"))],
     Some (Ast.StatementBlock ([Ast.Print (Ast.IdentVar ("msg"));
                                Ast.Return (Some (Ast.ConstExpr (Ast.VBool (true))))])));
    Ast.Method
    ([Ast.Public; Ast.Static], Ast.Void, "A21", [],
     Some (Ast.StatementBlock ([Ast.Try
                                (Ast.StatementBlock ([Ast.Throw (Ast.ClassCreate
                                                                 ("DivideByZeroException",
                                                                  []))]),
                                 [],
                                 Some (Ast.StatementBlock ([Ast.Print (
                                                             Ast.ConstExpr (
                                                              Ast.VString ("A21.finally()")));
                                                            Ast.Throw (
                                                             Ast.ClassCreate
                                                             ("NullReferenceException",
                                                              []))])));
                                Ast.Print (Ast.ConstExpr (Ast.VString ("Resuming A2")))])));
    Ast.Method
    ([Ast.Public; Ast.Static], Ast.Void, "B21", [],
     Some (Ast.StatementBlock ([Ast.Try
                                (Ast.StatementBlock ([Ast.Expression (Ast.CallMethod
                                                                      ("A21",
                                                                      []))]),
                                 [(Some ((Ast.CsClass ("DivideByZeroException"),
                                          None)),
                                   Some (Ast.CallMethod
                                         ("Filter",
                                          [Ast.ConstExpr (Ast.VString ("filter B21"))])),
                                   Ast.StatementBlock ([Ast.Print (Ast.ConstExpr (
                                                                    Ast.VString ("B21 DivideByZeroException")))]))],
                                 Some (Ast.StatementBlock ([Ast.Print (
                                                             Ast.ConstExpr (
                                                              Ast.VString ("B21.finally()")))])));
                                Ast.Print (Ast.ConstExpr (Ast.VString ("Resuming B21")))])));
    Ast.Method
    ([Ast.Public; Ast.Static], Ast.Void, "TrickyTest21", [],
     Some (Ast.StatementBlock ([Ast.Try
                                (Ast.StatementBlock ([Ast.Expression (Ast.CallMethod
                                                                      ("B21",
                                                                      []))]),
                                 [],
                                 Some (Ast.StatementBlock ([Ast.Print (
                                                             Ast.ConstExpr (
                                                              Ast.VString ("TrickyTest Finally")))])));
                                Ast.Print (Ast.ConstExpr (Ast.VString ("Resuming TrickyTest")))])))])
 
