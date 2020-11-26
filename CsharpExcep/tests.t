  $ (cd ../../../default && demos/demoFirst.exe)
  Ast.Class
  ([], Ast.IdentObj ("Program"), None,
   [Ast.Method
    ([Ast.Static], Ast.Void, Ast.IdentObj ("Main"), [],
     Some (Ast.StatementBlock ([Ast.Expression (Ast.CallMethod
                                                (Ast.IdentObj ("TrickyTest21"),
                                                 []))])));
    Ast.Method
    ([Ast.Public; Ast.Static], Ast.Bool, Ast.IdentObj ("Filter"),
     [(Ast.String, Ast.IdentObj ("msg"))],
     Some (Ast.StatementBlock ([Ast.Print (Ast.IdentObj ("msg"));
                                Ast.Return (Some (Ast.ConstExpr (Ast.VBool (true))))])));
    Ast.Method
    ([Ast.Public; Ast.Static], Ast.Void, Ast.IdentObj ("A21"), [],
     Some (Ast.StatementBlock ([Ast.Try
                                (Ast.StatementBlock ([Ast.Throw (Ast.ClassCreate
                                                                 (Ast.IdentObj ("DivideByZeroException"),
                                                                  []))]),
                                 [],
                                 Some (Ast.StatementBlock ([Ast.Print (
                                                             Ast.ConstExpr (
                                                              Ast.VString ("A21.finally()")));
                                                            Ast.Throw (
                                                             Ast.ClassCreate
                                                             (Ast.IdentObj ("NullReferenceException"),
                                                              []))])));
                                Ast.Print (Ast.ConstExpr (Ast.VString ("Resuming A2")))])));
    Ast.Method
    ([Ast.Public; Ast.Static], Ast.Void, Ast.IdentObj ("B21"), [],
     Some (Ast.StatementBlock ([Ast.Try
                                (Ast.StatementBlock ([Ast.Expression (Ast.CallMethod
                                                                      (Ast.IdentObj ("A21"),
                                                                      []))]),
                                 [(Some ((Ast.CsClass ("DivideByZeroException"),
                                          None)),
                                   Some (Ast.CallMethod
                                         (Ast.IdentObj ("Filter"),
                                          [Ast.ConstExpr (Ast.VString ("filter B21"))])),
                                   Ast.StatementBlock ([Ast.Print (Ast.ConstExpr (
                                                                    Ast.VString ("B21 DivideByZeroException")))]))],
                                 Some (Ast.StatementBlock ([Ast.Print (
                                                             Ast.ConstExpr (
                                                              Ast.VString ("B21.finally()")))])));
                                Ast.Print (Ast.ConstExpr (Ast.VString ("Resuming B21")))])));
    Ast.Method
    ([Ast.Public; Ast.Static], Ast.Void, Ast.IdentObj ("TrickyTest21"), 
     [],
     Some (Ast.StatementBlock ([Ast.Try
                                (Ast.StatementBlock ([Ast.Expression (Ast.CallMethod
                                                                      (Ast.IdentObj ("B21"),
                                                                      []))]),
                                 [],
                                 Some (Ast.StatementBlock ([Ast.Print (
                                                             Ast.ConstExpr (
                                                              Ast.VString ("TrickyTest Finally")))])));
                                Ast.Print (Ast.ConstExpr (Ast.VString ("Resuming TrickyTest")))])))])
