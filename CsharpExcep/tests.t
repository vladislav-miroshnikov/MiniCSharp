  $ (cd ../../../default && demos/demoFirst.exe)
  Class
  ([], "Program", None,
   [([Static],
     Method
     (Void, "Main", [],
      StatementBlock ([Expression (CallMethod ("TrickyTest21", []))])));
    ([Public; Static],
     Method
     (Bool, "Filter", [(String, IdentVar ("msg"))],
      StatementBlock ([Print (IdentVar ("msg"));
                       Return (Some (ConstExpr (VBool (true))))])));
    ([Public; Static],
     Method
     (Void, "A21", [],
      StatementBlock ([Try
                       (StatementBlock ([Throw (ClassCreate
                                                ("DivideByZeroException", []))]),
                        [],
                        Some (StatementBlock ([Print (ConstExpr (VString ("A21.finally()")));
                                               Throw (ClassCreate
                                                      ("NullReferenceException",
                                                       []))])));
                       Print (ConstExpr (VString ("Resuming A2")))])));
    ([Public; Static],
     Method
     (Void, "B21", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("A21", []))]),
                        [(Some ((CsClass ("DivideByZeroException"), None)),
                          Some (CallMethod
                                ("Filter",
                                 [ConstExpr (VString ("filter B21"))])),
                          StatementBlock ([Print (ConstExpr (VString ("B21 DivideByZeroException")))]))],
                        Some (StatementBlock ([Print (ConstExpr (VString ("B21.finally()")))])));
                       Print (ConstExpr (VString ("Resuming B21")))])));
    ([Public; Static],
     Method
     (Void, "TrickyTest21", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("B21", []))]),
                        [],
                        Some (StatementBlock ([Print (ConstExpr (VString ("TrickyTest Finally")))])));
                       Print (ConstExpr (VString ("Resuming TrickyTest")))])))])
