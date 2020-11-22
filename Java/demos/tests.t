  $ (cd ../../../../default && demos/demoFirst.exe)
  Ast.Class
  ([Ast.Public], Ast.Identifier ("Main"), None,
   [Ast.Method
    ([Ast.Public; Ast.Static], Ast.JVoid, Ast.Identifier ("main"),
     [(Ast.JArray (Ast.JString), Ast.Identifier ("args"))],
     Some (Ast.StatBlock ([Ast.VarDec
                           (Ast.JClassName ("Person"),
                            [(Ast.Identifier ("p"),
                              Some (Ast.ClassCreate
                                    ("Person",
                                     [Ast.Const (Ast.JVInt (80));
                                      Ast.Const (Ast.JVInt (45))])))]);
                           Ast.Expression (Ast.FieldAccess
                                           (Ast.FieldAccess
                                            (Ast.Identifier ("System"),
                                             Ast.Identifier ("out")),
                                            Ast.CallMethod
                                            (Ast.Identifier ("println"),
                                             [Ast.FieldAccess
                                              (Ast.Identifier ("p"),
                                               Ast.CallMethod
                                               (Ast.Identifier ("getWeight"),
                                                []))])));
                           Ast.VarDec
                           (Ast.JClassName ("Child"),
                            [(Ast.Identifier ("ch"),
                              Some (Ast.ClassCreate
                                    ("Child",
                                     [Ast.Const (Ast.JVInt (66));
                                      Ast.Const (Ast.JVInt (20))])))]);
                           Ast.Expression (Ast.FieldAccess
                                           (Ast.Identifier ("ch"),
                                            Ast.CallMethod
                                            (Ast.Identifier ("setCash"),
                                             [Ast.Const (Ast.JVInt (50))])));
                           Ast.Expression (Ast.FieldAccess
                                           (Ast.Identifier ("ch"),
                                            Ast.CallMethod
                                            (Ast.Identifier ("giveEvenNumbers100"),
                                             [])))])))])
  Ast.Class
  ([], Ast.Identifier ("Person"), None,
   [Ast.VarField ([Ast.Public], Ast.JInt, [(Ast.Identifier ("weight"), None)]);
    Ast.VarField ([Ast.Public], Ast.JInt, [(Ast.Identifier ("age"), None)]);
    Ast.Constructor
    ([Ast.Public], Ast.Identifier ("Person"),
     [(Ast.JInt, Ast.Identifier ("w")); (Ast.JInt, Ast.Identifier ("a"))],
     Ast.StatBlock ([Ast.Expression (Ast.Assign
                                     (Ast.FieldAccess
                                      (Ast.This, Ast.Identifier ("weight")),
                                      Ast.Identifier ("w")));
                     Ast.Expression (Ast.Assign
                                     (Ast.FieldAccess
                                      (Ast.This, Ast.Identifier ("age")),
                                      Ast.Identifier ("a")))]));
    Ast.Method
    ([Ast.Public], Ast.JInt, Ast.Identifier ("getWeight"), [],
     Some (Ast.StatBlock ([Ast.Return (Some (Ast.Identifier ("weight")))])));
    Ast.Method
    ([Ast.Public], Ast.JInt, Ast.Identifier ("getAge"), [],
     Some (Ast.StatBlock ([Ast.Return (Some (Ast.Identifier ("age")))])));
    Ast.Method
    ([Ast.Public], Ast.JVoid, Ast.Identifier ("setWeight"),
     [(Ast.JInt, Ast.Identifier ("w"))],
     Some (Ast.StatBlock ([Ast.Expression (Ast.Assign
                                           (Ast.FieldAccess
                                            (Ast.This,
                                             Ast.Identifier ("weight")),
                                            Ast.Identifier ("w")))])));
    Ast.Method
    ([Ast.Public], Ast.JVoid, Ast.Identifier ("setAge"),
     [(Ast.JInt, Ast.Identifier ("a"))],
     Some (Ast.StatBlock ([Ast.Expression (Ast.Assign
                                           (Ast.FieldAccess
                                            (Ast.This, Ast.Identifier ("age")),
                                            Ast.Identifier ("a")))])))])
  Ast.Class
  ([], Ast.Identifier ("Child"), Some (Ast.Identifier ("Person")),
   [Ast.VarField ([Ast.Public], Ast.JInt, [(Ast.Identifier ("cash"), None)]);
    Ast.Constructor
    ([Ast.Public], Ast.Identifier ("Child"),
     [(Ast.JInt, Ast.Identifier ("w")); (Ast.JInt, Ast.Identifier ("a"))],
     Ast.StatBlock ([Ast.Expression (Ast.CallMethod
                                     (Ast.Super,
                                      [Ast.Identifier ("w");
                                       Ast.Identifier ("a")]));
                     Ast.Expression (Ast.Assign
                                     (Ast.Identifier ("cash"),
                                      Ast.Const (Ast.JVInt (0))))]));
    Ast.Method
    ([Ast.Public], Ast.JInt, Ast.Identifier ("getCash"), [],
     Some (Ast.StatBlock ([Ast.Return (Some (Ast.Identifier ("cash")))])));
    Ast.Method
    ([Ast.Public], Ast.JVoid, Ast.Identifier ("setCash"),
     [(Ast.JInt, Ast.Identifier ("c"))],
     Some (Ast.StatBlock ([Ast.Expression (Ast.Assign
                                           (Ast.FieldAccess
                                            (Ast.This, Ast.Identifier ("cash")),
                                            Ast.Identifier ("c")))])));
    Ast.Constructor
    ([Ast.Public], Ast.Identifier ("Child"),
     [(Ast.JInt, Ast.Identifier ("w")); (Ast.JInt, Ast.Identifier ("a"));
      (Ast.JInt, Ast.Identifier ("c"))],
     Ast.StatBlock ([Ast.Expression (Ast.CallMethod
                                     (Ast.Super,
                                      [Ast.Identifier ("w");
                                       Ast.Identifier ("a")]));
                     Ast.Expression (Ast.Assign
                                     (Ast.Identifier ("cash"),
                                      Ast.Identifier ("c")))]));
    Ast.Method
    ([Ast.Public], Ast.JVoid, Ast.Identifier ("giveEvenNumbers100"), [],
     Some (Ast.StatBlock ([Ast.For
                           (Some (Ast.VarDec
                                  (Ast.JInt,
                                   [(Ast.Identifier ("i"),
                                     Some (Ast.Const (Ast.JVInt (0))))])),
                            Some (Ast.Less
                                  (Ast.Identifier ("i"),
                                   Ast.Const (Ast.JVInt (100)))),
                            [Ast.PostInc (Ast.Identifier ("i"))],
                            Ast.StatBlock ([Ast.If
                                            (Ast.And
                                             (Ast.Equal
                                              (Ast.Mod
                                               (Ast.Identifier ("i"),
                                                Ast.Const (Ast.JVInt (2))),
                                               Ast.Const (Ast.JVInt (0))),
                                              Ast.Not (Ast.Equal
                                                       (Ast.Mod
                                                        (Ast.Identifier ("i"),
                                                         Ast.Const (Ast.JVInt (2))),
                                                        Ast.Const (Ast.JVInt (1))))),
                                             Ast.StatBlock ([Ast.Expression (
                                                              Ast.FieldAccess
                                                              (Ast.FieldAccess
                                                               (Ast.Identifier ("System"),
                                                                Ast.Identifier ("out")),
                                                               Ast.CallMethod
                                                               (Ast.Identifier ("println"),
                                                                [Ast.Identifier ("i")])))]),
                                             Some (Ast.StatBlock ([Ast.Continue])))]))])))])


