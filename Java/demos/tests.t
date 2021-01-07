  $ (cd ../../../../default && demos/demoParserFirst.exe)
  Class
  ([Public], Name ("Main"), None,
   [([Public; Static],
     Method
     (Void, Name ("main"), [],
      Some (StmtBlock ([VarDec
                        (None, ClassName ("Person"),
                         [(Name ("p"),
                           Some (ClassCreate
                                 (Name ("Person"),
                                  [Const (VInt (80)); Const (VInt (45))])))]);
                        Expression (FieldAccess
                                    (FieldAccess
                                     (Identifier ("System"),
                                      Identifier ("out")),
                                     CallMethod
                                     (Identifier ("println"),
                                      [FieldAccess
                                       (Identifier ("p"),
                                        CallMethod
                                        (Identifier ("getWeight"), []))])));
                        VarDec
                        (None, ClassName ("Child"),
                         [(Name ("ch"),
                           Some (ClassCreate
                                 (Name ("Child"),
                                  [Const (VInt (66)); Const (VInt (20))])))]);
                        Expression (FieldAccess
                                    (Identifier ("ch"),
                                     CallMethod
                                     (Identifier ("setCash"),
                                      [Const (VInt (50))])));
                        Expression (FieldAccess
                                    (Identifier ("ch"),
                                     CallMethod
                                     (Identifier ("giveEvenNumbers100"), [])))]))))])
  Class
  ([], Name ("Person"), None,
   [([Public], VarField (Int, [(Name ("weight"), None)]));
    ([Public], VarField (Int, [(Name ("age"), None)]));
    ([Public],
     Constructor
     (Name ("Person"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("weight")),
                               Identifier ("w")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("age")),
                               Identifier ("a")))])));
    ([Public],
     Method
     (Int, Name ("getWeight"), [],
      Some (StmtBlock ([Return (Some (Identifier ("weight")))]))));
    ([Public],
     Method
     (Int, Name ("getAge"), [],
      Some (StmtBlock ([Return (Some (Identifier ("age")))]))));
    ([Public],
     Method
     (Void, Name ("setWeight"), [(Int, Name ("w"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("weight")),
                                     Identifier ("w")))]))));
    ([Public],
     Method
     (Void, Name ("setAge"), [(Int, Name ("a"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("age")),
                                     Identifier ("a")))]))))])
  Class
  ([], Name ("Child"), Some (Name ("Person")),
   [([Public], VarField (Int, [(Name ("cash"), None)]));
    ([Public],
     Constructor
     (Name ("Child"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Const (VInt (0))))])));
    ([Public],
     Method
     (Int, Name ("getCash"), [],
      Some (StmtBlock ([Return (Some (Identifier ("cash")))]))));
    ([Public],
     Method
     (Void, Name ("setCash"), [(Int, Name ("c"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("cash")),
                                     Identifier ("c")))]))));
    ([Public],
     Constructor
     (Name ("Child"),
      [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Identifier ("c")))])));
    ([Public],
     Method
     (Void, Name ("giveEvenNumbers100"), [],
      Some (StmtBlock ([For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("i"), Some (Const (VInt (0))))])),
                         Some (Less (Identifier ("i"), Const (VInt (100)))),
                         [PostInc (Identifier ("i"))],
                         StmtBlock ([If
                                     (And
                                      (Equal
                                       (Mod
                                        (Identifier ("i"), Const (VInt (2))),
                                        Const (VInt (0))),
                                       Not (Equal
                                            (Mod
                                             (Identifier ("i"),
                                              Const (VInt (2))),
                                             Const (VInt (1))))),
                                      StmtBlock ([Expression (FieldAccess
                                                              (FieldAccess
                                                               (Identifier ("System"),
                                                                Identifier ("out")),
                                                               CallMethod
                                                               (Identifier ("println"),
                                                                [Identifier ("i")])))]),
                                      Some (StmtBlock ([Continue])))]))]))))])
  $ (cd ../../../../default && demos/demoParserSecond.exe)
  Class
  ([Public], Name ("Main"), None,
   [([Public; Static],
     Method
     (Void, Name ("main"), [],
      Some (StmtBlock ([VarDec
                        (None, Array (ClassName ("Figure")),
                         [(Name ("list"),
                           Some (ArrayCreateElements
                                 (ClassName ("Figure"),
                                  [ClassCreate
                                   (Name ("Circle"), [Const (VInt (5))]);
                                   ClassCreate
                                   (Name ("Rectangle"),
                                    [Const (VInt (2)); Const (VInt (4))]);
                                   ClassCreate (Name ("Triangle"), [])])))]);
                        VarDec
                        (None, ClassName ("AreaVisitor"),
                         [(Name ("areaVisitor"),
                           Some (ClassCreate (Name ("AreaVisitor"), [])))]);
                        VarDec
                        (None, ClassName ("PerimeterVisitor"),
                         [(Name ("perimeterVisitor"),
                           Some (ClassCreate (Name ("PerimeterVisitor"), [])))]);
                        For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("i"), Some (Const (VInt (0))))])),
                         Some (Less
                               (Identifier ("i"),
                                FieldAccess
                                (Identifier ("list"), Identifier ("length")))),
                         [PostInc (Identifier ("i"))],
                         StmtBlock ([Expression (FieldAccess
                                                 (FieldAccess
                                                  (Identifier ("System"),
                                                   Identifier ("out")),
                                                  CallMethod
                                                  (Identifier ("println"),
                                                   [FieldAccess
                                                    (ArrayAccess
                                                     (Identifier ("list"),
                                                      Identifier ("i")),
                                                     CallMethod
                                                     (Identifier ("accept"),
                                                      [Identifier ("areaVisitor")]))])))]));
                        For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("j"), Some (Const (VInt (0))))])),
                         Some (Less
                               (Identifier ("j"),
                                FieldAccess
                                (Identifier ("list"), Identifier ("length")))),
                         [PostInc (Identifier ("j"))],
                         StmtBlock ([Expression (FieldAccess
                                                 (FieldAccess
                                                  (Identifier ("System"),
                                                   Identifier ("out")),
                                                  CallMethod
                                                  (Identifier ("println"),
                                                   [FieldAccess
                                                    (ArrayAccess
                                                     (Identifier ("list"),
                                                      Identifier ("j")),
                                                     CallMethod
                                                     (Identifier ("accept"),
                                                      [Identifier ("perimeterVisitor")]))])))]))]))))])
  Class
  ([Abstract], Name ("Figure"), None,
   [([Abstract],
     Method (Int, Name ("accept"), [(ClassName ("Visitor"), Name ("v"))], None))])
  Class
  ([Abstract], Name ("Visitor"), None,
   [([Abstract],
     Method
     (Int, Name ("visit"), [(ClassName ("Circle"), Name ("circle"))], None));
    ([Abstract],
     Method
     (Int, Name ("visit"), [(ClassName ("Rectangle"), Name ("rectangle"))],
      None));
    ([Abstract],
     Method
     (Int, Name ("visit"), [(ClassName ("Triangle"), Name ("triangle"))], None))])
  Class
  ([], Name ("AreaVisitor"), Some (Name ("Visitor")),
   [([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Circle"), Name ("circle"))],
      Some (StmtBlock ([Return (Some (Mult
                                      (Mult
                                       (Const (VInt (3)),
                                        FieldAccess
                                        (Identifier ("circle"),
                                         Identifier ("radius"))),
                                       FieldAccess
                                       (Identifier ("circle"),
                                        Identifier ("radius")))))]))));
    ([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Rectangle"), Name ("rectangle"))],
      Some (StmtBlock ([Return (Some (Mult
                                      (FieldAccess
                                       (Identifier ("rectangle"),
                                        Identifier ("a")),
                                       FieldAccess
                                       (Identifier ("rectangle"),
                                        Identifier ("b")))))]))));
    ([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Triangle"), Name ("triangle"))],
      Some (StmtBlock ([VarDec
                        (None, Int,
                         [(Name ("p"),
                           Some (Div
                                 (Add
                                  (Add
                                   (FieldAccess
                                    (Identifier ("triangle"), Identifier ("a")),
                                    FieldAccess
                                    (Identifier ("triangle"), Identifier ("b"))),
                                   FieldAccess
                                   (Identifier ("triangle"), Identifier ("c"))),
                                  Const (VInt (2)))))]);
                        Return (Some (Mult
                                      (Mult
                                       (Mult
                                        (Identifier ("p"),
                                         Sub
                                         (Identifier ("p"),
                                          FieldAccess
                                          (Identifier ("triangle"),
                                           Identifier ("a")))),
                                        Sub
                                        (Identifier ("p"),
                                         FieldAccess
                                         (Identifier ("triangle"),
                                          Identifier ("b")))),
                                       Sub
                                       (Identifier ("p"),
                                        FieldAccess
                                        (Identifier ("triangle"),
                                         Identifier ("c"))))))]))))])
  Class
  ([], Name ("PerimeterVisitor"), Some (Name ("Visitor")),
   [([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Circle"), Name ("circle"))],
      Some (StmtBlock ([Return (Some (Mult
                                      (Mult
                                       (Const (VInt (2)), Const (VInt (3))),
                                       FieldAccess
                                       (Identifier ("circle"),
                                        Identifier ("radius")))))]))));
    ([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Rectangle"), Name ("rectangle"))],
      Some (StmtBlock ([Return (Some (Mult
                                      (Add
                                       (FieldAccess
                                        (Identifier ("rectangle"),
                                         Identifier ("a")),
                                        FieldAccess
                                        (Identifier ("rectangle"),
                                         Identifier ("b"))),
                                       Const (VInt (2)))))]))));
    ([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Triangle"), Name ("triangle"))],
      Some (StmtBlock ([Return (Some (Add
                                      (Add
                                       (FieldAccess
                                        (Identifier ("triangle"),
                                         Identifier ("a")),
                                        FieldAccess
                                        (Identifier ("triangle"),
                                         Identifier ("b"))),
                                       FieldAccess
                                       (Identifier ("triangle"),
                                        Identifier ("c")))))]))))])
  Class
  ([], Name ("Circle"), Some (Name ("Figure")),
   [([Public], VarField (Int, [(Name ("radius"), None)]));
    ([Public],
     Constructor
     (Name ("Circle"), [(Int, Name ("radius"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("radius")),
                               Identifier ("radius")))])));
    ([Public],
     Constructor
     (Name ("Circle"), [],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("radius")),
                               Const (VInt (1))))])));
    ([Override],
     Method
     (Int, Name ("accept"), [(ClassName ("Visitor"), Name ("v"))],
      Some (StmtBlock ([Return (Some (FieldAccess
                                      (Identifier ("v"),
                                       CallMethod
                                       (Identifier ("visit"), [This]))))]))))])
  Class
  ([], Name ("Triangle"), Some (Name ("Figure")),
   [([Public],
     VarField
     (Int, [(Name ("a"), None); (Name ("b"), None); (Name ("c"), None)]));
    ([Public],
     Constructor
     (Name ("Triangle"),
      [(Int, Name ("a")); (Int, Name ("b")); (Int, Name ("c"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("a")),
                               Identifier ("a")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("b")),
                               Identifier ("b")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("c")),
                               Identifier ("c")))])));
    ([Public],
     Constructor
     (Name ("Triangle"), [],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("a")),
                               Const (VInt (1))));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("b")),
                               Const (VInt (1))));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("c")),
                               Const (VInt (1))))])));
    ([Override],
     Method
     (Int, Name ("accept"), [(ClassName ("Visitor"), Name ("v"))],
      Some (StmtBlock ([Return (Some (FieldAccess
                                      (Identifier ("v"),
                                       CallMethod
                                       (Identifier ("visit"), [This]))))]))))])
  Class
  ([], Name ("Rectangle"), Some (Name ("Figure")),
   [([Public], VarField (Int, [(Name ("a"), None); (Name ("b"), None)]));
    ([Public],
     Constructor
     (Name ("Rectangle"), [],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("a")),
                               Const (VInt (1))));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("b")),
                               Const (VInt (1))))])));
    ([Public],
     Constructor
     (Name ("Rectangle"), [(Int, Name ("a")); (Int, Name ("b"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("a")),
                               Identifier ("a")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("b")),
                               Identifier ("b")))])));
    ([Override],
     Method
     (Int, Name ("accept"), [(ClassName ("Visitor"), Name ("v"))],
      Some (StmtBlock ([Return (Some (FieldAccess
                                      (Identifier ("v"),
                                       CallMethod
                                       (Identifier ("visit"), [This]))))]))))])










  $ (cd ../../../../default && demos/demoClassLoader.exe)
  -------------------TESTING_INHERITANCE-------------------
  
  [["Child" ->
     { this_key = "Child";
       field_table =
       [["cash" ->
          { f_type = Int; key = "cash"; is_not_mutable = false; sub_tree = None
            }
       
  "weight" ->
   { f_type = Int; key = "weight"; is_not_mutable = false; sub_tree = None }
  
  "age" ->
   { f_type = Int; key = "age"; is_not_mutable = false; sub_tree = None }
  
  ]]
  ;
  method_table =
  [["getWeight@@" ->
     { m_type = Int; is_abstract = false; is_overridable = true;
       has_override_annotation = false; args = []; key = "getWeight@@";
       body = Some (StmtBlock ([Return (Some (Identifier ("weight")))]));
       is_overriden = false }
  
  "getAge@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = true; args = []; key = "getAge@@";
     body =
     Some (StmtBlock ([Return (Some (Add (Identifier ("age"), Const (VInt (1)))))]));
     is_overriden = true }
  
  "setAgeInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("a"))];
     key = "setAgeInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("age")),
                                    Identifier ("a")))]));
     is_overriden = false }
  
  "getCash@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "getCash@@";
     body = Some (StmtBlock ([Return (Some (Identifier ("cash")))]));
     is_overriden = false }
  
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  "setCashInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("c"))];
     key = "setCashInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("cash")),
                                    Identifier ("c")))]));
     is_overriden = false }
  
  "toString@@" ->
   { m_type = String; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  "setWeightInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("w"))];
     key = "setWeightInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("weight")),
                                    Identifier ("w")))]));
     is_overriden = false }
  
  "giveEvenNumbers100@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "giveEvenNumbers100@@";
     body =
     Some (StmtBlock ([For
                       (Some (VarDec
                              (None, Int,
                               [(Name ("i"), Some (Const (VInt (0))))])),
                        Some (Less (Identifier ("i"), Const (VInt (100)))),
                        [PostInc (Identifier ("i"))],
                        StmtBlock ([If
                                    (And
                                     (Equal
                                      (Mod (Identifier ("i"), Const (VInt (2))),
                                       Const (VInt (0))),
                                      Not (Equal
                                           (Mod
                                            (Identifier ("i"),
                                             Const (VInt (2))),
                                            Const (VInt (1))))),
                                     StmtBlock ([Expression (FieldAccess
                                                             (FieldAccess
                                                              (Identifier ("System"),
                                                               Identifier ("out")),
                                                              CallMethod
                                                              (Identifier ("println"),
                                                               [Identifier ("i")])))]),
                                     Some (StmtBlock ([Continue])))]))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["ChildIntIntInt$$" ->
     { key = "ChildIntIntInt$$";
       args = [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))];
       body =
       StmtBlock ([Expression (CallMethod
                               (Super, [Identifier ("w"); Identifier ("a")]));
                   Expression (Assign (Identifier ("cash"), Identifier ("c")))])
       }
  
  "ChildIntInt$$" ->
   { key = "ChildIntInt$$"; args = [(Int, Name ("w")); (Int, Name ("a"))];
     body =
     StmtBlock ([Expression (CallMethod
                             (Super, [Identifier ("w"); Identifier ("a")]));
                 Expression (Assign (Identifier ("cash"), Const (VInt (0))))])
     }
  
  ]]
  ; children_keys = []; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Person");
  dec_tree =
  Class
  ([], Name ("Child"), Some (Name ("Person")),
   [([Public], VarField (Int, [(Name ("cash"), None)]));
    ([Public],
     Constructor
     (Name ("Child"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Const (VInt (0))))])));
    ([Override; Public],
     Method
     (Int, Name ("getAge"), [],
      Some (StmtBlock ([Return (Some (Add
                                      (Identifier ("age"), Const (VInt (1)))))]))));
    ([Public],
     Method
     (Int, Name ("getCash"), [],
      Some (StmtBlock ([Return (Some (Identifier ("cash")))]))));
    ([Public],
     Method
     (Void, Name ("setCash"), [(Int, Name ("c"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("cash")),
                                     Identifier ("c")))]))));
    ([Public],
     Constructor
     (Name ("Child"),
      [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Identifier ("c")))])));
    ([Public],
     Method
     (Void, Name ("giveEvenNumbers100"), [],
      Some (StmtBlock ([For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("i"), Some (Const (VInt (0))))])),
                         Some (Less (Identifier ("i"), Const (VInt (100)))),
                         [PostInc (Identifier ("i"))],
                         StmtBlock ([If
                                     (And
                                      (Equal
                                       (Mod
                                        (Identifier ("i"), Const (VInt (2))),
                                        Const (VInt (0))),
                                       Not (Equal
                                            (Mod
                                             (Identifier ("i"),
                                              Const (VInt (2))),
                                             Const (VInt (1))))),
                                      StmtBlock ([Expression (FieldAccess
                                                              (FieldAccess
                                                               (Identifier ("System"),
                                                                Identifier ("out")),
                                                               CallMethod
                                                               (Identifier ("println"),
                                                                [Identifier ("i")])))]),
                                      Some (StmtBlock ([Continue])))]))]))))])
  }
  
  "Main" ->
   { this_key = "Main"; field_table = [[]]
                        ;
     method_table =
     [["equalsClassName (\"Object\")@@" ->
        { m_type = Int; is_abstract = false; is_overridable = true;
          has_override_annotation = false;
          args = [(ClassName ("Object"), Name ("obj"))];
          key = "equalsClassName (\"Object\")@@";
          body =
          Some (StmtBlock ([If
                            (Equal (This, Identifier ("obj")),
                             Return (Some (Const (VInt (1)))),
                             Some (Return (Some (Const (VInt (0))))))]));
          is_overriden = false }
     
  "toString@@" ->
   { m_type = String; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  "main@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "main@@";
     body =
     Some (StmtBlock ([VarDec
                       (None, ClassName ("Person"),
                        [(Name ("p"),
                          Some (ClassCreate
                                (Name ("Person"),
                                 [Const (VInt (80)); Const (VInt (45))])))])]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["Main$$" -> { key = "Main$$"; args = []; body = StmtBlock ([]) }
  
  ]]
  ; children_keys = []; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Object");
  dec_tree =
  Class
  ([Public], Name ("Main"), None,
   [([Public; Static],
     Method
     (Void, Name ("main"), [],
      Some (StmtBlock ([VarDec
                        (None, ClassName ("Person"),
                         [(Name ("p"),
                           Some (ClassCreate
                                 (Name ("Person"),
                                  [Const (VInt (80)); Const (VInt (45))])))])]))))])
  }
  
  "Object" ->
   { this_key = "Object"; field_table = [[]]
                          ;
     method_table =
     [["toString@@" ->
        { m_type = String; is_abstract = false; is_overridable = true;
          has_override_annotation = false; args = []; key = "toString@@";
          body =
          Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
          is_overriden = false }
     
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["Object$$" -> { key = "Object$$"; args = []; body = StmtBlock ([]) }
  
  ]]
  ; children_keys = ["Main"; "Person"]; is_abstract = false;
  is_inheritable = true; parent_key = None;
  dec_tree =
  Class
  ([Public], Name ("Object"), None,
   [([Public],
     Method
     (Int, Name ("equals"), [(ClassName ("Object"), Name ("obj"))],
      Some (StmtBlock ([If
                        (Equal (This, Identifier ("obj")),
                         Return (Some (Const (VInt (1)))),
                         Some (Return (Some (Const (VInt (0))))))]))));
    ([Public],
     Method
     (String, Name ("toString"), [],
      Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]))))])
  }
  
  "Person" ->
   { this_key = "Person";
     field_table =
     [["weight" ->
        { f_type = Int; key = "weight"; is_not_mutable = false; sub_tree = None
          }
     
  "age" ->
   { f_type = Int; key = "age"; is_not_mutable = false; sub_tree = None }
  
  ]]
  ;
  method_table =
  [["getWeight@@" ->
     { m_type = Int; is_abstract = false; is_overridable = true;
       has_override_annotation = false; args = []; key = "getWeight@@";
       body = Some (StmtBlock ([Return (Some (Identifier ("weight")))]));
       is_overriden = false }
  
  "getAge@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "getAge@@";
     body = Some (StmtBlock ([Return (Some (Identifier ("age")))]));
     is_overriden = false }
  
  "setAgeInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("a"))];
     key = "setAgeInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("age")),
                                    Identifier ("a")))]));
     is_overriden = false }
  
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  "toString@@" ->
   { m_type = String; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  "setWeightInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("w"))];
     key = "setWeightInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("weight")),
                                    Identifier ("w")))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["PersonIntInt$$" ->
     { key = "PersonIntInt$$"; args = [(Int, Name ("w")); (Int, Name ("a"))];
       body =
       StmtBlock ([Expression (Assign
                               (FieldAccess (This, Identifier ("weight")),
                                Identifier ("w")));
                   Expression (Assign
                               (FieldAccess (This, Identifier ("age")),
                                Identifier ("a")))])
       }
  
  ]]
  ; children_keys = ["Child"]; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Object");
  dec_tree =
  Class
  ([], Name ("Person"), None,
   [([Public], VarField (Int, [(Name ("weight"), None)]));
    ([Public], VarField (Int, [(Name ("age"), None)]));
    ([Public],
     Constructor
     (Name ("Person"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("weight")),
                               Identifier ("w")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("age")),
                               Identifier ("a")))])));
    ([Public],
     Method
     (Int, Name ("getWeight"), [],
      Some (StmtBlock ([Return (Some (Identifier ("weight")))]))));
    ([Public],
     Method
     (Int, Name ("getAge"), [],
      Some (StmtBlock ([Return (Some (Identifier ("age")))]))));
    ([Public],
     Method
     (Void, Name ("setWeight"), [(Int, Name ("w"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("weight")),
                                     Identifier ("w")))]))));
    ([Public],
     Method
     (Void, Name ("setAge"), [(Int, Name ("a"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("age")),
                                     Identifier ("a")))]))))])
  }
  
  -------------------SIMILAR_FIELDS-------------------
  
  Similar fields
  -------------------SIMILAR_METHODS_ERROR-------------------
  
  Method with this type exists
  -------------------SIMILAR_CONSTRUCTOR_ERROR-------------------
  
  Constructor with this type exists
  -------------------ABSTRACTNESS_ERRORS-------------------
  
  Abstract method in non-abstract class
  No body of non-abstract method
  Abstract method cannot have body
  Abstract method must be overriden
  -------------------FINAL_MODIFIERS_ERRORS-------------------
  
  Final class cannot be inherited
  ]]
  [["Child" ->
     { this_key = "Child";
       field_table =
       [["cash" ->
          { f_type = Int; key = "cash"; is_not_mutable = false; sub_tree = None
            }
       
  "weight" ->
   { f_type = Int; key = "weight"; is_not_mutable = false; sub_tree = None }
  
  "age" ->
   { f_type = Int; key = "age"; is_not_mutable = false; sub_tree = None }
  
  ]]
  ;
  method_table =
  [["getWeight@@" ->
     { m_type = Int; is_abstract = false; is_overridable = true;
       has_override_annotation = false; args = []; key = "getWeight@@";
       body = Some (StmtBlock ([Return (Some (Identifier ("weight")))]));
       is_overriden = false }
  
  "getCash@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "getCash@@";
     body = Some (StmtBlock ([Return (Some (Identifier ("cash")))]));
     is_overriden = false }
  
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  "setCashInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("c"))];
     key = "setCashInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("cash")),
                                    Identifier ("c")))]));
     is_overriden = false }
  
  "toString@@" ->
   { m_type = String; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["ChildIntIntInt$$" ->
     { key = "ChildIntIntInt$$";
       args = [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))];
       body =
       StmtBlock ([Expression (CallMethod
                               (Super, [Identifier ("w"); Identifier ("a")]));
                   Expression (Assign (Identifier ("cash"), Identifier ("c")))])
       }
  
  "ChildIntInt$$" ->
   { key = "ChildIntInt$$"; args = [(Int, Name ("w")); (Int, Name ("a"))];
     body =
     StmtBlock ([Expression (CallMethod
                             (Super, [Identifier ("w"); Identifier ("a")]));
                 Expression (Assign (Identifier ("cash"), Const (VInt (0))))])
     }
  
  ]]
  ; children_keys = []; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Person");
  dec_tree =
  Class
  ([], Name ("Child"), Some (Name ("Person")),
   [([Public], VarField (Int, [(Name ("cash"), None)]));
    ([Public],
     Constructor
     (Name ("Child"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Const (VInt (0))))])));
    ([Public],
     Method
     (Int, Name ("getCash"), [],
      Some (StmtBlock ([Return (Some (Identifier ("cash")))]))));
    ([Public],
     Method
     (Void, Name ("setCash"), [(Int, Name ("c"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("cash")),
                                     Identifier ("c")))]))));
    ([Public],
     Constructor
     (Name ("Child"),
      [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Identifier ("c")))])))])
  }
  
  "Main" ->
   { this_key = "Main"; field_table = [[]]
                        ;
     method_table =
     [["equalsClassName (\"Object\")@@" ->
        { m_type = Int; is_abstract = false; is_overridable = true;
          has_override_annotation = false;
          args = [(ClassName ("Object"), Name ("obj"))];
          key = "equalsClassName (\"Object\")@@";
          body =
          Some (StmtBlock ([If
                            (Equal (This, Identifier ("obj")),
                             Return (Some (Const (VInt (1)))),
                             Some (Return (Some (Const (VInt (0))))))]));
          is_overriden = false }
     
  "toString@@" ->
   { m_type = String; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  "main@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "main@@";
     body =
     Some (StmtBlock ([VarDec
                       (None, ClassName ("Person"),
                        [(Name ("p"),
                          Some (ClassCreate
                                (Name ("Person"),
                                 [Const (VInt (80)); Const (VInt (45))])))])]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["Main$$" -> { key = "Main$$"; args = []; body = StmtBlock ([]) }
  
  ]]
  ; children_keys = []; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Object");
  dec_tree =
  Class
  ([Public], Name ("Main"), None,
   [([Public; Static],
     Method
     (Void, Name ("main"), [],
      Some (StmtBlock ([VarDec
                        (None, ClassName ("Person"),
                         [(Name ("p"),
                           Some (ClassCreate
                                 (Name ("Person"),
                                  [Const (VInt (80)); Const (VInt (45))])))])]))))])
  }
  
  "Object" ->
   { this_key = "Object"; field_table = [[]]
                          ;
     method_table =
     [["toString@@" ->
        { m_type = String; is_abstract = false; is_overridable = true;
          has_override_annotation = false; args = []; key = "toString@@";
          body =
          Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
          is_overriden = false }
     
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["Object$$" -> { key = "Object$$"; args = []; body = StmtBlock ([]) }
  
  ]]
  ; children_keys = ["Main"; "Person"]; is_abstract = false;
  is_inheritable = true; parent_key = None;
  dec_tree =
  Class
  ([Public], Name ("Object"), None,
   [([Public],
     Method
     (Int, Name ("equals"), [(ClassName ("Object"), Name ("obj"))],
      Some (StmtBlock ([If
                        (Equal (This, Identifier ("obj")),
                         Return (Some (Const (VInt (1)))),
                         Some (Return (Some (Const (VInt (0))))))]))));
    ([Public],
     Method
     (String, Name ("toString"), [],
      Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]))))])
  }
  
  "Person" ->
   { this_key = "Person";
     field_table =
     [["weight" ->
        { f_type = Int; key = "weight"; is_not_mutable = false; sub_tree = None
          }
     
  "age" ->
   { f_type = Int; key = "age"; is_not_mutable = false; sub_tree = None }
  
  ]]
  ;
  method_table =
  [["getWeight@@" ->
     { m_type = Int; is_abstract = false; is_overridable = true;
       has_override_annotation = false; args = []; key = "getWeight@@";
       body = Some (StmtBlock ([Return (Some (Identifier ("weight")))]));
       is_overriden = false }
  
  "getAge@@" ->
   { m_type = Int; is_abstract = false; is_overridable = false;
     has_override_annotation = false; args = []; key = "getAge@@";
     body = Some (StmtBlock ([Return (Some (Identifier ("age")))]));
     is_overriden = false }
  
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  "toString@@" ->
   { m_type = String; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["PersonIntInt$$" ->
     { key = "PersonIntInt$$"; args = [(Int, Name ("w")); (Int, Name ("a"))];
       body =
       StmtBlock ([Expression (Assign
                               (FieldAccess (This, Identifier ("weight")),
                                Identifier ("w")));
                   Expression (Assign
                               (FieldAccess (This, Identifier ("age")),
                                Identifier ("a")))])
       }
  
  ]]
  ; children_keys = ["Child"]; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Object");
  dec_tree =
  Class
  ([], Name ("Person"), None,
   [([Public], VarField (Int, [(Name ("age"), None)]));
    ([Public], VarField (Int, [(Name ("weight"), None)]));
    ([Public],
     Constructor
     (Name ("Person"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("weight")),
                               Identifier ("w")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("age")),
                               Identifier ("a")))])));
    ([Public],
     Method
     (Int, Name ("getWeight"), [],
      Some (StmtBlock ([Return (Some (Identifier ("weight")))]))));
    ([Final; Public],
     Method
     (Int, Name ("getAge"), [],
      Some (StmtBlock ([Return (Some (Identifier ("age")))]))))])
  }
  
  -------------------@OVERRIDE_ERRORS-------------------
  
  @Override annotation on not overriden method
  ]]




  $ (cd ../../../../default && demos/demoInterpreter.exe)
  ------------------- FIRST TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["a" ->
       { v_type = Int; v_key = "a"; is_not_mutable = false;
         assignment_count = 1; v_value = VInt (1); scope_level = 0 }
    
  "b" ->
   { v_type = Int; v_key = "b"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (2); scope_level = 0 }
  
  "c" ->
   { v_type = Int; v_key = "c"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (3); scope_level = 0 }
  
  ]]
  ; last_expr_result = VInt (3); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 0; is_creation = false; constr_affilation = None
  }
  
  ------------------- LITTLE ARITHMETIC TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["val2" ->
       { v_type = Int; v_key = "val2"; is_not_mutable = false;
         assignment_count = 1; v_value = VInt (3); scope_level = 0 }
    
  "s1" ->
   { v_type = String; v_key = "s1"; is_not_mutable = false;
     assignment_count = 1; v_value = VString ("a"); scope_level = 0 }
  
  "a" ->
   { v_type = Int; v_key = "a"; is_not_mutable = false; assignment_count = 2;
     v_value = VInt (2); scope_level = 0 }
  
  "s3" ->
   { v_type = String; v_key = "s3"; is_not_mutable = false;
     assignment_count = 1; v_value = VString ("ab"); scope_level = 0 }
  
  "val7" ->
   { v_type = Int; v_key = "val7"; is_not_mutable = false;
     assignment_count = 1; v_value = VInt (124); scope_level = 0 }
  
  "val1" ->
   { v_type = Int; v_key = "val1"; is_not_mutable = false;
     assignment_count = 1; v_value = VInt (15); scope_level = 0 }
  
  "s4" ->
   { v_type = String; v_key = "s4"; is_not_mutable = false;
     assignment_count = 1; v_value = VString ("a2"); scope_level = 0 }
  
  "val3" ->
   { v_type = Int; v_key = "val3"; is_not_mutable = false;
     assignment_count = 1; v_value = VInt (101); scope_level = 0 }
  
  "b" ->
   { v_type = Int; v_key = "b"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (2); scope_level = 0 }
  
  "s5" ->
   { v_type = String; v_key = "s5"; is_not_mutable = false;
     assignment_count = 1; v_value = VString ("2b"); scope_level = 0 }
  
  "val4" ->
   { v_type = Int; v_key = "val4"; is_not_mutable = false;
     assignment_count = 1; v_value = VInt (5); scope_level = 0 }
  
  "s2" ->
   { v_type = String; v_key = "s2"; is_not_mutable = false;
     assignment_count = 1; v_value = VString ("b"); scope_level = 0 }
  
  "val5" ->
   { v_type = Int; v_key = "val5"; is_not_mutable = false;
     assignment_count = 1; v_value = VInt (0); scope_level = 0 }
  
  "val6" ->
   { v_type = Int; v_key = "val6"; is_not_mutable = false;
     assignment_count = 1; v_value = VInt (300); scope_level = 0 }
  
  "c" ->
   { v_type = Int; v_key = "c"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (3); scope_level = 0 }
  
  ]]
  ; last_expr_result = VString ("2b"); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 0; is_creation = false; constr_affilation = None
  }
  
  ------------------- BOOLEAN EXPRESSIONS TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["s1" ->
       { v_type = String; v_key = "s1"; is_not_mutable = false;
         assignment_count = 1; v_value = VString ("a"); scope_level = 0 }
    
  "a" ->
   { v_type = Int; v_key = "a"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (10); scope_level = 0 }
  
  "d" ->
   { v_type = Int; v_key = "d"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (10); scope_level = 0 }
  
  "meVal" ->
   { v_type = Int; v_key = "meVal"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  "p2" ->
   { v_type = ClassName ("Person"); v_key = "p2"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Person";
                         field_ref_table =
                         [["age" ->
                            { key = "age"; f_type = Int; f_value = VInt (30);
                              is_not_mutable = false; assignment_count = 0 }
                         
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Alice");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 2 })); scope_level = 0
  }
  
  "b" ->
   { v_type = Int; v_key = "b"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (50); scope_level = 0 }
  
  "objNEq" ->
   { v_type = Int; v_key = "objNEq"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  "eVal" ->
   { v_type = Int; v_key = "eVal"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  "p3" ->
   { v_type = ClassName ("Person"); v_key = "p3"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Person";
                         field_ref_table =
                         [["age" ->
                            { key = "age"; f_type = Int; f_value = VInt (20);
                              is_not_mutable = false; assignment_count = 0 }
                         
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "p1" ->
   { v_type = ClassName ("Person"); v_key = "p1"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Person";
                         field_ref_table =
                         [["age" ->
                            { key = "age"; f_type = Int; f_value = VInt (20);
                              is_not_mutable = false; assignment_count = 0 }
                         
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "s2" ->
   { v_type = String; v_key = "s2"; is_not_mutable = false;
     assignment_count = 1; v_value = VString ("b"); scope_level = 0 }
  
  "lVal" ->
   { v_type = Int; v_key = "lVal"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  "sEq" ->
   { v_type = Int; v_key = "sEq"; is_not_mutable = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "notVal" ->
   { v_type = Int; v_key = "notVal"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  "objEq" ->
   { v_type = Int; v_key = "objEq"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  "leVal" ->
   { v_type = Int; v_key = "leVal"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  "mVal" ->
   { v_type = Int; v_key = "mVal"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  "orVal" ->
   { v_type = Int; v_key = "orVal"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  "c" ->
   { v_type = Int; v_key = "c"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (100); scope_level = 0 }
  
  "sNEQ" ->
   { v_type = Int; v_key = "sNEQ"; is_not_mutable = false;
     assignment_count = 1; v_value = VInt (1); scope_level = 0 }
  
  "neVal" ->
   { v_type = Int; v_key = "neVal"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  "andVal" ->
   { v_type = Int; v_key = "andVal"; is_not_mutable = false;
     assignment_count = 2; v_value = VInt (1); scope_level = 0 }
  
  ]]
  ; last_expr_result = VInt (1); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 2; is_creation = false; constr_affilation = None
  }
  
  ------------------- SIMPLE METHOD CALL TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["a2" ->
       { v_type = Int; v_key = "a2"; is_not_mutable = false;
         assignment_count = 1; v_value = VInt (30); scope_level = 0 }
    
  "a1" ->
   { v_type = Int; v_key = "a1"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (25); scope_level = 0 }
  
  "res" ->
   { v_type = Int; v_key = "res"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (125); scope_level = 0 }
  
  "person" ->
   { v_type = ClassName ("Person"); v_key = "person"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Person";
                         field_ref_table =
                         [["age" ->
                            { key = "age"; f_type = Int; f_value = VInt (30);
                              is_not_mutable = false; assignment_count = 1 }
                         
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  ]]
  ; last_expr_result = VInt (30); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 1; is_creation = false; constr_affilation = None
  }
  
  ------------------- UPDATE OBJECT STATE IN MAIN TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["p2" ->
       { v_type = ClassName ("Person"); v_key = "p2"; is_not_mutable = false;
         assignment_count = 1;
         v_value =
         VObjectRef (RObj ({ class_key = "Person";
                             field_ref_table =
                             [["age" ->
                                { key = "age"; f_type = Int;
                                  f_value = VInt (55); is_not_mutable = false;
                                  assignment_count = 1 }
                             
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "p3" ->
   { v_type = ClassName ("Person"); v_key = "p3"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Person";
                         field_ref_table =
                         [["age" ->
                            { key = "age"; f_type = Int; f_value = VInt (55);
                              is_not_mutable = false; assignment_count = 1 }
                         
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "res" ->
   { v_type = Int; v_key = "res"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (55); scope_level = 0 }
  
  "p1" ->
   { v_type = ClassName ("Person"); v_key = "p1"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Person";
                         field_ref_table =
                         [["age" ->
                            { key = "age"; f_type = Int; f_value = VInt (55);
                              is_not_mutable = false; assignment_count = 1 }
                         
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "person" ->
   { v_type = ClassName ("Person"); v_key = "person"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Person";
                         field_ref_table =
                         [["age" ->
                            { key = "age"; f_type = Int; f_value = VInt (55);
                              is_not_mutable = false; assignment_count = 1 }
                         
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  ]]
  ; last_expr_result = VInt (55); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 1; is_creation = false; constr_affilation = None
  }
  
  ------------------- CHILD WORKING TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["childSecond" ->
       { v_type = ClassName ("Child"); v_key = "childSecond";
         is_not_mutable = false; assignment_count = 1;
         v_value =
         VObjectRef (RObj ({ class_key = "Child";
                             field_ref_table =
                             [["parent" ->
                                { key = "parent";
                                  f_type = ClassName ("Person");
                                  f_value =
                                  VObjectRef (RObj ({ class_key = "Person";
                                                      field_ref_table =
                                                      [["age" ->
                                                         { key = "age";
                                                           f_type = Int;
                                                           f_value = VInt (27);
                                                           is_not_mutable =
                                                           false;
                                                           assignment_count = 1
                                                           }
                                                      
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); is_not_mutable = false; assignment_count = 0
  }
  
  "age" ->
   { key = "age"; f_type = Int; f_value = VInt (20); is_not_mutable = false;
     assignment_count = 1 }
  
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 4 })); scope_level = 0
  }
  
  "person" ->
   { v_type = ClassName ("Person"); v_key = "person"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Person";
                         field_ref_table =
                         [["age" ->
                            { key = "age"; f_type = Int; f_value = VInt (27);
                              is_not_mutable = false; assignment_count = 1 }
                         
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "childFirst" ->
   { v_type = ClassName ("Child"); v_key = "childFirst";
     is_not_mutable = false; assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Child";
                         field_ref_table =
                         [["parent" ->
                            { key = "parent"; f_type = ClassName ("Person");
                              f_value =
                              VObjectRef (RObj ({ class_key = "Person";
                                                  field_ref_table =
                                                  [["age" ->
                                                     { key = "age";
                                                       f_type = Int;
                                                       f_value = VInt (40);
                                                       is_not_mutable = false;
                                                       assignment_count = 0 }
                                                  
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Flexer");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 3 })); is_not_mutable = false; assignment_count = 0
  }
  
  "age" ->
   { key = "age"; f_type = Int; f_value = VInt (4); is_not_mutable = false;
     assignment_count = 1 }
  
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Alice");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 2 })); scope_level = 0
  }
  
  ]]
  ;
  last_expr_result =
  VObjectRef (RObj ({ class_key = "Person";
                      field_ref_table =
                      [["age" ->
                         { key = "age"; f_type = Int; f_value = VInt (27);
                           is_not_mutable = false; assignment_count = 1 }
                      
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); runtime_signal = NoSignal; curr_method_type = Void;
  is_main_scope = true; nested_loops_cnt = 0; scope_level = 0;
  cur_constr_key = None; prev_context = None; obj_created_cnt = 4;
  is_creation = false; constr_affilation = None
  }
  
  ------------------- SCOPE TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["a" ->
       { v_type = Int; v_key = "a"; is_not_mutable = false;
         assignment_count = 5; v_value = VInt (1000); scope_level = 0 }
    
  "b" ->
   { v_type = Int; v_key = "b"; is_not_mutable = false; assignment_count = 4;
     v_value = VInt (2000); scope_level = 0 }
  
  "c" ->
   { v_type = Int; v_key = "c"; is_not_mutable = false; assignment_count = 4;
     v_value = VInt (3000); scope_level = 0 }
  
  "i" ->
   { v_type = Int; v_key = "i"; is_not_mutable = false; assignment_count = 4;
     v_value = VInt (3); scope_level = 0 }
  
  ]]
  ; last_expr_result = VInt (3000); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 1; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 0; is_creation = false; constr_affilation = None
  }
  
  ------------------- MANY CYCLES TEST + ARRAY SORTING ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["n" ->
       { v_type = Int; v_key = "n"; is_not_mutable = false;
         assignment_count = 1; v_value = VInt (11); scope_level = 0 }
    
  "arr" ->
   { v_type = Array (Int); v_key = "arr"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VArray (Arr ({ a_type = Int;
                    values =
                    [VInt (0); VInt (1); VInt (2); VInt (3); VInt (4);
                     VInt (5); VInt (6); VInt (7); VInt (8); VInt (9);
                     VInt (10)];
                    number = 1 }));
     scope_level = 0 }
  
  "i" ->
   { v_type = Int; v_key = "i"; is_not_mutable = false; assignment_count = 11;
     v_value = VInt (10); scope_level = 1 }
  
  ]]
  ; last_expr_result = VBool (false); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 2; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 1; is_creation = false; constr_affilation = None
  }
  
  ------------------- BREAK AND CONTINUE TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["b" ->
       { v_type = Array (Int); v_key = "b"; is_not_mutable = false;
         assignment_count = 1;
         v_value =
         VArray (Arr ({ a_type = Int;
                        values =
                        [VInt (0); VInt (1); VInt (2); VInt (3); VInt (4);
                         VInt (5); VInt (6); VInt (7); VInt (8); VInt (9);
                         VInt (10); VInt (11); VInt (12); VInt (13); VInt (14);
                         VInt (0); VInt (0); VInt (0); VInt (0); VInt (0)];
                        number = 2 }));
         scope_level = 1 }
    
  "arr" ->
   { v_type = Array (Int); v_key = "arr"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VArray (Arr ({ a_type = Int;
                    values =
                    [VInt (0); VInt (1); VInt (0); VInt (1); VInt (0);
                     VInt (1); VInt (0); VInt (1); VInt (0); VInt (1)];
                    number = 1 }));
     scope_level = 0 }
  
  ]]
  ; last_expr_result = VInt (16); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 2; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 2; is_creation = false; constr_affilation = None
  }
  
  ------------------- ARRAY SORT AS FUNCTION (CHECKING CHANGE OF ARRAY STATE IN OTHER CONTEXT) ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["bubbleSorter" ->
       { v_type = ClassName ("BubbleSorter"); v_key = "bubbleSorter";
         is_not_mutable = false; assignment_count = 1;
         v_value =
         VObjectRef (RObj ({ class_key = "BubbleSorter";
                             field_ref_table = [[]]
                             ; number = 2 }));
         scope_level = 0 }
    
  "arr" ->
   { v_type = Array (Int); v_key = "arr"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VArray (Arr ({ a_type = Int;
                    values =
                    [VInt (0); VInt (1); VInt (2); VInt (3); VInt (4);
                     VInt (5); VInt (6); VInt (7); VInt (8); VInt (9);
                     VInt (10); VInt (11); VInt (12); VInt (13); VInt (14);
                     VInt (15)];
                    number = 1 }));
     scope_level = 0 }
  
  ]]
  ; last_expr_result = VBool (false); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 2; is_creation = false; constr_affilation = None
  }
  
  ------------------- CHANGE OF OBJECT STATE IN OTHER CONTEXT ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["child" ->
       { v_type = ClassName ("Child"); v_key = "child"; is_not_mutable = false;
         assignment_count = 1;
         v_value =
         VObjectRef (RObj ({ class_key = "Child";
                             field_ref_table =
                             [["parent" ->
                                { key = "parent";
                                  f_type = ClassName ("Person");
                                  f_value =
                                  VObjectRef (RObj ({ class_key = "Person";
                                                      field_ref_table =
                                                      [["age" ->
                                                         { key = "age";
                                                           f_type = Int;
                                                           f_value = VInt (30);
                                                           is_not_mutable =
                                                           false;
                                                           assignment_count = 1
                                                           }
                                                      
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); is_not_mutable = false; assignment_count = 0
  }
  
  "age" ->
   { key = "age"; f_type = Int; f_value = VInt (0); is_not_mutable = false;
     assignment_count = 0 }
  
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 2 })); scope_level = 0
  }
  
  "person" ->
   { v_type = ClassName ("Person"); v_key = "person"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Person";
                         field_ref_table =
                         [["age" ->
                            { key = "age"; f_type = Int; f_value = VInt (30);
                              is_not_mutable = false; assignment_count = 1 }
                         
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  ]]
  ;
  last_expr_result =
  VObjectRef (RObj ({ class_key = "Person";
                      field_ref_table =
                      [["age" ->
                         { key = "age"; f_type = Int; f_value = VInt (30);
                           is_not_mutable = false; assignment_count = 1 }
                      
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); runtime_signal = NoSignal; curr_method_type = Void;
  is_main_scope = true; nested_loops_cnt = 0; scope_level = 0;
  cur_constr_key = None; prev_context = None; obj_created_cnt = 2;
  is_creation = false; constr_affilation = None
  }
  
  ------------------- PATTERN VISITOR TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["resPerimeter" ->
       { v_type = Array (Int); v_key = "resPerimeter"; is_not_mutable = false;
         assignment_count = 1;
         v_value =
         VArray (Arr ({ a_type = Int; values = [VInt (75); VInt (8); VInt (0)];
                        number = 7 }));
         scope_level = 0 }
    
  "areaVisitor" ->
   { v_type = ClassName ("AreaVisitor"); v_key = "areaVisitor";
     is_not_mutable = false; assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "AreaVisitor"; field_ref_table = [[]]
                                                    ;
                         number = 5 }));
     scope_level = 0 }
  
  "list" ->
   { v_type = Array (ClassName ("Figure")); v_key = "list";
     is_not_mutable = false; assignment_count = 1;
     v_value =
     VArray (Arr ({ a_type = ClassName ("Figure");
                    values =
                    [VObjectRef (RObj ({ class_key = "Circle";
                                         field_ref_table =
                                         [["radius" ->
                                            { key = "radius"; f_type = Int;
                                              f_value = VInt (5);
                                              is_not_mutable = false;
                                              assignment_count = 0 }
                                         
  ]]
  ; number = 1 }));
  VObjectRef (RObj ({ class_key = "Rectangle";
                      field_ref_table =
                      [["a" ->
                         { key = "a"; f_type = Int; f_value = VInt (2);
                           is_not_mutable = false; assignment_count = 0 }
                      
  "b" ->
   { key = "b"; f_type = Int; f_value = VInt (4); is_not_mutable = false;
     assignment_count = 0 }
  
  ]]
  ; number = 2 }));
  VObjectRef (RObj ({ class_key = "Triangle";
                      field_ref_table =
                      [["a" ->
                         { key = "a"; f_type = Int; f_value = VInt (1);
                           is_not_mutable = false; assignment_count = 0 }
                      
  "b" ->
   { key = "b"; f_type = Int; f_value = VInt (1); is_not_mutable = false;
     assignment_count = 0 }
  
  "c" ->
   { key = "c"; f_type = Int; f_value = VInt (1); is_not_mutable = false;
     assignment_count = 0 }
  
  ]]
  ; number = 3 }))]; number = 4 })); scope_level = 0
  }
  
  "resArea" ->
   { v_type = Array (Int); v_key = "resArea"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VArray (Arr ({ a_type = Int; values = [VInt (30); VInt (12); VInt (3)];
                    number = 8 }));
     scope_level = 0 }
  
  "perimeterVisitor" ->
   { v_type = ClassName ("PerimeterVisitor"); v_key = "perimeterVisitor";
     is_not_mutable = false; assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "PerimeterVisitor";
                         field_ref_table = [[]]
                         ; number = 6 }));
     scope_level = 0 }
  
  ]]
  ; last_expr_result = VBool (false); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 2; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 8; is_creation = false; constr_affilation = None
  }
  
  ------------------- RECURSION TEST (FACTORIAL) ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["f" ->
       { v_type = Int; v_key = "f"; is_not_mutable = false;
         assignment_count = 1; v_value = VInt (120); scope_level = 0 }
    
  "factorial" ->
   { v_type = ClassName ("Factorial"); v_key = "factorial";
     is_not_mutable = false; assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "Factorial"; field_ref_table = [[]]
                                                  ;
                         number = 1 }));
     scope_level = 0 }
  
  ]]
  ; last_expr_result = VInt (120); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 1; is_creation = false; constr_affilation = None
  }
  
  ------------------- RECURSION TEST (QUICK SORT) ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["n" ->
       { v_type = Int; v_key = "n"; is_not_mutable = false;
         assignment_count = 1; v_value = VInt (16); scope_level = 0 }
    
  "quickSorter" ->
   { v_type = ClassName ("QuickSorter"); v_key = "quickSorter";
     is_not_mutable = false; assignment_count = 1;
     v_value =
     VObjectRef (RObj ({ class_key = "QuickSorter"; field_ref_table = [[]]
                                                    ;
                         number = 2 }));
     scope_level = 0 }
  
  "arr" ->
   { v_type = Array (Int); v_key = "arr"; is_not_mutable = false;
     assignment_count = 1;
     v_value =
     VArray (Arr ({ a_type = Int;
                    values =
                    [VInt (0); VInt (1); VInt (2); VInt (3); VInt (4);
                     VInt (5); VInt (6); VInt (7); VInt (8); VInt (9);
                     VInt (10); VInt (11); VInt (12); VInt (13); VInt (14);
                     VInt (15)];
                    number = 1 }));
     scope_level = 0 }
  
  "high" ->
   { v_type = Int; v_key = "high"; is_not_mutable = false;
     assignment_count = 1; v_value = VInt (15); scope_level = 0 }
  
  "low" ->
   { v_type = Int; v_key = "low"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (0); scope_level = 0 }
  
  ]]
  ; last_expr_result = VBool (false); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 2; is_creation = false; constr_affilation = None
  }
  
  ------------------- ARRAY_TYPE_MISMATCH_EXCEPTION ERROR ------------------
  Wrong assign type!
  ------------------- CONSTRUCTOR CHAINING TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["cat" ->
       { v_type = ClassName ("Cat"); v_key = "cat"; is_not_mutable = false;
         assignment_count = 1;
         v_value =
         VObjectRef (RObj ({ class_key = "Cat";
                             field_ref_table =
                             [["age" ->
                                { key = "age"; f_type = Int;
                                  f_value = VInt (2); is_not_mutable = false;
                                  assignment_count = 0 }
                             
  "hairLevel" ->
   { key = "hairLevel"; f_type = Int; f_value = VInt (30);
     is_not_mutable = false; assignment_count = 0 }
  
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Mars");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  ]]
  ;
  last_expr_result =
  VObjectRef (RObj ({ class_key = "Cat";
                      field_ref_table =
                      [["age" ->
                         { key = "age"; f_type = Int; f_value = VInt (2);
                           is_not_mutable = false; assignment_count = 0 }
                      
  "hairLevel" ->
   { key = "hairLevel"; f_type = Int; f_value = VInt (30);
     is_not_mutable = false; assignment_count = 0 }
  
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Mars");
     is_not_mutable = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); runtime_signal = NoSignal; curr_method_type = Void;
  is_main_scope = true; nested_loops_cnt = 0; scope_level = 0;
  cur_constr_key = None; prev_context = None; obj_created_cnt = 1;
  is_creation = false; constr_affilation = None
  }
  
  ------------------- CONSTRUCTOR CHAINING RECURSION ------------------
  Constructor recursion!
  ------------------- FINAL FIELDS TEST ------------------
  Assignment to a constant field
  ------------------- FINAL VARIABLES TEST ------------------
  Assignment to a constant variable
  ------------------- OVERLOADING TEST ------------------
  { cur_object =
    RObj ({ class_key = "Main"; field_ref_table = [[]]
                                ; number = 0 });
    var_table =
    [["summator" ->
       { v_type = ClassName ("Summator"); v_key = "summator";
         is_not_mutable = false; assignment_count = 1;
         v_value =
         VObjectRef (RObj ({ class_key = "Summator"; field_ref_table = [[]]
                                                     ;
                             number = 1 }));
         scope_level = 0 }
    
  "a" ->
   { v_type = Int; v_key = "a"; is_not_mutable = false; assignment_count = 1;
     v_value = VInt (8); scope_level = 0 }
  
  "s" ->
   { v_type = String; v_key = "s"; is_not_mutable = false;
     assignment_count = 1; v_value = VString ("GGWP"); scope_level = 0 }
  
  ]]
  ; last_expr_result = VString ("GGWP"); runtime_signal = NoSignal;
  curr_method_type = Void; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 1; is_creation = false; constr_affilation = None
  }
  
