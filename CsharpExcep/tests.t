  $ (cd ../../../default && demos/demoFirst.exe) 
  Class
  ([], "Program", None,
   [([Static],
     Method
     (Void, "Main", [],
      StatementBlock ([Expression (CallMethod ("E3", []));
                       Throw (IdentVar ("e"))])));
    ([Public; Static],
     Method
     (Void, "A3", [(Int, "a"); (String, "b")],
      StatementBlock ([VarDeclare
                       (None, Int, [("x", Some (ConstExpr (VInt (0))))]);
                       Try
                       (StatementBlock ([Throw (ClassCreate ("cat", []))]), 
                        [], Some (StatementBlock ([Print (IdentVar ("x"))])));
                       VarDeclare
                       (None, Int, [("a", Some (ConstExpr (VInt (3))))]);
                       Expression (PostInc (IdentVar ("a")))])));
    ([Public; Static],
     Method
     (Void, "B3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("A3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("B")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "C3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("B3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("C")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "D3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("C3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("D")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "E3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("D3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("E")))]))],
                        None)])))])
  Class
  ([], "ShittyExn", Some ("Exception"),
   [([Public],
     Constructor
     ("ShittyExn", [],
      StatementBlock ([Expression (Assign (IdentVar ("f"), IdentVar ("f")))])));
    ([Public],
     Method
     (Bool, "Filter", [],
      StatementBlock ([Return (Some (CallMethod ("f", [])))])))])
  $ (cd ../../../default && demos/demoLoad.exe)
  ------------------- FIRST TEST ------------------
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["a" ->
       { var_key = "a"; var_type = Int; var_value = VInt (1); is_const = false;
         assignment_count = 1; visibility_level = 0 }
    
  "b" ->
   { var_key = "b"; var_type = Int; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "c" ->
   { var_key = "c"; var_type = Int; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_meth_type = Void; last_expr_result = VInt (3);
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
