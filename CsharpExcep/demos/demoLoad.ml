open Csharp_lib.Parser
open Csharp_lib.Interpreter.ClassLoader (Csharp_lib.Interpreter.Result)
open Csharp_lib.Interpreter.Interpreter (Csharp_lib.Interpreter.Result)

let test_interp test_val cl_t =
  match load_classes test_val cl_t with
  | Error m -> print_endline m ; Hashtbl.clear cl_t
  | Ok load_table -> (
    match interprete_program load_table with
    | Error m -> print_endline m ; Hashtbl.clear load_table
    | Ok res_context ->
        print_endline (show_context res_context ^ "\n") ;
        Hashtbl.clear load_table )

let () = print_string "------------------- FIRST TEST ------------------\n"

let test_val =
  Option.get
    (apply_parser parser
       {|
        class Program {

            public static void Main() {
                int a = 1;
                int b = 2;
                int c = 3;
            }  
        }
        |})

let () = test_interp test_val (Hashtbl.create 100)
