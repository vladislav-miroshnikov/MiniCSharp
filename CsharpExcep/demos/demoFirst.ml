open Csharp_lib.Ast
open Csharp_lib.Parser

let rec print_list : cs_class list -> unit = function
  | [] -> print_string ""
  | head :: tl ->
      print_string (show_cs_class head) ;
      print_endline "" ;
      print_list tl

let get_value_list list = match list with Some x -> x | None -> []

let parse_input =
  get_value_list
    (apply_parser parser
       {|  class Program
    {
        static void Main()
        {
            TrickyTest21();
        }

       public static bool Filter(string msg)
       	{
	        Console.WriteLine(msg);
            return true;
      	}

       public static void A21()
        {
            try
            {
                throw new DivideByZeroException();
            }
            finally
            {
                Console.WriteLine("A21.finally()");
                throw new NullReferenceException();
            }
            Console.WriteLine("Resuming A2");
        }

        public static void B21()
        {
            try
            {
                A21();
            }
            catch (DivideByZeroException) when (Filter("filter B21"))
            {
                Console.WriteLine("B21 DivideByZeroException");
            }
            finally
            {
                Console.WriteLine("B21.finally()");
            }

            Console.WriteLine("Resuming B21");
        }

        public static void TrickyTest21()
        {
            try
            {
                B21();
            }
            finally
            {
                Console.WriteLine("TrickyTest Finally");
            }

            Console.WriteLine("Resuming TrickyTest");
        }
    } |})

let testClass =
  let () = print_list parse_input in
  ()
