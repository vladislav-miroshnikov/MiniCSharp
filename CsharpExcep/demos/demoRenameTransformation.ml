open Format
open Csharp_lib.Rename_transform

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- First rename test -_-_-_-_-_-_-_-_-_-_-\n\n"

let input =
  {|
 public class Program
{
  public static void Main()
  {
    Factorial factorial = new Factorial();
    int f = factorial.GetFact(5);
  }
}

public class Factorial
{
  public int GetFact (int n)
  {
    if (n <= 1) return 1;
    else return n * GetFact(n - 1);
  }
}
        |}

let () = start_transform_rename input "n" "num" std_formatter

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Second rename test -_-_-_-_-_-_-_-_-_-_-\n\n"

let input =
  {|
        public class Program {

            static void Main()
        {
            E3();
        }


        public static void A3()
        {
            try
            {
                throw e;
            }
            finally
            {
                
            }
        }
        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (false)
            {
                Console.WriteLine("B");
            }
        }
        public static void C3()
        {
            try
            {
                B3();
            }
            catch (ShittyExn e) when (false)
            {
                Console.WriteLine("C");
            }
        }
        public static void D3()
        {
            try
            {
                C3();
            }
            catch (ShittyExn e) when (false)
            {
                Console.WriteLine(e);
            }
        }
        public static void E3()
        {
            try
            {
                D3();
            }
            catch (ShittyExn e) when (e.Filter())
            {
                Console.WriteLine("E");
            }
        }
    }

    class ShittyExn : Exception
        {
            public bool f;
            public ShittyExn(bool val)
            {
               f = val;
            }

            public bool Filter()
            {
                return f;
            }
        }
        |}

let () = start_transform_rename input "e" "excep" std_formatter

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Third rename test -_-_-_-_-_-_-_-_-_-_-\n\n"

let input =
  {|
        public class Program {

            public static void Main() {
                int andVal = 0, orVal = 0, notVal = 0;
        int mVal = 0, meVal = 0, lVal = 0, leVal = 0;
        int eVal = 0, neVal = 0;
        
        int a = 10, b = 50, c = 100;
        if (b > a) {
            mVal = 1;
        }
        if (a < c) {
            lVal = 1;
        } 
        if (b <= c) {
            leVal = 1;
        }
        if (c >= b) {
            meVal = 1;
        }
        int d = 10;
        if (d == a) {
            eVal = 1;
        }
        if (a != b) {
            neVal = 1;
        }
        if (a < b && b < c) {
            andVal = 1;
        }
        if (a < b || a != 10) {
            orVal = 1;
        }
        if (!(a >= c)) {
            notVal = 1;
        }
        string s1 = "a", s2 = "b";
        int sEq = 0, sNEQ; 
        if (s1 == "a") {
            sEq = 1;
        }
        if (s2 != "a") {
            sNEQ = 1;
        }
        Person p1 = new Person(20, "Bob"), p2 = new Person(30, "Alice"), p3 = p1;
        int objEq = 0, objNEq = 0; 
        if (p1 != p2) {
            objNEq = 1;
        }
        if (p1 == p3) {
            objEq = 1;
        }



        }  
     }

    class Person 
    {
    int age;
    string name;

    public Person(int agew, string namew) {
        age = agew;
        name = namew;
    }

    } 
      |}

let () = start_transform_rename input "a" "magic" std_formatter
