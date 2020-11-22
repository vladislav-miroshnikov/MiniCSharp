open Java_lib.Parser
open Opal
open Java_lib.Ast

let rec print_list : class_dec list -> unit = function
  | [] -> print_string ""
  | e :: l ->
      print_string (show_class_dec e);
      print_endline "";
      print_list l

let get_list_option opt = match opt with Some x -> x | None -> []

let value =
  get_list_option
    (parse parser
       (LazyStream.of_string
          {| 
public class Main
{
	public static void main(String[] args) {
		Person p = new Person(80, 45);
		System.out.println(p.getWeight());
		
		Child ch = new Child(66, 20);
		ch.setCash(50);
		ch.giveEvenNumbers100();
	    
	}
}

class Person {
    public int weight;
    public int age;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
    
    
    public int getWeight() {
        return weight;
    }
    
    public int getAge() {
        return age;
    }
    
    public void setWeight(int w) {
        this.weight = w;
    }
    public void setAge(int a) {
        this.age = a;
    }
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w,a);
        cash = 0;
    }
    
    public int getCash() {
        return cash;
    }
    
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }
    
    public void giveEvenNumbers100() {
        for (int i = 0; i < 100; i++) {
            if (i % 2 == 0 && !(i % 2 == 1)) {
                System.out.println(i);
            }
            else {
                continue;
            }
        }
    }
    
}
|}))

let testClass =
  let () = print_list value in
  ()
