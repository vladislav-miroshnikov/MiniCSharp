### An implementaion of Lambda mini-language

This is a homework for functional programming course.

License: LGPL

Author: Vasy Pupkin, vasya@pupkin.com

Features done (append only):

- Parser  (for example)
- interpreter of non-recursive functions (for example)
- ...

Features in progress (and TODOs):

- Interpreter of recursive functions is not yet ready  (for example)
- TODO: make pretty-printing less memory consuming (for example)
- ...


##### Замечания по стилю кодирования

Пока оно не проверяется в CI, но может неожиданно начать проверяться.

- если merge request не проходит CI -- проверять не буду
- имена типов и функций -- snake_case
- имена типов модулей и модулей -- CamelCase 
- ворнинги должны быть пофикшены
- Не стесняйтесь писать `if ... then ... else` вместо `match ... with true -> .. | false -> ...`
- Не стесняйтесь писать гварды в мэтчинге, например 
```ocaml
match ... with 
| x when f x -> ...
| x          -> ...
| ...
```
вместо 
```ocaml
match ... with 
| x -> if f x then ... else ...
| ...
```
- вместо `fun x y -> match y with` лучше писать короче: `fun x -> function`
- используйте quoted string literals в тестах, чтобы не экранировать руками 
```
─( 11:21:01 )─< command 1 >────────────────────────────
utop # {|
  int main () {
    return 0;
  }
  |};;
- : string = "\n  int main () {\n    return 0;\n  }\n  "
```
- Не надо писать 
```ocaml
match ... with 
| x -> 
    Hashtbl.replace tbl key value |> fun () -> ...
```
Лучше 
```ocaml
match ... with 
| x -> 
    let () = Hashtbl.replace tbl key value in 
    ...
```
или
```ocaml
match ... with 
| x -> (
    Hashtbl.replace tbl key value;
    ...
  )
```
или даже
```ocaml
match ... with 
| x -> begin
    Hashtbl.replace tbl key value;
    ...
  end
```
- Не надо писать 
```ocaml
let x = if long_expression then true else false in ...
```
лучше
```ocaml
let x = long_expression in ...
```

- 1
