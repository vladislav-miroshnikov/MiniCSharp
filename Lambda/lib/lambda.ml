type name = string

type t = Var of name | Abs of name * t | App of t * t

(* smart constructors *)
let var x = Var x

let abs x y = Abs (x, y)

let app x y = App (x, y)

let rec pp_lam fmt = function
  | Var s -> Format.fprintf fmt "%s" s
  | App (l, r) -> Format.fprintf fmt "(%a %a)" pp_lam l pp_lam r
  | Abs (v1, Abs (v2, Abs (v3, Abs (v4, t)))) ->
      Format.fprintf fmt "(λ %s %s %s %s -> %a)" v1 v2 v3 v4 pp_lam t
  | Abs (v1, Abs (v2, Abs (v3, t))) ->
      Format.fprintf fmt "(λ %s %s %s -> %a)" v1 v2 v3 pp_lam t
  | Abs (v1, Abs (v2, t)) ->
      Format.fprintf fmt "(λ %s %s -> %a)" v1 v2 pp_lam t
  | Abs (x, t) -> Format.fprintf fmt "(λ %s -> %a)" x pp_lam t

let list_remove x = List.filter (fun a -> a <> x)

let free_vars =
  let rec helper acc = function
    | Var s -> s :: acc
    | Abs (s, l) -> acc @ list_remove s (helper [] l)
    | App (l, r) -> helper (helper acc r) l
  in
  helper []

let is_free_in x term = List.mem x (free_vars term)

let replace_name x a t =
  let rec helper = function
    | Var y when x = y -> Var a
    | Var t -> Var t
    | App (l, r) -> App (helper l, helper r)
    | Abs (y, t) when x = y -> Abs (a, helper t)
    | Abs (z, t) -> Abs (z, helper t)
  in
  helper t

let rec next_name s old = if List.mem s old then next_name ("_" ^ s) old else s

(*  subst (x,v) e means `[x/v]e` or `e[v -> x]` *)
let subst (x, v) =
  let rec helper e =
    match e with
    | Var y when y = x -> v
    | Var y -> Var y
    | App (l, r) -> App (helper l, helper r)
    | Abs (y, b) when y == x -> Abs (y, b)
    | Abs (y, t) when is_free_in y v ->
        let frees = free_vars v @ free_vars t in
        let w = next_name y frees in
        helper (Abs (w, replace_name y w t))
    | Abs (y, b) -> abs y (helper b)
  in
  helper

type strat = {
  on_var : strat -> name -> t;
  on_abs : strat -> name -> t -> t;
  on_app : strat -> t -> t -> t;
}

let apply_strat st = function
  | Var name -> st.on_var st name
  | Abs (x, b) -> st.on_abs st x b
  | App (l, r) -> st.on_app st l r

let no_strat =
  let on_var _ name = Var name in
  let on_abs _start name body = Abs (name, body) in
  let on_app _start l r = App (l, r) in
  { on_var; on_abs; on_app }

let cbn_strat =
  let on_app st f arg =
    match apply_strat st f with
    | Abs (x, e) -> apply_strat st @@ subst (x, arg) e
    | f2 -> App (f2, arg)
  in
  { no_strat with on_app }

(* Normal Order Reduction to Normal Form
   Application function reduced as CBN first
   + Reduce under on_abstractions *)
let nor_strat =
  let on_abs st x b = Abs (x, apply_strat st b) in
  let on_app st f arg =
    match apply_strat cbn_strat f with
    | Abs (x, e) -> apply_strat st @@ subst (x, arg) e
    | f1 ->
        let f2 = apply_strat st f1 in
        let arg2 = apply_strat st arg in
        App (f2, arg2)
  in
  { no_strat with on_app; on_abs }

(* Call-by-Value Reduction to Weak Normal Form *)
let cbv_strat =
  let on_app st f arg =
    match apply_strat st f with
    | Abs (x, e) ->
        let arg2 = apply_strat st arg in
        apply_strat st @@ subst (x, arg2) e
    | f2 -> App (f2, apply_strat st arg)
  in
  { no_strat with on_app }

(* Applicative Order Reduction to Normal Form
   As CBV but reduce under on_abstractions *)

let ao_strat =
  let on_abs st x b = Abs (x, apply_strat st b) in
  { cbv_strat with on_abs }

let cbn_big_step_strat =
  let open Result in
  let rec helper = function
    | Var _ as l -> Result.Ok l
    | Abs (_, _) as l -> Result.Ok l
    | App (f, arg) -> (
        match helper f with
        | Error f2 -> Ok (App (f2, arg))
        | Ok (Abs (x, e)) -> Error (subst (x, arg) e)
        | Ok f2 -> Ok (App (f2, arg)) )
  in
  let rec main t =
    match helper t with
    | Ok x -> x
    | Error x ->
        Format.printf " -- %a\n%!" pp_lam x;
        main x
  in
  let on_app _st f arg = main (App (f, arg)) in
  let on_abs _st f x = main (Abs (f, x)) in
  let on_var _st x = main (Var x) in
  { on_var; on_abs; on_app }

let a = Var "a"

let x = Var "x"

let y = Var "y"

let z = Var "z"

let f = Var "f"

let g = Var "g"

let h = Var "h"

let m = Var "m"

let n = Var "n"

let p = Var "p"

let zero = abs "f" @@ abs "x" x

let one = abs "f" @@ abs "x" @@ App (f, x)

let two = abs "f" @@ abs "x" @@ App (f, App (f, x))

let three = abs "f" @@ abs "x" @@ App (f, App (f, App (f, x)))

let test strat term =
  Format.printf "Evaluating: %a\n%!" pp_lam term;
  let rez = apply_strat strat term in
  Format.printf "Result:     %a\n%!" pp_lam rez;
  rez
