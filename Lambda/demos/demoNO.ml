open Lambda_lib.Lambda

(*
let nor_small_step_strat =
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
  { on_var; on_abs; on_app } *)

let test_fac =
  let zero = abs "g" @@ abs "y" @@ Var "y" in
  let one = abs "f" @@ abs "x" @@ app f (Var "x") in
  let two = abs "f" @@ abs "x" @@ app f (app f x) in
  let three = abs "f" @@ abs "x" @@ app f (app f (app f x)) in
  let plus =
    abs "m" @@ abs "n" @@ abs "f" @@ abs "x" @@ app m @@ app f @@ app n
    @@ app f x
  in
  let mul = abs "x" @@ abs "y" @@ abs "z" @@ app x (app y z) in
  let true_ = abs "x" @@ abs "y" @@ Var "x" in
  let false_ = abs "x" @@ abs "y" @@ Var "y" in
  let isZero = abs "n" @@ app (app n (abs "x" false_)) true_ in
  let ite cond th el = app (app (app isZero cond) th) el in
  let pred =
    let xxx = abs "g" @@ abs "h" @@ app h (app g f) in
    abs "n" @@ abs "f" @@ abs "x"
    @@ app (app (app n xxx) (abs "u" x)) (abs "u" (Var "u"))
  in
  let fact =
    abs "self" @@ abs "N"
    @@ ite (Var "N") one
         (app (app mul (app (var "self") (app pred (var "N")))) (var "N"))
  in
  let ygrek =
    let hack = abs "x" (app f (app x x)) in
    abs "f" (app hack hack)
  in
  (* let () =
       test nor_strat @@ zero |> fun lam -> Format.printf "%a\n%!" pp_lam lam
     in
     let () =
       test nor_strat @@ one |> fun lam -> Format.printf "%a\n%!" pp_lam lam
     in
     let _ =
       test nor_strat @@ app plus @@ app one one |> fun lam ->
       Format.printf "%a\n%!" pp_lam lam
     in *)
  let () =
    test cbn_strat @@ app (app ygrek fact) three |> fun lam ->
    Format.printf "%a\n%!" pp_lam lam
  in
  ()
