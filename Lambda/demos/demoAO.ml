open Lambda_lib.Lambda

let testArithm =
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
  let () =
    test ao_strat @@ zero |> fun lam -> Format.printf "%a\n%!" pp_lam lam
  in
  let () =
    test ao_strat @@ one |> fun lam -> Format.printf "%a\n%!" pp_lam lam
  in
  let _ =
    test ao_strat @@ app plus @@ app one one |> fun lam ->
    Format.printf "%a\n%!" pp_lam lam
  in
  let () =
    test ao_strat @@ app isZero @@ app one @@ app two three |> fun lam ->
    Format.printf "%a\n%!" pp_lam lam
  in
  ()
