open Opal
open AST

let digit_c =
  let is_digit ch =
    let c = Char.code ch in
    Char.code '0' <= c && c <= Char.code '9'
  in
  satisfy is_digit

let digit = digit_c >>= fun c -> return (Char.code c - Char.code '0')
