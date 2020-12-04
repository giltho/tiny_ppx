open Tiny_lang.Tiny_ast

let add_3 a b c =
  let a = Number a in
  let b = Number b in
  let c = Number c in
  [%tiny_expr "++{{a}}{{b}}{{c}}"]

let rec eval = function
  | Number i -> i
  | Plus (e1, e2) -> eval e1 + eval e2
  | _ -> failwith "Not concrete expression"

let () = assert (Int.equal (eval (add_3 7 8 9)) 24)
