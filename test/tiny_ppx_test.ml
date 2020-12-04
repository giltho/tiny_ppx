open Tiny_lang.Tiny_ast

let add_3 a b c =
  let a = Number a in
  let b = Number b in
  let c = Number c in
  [%tiny_expr "++{{a}}{{b}}{{c}}"]
  
let add_5_plus_x a b c d e =
  let a = Number a in
  let b = Number b in
  let c = Number c in
  let d = Number d in
  let e = Number e in 
  let x = Var "x" in
  [%tiny_expr "+++++{{a}}{{b}}{{c}}{{d}}{{e}}{{x}}"]

let rec eval = function
  | Number i -> i
  | Plus (e1, e2) -> eval e1 + eval e2
  | _ -> failwith "Not concrete expression"

let () = Printf.printf "EVAL: %d\n" (eval (add_3 7 8 9))

let () = Format.printf "%a" Tiny_lang.Tiny_ast.pp_expr (add_5_plus_x 1 2 3 4 5)
