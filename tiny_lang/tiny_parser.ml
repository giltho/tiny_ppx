open Angstrom
open Tiny_ast

let parens p = char '(' *> p <* char ')'

let number =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| fun x ->
  Number (int_of_string x)

let var =
  take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
  >>| fun x -> Var x

let ext =
  let+ content =
    string "{{" *> take_till (function '}' -> true | _ -> false)
    <* string "}}"
  in
  Extension content

let plus e =
  let* _ = char '+' in
  let* a1 = e in
  let+ a2 = e in
  Plus (a1, a2)

let expr =
  fix (fun expr -> parens expr <|> number <|> var <|> ext <|> plus expr)

let parse_expr = parse_string ~consume:All expr
