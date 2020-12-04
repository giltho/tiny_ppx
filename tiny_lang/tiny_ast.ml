type expr =
  | Var of string
  | Number of int
  | Plus of expr * expr
  | Extension of string
[@@deriving show]
