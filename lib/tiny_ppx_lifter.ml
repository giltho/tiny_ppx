open Ppxlib
open Ast_helper
open Tiny_lang

let rec expand_expr ~loc (e : Tiny_ast.expr) =
  match e with
  | Var s ->
      (* Here, I'm explicitely lifting my string from the
         Tiny_lang AST to the OCaml AST *)
      (* If I decided to just do [%expr Var p] instead, that's what
         my OCaml Ast would contain, a `Var` contructor with one parameter
         that is the expression of one variable `p` *)
      let str = Exp.constant (Const.string s) in
      (* The [%e is interpreted litteraly as the previously build
         OCaml AST fragment str] *)
      [%expr Var [%e str]]
  | Number i ->
      let int = Exp.constant (Const.int i) in
      [%expr Number [%e int]]
  | Plus (e1, e2) ->
      let lifted_e1 = expand_expr ~loc e1 in
      let lifted_e2 = expand_expr ~loc e2 in
      [%expr Plus ([%e lifted_e1], [%e lifted_e2])]
  | Extension v_name ->
      let ident = { txt = Lident v_name; loc } in
      Exp.ident ident

let expand ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let expr = Tiny_parser.parse_expr payload in
  match expr with
  | Ok expr -> expand_expr ~loc expr
  | Error e -> Location.raise_errorf ~loc "ERROR: %s" e

let my_extension =
  Extension.V3.declare "tiny_expr" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
