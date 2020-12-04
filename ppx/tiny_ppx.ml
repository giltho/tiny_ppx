open Ppxlib

let () =
  Driver.register_transformation ~rules:[ Tiny_ppx_lifter.rule ] "tiny_expr"
