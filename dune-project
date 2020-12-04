(lang dune 2.7)
(name tiny_ppx)

(generate_opam_files true)

(maintainers "giltho")

(package
 (name tiny_ppx)
 (synopsis "Ppx lifter example")
 (description "
")
 (depends
  angstrom
  ppx_deriving
  ppxlib
  (ocaml (>= 4.11))))