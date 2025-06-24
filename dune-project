(lang dune 3.8)
(name z3i)
(implicit_transitive_deps true)

(generate_opam_files true)

(source
 (github smuenzel/ocaml_z3i))

(authors "Stefan Muenzel <source@s.muenzel.net>")

(maintainers "Stefan Muenzel <source@s.muenzel.net>")

(license MPL-2.0)

(package
  (name     z3i)
  (synopsis "A z3 interface")
  (description "A z3 interface with types")
  (depends
    dune
    core
    ppx_expect
    ppx_jane
    higher_kinded
    z3
    )
  (tags (z3i)))
