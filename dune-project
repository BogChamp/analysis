(lang dune 2.8)
(name mulua)

(generate_opam_files true)

(package
 (name mulua)
 (synopsis "The muLua programming language interpeter")
 (depends
  (ocaml
   (>= 4.12))
  (core
   (>= 0.14.0))
  (cmdliner
   (>= 1.0.4))
  (sedlex
   (>= 2.2))
  (ocamlgraph
   (>= 2.0))
  (menhir
   (>= 20210419))
  (ppx_jane
   (>= 0.14.0))
  (ppx_compose
   (>= 0.1.0))
  (ppx_gen_rec
   (>= 1.0.0))))

(using menhir 2.0)
