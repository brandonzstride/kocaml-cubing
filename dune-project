(lang dune 3.16)
(generate_opam_files true)
(authors "Brandon Stride")
(maintainers "bstride1@jhu.edu")
(package
 (name kocaml_cubing)
 (allow_empty)
 (synopsis "An OCaml implementation of Kociemba's Two-phase algorithm")
 (description "No further description")
 (depends
  (landmarks (>= 1.4))
  (ounit2 (>= 2.2.7))
  (core (>= 0.17.1))
  (core_unix (>= 0.17.0))
  (ppx_jane (>= 0.17.0))
  (psq (>= 0.2.1))
))