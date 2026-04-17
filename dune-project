(lang dune 3.16)

(name hqbricks)

(generate_opam_files true)

(version 1.0.0)

(source
 (github Qbricks/hqbricks))

(authors "Christophe Chareton" "Jad Issa" "Mathieu Nguyen")

(maintainers "Mathieu Nguyen")

(license LGPL-3.0-or-later)

(homepage "https://github.com/Qbricks/hqbricks")

(bug_reports "https://github.com/Qbricks/hqbricks/issues")

(package
 (name hqbricks)
 (synopsis "Analysis and verification of quantum programs")
 (description
  "The HQbricks library is a symbolic execution and specification verification tool for hybrid circuits (classical/quantum).")
 (depends
  (ocaml
   (= 5.3.0))
  dune
  (ocamlformat
   (= 0.27.0))
  odoc
  ppx_deriving
  ppx_deriving_yojson
  zarith
  mlmpfr
  (cmdliner
   (= 1.3.0))
  (alcotest :with-test)
  (bechamel :with-test)
  (bechamel-notty :with-test))
 (tags
  ("quantum computing" "static analysis" "verification (automated)")))
