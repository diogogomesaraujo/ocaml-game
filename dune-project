(lang dune 3.7)

(name game)

(generate_opam_files true)

(source
 (github diogogomesaraujo/ocaml-game))

(authors "Diogo Araújo")

(maintainers "Diogo Araújo")

(license LICENSE)

(package
 (name game)
 (synopsis "A Game in OCaml using Raylib")
 (description "A 2D shooter game inspired by a project by TJ Devries")
 (depends ocaml dune raylib)
 (tags
  (topics "game" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
