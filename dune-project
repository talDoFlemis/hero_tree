(lang dune 3.10)

(name hero_tree)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name hero_tree)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml fmt dune ppx_deriving ppx_expect 
          ppx_sexp_conv sexplib)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
