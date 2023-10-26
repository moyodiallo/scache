(lang dune 3.10)

(name scache)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "moyodiallo@gmail.com")

(maintainers "Alpha-Issiaga DIALLO <moyodiallo@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name scache)
 (synopsis "Simple cache system base on Sqlite3")
 (depends
  (ocaml (>= 4.12.0))
  (alcotest :with-test)
  sqlite3
  fpath
  (fmt (>= 0.8.7))
  bos
  ocaml))
