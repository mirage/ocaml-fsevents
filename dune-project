(lang dune 2.8)
(name fsevents)

(generate_opam_files true)

(source (github mirage/ocaml-fsevents))
(license ISC)
(authors "David Sheets" "Thomas Gazagnaire")
(maintainers "Thomas Gazagnaire")

(package
 (name fsevents)
 (synopsis "OCaml bindings to macOS FSEvents")
 (description "These bindings use [ctypes](https://github.com/ocamllabs/ocaml-ctypes)
for type-safe stub generation.")
 (depends
  (cf (>= "0.4"))
  (ctypes (>= "0.4.0"))))

(package
 (name fsevents-lwt)
 (synopsis "Lwt interface to macOS FSEvents")
 (description "An Lwt interface to the macOS FSEvents framework, using the
`cf-lwt` library for the low-level bindings.")
 (depends
  (fsevents (= :version))
  cf-lwt
  (cmdliner (>= "1.1.0"))
  (alcotest :with-test)
  (lwt (>= "5.0.0"))))
