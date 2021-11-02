(lang dune 2.0)

(generate_opam_files true)

(name caml_bot)

(version 0.1.0)

(authors "Matheus Henrique")

(license MIT)

(source
 (uri "git+https://github.com/mxthevs/caml_bot.git"))

(maintainers "mxthevsh@gmail.com")

(homepage "https://github.com/mxthevs/caml_bot")

(bug_reports "https://github.com/mxthevs/caml_bot/issues")

(documentation "https://github.com/mxthevs/caml_bot#readme")

(package
 (name caml_bot)
 (synopsis "Twitch IRC Bot built with OCaml")
 (description "Twitch IRC Bot built from scratch with OCaml")
 (depends
  ;; core
  (dune
   (>= 2))
  (ocaml
   (>= 4.12.0))
  ;; dependencies
  (lwt
   (>= 5.4.2))
  (uuidm
   (>= 0.9.7))
  (caqti
   (>= 1.6.0))
  (caqti-lwt
   (>= 1.6.0))
  (caqti-driver-postgresql
   (>= 1.6.0))
  (ppx_rapper
   (>= 3.0.0))
  (ppx_rapper_lwt
   (>= 3.0.0))
  ;; dev-dependencies
  (utop :dev)
  (merlin :dev)
  (ocamlformat :dev)
  (ocp-indent :dev)))
