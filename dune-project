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

(documentation "https://mxthevs.github.io/caml_bot/")

(package
 (name caml_bot)
 (synopsis "Twitch IRC Bot built with OCaml")
 (description "Twitch IRC Bot built from scratch with OCaml")
 (depends
  (dune
   (>= 2))
  (ocaml
   (>= 4.12.0))
  (core
   (>= v0.12.2))
  (utop :dev)
  (merlin :dev)
  (ocamlformat :dev)
  (ocp-indent :dev)))
