project = caml_bot

deps: $(opam_file)

build:
	dune build

dev:
	dune build -w

run:
	dune exec $(project) ./secrets.conf

debug:
	dune exec $(project) ./secrets.conf -- --debug
