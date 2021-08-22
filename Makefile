project = caml_bot

deps: $(opam_file)

build:
	dune build

dev:
	dune build -w

run:
	dune exec $(project)

debug:
	dune exec $(project) -- --debug
