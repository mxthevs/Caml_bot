PROJECT = caml_bot
SECRETS_PATH = ./secrets.conf

deps: $(opam_file)

build:
	dune build

dev:
	dune build -w

run:
	dune exec $(PROJECT) ${SECRETS_PATH}

debug:
	dune exec $(PROJECT) ${SECRETS_PATH} -- --debug
