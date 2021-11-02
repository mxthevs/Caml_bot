ENV_FILE=.env

include $(ENV_FILE)

PROJECT = caml_bot
SECRETS_PATH = ./secrets.conf

deps: $(opam_file)

up:
	docker-compose up -d

down:
	docker-compose down

down-v:
	docker-compose down -v

wait:
	@echo "Go grab a cup of tea. Maybe look outside."
	sleep 10

build:
	dune build

dev:
	dune build -w

run: export DATABASE=$(PG_DATABASE)
run: export USERNAME=$(PG_USERNAME)
run: export PASSWORD=$(PG_PASSWORD)
run: up wait
	dune exec $(PROJECT) ${SECRETS_PATH}

debug:
	dune exec $(PROJECT) ${SECRETS_PATH} -- --debug
