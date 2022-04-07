ENV_FILE=.env

include $(ENV_FILE)

PROJECT = caml_bot
DB_CONTAINER_NAME = caml_bot_db
SECRETS_PATH = ./secrets.conf

up:
	docker-compose up -d

down:
	docker-compose down

down-v:
	docker-compose down -v

psql: ID = $(shell docker ps -aqf "name=$(DB_CONTAINER_NAME)")
psql:
	docker exec -it $(ID) psql -U $(PG_USERNAME) -W $(PG_DATABASE)

wait:
	@echo "Go grab a cup of tea. Maybe look outside."
	sleep 5

deps:
	esy

build:
	esy build

dev:
	esy watch

clean:
	rm -rf _esy esy.lock

dce:
	esy dce

install: clean deps

run debug: export DATABASE=$(PG_DATABASE)
run debug: export USERNAME=$(PG_USERNAME)
run debug: export PASSWORD=$(PG_PASSWORD)
run debug: export PORT=$(PG_PORT)

run: export LOG_LEVEL=info
run: up wait
	esy x ${PROJECT} ${SECRETS_PATH}

debug: export LOG_LEVEL=debug
debug: up wait
	esy x ${PROJECT} ${SECRETS_PATH}
