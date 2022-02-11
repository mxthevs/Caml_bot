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
	sleep 10

deps:
	esy

build:
	esy build

dev:
	esy watch

clean:
	rm -rf _esy esy.lock

install: clean deps

run: export DATABASE=$(PG_DATABASE)
run: export USERNAME=$(PG_USERNAME)
run: export PASSWORD=$(PG_PASSWORD)
run: export PORT=$(PG_PORT)
run: up wait
	esy x ${PROJECT} ${SECRETS_PATH}

