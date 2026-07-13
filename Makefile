# -----------------------------------------------------------------------------
# Default target
# -----------------------------------------------------------------------------

.DEFAULT_GOAL := run

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

ENV_FILE     := .env
SECRETS_PATH := ./secrets.conf

PROJECT      := caml_bot
DB_CONTAINER := caml_bot_db

# -----------------------------------------------------------------------------
# Tools
# -----------------------------------------------------------------------------

COMPOSE := docker compose
DOCKER  := docker
ESY     := esy

# -----------------------------------------------------------------------------
# Environment
# -----------------------------------------------------------------------------

include $(ENV_FILE)

export GITHUB_TOKEN := $(GH_TOKEN)
export DATABASE     := $(PG_DATABASE)
export USERNAME     := $(PG_USERNAME)
export PASSWORD     := $(PG_PASSWORD)
export PORT         := $(PG_PORT)

# -----------------------------------------------------------------------------
# Internal variables
# -----------------------------------------------------------------------------

DB_CONTAINER_ID := $(shell $(DOCKER) ps -qf "name=$(DB_CONTAINER)")

# -----------------------------------------------------------------------------
# Phony targets
# -----------------------------------------------------------------------------

.PHONY: \
	up \
	down \
	down-v \
	restart \
	status \
	logs \
	psql \
	wait \
	deps \
	build \
	dev \
	clean \
	dce \
	install \
	app \
	run \
	debug

# -----------------------------------------------------------------------------
# Docker
# -----------------------------------------------------------------------------

up:
	$(COMPOSE) up -d

down:
	$(COMPOSE) down

down-v:
	$(COMPOSE) down -v

restart: down up

status:
	$(COMPOSE) ps

logs:
	$(COMPOSE) logs -f

psql:
	$(DOCKER) exec -it $(DB_CONTAINER_ID) \
		psql -U $(PG_USERNAME) -W $(PG_DATABASE)

wait:
	@echo "Waiting for PostgreSQL..."
	@until $(DOCKER) exec $(DB_CONTAINER) \
		pg_isready \
		-U $(PG_USERNAME) \
		-d $(PG_DATABASE) >/dev/null 2>&1; do \
		sleep 1; \
	done
	@echo "PostgreSQL is ready."

# -----------------------------------------------------------------------------
# Esy
# -----------------------------------------------------------------------------

deps:
	$(ESY)

build:
	$(ESY) build

dev:
	$(ESY) watch

clean:
	rm -rf _esy esy.lock

dce:
	$(ESY) dce

install: clean deps

# -----------------------------------------------------------------------------
# Application
# -----------------------------------------------------------------------------

app: build up wait
	$(ESY) x $(PROJECT) $(SECRETS_PATH)

run: LOG_LEVEL := info
run: export LOG_LEVEL
run: app

debug: LOG_LEVEL := debug
debug: export LOG_LEVEL
debug: app