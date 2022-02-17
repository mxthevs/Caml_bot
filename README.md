# 🐫 Caml_bot

![OCaml](https://img.shields.io/badge/-OCaml-c15540?style=square&logo=ocaml&logoColor=white)

## What this is?

Caml_bot is a Twitch.tv chatbot for my channel [mxthevsz](https://twitch.tv/mxthevsz) there! 🤗

## What does it do?

1. It has its own IRC Client built completely from scratch.
2. It parses incoming messages and handle them as commands.
3. It can have public and mod-only commands.

## What features it is planned to have in the future?

1. Parse commands from files: The things that game devs do, create a DSL and make it very easy to integrate new functionalities without having to touch source-code.

2. Parse natural language: Just an idea, but it would be very cool if the bot can respond to "what is X?" and other simple questions instead of replying to `!` started commands.

### Dependencies

This project uses `esy` for package management. It can install packages from `opam` and from `npm`. Checkout the [esy getting started section](https://esy.sh/docs/en/getting-started.html) for instructions on how to install it.

It also depends on `PostgreSQL` as the DBMS. A `PostgreSQL` container can be started by [docker-compose](./docker-compose.yaml).

For project related dependencies, see [esy.json](https://github.com/mxthevs/Caml_bot/blob/master/esy.json#L29).

## Running locally

1. Clone this repository
2. Create the configuration files `.env` and `secrets.conf`. You can checkout the example files for each of them in the root of the project. Two things to note here: for now only localhost is supported as the database host, and you will a need special token to connect to Twitch IRC. If you don't have a `TMI OAuth token`, you can generate one at [https://twitchapps.com/tmi/](https://twitchapps.com/tmi/).
3. Run `make install` to install the depencies.
4. Run `make build`
5. If you are going to develop some features, you can start the compiler by running `make dev`.
6. Now you can run `make run` and it will use the configurations files to run the project. Make sure you have Docker up and running before running.

See the [Makefile](./Makefile) for more details. It's very simple.

#### ⚠️ Disclaimer

This piece of software was built in a UNIX machine, and it depends on linkage of the Unix library in compile-time, therefore it was untested in a Windows environment, and you may need WSL2 to run Caml_bot in your Windows machine, but I don't know for sure.

If you have any problem, feel free to file an issue or if you are willing to contribute to the project, open a pull request as well!

_Released under MIT License 2021_
