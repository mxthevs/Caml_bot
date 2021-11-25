# 🐫 Caml_bot

![OCaml](https://img.shields.io/badge/-OCaml-c15540?style=square&logo=ocaml&logoColor=white)

## What this is?

Caml_bot is a Twitch.tv chatbot for my channel [mxthevsz](https://twitch.tv/mxthevsz) there! 🤗

## What does it do?

1. It have its own very performatic IRC Client, with **zero** third-party dependencies and built completely from scratch.
2. It parses incoming messages from chat to a nice fully typed format.
3. It has a full (opinionated) command parser, making it easy to configure, create and deploy new commands, without having to touch source.
4. It can have public and mod-only commands.

## Running locally

To run this program in your PC, you have to provide the path to a credentials file as an argument. If you are going to the bot this via the `make run` or the `make debug` command, you should specify the path to this file in the Makefile too. The file must be in the following format:

```conf
twitch.nick=bot_name
twitch.pass=oauth:your_key
twitch.chan=channel_name
```

If you don't have a `TMI OAuth token`, you can generate one at [https://twitchapps.com/tmi/](https://twitchapps.com/tmi/)

### Dependencies

- OCaml (>= v4.12.0)
- OCaml Core library (>= v0.12.2)
- Dune (>= v2)

#### ⚠️ Disclaimer

This piece of software was built in a UNIX machine, and it depends on linkage of the Unix library in compile-time, therefore it was untested in a Windows environment, and you may need WSL2 to run Caml_bot in your Windows machine.

### Running

```sh
  make build && make run
```

### Developing

```sh
  make dev
```

### Debug

```sh
  make debug
```

See the [Makefile](./Makefile) for more details. It's very simple.

If you have any problem, you can hit me up, for sure!

_Released under MIT License 2021_
