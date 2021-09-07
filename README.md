# 🐫 Caml_bot

![OCaml](https://img.shields.io/badge/-OCaml-c15540?style=square&logo=ocaml&logoColor=white)

## What this is?

Caml_bot is a Twitch.tv chatbot for my channel [matheusdev1](https://twitch.tv/matheusdev1) there! 🤗

## What does it do?

  1. It have its own very performatic IRC Client, with **zero** third-party dependencies and built completely from scratch.
  2. It parses incoming messages from chat to a nice fully typed format.
  3. It has a full (opinionated) command parser, making it easy to configure, create and deploy new commands, without having to touch source.
  4. It can have public and mod-only commands.

## Running locally

To run this program in your PC, you have to provide the path to a credentials file as an argument. The file must be in the following format:

```conf
twitch.username=bot_name
twitch.password=oauth:your_key
twitch.channel=channel_name
```

If you don't have a `TMI OAuth token`, you can generate one at [https://twitchapps.com/tmi/](https://twitchapps.com/tmi/)

### Dependencies
  * OCaml (>= v4.12.0)
  * OCaml Core library (>= v0.12.2)
  * Dune (>= v2)

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

*Released under MIT License 2021*
