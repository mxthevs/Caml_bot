type connection = {
  host : string;
  port : int;
}

let conn = { host = "irc.chat.twitch.tv"; port = 6667 }

module Irc_protocol = struct
  module Message_type = struct
    type t =
      | JOIN
      | NICK
      | PASS
      | PRIVMSG
      | PONG

    let to_string = function
      | JOIN -> "JOIN #"
      | NICK -> "NICK "
      | PASS -> "PASS "
      | PRIVMSG -> "PRIVMSG "
      | PONG -> "PONG :"
  end

  let create ~command content = Message_type.to_string command ^ content ^ "\r\n"

  let fmt_outgoing fn ~debug =
    let () =
      match debug with
      | true -> Printf.sprintf ">>> %s" fn |> print_string
      | false -> ()
    in
    fn

  let join channel = create ~command:JOIN channel
  let nick username = create ~command:NICK username
  let pass password = create ~command:PASS password
  let privmsg content ~target = content |> Printf.sprintf "%s :%s" target |> create ~command:PRIVMSG
  let pong target = create ~command:PONG target
end

type out_string = out_channel -> string -> unit

let join_and_greet (config : Config.t) (out_string : out_string) (out_descr : out_channel) =
  let pass, nick, chan, debug = (config.pass, config.nick, config.chan, config.debug) in

  let fmt = Irc_protocol.fmt_outgoing ~debug in

  pass |> Irc_protocol.pass |> out_string out_descr;
  nick |> Irc_protocol.nick |> fmt |> out_string out_descr;
  chan |> Irc_protocol.join |> fmt |> out_string out_descr

let start (config : Config.t) =
  if config.debug then Printf.printf "[Twitch_irc] Trying to connect to %s:%d\n" conn.host conn.port;
  flush stdout;

  let client_socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr =
    match (Unix.gethostbyname conn.host).h_addr_list |> Array.to_list |> Helpers.list_to_option with
    | Some addr -> addr
    | None -> (
      match config.debug with
      | true -> conn.host |> Printf.sprintf "[Twitch_irc] Could not resolve %s\n" |> failwith
      | false -> raise (Failure "oops"))
  in
  Unix.connect client_socket (ADDR_INET (addr, conn.port));

  if config.debug then Printf.printf "[Twitch_irc] Connected!\n";
  flush stdout;

  let input_channel = client_socket |> Unix.in_channel_of_descr in
  let output_channel = client_socket |> Unix.out_channel_of_descr in

  let rec wait_for_messages_and_reply () =
    let input = input_channel |> input_line in

    let handle_privsmg ~target ~message ~sender =
      if message.[0] = '!' then
        match Command.handle ~message ~user:sender with
        | Ok reply ->
          Irc_protocol.privmsg ~target reply
          |> Irc_protocol.fmt_outgoing ~debug:config.debug
          |> output_string output_channel
        | Error () -> ()
    in

    (match Message.parse input with
    | Ok message -> (
      match message.command with
      | PRIVMSG (target, message, sender) -> handle_privsmg ~target ~message ~sender
      | PING (target, _) ->
        target
        |> Irc_protocol.pong
        |> Irc_protocol.fmt_outgoing ~debug:config.debug
        |> output_string output_channel
      | _ -> ())
    | Error error -> (
      match config.debug with
      | true -> error |> Printf.sprintf "[Twitch_irc] %s\n" |> failwith
      | false -> ()));

    flush_all ();
    wait_for_messages_and_reply ()
  in

  join_and_greet config output_string output_channel;

  flush_all ();

  wait_for_messages_and_reply ()
