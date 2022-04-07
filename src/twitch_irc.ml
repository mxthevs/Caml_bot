open Logger

type connection = {
  host : string;
  port : int;
}

let conn = { host = "irc.chat.twitch.tv"; port = 6667 }

module Irc_protocol = struct
  type message_typ =
    | JOIN
    | NICK
    | PASS
    | PRIVMSG
    | PONG

  let typ_to_string = function
    | JOIN -> "JOIN #"
    | NICK -> "NICK "
    | PASS -> "PASS "
    | PRIVMSG -> "PRIVMSG "
    | PONG -> "PONG :"

  let create ~command content = typ_to_string command ^ content ^ "\r\n"

  let send_message message =
    [%log debug ">>> %s" (String.trim message)];
    message

  let join channel = create ~command:JOIN channel
  let nick username = create ~command:NICK username
  let pass password = create ~command:PASS password
  let privmsg content ~target = content |> Printf.sprintf "%s :%s" target |> create ~command:PRIVMSG
  let pong target = create ~command:PONG target
end

type out_string = out_channel -> string -> unit

let join_and_greet (config : Config.t) (out_string : out_string) (out_descr : out_channel) =
  let pass, nick, chan = (config.pass, config.nick, config.chan) in

  pass |> Irc_protocol.pass |> out_string out_descr;
  nick |> Irc_protocol.nick |> Irc_protocol.send_message |> out_string out_descr;
  chan |> Irc_protocol.join |> Irc_protocol.send_message |> out_string out_descr

let list_to_option xs =
  match xs with
  | x :: _ -> Some x
  | [] -> None

let start (config : Config.t) =
  [%log info "Trying to connect to %s:%d" conn.host conn.port];
  flush stdout;

  let client_socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr =
    match (Unix.gethostbyname conn.host).h_addr_list |> Array.to_list |> list_to_option with
    | Some addr -> addr
    | None ->
      [%log err "Could not resolve %s" conn.host];
      exit 1
  in
  Unix.connect client_socket (ADDR_INET (addr, conn.port));

  [%log info "Connected!"];
  flush stdout;

  let input_channel = client_socket |> Unix.in_channel_of_descr in
  let output_channel = client_socket |> Unix.out_channel_of_descr in

  let rec wait_for_messages_and_reply () =
    let input = input_channel |> input_line in

    let handle_privsmg ~target ~message ~sender =
      [%log debug "<<< %s" (String.trim message)];

      if message.[0] = '!' then
        match Bot.handle_command ~message ~user:sender with
        | Ok reply ->
          Irc_protocol.privmsg ~target reply
          |> Irc_protocol.send_message
          |> output_string output_channel
        | Error () -> ()
    in

    (match Message.parse input with
    | Ok message -> (
      match message.command with
      | PRIVMSG (target, message, sender) -> handle_privsmg ~target ~message ~sender
      | PING (target, _) ->
        target |> Irc_protocol.pong |> Irc_protocol.send_message |> output_string output_channel
      | _ -> ())
    | Error error -> [%log err "%s" error]);

    flush_all ();
    wait_for_messages_and_reply ()
  in

  join_and_greet config output_string output_channel;

  flush_all ();

  wait_for_messages_and_reply ()
