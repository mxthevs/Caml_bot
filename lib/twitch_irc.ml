type connection = { host : string; port : int }

let conn = { host = "irc.chat.twitch.tv"; port = 6667 }

module Irc_protocol = struct
  module Message_type = struct
    type t = JOIN | NICK | PASS | PRIVMSG | PONG

    let to_string = function
      | JOIN -> "JOIN #"
      | NICK -> "NICK "
      | PASS -> "PASS "
      | PRIVMSG -> "PRIVMSG "
      | PONG -> "PONG :"
  end

  let create ~command content = Message_type.to_string command ^ content ^ "\r\n"

  let fmt_incoming s = Printf.sprintf "<<< %s" s

  let fmt_outgoing fn =
    Printf.sprintf ">>> %s" fn |> print_string;
    fn

  let join channel = fmt_outgoing (create ~command:JOIN channel)

  let nick username = fmt_outgoing (create ~command:NICK username)

  let pass password = create ~command:PASS password

  let privmsg content ~target =
    fmt_outgoing (content |> Printf.sprintf "%s :%s" target |> create ~command:PRIVMSG)

  let pong target = fmt_outgoing (create ~command:PONG target)
end

type out_string = out_channel -> string -> unit

let join_and_greet (config : Config.t) (out_string : out_string) (out_descr : out_channel) =
  let pass, nick, chan = (config.pass, config.nick, config.chan) in
  pass |> Irc_protocol.pass |> out_string out_descr;
  nick |> Irc_protocol.nick |> out_string out_descr;
  chan |> Irc_protocol.join |> out_string out_descr;
  Irc_protocol.privmsg ~target:("#" ^ chan) "Initializing BOT MrDestructoid" |> out_string out_descr

let start (config : Config.t) =
  Printf.printf "[Twitch_irc] Trying to connect to %s:%d\n" conn.host conn.port;
  flush stdout;

  let client_socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr =
    match (Unix.gethostbyname conn.host).h_addr_list |> Array.to_list |> Helpers.list_to_option with
    | Some addr -> addr
    | None -> conn.host |> Printf.sprintf "[Twitch_irc] Could not resolve %s\n" |> failwith
  in
  Unix.connect client_socket (ADDR_INET (addr, conn.port));

  Printf.printf "[Twitch_irc] Connected!\n";
  flush stdout;

  let input_channel = client_socket |> Unix.in_channel_of_descr in
  let output_channel = client_socket |> Unix.out_channel_of_descr in

  let rec wait_for_messages_and_reply () =
    let input = input_channel |> input_line in

    (match Message.parse input with
    | Ok message -> (
        match message.command with
        | PRIVMSG (target, msg, user) -> (
            match Command.parse msg user with
            | Some reply -> Irc_protocol.privmsg ~target reply |> output_string output_channel
            | None -> ())
        | PING (target, _) -> target |> Irc_protocol.pong |> output_string output_channel
        | _ -> ())
    | Error error -> error |> Printf.sprintf "[Twitch_irc] %s\n" |> failwith);

    flush_all ();
    wait_for_messages_and_reply ()
  in

  join_and_greet config output_string output_channel;

  flush_all ();

  wait_for_messages_and_reply ()
