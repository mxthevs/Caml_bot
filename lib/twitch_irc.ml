type connection = { host : string; port : int }

let conn = { host = "irc.chat.twitch.tv"; port = 6667 }

module Irc_protocol = struct
  module Message_type = struct
    type t = JOIN | NICK | PASS | PRIVMSG | PONG

    let to_string = function
      | JOIN -> "JOIN #"
      | NICK -> "NICK "
      | PASS -> "PASS "
      | PRIVMSG -> "PRIVMSG #"
      | PONG -> "PONG :"
  end

  let create ~t content = Message_type.to_string t ^ content ^ "\r\n"

  let fmt_incoming s = Printf.sprintf "<<< %s" s

  let fmt_outgoing fn =
    Printf.sprintf ">>> %s" fn |> print_string;
    fn

  let join channel = fmt_outgoing (create ~t:JOIN channel)

  let nick username = fmt_outgoing (create ~t:NICK username)

  let pass password = create ~t:PASS password

  let privmsg channel content =
    fmt_outgoing (channel |> Printf.sprintf "%s :%s" content |> create ~t:PRIVMSG)

  let pong target = fmt_outgoing (create ~t:PONG target)
end

type out_string = out_channel -> string -> unit

let join_and_greet (config : Config.t) (out_string : out_string) (out_descr : out_channel) =
  config.pass |> Irc_protocol.pass |> out_string out_descr;
  config.nick |> Irc_protocol.nick |> out_string out_descr;
  config.chan |> Irc_protocol.join |> out_string out_descr;
  config.chan |> Irc_protocol.privmsg "Initializing BOT MrDestructoid" |> out_string out_descr

let start (config : Config.t) =
  Printf.printf "[Twitch_irc] Trying to connect to %s:%d\n" conn.host conn.port;
  flush stdout;

  let client_socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr =
    match (Unix.gethostbyname conn.host).h_addr_list |> Array.to_list |> Util.list_to_option with
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
        | PRIVMSG (target, message, user) ->
            Printf.sprintf "t: %s; m: %s; u: %s" target message user
            |> Irc_protocol.fmt_incoming
            |> print_endline
        | PING (target, _) -> target |> Irc_protocol.pong |> output_string output_channel
        | _ -> ())
    | Error error -> failwith error);

    flush_all ();
    wait_for_messages_and_reply ()
  in

  join_and_greet config output_string output_channel;

  flush_all ();

  wait_for_messages_and_reply ()
