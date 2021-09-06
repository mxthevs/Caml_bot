type config_t = { host : string; port : int }

let cfg = { host = "irc.chat.twitch.tv"; port = 6667 }

let start (config : Config.t) =
  Printf.printf "[Twitch_irc] Trying to connect to %s:%d\n" cfg.host cfg.port;
  flush stdout;

  let client_socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr =
    match (Unix.gethostbyname cfg.host).h_addr_list |> Array.to_list |> Util.list_to_option with
    | Some addr -> addr
    | None -> cfg.host |> Printf.sprintf "[Twitch_irc] Could not resolve %s\n" |> failwith
  in
  Unix.connect client_socket (ADDR_INET (addr, cfg.port));

  Printf.printf "[Twitch_irc] Connected!\n";
  flush stdout;

  let input_channel = client_socket |> Unix.in_channel_of_descr in
  let output_channel = client_socket |> Unix.out_channel_of_descr in

  let rec loop () =
    input_channel |> input_line |> print_endline;

    flush stdout;
    loop ()
  in

  config.twitch_password |> Printf.sprintf "PASS %s\r\n" |> output_string output_channel;
  config.twitch_username |> Printf.sprintf "NICK %s\r\n" |> output_string output_channel;
  config.twitch_channel |> Printf.sprintf "JOIN #%s\r\n" |> output_string output_channel;

  flush_all ();

  loop ()
