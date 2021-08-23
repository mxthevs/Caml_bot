type prefix_t = string

type command_t = string

type param_t = string

type message_t = { prefix : prefix_t option; command : command_t; params : param_t list }

let twitch_host = "irc.chat.twitch.tv"

let twitch_port = 6667

let is_digit (x : char) = '0' <= x && x <= '9'

let is_letter (x : char) = ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')

let is_special (x : char) =
  [ '-'; '['; ']'; '\\'; '`'; '^'; '{'; '}' ] |> List.find_opt (fun y -> x == y) |> Option.is_some

let is_nospcrlfcl (x : char) =
  let open Char in
  [ 0; code '\r'; code '\n'; code ' '; code ':' ]
  |> List.find_opt (fun y -> code x == y)
  |> Option.is_none

let middle : string Parser.parser =
  let open Parser in
  predicate_char is_nospcrlfcl
  <*> parse_while (fun x -> x == ':' || is_nospcrlfcl x)
  |> map (fun (a, b) -> String.make 1 a ^ b)

let trailing : string Parser.parser =
  let open Parser in
  parse_while (fun x -> x == ':' || is_nospcrlfcl x || x == ' ')

let params : param_t list Parser.parser =
  let open Parser in
  many (prefix " " *> middle)
  <*> optional (prefix " " *> prefix ":" *> trailing)
  |> map (fun (a, b) -> a @ (b |> Option.to_list))

let command : command_t Parser.parser =
  let open Parser in
  (* parse_while_1 is_letter <|> parse_n 3 is_digit *)
  parse_n 3 is_digit

let nickname : command_t Parser.parser =
  let open Parser in
  predicate_char (fun x -> is_letter x || is_digit x)
  <*> parse_while (fun x -> is_letter x || is_digit x || is_special x || x == '-')
  |> map (fun (a, b) -> String.make 1 a ^ b)

let user : string Parser.parser =
  let open Parser in
  let is_unwanted x =
    let open Char in
    [ 0; code '\r'; code '\n'; code ' '; code '@' ]
    |> List.find_opt (fun y -> code x == y)
    |> Option.is_some
  in
  parse_while (fun x -> not (is_unwanted x))

let shortname : string Parser.parser =
  let open Parser in
  parse_while (fun x -> x == '-' || is_digit x || is_letter x)

let hostname : string Parser.parser =
  let open Parser in
  shortname <*> many (prefix "." *> shortname) |> map (fun (x, xs) -> x :: xs |> String.concat ".")

let ip4addr : command_t Parser.parser =
  let open Parser in
  let octet = parse_n 3 is_digit in

  octet
  <*> prefix "." *> octet
  <*> prefix "." *> octet
  <*> prefix "." *> octet
  |> map (fun (((a, b), c), d) -> [ a; b; c; d ] |> String.concat ".")

let hostaddr : command_t Parser.parser = ip4addr (* <|> ip6addr *)

let host : string Parser.parser =
  let open Parser in
  hostname <|> hostaddr

let servername : string Parser.parser = hostname

let message_prefix : prefix_t Parser.parser = servername

let message_parser : message_t Parser.parser =
  let open Parser in
  optional (prefix ":" *> message_prefix <* prefix " ")
  <*> command
  <*> params
  |> map (fun ((p, c), ps) -> { prefix = p; command = c; params = ps })

let show_message (msg : message_t) : string =
  Printf.sprintf "{ prefix = %s; command = %s; params = [%s] }"
    (Option.value msg.prefix ~default:"<none>")
    msg.command
    (msg.params |> String.concat ",")

let start (config : Config.t) =
  Printf.printf "[Twitch_irc] Trying to connect to %s:%d\n" twitch_host twitch_port;
  flush stdout;

  let client_socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr =
    match (Unix.gethostbyname twitch_host).h_addr_list |> Array.to_list |> Util.list_to_option with
    | Some addr -> addr
    | None -> twitch_host |> Printf.sprintf "[Twitch_irc] Could not resolve %s\n" |> failwith
  in
  Unix.connect client_socket (ADDR_INET (addr, twitch_port));

  Printf.printf "[Twitch_irc] Connected!\n";
  flush stdout;

  let input_channel = client_socket |> Unix.in_channel_of_descr in
  let output_channel = client_socket |> Unix.out_channel_of_descr in

  let rec loop () =
    let open Parser in
    let input = input_channel |> input_line in
    let result = input |> make_input |> message_parser.run in

    (match result with
    | Ok (_, msg) -> msg |> show_message |> print_endline
    | Error e -> e.desc |> Printf.printf "[Twitch_irc] Error parsing `%s`");

    flush stdout;
    loop ()
  in

  config.twitch_password |> Printf.sprintf "PASS %s\r\n" |> output_string output_channel;
  config.twitch_username |> Printf.sprintf "NICK %s\r\n" |> output_string output_channel;

  flush_all ();

  loop ()
