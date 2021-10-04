type t = {
  name : string;
  max_words : int;
  required_words : string list list;
  unwanted_words : string list list;
  response : string;
}
[@@deriving yojson]

let replace_in_str expr str original = Str.global_replace expr str original

let count_spaces s = Helpers.count_char (Helpers.explode s) ' '

let parse message username =
  let response_field = "response" in

  if String.get message 0 = '!' && count_spaces message < 2 then
    let open Yojson.Safe in
    let cmd_meta = Helpers.read_file "./cmd/exclamation.json" in
    let cmd = from_string cmd_meta in

    let reply =
      Util.member response_field cmd
      |> to_string
      |> Helpers.explode
      |> List.filter (fun c -> c != '"')
      |> List.to_seq
      |> String.of_seq
      |> replace_in_str (Str.regexp "%1") username
    in

    Some reply
  else None
