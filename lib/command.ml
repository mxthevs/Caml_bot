type t = {
  name : string;
  max_words : int;
  required_words : string list list;
  unwanted_words : string list list;
  response : string;
}
[@@deriving yojson]

let replace_username str user = Str.global_replace (Str.regexp "{{USERNAME}}") user str

let read_file file_path =
  let ch = open_in file_path in
  let n = in_channel_length ch in
  let s = really_input_string ch n in
  close_in ch;
  s

let parse message username =
  let response_field = "response" in

  if String.get message 0 = '!' && Util.count_char (Util.explode message) ' ' < 2 then
    let cmd_meta = read_file "./cmd/exclamation.json" in
    let cmd = Yojson.Safe.from_string cmd_meta in

    let reply =
      Yojson.Safe.to_string (Yojson.Safe.Util.member response_field cmd)
      |> Util.explode
      |> List.filter (fun c -> c != '"')
      |> List.to_seq
      |> String.of_seq
    in

    Some (replace_username reply username)
  else None
