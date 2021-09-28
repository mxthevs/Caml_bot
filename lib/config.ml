type t = { nick : string; pass : string; chan : string }

let read_file file_path =
  let ch = open_in file_path in
  let n = in_channel_length ch in
  let s = really_input_string ch n in
  close_in ch;
  s

let empty = { nick = ""; pass = ""; chan = "" }

let to_string config =
  Printf.sprintf "{ twitch.nick = %s; twitch.pass = [REDACTED]; twitch.chan = %s }" config.nick
    config.chan

let from_file file_path =
  let update_config config pair =
    match pair with
    | [ "twitch.nick"; nick ] -> { config with nick }
    | [ "twitch.pass"; pass ] -> { config with pass }
    | [ "twitch.chan"; chan ] -> { config with chan }
    | [ unknown; _ ] -> unknown |> Printf.sprintf "`%s` config key is unknown." |> failwith
    | _ -> failwith "Your config file probably have some sort of syntax errors."
  in

  file_path
  |> read_file
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)
  |> List.map (fun s -> s |> String.split_on_char '=' |> List.map String.trim)
  |> List.fold_left update_config empty
