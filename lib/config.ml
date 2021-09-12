type t = { twitch_username : string; twitch_password : string; twitch_channel : string }

let read_file file_path =
  let ch = open_in file_path in
  let n = in_channel_length ch in
  let s = really_input_string ch n in
  close_in ch;
  s

let default_config = { twitch_username = ""; twitch_password = ""; twitch_channel = "" }

let to_string config =
  Printf.sprintf "{ twitch.username = %s; twitch.password = [REDACTED]; twitch.channel = %s }"
    config.twitch_username config.twitch_channel

let from_file file_path =
  let update_config config pair =
    match pair with
    | [ "twitch.username"; username ] -> { config with twitch_username = username }
    | [ "twitch.password"; password ] -> { config with twitch_password = password }
    | [ "twitch.channel"; channel ] -> { config with twitch_channel = channel }
    | [ unknown; _ ] -> unknown |> Printf.sprintf "`%s` config key is unknown." |> failwith
    | _ -> failwith "Your config file probably have some sort of syntax errors."
  in

  file_path
  |> read_file
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)
  |> List.map (fun s -> s |> String.split_on_char '=' |> List.map String.trim)
  |> List.fold_left update_config default_config
