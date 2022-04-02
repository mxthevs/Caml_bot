type t = {
  nick : string;
  pass : string;
  chan : string;
  debug : bool;
}

let empty = { nick = ""; pass = ""; chan = ""; debug = false }

let from_file file_path =
  let update_config config pair =
    match pair with
    | [ "twitch.nick"; nick ] -> { config with nick }
    | [ "twitch.pass"; pass ] -> { config with pass }
    | [ "twitch.chan"; chan ] -> { config with chan }
    | [ "client.debug"; debug ] -> { config with debug = bool_of_string debug }
    | [ unknown; _ ] -> unknown |> Printf.sprintf "`%s` config key is unknown." |> failwith
    | _ -> failwith "Your config file probably have some sort of syntax errors."
  in

  file_path
  |> File_utils.read_file
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)
  |> List.map (fun s -> s |> String.split_on_char '=' |> List.map String.trim)
  |> List.fold_left update_config empty
