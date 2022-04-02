open Caml_bot
open Storage.Db

let () = dispatch ensure_commands_table_exists |> Lwt_main.run

let () =
  match Sys.argv |> Array.to_list with
  | _ :: secret_path :: _ -> secret_path |> Config.from_file |> Twitch_irc.start
  | _ -> failwith "You need to specify the path to a valid secrets.conf file"
