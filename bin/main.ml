open Caml_bot

let () = Storage.dispatch Storage.migrate |> Lwt_main.run

let () =
  match Sys.argv |> Array.to_list with
  | _ :: secret_path :: _ -> secret_path |> Config.from_file |> Twitch_irc.start
  | _ -> failwith "You need to specify the path to a valid secrets.conf file"
