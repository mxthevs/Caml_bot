open Lwt
open Cohttp
open Cohttp_lwt_unix

let get_sync url =
  let get () =
    Client.get (Uri.of_string url) >>= fun (resp, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body -> body
  in

  Lwt_main.run (get ())
