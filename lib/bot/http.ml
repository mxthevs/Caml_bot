open Piaf

let get_sync url =
  let open Lwt_result.Syntax in
  let get () =
    let* response = Client.Oneshot.get (Uri.of_string url) in

    if Status.is_successful response.status then
      Body.to_string response.body
    else
      let message = Status.to_string response.status in
      Lwt.return (Error (`Msg message))
  in

  Lwt_main.run (get ())
