open Lwt
open Cohttp
open Cohttp_lwt_unix

let get_sync url =
  let get () =
    Client.get (Uri.of_string url) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in

    if Code.is_success code then
      body |> Cohttp_lwt.Body.to_string >|= fun body -> Ok body
    else
      Lwt.return (Error (`Msg (Printf.sprintf "Request failed with code %d" code)))
  in

  Lwt_main.run (get ())

let post_sync ?headers url ~body =
  let post () =
    Client.post (Uri.of_string url)
      ~headers:
        (match headers with
        | Some h -> Cohttp.Header.of_list h
        | None -> Cohttp.Header.init ())
      ~body:(Cohttp_lwt.Body.of_string body)
    >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in

    if Code.is_success code then
      body |> Cohttp_lwt.Body.to_string >|= fun body -> Ok body
    else
      Lwt.return (Error (`Msg (Printf.sprintf "Request failed with code %d" code)))
  in

  Lwt_main.run (post ())
