module Process = Lwt_process

let ( let* ) = Lwt.bind

let handle' input =
  let file = Printf.sprintf "tmp/%d.out" (int_of_float (Unix.time ())) in
  let out_channel = open_out file in

  (* TODO: maybe this is bad *)
  let* status =
    Process.exec
      ~stdout:(`FD_copy (Unix.descr_of_out_channel out_channel))
      ~stderr:(`FD_copy (Unix.descr_of_out_channel out_channel))
      ("node", [| "node"; "-e"; "console.log(" ^ input ^ ")" |])
  in

  (* TODO: Twitch does not support new lines *)
  let output = File_utils.read_file file in
  let result =
    match status with
    | Unix.WSTOPPED sig_ -> Printf.sprintf "Process was stopped by signal %d" sig_
    | Unix.WSIGNALED sig_ -> Printf.sprintf "Process was killed by signal %d" sig_
    | Unix.WEXITED code when code != 0 ->
      List.nth
        (output
        |> String.split_on_char '\n'
        |> List.filteri (fun i _ -> i = 4)
        |> List.map String.trim)
        0
    | Unix.WEXITED _ ->
      Sys.remove file;
      output
  in

  Lwt.return result

let handle ~args ~user =
  try Lwt_main.run (handle' args) with
  | Failure _ -> "Tentou quebrar o bot né, safadjenho?"
