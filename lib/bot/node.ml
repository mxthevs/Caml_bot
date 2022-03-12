module Process = Lwt_process

let ( let* ) = Lwt.bind

let read_file file_path =
  let ch = open_in file_path in
  let n = in_channel_length ch in
  let s = really_input_string ch n in
  close_in ch;
  s

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

  (* TODO: recover error information *)
  let result =
    match status with
    | Unix.WSTOPPED sig_ -> Printf.sprintf "Process was stopped by signal %d" sig_
    | Unix.WSIGNALED sig_ -> Printf.sprintf "Process was killed by signal %d" sig_
    | Unix.WEXITED code when code != 0 -> Printf.sprintf "Process exited with %d code" code
    | Unix.WEXITED _ ->
      let content = read_file file in
      let () = Sys.remove file in
      content
  in

  Lwt.return result

let handle (input, _sender) = Lwt_main.run (handle' input)
