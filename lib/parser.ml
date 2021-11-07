let skip i s = String.sub s i (String.length s - i)

let take_until c s = s |> String.split_on_char c |> List.hd

let take_after_exn c s =
  let lsplit2 str ~on =
    let open String in
    match index_opt str on with
    | Some pos -> Some (sub str 0 pos, sub str (pos + 1) (length str - pos - 1))
    | None -> None
  in

  match lsplit2 s ~on:c with
  | Some (_, rest) -> rest
  | None -> Printf.sprintf "%s has no character %c" s c |> failwith

let take_after c s = try take_after_exn c s with Failure _ -> ""
