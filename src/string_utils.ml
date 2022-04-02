let skip i s = String.sub s i (String.length s - i)

let lsplit2 str ~on =
  let open String in
  match index_opt str on with
  | Some pos -> Some (sub str 0 pos, sub str (pos + 1) (length str - pos - 1))
  | None -> None

let take_until_exn c s =
  match lsplit2 s ~on:c with
  | Some (hd, _) -> hd
  | None -> Printf.sprintf "%s has no character %c" s c |> failwith

let take_until c s =
  try take_until_exn c s with
  | Failure _ -> ""

let take_after_exn c s =
  match lsplit2 s ~on:c with
  | Some (_, rest) -> rest
  | None -> Printf.sprintf "%s has no character %c" s c |> failwith

let take_after c s =
  try take_after_exn c s with
  | Failure _ -> ""

let split_on_first_space =
  let re = Str.regexp "[ \t\r\n]" in
  function
  | s -> Str.bounded_split re s 2

let split ~str ~c =
  let rec rev_split' ~str ~i ~c ~acc =
    try
      let index = String.index_from str i c in
      let before = String.sub str i (index - i) in
      rev_split' ~str ~c ~i:(index + 1) ~acc:(before :: acc)
    with
    | Not_found -> String.sub str i (String.length str - i) :: acc
  in
  List.rev (rev_split' ~str ~i:0 ~c ~acc:[])

let split1_exn ~str ~c =
  let index = String.index str c in
  let before = String.sub str 0 index in
  let after = String.sub str (index + 1) (String.length str - index - 1) in
  (before, after)

let has_char c s =
  match String.index_opt s c with
  | Some _ -> true
  | None -> false
