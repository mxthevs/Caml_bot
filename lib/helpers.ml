let list_to_option xs = match xs with x :: _ -> Some x | [] -> None

let split ~str ~c =
  let rec rev_split' ~str ~i ~c ~acc =
    try
      let index = String.index_from str i c in
      let before = String.sub str i (index - i) in
      rev_split' ~str ~c ~i:(index + 1) ~acc:(before :: acc)
    with Not_found -> String.sub str i (String.length str - i) :: acc
  in
  List.rev (rev_split' ~str ~i:0 ~c ~acc:[])

let split1_exn ~str ~c =
  let index = String.index str c in
  let before = String.sub str 0 index in
  let after = String.sub str (index + 1) (String.length str - index - 1) in
  (before, after)

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let count_char l (ch : char) = List.fold_left (fun acc c -> if c = ch then acc + 1 else acc) 0 l

let read_file file_path =
  let ch = open_in file_path in
  let n = in_channel_length ch in
  let s = really_input_string ch n in
  close_in ch;
  s
