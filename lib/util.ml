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
