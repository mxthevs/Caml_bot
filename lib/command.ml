type strtup2 = string * string

type handler = string * string -> string

type builtin_command = { name : string; handler : handler }

let builtin_commands = [ { name = "flip"; handler = Bot.Flip.handle } ]

let parse_as_builtin ((message, sender) : strtup2) ~handler : string = handler (message, sender)

let extract_params message =
  let open Parser in
  if message <> "" && message.[0] = '!' then
    let command = message |> skip 1 |> take_until ' ' in
    let rest = message |> take_after ' ' in
    (command, rest)
  else ("", "")

let parse message sender =
  let command, content = extract_params message in

  let handler = List.find_opt (fun cmd -> cmd.name = command) builtin_commands in

  match handler with
  | Some { handler; _ } -> Some (parse_as_builtin (content, sender) ~handler)
  | None -> None
