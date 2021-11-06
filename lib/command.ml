type handler = string -> string

type builtin_command = { name : string; handler : handler }

let builtin_commands = [ { name = "flip"; handler = Bot.Flip.handle } ]

let parse_as_builtin (message : string) ~handler : string = handler message

let extract_params message =
  if message <> "" && message.[0] = '!' then
    let command = message |> Parser.skip 1 |> Parser.take_until ' ' in
    let rest = Parser.take_after ' ' message in
    (command, rest)
  else ("", "")

let parse message _sender =
  let command, content = extract_params message in

  let handler = List.find_opt (fun cmd -> cmd.name = command) builtin_commands in

  match handler with
  | Some { handler; _ } -> Some (parse_as_builtin content ~handler)
  | None -> None
