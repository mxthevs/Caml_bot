type strtup2 = string * string

type handler = string * string -> string

type builtin_command = { name : string; handler : handler }

let builtin_commands =
  [ { name = "flip"; handler = Bot.Flip.handle }; { name = "clima"; handler = Bot.Wttr.handle } ]

let show_builtin_commands sender command_list =
  sender
  ^ " -> "
  ^ (command_list
    |> List.map (fun cmd -> cmd.name)
    |> List.fold_left (fun acc el -> if acc = "" then acc ^ "!" ^ el else acc ^ " !" ^ el) "")

let parse_as_builtin ((message, sender) : strtup2) ~handler : string = handler (message, sender)

let extract_params message =
  let open Parser in
  let open Helpers in
  if message <> "" && message.[0] = '!' then
    let command =
      if has_char ' ' message then message |> skip 1 |> take_until ' '
      else message |> skip 1 |> String.trim
    in
    let rest = message |> take_after ' ' in
    (command, rest)
  else ("", "")

let parse message sender =
  let command, content = extract_params message in

  if command = "comandos" then Some (show_builtin_commands sender builtin_commands)
  else
    let handler = List.find_opt (fun cmd -> cmd.name = command) builtin_commands in

    match handler with
    | Some { handler; _ } -> Some (parse_as_builtin (content, sender) ~handler)
    | None -> None
