type strtup2 = string * string

type handler = string * string -> string

type builtin_command = { name : string; handler : handler; mod_only : bool }

let builtin_commands =
  [
    { name = "addcmd"; handler = Bot.Addcmd.handle; mod_only = true };
    { name = "flip"; handler = Bot.Flip.handle; mod_only = false };
    { name = "clima"; handler = Bot.Wttr.handle; mod_only = false };
    { name = "roleta"; handler = Bot.Rr.handle; mod_only = false };
  ]

let filter_mod_only_commands commands = List.filter (fun cmd -> not cmd.mod_only) commands

let find_command name command_list ~include_mod_only =
  let list = if include_mod_only then command_list else filter_mod_only_commands command_list in
  List.find_opt (fun cmd -> cmd.name = name) list

let show_builtin_handler = "comandos"

let show_builtin_commands sender command_list =
  sender
  ^ " , os comandos são: "
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

  if command = show_builtin_handler then Some (show_builtin_commands sender builtin_commands)
  else
    let handler = find_command command builtin_commands ~include_mod_only:true in

    match handler with
    | Some { handler; _ } -> Some (parse_as_builtin (content, sender) ~handler)
    | None -> None
