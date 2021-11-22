type strtup2 = string * string

type handler = string * string -> string

type builtin_command = { name : string; handler : handler; mod_only : bool }

let builtin_commands =
  [
    { name = "addcmd"; handler = Bot.Addcmd.handle; mod_only = true };
    { name = "delcmd"; handler = Bot.Delcmd.handle; mod_only = true };
    { name = "flip"; handler = Bot.Flip.handle; mod_only = false };
    { name = "clima"; handler = Bot.Wttr.handle; mod_only = false };
    { name = "roleta"; handler = Bot.Rr.handle; mod_only = false };
  ]

let filter_mod_only_commands commands = List.filter (fun cmd -> not cmd.mod_only) commands

let find_builtin_command name command_list ~include_mod_only =
  let list = if include_mod_only then command_list else filter_mod_only_commands command_list in
  List.find_opt (fun cmd -> cmd.name = name) list

let show_commands_handler = "comandos"

let build_command_string acc el = if acc = "" then acc ^ "!" ^ el else acc ^ " !" ^ el

let show_builtin_commands command_list =
  command_list
  |> filter_mod_only_commands
  |> List.map (fun cmd -> cmd.name)
  |> List.fold_left build_command_string ""

let show_external_commands () =
  match Bot.Storage.index () with
  | Ok command_list ->
      command_list
      |> List.map (fun (cmd : Bot.Storage.command) -> cmd.name)
      |> List.fold_left build_command_string ""
  | Error _ -> ""

let show_commands sender command_list =
  sender
  ^ " , os comandos são: "
  ^ show_builtin_commands command_list
  ^ " "
  ^ show_external_commands ()

let parse_as_builtin ((message, sender) : strtup2) ~handler : string = handler (message, sender)

let parse_as_external ((message, _sender) : strtup2) =
  (* TODO: come up with a way to tag the sender in reply *)
  match Bot.Storage.show message with Ok command -> command | Error _ -> None

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

let say (s : string) = Some s

let parse message sender =
  let command, content = extract_params message in

  if command = show_commands_handler then Some (show_commands sender builtin_commands)
  else
    (* TODO: actually verify if the sender is a mod or not *)
    let handler = find_builtin_command command builtin_commands ~include_mod_only:true in

    match handler with
    | Some { handler; _ } -> say (parse_as_builtin (content, sender) ~handler)
    | None -> (
        match parse_as_external (command, sender) with
        | Some { reply; _ } -> say reply
        | None -> say (Printf.sprintf "Não conheço esse comando \"!%s\", %s" command sender))
