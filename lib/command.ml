type payload = string * string

type builtin_command = {
  name : string;
  handler : string * string -> string;
  mod_only : bool;
}

module Reply = struct
  type funcall =
    | Sender
    | FstOrSender
    | Noop

  type t = funcall * string

  let funcall_of_string = function
    | {|%user()|} -> Sender
    | {|%or(fst,user)|} -> FstOrSender
    | _ -> Noop

  let get_reply str =
    (* TODO: use a proper regex *)
    let has_function = String.contains str '(' && String.contains str ')' in
    let funcall = if has_function then Parser.split_on_first_space str else [ str ] in

    match funcall with
    | [ call; arguments ] -> (funcall_of_string call, arguments)
    | arguments -> (Noop, List.nth arguments 0)
end

let builtin_commands =
  [
    { name = "addcmd"; handler = Bot.Addcmd.handle; mod_only = true };
    { name = "updcmd"; handler = Bot.Updcmd.handle; mod_only = true };
    { name = "delcmd"; handler = Bot.Delcmd.handle; mod_only = true };
    { name = "flip"; handler = Bot.Flip.handle; mod_only = false };
    { name = "clima"; handler = Bot.Wttr.handle; mod_only = false };
    { name = "roleta"; handler = Bot.Rr.handle; mod_only = false };
  ]

let filter_mod_only_commands commands = List.filter (fun cmd -> not cmd.mod_only) commands

let find_builtin_command name command_list ~include_mod_only =
  let list = if include_mod_only then command_list else filter_mod_only_commands command_list in
  List.find_opt (fun cmd -> cmd.name = name) list

let build_command_string acc el = if acc = "" then acc ^ "!" ^ el else acc ^ " !" ^ el

let show_builtin_commands () =
  builtin_commands
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

let show_commands sender =
  "@" ^ sender ^ ", os comandos são: " ^ show_builtin_commands () ^ " " ^ show_external_commands ()

let parse_as_builtin ((message, sender) : payload) ~handler : string =
  handler (String.trim message, sender)

let parse_as_external ((message, _sender) : payload) =
  match Bot.Storage.show (String.trim message) with
  | Ok command -> command
  | Error _ -> None

let extract_params message =
  let open Parser in
  let open Helpers in
  if message <> "" && message.[0] = '!' then
    let command =
      if has_char ' ' message then
        message |> skip 1 |> take_until ' '
      else
        message |> skip 1 |> String.trim
    in
    let rest = message |> take_after ' ' in
    (command, rest)
  else
    ("", "")

let is_authorized sender =
  (* TODO: verify this dynamically *)
  [ "mxthevsz"; "caml_bot" ]
  |> List.find_opt (fun authorized -> authorized = String.lowercase_ascii sender)
  |> Option.is_some

let parse message sender =
  let command, content = extract_params message in

  let reply =
    match command with
    | "cmd"
    | "comandos" ->
      show_commands sender
    | other -> (
      let maybe_command =
        find_builtin_command command builtin_commands ~include_mod_only:(is_authorized sender)
      in

      match maybe_command with
      | Some { handler; _ } -> parse_as_builtin (content, sender) ~handler
      | None -> (
        match parse_as_external (command, sender) with
        | Some { reply; _ } -> reply
        | None -> Printf.sprintf "Não conheço esse comando %s, %s" command sender))
  in

  let open Reply in
  match get_reply reply with
  | Sender, parsed_reply -> Some (sender ^ ", " ^ parsed_reply)
  | FstOrSender, parsed_reply ->
    let maybe_fst = List.nth_opt (Parser.split_on_first_space content) 0 in

    let tagged =
      match maybe_fst with
      | Some fst -> fst
      | None -> sender
    in

    Some (tagged ^ ", " ^ parsed_reply)
  | Noop, parsed_reply -> Some parsed_reply
