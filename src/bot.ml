module Reply = struct
  type funcall =
    | User
    | FstOrUser
    | Noop

  type t = funcall * string

  let funcall_command_of_string = function
    | {|%user()|} -> User
    | {|%or(fst,user)|} -> FstOrUser
    | _ -> Noop

  let get_reply response =
    (* TODO: capture `(` and `)` correctly instead of using `.`*)
    let re = Str.regexp {|^%\b+.\(\b+\)?\(,\)?\(\b+\)?.|} in
    let has_function = Str.string_match re response 0 in
    let funcall =
      if has_function then
        String_utils.split_on_first_space response
      else
        [ response ]
    in

    match funcall with
    | [ call; arguments ] -> (funcall_command_of_string call, arguments)
    | arguments -> (Noop, List.nth arguments 0)

  let fmt_reply response ~user ~args =
    match get_reply response with
    | User, parsed_reply -> user ^ ", " ^ parsed_reply
    | FstOrUser, parsed_reply ->
      let tagged = List.nth_opt (String_utils.split_on_first_space args) 0 in
      Option.value tagged ~default:user ^ ", " ^ parsed_reply
    | Noop, parsed_reply -> parsed_reply
end

module type HANDLER = sig
  val handle : args:string -> user:string -> string
end

type command =
  | Addcmd  of [ `Authorized ]
  | Updcmd  of [ `Authorized ]
  | Delcmd  of [ `Authorized ]
  | Flip
  | Wttr
  | Rr
  | Node    of [ `Authorized ]
  | Trust   of [ `Authorized ]
  | Untrust of [ `Authorized ]
  | Sync    of [ `Authorized ]

let command_of_string = function
  | "addcmd" -> Ok (Addcmd `Authorized)
  | "updcmd" -> Ok (Updcmd `Authorized)
  | "delcmd" -> Ok (Delcmd `Authorized)
  | "flip" -> Ok Flip
  | "clima" -> Ok Wttr
  | "roleta" -> Ok Rr
  | "node" -> Ok (Node `Authorized)
  | "trust" -> Ok (Trust `Authorized)
  | "untrust" -> Ok (Untrust `Authorized)
  | "sync" -> Ok (Sync `Authorized)
  | _ -> Error ()

let command_to_string = function
  | Addcmd `Authorized -> "addcmd"
  | Updcmd `Authorized -> "updcmd"
  | Delcmd `Authorized -> "delcmd"
  | Flip -> "flip"
  | Wttr -> "clima"
  | Rr -> "roleta"
  | Node `Authorized -> "node"
  | Trust `Authorized -> "trust"
  | Untrust `Authorized -> "untrust"
  | Sync `Authorized -> "sync"

let is_mod_only = function
  | Addcmd `Authorized -> true
  | Updcmd `Authorized -> true
  | Delcmd `Authorized -> true
  | Flip -> false
  | Wttr -> false
  | Rr -> false
  | Node `Authorized -> true
  | Trust `Authorized -> true
  | Untrust `Authorized -> true
  | Sync `Authorized -> true

let all =
  [
    Addcmd `Authorized;
    Updcmd `Authorized;
    Delcmd `Authorized;
    Flip;
    Wttr;
    Rr;
    Node `Authorized;
    Trust `Authorized;
    Untrust `Authorized;
    Sync `Authorized;
  ]

let list_external () =
  match Storage.Command.index () with
  | Ok command_list ->
    command_list |> List.map (fun (cmd : Storage.Command.external_command) -> (cmd.name, cmd.reply))
  | Error _ -> []

let get_handler t : (module HANDLER) =
  match t with
  | Addcmd `Authorized -> (module Addcmd)
  | Updcmd `Authorized -> (module Updcmd)
  | Delcmd `Authorized -> (module Delcmd)
  | Flip -> (module Flip)
  | Wttr -> (module Wttr)
  | Rr -> (module Rr)
  | Node `Authorized -> (module Node)
  | Trust `Authorized -> (module Trust)
  | Untrust `Authorized -> (module Untrust)
  | Sync `Authorized ->
    (module Sync.Make (struct
      type t = command

      let builtin = all
      let external_ = list_external ()
      let is_mod_only = is_mod_only
      let to_string = command_to_string
    end))

let parse ~args ~user ~handler : string = handler ~args:(String.trim args) ~user

let parse_as_external ~command =
  match Storage.Command.show command with
  | Ok command -> command
  | Error _ -> None

let is_authorized user =
  let user = Storage.Trusted_users.show user in

  match user with
  | Ok maybe_user -> (
    match maybe_user with
    | Some _ -> true
    | None -> false)
  | Error _ -> false

let extract_params message =
  let open String_utils in
  let command =
    if has_char ' ' message then
      message |> skip 1 |> take_until ' '
    else
      message |> skip 1 |> String.trim
  in
  let rest = message |> take_after ' ' in
  (command, rest)

let handle_command ~message ~user =
  let name, args = extract_params message in

  let response =
    let command = command_of_string name in
    match command with
    | Ok command ->
      let module Handler = (val get_handler command) in
      if is_mod_only command && (not @@ is_authorized user) then
        Some (Printf.sprintf "@%s, esse comando é apenas para usuários autorizados" user)
      else
        Some (parse ~handler:Handler.handle ~args ~user)
    | Error () -> (
      match parse_as_external ~command:(String.trim name) with
      | Some { reply; _ } -> Some reply
      | None -> None)
  in

  match response with
  | Some response -> Ok (Reply.fmt_reply response ~user ~args)
  | None -> Error ()
