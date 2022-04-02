module Reply = struct
  type funcall =
    | User
    | FstOrUser
    | Noop

  type t = funcall * string

  let funcall_of_string = function
    | {|%user()|} -> User
    | {|%or(fst,user)|} -> FstOrUser
    | _ -> Noop

  let get_reply str =
    (* TODO: capture `(` and `)` correctly instead of using `.`*)
    let re = Str.regexp {|^%\b+.\(\b+\)?\(,\)?\(\b+\)?.|} in
    let has_function = Str.string_match re str 0 in
    let funcall = if has_function then Parser.split_on_first_space str else [ str ] in

    match funcall with
    | [ call; arguments ] -> (funcall_of_string call, arguments)
    | arguments -> (Noop, List.nth arguments 0)
end

module type HANDLER = sig
  val handle : args:string -> user:string -> string
end

type t =
  | Addcmd of [ `Mod_only ]
  | Updcmd of [ `Mod_only ]
  | Delcmd of [ `Mod_only ]
  | Flip
  | Wttr
  | Rr
  | Node   of [ `Mod_only ]

let of_string = function
  | "addcmd" -> Ok (Addcmd `Mod_only)
  | "updcmd" -> Ok (Updcmd `Mod_only)
  | "delcmd" -> Ok (Delcmd `Mod_only)
  | "flip" -> Ok Flip
  | "clima" -> Ok Wttr
  | "roleta" -> Ok Rr
  | "node" -> Ok (Node `Mod_only)
  | _ -> Error ()

let to_string = function
  | Addcmd `Mod_only -> "addcmd"
  | Updcmd `Mod_only -> "updcmd"
  | Delcmd `Mod_only -> "delcmd"
  | Flip -> "flip"
  | Wttr -> "clima"
  | Rr -> "roleta"
  | Node `Mod_only -> "node"

let is_mod_only = function
  | Addcmd `Mod_only -> true
  | Updcmd `Mod_only -> true
  | Delcmd `Mod_only -> true
  | Flip -> false
  | Wttr -> false
  | Rr -> false
  | Node `Mod_only -> true

let all = [ Addcmd `Mod_only; Updcmd `Mod_only; Delcmd `Mod_only; Flip; Wttr; Rr; Node `Mod_only ]
let public = List.filter (fun command -> not @@ is_mod_only command) all

let list_public () =
  public |> List.map to_string |> List.fold_left (fun acc el -> acc ^ " !" ^ el) ""

let list_external () =
  match Storage.index () with
  | Ok command_list ->
    command_list
    |> List.map (fun (cmd : Storage.external_command) -> cmd.name)
    |> List.fold_left (fun acc el -> acc ^ " !" ^ el) ""
  | Error _ -> ""

let list ~to_:user = "@" ^ user ^ ", os comandos são: " ^ list_public () ^ list_external ()

let get_handler t : (module HANDLER) =
  match t with
  | Addcmd `Mod_only -> (module Addcmd)
  | Updcmd `Mod_only -> (module Updcmd)
  | Delcmd `Mod_only -> (module Delcmd)
  | Flip -> (module Flip)
  | Wttr -> (module Wttr)
  | Rr -> (module Rr)
  | Node `Mod_only -> (module Node)

let parse ~args ~user ~handler : string = handler ~args:(String.trim args) ~user

let parse_as_external ~command =
  match Storage.show command with
  | Ok command -> command
  | Error _ -> None

let is_authorized user =
  (* TODO: verify this dynamically *)
  (* Create the `trusted user` concept *)
  (* !trust ${user} || !untrust ${user} *)
  [ "mxthevsz"; "caml_bot" ]
  |> List.find_opt (fun authorized -> authorized = String.lowercase_ascii user)
  |> Option.is_some

let handle ~message ~user =
  let name, args = Helpers.extract_params message in

  let response =
    match name with
    | "comandos" -> Some (list ~to_:user)
    | _ -> (
      let command = of_string name in
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
        | None -> None))
  in

  match response with
  | Some response -> (
    let open Reply in
    match get_reply response with
    | User, parsed_reply -> Ok (user ^ ", " ^ parsed_reply)
    | FstOrUser, parsed_reply ->
      let tagged = List.nth_opt (Parser.split_on_first_space args) 0 in
      Ok (Option.value tagged ~default:user ^ ", " ^ parsed_reply)
    | Noop, parsed_reply -> Ok parsed_reply)
  | None -> Error ()
