let handle ~args ~user =
  let params =
    match String_utils.split_on_first_space args with
    | [ name; reply ] -> Some (name, reply)
    | _ -> None
  in

  match params with
  | Some (command, response) -> (
    match Storage.Command.store { name = command; reply = response } with
    | Ok () -> "Comando criado com sucesso. !addcmd é um comando apenas para moderadores"
    | Error _ -> "Não foi possível criar o comando e a culpa é do criador desse bot")
  | None -> "Uso: `!addcmd $1 $2` - Esse é um comando apenas para moderadores"
