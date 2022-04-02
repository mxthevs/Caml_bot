let handle ~args ~user =
  let params =
    match String_utils.split_on_first_space args with
    | [ name; reply ] -> Some (name, reply)
    | _ -> None
  in

  match params with
  | Some (command, response) -> (
    match Storage.update { name = command; reply = response } with
    | Ok () -> "Comando atualizado com sucesso. !updcmd é um comando apenas para moderadores"
    | Error (`Not_found _) -> "O comando que você está tentando atualizar não existe"
    | Error (`Msg _) -> "Não foi possível atualizar o comando e a culpa é do criador desse bot")
  | None -> "Uso: `!updcmd $1 $2` - Esse é um comando apenas para moderadores"
