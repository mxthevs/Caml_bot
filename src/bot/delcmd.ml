let handle ~args ~user =
  let params =
    match String_utils.split_on_first_space args with
    | [ name ] -> Some name
    | _ -> None
  in

  match params with
  | Some command -> (
    match Storage.Command.destroy command with
    | Ok () -> "Comando deletado com sucesso. !delcmd é um comando apenas para moderadores"
    | Error (`Not_found _) -> "O comando que você está tentando deletar não existe"
    | Error (`Msg _) -> "Não foi possível deletar o comando e a culpa é do criador desse bot")
  | None -> "Uso: `!delcmd $1` - Esse é um comando apenas para moderadores"
