let split_on_first_space =
  let re = Str.regexp "[ \t\r\n]" in
  function
  | s -> Str.bounded_split re s 2

let handle (text, _sender) =
  let params =
    match split_on_first_space text with
    | [ name ] -> Some name
    | _ -> None
  in

  match params with
  | Some command -> (
    match Storage.destroy command with
    | Ok () -> "Comando deletado com sucesso. !delcmd é um comando apenas para moderadores"
    | Error _ -> "Não foi possível deletar o comando. Tem certeza que ele existe?")
  | None -> "Uso: `!delcmd $1` - Esse é um comando apenas para moderadores"
