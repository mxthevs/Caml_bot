let split_on_first_space =
  let re = Str.regexp "[ \t\r\n]" in
  function s -> Str.bounded_split re s 2

let handle (text, _sender) =
  let params =
    match split_on_first_space text with [ name; reply ] -> Some (name, reply) | _ -> None
  in

  match params with
  | Some (command, response) -> (
      match Storage.store { name = command; reply = response } with
      | Ok () -> "Comando criado com sucesso. !addcmd é um comando apenas para moderadores"
      | Error _ -> "Não foi possível criar o comando e a culpa é do criador desse bot")
  | None -> "Uso: `!addcmd $1 $2` - Esse é um comando apenas para moderadores"
