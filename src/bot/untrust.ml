let handle ~args ~user =
  let params =
    match String_utils.split_on_first_space args with
    | [ name ] -> Some name
    | _ -> None
  in

  match params with
  | Some username -> (
    match Storage.Trusted_users.destroy username with
    | Ok () ->
      "O usuário @"
      ^ username
      ^ " não tem mais permissões especiais. !untrust é um comando apenas para moderadores"
    | Error _ -> "Não foi possível desautorizar o usuário e a culpa é do criador desse bot")
  | None -> "Uso: `!untrust $1` - Esse é um comando apenas para moderadores"
