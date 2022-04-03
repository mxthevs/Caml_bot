let handle ~args ~user =
  let params =
    match String_utils.split_on_first_space args with
    | [ name ] -> Some name
    | _ -> None
  in

  match params with
  | Some username -> (
    let user = Storage.Trusted_users.show username in
    let already_exists =
      match user with
      | Ok maybe_user -> (
        match maybe_user with
        | Some _ -> true
        | None -> false)
      | Error _ -> false
    in

    if already_exists then
      "O usuário @" ^ username ^ " já tem permissões especiais"
    else
      match Storage.Trusted_users.store username with
      | Ok () ->
        "O usuário @"
        ^ username
        ^ " agora tem permissões especiais. !trust é um comando apenas para moderadores"
      | Error _ -> "Não foi possível autorizar o usuário e a culpa é do criador desse bot")
  | None -> "Uso: `!trust $1` - Esse é um comando apenas para moderadores"
