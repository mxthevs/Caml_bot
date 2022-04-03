type trusted_user = { username : string }

type external_user = {
  id : string;
  username : string;
}

let ( let* ) = Lwt.bind

module Async = struct
  exception User_not_found

  let index () =
    let read_all =
      [%rapper
        get_many
          {sql|
            SELECT @string{id}, @string{username}
            FROM trusted_users
          |sql}
          record_out]
        ()
    in
    let* users = Database.dispatch read_all in
    users |> List.map (fun { username; _ } -> { username }) |> Lwt.return

  let show username =
    let read_one =
      [%rapper
        get_opt
          {sql|
            SELECT @string{id}, @string{username}
            FROM trusted_users
            WHERE username = %string{username}
          |sql}
          record_out]
    in

    let* database_user = Database.dispatch (read_one ~username) in
    let user =
      match database_user with
      | Some { username; _ } -> Some { username }
      | None -> None
    in

    Lwt.return user

  let store username =
    let insert =
      [%rapper
        execute
          {sql|
            INSERT INTO trusted_users
            VALUES(%string{id}, %string{username})
          |sql}
          record_in]
    in
    let id = Uuidm.create `V4 |> Uuidm.to_string in
    Database.dispatch (insert { id; username })

  let destroy name =
    let* database_user = show name in

    match database_user with
    | Some user ->
      let delete =
        [%rapper
          execute
            {sql|
              DELETE FROM trusted_users
              WHERE username = %string{username}
            |sql}]
      in
      Database.dispatch (delete ~username:user.username)
    | None -> raise User_not_found
end

let index () =
  try Ok (Lwt_main.run (Async.index ())) with
  | Database.Query_failed error -> Error (Printf.sprintf "Could not retrieve users: %s" error)

let show name =
  try Ok (Lwt_main.run (Async.show name)) with
  | Database.Query_failed error -> Error (Printf.sprintf "Could not retrieve user: %s" error)

let store username =
  try Ok (Lwt_main.run (Async.store username)) with
  | Database.Query_failed error -> Error (`Msg (Printf.sprintf "Could not create user: %s" error))

let destroy name =
  try Ok (Lwt_main.run (Async.destroy name)) with
  | Async.User_not_found -> Error (`Not_found (Printf.sprintf "user %s not found" name))
  | Database.Query_failed error -> Error (`Msg (Printf.sprintf "Could not destroy user: %s" error))
