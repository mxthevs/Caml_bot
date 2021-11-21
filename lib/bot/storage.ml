type command = { name : string; reply : string }

type database_command = { id : string; name : string; reply : string }

let ( let* ) = Lwt.bind

module Db = struct
  module type DB = Rapper_helper.CONNECTION

  exception Query_failed of string

  let pool =
    match Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string Config.Database.connection_url) with
    | Ok pool -> pool
    | Error error -> failwith (Caqti_error.show error)

  let dispatch f =
    let* result = Caqti_lwt.Pool.use f pool in
    match result with
    | Ok data -> Lwt.return data
    | Error error -> Lwt.fail (Query_failed (Caqti_error.show error))

  let ensure_commands_table_exists =
    [%rapper
      execute
        {sql|
          CREATE TABLE IF NOT EXISTS commands (
            id    UUID PRIMARY KEY NOT NULL,
            name  VARCHAR (25) UNIQUE NOT NULL,
            reply VARCHAR (255) NOT NULL
          );
        |sql}]
      ()
end

module Async = struct
  let show name =
    let read_one =
      [%rapper
        get_opt
          {sql|
            SELECT @string{id}, @string{name}, @string{reply}
            FROM commands
            WHERE name = %string{name}
          |sql}
          record_out]
    in

    let* database_command = Db.dispatch (read_one ~name) in
    let command =
      match database_command with Some { name; reply; _ } -> Some { name; reply } | None -> None
    in

    Lwt.return command

  let store ({ name; reply } : command) =
    let insert =
      [%rapper
        execute
          {sql|
            INSERT INTO commands
            VALUES(%string{id}, %string{name}, %string{reply})
          |sql}
          record_in]
    in
    let id = Uuidm.create `V4 |> Uuidm.to_string in
    Db.dispatch (insert { id; name; reply })
end

let show name =
  try Ok (Lwt_main.run (Async.show name))
  with Db.Query_failed error -> Error (Printf.sprintf "Could not retrieve command: %s" error)

let store ({ name; reply } : command) =
  try Ok (Lwt_main.run (Async.store { name; reply }))
  with Db.Query_failed error -> Error (Printf.sprintf "Could not create command: %s" error)
