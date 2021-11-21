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
