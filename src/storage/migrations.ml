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

let migrate_database () = Database.dispatch ensure_commands_table_exists
