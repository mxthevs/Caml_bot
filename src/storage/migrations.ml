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

let ensure_trusted_users_table_exists =
  [%rapper
    execute
      {sql|
        CREATE TABLE IF NOT EXISTS trusted_users (
          id       UUID PRIMARY KEY NOT NULL,
          username VARCHAR (255) UNIQUE NOT NULL
        );
      |sql}]
    ()
