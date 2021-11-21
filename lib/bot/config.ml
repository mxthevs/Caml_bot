module Database = struct
  let connection_url =
    let username = Sys.getenv "USERNAME" in
    let password = Sys.getenv "PASSWORD" in
    let database = Sys.getenv "DATABASE" in

    "postgresql://" ^ username ^ ":" ^ password ^ "@localhost:5432/" ^ database
end
