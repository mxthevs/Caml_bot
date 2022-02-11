module Database = struct
  let connection_url =
    let username = Sys.getenv "USERNAME" in
    let password = Sys.getenv "PASSWORD" in
    let database = Sys.getenv "DATABASE" in
    let port = Sys.getenv "PORT" in

    Printf.sprintf "postgresql://%s:%s@localhost:%s/%s" username password port database
end
