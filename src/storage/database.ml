module type DB = Rapper_helper.CONNECTION

exception Query_failed of string

let ( let* ) = Lwt.bind

let connection_url =
  let username = Sys.getenv "USERNAME" in
  let password = Sys.getenv "PASSWORD" in
  let database = Sys.getenv "DATABASE" in
  let port = Sys.getenv "PORT" in

  Printf.sprintf "postgresql://%s:%s@localhost:%s/%s" username password port database

let pool =
  match Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string connection_url) with
  | Ok pool -> pool
  | Error error -> failwith (Caqti_error.show error)

let dispatch f =
  let* result = Caqti_lwt.Pool.use f pool in
  match result with
  | Ok data -> Lwt.return data
  | Error error -> Lwt.fail (Query_failed (Caqti_error.show error))
