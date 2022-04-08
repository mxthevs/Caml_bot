module Make (C : sig
  type t

  val builtin : t list
  val external_ : string list
  val is_mod_only : t -> bool
  val to_string : t -> string
end) =
struct
  let handle ~args ~user =
    let table_header = [ "| Comando | Público |"; "|---------|---------|" ] in
    let table_body_builtin =
      C.builtin
      |> List.map (fun c ->
             Printf.sprintf "| !%s | %s |" (C.to_string c)
               (match C.is_mod_only c with
               | true -> "❌"
               | false -> "✅"))
    in
    let table_body_external = C.external_ |> List.map (Printf.sprintf "| !%s | ✅ |") in

    let table = table_header @ table_body_builtin @ table_body_external in
    let table = table |> List.fold_left (fun acc el -> acc ^ {|\n|} ^ el) "" in

    let response =
      let body =
        Str.(
          replace_first (regexp "COMMANDS_TABLE") table
            {|
              {
                "description": "Caml_bot Commands",
                "public": true,
                "files": {
                  "commands.md": {
                    "content": "COMMANDS_TABLE"
                  }
                }
              }
            |})
      in

      (* TODO: update gists *)
      Http.post_sync "https://api.github.com/gists" ~body
        ~headers:
          [
            ("accept", "application/vnd.github.v3+json");
            ("Authorization", "token " ^ Sys.getenv "GITHUB_TOKEN");
          ]
    in

    match response with
    | Ok body -> "Comandos atualizados com sucesso. !sync é um comando apenas para moderadores."
    | Error _ -> "Oops, deu ruim ao atualizar os comandos."
end
