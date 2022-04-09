module Make (C : sig
  type t

  val builtin : t list
  val external_ : (string * string) list
  val is_mod_only : t -> bool
  val to_string : t -> string
end) =
struct
  let base_url = "https://api.github.com/gists/"
  let gist_id = "69fb84570a74c18dfcbc48cbfc86ebbc"

  let template =
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
    |}

  let make_body data = Str.(replace_first (regexp "COMMANDS_TABLE") data template)

  let level = function
    | true -> "❌"
    | false -> "✅"

  let handle ~args ~user =
    let table_header =
      [ "| Comando | Resposta | Todos podem usar |"; "| ------- | -------- | :--------------: |" ]
    in

    let table_body_builtin =
      C.builtin
      |> List.map (fun c ->
             Printf.sprintf "| !%s | *builtin* | %s |" (C.to_string c) (level (C.is_mod_only c)))
    in

    let table_body_external =
      C.external_
      |> List.map (fun (cmd, reply) -> Printf.sprintf "| !%s | %s | ✅ |" cmd (String.trim reply))
    in

    let table = table_header @ table_body_builtin @ table_body_external in
    let table = table |> List.fold_left (fun acc el -> acc ^ {|\n|} ^ el) "" in

    let response =
      Http.patch_sync (base_url ^ gist_id) ~body:(make_body table)
        ~headers:
          [
            ("accept", "application/vnd.github.v3+json");
            ("Authorization", "token " ^ Sys.getenv "GITHUB_TOKEN");
          ]
    in

    match response with
    | Ok body -> "Comandos atualizados com sucesso. !sync é um comando apenas para moderadores."
    | Error (`Body err) ->
      "Não foi possível atualizar os comandos e a culpa é do criador desse bot."
end
