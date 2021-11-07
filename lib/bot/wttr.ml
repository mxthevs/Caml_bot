let handle (location, _sender) =
  let format = {|%l:+%c+%t\n|} in
  let url = "https://wttr.in/" ^ location ^ "?format=" ^ format in

  match Http.get_sync url with
  | Ok body -> body
  | Error error ->
      let message = Piaf.Error.to_string error in
      failwith ("Error: " ^ message)
