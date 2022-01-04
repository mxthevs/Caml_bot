let handle (location, sender) =
  let format = {|%l: %c 🌡️%t\n|} in
  let url = "https://wttr.in/" ^ location ^ "?format=" ^ format in

  match Http.get_sync url with
  | Ok body -> body
  | Error _ -> "@" ^ sender ^ ", esse lugar nem existe cara"
