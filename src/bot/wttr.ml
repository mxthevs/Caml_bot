let handle ~args:location ~user =
  let format = {|%l: %c 🌡️%t\n|} in
  let url = "https://wttr.in/" ^ location ^ "?format=" ^ format ^ "&m" in

  match Http.get_sync url with
  | Ok weather -> weather
  | Error _ -> "@" ^ user ^ ", esse lugar nem existe cara."
