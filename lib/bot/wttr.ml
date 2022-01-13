let handle (location, sender) =
  let format = {|%l: %c 🌡️%t\n|} in
  let url = "https://wttr.in/" ^ location ^ "?format=" ^ format in

  Http.get_sync url
