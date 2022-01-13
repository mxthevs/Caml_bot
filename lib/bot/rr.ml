let random_float ~bound = Random.float bound
let random_int ~bound = Random.int bound
let phrases = [ "Deu sorte"; "Quase"; "Passou perto"; "Por um triz"; "Passou raspando" ]

let kaomojis = [ "ʘ‿ʘ"; "ಠ_ಠ"; "◔_◔"; "ಠ‿ಠ"; "(⊙_⊙)"; "ヽ(°〇°)ﾉ"; "(¬ ¬ )" ]

let handle (_text, sender) =
  Random.init (Int.of_float (Unix.time ()));

  let chance = random_float ~bound:0.6 in
  let message = List.nth phrases (random_int ~bound:(List.length phrases)) in
  let kaomoji = List.nth kaomojis (random_int ~bound:(List.length kaomojis)) in

  let reply =
    if chance < 0.1 then "/timeout " ^ sender ^ " 1" else message ^ ", " ^ sender ^ " " ^ kaomoji
  in

  reply
