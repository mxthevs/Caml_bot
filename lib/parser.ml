let skip i s = String.sub s i (String.length s - i)

let take_until c s = s |> String.split_on_char c |> List.hd

let take_after c s = s |> String.split_on_char c |> List.tl |> String.concat ""
