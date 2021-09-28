module Tag = struct
  type t = { key : string; value : string }

  let empty = { key = ""; value = "" }

  let to_string tag = Printf.sprintf "{ key = %s; value = %s }" tag.key tag.value
end

module Message = struct
  type t = {
    raw_content : string;
    tags : Tag.t list;
    prefix : string option;
    command : string option;
    params : string list;
  }

  let empty = { raw_content = ""; tags = []; prefix = None; command = None; params = [] }

  let to_string message =
    Printf.sprintf "{ raw_content = %s; tags = [%s]; prefix = %s; command = %s; params = [%s]}"
      message.raw_content
      (message.tags |> List.fold_left (fun acc tag -> acc ^ "," ^ Tag.to_string tag) "")
      (match message.prefix with Some a -> a | None -> "[NONE]")
      (match message.command with Some a -> a | None -> "[NONE]")
      (message.params |> List.fold_left (fun acc param -> acc ^ "," ^ param) "")
end

module S = struct
  let skip n s = String.sub s n (String.length s - 1)

  let take_until c s = s |> String.split_on_char c |> List.hd
end
