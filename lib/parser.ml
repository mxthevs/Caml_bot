module Tag = struct
  type t = { key : string; value : string }

  let empty = { key = ""; value = "" }

  let to_string tag = Printf.sprintf "{ key = %s; value = %s }" tag.key tag.value
end

module Message = struct
  type t = {
    raw_content : string;
    tags : Tag.t array;
    prefix : string option;
    command : string option;
    params : string array;
  }

  let empty =
    {
      raw_content = "";
      tags = Array.make 0 Tag.empty;
      prefix = None;
      command = None;
      params = Array.make 0 "";
    }

  let to_string message =
    Printf.sprintf "{ raw_content = %s; tags = [TODO]; prefix = %s; command = %s; params = [%s]}"
      message.raw_content
      (match message.prefix with Some a -> a | None -> "[NONE]")
      (match message.command with Some a -> a | None -> "[NONE]")
      (message.params |> Array.fold_left (fun acc s -> acc ^ "," ^ s) "")
end

