type input = { text : string; pos : int }

let input_sub (start : int) (len : int) (s : input) : input =
  { text = String.sub s.text start len; pos = s.pos + start }

let make_input (s : string) : input = { text = s; pos = 0 }

type error = { desc : string; pos : int }

type 'a parser = { run : input -> (input * 'a, error) result }

let fail (e : error) = { run = (fun _ -> Error e) }

let wrap (x : 'a) = { run = (fun input -> Ok (input, x)) }

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  {
    run =
      (fun input ->
        match p.run input with Ok (input', x) -> Ok (input', f x) | Error error -> Error error);
  }

let bind (f : 'a -> 'b parser) (p : 'a parser) : 'b parser =
  {
    run =
      (fun input ->
        match p.run input with Ok (input', x) -> (f x).run input' | Error error -> Error error);
  }

let parse_while (p : char -> bool) : string parser =
  {
    run =
      (fun input ->
        let n = String.length input.text in
        let i = ref 0 in
        while !i < n && String.get input.text !i |> p do
          incr i
        done;
        Ok (input_sub !i (n - !i) input, String.sub input.text 0 !i));
  }

let parse_n (m : int) (p : char -> bool) : string parser =
  {
    run =
      (fun input ->
        let n = String.length input.text in
        let i = ref 0 in
        while !i < m && !i < n && String.get input.text !i |> p do
          incr i
        done;
        Ok (input_sub !i (n - !i) input, String.sub input.text 0 !i));
  }

let prefix (prefix_str : string) : string parser =
  {
    run =
      (fun input ->
        let unexpected_prefix_error =
          { pos = input.pos; desc = Printf.sprintf "expected `%s`" prefix_str }
        in
        try
          let prefix_size = String.length prefix_str in
          let input_size = String.length input.text in
          let prefix_input = input |> input_sub 0 prefix_size in
          if String.equal prefix_input.text prefix_str then
            let rest = input |> input_sub prefix_size (input_size - prefix_size) in
            Ok (rest, prefix_str)
          else Error unexpected_prefix_error
        with Invalid_argument _ -> Error unexpected_prefix_error);
  }

let ( *> ) (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  {
    run =
      (fun input -> input |> p1.run |> Result.map (fun (input', _) -> p2.run input') |> Result.join);
  }

let ( <* ) (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  {
    run =
      (fun input ->
        input
        |> p1.run
        |> Result.map (fun (input', x) ->
               input' |> p2.run |> Result.map (fun (input, _) -> (input, x)))
        |> Result.join);
  }

let ( <*> ) (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
  {
    run =
      (fun input ->
        input
        |> p1.run
        |> Result.map (fun (input', x) ->
               input' |> p2.run |> Result.map (fun (input, y) -> (input, (x, y))))
        |> Result.join);
  }

let ( <|> ) (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  {
    run =
      (fun input ->
        match p1.run input with
        | Ok (input', x) -> Ok (input', x)
        | Error left_error ->
            input
            |> p2.run
            |> Result.map_error (fun right_error ->
                   {
                     pos = left_error.pos;
                     desc = Printf.sprintf "%s or %s" left_error.desc right_error.desc;
                   }));
  }

let optional (p : 'a parser) : 'a option parser =
  {
    run =
      (fun input ->
        match p.run input with Ok (input', x) -> Ok (input', Some x) | Error _ -> Ok (input, None));
  }

let many (p : 'a parser) : 'a list parser =
  {
    run =
      (fun input ->
        let result = ref [] in
        let rec loop input =
          match p.run input with
          | Ok (input', x) ->
              result := x :: !result;
              loop input'
          | Error _ -> input
        in
        let input' = loop input in
        Ok (input', !result |> List.rev));
  }

let any_char : char parser =
  {
    run =
      (fun input ->
        let n = String.length input.text in
        try Ok (input_sub 1 (n - 1) input, String.get input.text 0)
        with Invalid_argument _ -> Error { pos = input.pos; desc = "expected any character" });
  }

let make_error (desc : string) : error = { desc; pos = 0 }

let predicate_char (p : char -> bool) : char parser =
  any_char
  |> bind (fun x ->
         if p x then wrap x else x |> Printf.sprintf "Unexpected char `%c`" |> make_error |> fail)
