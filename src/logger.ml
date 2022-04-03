let log_source = Logs.Src.create "caml_bot" ~doc:"Logs Twitch.tv IRC client"

let stamp_tag : Mtime.span Logs.Tag.def =
  Logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.Span.pp

let stamp c = Logs.Tag.(empty |> add stamp_tag (Mtime_clock.count c))

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let formatter h tags k ppf fmt =
      let color =
        match level with
        | Logs.App -> "\x1b[0;39m"
        | Logs.Info -> "\x1b[0;34m"
        | Logs.Debug -> "\x1b[0;36m"
        | Logs.Warning -> "\x1b[0;33m"
        | Logs.Error -> "\x1b[0;31m"
      in
      let stamp =
        match tags with
        | None -> None
        | Some tags -> Logs.Tag.find stamp_tag tags
      in
      let dt =
        match stamp with
        | None -> 0.
        | Some s -> Mtime.Span.to_us s
      in
      Format.kfprintf k ppf
        ("%s%a[%0+04.0fus] @[" ^^ fmt ^^ "@]@.\x1b[0m")
        color Logs.pp_header (level, h) dt
    in
    msgf @@ fun ?header ?tags fmt -> formatter header tags k ppf fmt
  in
  { Logs.report }

let () = Logs.set_reporter (reporter Format.std_formatter)
let () = Logs.Src.set_level log_source (Some Logs.Info)

module Log = (val Logs.src_log log_source : Logs.LOG)
