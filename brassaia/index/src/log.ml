(* The MIT License

   Copyright (c) 2019 Craig Ferguson <craig@tarides.com>
                      Thomas Gazagnaire <thomas@tarides.com>
                      Ioana Cristescu <ioana@tarides.com>
                      Clément Pascutto <clement@tarides.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software. *)

open! Import

let src = Logs.Src.create "index" ~doc:"Index"

module Log = (val Logs.src_log src : Logs.LOG)
include Log

let default_reporter (type c) ?(prefix = "")
    (module Clock : Platform.CLOCK with type counter = c) (counter : c) =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.span_to_us (Clock.count counter) in
      Fmt.kpf k ppf
        ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix dt Logs_fmt.pp_header (level, h)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  { Logs.report }

let setup ?reporter ?style_renderer ?level (module Clock : Platform.CLOCK)
    (module Fmt_tty : Platform.FMT_TTY) =
  let start_time = Clock.counter () in
  let reporter =
    match reporter with
    | Some x -> x
    | None -> default_reporter (module Clock) start_time
  in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter reporter;
  ()

open Cmdliner

let ( let+ ) t f = Term.(const f $ t)
let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)

let setup_term ?reporter clock fmt_tty =
  let+ style_renderer = Fmt_cli.style_renderer ()
  and+ level = Logs_cli.level () in
  setup ?reporter ?style_renderer ?level clock fmt_tty
