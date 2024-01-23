(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

include Progress

type 'a line = {when_tty : 'a Line.t; when_no_tty : string}

let progress_bar ~message ~counter ?color total =
  let open Line in
  let pcount = match counter with `Bytes -> bytes | `Int -> count_to total in
  {
    when_no_tty = message;
    when_tty =
      list
        [
          const message;
          pcount;
          elapsed ();
          bar ~style:`UTF8 ?color total;
          percentage_of total;
        ];
  }

let spinner ~message =
  let open Line in
  {
    when_no_tty = message;
    when_tty = list [const (message ^ " "); spinner (); const " "; elapsed ()];
  }

let with_reporter_tty {when_tty; _} = with_reporter ?config:None when_tty

let with_reporter_no_tty {when_no_tty; _} f =
  Format.eprintf "%s ...%!" when_no_tty ;
  let res = f ignore in
  Format.eprintf " Done.@." ;
  res

let with_reporter line f =
  (* Progress bars are displayed on stderr by default. *)
  if Unix.isatty Unix.stderr then with_reporter_tty line f
  else with_reporter_no_tty line f

(* Progress bar reporter compatible with Lwt. See
   https://github.com/craigfe/progress/issues/25#issuecomment-1030596594 *)
module Lwt = struct
  let flush () =
    let open Lwt_syntax in
    let+ () = Lwt_io.eprintf "%s\n%!" @@ Format.flush_str_formatter () in
    Terminal.Ansi.move_up Format.str_formatter 1

  let with_reporter_tty {when_tty = line; _} f =
    let open Lwt_syntax in
    let config =
      Config.v
        ~ppf:Format.str_formatter
        ~hide_cursor:false
        ~persistent:true
        ~max_width:None
        ~min_interval:None
        ()
    in
    let display = Display.start ~config (Multi.line line) in
    Lwt.finalize
      (fun () ->
        let [report] = Display.reporters display in
        let* () = flush () in
        let report n =
          report n ;
          flush ()
        in
        f report)
      (fun () ->
        Display.finalise display ;
        Lwt.return_unit)

  let with_reporter_no_tty {when_no_tty; _} f =
    let open Lwt_syntax in
    let* () = Lwt_io.eprintf "%s ...%!" when_no_tty in
    let* res = f (fun _ -> Lwt.return_unit) in
    let* () = Lwt_io.eprintf " Done.\n%!" in
    return res

  let with_reporter line f =
    let open Lwt_syntax in
    let* tty = Lwt_unix.isatty Lwt_unix.stderr in
    if tty then with_reporter_tty line f else with_reporter_no_tty line f
end
