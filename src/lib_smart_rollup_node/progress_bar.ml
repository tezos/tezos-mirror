(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

include Progress

let progress_bar ~message ~counter ?color total =
  let open Line in
  let pcount = match counter with `Bytes -> bytes | `Int -> count_to total in
  list
    [
      const message;
      pcount;
      elapsed ();
      bar ~style:`UTF8 ?color total;
      percentage_of total;
    ]

let spinner ~message =
  let open Line in
  list [const (message ^ " "); spinner (); const " "; elapsed ()]

(* Progress bar reporter compatible with Lwt. See
   https://github.com/craigfe/progress/issues/25#issuecomment-1030596594 *)
module Lwt = struct
  let flush () =
    let open Lwt_syntax in
    let+ () = Lwt_io.printf "%s\n%!" @@ Format.flush_str_formatter () in
    Terminal.Ansi.move_up Format.str_formatter 1

  let with_reporter line f =
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
    let [report] = Display.reporters display in
    let* () = flush () in
    let report n =
      report n ;
      flush ()
    in
    f report
end
