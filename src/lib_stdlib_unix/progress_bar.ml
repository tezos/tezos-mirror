(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad
include Progress

type 'a line = {
  when_tty : 'a Line.t;
  when_no_tty : string option;
  update_interval : float option;
}

let progress_bar ~message ~counter ?update_interval ?color
    ?(no_tty_quiet = false) total =
  let open Line in
  let pcount =
    match counter with `Bytes | `Bytes_speed -> bytes | `Int -> count_to total
  in
  let additional =
    match counter with `Bytes_speed -> [brackets bytes_per_sec] | _ -> []
  in
  {
    update_interval;
    when_no_tty = (if no_tty_quiet then None else Some message);
    when_tty =
      list
        ([const message; pcount]
        @ additional
        @ [elapsed (); bar ~style:`UTF8 ?color total; percentage_of total]);
  }

let spinner ?(no_tty_quiet = false) message =
  let open Line in
  {
    update_interval = None;
    when_no_tty = (if no_tty_quiet then None else Some message);
    when_tty = list [const (message ^ " "); spinner (); const " "; elapsed ()];
  }

let with_reporter_tty {when_tty; update_interval; _} =
  let min_interval =
    match update_interval with
    | None -> None
    | Some i -> Some (Some (Duration.of_sec i))
  in
  with_reporter ~config:(Config.v ?min_interval ()) when_tty

let with_reporter_no_tty {when_no_tty; _} f =
  Option.iter (Format.eprintf "%s ...%!.") when_no_tty ;
  let res = f ignore in
  Option.iter (fun _ -> Format.eprintf " Done.@.") when_no_tty ;
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

  let with_reporter_tty {when_tty = line; update_interval; _} f =
    let open Lwt_syntax in
    let min_interval =
      match update_interval with
      | None -> None
      | Some i -> Some (Some (Duration.of_sec i))
    in
    let config =
      Config.v
        ~ppf:Format.str_formatter
        ~hide_cursor:false
        ~persistent:true
        ?min_interval
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
    let* () = Option.iter_s (Lwt_io.eprintf "%s ...%!") when_no_tty in
    let* res = f (fun _ -> Lwt.return_unit) in
    let* () =
      Option.iter_s (fun _ -> Lwt_io.eprintf " Done.\n%!") when_no_tty
    in
    return res

  let with_reporter line f =
    let open Lwt_syntax in
    let* tty = Lwt_unix.isatty Lwt_unix.stderr in
    if tty then with_reporter_tty line f else with_reporter_no_tty line f

  let with_background_spinner ?no_tty_quiet ~message promise =
    let open Lwt_syntax in
    let spinner = spinner ?no_tty_quiet message in
    with_reporter spinner @@ fun count_progress ->
    let rec spin_loop () =
      let* () = Lwt_unix.sleep 0.1 in
      let* () = count_progress 1 in
      spin_loop ()
    in
    Lwt.pick [promise; spin_loop ()]
end
