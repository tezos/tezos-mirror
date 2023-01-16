(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Lwt.Syntax

type progress_display_mode = Auto | Always | Never

let progress_display_mode_enum =
  [("auto", Auto); ("always", Always); ("never", Never)]

let progress_display_mode_encoding =
  Data_encoding.string_enum progress_display_mode_enum

let animation =
  [|
    "|.....|";
    "|o....|";
    "|oo...|";
    "|ooo..|";
    "|.ooo.|";
    "|..ooo|";
    "|...oo|";
    "|....o|";
    "|.....|";
    "|.....|";
    "|.....|";
    "|.....|";
  |]

let init = String.make (String.length animation.(0)) ' '

let clean = String.make (String.length animation.(0)) '\b'

let animation = Array.map (fun x -> clean ^ x) animation

let number_of_frames = Array.length animation

let make_with_animation ppf ~make ~on_retry seed =
  Format.fprintf ppf "%s%!" init ;
  let rec loop n seed =
    let start = Mtime_clock.counter () in
    Format.fprintf ppf "%s%!" animation.(n mod number_of_frames) ;
    let* r = make seed in
    match r with
    | Ok v -> Lwt.return v
    | Error r ->
        let time = Mtime_clock.count start in
        let* v = on_retry time r in
        loop (n + 1) v
  in
  let* result = loop 0 seed in
  Format.fprintf ppf "%s%s\n%!" clean init ;
  Lwt.return result

type 'a progress_fun =
  | With_progress of {
      step : (unit -> unit Lwt.t) -> 'a Lwt.t;
      pp_print_step : Format.formatter -> int -> unit;
    }
  | Unknown_progress of {f : unit -> 'a Lwt.t; msg : string}

let generic_display_progress ?(every = 1) ?(out = Lwt_unix.stdout)
    ~progress_display_mode progress_fun =
  if every <= 0 then
    raise
      (Invalid_argument "display_progress: negative or null repetition period") ;
  (* pp_print_step must only write on a single-line with no carriage return *)
  let* print_progress =
    match progress_display_mode with
    | Auto -> Lwt_unix.isatty out
    | Always -> Lwt.return_true
    | Never -> Lwt.return_false
  in
  if not print_progress then
    match progress_fun with
    | With_progress {step; _} -> step (fun () -> Lwt.return_unit)
    | Unknown_progress {f; _} -> f ()
  else
    let clear_line fmt = Format.fprintf fmt "\027[2K\r" in
    let stream, notifier = Lwt_stream.create () in
    let wrapped_notifier () =
      notifier (Some ()) ;
      Lwt.pause ()
    in
    let main_promise =
      Lwt.finalize
        (fun () ->
          match progress_fun with
          | With_progress {step; _} -> step wrapped_notifier
          | Unknown_progress {f; _} -> f ())
        (fun () ->
          notifier None ;
          Lwt.return_unit)
    in
    let oc = Unix.out_channel_of_descr (Lwt_unix.unix_file_descr out) in
    let fmt = Format.formatter_of_out_channel oc in
    let cpt = ref 0 in
    let pp_cpt = ref 0 in
    let rec tick_promise () =
      let* () = Lwt_unix.sleep 1. in
      let* () =
        match progress_fun with
        | Unknown_progress _ -> wrapped_notifier ()
        | _ -> Lwt.return_unit
      in
      incr pp_cpt ;
      tick_promise ()
    in
    let loop = tick_promise () in
    let dot_array = [|""; "."; ".."; "..."|] in
    let dots () = dot_array.(!pp_cpt mod 4) in
    let pp () =
      clear_line fmt ;
      match progress_fun with
      | With_progress {pp_print_step; _} ->
          Format.fprintf fmt "%a%s%!" pp_print_step !cpt (dots ())
      | Unknown_progress {msg; _} -> Format.fprintf fmt "%s%s%!" msg (dots ())
    in
    let pp_done () =
      clear_line fmt ;
      match progress_fun with
      | With_progress {pp_print_step; _} ->
          Format.fprintf fmt "%a Done@\n%!" pp_print_step !cpt
      | Unknown_progress {msg; _} -> Format.fprintf fmt "%s Done@\n%!" msg
    in
    pp () ;
    incr cpt ;
    let printer =
      Lwt_stream.iter_s
        (fun () ->
          if !cpt mod every = 0 then pp () ;
          incr cpt ;
          Lwt.return_unit)
        stream
    in
    let* e = main_promise in
    Lwt.cancel loop ;
    let* () = printer in
    decr cpt ;
    pp_done () ;
    Lwt.return e

let display_progress ?every ?out ~progress_display_mode ~pp_print_step step =
  generic_display_progress
    ?every
    ?out
    ~progress_display_mode
    (With_progress {step; pp_print_step})

let three_dots ?out ~progress_display_mode ~msg f =
  generic_display_progress
    ?out
    ~progress_display_mode
    (Unknown_progress {f; msg})
