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

open Lwt.Infix

let animation =
  [| "|.....|";
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
     "|.....|" |]

let init = String.make (String.length animation.(0)) ' '

let clean = String.make (String.length animation.(0)) '\b'

let animation = Array.map (fun x -> clean ^ x) animation

let number_of_frames = Array.length animation

let make_with_animation ppf ~make ~on_retry seed =
  Format.fprintf ppf "%s%!" init ;
  let rec loop n seed =
    let start = Mtime_clock.counter () in
    Format.fprintf ppf "%s%!" animation.(n mod number_of_frames) ;
    make seed
    >>= function
    | Ok v ->
        Lwt.return v
    | Error r ->
        let time = Mtime_clock.count start in
        on_retry time r >>= fun v -> loop (n + 1) v
  in
  loop 0 seed
  >|= fun result ->
  Format.fprintf ppf "%s%s\n%!" clean init ;
  result

let display_progress ?(every = 1) ?(out = Lwt_unix.stdout) ~pp_print_step f =
  if every <= 0 then
    raise
      (Invalid_argument "display_progress: negative or null repetition period") ;
  (* pp_print_step must only write on a single-line with no carriage return *)
  Lwt_unix.isatty out
  >>= fun is_a_tty ->
  if not is_a_tty then f (fun () -> Lwt.return_unit)
  else
    let clear_line fmt = Format.fprintf fmt "\027[2K\r" in
    let (stream, notifier) = Lwt_stream.create () in
    let wrapped_notifier () = notifier (Some ()) ; Lwt_unix.yield () in
    let main_promise =
      Lwt.finalize
        (fun () -> f wrapped_notifier)
        (fun () -> notifier None ; Lwt.return_unit)
    in
    let oc = Unix.out_channel_of_descr (Lwt_unix.unix_file_descr out) in
    let fmt = Format.formatter_of_out_channel oc in
    let cpt = ref 0 in
    let pp_cpt = ref 0 in
    let rec tick_promise () =
      Lwt_unix.sleep 1. >>= fun () -> incr pp_cpt ; tick_promise ()
    in
    let loop = tick_promise () in
    let dot_array = [|""; "."; ".."; "..."|] in
    let dots () = dot_array.(!pp_cpt mod 4) in
    let pp () =
      clear_line fmt ;
      Format.fprintf fmt "%a%s%!" pp_print_step !cpt (dots ())
    in
    let pp_done () =
      clear_line fmt ;
      Format.fprintf fmt "%a Done@\n%!" pp_print_step !cpt
    in
    pp () ;
    incr cpt ;
    let printer =
      Lwt_stream.iter_s
        (fun () ->
          ( if !cpt mod every = 0 then (pp () ; Lwt.return_unit)
          else Lwt.return_unit )
          >>= fun () -> incr cpt ; Lwt.return_unit)
        stream
    in
    main_promise
    >>= fun e ->
    Lwt.cancel loop ;
    printer >>= fun () -> decr cpt ; pp_done () ; Lwt.return e
