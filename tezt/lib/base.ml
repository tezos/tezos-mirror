(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

let ( // ) = Filename.concat

let sf = Printf.sprintf

let ( let* ) = Lwt.bind

let ( and* ) = Lwt.both

let ( and*! ) a b =
  let (main_promise, main_awakener) = Lwt.task () in
  let already_woke_up = ref false in
  Lwt.on_failure a (fun exn ->
      if not !already_woke_up then (
        already_woke_up := true ;
        Lwt.wakeup_exn main_awakener exn) ;
      Lwt.cancel b) ;
  Lwt.on_failure b (fun exn ->
      if not !already_woke_up then (
        already_woke_up := true ;
        Lwt.wakeup_exn main_awakener exn) ;
      Lwt.cancel a) ;
  let both = Lwt.both a b in
  Lwt.on_success both (fun x -> Lwt.wakeup main_awakener x) ;
  Lwt.on_cancel main_promise (fun () -> Lwt.cancel both) ;
  main_promise

let return = Lwt.return

let unit = Lwt.return_unit

let none = Lwt.return_none

let some = Lwt.return_some

let range a b =
  let rec range ?(acc = []) a b =
    if b < a then acc else range ~acc:(b :: acc) a (b - 1)
  in
  range a b

let rec list_find_map f = function
  | [] -> None
  | head :: tail -> (
      match f head with None -> list_find_map f tail | Some _ as x -> x)

type rex = string * Re.re

let rec take n l =
  if n < 0 then invalid_arg "Tezt.Base.take: argument cannot be negative"
  else if n = 0 then []
  else match l with [] -> [] | hd :: rest -> hd :: take (n - 1) rest

let rex ?opts r = (r, Re.compile (Re.Perl.re ?opts r))

let show_rex = fst

let ( =~ ) s (_, r) = Re.execp r s

let ( =~! ) s (_, r) = not (Re.execp r s)

let ( =~* ) s (_, r) =
  match Re.exec_opt r s with
  | None -> None
  | Some group -> Some (Re.Group.get group 1)

let ( =~** ) s (_, r) =
  match Re.exec_opt r s with
  | None -> None
  | Some group -> Some (Re.Group.get group 1, Re.Group.get group 2)

let replace_string ?pos ?len ?all (_, r) ~by s =
  Re.replace_string ?pos ?len ?all r ~by s

let rec repeat n f =
  if n <= 0 then unit
  else
    let* () = f () in
    repeat (n - 1) f

let with_open_out file write_f =
  let chan = open_out file in
  try
    write_f chan ;
    close_out chan
  with x ->
    close_out chan ;
    raise x

let with_open_in file read_f =
  let chan = open_in file in
  try
    let value = read_f chan in
    close_in chan ;
    value
  with x ->
    close_in chan ;
    raise x

let read_file filename =
  let* ic = Lwt_io.open_file ~mode:Lwt_io.Input filename in
  Lwt_io.read ic

module String_map = Map.Make (String)
module String_set = Set.Make (String)
