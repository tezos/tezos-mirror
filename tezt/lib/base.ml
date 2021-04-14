(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let (let*) = Lwt.bind

let (and*) = Lwt.both

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
  | [] ->
      None
  | head :: tail ->
      match f head with
        | None ->
            list_find_map f tail
        | Some _ as x ->
            x

type rex = Re.re

let rex r = Re.compile (Re.Perl.re r)

let ( =~ ) s r = Re.execp r s

let ( =~! ) s r = not (Re.execp r s)

let ( =~* ) s r =
  match Re.exec_opt r s with
  | None ->
      None
  | Some group ->
      Some (Re.Group.get group 1)

let ( =~** ) s r =
  match Re.exec_opt r s with
  | None ->
      None
  | Some group ->
      Some (Re.Group.get group 1, Re.Group.get group 2)

let replace_string = Re.replace_string

let async_promises = ref []

let async promise = async_promises := promise :: !async_promises

let wait_for_async () =
  let promises = !async_promises in
  async_promises := [] ;
  Lwt.join promises

let rec repeat n f =
  if n <= 0 then unit
  else
    let* () = f () in
    repeat (n - 1) f

let with_open_out file write_f =
  let chan = open_out file in
  try (write_f chan; close_out chan;)
  with x -> close_out chan; raise x

let with_open_in file read_f =
  let chan = open_in file in
  try (let value = read_f chan in close_in chan; value)
  with x -> close_in chan; raise x

let read_file filename =
  let* ic = Lwt_io.open_file ~mode:Lwt_io.Input filename in
  Lwt_io.read ic

module String_map = Map.Make (String)
module String_set = Set.Make (String)
