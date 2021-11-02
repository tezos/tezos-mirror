(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type S = sig
  val duration : Measurement.key -> (unit -> 'a) -> 'a

  val duration_lwt : Measurement.key -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val timestamp_pre : Measurement.key -> (unit -> 'a) -> 'a

  val flush : unit -> unit Lwt.t
end

(* TODO: https://gitlab.com/tezos/tezos/-/issues/1869
   Unit test this module. *)
module Make
    (S : State.S with type elt := Measurement.t)
    (P : Publisher.S)
    (C : Clock.S) : S = struct
  let span start stop = stop -. start

  let duration (label, metadata) f =
    let start = C.current_time () in
    let res = f () in
    let stop = C.current_time () in
    let duration = span start stop in
    let measurement = Measurement.create label metadata duration in
    S.push measurement ;
    res

  let duration_lwt (label, metadata) f =
    let start = C.current_time () in
    f () >>= fun res ->
    let stop = C.current_time () in
    let duration = span start stop in
    let measurement = Measurement.create label metadata duration in
    S.push measurement ;
    return res

  let timestamp_pre (label, metadata) f =
    let timestamp = C.current_time () in
    let measurement = Measurement.create label metadata timestamp in
    S.push measurement ;
    f ()

  let flush () = S.get_all_and_reset () |> P.publish
end
