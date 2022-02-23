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

(** {1 A loggable batch of time measurements} *)

(** {b PLEASE, DO NOT DIRECTLY USE THIS MODULE BY YOURSELF
    IN YOUR OWN CODE. IT IS ONLY INTENDED TO BE USED THROUGH
    [Tezos_time_measurement_ppx] PPX REWRITERS FOR TESTING.

    AN UNWISE USE COULD SEVERELY IMPACT THE MEMORY USAGE OF
    YOUR PROGRAM.} *)

(** This module offers several functionalities:
    - Measuring time of Ocaml expression execution.
    - Mesuring the current time just before executing an Ocaml
      expression.
    - Labelling these mesurements and storing them.
    - Logging all stored measurements as a batch. *)

module type S = sig
  (** [duration key thunk] mesures the time taken to run the
      given [thunk], binds the resulting time measurement
      with the given [key] and stores it in the batch. It will
      then evaluate in the result of the [thunk]. *)
  val duration : Measurement.key -> (unit -> 'a) -> 'a

  (** Same as [duration], but waits for the [thunk]'s resulting
      promise to be resolved in order to properly measure
      time when using Lwt. *)
  val duration_lwt : Measurement.key -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  (** [timestamp_pre key thunk] mesures the current time, binds
      the resulting time measurement with the given [key]
      and stores it in the batch. It will then execute the
      given [thunk] and evaluates in its result. *)
  val timestamp_pre : Measurement.key -> (unit -> 'a) -> 'a

  (** [flush ()] cleans the batch and emits an event log that
      will display all measured times collected within the batch. *)
  val flush : unit -> unit Lwt.t
end

(** A time measurement tool that uses the given clock [C] to measure
    the time, stores resulting measurements inside the given state [S]
    and publishes stored measurements using the publisher [P]. *)
module Make
    (S : State.S with type elt := Measurement.t)
    (P : Publisher.S)
    (C : Clock.S) : S
