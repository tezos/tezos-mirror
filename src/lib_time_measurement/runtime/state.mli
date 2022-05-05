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

(** {b PLEASE, DO NOT DIRECTLY USE THIS MODULE BY YOURSELF
    IN YOUR OWN CODE. IT IS ONLY INTENDED TO BE USED THROUGH
    [Tezos_time_measurement_ppx] PPX REWRITERS FOR TESTING.}

    {b AN UNWISE USE COULD SEVERELY IMPACT THE MEMORY USAGE OF
    YOUR PROGRAM.} *)

(** Defines the type of an element inside the state. *)
module type Elt = sig
  type t
end

(** Defines an interface to interact with a mutable state
    that could aggregate values. *)
module type S = sig
  (** The type of elements in the state. *)
  type elt

  (** [get_all_and_reset ()] evaluates in all the elements that
      were aggregated inside the state and then resets the state
      to its initial value.
      Elements are returned in the order they were pushed in the
      state (the resulting list starts by older elements). *)
  val get_all_and_reset : unit -> elt list

  (** [push elt] aggregates the given element [elt] in the state. *)
  val push : elt -> unit
end

(** [State.S] default implementation relying on Ocaml [ref].
    The state is initialized with the given [Elt.init] value. *)
module WithRef (E : Elt) : S with type elt := E.t
