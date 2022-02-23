(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Li-yao Xia                                             *)
(* Copyright (c) 2021 Trili Tech <contact@trili.tech>                        *)
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

(** Pseudo-random value generator.

    Similar to [Stdlib.Random], but with a functional interface. *)

(** Random generator state, immutable. *)
type t

(** {2 Initialization} *)

(** Initialize from an integer. *)
val of_seed : int64 -> t

(** Split the state. *)
val split : t -> t * t

(** Generate 64 random bits, and return a new state. *)
val next_int64 : t -> int64 * t

(** {2 Generate values} *)

(** A state should be used only once to ensure the outputs are independent. *)

(** Generate an [int] uniformly in a range [\[0, max)].

    This does not advance the state of the generator.
    *)
val int : t -> int -> int

(** Generate a [bool].

    This does not advance the state of the generator.
    *)
val bool : t -> bool
