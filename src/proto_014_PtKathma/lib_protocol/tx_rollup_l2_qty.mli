(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module is an abstraction on top of int64 to build positive (or zero)
    quantities within the int64 bounds. It comes with a compact encoding to be
    used in the transaction rollup batches. *)

(** Type of postive quantities. Quantities are bounded by {!Int64.max_int}. *)
type t

(** The zero quantity. *)
val zero : t

(** One quantity. *)
val one : t

(** Build a quantity from an int64. Returns [None] if the argument is negative. *)
val of_int64 : int64 -> t option

(** Build a quantity from an int64 and raise [Invalid_argument] on negative quantities. *)
val of_int64_exn : int64 -> t

(** Convert a quantity to [int64]. *)
val to_int64 : t -> int64

(** Convert a quantity to [z]. *)
val to_z : t -> Z.t

(** Returns a string representation of a quantity. *)
val to_string : t -> string

(** Parse a quantity from a string. Returns [None] if the string is not a valid
   quantity representation. *)
val of_string : string -> t option

(** Pretty-printer for quantities. *)
val pp : Format.formatter -> t -> unit

(** Compact encoding for quantities *)
val compact_encoding : t Data_encoding.Compact.t

(** Encoding for quantities *)
val encoding : t Data_encoding.t

(** Substract two quantities. Returns [None] on subtraction underflow. *)
val sub : t -> t -> t option

(** Add two quantities. Returns [None] on addition overflow. *)
val add : t -> t -> t option

(** Return the [t] successor. Returns [None] on overflow. *)
val succ : t -> t option

(** Quantities substraction. *)
val ( - ) : t -> t -> t option

(** Quantities addition. *)
val ( + ) : t -> t -> t option

include Compare.S with type t := t
