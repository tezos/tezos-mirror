(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Transform multiplications by constants in a costlang expression to fixed
    point arithmetic. Allows to make cost functions protocol-compatible. *)

(** Modes of casting of float to int *)
type cast_mode = Ceil | Floor | Round

(** Parameters for conversion to fixed point *)
type options = {
  precision : int;
      (** Number of bits to consider when decomposing the
                          mantissa *)
  max_relative_error : float;
      (** Percentage of admissible relative error when casting floats to ints  *)
  cast_mode : cast_mode;
  inverse_scaling : int;
      (** The constant prettification will consider 1/inverse_scaling digits to
          be not significant. *)
  resolution : int;
      (** Resolution of the grid using when prettifying constants.  *)
}

val default_options : options

val options_encoding : options Data_encoding.t

module Apply (P : sig
  val options : options
end) : Costlang.Transform
