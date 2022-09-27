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

(** This module defines a standard signature for modules providing fixed-point
    arithmetic. *)

type fp_tag (* Tag for fixed point computations *)

type integral_tag (* Tag for integral computations *)

(** A signature for modules implementing a fixed-point arithmetic.

    Fixed-point types come in two flavours:
    - integral (marked with [integral_tag]), behaving like integers;
    - fp (marked with [fp_tag]), allowing for fractions.

    Such numbers represent standard arithmetic, rounding (converting fp
    flavour to integral one) and comparisons (which can work across flavours). *)
module type Safe = sig
  type 'a t

  type fp = fp_tag t

  type integral = integral_tag t

  val integral_exn : Z.t -> integral

  val integral_of_int_exn : int -> integral

  val integral_to_z : integral -> Z.t

  val zero : 'a t

  val add : 'a t -> 'a t -> 'a t

  val sub : 'a t -> 'a t -> 'a t

  val ceil : fp -> integral

  val floor : fp -> integral

  val fp : 'a t -> fp

  val ( = ) : 'a t -> 'b t -> bool

  val ( <> ) : 'a t -> 'b t -> bool

  val ( < ) : 'a t -> 'b t -> bool

  val ( <= ) : 'a t -> 'b t -> bool

  val ( >= ) : 'a t -> 'b t -> bool

  val ( > ) : 'a t -> 'b t -> bool

  val compare : 'a t -> 'b t -> int

  val equal : 'a t -> 'b t -> bool

  val max : 'a t -> 'a t -> 'a t

  val min : 'a t -> 'a t -> 'a t

  val pp : Format.formatter -> 'a t -> unit

  val pp_integral : Format.formatter -> integral -> unit

  val n_fp_encoding : fp Data_encoding.t

  val n_integral_encoding : integral Data_encoding.t

  val z_fp_encoding : fp Data_encoding.t

  val z_integral_encoding : integral Data_encoding.t
end

module type Full = sig
  type 'a t

  include Safe with type 'a t := 'a t

  val unsafe_fp : Z.t -> fp
end
