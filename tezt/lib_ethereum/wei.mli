(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Helpers for dealing with units of wei.
    `wei` is the smallest denomination of ether.
    1 eth = 1_000_000_000_000_000_000 wei (10^18)

    Please note that none of the functions here perform any bounds checks. *)

(** A unit of wei *)
type t

(** 0 wei *)
val zero : t

(** 1 wei *)
val one : t

(** Convert [t] to a string. *)
val to_string : t -> string

(** Convert a string to a [t]. *)
val of_string : string -> t

(** Converts a [Z.t] to a wei value. *)
val to_wei_z : Z.t -> t

(** Converts a wei value to a [Z.t]. *)
val of_wei_z : t -> Z.t

(** Convert an [int] amount of eth to a wei one.
    This doesn't perform any bounds checks. *)
val of_eth_int : int -> t

(** Addition. This doesn't perform any bounds checks. *)
val ( + ) : t -> t -> t

(** Subtraction. This doesn't perform any bound checks. *)
val ( - ) : t -> t -> t

(** Multiplication. This doesn't perform any bound checks. *)
val ( * ) : t -> t -> t

(** Division. This doesn't perform any bound checks. *)
val ( / ) : t -> t -> t

(** The wei {!Check.typ}. *)
val typ : t Check.typ
