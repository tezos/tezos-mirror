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

(** Pseudotoken used in staking.
    It represents a share of the total frozen deposits of a baker. *)
include Compare.S

val encoding : t Data_encoding.t

val balance_update_encoding : [`Credited of t | `Debited of t] Data_encoding.t

val zero : t

val of_z_exn : Z.t -> t

val to_int64 : t -> Int64.t

val to_z : t -> Z.t

val init_of_tez : Tez_repr.t -> t

val ( +? ) : t -> t -> t tzresult

val ( -? ) : t -> t -> t tzresult

val pred : t -> t option

(** See {!Tez_repr.mul_ratio}. *)
val mul_ratio :
  rounding:[`Down | `Up] -> t -> num:int64 -> den:int64 -> t tzresult

val pp : Format.formatter -> t -> unit
