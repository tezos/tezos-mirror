(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module Arith : Fixed_point_repr.Full

type t = Unaccounted | Limited of {remaining : Arith.fp}

val encoding : t Data_encoding.encoding

val pp : Format.formatter -> t -> unit

type cost = Saturation_repr.may_saturate Saturation_repr.t

val cost_encoding : cost Data_encoding.encoding

val pp_cost : Format.formatter -> cost -> unit

val raw_consume : Arith.fp -> cost -> Arith.fp option

val free : cost

val atomic_step_cost : _ Saturation_repr.t -> cost

val step_cost : _ Saturation_repr.t -> cost

val alloc_cost : _ Saturation_repr.t -> cost

val alloc_bytes_cost : int -> cost

val alloc_mbytes_cost : int -> cost

val read_bytes_cost : int -> cost

val write_bytes_cost : int -> cost

val ( *@ ) : _ Saturation_repr.t -> cost -> cost

val ( +@ ) : cost -> cost -> cost
