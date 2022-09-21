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

(** Internal representation of the gas limit available to the node baking a new
    block. It should be proportional to the time and energy required to perform a
    computation.

    This protects the bakers from performing exceedingly costly computations
    while baking and also allows them to select cheaper-to-compute operations to
    include in their blocks, as their reward for baking a block is not directly
    related to the resources consumed by the machine performing the operation.

    It can be [Unaccounted] (unlimited) or [Limited] to some fixed-point value
    (see [Fixed_point_repr] for the details). The value is represented with 3
    decimal places of precision.

    All computations on gas are performed in saturation arithmetic (see
    [Saturation_repr]) bounded between [0] and [2 ^ 62 - 1]*)

module Arith :
  Fixed_point_repr.Full
    with type 'a t = private Saturation_repr.may_saturate Saturation_repr.t

type t = Unaccounted | Limited of {remaining : Arith.fp}

val encoding : t Data_encoding.encoding

val pp : Format.formatter -> t -> unit

(** Represents a gas cost of an operation. The gas model is constructed such
    that the cost of each operation is roughly proportional to the time required
    to perform the operation. If the gas cost of an operation exceeds the
    available limit, such an operation is rejected. This is especially meant to
    protect bakers against DoS attacks. *)
type cost = Saturation_repr.may_saturate Saturation_repr.t

val cost_encoding : cost Data_encoding.encoding

val pp_cost : Format.formatter -> cost -> unit

(** Print the gas cost as gas unit *)
val pp_cost_as_gas : Format.formatter -> cost -> unit

(** Subtracts the cost from the current limit. Returns [None] if the limit
    would fall below [0]. *)
val raw_consume : Arith.fp -> cost -> Arith.fp option

(** The cost of free operation is [0]. *)
val free : cost

(** Convert a fixed-point amount of gas to a cost. *)
val cost_of_gas : 'a Arith.t -> cost

(** Convert an amount of milligas expressed as a value of type [int] to [Arith.fp].  *)
val fp_of_milligas_int : int -> Arith.fp

(** [atomic_step_cost x] corresponds to [x] milliunit of gas. *)
val atomic_step_cost : _ Saturation_repr.t -> cost

(** [step_cost x] corresponds to [x] units of gas. *)
val step_cost : _ Saturation_repr.t -> cost

(** Cost of allocating qwords of storage.

    [alloc_cost n] estimates the cost of allocating [n] qwords of storage. *)
val alloc_cost : _ Saturation_repr.t -> cost

(** Cost of allocating bytes in the storage.

    [alloc_bytes_cost b] estimates the cost of allocating [b] bytes of
    storage. *)
val alloc_bytes_cost : int -> cost

(** Cost of allocating bytes in the storage.

    [alloc_mbytes_cost b] estimates the cost of allocating [b] bytes of
    storage and the cost of a header to describe these bytes. *)
val alloc_mbytes_cost : int -> cost

(** Cost of reading the storage.

    [read_bytes_const n] estimates the cost of reading [n] bytes of storage. *)
val read_bytes_cost : int -> cost

(** Cost of writing to storage.

    [write_bytes_const n] estimates the cost of writing [n] bytes to the
    storage. *)
val write_bytes_cost : int -> cost

(** Multiply a cost by a factor. Both arguments are saturated arithmetic values,
    so no negative numbers are involved. *)
val ( *@ ) : _ Saturation_repr.t -> cost -> cost

(** Add two costs together. *)
val ( +@ ) : cost -> cost -> cost

(** Ill-formed [gas_limit]: see {!check_gas_limit}. *)
type error += Gas_limit_too_high (* `Permanent *)

(** Check that [gas_limit] is well-formed, i.e. it is at most the
    given [hard_gas_limit_per_operation], and it is nonnegative.

    @return [Error Gas_limit_too_high] otherwise. *)
val check_gas_limit :
  hard_gas_limit_per_operation:Arith.integral ->
  gas_limit:'a Arith.t ->
  unit tzresult
