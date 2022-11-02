(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

(** The type of the [destination] argument of the
    {!Operation_repr.Transaction} manager operation.

    The introduction of this type allows to interact with emerging
    layer-2 solutions using the API Tezos users and tooling
    are already used to: contract calls to entrypoint. These solutions
    cannot be integrated to {!Contract_repr.t} directly, because
    values of this type are given a balance, which has an impact on
    the delegation system. *)

(** This type is a superset of the set of contracts ({!Contract_repr.t}).

    {b Note:} It is of key importance that the encoding of this type
    remains compatible with {!Contract_repr.encoding}, for the
    introduction to this type to remain transparent from the existing
    tooling perspective.  *)
type t =
  | Contract of Contract_repr.t
  | Tx_rollup of Tx_rollup_repr.t
  | Sc_rollup of Sc_rollup_repr.t
  | Zk_rollup of Zk_rollup_repr.t

include Compare.S with type t := t

val to_b58check : t -> string

val of_b58check : string -> t tzresult

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

(** [in_memory_size contract] returns the number of bytes that are
    allocated in the RAM for [contract]. *)
val in_memory_size : t -> Cache_memory_helpers.sint

type error += Invalid_destination_b58check of string
