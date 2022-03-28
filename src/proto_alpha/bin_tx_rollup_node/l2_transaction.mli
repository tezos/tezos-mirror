(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol

(**  {2 Types for L2 transactions} *)

type t = {
  transaction :
    (Indexable.unknown, Indexable.unknown) Tx_rollup_l2_batch.V1.transaction;
  signature : Tx_rollup_l2_batch.V1.signature;
}

(** Hash with b58check encoding txL2(54), for hashes of L2 transactions *)
module Hash : S.HASH

(** Alias for transaction hash *)
type hash = Hash.t

(**  {2 Serialization} *)

val encoding : t Data_encoding.encoding

val hash : t -> Hash.t

(**  {2 Batching} *)

(** Build a L2 batch of transactions by aggregating the BLS signatures of
    individual transactions *)
val batch :
  t list -> (Indexable.unknown, Indexable.unknown) Tx_rollup_l2_batch.t tzresult
