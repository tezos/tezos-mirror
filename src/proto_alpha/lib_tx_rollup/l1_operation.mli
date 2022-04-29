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

open Protocol.Alpha_context

(** Hash with b58check encoding mop(53), for hashes of L1 manager operations *)
module Hash : S.HASH

(** Alias for L1 operations hashes *)
type hash = Hash.t

(** The type of L1 operations that are injected on Tezos by the rollup node *)
type t = private {
  hash : hash;  (** The hash of the L1 manager operation (without the source) *)
  source : public_key_hash;
      (** The source of the operation, i.e., the key that will sign the
          operation for injection. Note: the source is decided when the
          operation is queued in the injector at the moment. *)
  manager_operation : packed_manager_operation;  (** The manager operation *)
}

(** [make ~source op] returns an L1 operation with the corresponding hash and
    whose source is set to [source]. *)
val make : source:public_key_hash -> 'a manager_operation -> t

(** Hash a manager operation *)
val hash_manager_operation : packed_manager_operation -> hash

(** Encoding for L1 operations *)
val encoding : t Data_encoding.t

(** Pretty printer for L1 operations. Only the relevant part for the rollup node
    is printed. *)
val pp : Format.formatter -> t -> unit
