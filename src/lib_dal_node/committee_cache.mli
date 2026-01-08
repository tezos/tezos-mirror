(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** Cache for the DAL committee. *)

type t

(** Represents the tenderbake attestation slot. *)
type tb_attestation_slot = int

(** Represents shard indexes of an attester. *)
type shard_indexes = int list

(** Represents the committee for a given level,
    as a mapping from an attester to its assigned shard indexes. *)
type committee =
  (shard_indexes * tb_attestation_slot) Signature.Public_key_hash.Map.t

(** [create ~max_size] returns an empty cache. If the cache size exceeds [max_size],
    committees of old [level]s are removed in FIFO order. *)
val create : max_size:int -> t

(** [find t ~level] returns the {!committee} at [level].
    When the committee for block level [level] is not stored in the cache it returns [None]. *)
val find : t -> level:int32 -> committee option

(** [add t ~level ~committee] adds the committee [committee] for level [level].
    If the committee for [level] already exists in the cache, it is removed and
    replaced by the given [committee]. *)
val add : t -> level:int32 -> committee:committee -> unit
