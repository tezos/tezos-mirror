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

(**
   Functions to manage slots storage.

   - writing a slot means splitting it in shards and store them on disk
   - reading a slot means rebuild it from the shards
   *)

(** FIXME: https://gitlab.com/tezos/tezos/-/issues/4099
    DAL/Node: make slot_header/commitment definition consistent with
    alpha_context.mli *)

type slot = bytes

(** [split_and_store watcher dal_constants store slot] splits [slot] in shards,
    stores it onto the [store] and returns the corresponding [slot_header],
    using [dal_constants].

    [watcher] is notified when the slot is added to the store. *)
val split_and_store :
  Cryptobox.commitment Lwt_watcher.input ->
  Cryptobox.t ->
  Store.t ->
  slot ->
  Cryptobox.Commitment.t tzresult Lwt.t

(** [get_shard store slot_header shard_id] gets the shard associated to
    [slot_header] at the range [shard_id]. *)
val get_shard :
  Store.t -> Cryptobox.commitment -> int -> Cryptobox.shard tzresult Lwt.t

(** A set of shard indexes *)
module Shard_id_set : Set.S with type elt = int

(** [get_shards store dal_parameters slot_header shard_ids] gets the shards
    associated to [slot_header] at the ranges [shard_ids]. *)
val get_shards :
  Store.t ->
  Cryptobox.parameters ->
  Cryptobox.commitment ->
  Shard_id_set.t ->
  Cryptobox.shard list tzresult Lwt.t

(** [get_slot dal_parameters dal_constants store slot_header] fetches from
    disk the shards associated to [slot_header], gathers them, rebuilds and
    returns the [slot]. *)
val get_slot :
  Cryptobox.parameters ->
  Cryptobox.t ->
  Store.t ->
  Cryptobox.commitment ->
  slot tzresult Lwt.t

(** [get_slot_pages] behaves as [get_slot], except that it also
    splits the slot into pages before returning them.

    Returns an [Error _] if the length of the slot associated to the
    [Cryptobox.commitment] is ill-formed. Specifically, when its
    length is not a multiple of the page-size specified in the
    [Cryptobox.parameters] argument. *)
val get_slot_pages :
  Cryptobox.parameters ->
  Cryptobox.t ->
  Store.t ->
  Cryptobox.commitment ->
  bytes list tzresult Lwt.t

(** [save_shards store slot_header shards] stores [shards] onto the [store]
    associated to the given [slot_header] *)
val save_shards :
  Store.t ->
  Cryptobox.commitment Lwt_watcher.input ->
  Cryptobox.t ->
  Cryptobox.commitment ->
  Cryptobox.share Cryptobox.IntMap.t ->
  unit tzresult Lwt.t

(** [store_slot_headers store block_hash slot_headers] stores [slot_headers]
    onto the [store] associated to the given [block_hash] *)
val store_slot_headers :
  Slot_headers_store.t ->
  Tezos_crypto.Block_hash.t ->
  (int * Cryptobox.commitment) list ->
  unit Lwt.t
