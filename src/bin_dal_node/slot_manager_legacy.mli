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

(** [get_shard store commitment shard_id] gets the shard associated to
    [commitment] at the range [shard_id]. *)
val get_shard :
  Store.Shards.t ->
  Cryptobox.commitment ->
  int ->
  Cryptobox.shard tzresult Lwt.t

(** [get_shards cryptobox store commitment shard_ids] gets the shards
    associated to [commitment] at the ranges [shard_ids]. *)
val get_shards :
  Cryptobox.t ->
  Store.Shards.t ->
  Cryptobox.commitment ->
  int list ->
  Cryptobox.shard list tzresult Lwt.t

(** [get_slot_pages] behaves as [get_slot], except that it also
    splits the slot into pages before returning them.

    Returns an [Error _] if the length of the slot associated to the
    [Cryptobox.commitment] is ill-formed. Specifically, when its
    length is not a multiple of the page-size specified in the
    [Cryptobox.parameters] argument. *)
val get_slot_pages :
  Cryptobox.t ->
  Store.Shards.t ->
  Cryptobox.commitment ->
  bytes list tzresult Lwt.t

(** [save_shards store commitment shards] stores [shards] onto the [store]
    associated to the given [commitment] *)
val save_shards :
  Store.node_store ->
  Cryptobox.t ->
  Cryptobox.commitment ->
  Cryptobox.shard Seq.t ->
  unit tzresult Lwt.t
