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

(** [validation_slack] is the slack (in blocks) for outdated message validation.
    Messages older than [attestation_lag + validation_slack] blocks from the
    head are considered too old and rejected. *)
val validation_slack : int

(** [shards_store_lru_size] is the maximum shards store LRU size. See
    {!Key_value_store.init} and {!Store.Shards.init}. *)
val shards_store_lru_size : number_of_slots:int -> int

val slots_store_lru_size : int

val status_store_lru_size : int

(** [committee_cache_size] is the size of the DAL committee cache. *)
val committee_cache_size : int

(** [attestation_ops_cache_size] is the size of the attestation operations
    cache. *)
val attestation_ops_cache_size : int

(** [not_yet_published_cache_size] is the size (in number of slots) of the cache
    of not-yet-published slots, shards, and shard proofs. *)
val not_yet_published_cache_size : int

(** [slot_id_cache_size] is the size (in number of levels) of the cache to
    associate commitments with slot ids at a given level. *)
val slot_id_cache_size : number_of_slots:int -> attestation_lag:int -> int

(** [statuses_cache_size] is the size (in number of slots) of the cache
    to associate slot ids to slot status. *)
val statuses_cache_size : number_of_slots:int -> attestation_lag:int -> int

(** The frequency at which we sample the time spent in shards crypto
    verification. *)
val shards_verification_sampling_frequency : int

(** During amplification, if the forked process takes more time than this
    timeout to send the proved shards, then amplification attempt is aborted to
    avoid keeping a pending promise forever. *)
val amplification_timeout : float

(** Initial reconnection delay to L1 node from the DAL crawler in seconds. *)
val initial_l1_crawler_reconnection_delay : float

(** Controls the size of the blocks cache in the L1 crawler. *)
val crawler_l1_blocks_cache_size : int

(** Number of blocks processing retries in the L1 crawler in case a disconnection
   error is encountered while retrieving data from L1 outside the
   {!Layer1.iter_heads} callback. *)
val crawler_retries_on_disconnection : int

(** Sleep delay before retrying processing a block in the L1 crawler in case a
    disconnection error is encountered while retrieving data from L1 outside the
    {!Layer1.iter_heads} callback. *)
val crawler_re_processing_delay : float

(** Sleep delay between refreshing the ips associated to bootstrap dns names *)
val bootstrap_dns_refresh_delay : float

(** The size of the node store's traps cache. *)
val traps_cache_size :
  number_of_slots:int ->
  number_of_shards:int ->
  attestation_lag:int ->
  traps_fraction:Q.t ->
  int

(** The expected time, in levels, sufficient to subscribe and connect to new
    peers on a (new) topic. *)
val time_to_join_new_topics_in_levels : minimal_block_delay:int -> int

(* The default retention period of slots and shard for non-operator
   profiles. For observability purposes, we aim for a non-slot operator profile
   to keep shards for about 10 minutes. 150 blocks is 10 minutes on Ghostnet, 20
   minutes on Mainnet. *)
val shard_retention_period_in_levels : int
