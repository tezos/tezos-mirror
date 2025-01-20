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

(** [shards_store_lru_size] is the maximum shards store LRU size. See
    {!Key_value_store.init} and {!Store.Shards.init}.
*)
val shards_store_lru_size : int

val slots_store_lru_size : int

val status_store_lru_size : int

(** [committee_cache_size] is the size of the DAL committee cache. *)
val committee_cache_size : int

(** [cache_size] is the size (in number of slots) of the cache of
    not-yet-published slots, shards, and shard proofs. *)
val cache_size : int

(** [slot_id_cache_size] is the size (in number of levels) of the cache to
    associate commitments with slot ids at a given level. *)
val slot_id_cache_size : int

(** The frequency at which we sample the time spent in shards crypto
    verification. *)
val shards_verification_sampling_frequency : int

(** When receiving shards from the network, the DAL node does not
    perform the amplification as soon as it has enough shards to do
    the reconstruction but waits a bit in case the missing shards are
    received from the network. The duration of the delay is picked at
    random between [amplification_random_delay_min] and
    [amplification_random_delay_max]. *)

val amplification_random_delay_min : float

val amplification_random_delay_max : float

(* During amplification, if the forked process takes more time than
   this timeout to send the proved shards, then amplification attempt
   is aborted to avoid keeping a pending promise forever. *)
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

(* Sleep delay between refreshing the ips associated to bootstrap dns names *)
val bootstrap_dns_refresh_delay : float

(** This size is being used for the store's traps cache. While
    [proto_parameters.Dal_plugin.attestation_lag] should define the
    minimum number of levels for which traps must be retained, we
    maintain a larger cache capacity of 50 levels. This extended size
    is acceptable since the cache is sparsely populated due to
    [proto_parameters.traps_fraction]. *)
val traps_cache_size : int
