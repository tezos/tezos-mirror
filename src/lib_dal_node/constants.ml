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

let number_of_slots = 32

let number_of_shards = 512

let attestation_lag = 8

let traps_fraction = Q.(1 // 2000)

(* Each entry in the cache maintains two open file descriptors (one via
   regular file opening and one via mmap on the bitset region).
   So the selected value should be bigger than twice the number of slots per level.
   Note that setting a too high value causes a "Too many open files" error. *)
let shards_store_lru_size = 2 * number_of_slots

(* There is no real rationale for the slot and status parts of the
   store; we just put low-enough values to avoid consuming too many
   file descriptors. *)
let slots_store_lru_size = 64

let status_store_lru_size = 64

(* Fewer cache sizes should be enough in practice,
   but we cache 50 since each cache entry has quite a small memory footprint. *)
let committee_cache_size = 50

(* The size of the shard cache is chosen large enough to enable
   publishing slots on 5 slot indices.  We need to keep shards in
   the cache for at least three levels (if the commitment is published
   immediately, one level to include the publication + 2 levels to
   finalize it) but a few more levels may be needed if the commitment
   is not published immediately so we consider a cache large enough to
   keep the shards for 5 levels. *)
let not_yet_published_cache_size =
  let number_of_levels_to_keep = 5 in
  let number_of_slots = 5 in
  number_of_levels_to_keep * number_of_slots

(* This cache is being used for the validation of message ids, in particular
   messages in the future, it does not have to be big. We take the number of
   slots multiplied by the attestation lag, which sounds reasonable. *)
let slot_id_cache_size = number_of_slots * attestation_lag

(* This cache is used for transient slot header status info.
   Permanent info is stored on disk.
   The size needs to be at least [number_of_slots * (attestation_lag + 1) * 2].
   That's because the slot status of a slot published at [L] is first added
   to the cache at level [L + 1], and then updated at level
   [L + attestation_lag + tb_finality]. Also, when a block is finalized,
   the slots will be updated with a attested/unattested status, and you don't
   want it to erase later levels from statuses cache. Using twice the cache
   size solves this problem. *)
let statuses_cache_size = number_of_slots * (attestation_lag + 1) * 2

let shards_verification_sampling_frequency = 100

let amplification_timeout = 120.

(* Initial reconnection delay to L1 node from the DAL crawler in seconds. See
   {!layer_1.start} in lib_crawler for more details. *)
let initial_l1_crawler_reconnection_delay = 5.

(* Controls the size of the blocks cache in the L1 crawler. It is used in
   {!Crawler.start}. *)
let crawler_l1_blocks_cache_size = 64

(* Number of times that block processing is retried in the L1 crawler in case a disconnection
   error is encountered while retrieving data from L1 outside the
   {!Layer1.iter_heads} callback. *)
let crawler_retries_on_disconnection = 5

(* Sleep delay before retrying processing a block in the L1 crawler in case a
   disconnection error is encountered while retrieving data from L1 outside the
   {!Layer1.iter_heads} callback. *)
let crawler_re_processing_delay = 5.

(* Sleep delay between refreshing the ips associated to bootstrap dns names *)
let bootstrap_dns_refresh_delay = 300.

(* The size of the node store's traps cache. We set it to 2 times the maximum
   expected size when all slots are used. *)
let traps_cache_size =
  let open Q in
  mul (of_int 2)
  @@ mul (of_int number_of_slots)
  @@ mul (of_int number_of_shards)
  @@ mul (of_int attestation_lag)
  @@ traps_fraction
  |> to_int

(* The expected time, in seconds, sufficient to subscribe and connect to new
   peers on a (new) topic. This was not measured and the value is meant to be a
   gross over-approximation. *)
let time_to_join_new_topics = 5
