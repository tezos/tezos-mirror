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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4458
          Better handling of this limitation *)
let shards_store_lru_size =
  (* The size of the LRU is determined by a set of parameters, including the
     number of levels for which we want to maintain the shards in the cache. *)
  let number_of_shards_per_slot = 2048 in
  let number_of_slots = 256 in
  let number_of_remembered_levels = 4 in
  number_of_shards_per_slot * number_of_slots * number_of_remembered_levels

(* Fewer cache sizes should be enough in practice,
   but we cache 50 since each cache entry has quite a small memory footprint. *)
let committee_cache_size = 50

(* The size of the cache of 1024 entries (one per slot) is chosen such
   that: if a DAL node stores the shard proofs of 128 slots per level,
   the cache will be able to store the proofs for 8 levels, which
   should be quite sufficient with the current attestation lag.

   A shard proof takes 52 bytes with the current encoding (could be improved
   to 48), so the maximum memory footprint of the cache is dominated by (keys
   size is negligible):

   1024 (cache size) * 2048 (shards per slot) * 52 bytes = 109 mb *)
let shards_proofs_cache_size = 1024
