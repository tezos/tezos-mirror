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

(*

  /!\ WARNING:

  If the daemon's logic regarding block processing changes, make sure that
  the L1 crawler's [status] field in [node_context] is updated accordingly.

*)

(** [new_finalized_head ctxt cctxt l1_crawler cryptobox block_hash ~launch_time
    ?amplificator shell_header] processes a new finalized L1 block. It performs cleanup of old
    DAL data, updates the committee cache, re-registers the gossipsub message
    validation hook, and triggers block-level processing (e.g. slot header
    storage, shard publishing to Gossipsub, attestation analysis). This function
    is expected to be called for every finalized head. *)
val new_finalized_head :
  Node_context.t ->
  Rpc_context.t ->
  Crawler.t ->
  Block_hash.t ->
  launch_time:float ->
  ?amplificator:Amplificator.t ->
  Block_header.shell_header ->
  (unit, tztrace) result Lwt.t

(** [fetch_and_store_skip_list_cells ctxt cctxt proto_parameters ~attested_level plugin]
    extracts and stores the skip list cells from [block_info] at [block_level],
    using the encoding from the corresponding [plugin]. It is used to support
    DAL refutation. *)
val fetch_and_store_skip_list_cells :
  Node_context.t ->
  Rpc_context.t ->
  Tezos_dal_node_services.Types.proto_parameters ->
  attested_level:int32 ->
  (module Dal_plugin.T) ->
  (unit, tztrace) result Lwt.t

(** [remove_old_level_stored_data proto_parameters ctxt current_level]
    removes all stored DAL data (slots, shards, statuses, skip list cells)
    associated with the level determined by the GC logic for [current_level]. *)
val remove_old_level_stored_data :
  Tezos_dal_node_services.Types.proto_parameters ->
  Node_context.t ->
  int32 ->
  (unit, tztrace) result Lwt.t
