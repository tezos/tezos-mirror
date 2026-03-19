(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>         *)
(*                                                                           *)
(*****************************************************************************)

include Protocol_plugin_sig.LAYER1_HELPERS

(** [fetch_tezos_block cctxt hash] returns a block info given a block hash.
    Looks for the block in the blocks cache first, and fetches it from the L1
    node otherwise. *)
val fetch_tezos_block :
  Layer1.t ->
  Block_hash.t ->
  Protocol_client_context.Alpha_block_services.block_info tzresult Lwt.t

(** [get_dal_attested_slots_messages fetch_dal_params cctxt block_hash]
    fetches the DAL attested slots messages from L1 for the given [block_hash]
    using the [skip_list_cells_of_level] RPC.

    The messages are constructed using the protocol's
    {!Sc_rollup.Inbox_message.dal_attested_slots_messages_of_cells} function,
    ensuring consistency with the protocol's message construction.

    @param fetch_dal_params callback to fetch DAL parameters for a given
    published level (using protocol environment tzresult)

    Returns a list of messages, one per published level with at least one
    attested slot. *)
val get_dal_attested_slots_messages :
  (published_level:Protocol.Alpha_context.Raw_level.t ->
  (int * int * int) tzresult Lwt.t) ->
  #Client_context.full ->
  Block_hash.t ->
  Protocol.Alpha_context.Sc_rollup.Inbox_message.internal_inbox_message list
  tzresult
  Lwt.t
