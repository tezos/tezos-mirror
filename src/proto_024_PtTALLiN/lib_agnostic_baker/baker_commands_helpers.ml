(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Configuration
open Protocol.Alpha_context

(** [parse_operations] parses uri's to either [Remote] or [Local] operations sources. *)
let parse_operations =
  let open Baking_configuration.Operations_source in
  Option.map (fun uri ->
      match Uri.scheme uri with
      | Some "http" | Some "https" -> Remote {uri; http_headers}
      | None | Some _ ->
          (* acts as if it were file even though it might no be *)
          Local {filename = Uri.to_string uri})

(** [parse_state_recorder] parses a boolean flag to either [Filesystem] or [Memory]
    baking configuration. *)
let parse_state_recorder state_recorder =
  if state_recorder then Baking_configuration.Filesystem else Memory

let run_baker (cctxt : Tezos_client_base.Client_context.full) ?dal_node_rpc_ctxt
    ?minimal_fees ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte ?votes
    ?extra_operations ?pre_emptive_forge_time ?force_apply_from_round
    ?remote_calls_timeout ?data_dir ?state_recorder ~chain ~keep_alive sources =
  let open Lwt_result_syntax in
  let cctxt = new Protocol_client_context.wrap_full cctxt in
  let votes =
    let to_protocol = function
      | Octez_agnostic_baker.Per_block_votes.Per_block_vote_on ->
          Protocol.Alpha_context.Per_block_votes.Per_block_vote_on
      | Octez_agnostic_baker.Per_block_votes.Per_block_vote_off ->
          Protocol.Alpha_context.Per_block_votes.Per_block_vote_off
      | Octez_agnostic_baker.Per_block_votes.Per_block_vote_pass ->
          Protocol.Alpha_context.Per_block_votes.Per_block_vote_pass
    in
    Option.map
      (fun {vote_file; liquidity_baking_vote} ->
        Baking_configuration.
          {vote_file; liquidity_baking_vote = to_protocol liquidity_baking_vote})
      votes
  in
  let*? sources =
    List.map_e Signature.Of_V_latest.get_public_key_hash sources
  in
  let* delegates = Baking_commands.get_delegates cctxt sources in
  Client_daemon.Baker.run
    cctxt
    ?dal_node_rpc_ctxt
    ?minimal_fees:(Option.map Tez.of_mutez_exn minimal_fees)
    ?minimal_nanotez_per_gas_unit
    ?minimal_nanotez_per_byte
    ?votes
    ?extra_operations:(parse_operations extra_operations)
    ?pre_emptive_forge_time
    ?remote_calls_timeout
    ?force_apply_from_round
    ?data_dir
    ?state_recorder:(Option.map parse_state_recorder state_recorder)
    ~chain
    ~keep_alive
    delegates

let run_vdf_daemon ~cctxt ~keep_alive =
  let cctxt = new Protocol_client_context.wrap_full cctxt in
  Client_daemon.VDF.run cctxt ~chain:cctxt#chain ~keep_alive
