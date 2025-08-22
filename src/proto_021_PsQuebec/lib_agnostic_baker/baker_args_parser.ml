(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Configuration
open Protocol.Alpha_context

(** [parse_minimal_fees] parses integer valued fees to [Tez] values. *)
let parse_minimal_fees = Tez.of_mutez_exn

(** [parse_per_block_vote] parses string valued voting options to protocol
    specific [Per_block_votes] values. *)
let parse_per_block_vote =
  Option.map (function
    | "on" -> Per_block_votes.Per_block_vote_on
    | "off" -> Per_block_vote_off
    | "pass" -> Per_block_vote_pass
    (* This is unreachable because any other value would fail in CLI configuration parsing *)
    | _ -> assert false)

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

let parse_configuration
    {
      pidfile;
      node_version_check_bypass;
      node_version_allowed;
      minimal_fees;
      minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte;
      force_apply_from_round;
      keep_alive;
      liquidity_baking_vote;
      adaptive_issuance_vote;
      per_block_vote_file;
      extra_operations;
      dal_node_endpoint;
      without_dal;
      state_recorder;
      pre_emptive_forge_time;
      remote_calls_timeout;
    } =
  ( pidfile,
    node_version_check_bypass,
    node_version_allowed,
    parse_minimal_fees minimal_fees,
    minimal_nanotez_per_gas_unit,
    minimal_nanotez_per_byte,
    force_apply_from_round,
    keep_alive,
    parse_per_block_vote liquidity_baking_vote,
    parse_per_block_vote adaptive_issuance_vote,
    per_block_vote_file,
    parse_operations extra_operations,
    dal_node_endpoint,
    without_dal,
    parse_state_recorder state_recorder,
    pre_emptive_forge_time,
    remote_calls_timeout )

let parse_baking_mode baking_mode =
  match baking_mode with
  | Some local_data_dir_path -> Baking_commands.Local {local_data_dir_path}
  | None -> Remote

let parse_sources =
  List.map_es (fun source ->
      match Tezos_crypto.Signature.V1.Of_V_latest.public_key_hash source with
      | Some source -> Lwt_result_syntax.return source
      | None -> failwith "Cannot convert from V_latest signature to V1.")
