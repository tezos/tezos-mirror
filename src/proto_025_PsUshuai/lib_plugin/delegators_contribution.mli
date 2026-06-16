(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Protocol-dependent helpers for the delegators_contribution RPC,
    which is defined in [src/lib_shell_services/chain_services.ml] and
    implemented in [src/lib_shell/chain_directory.ml].

    See [src/lib_validation/protocol_plugin.mli] for the descriptions of
    the types and functions below.
*)

type delegated_breakdown_at_sampling = {
  min_delegated_amount : int64;
  min_delegated_level : int32;
  overstaked : int64;
  total_delegated_including_overdelegated : int64;
  total_delegated_after_limits : int64;
  overdelegated : int64;
}

val delegated_breakdown_at_sampling :
  Block_header.shell_header * Environment.Context.t ->
  cycle:int32 ->
  delegate_pkh:Environment.Signature.public_key_hash ->
  [ `Ok of delegated_breakdown_at_sampling
  | `Retry_at_level of int32
  | `Cycle_too_far_in_future ]
  Environment.Error_monad.shell_tzresult
  Lwt.t

type min_delegated_breakdown = {
  total_delegated : int64;
  own_delegated : int64;
  delegators_contributions : (string * int64) list;
  former_delegators_unstake_requests : int64;
}

val min_delegated_breakdown :
  Block_header.shell_header * Environment.Context.t ->
  delegate_pkh:Environment.Signature.public_key_hash ->
  min_delegated_breakdown Environment.Error_monad.shell_tzresult Lwt.t
