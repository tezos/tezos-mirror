(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

val pidfile_arg :
  (string option, Tezos_client_base.Client_context.full) Tezos_clic.arg

val keep_alive_arg :
  (bool, Tezos_client_base.Client_context.full) Tezos_clic.arg

val http_headers_env_variable : string

val http_headers : (string * string) list option

val baker_args :
  ( string option
    * bool
    * string option
    * int64
    * Q.t
    * Q.t
    * int option
    * bool
    * Per_block_votes.per_block_vote option
    * Per_block_votes.per_block_vote option
    * string option
    * Uri.t option
    * Uri.t option
    * bool
    * bool
    * Q.t option
    * Q.t option
    * bool,
    Tezos_client_base.Client_context.full )
  Tezos_clic.options

val directory_parameter :
  (string, Tezos_client_base.Client_context.full) Tezos_clic.parameter

val sources_param :
  ( Tezos_crypto.Signature.Public_key_hash.t list ->
    Tezos_client_base.Client_context.full ->
    unit Error_monad.tzresult Lwt.t,
    Tezos_client_base.Client_context.full )
  Tezos_clic.params

val preserved_levels_arg :
  (int, Tezos_client_base.Client_context.full) Tezos_clic.arg

type t = {
  pidfile : string option;
  node_version_check_bypass : bool;
  node_version_allowed : string option;
  minimal_fees : int64;
  minimal_nanotez_per_gas_unit : Q.t;
  minimal_nanotez_per_byte : Q.t;
  force_apply_from_round : int option;
  keep_alive : bool;
  liquidity_baking_vote : Per_block_votes.per_block_vote option;
  adaptive_issuance_vote : Per_block_votes.per_block_vote option;
  per_block_vote_file : string option;
  extra_operations : Uri.t option;
  dal_node_endpoint : Uri.t option;
  without_dal : bool;
  state_recorder : bool;
  pre_emptive_forge_time : Q.t option;
  remote_calls_timeout : Q.t option;
  allow_signing_delay : bool;
}

val create_config :
  string option
  * bool
  * string option
  * int64
  * Q.t
  * Q.t
  * int option
  * bool
  * Per_block_votes.per_block_vote option
  * Per_block_votes.per_block_vote option
  * string option
  * Uri.t option
  * Uri.t option
  * bool
  * bool
  * Q.t option
  * Q.t option
  * bool ->
  t

type per_block_votes_config = {
  vote_file : string option;
  liquidity_baking_vote : Per_block_votes.per_block_vote;
}
