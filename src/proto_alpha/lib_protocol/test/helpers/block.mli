(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Protocol
open Alpha_context

type t = {
  hash : Block_hash.t;
  header : Block_header.t;
  operations : Operation.packed list;
  context : Tezos_protocol_environment.Context.t;  (** Resulting context *)
}

type block = t

val rpc_ctxt : t Environment.RPC_context.simple

(** Policies to select the next baker:
    - [By_round r] selects the baker at round [r]
    - [By_account pkh] selects the first slot for baker [pkh]
    - [Excluding pkhs] selects the first baker that doesn't belong to [pkhs]
*)
type baker_policy =
  | By_round of int
  | By_account of public_key_hash
  | Excluding of public_key_hash list

(**
   The default baking functions below is to use (blocks) [Application] mode.
   Setting [baking_mode] allows to switch to [Full_construction] mode.
*)
type baking_mode = Application | Baking

(** Returns (account, round, timestamp) of the next baker given
    a policy, defaults to By_round 0. *)
val get_next_baker :
  ?policy:baker_policy ->
  t ->
  (public_key_hash * int * Time.Protocol.t) tzresult Lwt.t

val get_round : block -> Round.t tzresult

module Forge : sig
  val contents :
    ?proof_of_work_nonce:Bytes.t ->
    ?seed_nonce_hash:Nonce_hash.t ->
    ?liquidity_baking_escape_vote:bool ->
    payload_hash:Block_payload_hash.t ->
    payload_round:Round.t ->
    unit ->
    Block_header.contents

  type header

  val classify_operations : packed_operation list -> packed_operation list list

  (** Forges a correct header following the policy.
      The header can then be modified and applied with [apply]. *)
  val forge_header :
    ?locked_round:Alpha_context.Round.t option ->
    ?payload_round:Round.t option ->
    ?policy:baker_policy ->
    ?timestamp:Timestamp.time ->
    ?operations:Operation.packed list ->
    ?liquidity_baking_escape_vote:bool ->
    t ->
    header tzresult Lwt.t

  (** Sets uniquely seed_nonce_hash of a header *)
  val set_seed_nonce_hash : Nonce_hash.t option -> header -> header

  (** Sets the baker that will sign the header to an arbitrary pkh *)
  val set_baker : public_key_hash -> header -> header

  (** Signs the header with the key of the baker configured in the header.
      The header can no longer be modified, only applied. *)
  val sign_header : header -> Block_header.block_header tzresult Lwt.t
end

val check_constants_consistency : Constants.parametric -> unit tzresult Lwt.t

(** [genesis <opts> accounts] : generates an initial block with the
    given constants [<opts>] and initializes [accounts] with their
    associated amounts.
*)
val genesis :
  ?commitments:Commitment.t list ->
  ?consensus_threshold:int ->
  ?min_proposal_quorum:int32 ->
  ?bootstrap_contracts:Parameters.bootstrap_contract list ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?liquidity_baking_subsidy:Tez.t ->
  ?endorsing_reward_per_slot:Tez.t ->
  ?baking_reward_bonus_per_slot:Tez.t ->
  ?baking_reward_fixed_portion:Tez.t ->
  ?origination_size:int ->
  ?blocks_per_cycle:int32 ->
  ?tx_rollup_enable:bool ->
  ?sc_rollup_enable:bool ->
  (Account.t * Tez.tez) list ->
  block tzresult Lwt.t

val genesis_with_parameters : Parameters.t -> block tzresult Lwt.t

(** [alpha_context <opts> accounts] : instantiates an alpha_context with the
    given constants [<opts>] and initializes [accounts] with their
    associated amounts.
*)
val alpha_context :
  ?commitments:Commitment.t list ->
  ?min_proposal_quorum:int32 ->
  (Account.t * Tez.tez) list ->
  Alpha_context.t tzresult Lwt.t

(**
   [get_application_vstate pred operations] constructs a protocol validation
   environment for operations in application mode on top of the given block
   with the given operations. It's a shortcut for [begin_application]
*)
val get_application_vstate :
  t -> Protocol.operation list -> validation_state tzresult Lwt.t

(**
   [get_construction_vstate ?policy ?timestamp ?protocol_data pred]
   constructs a protocol validation environment for operations in
   construction mode on top of the given block. The mode is
   full(baking)/partial(mempool) if [protocol_data] given/absent. It's a
   shortcut for [begin_construction]
 *)
val get_construction_vstate :
  ?policy:baker_policy ->
  ?timestamp:Timestamp.time ->
  ?protocol_data:block_header_data option ->
  block ->
  validation_state tzresult Lwt.t

(** applies a signed header and its operations to a block and
    obtains a new block *)
val apply :
  Block_header.block_header ->
  ?operations:Operation.packed list ->
  t ->
  t tzresult Lwt.t

(**
   [bake b] returns a block [b'] which has as predecessor block [b].
   Optional parameter [policy] allows to pick the next baker in several ways.
   This function bundles together [forge_header], [sign_header] and [apply].
   These functions should be used instead of bake to craft unusual blocks for
   testing together with setters for properties of the headers.
   For examples see seed.ml or double_baking.ml
*)
val bake :
  ?baking_mode:baking_mode ->
  ?payload_round:Round.t option ->
  ?locked_round:Alpha_context.Round.t option ->
  ?policy:baker_policy ->
  ?timestamp:Timestamp.time ->
  ?operation:Operation.packed ->
  ?operations:Operation.packed list ->
  ?liquidity_baking_escape_vote:bool ->
  t ->
  t tzresult Lwt.t

(** Bakes [n] blocks. *)
val bake_n :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  ?liquidity_baking_escape_vote:bool ->
  int ->
  t ->
  block tzresult Lwt.t

(** Version of bake_n that returns a list of all balance updates included
    in the metadata of baked blocks. **)
val bake_n_with_all_balance_updates :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  ?liquidity_baking_escape_vote:bool ->
  int ->
  t ->
  (block * Alpha_context.Receipt.balance_updates) tzresult Lwt.t

(** Version of bake_n that returns a list of all origination results
    in the metadata of baked blocks. **)
val bake_n_with_origination_results :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  int ->
  t ->
  (block
  * Alpha_context.Kind.origination
    Apply_results.successful_manager_operation_result
    list)
  tzresult
  Lwt.t

(** Version of bake_n that returns the liquidity baking escape EMA after [n] blocks. **)
val bake_n_with_liquidity_baking_escape_ema :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  ?liquidity_baking_escape_vote:bool ->
  int ->
  t ->
  (block * Alpha_context.Liquidity_baking.escape_ema) tzresult Lwt.t

val current_cycle : t -> Cycle.t tzresult Lwt.t

(** Given a block [b] at level [l] bakes enough blocks to complete a cycle,
    that is [blocks_per_cycle - (l % blocks_per_cycle)]. *)
val bake_until_cycle_end : ?policy:baker_policy -> t -> t tzresult Lwt.t

(** Bakes enough blocks to end [n] cycles. *)
val bake_until_n_cycle_end :
  ?policy:baker_policy -> int -> t -> t tzresult Lwt.t

(** Bakes enough blocks to reach the cycle. *)
val bake_until_cycle : ?policy:baker_policy -> Cycle.t -> t -> t tzresult Lwt.t

(** Common util function to create parameters for [initial_context] function *)
val prepare_initial_context_params :
  ?consensus_threshold:int ->
  ?min_proposal_quorum:int32 ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?liquidity_baking_subsidy:Tez.t ->
  ?endorsing_reward_per_slot:Tez.t ->
  ?baking_reward_bonus_per_slot:Tez.t ->
  ?baking_reward_fixed_portion:Tez.t ->
  ?origination_size:int ->
  ?blocks_per_cycle:int32 ->
  ?tx_rollup_enable:bool ->
  ?sc_rollup_enable:bool ->
  (Account.t * Tez.t) list ->
  ( Constants.parametric * Block_header.shell_header * Block_hash.t,
    tztrace )
  result
  Lwt.t
