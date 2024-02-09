(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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
  constants : Constants.Parametric.t;
}

type block = t

val rpc_ctxt : t Environment.RPC_context.simple

(** Policies to select the next baker:
    - [By_round r] selects the baker at round [r]
    - [By_account pkh] selects the first slot for baker [pkh]
    - [Excluding pkhs] selects the first baker that doesn't belong to [pkhs]

    Note that bakers can have active consensus keys different from
    their regular delegate keys. For the [By_account pkh] policy, [pkh]
    refers to the baker's delegate key. However, for the [Excluding pkhs]
    policy, [pkhs] refer to the baker's active consensus key. *)
type baker_policy =
  | By_round of int
  | By_account of public_key_hash
  | Excluding of public_key_hash list

(**
   The default baking functions below is to use (blocks) [Application] mode.
   Setting [baking_mode] allows to switch to [Full_construction] mode.
*)
type baking_mode = Application | Baking

type error += No_slots_found_for of Signature.Public_key_hash.t

(** Returns (account, consensus_key, round, timestamp) of the next baker given
    a policy, defaults to By_round 0. *)
val get_next_baker :
  ?policy:baker_policy ->
  t ->
  (public_key_hash * public_key_hash * int * Time.Protocol.t) tzresult Lwt.t

val get_round : block -> Round.t Environment.Error_monad.tzresult

module Forge : sig
  val contents :
    ?proof_of_work_threshold:Int64.t ->
    ?seed_nonce_hash:Nonce_hash.t ->
    ?liquidity_baking_toggle_vote:Per_block_votes.per_block_vote ->
    ?adaptive_issuance_vote:Per_block_votes.per_block_vote ->
    payload_hash:Block_payload_hash.t ->
    payload_round:Round.t ->
    Block_header.shell_header ->
    Block_header.contents tzresult Lwt.t

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
    ?liquidity_baking_toggle_vote:Per_block_votes.per_block_vote ->
    ?adaptive_issuance_vote:Per_block_votes.per_block_vote ->
    t ->
    header tzresult Lwt.t

  (** Sets uniquely seed_nonce_hash of a header *)
  val set_seed_nonce_hash :
    ?proof_of_work_threshold:int64 ->
    Nonce_hash.t option ->
    header ->
    (header, tztrace) result Lwt.t

  (** Sets the baker that will sign the header to an arbitrary pkh *)
  val set_baker :
    public_key_hash ->
    ?consensus_key:Signature.public_key_hash ->
    header ->
    header

  (** Signs the header with the key of the baker configured in the header.
      The header can no longer be modified, only applied. *)
  val sign_header : header -> Block_header.block_header tzresult Lwt.t
end

val check_constants_consistency : Constants.Parametric.t -> unit tzresult Lwt.t

(** [genesis <opts> accounts] : generates an initial block with the
    given constants [<opts>] and initializes [accounts] with their
    associated amounts.
*)
val genesis :
  ?commitments:Commitment.t list ->
  ?consensus_committee_size:int ->
  ?consensus_threshold:int ->
  ?min_proposal_quorum:int32 ->
  ?bootstrap_contracts:Parameters.bootstrap_contract list ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?issuance_weights:Constants.Parametric.issuance_weights ->
  ?origination_size:int ->
  ?blocks_per_cycle:int32 ->
  ?cycles_per_voting_period:int32 ->
  ?sc_rollup_arith_pvm_enable:bool ->
  ?sc_rollup_private_enable:bool ->
  ?sc_rollup_riscv_pvm_enable:bool ->
  ?dal_enable:bool ->
  ?zk_rollup_enable:bool ->
  ?hard_gas_limit_per_block:Gas.Arith.integral ->
  ?nonce_revelation_threshold:int32 ->
  ?dal:Constants.Parametric.dal ->
  ?adaptive_issuance:Constants.Parametric.adaptive_issuance ->
  Parameters.bootstrap_account list ->
  block tzresult Lwt.t

val genesis_with_parameters : Parameters.t -> block tzresult Lwt.t

(** [alpha_context <opts> accounts] : instantiates an alpha_context with the
    given constants [<opts>] and initializes [accounts] with their
    associated amounts.
*)
val alpha_context :
  ?commitments:Commitment.t list ->
  ?min_proposal_quorum:int32 ->
  Parameters.bootstrap_account list ->
  Alpha_context.t tzresult Lwt.t

(**
   [get_application_vstate pred operations] constructs a protocol validation
   environment for operations in application mode on top of the given block
   with the given operations. It's a shortcut for [begin_application]
*)
val get_application_vstate :
  t ->
  Protocol.operation list ->
  (validation_state * application_state) tzresult Lwt.t

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
  (validation_state * application_state) tzresult Lwt.t

(** applies a signed header and its operations to a block and
    obtains a new block *)
val apply :
  Block_header.block_header ->
  ?operations:Operation.packed list ->
  ?allow_manager_failures:bool ->
  t ->
  t tzresult Lwt.t

(** [bake b] returns a block [b'] which has as predecessor block [b].
    Optional parameter [policy] allows to pick the next baker in
    several ways. If [check_size] is [true] (the default case), then
    the function checks that the operations passed as arguments satisfy
    the size limit of Tezos operations, as defined in the protocol.
    This function bundles together [forge_header], [sign_header] and
    [apply].  These functions should be used instead of bake to craft
    unusual blocks for testing together with setters for properties of
    the headers.  Setting [allow_manager_failures] (default=false),
    allows baking blocks with manager operation(s) that are valid but
    that could fail during their application. If this is not set, the
    block is correctly baked but the operations' application will fail
    silently.  For examples see seed.ml or double_baking.ml
*)
val bake :
  ?baking_mode:baking_mode ->
  ?allow_manager_failures:bool ->
  ?payload_round:Round.t option ->
  ?locked_round:Alpha_context.Round.t option ->
  ?policy:baker_policy ->
  ?timestamp:Timestamp.time ->
  ?operation:Operation.packed ->
  ?operations:Operation.packed list ->
  ?liquidity_baking_toggle_vote:Per_block_votes.per_block_vote ->
  ?adaptive_issuance_vote:Per_block_votes.per_block_vote ->
  ?check_size:bool ->
  t ->
  t tzresult Lwt.t

(** Variant of [bake] that returns the block metadata of the baked block. **)
val bake_with_metadata :
  ?locked_round:Alpha_context.Round.t option ->
  ?policy:baker_policy ->
  ?timestamp:Timestamp.time ->
  ?operation:Operation.packed ->
  ?operations:Operation.packed list ->
  ?payload_round:Round.t option ->
  ?check_size:bool ->
  ?baking_mode:baking_mode ->
  ?allow_manager_failures:bool ->
  ?liquidity_baking_toggle_vote:Per_block_votes.per_block_vote ->
  ?adaptive_issuance_vote:Per_block_votes.per_block_vote ->
  t ->
  (t * (block_header_metadata * operation_receipt list)) tzresult Lwt.t

(** Bakes [n] blocks. *)
val bake_n :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  ?liquidity_baking_toggle_vote:Per_block_votes.per_block_vote ->
  ?adaptive_issuance_vote:Per_block_votes.per_block_vote ->
  int ->
  t ->
  block tzresult Lwt.t

(** Bakes until the given level is reached. *)
val bake_until_level :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  ?liquidity_baking_toggle_vote:Per_block_votes.per_block_vote ->
  ?adaptive_issuance_vote:Per_block_votes.per_block_vote ->
  Raw_level.t ->
  t ->
  block tzresult Lwt.t

(** Version of bake_n that returns a list of all balance updates included
    in the metadata of baked blocks. **)
val bake_n_with_all_balance_updates :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  ?liquidity_baking_toggle_vote:Per_block_votes.per_block_vote ->
  ?adaptive_issuance_vote:Per_block_votes.per_block_vote ->
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

(** Version of bake_n that returns the liquidity baking toggle EMA after [n] blocks. **)
val bake_n_with_liquidity_baking_toggle_ema :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  ?liquidity_baking_toggle_vote:Per_block_votes.per_block_vote ->
  ?adaptive_issuance_vote:Per_block_votes.per_block_vote ->
  int ->
  t ->
  (block * Alpha_context.Per_block_votes.Liquidity_baking_toggle_EMA.t) tzresult
  Lwt.t

(** Variant of [bake_n] that returns the block metadata of the last
    baked block. [n] must be positive, otherwise a single block is baked. **)
val bake_n_with_metadata :
  ?locked_round:Round.t option ->
  ?policy:baker_policy ->
  ?timestamp:Timestamp.time ->
  ?payload_round:Round.t option ->
  ?check_size:bool ->
  ?baking_mode:baking_mode ->
  ?allow_manager_failures:bool ->
  ?liquidity_baking_toggle_vote:Per_block_votes_repr.per_block_vote ->
  ?adaptive_issuance_vote:Per_block_votes_repr.per_block_vote ->
  int ->
  block ->
  (block * (block_header_metadata * operation_receipt list)) tzresult Lwt.t

val get_balance_updates_from_metadata :
  block_header_metadata * operation_receipt list ->
  Alpha_context.Receipt.balance_updates

(** Bake blocks while a predicate over the block holds. The returned
    block is the last one for which the predicate holds; in case the
    predicate never holds, the input block is returned. When the
    optional [invariant] argument is provided, it is checked on the
    input block and on each baked block, including the returned one
    (the last one satisfy the predicate); it is however not checked
    on the next block (the first one to invalidate the predicate). *)
val bake_while :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  ?liquidity_baking_toggle_vote:Per_block_votes_repr.per_block_vote ->
  ?adaptive_issuance_vote:Per_block_votes_repr.per_block_vote ->
  ?invariant:(block -> unit tzresult Lwt.t) ->
  (block -> bool) ->
  block ->
  block tzresult Lwt.t

(* Same as [bake_while] but the predicate also has access to the
   metadata resulting from the application of the block.

   optionnal metadata of the last block stisfying the condition and metadata of
   the last block (which don't) are returned together with the block.
*)
val bake_while_with_metadata :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  ?liquidity_baking_toggle_vote:Per_block_votes_repr.per_block_vote ->
  ?adaptive_issuance_vote:Per_block_votes_repr.per_block_vote ->
  ?invariant:(block -> unit tzresult Lwt.t) ->
  (block -> block_header_metadata -> bool) ->
  block ->
  (block * (block_header_metadata option * block_header_metadata)) tzresult
  Lwt.t

val current_cycle_of_level :
  blocks_per_cycle:int32 -> current_level:int32 -> Cycle.t

val current_cycle : block -> Cycle.t

val last_block_of_cycle : block -> bool

(** Given a block [b] at level [l] bakes enough blocks to complete a cycle,
    that is [blocks_per_cycle - (l % blocks_per_cycle)]. *)
val bake_until_cycle_end :
  ?baking_mode:baking_mode -> ?policy:baker_policy -> t -> t tzresult Lwt.t

(** Given a block [b] at level [l] bakes enough blocks to complete a cycle,
    that is [blocks_per_cycle - (l % blocks_per_cycle)]. *)
val bake_until_cycle_end_with_metadata :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  block ->
  (block * block_header_metadata option * block_header_metadata) tzresult Lwt.t

(** Bakes enough blocks to end [n] cycles. *)
val bake_until_n_cycle_end :
  ?policy:baker_policy -> int -> t -> t tzresult Lwt.t

(** Bakes enough blocks to reach the cycle. *)
val bake_until_cycle :
  ?baking_mode:baking_mode ->
  ?policy:baker_policy ->
  Cycle.t ->
  t ->
  t tzresult Lwt.t

(** Common util function to create parameters for [initial_context] function *)
val prepare_initial_context_params :
  ?consensus_committee_size:int ->
  ?consensus_threshold:int ->
  ?min_proposal_quorum:int32 ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?issuance_weights:Constants.Parametric.issuance_weights ->
  ?origination_size:int ->
  ?blocks_per_cycle:int32 ->
  ?cycles_per_voting_period:int32 ->
  ?sc_rollup_arith_pvm_enable:bool ->
  ?sc_rollup_private_enable:bool ->
  ?sc_rollup_riscv_pvm_enable:bool ->
  ?dal_enable:bool ->
  ?zk_rollup_enable:bool ->
  ?hard_gas_limit_per_block:Gas.Arith.integral ->
  ?nonce_revelation_threshold:int32 ->
  ?dal:Constants.Parametric.dal ->
  ?adaptive_issuance:Constants.Parametric.adaptive_issuance ->
  unit ->
  ( Constants.Parametric.t * Block_header.shell_header * Block_hash.t,
    tztrace )
  result
  Lwt.t

(** [autostaked_opt delegate metadata] returns [Some amount] if [amount] tez
    have been staked for the given [delegate]. [None] otherwise. *)
val autostaked_opt : public_key_hash -> block_header_metadata -> Tez.t option

(**  same as [autostaked_opt] but fails in case autostaking didn't provoke a
     stake operation.  *)
val autostaked :
  ?loc:string -> public_key_hash -> block_header_metadata -> Tez.t
