(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2021-2022 Trili Tech, <contact@trili.tech>                  *)
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
val version_value : string

val version : string

val mainnet_id : Chain_id.t

val shadownet_id : Chain_id.t

val fitness_version_number : string

val proof_of_work_nonce_size : int

val nonce_length : int

val max_anon_ops_per_block : int

val max_proposals_per_delegate : int

val max_operation_data_length : int

(** A global size limit on the size of Micheline expressions
    after expansion.

    We want to prevent constants from being
    used to create huge values that could potentially do damage
    if ever printed or sent over the network. We arrived at this
    number by finding the largest possible contract in terms of
    number of nodes. The number of nodes is constrained by the
    current "max_operation_data_length" (32768) to be ~10,000 (
    see "large_flat_contract.tz" in the tezt suite for the largest
    contract with constants that can be originated). As a first
    approximation, we set the node size limit to 5 times this amount. *)
val max_micheline_node_count : int

(** Same as [max_micheline_node_count] but for limiting the combined
    bytes of the strings, ints and bytes in a expanded Micheline
    expression.  *)
val max_micheline_bytes_limit : int

(** Represents the maximum depth of an expression stored
    in the table after all references to other constants have
    (recursively) been expanded, where depth refers to the
    nesting of [Prim] and/or [Seq] nodes.

    The size was chosen arbitrarily to match the typechecker
    in [Script_ir_translator]. *)
val max_allowed_global_constant_depth : int

(** A global size limit on the size of Michelson types.

    The size of a type is the number of nodes in its AST
    representation. See [Script_typed_ir.TYPE_SIZE].
 *)
val michelson_maximum_type_size : int

(** The number of **full cycles** after a misbehaviour during which it
    can still be denounced.

    More precisely, the window for denouncing a misbehaviour that
    happens during cycle [n] spans from the misbehaviour itself until
    the end of cycle [n + denunciation_period]. So it indeed lasts
    [denunciation_period] full cycles plus part of cycle [n].

    (Technically, denunciations of the misbehaviour are actually
    allowed from the beginning of cycle [n], even on earlier levels
    than the misbehaviour's. But this is only relevant if someone
    crafts double blocks or consensus operations in the future for some
    reason. In practice, we expect misbehaviours to only happen around
    the time when the chain reaches their level, which is why we don't
    count cycle [n] itself as a full cycle for denouncing.)

    Note that we must have [denunciation_period >= 1], otherwise if a
    misbehaviour happens at the very last block of a cycle, there is no
    time frame to denounce it.

    Currently [denunciation_period = 1] which is considered enough
    time for all misbehaviours to be denounced.

    /!\ Several parts of the codebase may assume that
    [denunciation_period = 1] **without being parametrized using this
    constant**. So if it is ever modified, the codebase needs to be
    examined extensively; searching for places that use this constant
    is not enough. *)
val denunciation_period : int

(** The number of **full cycles** between a misbehaviour and its
    slashing (assuming that it has been denounced).

    That is, a misbehaviour that happens during cycle [n] is always
    slashed at the end of cycle [n + slashing_delay], regardless of
    when it has been denounced (and if it has not been denounced by
    then, then it is too late and it will never be slashed).

    Note that we must have [slashing_delay >= denunciation_period],
    otherwise the chain would accept denunciations for which the
    slashing can no longer happen because it is too late.

    Also note that to make the slashing possible, information on
    baking rights for cycle [n] needs to be kept in the context until
    the end of cycle [n + slashing_delay].

    /!\ Several parts of the codebase may assume that
    [slashing_delay = 1] **without being parametrized using this
    constant**. So if it is ever modified, the codebase needs to be
    examined extensively; searching for places that use this constant
    is not enough. *)
val slashing_delay : int

(** A size limit for {!Sc_rollups.wrapped_proof} binary encoding. *)
val sc_max_wrapped_proof_binary_size : int

(** A limit on the size of the binary encoding for sc rollup messages:
    {!Sc_rollup_inbox_message_repr.t} and {!Sc_rollup_outbox_message_repr.t}
*)
val sc_rollup_message_size_limit : int

(** A limit on the number of messages in a inbox level enforced in
    {!Sc_rollup_inbox_repr.t}. *)
val sc_rollup_max_number_of_messages_per_level : Z.t

type fixed

val fixed_encoding : fixed Data_encoding.encoding

type error += (* `Permanent *) Invalid_protocol_constants of string

(** performs some consistency checks on the protocol parameters *)
val check_constants : Constants_parametric_repr.t -> unit tzresult

module Generated : sig
  type t = {
    consensus_threshold_size : int;
    issuance_weights : Constants_parametric_repr.issuance_weights;
  }

  (* This function is meant to be used just in lib_parameters and in the
     migration code to be sure that the parameters are consistent. *)
  val generate : consensus_committee_size:int -> dal_rewards_ratio:Q.t -> t
end

(** Pseudo-constants that are neither fixed as the ones defined above,
    nor protocol parameters from {!Constants_parametric_repr}. Instead,
    they are derived from the protocol parameters. *)
module Derived : sig
  type t = {
    issuance_modification_delay : int;
    consensus_key_activation_delay : int;
    unstake_finalization_delay : int;
  }

  val encoding : t Data_encoding.t

  (* The descriptions of these pseudo-constants are in the .ml, to
     keep them more easily synchronized with the implementation. *)

  val issuance_modification_delay :
    parametric:Constants_parametric_repr.t -> int

  val consensus_key_activation_delay :
    parametric:Constants_parametric_repr.t -> int

  val unstake_finalization_delay : parametric:Constants_parametric_repr.t -> int
end

type t = private {
  fixed : fixed;
  parametric : Constants_parametric_repr.t;
  derived : Derived.t;
}

val all_of_parametric : Constants_parametric_repr.t -> t

val encoding : t Data_encoding.encoding

(** For each subcache, a size limit needs to be declared once. However,
    depending how the protocol will be instantiated (sandboxed mode,
    test network, ...) we may want to change this limit. For each
    subcache, a parametric constant can be used to change the limit
    (see {!parametric}).

    The number of subcaches and the limits for all those subcaches form
    together what is called the [cache_layout]. *)
val cache_layout_size : int

(** The [cache_layout] depends on parametric constants. *)
val cache_layout : Constants_parametric_repr.t -> int list
