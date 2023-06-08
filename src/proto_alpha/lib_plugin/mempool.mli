(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** Plugin for the shell mempool. It must include the signature
    [FILTER.Mempool] from [lib_shell/shell_plugin.mli]. *)

(** Settings for the {!pre_filter}:
    - minimal fees to accept an operation (absolute, relative to the
      gas limit, and relative to the byte size)
    - clock drift for the prefiltering of consensus operations

    and for the {!conflict_handler}:
    - replacement factor, that is, how much better a new manager
      operation needs to be, in terms of both absolute fees and fee/gas
      ratios, in order to replace an old conflicting manager operation. *)
type config

(** Default parameters. *)
val default_config : config

(** Encoding for {!config}. *)
val config_encoding : config Data_encoding.t

(** Static information needed by {!pre_filter}.

    It depends on the [head] block upon which a mempool is built. *)
type filter_info

(** Create a {!filter_info} based on the [head] block and the current
    context. *)
val init :
  Environment.Context.t ->
  head:Block_header.shell_header ->
  (filter_info, tztrace) result Lwt.t

(** Create a new {!filter_info} based on the [head] block.

    Parts of the old {!filter_info} (which may have been built on
    a different block) are recycled, so that this function is more
    efficient than {!init} and does not need an
    {!Environment.Context.t} argument. *)
val flush :
  filter_info -> head:Block_header.shell_header -> filter_info tzresult Lwt.t

(** Perform some preliminary checks on an operation.

    For manager operations, check that its fee, fee/gas ratio, and
    fee/size ratio all meet the minimal requirements specified in the
    {!config}.

    For consensus operations, check that it is possible for the
    operation to have been produced before now (plus additional time
    equal to the [clock_drift] from {!config}, as a safety margin).
    Indeed, without this check, a baker could flood the network with
    consensus operations for any future rounds or levels. The ml file
    contains more detailled explanations with diagrams. *)
val pre_filter :
  filter_info ->
  config ->
  Protocol.Alpha_context.packed_operation ->
  [ `Passed_prefilter of [`High | `Medium | `Low of Q.t list]
  | `Branch_delayed of tztrace
  | `Branch_refused of tztrace
  | `Refused of tztrace
  | `Outdated of tztrace ]
  Lwt.t

(** Return a conflict handler for {!Protocol.Mempool.add_operation}
    (see {!Protocol.Mempool.conflict_handler}).

    For non-manager operations, select the greater operation according
    to {!Protocol.Alpha_context.Operation.compare}.

    A manager operation is replaced only when the new operation's fee
    and fee/gas ratio both exceed (or match) the old operation's metrics
    multiplied by the [replace_by_fee] factor specified in the {!config}.

    Precondition: both operations must be individually valid (to be
    able to call {!Protocol.Alpha_context.Operation.compare}). *)
val conflict_handler : config -> Protocol.Mempool.conflict_handler

(** The purpose of this module is to provide the
    [fee_needed_to_replace_by_fee] function. For this function to be
    correct, the caller must maintain the state of type [t] by calling
    [update] on each successfully validated operation and its induced
    replacements. *)
module Conflict_map : sig
  (** Internal state needed by [fee_needed_to_replace_by_fee]. *)
  type t

  (** Initial state. *)
  val empty : t

  (** Removes all the [replacements] from the state then adds
      [new_operation]. *)
  val update :
    t ->
    new_operation:Protocol.Alpha_context.packed_operation ->
    replacements:Protocol.Alpha_context.packed_operation list ->
    t

  (** This function should be called when
      [Protocol.Mempool.add_operation] has returned [Unchanged]. This
      means that the [candidate_op] has been rejected because there was
      a conflict with an pre-existing operation and the
      {!val-conflict_handler} has returned [`Keep]. When both
      operations are manager operations, this function returns the
      minimal fee (in mutez) that [candidate_op] would need in order to
      win the conflict, i.e. make the {!val-conflict_handler} return
      [`Replace] instead. Otherwise, it returns [None]. *)
  val fee_needed_to_replace_by_fee :
    config ->
    candidate_op:Protocol.Alpha_context.packed_operation ->
    conflict_map:t ->
    int64 option
end

(** Compute the minimal fee (expressed in mutez) that [candidate_op] would
    need to have in order to be strictly greater than [op_to_overtake]
    according to {!Protocol.Alpha_context.Operation.compare}, when both
    operations are manager operations.

    Return [None] when at least one operation is not a manager operation.

    Also return [None] if both operations are manager operations but
    there was an error while computing the needed fee. However, note
    that this cannot happen when both manager operations have been
    successfully validated by the protocol. *)
val fee_needed_to_overtake :
  op_to_overtake:Protocol.Alpha_context.packed_operation ->
  candidate_op:Protocol.Alpha_context.packed_operation ->
  int64 option

(** The following type, encoding, and default values are exported for
    [bin_sc_rollup_node/configuration.ml]. *)

(** An amount of fees in nanotez. *)
type nanotez = Q.t

(** Encoding for {!nanotez}. *)
val nanotez_enc : nanotez Data_encoding.t

(** Minimal absolute fees in {!default_config}. *)
val default_minimal_fees : Protocol.Alpha_context.Tez.t

(** Minimal fee over gas_limit ratio in {!default_config}. *)
val default_minimal_nanotez_per_gas_unit : nanotez

(** Minimal fee over byte size ratio in {!default_config}. *)
val default_minimal_nanotez_per_byte : nanotez

module Internal_for_tests : sig
  open Protocol.Alpha_context

  (** {!default_config} with a custom value for the [clock_drift] field. *)
  val default_config_with_clock_drift : Period.t option -> config

  (** {!default_config} with a custom [replace_by_fee_factor]. *)
  val default_config_with_replace_factor : nanotez -> config

  (** Return the [clock_drift] field of the given {!config}. *)
  val get_clock_drift : config -> Period.t option

  (** The main auxiliary function for {!pre_filter} regarding
      consensus operations. *)
  val acceptable_op :
    config:config ->
    round_durations:Round.round_durations ->
    round_zero_duration:Period.t ->
    proposal_level:Raw_level.t ->
    proposal_round:Round.t ->
    proposal_timestamp:Timestamp.time ->
    proposal_predecessor_level_start:Timestamp.time ->
    op_level:Raw_level.t ->
    op_round:Round.t ->
    now_timestamp:Timestamp.time ->
    bool Environment.Error_monad.tzresult

  (** The main component of
      {!Conflict_map.fee_needed_to_replace_by_fee}. See comment in the
      ml file. *)
  val fee_needed_to_replace_by_fee :
    config ->
    op_to_replace:Protocol.Alpha_context.packed_operation ->
    candidate_op:Protocol.Alpha_context.packed_operation ->
    int64 option
end
