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

(** The max slashing period is the maximum number of cycles after which a
    misbehaviour can be denounced, i.e. if a misbehaviour happened at cycle [c],
    it will be rejected if it is denounced at cycle [c + max_slashing_period].
    Having [max_slashing_period] strictly smaller than 2 doesn't make sense.
    Indeed, if a misbehaviour happens at the very last block of a cycle, it
    couldn't be denounced.
    [max_slashing_period = 2] leaves one cycle to denounce a misbehaviour in
    the worst case, which is deemed enough.
    Several parts of the codebase may use the fact that
    [max_slashing_period = 2], so let's ensure it cannot be different. *)
val max_slashing_period : int

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

type t = private {fixed : fixed; parametric : Constants_parametric_repr.t}

val all_of_parametric : Constants_parametric_repr.t -> t

val encoding : t Data_encoding.encoding

type error += (* `Permanent *) Invalid_protocol_constants of string

(** performs some consistency checks on the protocol parameters *)
val check_constants : Constants_parametric_repr.t -> unit tzresult

module Generated : sig
  type t = {
    consensus_threshold : int;
    issuance_weights : Constants_parametric_repr.issuance_weights;
    max_slashing_threshold : int;
  }

  (* This function is meant to be used just in lib_parameters and in the
     migration code to be sure that the parameters are consistent. *)
  val generate : consensus_committee_size:int -> t
end

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
