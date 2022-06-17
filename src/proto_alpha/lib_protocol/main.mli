(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Protocol Implementation - Protocol Signature Instance

    This module is the  entrypoint to the protocol for shells and other
    embedders.  This signature is an instance of
    {{!Tezos_protocol_environment_sigs.V5.T.Updater.PROTOCOL} the
    [Updater.PROTOCOL] signature} from the
    {{:https://tezos.gitlab.io/shell/the_big_picture.html#the-economic-protocol-environment-and-compiler}
    Protocol Environment}.

    Each Protocol depends on a version of the Protocol Environment. For the
    currently developed protocol, this is normally the latest version.  You can
    see {{!Tezos_protocol_environment_sigs} the full list of versions here}.

    For details on how Protocol and Environment interact, see
    {{:https://tezos.gitlab.io/shell/the_big_picture.html} this overview}.
 *)

(** [validation_mode] permits to differenciate [!type:validation_state]
    values.

    There are four validation modes:
    - [Application]
    - [Partial_application]
    - [Partial_construction]
    - [Full_construction]

    For the meaning and typical uses of each mode, refer to the
    comments attached to the corresponding type constructors below.
*)
type validation_mode =
  | Application of {
      block_header : Alpha_context.Block_header.t;
      fitness : Alpha_context.Fitness.t;
      payload_producer : Alpha_context.public_key_hash;
      block_producer : Alpha_context.public_key_hash;
      predecessor_round : Alpha_context.Round.t;
      predecessor_level : Alpha_context.Level.t;
    }
      (** Full Validation of a block. See
          {!val:Tezos_protocol_environment_sigs.V5.T.Updater.PROTOCOL.begin_application}**)
  | Partial_application of {
      block_header : Alpha_context.Block_header.t;
      fitness : Alpha_context.Fitness.t;
      payload_producer : Alpha_context.public_key_hash;
      block_producer : Alpha_context.public_key_hash;
      predecessor_level : Alpha_context.Level.t;
      predecessor_round : Alpha_context.Round.t;
    }
      (** [Partial_application] is used in pre-checking of blocks - not all checks
         are done. Special case of [Application] to allow quick rejection of bad
         blocks. See
         {!val:Tezos_protocol_environment_sigs.V5.T.Updater.PROTOCOL.begin_partial_application}
       *)
  | Partial_construction of {
      predecessor : Block_hash.t;
      predecessor_fitness : Fitness.t;
      predecessor_level : Alpha_context.Level.t;
      predecessor_round : Alpha_context.Round.t;
    }
      (** Shell/mempool-only construction of a virtual block. See
          {!val:Tezos_protocol_environment_sigs.V5.T.Updater.PROTOCOL.begin_construction} *)
  | Full_construction of {
      predecessor : Block_hash.t;
      payload_producer : Alpha_context.public_key_hash;
      block_producer : Alpha_context.public_key_hash;
      protocol_data_contents : Alpha_context.Block_header.contents;
      level : Int32.t;
      round : Alpha_context.Round.t;
      predecessor_level : Alpha_context.Level.t;
      predecessor_round : Alpha_context.Round.t;
    }
      (** Baker-only block construction for baking in. See
          {!val:Tezos_protocol_environment_sigs.V5.T.Updater.PROTOCOL.begin_construction}
       *)

type validation_state = {
  mode : validation_mode;
  chain_id : Chain_id.t;
  ctxt : Alpha_context.t;
  op_count : int;
  migration_balance_updates : Alpha_context.Receipt.balance_updates;
  liquidity_baking_toggle_ema : Alpha_context.Liquidity_baking.Toggle_EMA.t;
  implicit_operations_results :
    Apply_results.packed_successful_manager_operation_result list;
  validate_operation_info : Validate_operation.validate_operation_info;
  validate_operation_state : Validate_operation.validate_operation_state;
}

type operation_data = Alpha_context.packed_protocol_data

type operation = Alpha_context.packed_operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

(** See {!Validate_operation.precheck_manager}. *)
val precheck_manager :
  validation_state ->
  'a Alpha_context.Kind.manager Alpha_context.contents_list ->
  'a Alpha_context.Kind.manager
  Validate_operation.TMP_for_plugin.should_check_signature ->
  Validate_operation.stamp tzresult Lwt.t

include
  Updater.PROTOCOL
    with type block_header_data = Alpha_context.Block_header.protocol_data
     and type block_header_metadata = Apply_results.block_metadata
     and type block_header = Alpha_context.Block_header.t
     and type operation_data := operation_data
     and type operation_receipt = Apply_results.packed_operation_metadata
     and type operation := operation
     and type validation_state := validation_state
