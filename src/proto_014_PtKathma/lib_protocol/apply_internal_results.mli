(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Types representing results of applying an internal operation.

    These are used internally by [Apply].
*)

open Alpha_context

type 'kind internal_manager_operation =
  | Transaction : {
      amount : Tez.tez;
      parameters : Script.lazy_expr;
      entrypoint : Entrypoint.t;
      destination : Destination.t;
    }
      -> Kind.transaction internal_manager_operation
  | Origination : {
      delegate : Signature.Public_key_hash.t option;
      script : Script.t;
      credit : Tez.tez;
    }
      -> Kind.origination internal_manager_operation
  | Delegation :
      Signature.Public_key_hash.t option
      -> Kind.delegation internal_manager_operation
  | Event : {
      ty : Script.expr;
      tag : Entrypoint.t;
      payload : Script.expr;
    }
      -> Kind.event internal_manager_operation

type 'kind internal_contents = {
  source : Contract.t;
  operation : 'kind internal_manager_operation;
  nonce : int;
}

type packed_internal_contents =
  | Internal_contents : 'kind internal_contents -> packed_internal_contents

val contents_of_packed_internal_operation :
  Script_typed_ir.packed_internal_operation -> packed_internal_contents

val contents_of_packed_internal_operations :
  Script_typed_ir.packed_internal_operation list ->
  packed_internal_contents list

(** Result of applying an internal transaction. *)
type successful_transaction_result =
  | Transaction_to_contract_result of {
      storage : Script.expr option;
      lazy_storage_diff : Lazy_storage.diffs option;
      balance_updates : Receipt.balance_updates;
      originated_contracts : Contract_hash.t list;
      consumed_gas : Gas.Arith.fp;
      storage_size : Z.t;
      paid_storage_size_diff : Z.t;
      allocated_destination_contract : bool;
    }
  | Transaction_to_tx_rollup_result of {
      ticket_hash : Ticket_hash.t;
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
    }
  | Transaction_to_sc_rollup_result of {
      consumed_gas : Gas.Arith.fp;
      inbox_after : Sc_rollup.Inbox.t;
    }

(** Result of applying an internal origination. *)
type successful_origination_result = {
  lazy_storage_diff : Lazy_storage.diffs option;
  balance_updates : Receipt.balance_updates;
  originated_contracts : Contract_hash.t list;
  consumed_gas : Gas.Arith.fp;
  storage_size : Z.t;
  paid_storage_size_diff : Z.t;
}

(** Result of applying a {!Script_typed_ir.internal_operation}. *)
type _ successful_internal_manager_operation_result =
  | ITransaction_result :
      successful_transaction_result
      -> Kind.transaction successful_internal_manager_operation_result
  | IOrigination_result :
      successful_origination_result
      -> Kind.origination successful_internal_manager_operation_result
  | IDelegation_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.delegation successful_internal_manager_operation_result
  | IEvent_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.event successful_internal_manager_operation_result

type 'kind internal_manager_operation_result =
  ( 'kind,
    'kind Kind.manager,
    'kind successful_internal_manager_operation_result )
  Apply_operation_result.operation_result

type packed_internal_manager_operation_result =
  | Internal_manager_operation_result :
      'kind internal_contents * 'kind internal_manager_operation_result
      -> packed_internal_manager_operation_result

val contents_of_internal_operation :
  'kind Script_typed_ir.internal_operation -> 'kind internal_contents

val pack_internal_manager_operation_result :
  'kind Script_typed_ir.internal_operation ->
  'kind internal_manager_operation_result ->
  packed_internal_manager_operation_result

val internal_contents_encoding : packed_internal_contents Data_encoding.t

val internal_manager_operation_result_encoding :
  packed_internal_manager_operation_result Data_encoding.t
