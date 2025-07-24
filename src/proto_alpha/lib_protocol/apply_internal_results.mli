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

(** [internal_operation_contents] are the internal operations as output in
    receipts.
    The type simply weakens {!Script_typed_ir.internal_operation_contents} so
    that it is easier to define an encoding for it (i.e. we remove the typed
    parameter). *)
type 'kind internal_operation_contents =
  | Transaction : {
      amount : Tez.t;
      parameters : Script.lazy_expr;
      entrypoint : Entrypoint.t;
      destination : Destination.t;
    }
      -> Kind.transaction internal_operation_contents
  | Origination : {
      delegate : Signature.Public_key_hash.t option;
      script : Script.t;
      credit : Tez.t;
    }
      -> Kind.origination internal_operation_contents
  | Delegation :
      Signature.Public_key_hash.t option
      -> Kind.delegation internal_operation_contents
  | Event : {
      ty : Script.expr;
      tag : Entrypoint.t;
      payload : Script.expr;
    }
      -> Kind.event internal_operation_contents

type 'kind internal_operation = {
  sender : Destination.t;
  operation : 'kind internal_operation_contents;
  nonce : int;
}

type packed_internal_operation =
  | Internal_operation : 'kind internal_operation -> packed_internal_operation

val packed_internal_operation :
  Script_typed_ir.packed_internal_operation -> packed_internal_operation

val packed_internal_operations :
  Script_typed_ir.packed_internal_operation list ->
  packed_internal_operation list

(** Result of applying an internal transaction. *)
type successful_transaction_result =
  | Transaction_to_contract_result of {
      storage : Script.expr option;
      lazy_storage_diff : Lazy_storage.diffs option;
      balance_updates : Receipt.balance_updates;
      ticket_receipt : Ticket_receipt.t;
      originated_contracts : Contract_hash.t list;
      consumed_gas : Gas.Arith.fp;
      storage_size : Z.t;
      paid_storage_size_diff : Z.t;
      allocated_destination_contract : bool;
      address_registry_diff : Address_registry.diff list;
    }
  | Transaction_to_sc_rollup_result of {
      consumed_gas : Gas.Arith.fp;
      ticket_receipt : Ticket_receipt.t;
    }
  | Transaction_to_zk_rollup_result of {
      ticket_hash : Ticket_hash.t;
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
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

(** Result of applying a {!Script_typed_ir.internal_operation_contents}. *)
type _ successful_internal_operation_result =
  | ITransaction_result :
      successful_transaction_result
      -> Kind.transaction successful_internal_operation_result
  | IOrigination_result :
      successful_origination_result
      -> Kind.origination successful_internal_operation_result
  | IDelegation_result : {
      consumed_gas : Gas.Arith.fp;
      balance_updates : Receipt.balance_updates;
    }
      -> Kind.delegation successful_internal_operation_result
  | IEvent_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.event successful_internal_operation_result

type 'kind internal_operation_result =
  ( 'kind,
    'kind Kind.manager,
    'kind successful_internal_operation_result )
  Apply_operation_result.operation_result

type packed_internal_operation_result =
  | Internal_operation_result :
      'kind internal_operation * 'kind internal_operation_result
      -> packed_internal_operation_result

val internal_operation :
  'kind Script_typed_ir.internal_operation -> 'kind internal_operation

val pack_internal_operation_result :
  'kind Script_typed_ir.internal_operation ->
  'kind internal_operation_result ->
  packed_internal_operation_result

val internal_operation_encoding : packed_internal_operation Data_encoding.t

val internal_operation_result_encoding :
  packed_internal_operation_result Data_encoding.t
