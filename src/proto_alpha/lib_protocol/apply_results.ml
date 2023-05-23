(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Alpha_context
open Data_encoding
open Apply_operation_result
open Apply_internal_results

type successful_transaction_result =
  Apply_internal_results.successful_transaction_result

type successful_origination_result =
  Apply_internal_results.successful_origination_result

type _ successful_manager_operation_result =
  | Reveal_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.reveal successful_manager_operation_result
  | Transaction_result :
      successful_transaction_result
      -> Kind.transaction successful_manager_operation_result
  | Origination_result :
      successful_origination_result
      -> Kind.origination successful_manager_operation_result
  | Delegation_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.delegation successful_manager_operation_result
  | Register_global_constant_result : {
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
      size_of_constant : Z.t;
      global_address : Script_expr_hash.t;
    }
      -> Kind.register_global_constant successful_manager_operation_result
  | Increase_paid_storage_result : {
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.increase_paid_storage successful_manager_operation_result
  | Update_consensus_key_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.update_consensus_key successful_manager_operation_result
  | Transfer_ticket_result : {
      balance_updates : Receipt.balance_updates;
      ticket_receipt : Ticket_receipt.t;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.transfer_ticket successful_manager_operation_result
  | Dal_publish_slot_header_result : {
      slot_header : Dal.Slot.Header.t;
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.dal_publish_slot_header successful_manager_operation_result
  | Sc_rollup_originate_result : {
      balance_updates : Receipt.balance_updates;
      address : Sc_rollup.Address.t;
      genesis_commitment_hash : Sc_rollup.Commitment.Hash.t;
      consumed_gas : Gas.Arith.fp;
      size : Z.t;
    }
      -> Kind.sc_rollup_originate successful_manager_operation_result
  | Sc_rollup_add_messages_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.sc_rollup_add_messages successful_manager_operation_result
  | Sc_rollup_cement_result : {
      consumed_gas : Gas.Arith.fp;
      inbox_level : Raw_level.t;
      commitment_hash : Sc_rollup.Commitment.Hash.t;
    }
      -> Kind.sc_rollup_cement successful_manager_operation_result
  | Sc_rollup_publish_result : {
      consumed_gas : Gas.Arith.fp;
      staked_hash : Sc_rollup.Commitment.Hash.t;
      published_at_level : Raw_level.t;
      balance_updates : Receipt.balance_updates;
    }
      -> Kind.sc_rollup_publish successful_manager_operation_result
  | Sc_rollup_refute_result : {
      consumed_gas : Gas.Arith.fp;
      game_status : Sc_rollup.Game.status;
      balance_updates : Receipt.balance_updates;
    }
      -> Kind.sc_rollup_refute successful_manager_operation_result
  | Sc_rollup_timeout_result : {
      consumed_gas : Gas.Arith.fp;
      game_status : Sc_rollup.Game.status;
      balance_updates : Receipt.balance_updates;
    }
      -> Kind.sc_rollup_timeout successful_manager_operation_result
  | Sc_rollup_execute_outbox_message_result : {
      balance_updates : Receipt.balance_updates;
      ticket_receipt : Ticket_receipt.t;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.sc_rollup_execute_outbox_message
         successful_manager_operation_result
  | Sc_rollup_recover_bond_result : {
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.sc_rollup_recover_bond successful_manager_operation_result
  | Zk_rollup_origination_result : {
      balance_updates : Receipt.balance_updates;
      originated_zk_rollup : Zk_rollup.t;
      consumed_gas : Gas.Arith.fp;
      storage_size : Z.t;
    }
      -> Kind.zk_rollup_origination successful_manager_operation_result
  | Zk_rollup_publish_result : {
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.zk_rollup_publish successful_manager_operation_result
  | Zk_rollup_update_result : {
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.zk_rollup_update successful_manager_operation_result

let migration_origination_result_to_successful_manager_operation_result
    ({
       balance_updates;
       originated_contracts;
       storage_size;
       paid_storage_size_diff;
     } :
      Migration.origination_result) =
  Origination_result
    {
      lazy_storage_diff = None;
      balance_updates;
      originated_contracts;
      consumed_gas = Gas.Arith.zero;
      storage_size;
      paid_storage_size_diff;
    }

type packed_successful_manager_operation_result =
  | Successful_manager_result :
      'kind successful_manager_operation_result
      -> packed_successful_manager_operation_result

let pack_migration_operation_results results =
  List.map
    (fun el ->
      Successful_manager_result
        (migration_origination_result_to_successful_manager_operation_result el))
    results

type 'kind manager_operation_result =
  ( 'kind,
    'kind Kind.manager,
    'kind successful_manager_operation_result )
  operation_result

module Manager_result = struct
  type 'kind case =
    | MCase : {
        op_case : 'kind Operation.Encoding.Manager_operations.case;
        encoding : 'a Data_encoding.t;
        kind : 'kind Kind.manager;
        select :
          packed_successful_manager_operation_result ->
          'kind successful_manager_operation_result option;
        proj : 'kind successful_manager_operation_result -> 'a;
        inj : 'a -> 'kind successful_manager_operation_result;
        t : 'kind manager_operation_result Data_encoding.t;
      }
        -> 'kind case

  let make ~op_case ~encoding ~kind ~select ~proj ~inj =
    let (Operation.Encoding.Manager_operations.MCase {name; _}) = op_case in
    let t =
      def (Format.asprintf "operation.alpha.operation_result.%s" name)
      @@ union
           ~tag_size:`Uint8
           [
             case
               (Tag 0)
               ~title:"Applied"
               (merge_objs (obj1 (req "status" (constant "applied"))) encoding)
               (fun o ->
                 match o with
                 | Skipped _ | Failed _ | Backtracked _ -> None
                 | Applied o -> (
                     match select (Successful_manager_result o) with
                     | None -> None
                     | Some o -> Some ((), proj o)))
               (fun ((), x) -> Applied (inj x));
             case
               (Tag 1)
               ~title:"Failed"
               (obj2
                  (req "status" (constant "failed"))
                  (req "errors" trace_encoding))
               (function Failed (_, errs) -> Some ((), errs) | _ -> None)
               (fun ((), errs) -> Failed (kind, errs));
             case
               (Tag 2)
               ~title:"Skipped"
               (obj1 (req "status" (constant "skipped")))
               (function Skipped _ -> Some () | _ -> None)
               (fun () -> Skipped kind);
             case
               (Tag 3)
               ~title:"Backtracked"
               (merge_objs
                  (obj2
                     (req "status" (constant "backtracked"))
                     (opt "errors" trace_encoding))
                  encoding)
               (fun o ->
                 match o with
                 | Skipped _ | Failed _ | Applied _ -> None
                 | Backtracked (o, errs) -> (
                     match select (Successful_manager_result o) with
                     | None -> None
                     | Some o -> Some (((), errs), proj o)))
               (fun (((), errs), x) -> Backtracked (inj x, errs));
           ]
    in
    MCase {op_case; encoding; kind; select; proj; inj; t}

  let reveal_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.reveal_case
      ~encoding:
        Data_encoding.(
          obj1 (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~select:(function
        | Successful_manager_result (Reveal_result _ as op) -> Some op
        | _ -> None)
      ~kind:Kind.Reveal_manager_kind
      ~proj:(function Reveal_result {consumed_gas} -> consumed_gas)
      ~inj:(fun consumed_gas -> Reveal_result {consumed_gas})

  let transaction_contract_variant_cases =
    let case = function
      | Tag tag ->
          (* The tag was used by old variant. It have been removed in
             protocol proposal O, it can be unblocked in the future. *)
          let to_tx_rollup_reserved_tag = 1 in
          assert (Compare.Int.(tag <> to_tx_rollup_reserved_tag)) ;
          case (Tag tag)
      | _ as c -> case c
    in
    union
      [
        case
          ~title:"To_contract"
          (Tag 0)
          (obj9
             (opt "storage" Script.expr_encoding)
             (dft "balance_updates" Receipt.balance_updates_encoding [])
             (dft "ticket_updates" Ticket_receipt.encoding [])
             (dft "originated_contracts" (list Contract.originated_encoding) [])
             (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
             (dft "storage_size" z Z.zero)
             (dft "paid_storage_size_diff" z Z.zero)
             (dft "allocated_destination_contract" bool false)
             (opt "lazy_storage_diff" Lazy_storage.encoding))
          (function
            | Transaction_to_contract_result
                {
                  storage;
                  lazy_storage_diff;
                  balance_updates;
                  ticket_receipt;
                  originated_contracts;
                  consumed_gas;
                  storage_size;
                  paid_storage_size_diff;
                  allocated_destination_contract;
                } ->
                Some
                  ( storage,
                    balance_updates,
                    ticket_receipt,
                    originated_contracts,
                    consumed_gas,
                    storage_size,
                    paid_storage_size_diff,
                    allocated_destination_contract,
                    lazy_storage_diff )
            | _ -> None)
          (fun ( storage,
                 balance_updates,
                 ticket_receipt,
                 originated_contracts,
                 consumed_gas,
                 storage_size,
                 paid_storage_size_diff,
                 allocated_destination_contract,
                 lazy_storage_diff ) ->
            Transaction_to_contract_result
              {
                storage;
                lazy_storage_diff;
                balance_updates;
                ticket_receipt;
                originated_contracts;
                consumed_gas;
                storage_size;
                paid_storage_size_diff;
                allocated_destination_contract;
              });
        case
          ~title:"To_smart_rollup"
          (Tag 2)
          (obj2
             (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
             (req "ticket_updates" Ticket_receipt.encoding))
          (function
            | Transaction_to_sc_rollup_result {consumed_gas; ticket_receipt} ->
                Some (consumed_gas, ticket_receipt)
            | _ -> None)
          (function
            | consumed_gas, ticket_receipt ->
                Transaction_to_sc_rollup_result {consumed_gas; ticket_receipt});
      ]

  let transaction_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.transaction_case
      ~encoding:transaction_contract_variant_cases
      ~select:(function
        | Successful_manager_result (Transaction_result _ as op) -> Some op
        | _ -> None)
      ~kind:Kind.Transaction_manager_kind
      ~proj:(function Transaction_result x -> x)
      ~inj:(fun x -> Transaction_result x)

  let origination_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.origination_case
      ~encoding:
        (obj6
           (dft "balance_updates" Receipt.balance_updates_encoding [])
           (dft "originated_contracts" (list Contract.originated_encoding) [])
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
           (dft "storage_size" z Z.zero)
           (dft "paid_storage_size_diff" z Z.zero)
           (opt "lazy_storage_diff" Lazy_storage.encoding))
      ~select:(function
        | Successful_manager_result (Origination_result _ as op) -> Some op
        | _ -> None)
      ~proj:(function
        | Origination_result
            {
              lazy_storage_diff;
              balance_updates;
              originated_contracts;
              consumed_gas;
              storage_size;
              paid_storage_size_diff;
            } ->
            (* There used to be a [legacy_lazy_storage_diff] returned as the
               first component of the tuple below, and the non-legacy one
               returned as the last component. The legacy one has been removed,
               but it was chosen to keep the non-legacy one at its position,
               hence the order difference with regards to the record above. *)
            ( balance_updates,
              originated_contracts,
              consumed_gas,
              storage_size,
              paid_storage_size_diff,
              lazy_storage_diff ))
      ~kind:Kind.Origination_manager_kind
      ~inj:
        (fun ( balance_updates,
               originated_contracts,
               consumed_gas,
               storage_size,
               paid_storage_size_diff,
               lazy_storage_diff ) ->
        Origination_result
          {
            lazy_storage_diff;
            balance_updates;
            originated_contracts;
            consumed_gas;
            storage_size;
            paid_storage_size_diff;
          })

  let register_global_constant_case =
    make
      ~op_case:
        Operation.Encoding.Manager_operations.register_global_constant_case
      ~encoding:
        (obj4
           (dft "balance_updates" Receipt.balance_updates_encoding [])
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
           (dft "storage_size" z Z.zero)
           (req "global_address" Script_expr_hash.encoding))
      ~select:(function
        | Successful_manager_result (Register_global_constant_result _ as op) ->
            Some op
        | _ -> None)
      ~proj:(function
        | Register_global_constant_result
            {balance_updates; consumed_gas; size_of_constant; global_address} ->
            (balance_updates, consumed_gas, size_of_constant, global_address))
      ~kind:Kind.Register_global_constant_manager_kind
      ~inj:
        (fun (balance_updates, consumed_gas, size_of_constant, global_address) ->
        Register_global_constant_result
          {balance_updates; consumed_gas; size_of_constant; global_address})

  let delegation_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.delegation_case
      ~encoding:
        Data_encoding.(
          obj1 (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~select:(function
        | Successful_manager_result (Delegation_result _ as op) -> Some op
        | _ -> None)
      ~kind:Kind.Delegation_manager_kind
      ~proj:(function Delegation_result {consumed_gas} -> consumed_gas)
      ~inj:(fun consumed_gas -> Delegation_result {consumed_gas})

  let update_consensus_key_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.update_consensus_key_case
      ~encoding:
        Data_encoding.(
          obj1 (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~select:(function
        | Successful_manager_result (Update_consensus_key_result _ as op) ->
            Some op
        | _ -> None)
      ~kind:Kind.Update_consensus_key_manager_kind
      ~proj:(function
        | Update_consensus_key_result {consumed_gas} -> consumed_gas)
      ~inj:(fun consumed_gas -> Update_consensus_key_result {consumed_gas})

  let increase_paid_storage_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.increase_paid_storage_case
      ~encoding:
        Data_encoding.(
          obj2
            (dft "balance_updates" Receipt.balance_updates_encoding [])
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~select:(function
        | Successful_manager_result (Increase_paid_storage_result _ as op) ->
            Some op
        | _ -> None)
      ~kind:Kind.Increase_paid_storage_manager_kind
      ~proj:(function
        | Increase_paid_storage_result {balance_updates; consumed_gas} ->
            (balance_updates, consumed_gas))
      ~inj:(fun (balance_updates, consumed_gas) ->
        Increase_paid_storage_result {balance_updates; consumed_gas})

  let transfer_ticket_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.transfer_ticket_case
      ~encoding:
        Data_encoding.(
          obj4
            (req "balance_updates" Receipt.balance_updates_encoding)
            (req "ticket_updates" Ticket_receipt.encoding)
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
            (dft "paid_storage_size_diff" z Z.zero))
      ~select:(function
        | Successful_manager_result (Transfer_ticket_result _ as op) -> Some op
        | _ -> None)
      ~kind:Kind.Transfer_ticket_manager_kind
      ~proj:(function
        | Transfer_ticket_result
            {
              balance_updates;
              ticket_receipt;
              consumed_gas;
              paid_storage_size_diff;
            } ->
            ( balance_updates,
              ticket_receipt,
              consumed_gas,
              paid_storage_size_diff ))
      ~inj:
        (fun ( balance_updates,
               ticket_receipt,
               consumed_gas,
               paid_storage_size_diff ) ->
        Transfer_ticket_result
          {
            balance_updates;
            ticket_receipt;
            consumed_gas;
            paid_storage_size_diff;
          })

  let dal_publish_slot_header_case =
    make
      ~op_case:
        Operation.Encoding.Manager_operations.dal_publish_slot_header_case
      ~encoding:
        (obj2
           (req "slot_header" Dal.Slot.Header.encoding)
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~select:(function
        | Successful_manager_result (Dal_publish_slot_header_result _ as op) ->
            Some op
        | _ -> None)
      ~proj:(function
        | Dal_publish_slot_header_result {slot_header; consumed_gas} ->
            (slot_header, consumed_gas))
      ~kind:Kind.Dal_publish_slot_header_manager_kind
      ~inj:(fun (slot_header, consumed_gas) ->
        Dal_publish_slot_header_result {slot_header; consumed_gas})

  let zk_rollup_origination_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.zk_rollup_origination_case
      ~encoding:
        Data_encoding.(
          obj4
            (req "balance_updates" Receipt.balance_updates_encoding)
            (req "originated_zk_rollup" Zk_rollup.Address.encoding)
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
            (req "size" z))
      ~select:(function
        | Successful_manager_result (Zk_rollup_origination_result _ as op) ->
            Some op
        | _ -> None)
      ~kind:Kind.Zk_rollup_origination_manager_kind
      ~proj:(function
        | Zk_rollup_origination_result
            {balance_updates; originated_zk_rollup; consumed_gas; storage_size}
          ->
            (balance_updates, originated_zk_rollup, consumed_gas, storage_size))
      ~inj:
        (fun (balance_updates, originated_zk_rollup, consumed_gas, storage_size) ->
        Zk_rollup_origination_result
          {balance_updates; originated_zk_rollup; consumed_gas; storage_size})

  let zk_rollup_publish_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.zk_rollup_publish_case
      ~encoding:
        Data_encoding.(
          obj3
            (req "balance_updates" Receipt.balance_updates_encoding)
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
            (req "size" z))
      ~select:(function
        | Successful_manager_result (Zk_rollup_publish_result _ as op) ->
            Some op
        | _ -> None)
      ~kind:Kind.Zk_rollup_publish_manager_kind
      ~proj:(function
        | Zk_rollup_publish_result
            {balance_updates; consumed_gas; paid_storage_size_diff} ->
            (balance_updates, consumed_gas, paid_storage_size_diff))
      ~inj:(fun (balance_updates, consumed_gas, paid_storage_size_diff) ->
        Zk_rollup_publish_result
          {balance_updates; consumed_gas; paid_storage_size_diff})

  let zk_rollup_update_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.zk_rollup_update_case
      ~encoding:
        Data_encoding.(
          obj3
            (req "balance_updates" Receipt.balance_updates_encoding)
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
            (dft "paid_storage_size_diff" z Z.zero))
      ~select:(function
        | Successful_manager_result (Zk_rollup_update_result _ as op) -> Some op
        | _ -> None)
      ~kind:Kind.Zk_rollup_update_manager_kind
      ~proj:(function
        | Zk_rollup_update_result
            {balance_updates; consumed_gas; paid_storage_size_diff} ->
            (balance_updates, consumed_gas, paid_storage_size_diff))
      ~inj:(fun (balance_updates, consumed_gas, paid_storage_size_diff) ->
        Zk_rollup_update_result
          {balance_updates; consumed_gas; paid_storage_size_diff})

  let sc_rollup_originate_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.sc_rollup_originate_case
      ~encoding:
        (obj5
           (req "balance_updates" Receipt.balance_updates_encoding)
           (req "address" Sc_rollup.Address.encoding)
           (req "genesis_commitment_hash" Sc_rollup.Commitment.Hash.encoding)
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
           (req "size" z))
      ~select:(function
        | Successful_manager_result (Sc_rollup_originate_result _ as op) ->
            Some op
        | _ -> None)
      ~proj:(function
        | Sc_rollup_originate_result
            {
              balance_updates;
              address;
              genesis_commitment_hash;
              consumed_gas;
              size;
            } ->
            ( balance_updates,
              address,
              genesis_commitment_hash,
              consumed_gas,
              size ))
      ~kind:Kind.Sc_rollup_originate_manager_kind
      ~inj:
        (fun ( balance_updates,
               address,
               genesis_commitment_hash,
               consumed_gas,
               size ) ->
        Sc_rollup_originate_result
          {
            balance_updates;
            address;
            genesis_commitment_hash;
            consumed_gas;
            size;
          })

  let sc_rollup_add_messages_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.sc_rollup_add_messages_case
      ~encoding:
        (obj1 (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~select:(function
        | Successful_manager_result (Sc_rollup_add_messages_result _ as op) ->
            Some op
        | _ -> None)
      ~proj:(function
        | Sc_rollup_add_messages_result {consumed_gas} -> consumed_gas)
      ~kind:Kind.Sc_rollup_add_messages_manager_kind
      ~inj:(fun consumed_gas -> Sc_rollup_add_messages_result {consumed_gas})

  let sc_rollup_cement_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.sc_rollup_cement_case
      ~encoding:
        (obj3
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
           (req "inbox_level" Raw_level.encoding)
           (req "commitment_hash" Sc_rollup.Commitment.Hash.encoding))
      ~select:(function
        | Successful_manager_result (Sc_rollup_cement_result _ as op) -> Some op
        | _ -> None)
      ~proj:(function
        | Sc_rollup_cement_result {consumed_gas; inbox_level; commitment_hash}
          ->
            (consumed_gas, inbox_level, commitment_hash))
      ~kind:Kind.Sc_rollup_cement_manager_kind
      ~inj:(fun (consumed_gas, inbox_level, commitment_hash) ->
        Sc_rollup_cement_result {consumed_gas; inbox_level; commitment_hash})

  let sc_rollup_publish_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.sc_rollup_publish_case
      ~encoding:
        (obj4
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
           (req "staked_hash" Sc_rollup.Commitment.Hash.encoding)
           (req "published_at_level" Raw_level.encoding)
           (req "balance_updates" Receipt.balance_updates_encoding))
      ~select:(function
        | Successful_manager_result (Sc_rollup_publish_result _ as op) ->
            Some op
        | _ -> None)
      ~proj:(function
        | Sc_rollup_publish_result
            {consumed_gas; staked_hash; published_at_level; balance_updates} ->
            (consumed_gas, staked_hash, published_at_level, balance_updates))
      ~kind:Kind.Sc_rollup_publish_manager_kind
      ~inj:
        (fun (consumed_gas, staked_hash, published_at_level, balance_updates) ->
        Sc_rollup_publish_result
          {consumed_gas; staked_hash; published_at_level; balance_updates})

  let sc_rollup_refute_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.sc_rollup_refute_case
      ~encoding:
        Data_encoding.(
          obj3
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
            (req "game_status" Sc_rollup.Game.status_encoding)
            (req "balance_updates" Receipt.balance_updates_encoding))
      ~select:(function
        | Successful_manager_result (Sc_rollup_refute_result _ as op) -> Some op
        | _ -> None)
      ~proj:(function
        | Sc_rollup_refute_result {consumed_gas; game_status; balance_updates}
          ->
            (consumed_gas, game_status, balance_updates))
      ~kind:Kind.Sc_rollup_refute_manager_kind
      ~inj:(fun (consumed_gas, game_status, balance_updates) ->
        Sc_rollup_refute_result {consumed_gas; game_status; balance_updates})

  let sc_rollup_timeout_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.sc_rollup_timeout_case
      ~encoding:
        (obj3
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
           (req "game_status" Sc_rollup.Game.status_encoding)
           (req "balance_updates" Receipt.balance_updates_encoding))
      ~select:(function
        | Successful_manager_result (Sc_rollup_timeout_result _ as op) ->
            Some op
        | _ -> None)
      ~proj:(function
        | Sc_rollup_timeout_result {consumed_gas; game_status; balance_updates}
          ->
            (consumed_gas, game_status, balance_updates))
      ~kind:Kind.Sc_rollup_timeout_manager_kind
      ~inj:(fun (consumed_gas, game_status, balance_updates) ->
        Sc_rollup_timeout_result {consumed_gas; game_status; balance_updates})

  let sc_rollup_execute_outbox_message_case =
    make
      ~op_case:
        Operation.Encoding.Manager_operations
        .sc_rollup_execute_outbox_message_case
      ~encoding:
        Data_encoding.(
          obj4
            (req "balance_updates" Receipt.balance_updates_encoding)
            (req "ticket_updates" Ticket_receipt.encoding)
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
            (dft "paid_storage_size_diff" z Z.zero))
      ~select:(function
        | Successful_manager_result
            (Sc_rollup_execute_outbox_message_result _ as op) ->
            Some op
        | _ -> None)
      ~kind:Kind.Sc_rollup_execute_outbox_message_manager_kind
      ~proj:(function
        | Sc_rollup_execute_outbox_message_result
            {
              balance_updates;
              ticket_receipt;
              consumed_gas;
              paid_storage_size_diff;
            } ->
            ( balance_updates,
              ticket_receipt,
              consumed_gas,
              paid_storage_size_diff ))
      ~inj:
        (fun ( balance_updates,
               ticket_receipt,
               consumed_gas,
               paid_storage_size_diff ) ->
        Sc_rollup_execute_outbox_message_result
          {
            balance_updates;
            ticket_receipt;
            consumed_gas;
            paid_storage_size_diff;
          })

  let sc_rollup_recover_bond_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.sc_rollup_recover_bond_case
      ~encoding:
        Data_encoding.(
          obj2
            (req "balance_updates" Receipt.balance_updates_encoding)
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~select:(function
        | Successful_manager_result (Sc_rollup_recover_bond_result _ as op) ->
            Some op
        | _ -> None)
      ~kind:Kind.Sc_rollup_recover_bond_manager_kind
      ~proj:(function
        | Sc_rollup_recover_bond_result {balance_updates; consumed_gas} ->
            (balance_updates, consumed_gas))
      ~inj:(fun (balance_updates, consumed_gas) ->
        Sc_rollup_recover_bond_result {balance_updates; consumed_gas})
end

let successful_manager_operation_result_encoding :
    packed_successful_manager_operation_result Data_encoding.t =
  let make (type kind)
      (Manager_result.MCase res_case : kind Manager_result.case) =
    let (Operation.Encoding.Manager_operations.MCase op_case) =
      res_case.op_case
    in
    case
      (Tag op_case.tag)
      ~title:op_case.name
      (merge_objs (obj1 (req "kind" (constant op_case.name))) res_case.encoding)
      (fun res ->
        match res_case.select res with
        | Some res -> Some ((), res_case.proj res)
        | None -> None)
      (fun ((), res) -> Successful_manager_result (res_case.inj res))
  in
  def "operation.alpha.successful_manager_operation_result"
  @@ union
       [
         make Manager_result.reveal_case;
         make Manager_result.transaction_case;
         make Manager_result.origination_case;
         make Manager_result.delegation_case;
         make Manager_result.update_consensus_key_case;
         make Manager_result.increase_paid_storage_case;
         make Manager_result.sc_rollup_originate_case;
       ]

type 'kind contents_result =
  | Preendorsement_result : {
      balance_updates : Receipt.balance_updates;
      delegate : Signature.public_key_hash;
      consensus_key : Signature.public_key_hash;
      consensus_power : int;
    }
      -> Kind.preendorsement contents_result
  | Endorsement_result : {
      balance_updates : Receipt.balance_updates;
      delegate : Signature.public_key_hash;
      consensus_key : Signature.public_key_hash;
      consensus_power : int;
    }
      -> Kind.endorsement contents_result
  | Dal_attestation_result : {
      delegate : Signature.Public_key_hash.t;
    }
      -> Kind.dal_attestation contents_result
  | Seed_nonce_revelation_result :
      Receipt.balance_updates
      -> Kind.seed_nonce_revelation contents_result
  | Vdf_revelation_result :
      Receipt.balance_updates
      -> Kind.vdf_revelation contents_result
  | Double_endorsement_evidence_result :
      Receipt.balance_updates
      -> Kind.double_endorsement_evidence contents_result
  | Double_preendorsement_evidence_result :
      Receipt.balance_updates
      -> Kind.double_preendorsement_evidence contents_result
  | Double_baking_evidence_result :
      Receipt.balance_updates
      -> Kind.double_baking_evidence contents_result
  | Activate_account_result :
      Receipt.balance_updates
      -> Kind.activate_account contents_result
  | Proposals_result : Kind.proposals contents_result
  | Ballot_result : Kind.ballot contents_result
  | Drain_delegate_result : {
      balance_updates : Receipt.balance_updates;
      allocated_destination_contract : bool;
    }
      -> Kind.drain_delegate contents_result
  | Manager_operation_result : {
      balance_updates : Receipt.balance_updates;
      operation_result : 'kind manager_operation_result;
      internal_operation_results : packed_internal_operation_result list;
    }
      -> 'kind Kind.manager contents_result

type packed_contents_result =
  | Contents_result : 'kind contents_result -> packed_contents_result

type packed_contents_and_result =
  | Contents_and_result :
      'kind Operation.contents * 'kind contents_result
      -> packed_contents_and_result

type ('a, 'b) eq = Eq : ('a, 'a) eq

let equal_manager_kind :
    type a b. a Kind.manager -> b Kind.manager -> (a, b) eq option =
 fun ka kb ->
  match (ka, kb) with
  | Kind.Reveal_manager_kind, Kind.Reveal_manager_kind -> Some Eq
  | Kind.Reveal_manager_kind, _ -> None
  | Kind.Transaction_manager_kind, Kind.Transaction_manager_kind -> Some Eq
  | Kind.Transaction_manager_kind, _ -> None
  | Kind.Origination_manager_kind, Kind.Origination_manager_kind -> Some Eq
  | Kind.Origination_manager_kind, _ -> None
  | Kind.Delegation_manager_kind, Kind.Delegation_manager_kind -> Some Eq
  | Kind.Delegation_manager_kind, _ -> None
  | ( Kind.Update_consensus_key_manager_kind,
      Kind.Update_consensus_key_manager_kind ) ->
      Some Eq
  | Kind.Update_consensus_key_manager_kind, _ -> None
  | ( Kind.Register_global_constant_manager_kind,
      Kind.Register_global_constant_manager_kind ) ->
      Some Eq
  | Kind.Event_manager_kind, Kind.Event_manager_kind -> Some Eq
  | Kind.Event_manager_kind, _ -> None
  | Kind.Register_global_constant_manager_kind, _ -> None
  | ( Kind.Increase_paid_storage_manager_kind,
      Kind.Increase_paid_storage_manager_kind ) ->
      Some Eq
  | Kind.Increase_paid_storage_manager_kind, _ -> None
  | Kind.Transfer_ticket_manager_kind, Kind.Transfer_ticket_manager_kind ->
      Some Eq
  | Kind.Transfer_ticket_manager_kind, _ -> None
  | ( Kind.Dal_publish_slot_header_manager_kind,
      Kind.Dal_publish_slot_header_manager_kind ) ->
      Some Eq
  | Kind.Dal_publish_slot_header_manager_kind, _ -> None
  | Kind.Sc_rollup_originate_manager_kind, Kind.Sc_rollup_originate_manager_kind
    ->
      Some Eq
  | Kind.Sc_rollup_originate_manager_kind, _ -> None
  | ( Kind.Sc_rollup_add_messages_manager_kind,
      Kind.Sc_rollup_add_messages_manager_kind ) ->
      Some Eq
  | Kind.Sc_rollup_add_messages_manager_kind, _ -> None
  | Kind.Sc_rollup_cement_manager_kind, Kind.Sc_rollup_cement_manager_kind ->
      Some Eq
  | Kind.Sc_rollup_cement_manager_kind, _ -> None
  | Kind.Sc_rollup_publish_manager_kind, Kind.Sc_rollup_publish_manager_kind ->
      Some Eq
  | Kind.Sc_rollup_publish_manager_kind, _ -> None
  | Kind.Sc_rollup_refute_manager_kind, Kind.Sc_rollup_refute_manager_kind ->
      Some Eq
  | Kind.Sc_rollup_refute_manager_kind, _ -> None
  | Kind.Sc_rollup_timeout_manager_kind, Kind.Sc_rollup_timeout_manager_kind ->
      Some Eq
  | Kind.Sc_rollup_timeout_manager_kind, _ -> None
  | ( Kind.Sc_rollup_execute_outbox_message_manager_kind,
      Kind.Sc_rollup_execute_outbox_message_manager_kind ) ->
      Some Eq
  | Kind.Sc_rollup_execute_outbox_message_manager_kind, _ -> None
  | ( Kind.Sc_rollup_recover_bond_manager_kind,
      Kind.Sc_rollup_recover_bond_manager_kind ) ->
      Some Eq
  | Kind.Sc_rollup_recover_bond_manager_kind, _ -> None
  | ( Kind.Zk_rollup_origination_manager_kind,
      Kind.Zk_rollup_origination_manager_kind ) ->
      Some Eq
  | Kind.Zk_rollup_origination_manager_kind, _ -> None
  | Kind.Zk_rollup_publish_manager_kind, Kind.Zk_rollup_publish_manager_kind ->
      Some Eq
  | Kind.Zk_rollup_publish_manager_kind, _ -> None
  | Kind.Zk_rollup_update_manager_kind, Kind.Zk_rollup_update_manager_kind ->
      Some Eq
  | Kind.Zk_rollup_update_manager_kind, _ -> None

module Encoding = struct
  let consensus_result_encoding power_name =
    let open Data_encoding in
    obj4
      (dft "balance_updates" Receipt.balance_updates_encoding [])
      (req "delegate" Signature.Public_key_hash.encoding)
      (req (Format.asprintf "%s_power" power_name) int31)
      (req "consensus_key" Signature.Public_key_hash.encoding)

  let consensus_result_encoding_legacy power_name =
    consensus_result_encoding power_name

  let consensus_result_encoding = consensus_result_encoding "consensus"

  type case =
    | Case : {
        op_case : 'kind Operation.Encoding.case;
        encoding : 'a Data_encoding.t;
        select : packed_contents_result -> 'kind contents_result option;
        mselect :
          packed_contents_and_result ->
          ('kind contents * 'kind contents_result) option;
        proj : 'kind contents_result -> 'a;
        inj : 'a -> 'kind contents_result;
      }
        -> case

  let tagged_case tag name args proj inj =
    let open Data_encoding in
    case
      tag
      ~title:(String.capitalize_ascii name)
      (merge_objs (obj1 (req "kind" (constant name))) args)
      (fun x -> match proj x with None -> None | Some x -> Some ((), x))
      (fun ((), x) -> inj x)

  let preendorsement_case =
    Case
      {
        op_case = Operation.Encoding.preendorsement_case;
        encoding = consensus_result_encoding_legacy "preendorsement";
        select =
          (function
          | Contents_result (Preendorsement_result _ as op) -> Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Preendorsement _ as op), res) -> Some (op, res)
          | _ -> None);
        proj =
          (function
          | Preendorsement_result
              {balance_updates; delegate; consensus_key; consensus_power} ->
              (balance_updates, delegate, consensus_power, consensus_key));
        inj =
          (fun (balance_updates, delegate, consensus_power, consensus_key) ->
            Preendorsement_result
              {balance_updates; delegate; consensus_key; consensus_power});
      }

  let preattestation_case =
    Case
      {
        op_case = Operation.Encoding.preattestation_case;
        encoding = consensus_result_encoding;
        select =
          (function
          | Contents_result (Preendorsement_result _ as op) -> Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Preendorsement _ as op), res) -> Some (op, res)
          | _ -> None);
        proj =
          (function
          | Preendorsement_result
              {balance_updates; delegate; consensus_key; consensus_power} ->
              (balance_updates, delegate, consensus_power, consensus_key));
        inj =
          (fun (balance_updates, delegate, consensus_power, consensus_key) ->
            Preendorsement_result
              {balance_updates; delegate; consensus_key; consensus_power});
      }

  let endorsement_case =
    Case
      {
        op_case = Operation.Encoding.endorsement_case;
        encoding = consensus_result_encoding_legacy "endorsement";
        select =
          (function
          | Contents_result (Endorsement_result _ as op) -> Some op | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Endorsement _ as op), res) -> Some (op, res)
          | _ -> None);
        proj =
          (function
          | Endorsement_result
              {balance_updates; delegate; consensus_key; consensus_power} ->
              (balance_updates, delegate, consensus_power, consensus_key));
        inj =
          (fun (balance_updates, delegate, consensus_power, consensus_key) ->
            Endorsement_result
              {balance_updates; delegate; consensus_key; consensus_power});
      }

  let attestation_case =
    Case
      {
        op_case = Operation.Encoding.attestation_case;
        encoding = consensus_result_encoding;
        select =
          (function
          | Contents_result (Endorsement_result _ as op) -> Some op | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Endorsement _ as op), res) -> Some (op, res)
          | _ -> None);
        proj =
          (function
          | Endorsement_result
              {balance_updates; delegate; consensus_key; consensus_power} ->
              (balance_updates, delegate, consensus_power, consensus_key));
        inj =
          (fun (balance_updates, delegate, consensus_power, consensus_key) ->
            Endorsement_result
              {balance_updates; delegate; consensus_key; consensus_power});
      }

  let dal_attestation_case =
    Case
      {
        op_case = Operation.Encoding.dal_attestation_case;
        encoding = obj1 (req "delegate" Signature.Public_key_hash.encoding);
        select =
          (function
          | Contents_result (Dal_attestation_result _ as op) -> Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Dal_attestation _ as op), res) ->
              Some (op, res)
          | _ -> None);
        proj = (function Dal_attestation_result {delegate} -> delegate);
        inj = (fun delegate -> Dal_attestation_result {delegate});
      }

  let seed_nonce_revelation_case =
    Case
      {
        op_case = Operation.Encoding.seed_nonce_revelation_case;
        encoding =
          obj1 (dft "balance_updates" Receipt.balance_updates_encoding []);
        select =
          (function
          | Contents_result (Seed_nonce_revelation_result _ as op) -> Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Seed_nonce_revelation _ as op), res) ->
              Some (op, res)
          | _ -> None);
        proj = (fun (Seed_nonce_revelation_result bus) -> bus);
        inj = (fun bus -> Seed_nonce_revelation_result bus);
      }

  let vdf_revelation_case =
    Case
      {
        op_case = Operation.Encoding.vdf_revelation_case;
        encoding =
          obj1 (dft "balance_updates" Receipt.balance_updates_encoding []);
        select =
          (function
          | Contents_result (Vdf_revelation_result _ as op) -> Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Vdf_revelation _ as op), res) -> Some (op, res)
          | _ -> None);
        proj = (fun (Vdf_revelation_result bus) -> bus);
        inj = (fun bus -> Vdf_revelation_result bus);
      }

  let double_endorsement_evidence_case =
    Case
      {
        op_case = Operation.Encoding.double_endorsement_evidence_case;
        encoding =
          obj1 (dft "balance_updates" Receipt.balance_updates_encoding []);
        select =
          (function
          | Contents_result (Double_endorsement_evidence_result _ as op) ->
              Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Double_endorsement_evidence _ as op), res) ->
              Some (op, res)
          | _ -> None);
        proj = (fun (Double_endorsement_evidence_result bus) -> bus);
        inj = (fun bus -> Double_endorsement_evidence_result bus);
      }

  let double_attestation_evidence_case =
    Case
      {
        op_case = Operation.Encoding.double_attestation_evidence_case;
        encoding =
          obj1 (dft "balance_updates" Receipt.balance_updates_encoding []);
        select =
          (function
          | Contents_result (Double_endorsement_evidence_result _ as op) ->
              Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Double_endorsement_evidence _ as op), res) ->
              Some (op, res)
          | _ -> None);
        proj = (fun (Double_endorsement_evidence_result bus) -> bus);
        inj = (fun bus -> Double_endorsement_evidence_result bus);
      }

  let double_preendorsement_evidence_case =
    Case
      {
        op_case = Operation.Encoding.double_preendorsement_evidence_case;
        encoding =
          obj1 (dft "balance_updates" Receipt.balance_updates_encoding []);
        select =
          (function
          | Contents_result (Double_preendorsement_evidence_result _ as op) ->
              Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Double_preendorsement_evidence _ as op), res)
            ->
              Some (op, res)
          | _ -> None);
        proj = (fun (Double_preendorsement_evidence_result bus) -> bus);
        inj = (fun bus -> Double_preendorsement_evidence_result bus);
      }

  let double_preattestation_evidence_case =
    Case
      {
        op_case = Operation.Encoding.double_preattestation_evidence_case;
        encoding =
          obj1 (dft "balance_updates" Receipt.balance_updates_encoding []);
        select =
          (function
          | Contents_result (Double_preendorsement_evidence_result _ as op) ->
              Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Double_preendorsement_evidence _ as op), res)
            ->
              Some (op, res)
          | _ -> None);
        proj = (fun (Double_preendorsement_evidence_result bus) -> bus);
        inj = (fun bus -> Double_preendorsement_evidence_result bus);
      }

  let double_baking_evidence_case =
    Case
      {
        op_case = Operation.Encoding.double_baking_evidence_case;
        encoding =
          obj1 (dft "balance_updates" Receipt.balance_updates_encoding []);
        select =
          (function
          | Contents_result (Double_baking_evidence_result _ as op) -> Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Double_baking_evidence _ as op), res) ->
              Some (op, res)
          | _ -> None);
        proj = (fun (Double_baking_evidence_result bus) -> bus);
        inj = (fun bus -> Double_baking_evidence_result bus);
      }

  let activate_account_case =
    Case
      {
        op_case = Operation.Encoding.activate_account_case;
        encoding =
          obj1 (dft "balance_updates" Receipt.balance_updates_encoding []);
        select =
          (function
          | Contents_result (Activate_account_result _ as op) -> Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Activate_account _ as op), res) ->
              Some (op, res)
          | _ -> None);
        proj = (fun (Activate_account_result bus) -> bus);
        inj = (fun bus -> Activate_account_result bus);
      }

  let proposals_case =
    Case
      {
        op_case = Operation.Encoding.proposals_case;
        encoding = Data_encoding.empty;
        select =
          (function
          | Contents_result (Proposals_result as op) -> Some op | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Proposals _ as op), res) -> Some (op, res)
          | _ -> None);
        proj = (fun Proposals_result -> ());
        inj = (fun () -> Proposals_result);
      }

  let ballot_case =
    Case
      {
        op_case = Operation.Encoding.ballot_case;
        encoding = Data_encoding.empty;
        select =
          (function
          | Contents_result (Ballot_result as op) -> Some op | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Ballot _ as op), res) -> Some (op, res)
          | _ -> None);
        proj = (fun Ballot_result -> ());
        inj = (fun () -> Ballot_result);
      }

  let drain_delegate_case =
    Case
      {
        op_case = Operation.Encoding.drain_delegate_case;
        encoding =
          Data_encoding.(
            obj2
              (dft "balance_updates" Receipt.balance_updates_encoding [])
              (dft "allocated_destination_contract" bool false));
        select =
          (function
          | Contents_result (Drain_delegate_result _ as op) -> Some op
          | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Drain_delegate _ as op), res) -> Some (op, res)
          | _ -> None);
        proj =
          (function
          | Drain_delegate_result
              {balance_updates; allocated_destination_contract} ->
              (balance_updates, allocated_destination_contract));
        inj =
          (fun (balance_updates, allocated_destination_contract) ->
            Drain_delegate_result
              {balance_updates; allocated_destination_contract});
      }

  let make_manager_case (type kind)
      (Operation.Encoding.Case op_case :
        kind Kind.manager Operation.Encoding.case)
      (Manager_result.MCase res_case : kind Manager_result.case) mselect =
    Case
      {
        op_case = Operation.Encoding.Case op_case;
        encoding =
          obj3
            (dft "balance_updates" Receipt.balance_updates_encoding [])
            (req "operation_result" res_case.t)
            (dft
               "internal_operation_results"
               (list internal_operation_result_encoding)
               []);
        select =
          (function
          | Contents_result
              (Manager_operation_result
                ({operation_result = Applied res; _} as op)) -> (
              match res_case.select (Successful_manager_result res) with
              | Some res ->
                  Some
                    (Manager_operation_result
                       {op with operation_result = Applied res})
              | None -> None)
          | Contents_result
              (Manager_operation_result
                ({operation_result = Backtracked (res, errs); _} as op)) -> (
              match res_case.select (Successful_manager_result res) with
              | Some res ->
                  Some
                    (Manager_operation_result
                       {op with operation_result = Backtracked (res, errs)})
              | None -> None)
          | Contents_result
              (Manager_operation_result
                ({operation_result = Skipped kind; _} as op)) -> (
              match equal_manager_kind kind res_case.kind with
              | None -> None
              | Some Eq ->
                  Some
                    (Manager_operation_result
                       {op with operation_result = Skipped kind}))
          | Contents_result
              (Manager_operation_result
                ({operation_result = Failed (kind, errs); _} as op)) -> (
              match equal_manager_kind kind res_case.kind with
              | None -> None
              | Some Eq ->
                  Some
                    (Manager_operation_result
                       {op with operation_result = Failed (kind, errs)}))
          | Contents_result (Preendorsement_result _) -> None
          | Contents_result (Endorsement_result _) -> None
          | Contents_result (Dal_attestation_result _) -> None
          | Contents_result Ballot_result -> None
          | Contents_result (Seed_nonce_revelation_result _) -> None
          | Contents_result (Vdf_revelation_result _) -> None
          | Contents_result (Double_endorsement_evidence_result _) -> None
          | Contents_result (Double_preendorsement_evidence_result _) -> None
          | Contents_result (Double_baking_evidence_result _) -> None
          | Contents_result (Activate_account_result _) -> None
          | Contents_result (Drain_delegate_result _) -> None
          | Contents_result Proposals_result -> None);
        mselect;
        proj =
          (fun (Manager_operation_result
                 {
                   balance_updates = bus;
                   operation_result = r;
                   internal_operation_results = rs;
                 }) ->
            (bus, r, rs));
        inj =
          (fun (bus, r, rs) ->
            Manager_operation_result
              {
                balance_updates = bus;
                operation_result = r;
                internal_operation_results = rs;
              });
      }

  let reveal_case =
    make_manager_case
      Operation.Encoding.reveal_case
      Manager_result.reveal_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Reveal _; _} as op), res) ->
            Some (op, res)
        | _ -> None)

  let transaction_case =
    make_manager_case
      Operation.Encoding.transaction_case
      Manager_result.transaction_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Transaction _; _} as op), res) ->
            Some (op, res)
        | _ -> None)

  let origination_case =
    make_manager_case
      Operation.Encoding.origination_case
      Manager_result.origination_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Origination _; _} as op), res) ->
            Some (op, res)
        | _ -> None)

  let delegation_case =
    make_manager_case
      Operation.Encoding.delegation_case
      Manager_result.delegation_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Delegation _; _} as op), res) ->
            Some (op, res)
        | _ -> None)

  let update_consensus_key_case =
    make_manager_case
      Operation.Encoding.update_consensus_key_case
      Manager_result.update_consensus_key_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Update_consensus_key _; _} as op),
              res ) ->
            Some (op, res)
        | _ -> None)

  let register_global_constant_case =
    make_manager_case
      Operation.Encoding.register_global_constant_case
      Manager_result.register_global_constant_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Register_global_constant _; _} as
              op),
              res ) ->
            Some (op, res)
        | _ -> None)

  let increase_paid_storage_case =
    make_manager_case
      Operation.Encoding.increase_paid_storage_case
      Manager_result.increase_paid_storage_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Increase_paid_storage _; _} as op),
              res ) ->
            Some (op, res)
        | _ -> None)

  let transfer_ticket_case =
    make_manager_case
      Operation.Encoding.transfer_ticket_case
      Manager_result.transfer_ticket_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Transfer_ticket _; _} as op), res)
          ->
            Some (op, res)
        | _ -> None)

  let dal_publish_slot_header_case =
    make_manager_case
      Operation.Encoding.dal_publish_slot_header_case
      Manager_result.dal_publish_slot_header_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Dal_publish_slot_header _; _} as
              op),
              res ) ->
            Some (op, res)
        | _ -> None)

  let sc_rollup_originate_case =
    make_manager_case
      Operation.Encoding.sc_rollup_originate_case
      Manager_result.sc_rollup_originate_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Sc_rollup_originate _; _} as op),
              res ) ->
            Some (op, res)
        | _ -> None)

  let sc_rollup_add_messages_case =
    make_manager_case
      Operation.Encoding.sc_rollup_add_messages_case
      Manager_result.sc_rollup_add_messages_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Sc_rollup_add_messages _; _} as op),
              res ) ->
            Some (op, res)
        | _ -> None)

  let sc_rollup_cement_case =
    make_manager_case
      Operation.Encoding.sc_rollup_cement_case
      Manager_result.sc_rollup_cement_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Sc_rollup_cement _; _} as op), res)
          ->
            Some (op, res)
        | _ -> None)

  let sc_rollup_publish_case =
    make_manager_case
      Operation.Encoding.sc_rollup_publish_case
      Manager_result.sc_rollup_publish_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Sc_rollup_publish _; _} as op), res)
          ->
            Some (op, res)
        | _ -> None)

  let sc_rollup_refute_case =
    make_manager_case
      Operation.Encoding.sc_rollup_refute_case
      Manager_result.sc_rollup_refute_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Sc_rollup_refute _; _} as op), res)
          ->
            Some (op, res)
        | _ -> None)

  let sc_rollup_timeout_case =
    make_manager_case
      Operation.Encoding.sc_rollup_timeout_case
      Manager_result.sc_rollup_timeout_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Sc_rollup_timeout _; _} as op), res)
          ->
            Some (op, res)
        | _ -> None)

  let sc_rollup_execute_outbox_message_case =
    make_manager_case
      Operation.Encoding.sc_rollup_execute_outbox_message_case
      Manager_result.sc_rollup_execute_outbox_message_case
      (function
        | Contents_and_result
            ( (Manager_operation
                 {operation = Sc_rollup_execute_outbox_message _; _} as op),
              res ) ->
            Some (op, res)
        | _ -> None)

  let sc_rollup_recover_bond_case =
    make_manager_case
      Operation.Encoding.sc_rollup_recover_bond_case
      Manager_result.sc_rollup_recover_bond_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Sc_rollup_recover_bond _; _} as op),
              res ) ->
            Some (op, res)
        | _ -> None)

  let zk_rollup_origination_case =
    make_manager_case
      Operation.Encoding.zk_rollup_origination_case
      Manager_result.zk_rollup_origination_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Zk_rollup_origination _; _} as op),
              res ) ->
            Some (op, res)
        | _ -> None)

  let zk_rollup_publish_case =
    make_manager_case
      Operation.Encoding.zk_rollup_publish_case
      Manager_result.zk_rollup_publish_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Zk_rollup_publish _; _} as op), res)
          ->
            Some (op, res)
        | _ -> None)

  let zk_rollup_update_case =
    make_manager_case
      Operation.Encoding.zk_rollup_update_case
      Manager_result.zk_rollup_update_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Zk_rollup_update _; _} as op), res)
          ->
            Some (op, res)
        | _ -> None)
end

let common_cases =
  let open Encoding in
  [
    seed_nonce_revelation_case;
    vdf_revelation_case;
    dal_attestation_case;
    double_baking_evidence_case;
    activate_account_case;
    proposals_case;
    ballot_case;
    drain_delegate_case;
    reveal_case;
    transaction_case;
    origination_case;
    delegation_case;
    register_global_constant_case;
    increase_paid_storage_case;
    update_consensus_key_case;
    transfer_ticket_case;
    dal_publish_slot_header_case;
    sc_rollup_originate_case;
    sc_rollup_add_messages_case;
    sc_rollup_cement_case;
    sc_rollup_publish_case;
    sc_rollup_refute_case;
    sc_rollup_timeout_case;
    sc_rollup_execute_outbox_message_case;
    sc_rollup_recover_bond_case;
    zk_rollup_origination_case;
    zk_rollup_publish_case;
    zk_rollup_update_case;
  ]

let contents_cases =
  let open Encoding in
  attestation_case :: preattestation_case :: double_attestation_evidence_case
  :: double_preattestation_evidence_case :: common_cases

let contents_cases_with_legacy_attestation_name =
  let open Encoding in
  endorsement_case :: preendorsement_case :: double_endorsement_evidence_case
  :: double_preendorsement_evidence_case :: common_cases

let make_contents_result
    (Encoding.Case
      {
        op_case = Operation.Encoding.Case {tag; name; _};
        encoding;
        mselect = _;
        select;
        proj;
        inj;
      }) =
  let proj x = match select x with None -> None | Some x -> Some (proj x) in
  let inj x = Contents_result (inj x) in
  Encoding.tagged_case (Tag tag) name encoding proj inj

let contents_result_encoding =
  def "operation.alpha.contents_result"
  @@ union (List.map make_contents_result contents_cases)

let contents_result_encoding_with_legacy_attestation_name =
  def "operation_with_legacy_attestation_name.alpha.contents_result"
  @@ union
       (List.map
          make_contents_result
          contents_cases_with_legacy_attestation_name)

let make_contents_and_result
    (Encoding.Case
      {
        op_case = Operation.Encoding.Case {tag; name; encoding; proj; inj; _};
        mselect;
        encoding = meta_encoding;
        proj = meta_proj;
        inj = meta_inj;
        _;
      }) =
  let proj c =
    match mselect c with
    | Some (op, res) -> Some (proj op, meta_proj res)
    | _ -> None
  in
  let inj (op, res) = Contents_and_result (inj op, meta_inj res) in
  let encoding = merge_objs encoding (obj1 (req "metadata" meta_encoding)) in
  Encoding.tagged_case (Tag tag) name encoding proj inj

let contents_and_result_encoding =
  def "operation.alpha.operation_contents_and_result"
  @@ union (List.map make_contents_and_result contents_cases)

let contents_and_result_encoding_with_legacy_attestation_name =
  def
    "operation_with_legacy_attestation_name.alpha.operation_contents_and_result"
  @@ union
       (List.map
          make_contents_and_result
          contents_cases_with_legacy_attestation_name)

type 'kind contents_result_list =
  | Single_result : 'kind contents_result -> 'kind contents_result_list
  | Cons_result :
      'kind Kind.manager contents_result
      * 'rest Kind.manager contents_result_list
      -> ('kind * 'rest) Kind.manager contents_result_list

type packed_contents_result_list =
  | Contents_result_list :
      'kind contents_result_list
      -> packed_contents_result_list

let contents_result_list_conv_with_guard =
  let open Result_syntax in
  let rec to_list = function
    | Contents_result_list (Single_result o) -> [Contents_result o]
    | Contents_result_list (Cons_result (o, os)) ->
        Contents_result o :: to_list (Contents_result_list os)
  in
  let rec of_list = function
    | [] -> Error "cannot decode empty operation result"
    | [Contents_result o] -> Ok (Contents_result_list (Single_result o))
    | Contents_result o :: os -> (
        let* (Contents_result_list os) = of_list os in
        match (o, os) with
        | Manager_operation_result _, Single_result (Manager_operation_result _)
          ->
            Ok (Contents_result_list (Cons_result (o, os)))
        | Manager_operation_result _, Cons_result _ ->
            Ok (Contents_result_list (Cons_result (o, os)))
        | _ -> Error "cannot decode ill-formed operation result")
  in
  conv_with_guard to_list of_list

let contents_result_list_encoding =
  def "operation.alpha.contents_list_result"
  @@ contents_result_list_conv_with_guard (list contents_result_encoding)

let contents_result_list_encoding_with_legacy_attestation_name =
  def "operation_with_legacy_attestation_name.alpha.contents_list_result"
  @@ contents_result_list_conv_with_guard
       (list contents_result_encoding_with_legacy_attestation_name)

type 'kind contents_and_result_list =
  | Single_and_result :
      'kind Alpha_context.contents * 'kind contents_result
      -> 'kind contents_and_result_list
  | Cons_and_result :
      'kind Kind.manager Alpha_context.contents
      * 'kind Kind.manager contents_result
      * 'rest Kind.manager contents_and_result_list
      -> ('kind * 'rest) Kind.manager contents_and_result_list

type packed_contents_and_result_list =
  | Contents_and_result_list :
      'kind contents_and_result_list
      -> packed_contents_and_result_list

let contents_and_result_conv_with_guard =
  let open Result_syntax in
  let rec to_list = function
    | Contents_and_result_list (Single_and_result (op, res)) ->
        [Contents_and_result (op, res)]
    | Contents_and_result_list (Cons_and_result (op, res, rest)) ->
        Contents_and_result (op, res) :: to_list (Contents_and_result_list rest)
  in
  let rec of_list = function
    | [] -> Error "cannot decode empty combined operation result"
    | [Contents_and_result (op, res)] ->
        Ok (Contents_and_result_list (Single_and_result (op, res)))
    | Contents_and_result (op, res) :: rest -> (
        let* (Contents_and_result_list rest) = of_list rest in
        match (op, rest) with
        | Manager_operation _, Single_and_result (Manager_operation _, _) ->
            Ok (Contents_and_result_list (Cons_and_result (op, res, rest)))
        | Manager_operation _, Cons_and_result (_, _, _) ->
            Ok (Contents_and_result_list (Cons_and_result (op, res, rest)))
        | _ -> Error "cannot decode ill-formed combined operation result")
  in
  conv_with_guard to_list of_list

let contents_and_result_list_encoding =
  contents_and_result_conv_with_guard
    (Variable.list contents_and_result_encoding)

let contents_and_result_list_encoding_with_legacy_attestation_name =
  contents_and_result_conv_with_guard
    (Variable.list contents_and_result_encoding_with_legacy_attestation_name)

type 'kind operation_metadata = {contents : 'kind contents_result_list}

type packed_operation_metadata =
  | Operation_metadata : 'kind operation_metadata -> packed_operation_metadata
  | No_operation_metadata : packed_operation_metadata

let operation_metadata_encoding =
  def "operation.alpha.result"
  @@ union
       [
         case
           (Tag 0)
           ~title:"Operation_metadata"
           contents_result_list_encoding
           (function
             | Operation_metadata {contents} ->
                 Some (Contents_result_list contents)
             | _ -> None)
           (fun (Contents_result_list contents) ->
             Operation_metadata {contents});
         case
           (Tag 1)
           ~title:"No_operation_metadata"
           empty
           (function No_operation_metadata -> Some () | _ -> None)
           (fun () -> No_operation_metadata);
       ]

let operation_metadata_encoding_with_legacy_attestation_name =
  def "operation_with_legacy_attestation_name.alpha.result"
  @@ union
       [
         case
           (Tag 0)
           ~title:"Operation_metadata"
           contents_result_list_encoding_with_legacy_attestation_name
           (function
             | Operation_metadata {contents} ->
                 Some (Contents_result_list contents)
             | _ -> None)
           (fun (Contents_result_list contents) ->
             Operation_metadata {contents});
         case
           (Tag 1)
           ~title:"No_operation_metadata"
           empty
           (function No_operation_metadata -> Some () | _ -> None)
           (fun () -> No_operation_metadata);
       ]

let kind_equal :
    type kind kind2.
    kind contents -> kind2 contents_result -> (kind, kind2) eq option =
 fun op res ->
  match (op, res) with
  | Endorsement _, Endorsement_result _ -> Some Eq
  | Endorsement _, _ -> None
  | Preendorsement _, Preendorsement_result _ -> Some Eq
  | Preendorsement _, _ -> None
  | Dal_attestation _, Dal_attestation_result _ -> Some Eq
  | Dal_attestation _, _ -> None
  | Seed_nonce_revelation _, Seed_nonce_revelation_result _ -> Some Eq
  | Seed_nonce_revelation _, _ -> None
  | Vdf_revelation _, Vdf_revelation_result _ -> Some Eq
  | Vdf_revelation _, _ -> None
  | Double_preendorsement_evidence _, Double_preendorsement_evidence_result _ ->
      Some Eq
  | Double_preendorsement_evidence _, _ -> None
  | Double_endorsement_evidence _, Double_endorsement_evidence_result _ ->
      Some Eq
  | Double_endorsement_evidence _, _ -> None
  | Double_baking_evidence _, Double_baking_evidence_result _ -> Some Eq
  | Double_baking_evidence _, _ -> None
  | Activate_account _, Activate_account_result _ -> Some Eq
  | Activate_account _, _ -> None
  | Proposals _, Proposals_result -> Some Eq
  | Proposals _, _ -> None
  | Ballot _, Ballot_result -> Some Eq
  | Ballot _, _ -> None
  | Drain_delegate _, Drain_delegate_result _ -> Some Eq
  | Drain_delegate _, _ -> None
  | Failing_noop _, _ ->
      (* the Failing_noop operation always fails and can't have result *)
      None
  | ( Manager_operation {operation = Reveal _; _},
      Manager_operation_result {operation_result = Applied (Reveal_result _); _}
    ) ->
      Some Eq
  | ( Manager_operation {operation = Reveal _; _},
      Manager_operation_result
        {operation_result = Backtracked (Reveal_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Reveal _; _},
      Manager_operation_result
        {
          operation_result = Failed (Alpha_context.Kind.Reveal_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Reveal _; _},
      Manager_operation_result
        {operation_result = Skipped Alpha_context.Kind.Reveal_manager_kind; _} )
    ->
      Some Eq
  | Manager_operation {operation = Reveal _; _}, _ -> None
  | ( Manager_operation {operation = Transaction _; _},
      Manager_operation_result
        {operation_result = Applied (Transaction_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Transaction _; _},
      Manager_operation_result
        {operation_result = Backtracked (Transaction_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Transaction _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Transaction_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Transaction _; _},
      Manager_operation_result
        {
          operation_result = Skipped Alpha_context.Kind.Transaction_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Transaction _; _}, _ -> None
  | ( Manager_operation {operation = Origination _; _},
      Manager_operation_result
        {operation_result = Applied (Origination_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Origination _; _},
      Manager_operation_result
        {operation_result = Backtracked (Origination_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Origination _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Origination_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Origination _; _},
      Manager_operation_result
        {
          operation_result = Skipped Alpha_context.Kind.Origination_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Origination _; _}, _ -> None
  | ( Manager_operation {operation = Delegation _; _},
      Manager_operation_result
        {operation_result = Applied (Delegation_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Delegation _; _},
      Manager_operation_result
        {operation_result = Backtracked (Delegation_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Delegation _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Delegation_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Delegation _; _},
      Manager_operation_result
        {
          operation_result = Skipped Alpha_context.Kind.Delegation_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Delegation _; _}, _ -> None
  | ( Manager_operation {operation = Update_consensus_key _; _},
      Manager_operation_result
        {operation_result = Applied (Update_consensus_key_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Update_consensus_key _; _},
      Manager_operation_result
        {operation_result = Backtracked (Update_consensus_key_result _, _); _} )
    ->
      Some Eq
  | ( Manager_operation {operation = Update_consensus_key _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Update_consensus_key_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Update_consensus_key _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Update_consensus_key_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Update_consensus_key _; _}, _ -> None
  | ( Manager_operation {operation = Register_global_constant _; _},
      Manager_operation_result
        {operation_result = Applied (Register_global_constant_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Register_global_constant _; _},
      Manager_operation_result
        {
          operation_result = Backtracked (Register_global_constant_result _, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Register_global_constant _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Register_global_constant_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Register_global_constant _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Register_global_constant_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Register_global_constant _; _}, _ -> None
  | ( Manager_operation {operation = Increase_paid_storage _; _},
      Manager_operation_result
        {operation_result = Applied (Increase_paid_storage_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Increase_paid_storage _; _},
      Manager_operation_result
        {operation_result = Backtracked (Increase_paid_storage_result _, _); _}
    ) ->
      Some Eq
  | ( Manager_operation {operation = Increase_paid_storage _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Increase_paid_storage_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Increase_paid_storage _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Increase_paid_storage_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Increase_paid_storage _; _}, _ -> None
  | ( Manager_operation {operation = Sc_rollup_recover_bond _; _},
      Manager_operation_result
        {operation_result = Applied (Sc_rollup_recover_bond_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_recover_bond _; _},
      Manager_operation_result
        {operation_result = Backtracked (Sc_rollup_recover_bond_result _, _); _}
    ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_recover_bond _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Sc_rollup_recover_bond_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_recover_bond _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Sc_rollup_recover_bond_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Sc_rollup_recover_bond _; _}, _ -> None
  | ( Manager_operation {operation = Transfer_ticket _; _},
      Manager_operation_result
        {operation_result = Applied (Transfer_ticket_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Transfer_ticket _; _},
      Manager_operation_result
        {operation_result = Backtracked (Transfer_ticket_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Transfer_ticket _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Transfer_ticket_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Transfer_ticket _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Transfer_ticket_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Transfer_ticket _; _}, _ -> None
  | ( Manager_operation {operation = Dal_publish_slot_header _; _},
      Manager_operation_result
        {operation_result = Applied (Dal_publish_slot_header_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Dal_publish_slot_header _; _},
      Manager_operation_result
        {
          operation_result = Backtracked (Dal_publish_slot_header_result _, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Dal_publish_slot_header _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Dal_publish_slot_header_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Dal_publish_slot_header _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Dal_publish_slot_header_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Dal_publish_slot_header _; _}, _ -> None
  | ( Manager_operation {operation = Sc_rollup_originate _; _},
      Manager_operation_result
        {operation_result = Applied (Sc_rollup_originate_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_originate _; _},
      Manager_operation_result
        {operation_result = Backtracked (Sc_rollup_originate_result _, _); _} )
    ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_originate _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Sc_rollup_originate_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_originate _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Sc_rollup_originate_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Sc_rollup_originate _; _}, _ -> None
  | ( Manager_operation {operation = Sc_rollup_add_messages _; _},
      Manager_operation_result
        {operation_result = Applied (Sc_rollup_add_messages_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_add_messages _; _},
      Manager_operation_result
        {operation_result = Backtracked (Sc_rollup_add_messages_result _, _); _}
    ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_add_messages _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Sc_rollup_add_messages_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_add_messages _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Sc_rollup_add_messages_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Sc_rollup_add_messages _; _}, _ -> None
  | ( Manager_operation {operation = Sc_rollup_cement _; _},
      Manager_operation_result
        {operation_result = Applied (Sc_rollup_cement_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_cement _; _},
      Manager_operation_result
        {operation_result = Backtracked (Sc_rollup_cement_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_cement _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Sc_rollup_cement_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_cement _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Sc_rollup_cement_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Sc_rollup_cement _; _}, _ -> None
  | ( Manager_operation {operation = Sc_rollup_publish _; _},
      Manager_operation_result
        {operation_result = Applied (Sc_rollup_publish_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_publish _; _},
      Manager_operation_result
        {operation_result = Backtracked (Sc_rollup_publish_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_publish _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Sc_rollup_publish_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_publish _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Sc_rollup_publish_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Sc_rollup_publish _; _}, _ -> None
  | ( Manager_operation {operation = Sc_rollup_refute _; _},
      Manager_operation_result
        {operation_result = Applied (Sc_rollup_refute_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_refute _; _},
      Manager_operation_result
        {operation_result = Backtracked (Sc_rollup_refute_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_refute _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Sc_rollup_refute_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_refute _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Sc_rollup_refute_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Sc_rollup_refute _; _}, _ -> None
  | ( Manager_operation {operation = Sc_rollup_timeout _; _},
      Manager_operation_result
        {operation_result = Applied (Sc_rollup_timeout_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_timeout _; _},
      Manager_operation_result
        {operation_result = Backtracked (Sc_rollup_timeout_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_timeout _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Sc_rollup_timeout_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_timeout _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Sc_rollup_timeout_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Sc_rollup_timeout _; _}, _ -> None
  | ( Manager_operation {operation = Sc_rollup_execute_outbox_message _; _},
      Manager_operation_result
        {
          operation_result = Applied (Sc_rollup_execute_outbox_message_result _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_execute_outbox_message _; _},
      Manager_operation_result
        {
          operation_result =
            Backtracked (Sc_rollup_execute_outbox_message_result _, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_execute_outbox_message _; _},
      Manager_operation_result
        {
          operation_result =
            Failed
              ( Alpha_context.Kind.Sc_rollup_execute_outbox_message_manager_kind,
                _ );
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Sc_rollup_execute_outbox_message _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped
              Alpha_context.Kind.Sc_rollup_execute_outbox_message_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Sc_rollup_execute_outbox_message _; _}, _ ->
      None
  | ( Manager_operation {operation = Zk_rollup_origination _; _},
      Manager_operation_result
        {operation_result = Applied (Zk_rollup_origination_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Zk_rollup_origination _; _},
      Manager_operation_result
        {operation_result = Backtracked (Zk_rollup_origination_result _, _); _}
    ) ->
      Some Eq
  | ( Manager_operation {operation = Zk_rollup_origination _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Zk_rollup_origination_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Zk_rollup_origination _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Zk_rollup_origination_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Zk_rollup_origination _; _}, _ -> None
  | ( Manager_operation {operation = Zk_rollup_publish _; _},
      Manager_operation_result
        {operation_result = Applied (Zk_rollup_publish_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Zk_rollup_publish _; _},
      Manager_operation_result
        {operation_result = Backtracked (Zk_rollup_publish_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Zk_rollup_publish _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Zk_rollup_publish_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Zk_rollup_publish _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Zk_rollup_publish_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Zk_rollup_publish _; _}, _ -> None
  | ( Manager_operation {operation = Zk_rollup_update _; _},
      Manager_operation_result
        {operation_result = Applied (Zk_rollup_update_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Zk_rollup_update _; _},
      Manager_operation_result
        {operation_result = Backtracked (Zk_rollup_update_result _, _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Zk_rollup_update _; _},
      Manager_operation_result
        {
          operation_result =
            Failed (Alpha_context.Kind.Zk_rollup_update_manager_kind, _);
          _;
        } ) ->
      Some Eq
  | ( Manager_operation {operation = Zk_rollup_update _; _},
      Manager_operation_result
        {
          operation_result =
            Skipped Alpha_context.Kind.Zk_rollup_update_manager_kind;
          _;
        } ) ->
      Some Eq
  | Manager_operation {operation = Zk_rollup_update _; _}, _ -> None

let rec kind_equal_list :
    type kind kind2.
    kind contents_list -> kind2 contents_result_list -> (kind, kind2) eq option
    =
 fun contents res ->
  match (contents, res) with
  | Single op, Single_result res -> (
      match kind_equal op res with None -> None | Some Eq -> Some Eq)
  | Cons (op, ops), Cons_result (res, ress) -> (
      match kind_equal op res with
      | None -> None
      | Some Eq -> (
          match kind_equal_list ops ress with
          | None -> None
          | Some Eq -> Some Eq))
  | _ -> None

let rec pack_contents_list :
    type kind.
    kind contents_list ->
    kind contents_result_list ->
    kind contents_and_result_list =
 fun contents res ->
  match (contents, res) with
  | Single op, Single_result res -> Single_and_result (op, res)
  | Cons (op, ops), Cons_result (res, ress) ->
      Cons_and_result (op, res, pack_contents_list ops ress)
  | ( Single (Manager_operation _),
      Cons_result (Manager_operation_result _, Single_result _) ) ->
      .
  | ( Cons (_, _),
      Single_result (Manager_operation_result {operation_result = Failed _; _})
    ) ->
      .
  | ( Cons (_, _),
      Single_result (Manager_operation_result {operation_result = Skipped _; _})
    ) ->
      .
  | ( Cons (_, _),
      Single_result (Manager_operation_result {operation_result = Applied _; _})
    ) ->
      .
  | ( Cons (_, _),
      Single_result
        (Manager_operation_result {operation_result = Backtracked _; _}) ) ->
      .
  | Single _, Cons_result _ -> .

let rec unpack_contents_list :
    type kind.
    kind contents_and_result_list ->
    kind contents_list * kind contents_result_list = function
  | Single_and_result (op, res) -> (Single op, Single_result res)
  | Cons_and_result (op, res, rest) ->
      let ops, ress = unpack_contents_list rest in
      (Cons (op, ops), Cons_result (res, ress))

let rec to_list = function
  | Contents_result_list (Single_result o) -> [Contents_result o]
  | Contents_result_list (Cons_result (o, os)) ->
      Contents_result o :: to_list (Contents_result_list os)

let operation_data_and_metadata_encoding =
  def "operation.alpha.operation_with_metadata"
  @@ union
       [
         case
           (Tag 0)
           ~title:"Operation_with_metadata"
           (obj2
              (req "contents" (dynamic_size contents_and_result_list_encoding))
              (opt "signature" Signature.encoding))
           (function
             | Operation_data _, No_operation_metadata -> None
             | Operation_data op, Operation_metadata res -> (
                 match kind_equal_list op.contents res.contents with
                 | None ->
                     Pervasives.failwith
                       "cannot decode inconsistent combined operation result"
                 | Some Eq ->
                     Some
                       ( Contents_and_result_list
                           (pack_contents_list op.contents res.contents),
                         op.signature )))
           (fun (Contents_and_result_list contents, signature) ->
             let op_contents, res_contents = unpack_contents_list contents in
             ( Operation_data {contents = op_contents; signature},
               Operation_metadata {contents = res_contents} ));
         case
           (Tag 1)
           ~title:"Operation_without_metadata"
           (obj2
              (req "contents" (dynamic_size Operation.contents_list_encoding))
              (opt "signature" Signature.encoding))
           (function
             | Operation_data op, No_operation_metadata ->
                 Some (Contents_list op.contents, op.signature)
             | Operation_data _, Operation_metadata _ -> None)
           (fun (Contents_list contents, signature) ->
             (Operation_data {contents; signature}, No_operation_metadata));
       ]

let operation_data_and_metadata_encoding_with_legacy_attestation_name =
  def "operation_with_legacy_attestation_name.alpha.operation_with_metadata"
  @@ union
       [
         case
           (Tag 0)
           ~title:"Operation_with_metadata"
           (obj2
              (req
                 "contents"
                 (dynamic_size
                    contents_and_result_list_encoding_with_legacy_attestation_name))
              (opt "signature" Signature.encoding))
           (function
             | Operation_data _, No_operation_metadata -> None
             | Operation_data op, Operation_metadata res -> (
                 match kind_equal_list op.contents res.contents with
                 | None ->
                     Pervasives.failwith
                       "cannot decode inconsistent combined operation result"
                 | Some Eq ->
                     Some
                       ( Contents_and_result_list
                           (pack_contents_list op.contents res.contents),
                         op.signature )))
           (fun (Contents_and_result_list contents, signature) ->
             let op_contents, res_contents = unpack_contents_list contents in
             ( Operation_data {contents = op_contents; signature},
               Operation_metadata {contents = res_contents} ));
         case
           (Tag 1)
           ~title:"Operation_without_metadata"
           (obj2
              (req
                 "contents"
                 (dynamic_size
                    Operation
                    .contents_list_encoding_with_legacy_attestation_name))
              (opt "signature" Signature.encoding))
           (function
             | Operation_data op, No_operation_metadata ->
                 Some (Contents_list op.contents, op.signature)
             | Operation_data _, Operation_metadata _ -> None)
           (fun (Contents_list contents, signature) ->
             (Operation_data {contents; signature}, No_operation_metadata));
       ]

type block_metadata = {
  proposer : Consensus_key.t;
  baker : Consensus_key.t;
  level_info : Level.t;
  voting_period_info : Voting_period.info;
  nonce_hash : Nonce_hash.t option;
  consumed_gas : Gas.Arith.fp;
  deactivated : Signature.Public_key_hash.t list;
  balance_updates : Receipt.balance_updates;
  liquidity_baking_toggle_ema : Toggle_votes.Liquidity_baking_toggle_EMA.t;
  adaptive_inflation_toggle_ema : Toggle_votes.Adaptive_inflation_launch_EMA.t;
  implicit_operations_results : packed_successful_manager_operation_result list;
  dal_attestation : Dal.Attestation.t option;
}

let block_metadata_encoding =
  let open Data_encoding in
  def "block_header.alpha.metadata"
  @@ conv
       (fun {
              proposer =
                {delegate = proposer; consensus_pkh = proposer_active_key};
              baker = {delegate = baker; consensus_pkh = baker_active_key};
              level_info;
              voting_period_info;
              nonce_hash;
              consumed_gas;
              deactivated;
              balance_updates;
              liquidity_baking_toggle_ema;
              adaptive_inflation_toggle_ema;
              implicit_operations_results;
              dal_attestation;
            } ->
         ( ( proposer,
             baker,
             level_info,
             voting_period_info,
             nonce_hash,
             deactivated,
             balance_updates,
             liquidity_baking_toggle_ema,
             adaptive_inflation_toggle_ema,
             implicit_operations_results ),
           (proposer_active_key, baker_active_key, consumed_gas, dal_attestation)
         ))
       (fun ( ( proposer,
                baker,
                level_info,
                voting_period_info,
                nonce_hash,
                deactivated,
                balance_updates,
                liquidity_baking_toggle_ema,
                adaptive_inflation_toggle_ema,
                implicit_operations_results ),
              ( proposer_active_key,
                baker_active_key,
                consumed_gas,
                dal_attestation ) ) ->
         {
           proposer = {delegate = proposer; consensus_pkh = proposer_active_key};
           baker = {delegate = baker; consensus_pkh = baker_active_key};
           level_info;
           voting_period_info;
           nonce_hash;
           consumed_gas;
           deactivated;
           balance_updates;
           liquidity_baking_toggle_ema;
           adaptive_inflation_toggle_ema;
           implicit_operations_results;
           dal_attestation;
         })
       (merge_objs
          (obj10
             (req "proposer" Signature.Public_key_hash.encoding)
             (req "baker" Signature.Public_key_hash.encoding)
             (req "level_info" Level.encoding)
             (req "voting_period_info" Voting_period.info_encoding)
             (req "nonce_hash" (option Nonce_hash.encoding))
             (req "deactivated" (list Signature.Public_key_hash.encoding))
             (dft "balance_updates" Receipt.balance_updates_encoding [])
             (req
                "liquidity_baking_toggle_ema"
                Toggle_votes.Liquidity_baking_toggle_EMA.encoding)
             (req
                "adaptive_inflation_toggle_ema"
                Toggle_votes.Adaptive_inflation_launch_EMA.encoding)
             (req
                "implicit_operations_results"
                (list successful_manager_operation_result_encoding)))
          (obj4
             (req "proposer_consensus_key" Signature.Public_key_hash.encoding)
             (req "baker_consensus_key" Signature.Public_key_hash.encoding)
             (req "consumed_milligas" Gas.Arith.n_fp_encoding)
             (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3119
                This varopt is here while the DAL is behind a feature
                flag. This should be replaced by a required field once
                the feature flag will be activated. *)
             (varopt "dal_attestation" Dal.Attestation.encoding)))
