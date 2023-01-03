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

open Alpha_context
open Data_encoding
open Apply_operation_result

type 'kind internal_operation_contents =
  | Transaction : {
      amount : Tez.tez;
      parameters : Script.lazy_expr;
      entrypoint : Entrypoint.t;
      destination : Destination.t;
    }
      -> Kind.transaction internal_operation_contents
  | Origination : {
      delegate : Signature.Public_key_hash.t option;
      script : Script.t;
      credit : Tez.tez;
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

type packed_internal_operation_contents =
  | Internal_operation_contents :
      'kind internal_operation_contents
      -> packed_internal_operation_contents

type 'kind internal_operation = {
  source : Destination.t;
  operation : 'kind internal_operation_contents;
  nonce : int;
}

type packed_internal_operation =
  | Internal_operation : 'kind internal_operation -> packed_internal_operation

let internal_operation (type kind)
    ({source; operation; nonce} : kind Script_typed_ir.internal_operation) :
    kind internal_operation =
  let operation : kind internal_operation_contents =
    match operation with
    | Transaction_to_implicit {destination; amount} ->
        Transaction
          {
            destination = Contract (Implicit destination);
            amount;
            entrypoint = Entrypoint.default;
            parameters = Script.unit_parameter;
          }
    | Transaction_to_implicit_with_ticket
        {destination; unparsed_ticket; amount; ticket_ty = _; ticket = _} ->
        Transaction
          {
            destination = Contract (Implicit destination);
            amount;
            entrypoint = Entrypoint.default;
            parameters = unparsed_ticket;
          }
    | Transaction_to_smart_contract
        {destination; amount; entrypoint; unparsed_parameters; _} ->
        Transaction
          {
            destination = Contract (Originated destination);
            amount;
            entrypoint;
            parameters = Script.lazy_expr unparsed_parameters;
          }
    | Transaction_to_tx_rollup {destination; unparsed_parameters; _} ->
        Transaction
          {
            destination = Tx_rollup destination;
            (* Dummy amount used for the external untyped view of internal transactions *)
            amount = Tez.zero;
            entrypoint = Entrypoint.deposit;
            parameters = Script.lazy_expr unparsed_parameters;
          }
    | Transaction_to_sc_rollup {destination; entrypoint; unparsed_parameters; _}
      ->
        Transaction
          {
            destination = Sc_rollup destination;
            amount = Tez.zero;
            entrypoint;
            parameters = Script.lazy_expr unparsed_parameters;
          }
    | Event {ty; tag; unparsed_data} -> Event {ty; tag; payload = unparsed_data}
    | Transaction_to_zk_rollup {destination; unparsed_parameters; _} ->
        Transaction
          {
            destination = Zk_rollup destination;
            amount = Tez.zero;
            entrypoint = Entrypoint.deposit;
            parameters = Script.lazy_expr unparsed_parameters;
          }
    | Origination {delegate; code; unparsed_storage; credit; _} ->
        let script =
          {
            Script.code = Script.lazy_expr code;
            storage = Script.lazy_expr unparsed_storage;
          }
        in
        Origination {delegate; script; credit}
    | Delegation delegate -> Delegation delegate
  in
  {source; operation; nonce}

let packed_internal_operation (Script_typed_ir.Internal_operation op) =
  Internal_operation (internal_operation op)

let packed_internal_operations = List.map packed_internal_operation

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
    }
  | Transaction_to_tx_rollup_result of {
      ticket_hash : Ticket_hash.t;
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
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

type successful_origination_result = {
  lazy_storage_diff : Lazy_storage.diffs option;
  balance_updates : Receipt.balance_updates;
  originated_contracts : Contract_hash.t list;
  consumed_gas : Gas.Arith.fp;
  storage_size : Z.t;
  paid_storage_size_diff : Z.t;
}

(** Result of applying an internal operation. *)
type _ successful_internal_operation_result =
  | ITransaction_result :
      successful_transaction_result
      -> Kind.transaction successful_internal_operation_result
  | IOrigination_result :
      successful_origination_result
      -> Kind.origination successful_internal_operation_result
  | IDelegation_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.delegation successful_internal_operation_result
  | IEvent_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.event successful_internal_operation_result

type packed_successful_internal_operation_result =
  | Successful_internal_operation_result :
      'kind successful_internal_operation_result
      -> packed_successful_internal_operation_result

type 'kind internal_operation_result =
  ( 'kind,
    'kind Kind.manager,
    'kind successful_internal_operation_result )
  operation_result

type packed_internal_operation_result =
  | Internal_operation_result :
      'kind internal_operation * 'kind internal_operation_result
      -> packed_internal_operation_result

let pack_internal_operation_result (type kind)
    (internal_op : kind Script_typed_ir.internal_operation)
    (manager_op : kind internal_operation_result) =
  let internal_op = internal_operation internal_op in
  Internal_operation_result (internal_op, manager_op)

type 'kind iselect =
  packed_internal_operation_result ->
  ('kind internal_operation * 'kind internal_operation_result) option

module Internal_operation = struct
  open Data_encoding

  type 'kind case =
    | MCase : {
        tag : int;
        name : string;
        encoding : 'a Data_encoding.t;
        iselect : 'kind iselect;
        select :
          packed_internal_operation_contents ->
          'kind internal_operation_contents option;
        proj : 'kind internal_operation_contents -> 'a;
        inj : 'a -> 'kind internal_operation_contents;
      }
        -> 'kind case

  let transaction_contract_variant_cases =
    union
      [
        case
          ~title:"To_contract"
          (Tag 0)
          (obj9
             (opt "storage" Script.expr_encoding)
             (dft "balance_updates" Receipt.balance_updates_encoding [])
             (dft "ticket_receipt" Ticket_receipt.encoding [])
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
          ~title:"To_tx_rollup"
          (Tag 1)
          (obj4
             (dft "balance_updates" Receipt.balance_updates_encoding [])
             (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
             (req "ticket_hash" Ticket_hash.encoding)
             (req "paid_storage_size_diff" n))
          (function
            | Transaction_to_tx_rollup_result
                {
                  balance_updates;
                  consumed_gas;
                  ticket_hash;
                  paid_storage_size_diff;
                } ->
                Some
                  ( balance_updates,
                    consumed_gas,
                    ticket_hash,
                    paid_storage_size_diff )
            | _ -> None)
          (fun ( balance_updates,
                 consumed_gas,
                 ticket_hash,
                 paid_storage_size_diff ) ->
            Transaction_to_tx_rollup_result
              {
                balance_updates;
                consumed_gas;
                ticket_hash;
                paid_storage_size_diff;
              });
        case
          ~title:"To_smart_rollup"
          (Tag 2)
          (obj2
             (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
             (req "ticket_receipt" Ticket_receipt.encoding))
          (function
            | Transaction_to_sc_rollup_result {consumed_gas; ticket_receipt} ->
                Some (consumed_gas, ticket_receipt)
            | _ -> None)
          (function
            | consumed_gas, ticket_receipt ->
                Transaction_to_sc_rollup_result {consumed_gas; ticket_receipt});
      ]

  let transaction_case =
    MCase
      {
        (* This value should be changed with care: maybe receipts are read by
           external tools such as indexers. *)
        tag = 1;
        name = "transaction";
        encoding =
          obj3
            (req "amount" Tez.encoding)
            (req "destination" Destination.encoding)
            (opt
               "parameters"
               (obj2
                  (req "entrypoint" Entrypoint.smart_encoding)
                  (req "value" Script.lazy_expr_encoding)));
        iselect : Kind.transaction iselect =
          (function
          | Internal_operation_result
              (({operation = Transaction _; _} as op), res) ->
              Some (op, res)
          | _ -> None);
        select =
          (function
          | Internal_operation_contents (Transaction _ as op) -> Some op
          | _ -> None);
        proj =
          (function
          | Transaction {amount; destination; parameters; entrypoint} ->
              let parameters =
                if
                  Script_repr.is_unit_parameter parameters
                  && Entrypoint.is_default entrypoint
                then None
                else Some (entrypoint, parameters)
              in
              (amount, destination, parameters));
        inj =
          (fun (amount, destination, parameters) ->
            let entrypoint, parameters =
              match parameters with
              | None -> (Entrypoint.default, Script.unit_parameter)
              | Some (entrypoint, value) -> (entrypoint, value)
            in
            Transaction {amount; destination; parameters; entrypoint});
      }

  let origination_case =
    MCase
      {
        (* This value should be changed with care: maybe receipts are read by
           external tools such as indexers. *)
        tag = 2;
        name = "origination";
        encoding =
          obj3
            (req "balance" Tez.encoding)
            (opt "delegate" Signature.Public_key_hash.encoding)
            (req "script" Script.encoding);
        iselect : Kind.origination iselect =
          (function
          | Internal_operation_result
              (({operation = Origination _; _} as op), res) ->
              Some (op, res)
          | _ -> None);
        select =
          (function
          | Internal_operation_contents (Origination _ as op) -> Some op
          | _ -> None);
        proj =
          (function
          | Origination {credit; delegate; script} -> (credit, delegate, script));
        inj =
          (fun (credit, delegate, script) ->
            Origination {credit; delegate; script});
      }

  let delegation_case =
    MCase
      {
        (* This value should be changed with care: maybe receipts are read by
           external tools such as indexers. *)
        tag = 3;
        name = "delegation";
        encoding = obj1 (opt "delegate" Signature.Public_key_hash.encoding);
        iselect : Kind.delegation iselect =
          (function
          | Internal_operation_result
              (({operation = Delegation _; _} as op), res) ->
              Some (op, res)
          | _ -> None);
        select =
          (function
          | Internal_operation_contents (Delegation _ as op) -> Some op
          | _ -> None);
        proj = (function Delegation key -> key);
        inj = (fun key -> Delegation key);
      }

  let event_case =
    MCase
      {
        (* This value should be changed with care: maybe receipts are read by
           external tools such as indexers. *)
        tag = 4;
        name = "event";
        encoding =
          obj3
            (req "type" Script.expr_encoding)
            (opt "tag" Entrypoint.smart_encoding)
            (opt "payload" Script.expr_encoding);
        iselect : Kind.event iselect =
          (function
          | Internal_operation_result (({operation = Event _; _} as op), res) ->
              Some (op, res)
          | _ -> None);
        select =
          (function
          | Internal_operation_contents (Event _ as op) -> Some op | _ -> None);
        proj =
          (function
          | Event {ty; tag; payload} ->
              let tag = if Entrypoint.is_default tag then None else Some tag in
              let payload =
                if Script_repr.is_unit payload then None else Some payload
              in
              (ty, tag, payload));
        inj =
          (fun (ty, tag, payload) ->
            let tag = Option.value ~default:Entrypoint.default tag in
            let payload = Option.value ~default:Script_repr.unit payload in
            Event {ty; tag; payload});
      }

  let case tag name args proj inj =
    case
      tag
      ~title:(String.capitalize_ascii name)
      (merge_objs (obj1 (req "kind" (constant name))) args)
      (fun x -> match proj x with None -> None | Some x -> Some ((), x))
      (fun ((), x) -> inj x)

  let encoding =
    let make (MCase {tag; name; encoding; iselect = _; select; proj; inj}) =
      case
        (Tag tag)
        name
        encoding
        (fun o -> match select o with None -> None | Some o -> Some (proj o))
        (fun x -> Internal_operation_contents (inj x))
    in
    union
      ~tag_size:`Uint8
      [
        make transaction_case;
        make origination_case;
        make delegation_case;
        make event_case;
      ]
end

let internal_operation_encoding : packed_internal_operation Data_encoding.t =
  def "apply_internal_results.alpha.operation_result"
  @@ conv
       (fun (Internal_operation {source; operation; nonce}) ->
         ((source, nonce), Internal_operation_contents operation))
       (fun ((source, nonce), Internal_operation_contents operation) ->
         Internal_operation {source; operation; nonce})
       (merge_objs
          (obj2 (req "source" Destination.encoding) (req "nonce" uint16))
          Internal_operation.encoding)

module Internal_operation_result = struct
  type 'kind case =
    | MCase : {
        op_case : 'kind Internal_operation.case;
        encoding : 'a Data_encoding.t;
        kind : 'kind Kind.manager;
        select :
          packed_successful_internal_operation_result ->
          'kind successful_internal_operation_result option;
        proj : 'kind successful_internal_operation_result -> 'a;
        inj : 'a -> 'kind successful_internal_operation_result;
        t : 'kind internal_operation_result Data_encoding.t;
      }
        -> 'kind case

  let make ~op_case ~encoding ~kind ~select ~proj ~inj =
    let (Internal_operation.MCase {name; _}) = op_case in
    let t =
      def (Format.asprintf "operation.alpha.internal_operation_result.%s" name)
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
                     match select (Successful_internal_operation_result o) with
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
                     match select (Successful_internal_operation_result o) with
                     | None -> None
                     | Some o -> Some (((), errs), proj o)))
               (fun (((), errs), x) -> Backtracked (inj x, errs));
           ]
    in
    MCase {op_case; encoding; kind; select; proj; inj; t}

  let transaction_case =
    make
      ~op_case:Internal_operation.transaction_case
      ~encoding:Internal_operation.transaction_contract_variant_cases
      ~select:(function
        | Successful_internal_operation_result (ITransaction_result _ as op) ->
            Some op
        | _ -> None)
      ~kind:Kind.Transaction_manager_kind
      ~proj:(function ITransaction_result x -> x)
      ~inj:(fun x -> ITransaction_result x)

  let origination_case =
    make
      ~op_case:Internal_operation.origination_case
      ~encoding:
        (obj6
           (dft "balance_updates" Receipt.balance_updates_encoding [])
           (dft "originated_contracts" (list Contract.originated_encoding) [])
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
           (dft "storage_size" z Z.zero)
           (dft "paid_storage_size_diff" z Z.zero)
           (opt "lazy_storage_diff" Lazy_storage.encoding))
      ~select:(function
        | Successful_internal_operation_result (IOrigination_result _ as op) ->
            Some op
        | _ -> None)
      ~proj:(function
        | IOrigination_result
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
        IOrigination_result
          {
            lazy_storage_diff;
            balance_updates;
            originated_contracts;
            consumed_gas;
            storage_size;
            paid_storage_size_diff;
          })

  let delegation_case =
    make
      ~op_case:Internal_operation.delegation_case
      ~encoding:
        Data_encoding.(
          obj1 (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~select:(function
        | Successful_internal_operation_result (IDelegation_result _ as op) ->
            Some op
        | _ -> None)
      ~kind:Kind.Delegation_manager_kind
      ~proj:(function IDelegation_result {consumed_gas} -> consumed_gas)
      ~inj:(fun consumed_gas -> IDelegation_result {consumed_gas})

  let event_case =
    make
      ~op_case:Internal_operation.event_case
      ~encoding:
        Data_encoding.(
          obj1 (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~select:(function
        | Successful_internal_operation_result (IEvent_result _ as op) ->
            Some op
        | _ -> None)
      ~kind:Kind.Event_manager_kind
      ~proj:(function IEvent_result {consumed_gas} -> consumed_gas)
      ~inj:(fun consumed_gas -> IEvent_result {consumed_gas})
end

let internal_operation_result_encoding :
    packed_internal_operation_result Data_encoding.t =
  let make (type kind)
      (Internal_operation_result.MCase res_case :
        kind Internal_operation_result.case)
      (Internal_operation.MCase ires_case : kind Internal_operation.case) =
    let (Internal_operation.MCase op_case) = res_case.op_case in
    case
      (Tag op_case.tag)
      ~title:op_case.name
      (merge_objs
         (obj3
            (req "kind" (constant op_case.name))
            (req "source" Destination.encoding)
            (req "nonce" uint16))
         (merge_objs ires_case.encoding (obj1 (req "result" res_case.t))))
      (fun op ->
        match ires_case.iselect op with
        | Some (op, res) ->
            Some (((), op.source, op.nonce), (ires_case.proj op.operation, res))
        | None -> None)
      (fun (((), source, nonce), (op, res)) ->
        let op = {source; operation = ires_case.inj op; nonce} in
        Internal_operation_result (op, res))
  in
  def "apply_internal_results.alpha.operation_result"
  @@ union
       [
         make
           Internal_operation_result.transaction_case
           Internal_operation.transaction_case;
         make
           Internal_operation_result.origination_case
           Internal_operation.origination_case;
         make
           Internal_operation_result.delegation_case
           Internal_operation.delegation_case;
         make Internal_operation_result.event_case Internal_operation.event_case;
       ]
