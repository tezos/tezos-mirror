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

open Alpha_context
open Data_encoding

let error_encoding =
  def
    "error"
    ~description:
      "The full list of RPC errors would be too long to include.\n\
       It is available at RPC `/errors` (GET).\n\
       Errors specific to protocol Alpha have an id that starts with \
       `proto.alpha`."
  @@ splitted
       ~json:
         (conv
            (fun err ->
              Data_encoding.Json.construct Error_monad.error_encoding err)
            (fun json ->
              Data_encoding.Json.destruct Error_monad.error_encoding json)
            json)
       ~binary:Error_monad.error_encoding

let trace_encoding = make_trace_encoding error_encoding

type _ successful_manager_operation_result =
  | Reveal_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.reveal successful_manager_operation_result
  | Transaction_result : {
      storage : Script.expr option;
      lazy_storage_diff : Lazy_storage.diffs option;
      balance_updates : Receipt.balance_updates;
      originated_contracts : Contract.t list;
      consumed_gas : Gas.Arith.fp;
      storage_size : Z.t;
      paid_storage_size_diff : Z.t;
      allocated_destination_contract : bool;
    }
      -> Kind.transaction successful_manager_operation_result
  | Origination_legacy_result : {
      lazy_storage_diff : Lazy_storage.diffs option;
      balance_updates : Receipt.balance_updates;
      originated_contracts : Contract.t list;
      consumed_gas : Gas.Arith.fp;
      storage_size : Z.t;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.origination_legacy successful_manager_operation_result
  | Origination_result : {
      lazy_storage_diff : Lazy_storage.diffs option;
      balance_updates : Receipt.balance_updates;
      originated_contracts : Contract.t list;
      consumed_gas : Gas.Arith.fp;
      storage_size : Z.t;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.origination successful_manager_operation_result
  | Delegation_legacy_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.delegation_legacy successful_manager_operation_result
  | Delegation_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.delegation successful_manager_operation_result
  | Baker_registration_result : {
      balance_updates : Receipt.balance_updates;
      registered_baker : Baker_hash.t;
      consumed_gas : Gas.Arith.fp;
      storage_size : Z.t;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.baker_registration successful_manager_operation_result

type _ successful_baker_operation_result =
  | Baker_proposals_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.proposals successful_baker_operation_result
  | Baker_ballot_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.ballot successful_baker_operation_result
  | Set_baker_active_result : {
      active : bool;
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.set_baker_active successful_baker_operation_result
  | Set_baker_consensus_key_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.set_baker_consensus_key successful_baker_operation_result
  | Set_baker_pvss_key_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.set_baker_pvss_key successful_baker_operation_result

type packed_successful_manager_operation_result =
  | Successful_manager_result :
      'kind successful_manager_operation_result
      -> packed_successful_manager_operation_result

type packed_successful_baker_operation_result =
  | Successful_baker_result :
      'kind successful_baker_operation_result
      -> packed_successful_baker_operation_result

type ('kind, 'successful_result) internal_operation_result =
  | Applied of 'successful_result
  | Backtracked of 'successful_result * error trace option
  | Failed :
      'kind * error trace
      -> ('kind, 'successful_result) internal_operation_result
  | Skipped : 'kind -> ('kind, 'successful_result) internal_operation_result

type 'kind manager_operation_result =
  ( 'kind Kind.manager,
    'kind successful_manager_operation_result )
  internal_operation_result

type 'kind baker_operation_result =
  ( 'kind Kind.baker,
    'kind successful_baker_operation_result )
  internal_operation_result

and packed_internal_operation_result =
  | Internal_manager_operation_result :
      'kind internal_manager_operation * 'kind manager_operation_result
      -> packed_internal_operation_result
  | Internal_baker_operation_result :
      'kind internal_baker_operation * 'kind baker_operation_result
      -> packed_internal_operation_result

let make_internal_operation_case ~name ~result_encoding ~kind ~select ~proj
    ~inj =
  def (Format.asprintf "operation.alpha.operation_result.%s" name)
  @@ union
       ~tag_size:`Uint8
       [ case
           (Tag 0)
           ~title:"Applied"
           (merge_objs
              (obj1 (req "status" (constant "applied")))
              result_encoding)
           (fun o ->
             match o with
             | Skipped _ | Failed _ | Backtracked _ ->
                 None
             | Applied o -> (
               match select o with None -> None | Some o -> Some ((), proj o) ))
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
              result_encoding)
           (fun o ->
             match o with
             | Skipped _ | Failed _ | Applied _ ->
                 None
             | Backtracked (o, errs) -> (
               match select o with
               | None ->
                   None
               | Some o ->
                   Some (((), errs), proj o) ))
           (fun (((), errs), x) -> Backtracked (inj x, errs)) ]

module Manager_result = struct
  type 'kind case =
    | MCase : {
        op_case : 'kind Operation.Encoding.Manager_operations.case;
        kind : 'kind Kind.manager;
        iselect :
          packed_internal_operation_result ->
          ('kind internal_manager_operation * 'kind manager_operation_result)
          option;
        select :
          packed_successful_manager_operation_result ->
          'kind successful_manager_operation_result option;
        proj : 'kind successful_manager_operation_result -> 'a;
        inj : 'a -> 'kind successful_manager_operation_result;
        t : 'kind manager_operation_result Data_encoding.t;
      }
        -> 'kind case

  let make (type kind) ?(legacy = false)
      ~(op_case : kind Operation.Encoding.Manager_operations.case) ~encoding
      ~kind ~iselect ~select ~proj ~inj () =
    let (Operation.Encoding.Manager_operations.MCase {name; _}) = op_case in
    (* The name has to be different for legacy operations to avoid
       "Duplicate definition" error *)
    let name = if legacy then name ^ ".legacy" else name in
    let t : kind manager_operation_result Data_encoding.t =
      make_internal_operation_case
        ~name
        ~result_encoding:encoding
        ~kind
        ~select:(fun x -> select (Successful_manager_result x))
        ~proj
        ~inj
    in
    MCase {op_case; kind; iselect; select; proj; inj; t}

  let reveal_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.reveal_case
      ~encoding:
        Data_encoding.(
          obj2
            (dft "consumed_gas" Gas.Arith.n_integral_encoding Gas.Arith.zero)
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~iselect:(function
        | Internal_manager_operation_result
            (({operation = Reveal _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_manager_result (Reveal_result _ as op) ->
            Some op
        | _ ->
            None)
      ~kind:Kind.Reveal_manager_kind
      ~proj:(function
        | Reveal_result {consumed_gas} ->
            (Gas.Arith.ceil consumed_gas, consumed_gas))
      ~inj:(fun (consumed_gas, consumed_milligas) ->
        assert (Gas.Arith.(equal (ceil consumed_milligas) consumed_gas)) ;
        Reveal_result {consumed_gas = consumed_milligas})
      ()

  let transaction_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.transaction_case
      ~encoding:
        (obj10
           (opt "storage" Script.expr_encoding)
           (opt
              (* The field [big_map_diff] is deprecated since 008, use [lazy_storage_diff] instead.
                 Is it kept here for a transition period, for tool like indexers to update.
                 TODO(009): remove it. *)
              "big_map_diff"
              Lazy_storage.legacy_big_map_diff_encoding)
           (dft "balance_updates" Receipt.balance_updates_encoding [])
           (dft "originated_contracts" (list Contract.encoding) [])
           (dft "consumed_gas" Gas.Arith.n_integral_encoding Gas.Arith.zero)
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
           (dft "storage_size" z Z.zero)
           (dft "paid_storage_size_diff" z Z.zero)
           (dft "allocated_destination_contract" bool false)
           (opt "lazy_storage_diff" Lazy_storage.encoding))
      ~iselect:(function
        | Internal_manager_operation_result
            (({operation = Transaction _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_manager_result (Transaction_result _ as op) ->
            Some op
        | _ ->
            None)
      ~kind:Kind.Transaction_manager_kind
      ~proj:(function
        | Transaction_result
            { storage;
              lazy_storage_diff;
              balance_updates;
              originated_contracts;
              consumed_gas;
              storage_size;
              paid_storage_size_diff;
              allocated_destination_contract } ->
            ( storage,
              lazy_storage_diff,
              balance_updates,
              originated_contracts,
              Gas.Arith.ceil consumed_gas,
              consumed_gas,
              storage_size,
              paid_storage_size_diff,
              allocated_destination_contract,
              lazy_storage_diff ))
      ~inj:
        (fun ( storage,
               legacy_lazy_storage_diff,
               balance_updates,
               originated_contracts,
               consumed_gas,
               consumed_milligas,
               storage_size,
               paid_storage_size_diff,
               allocated_destination_contract,
               lazy_storage_diff ) ->
        assert (Gas.Arith.(equal (ceil consumed_milligas) consumed_gas)) ;
        let lazy_storage_diff =
          Option.either lazy_storage_diff legacy_lazy_storage_diff
        in
        Transaction_result
          {
            storage;
            lazy_storage_diff;
            balance_updates;
            originated_contracts;
            consumed_gas = consumed_milligas;
            storage_size;
            paid_storage_size_diff;
            allocated_destination_contract;
          })
      ()

  let origination_legacy_case =
    make
      ~legacy:true
      ~op_case:Operation.Encoding.Manager_operations.origination_legacy_case
      ~encoding:
        (obj8
           (opt
              (* The field [big_map_diff] is deprecated since 008, use [lazy_storage_diff] instead.
                 Is it kept here for a transition period, for tool like indexers to update.
                 TODO(009): remove it. *)
              "big_map_diff"
              Lazy_storage.legacy_big_map_diff_encoding)
           (dft "balance_updates" Receipt.balance_updates_encoding [])
           (dft "originated_contracts" (list Contract.encoding) [])
           (dft "consumed_gas" Gas.Arith.n_integral_encoding Gas.Arith.zero)
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
           (dft "storage_size" z Z.zero)
           (dft "paid_storage_size_diff" z Z.zero)
           (opt "lazy_storage_diff" Lazy_storage.encoding))
      ~iselect:(function
        | Internal_manager_operation_result
            (({operation = Origination_legacy _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_manager_result (Origination_legacy_result _ as op) ->
            Some op
        | _ ->
            None)
      ~proj:(function
        | Origination_legacy_result
            { lazy_storage_diff;
              balance_updates;
              originated_contracts;
              consumed_gas;
              storage_size;
              paid_storage_size_diff } ->
            ( lazy_storage_diff,
              balance_updates,
              originated_contracts,
              Gas.Arith.ceil consumed_gas,
              consumed_gas,
              storage_size,
              paid_storage_size_diff,
              lazy_storage_diff ))
      ~kind:Kind.Origination_legacy_manager_kind
      ~inj:
        (fun ( legacy_lazy_storage_diff,
               balance_updates,
               originated_contracts,
               consumed_gas,
               consumed_milligas,
               storage_size,
               paid_storage_size_diff,
               lazy_storage_diff ) ->
        assert (Gas.Arith.(equal (ceil consumed_milligas) consumed_gas)) ;
        let lazy_storage_diff =
          Option.either lazy_storage_diff legacy_lazy_storage_diff
        in
        Origination_legacy_result
          {
            lazy_storage_diff;
            balance_updates;
            originated_contracts;
            consumed_gas = consumed_milligas;
            storage_size;
            paid_storage_size_diff;
          })
      ()

  let origination_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.origination_case
      ~encoding:
        (obj7
           (dft "balance_updates" Receipt.balance_updates_encoding [])
           (dft "originated_contracts" (list Contract.encoding) [])
           (dft "consumed_gas" Gas.Arith.n_integral_encoding Gas.Arith.zero)
           (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero)
           (dft "storage_size" z Z.zero)
           (dft "paid_storage_size_diff" z Z.zero)
           (opt "lazy_storage_diff" Lazy_storage.encoding))
      ~iselect:(function
        | Internal_manager_operation_result
            (({operation = Origination _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_manager_result (Origination_result _ as op) ->
            Some op
        | _ ->
            None)
      ~proj:(function
        | Origination_result
            { lazy_storage_diff;
              balance_updates;
              originated_contracts;
              consumed_gas;
              storage_size;
              paid_storage_size_diff } ->
            ( balance_updates,
              originated_contracts,
              Gas.Arith.ceil consumed_gas,
              consumed_gas,
              storage_size,
              paid_storage_size_diff,
              lazy_storage_diff ))
      ~kind:Kind.Origination_manager_kind
      ~inj:
        (fun ( balance_updates,
               originated_contracts,
               consumed_gas,
               consumed_milligas,
               storage_size,
               paid_storage_size_diff,
               lazy_storage_diff ) ->
        assert (Gas.Arith.(equal (ceil consumed_milligas) consumed_gas)) ;
        Origination_result
          {
            lazy_storage_diff;
            balance_updates;
            originated_contracts;
            consumed_gas = consumed_milligas;
            storage_size;
            paid_storage_size_diff;
          })
      ()

  let delegation_legacy_case =
    make
      ~legacy:true
      ~op_case:Operation.Encoding.Manager_operations.delegation_legacy_case
      ~encoding:
        Data_encoding.(
          obj2
            (dft "consumed_gas" Gas.Arith.n_integral_encoding Gas.Arith.zero)
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~iselect:(function
        | Internal_manager_operation_result
            (({operation = Delegation_legacy _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_manager_result (Delegation_legacy_result _ as op) ->
            Some op
        | _ ->
            None)
      ~kind:Kind.Delegation_legacy_manager_kind
      ~proj:(function
        | Delegation_legacy_result {consumed_gas} ->
            (Gas.Arith.ceil consumed_gas, consumed_gas))
      ~inj:(fun (consumed_gas, consumed_milligas) ->
        assert (Gas.Arith.(equal (ceil consumed_milligas) consumed_gas)) ;
        Delegation_legacy_result {consumed_gas = consumed_milligas})
      ()

  let delegation_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.delegation_case
      ~encoding:
        Data_encoding.(
          obj2
            (dft "consumed_gas" Gas.Arith.n_integral_encoding Gas.Arith.zero)
            (dft "consumed_milligas" Gas.Arith.n_fp_encoding Gas.Arith.zero))
      ~iselect:(function
        | Internal_manager_operation_result
            (({operation = Delegation _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_manager_result (Delegation_result _ as op) ->
            Some op
        | _ ->
            None)
      ~kind:Kind.Delegation_manager_kind
      ~proj:(function
        | Delegation_result {consumed_gas} ->
            (Gas.Arith.ceil consumed_gas, consumed_gas))
      ~inj:(fun (consumed_gas, consumed_milligas) ->
        assert (Gas.Arith.(equal (ceil consumed_milligas) consumed_gas)) ;
        Delegation_result {consumed_gas = consumed_milligas})
      ()

  let baker_registration_case =
    make
      ~op_case:Operation.Encoding.Manager_operations.baker_registration_case
      ~encoding:
        (obj5
           (dft "balance_updates" Receipt.balance_updates_encoding [])
           (req "registered_baker" Baker_hash.encoding)
           (dft "consumed_gas" Gas.Arith.z_fp_encoding Gas.Arith.zero)
           (dft "storage_size" z Z.zero)
           (dft "paid_storage_size_diff" z Z.zero))
      ~iselect:(function
        | Internal_manager_operation_result
            (({operation = Baker_registration _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_manager_result (Baker_registration_result _ as op) ->
            Some op
        | _ ->
            None)
      ~proj:(function
        | Baker_registration_result
            { balance_updates;
              registered_baker;
              consumed_gas;
              storage_size;
              paid_storage_size_diff } ->
            ( balance_updates,
              registered_baker,
              consumed_gas,
              storage_size,
              paid_storage_size_diff ))
      ~kind:Kind.Baker_registration_manager_kind
      ~inj:
        (fun ( balance_updates,
               registered_baker,
               consumed_gas,
               storage_size,
               paid_storage_size_diff ) ->
        Baker_registration_result
          {
            balance_updates;
            registered_baker;
            consumed_gas;
            storage_size;
            paid_storage_size_diff;
          })
      ()

  let encoding =
    let make_manager (type kind) (MCase res_case : kind case) =
      let (Operation.Encoding.Manager_operations.MCase op_case) =
        res_case.op_case
      in
      case
        (Tag op_case.tag)
        ~title:op_case.name
        (merge_objs
           (obj3
              (req "kind" (constant op_case.name))
              (req "source" Contract.encoding)
              (req "nonce" uint16))
           (merge_objs op_case.encoding (obj1 (req "result" res_case.t))))
        (fun op ->
          match res_case.iselect op with
          | Some (op, res) ->
              Some (((), op.source, op.nonce), (op_case.proj op.operation, res))
          | None ->
              None)
        (fun (((), source, nonce), (op, res)) ->
          let op = {source; operation = op_case.inj op; nonce} in
          Internal_manager_operation_result (op, res))
    in
    def "operation.alpha.internal_manager_operation_result"
    @@ union
         [ make_manager reveal_case;
           make_manager transaction_case;
           make_manager origination_legacy_case;
           make_manager origination_case;
           make_manager delegation_legacy_case;
           make_manager delegation_case;
           make_manager baker_registration_case ]
end

module Baker_result = struct
  type 'kind case =
    | BCase : {
        op_case : 'kind Operation.Encoding.Baker_operations.case;
        kind : 'kind Kind.baker;
        iselect :
          packed_internal_operation_result ->
          ('kind internal_baker_operation * 'kind baker_operation_result)
          option;
        select :
          packed_successful_baker_operation_result ->
          'kind successful_baker_operation_result option;
        proj : 'kind successful_baker_operation_result -> 'a;
        inj : 'a -> 'kind successful_baker_operation_result;
        t : 'kind baker_operation_result Data_encoding.t;
      }
        -> 'kind case

  let make (type kind)
      ~(op_case : kind Operation.Encoding.Baker_operations.case) ~encoding
      ~kind ~iselect ~select ~proj ~inj =
    let (Operation.Encoding.Baker_operations.BCase {name; _}) = op_case in
    let t : kind baker_operation_result Data_encoding.t =
      make_internal_operation_case
        ~name
        ~result_encoding:encoding
        ~kind
        ~select:(fun x -> select (Successful_baker_result x))
        ~proj
        ~inj
    in
    BCase {op_case; kind; iselect; select; proj; inj; t}

  let baker_proposals_case =
    make
      ~op_case:Operation.Encoding.Baker_operations.baker_proposals_case
      ~encoding:
        Data_encoding.(
          obj1 (dft "consumed_gas" Gas.Arith.z_fp_encoding Gas.Arith.zero))
      ~iselect:(function
        | Internal_baker_operation_result
            (({operation = Baker_proposals _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_baker_result (Baker_proposals_result _ as op) ->
            Some op
        | _ ->
            None)
      ~proj:(function Baker_proposals_result {consumed_gas} -> consumed_gas)
      ~kind:Kind.Baker_proposals_kind
      ~inj:(fun consumed_gas -> Baker_proposals_result {consumed_gas})

  let baker_ballot_case =
    make
      ~op_case:Operation.Encoding.Baker_operations.baker_ballot_case
      ~encoding:
        Data_encoding.(
          obj1 (dft "consumed_gas" Gas.Arith.z_fp_encoding Gas.Arith.zero))
      ~iselect:(function
        | Internal_baker_operation_result
            (({operation = Baker_ballot _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_baker_result (Baker_ballot_result _ as op) ->
            Some op
        | _ ->
            None)
      ~proj:(function Baker_ballot_result {consumed_gas} -> consumed_gas)
      ~kind:Kind.Baker_ballot_kind
      ~inj:(fun consumed_gas -> Baker_ballot_result {consumed_gas})

  let set_baker_active_case =
    make
      ~op_case:Operation.Encoding.Baker_operations.set_baker_active_case
      ~encoding:
        Data_encoding.(
          obj2
            (req "active" bool)
            (dft "consumed_gas" Gas.Arith.z_fp_encoding Gas.Arith.zero))
      ~iselect:(function
        | Internal_baker_operation_result
            (({operation = Set_baker_active _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_baker_result (Set_baker_active_result _ as op) ->
            Some op
        | _ ->
            None)
      ~proj:(function
        | Set_baker_active_result {active; consumed_gas} ->
            (active, consumed_gas))
      ~kind:Kind.Set_baker_active_baker_kind
      ~inj:(fun (active, consumed_gas) ->
        Set_baker_active_result {active; consumed_gas})

  let set_baker_consensus_key_case =
    make
      ~op_case:Operation.Encoding.Baker_operations.set_baker_consensus_key_case
      ~encoding:
        Data_encoding.(
          obj1 (dft "consumed_gas" Gas.Arith.z_fp_encoding Gas.Arith.zero))
      ~iselect:(function
        | Internal_baker_operation_result
            (({operation = Set_baker_consensus_key _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_baker_result (Set_baker_consensus_key_result _ as op) ->
            Some op
        | _ ->
            None)
      ~proj:(function
        | Set_baker_consensus_key_result {consumed_gas} -> consumed_gas)
      ~kind:Kind.Set_baker_consensus_key_baker_kind
      ~inj:(fun consumed_gas -> Set_baker_consensus_key_result {consumed_gas})

  let set_baker_pvss_key_case =
    make
      ~op_case:Operation.Encoding.Baker_operations.set_baker_pvss_key_case
      ~encoding:
        Data_encoding.(
          obj1 (dft "consumed_gas" Gas.Arith.z_fp_encoding Gas.Arith.zero))
      ~iselect:(function
        | Internal_baker_operation_result
            (({operation = Set_baker_pvss_key _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)
      ~select:(function
        | Successful_baker_result (Set_baker_pvss_key_result _ as op) ->
            Some op
        | _ ->
            None)
      ~proj:(function
        | Set_baker_pvss_key_result {consumed_gas} -> consumed_gas)
      ~kind:Kind.Set_baker_pvss_key_baker_kind
      ~inj:(fun consumed_gas -> Set_baker_pvss_key_result {consumed_gas})

  let encoding =
    let make_baker (type kind) (BCase res_case : kind case) =
      let (Operation.Encoding.Baker_operations.BCase op_case) =
        res_case.op_case
      in
      case
        (Tag op_case.tag)
        ~title:op_case.name
        (merge_objs
           (obj3
              (req "kind" (constant op_case.name))
              (req "baker" Baker_hash.encoding)
              (req "nonce" uint16))
           (merge_objs op_case.encoding (obj1 (req "result" res_case.t))))
        (fun op ->
          match res_case.iselect op with
          | Some (op, res) ->
              Some (((), op.baker, op.nonce), (op_case.proj op.operation, res))
          | None ->
              None)
        (fun (((), baker, nonce), (op, res)) ->
          let op = {baker; operation = op_case.inj op; nonce} in
          Internal_baker_operation_result (op, res))
    in
    def "operation.alpha.internal_baker_operation_result"
    @@ union
         [ make_baker baker_proposals_case;
           make_baker baker_ballot_case;
           make_baker set_baker_active_case;
           make_baker set_baker_consensus_key_case;
           make_baker set_baker_pvss_key_case ]
end

type 'a icase =
  | ICase : {
      tag : int;
      name : string;
      encoding : 'a Data_encoding.t;
      select : 'a -> 'a option;
    }
      -> 'a icase

let manager_case =
  ICase
    {
      tag = 0;
      name = "internal_manager_operation";
      encoding = Manager_result.encoding;
      select =
        (function
        | Internal_manager_operation_result _ as result ->
            Some result
        | _ ->
            None);
    }

let baker_case =
  ICase
    {
      tag = 1;
      name = "internal_baker_operation";
      encoding = Baker_result.encoding;
      select =
        (function
        | Internal_baker_operation_result _ as result ->
            Some result
        | _ ->
            None);
    }

let internal_operation_result_encoding :
    packed_internal_operation_result Data_encoding.t =
  let make (ICase {tag; name; encoding; select}) =
    Data_encoding.case
      (Tag tag)
      ~title:name
      encoding
      (fun x -> match select x with None -> None | Some x -> Some x)
      (fun x -> x)
  in
  def "operation.alpha.internal_operation_result"
  @@ union [make manager_case; make baker_case]

type 'kind contents_result =
  | Endorsement_result : {
      balance_updates : Receipt.balance_updates;
      baker : Baker_hash.t;
      slots : int list;
    }
      -> Kind.endorsement contents_result
  | Seed_nonce_revelation_result :
      Receipt.balance_updates
      -> Kind.seed_nonce_revelation contents_result
  | Endorsement_with_slot_result :
      Kind.endorsement contents_result
      -> Kind.endorsement_with_slot contents_result
  | Double_endorsement_evidence_result :
      Receipt.balance_updates
      -> Kind.double_endorsement_evidence contents_result
  | Double_baking_evidence_result :
      Receipt.balance_updates
      -> Kind.double_baking_evidence contents_result
  | Activate_account_result :
      Receipt.balance_updates
      -> Kind.activate_account contents_result
  | Proposals_result : Kind.proposals contents_result
  | Ballot_result : Kind.ballot contents_result
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
  | (Kind.Reveal_manager_kind, Kind.Reveal_manager_kind) ->
      Some Eq
  | (Kind.Reveal_manager_kind, _) ->
      None
  | (Kind.Transaction_manager_kind, Kind.Transaction_manager_kind) ->
      Some Eq
  | (Kind.Transaction_manager_kind, _) ->
      None
  | (Kind.Origination_legacy_manager_kind, Kind.Origination_legacy_manager_kind)
    ->
      Some Eq
  | (Kind.Origination_legacy_manager_kind, _) ->
      None
  | (Kind.Origination_manager_kind, Kind.Origination_manager_kind) ->
      Some Eq
  | (Kind.Origination_manager_kind, _) ->
      None
  | (Kind.Delegation_legacy_manager_kind, Kind.Delegation_legacy_manager_kind)
    ->
      Some Eq
  | (Kind.Delegation_legacy_manager_kind, _) ->
      None
  | (Kind.Delegation_manager_kind, Kind.Delegation_manager_kind) ->
      Some Eq
  | (Kind.Delegation_manager_kind, _) ->
      None
  | (Kind.Baker_registration_manager_kind, Kind.Baker_registration_manager_kind)
    ->
      Some Eq
  | (Kind.Baker_registration_manager_kind, _) ->
      None

module Encoding = struct
  type 'kind case =
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
        -> 'kind case

  let tagged_case tag name args proj inj =
    let open Data_encoding in
    case
      tag
      ~title:(String.capitalize_ascii name)
      (merge_objs (obj1 (req "kind" (constant name))) args)
      (fun x -> match proj x with None -> None | Some x -> Some ((), x))
      (fun ((), x) -> inj x)

  let endorsement_case =
    Case
      {
        op_case = Operation.Encoding.endorsement_case;
        encoding =
          obj3
            (req "balance_updates" Receipt.balance_updates_encoding)
            (req "delegate" Signature.Public_key_hash.encoding)
            (req "slots" (list uint16));
        select =
          (function
          | Contents_result (Endorsement_result _ as op) -> Some op | _ -> None);
        mselect =
          (function
          | Contents_and_result ((Endorsement _ as op), res) ->
              Some (op, res)
          | _ ->
              None);
        proj =
          (function
          | Endorsement_result {balance_updates; delegate; slots} ->
              (balance_updates, delegate, slots));
        inj =
          (fun (balance_updates, delegate, slots) ->
            Endorsement_result {balance_updates; delegate; slots});
      }

  let seed_nonce_revelation_case =
    Case
      {
        op_case = Operation.Encoding.seed_nonce_revelation_case;
        encoding =
          obj1 (req "balance_updates" Receipt.balance_updates_encoding);
        select =
          (function
          | Contents_result (Seed_nonce_revelation_result _ as op) ->
              Some op
          | _ ->
              None);
        mselect =
          (function
          | Contents_and_result ((Seed_nonce_revelation _ as op), res) ->
              Some (op, res)
          | _ ->
              None);
        proj = (fun (Seed_nonce_revelation_result bus) -> bus);
        inj = (fun bus -> Seed_nonce_revelation_result bus);
      }

  let endorsement_with_slot_case =
    Case
      {
        op_case = Operation.Encoding.endorsement_with_slot_case;
        encoding =
          obj3
            (req "balance_updates" Receipt.balance_updates_encoding)
            (req "delegate" Signature.Public_key_hash.encoding)
            (req "slots" (list uint16));
        select =
          (function
          | Contents_result (Endorsement_with_slot_result _ as op) ->
              Some op
          | _ ->
              None);
        mselect =
          (function
          | Contents_and_result ((Endorsement_with_slot _ as op), res) ->
              Some (op, res)
          | _ ->
              None);
        proj =
          (function
          | Endorsement_with_slot_result
              (Endorsement_result {balance_updates; delegate; slots}) ->
              (balance_updates, delegate, slots));
        inj =
          (fun (balance_updates, delegate, slots) ->
            Endorsement_with_slot_result
              (Endorsement_result {balance_updates; delegate; slots}));
      }

  let double_endorsement_evidence_case =
    Case
      {
        op_case = Operation.Encoding.double_endorsement_evidence_case;
        encoding =
          obj1 (req "balance_updates" Receipt.balance_updates_encoding);
        select =
          (function
          | Contents_result (Double_endorsement_evidence_result _ as op) ->
              Some op
          | _ ->
              None);
        mselect =
          (function
          | Contents_and_result ((Double_endorsement_evidence _ as op), res) ->
              Some (op, res)
          | _ ->
              None);
        proj = (fun (Double_endorsement_evidence_result bus) -> bus);
        inj = (fun bus -> Double_endorsement_evidence_result bus);
      }

  let double_baking_evidence_case =
    Case
      {
        op_case = Operation.Encoding.double_baking_evidence_case;
        encoding =
          obj1 (req "balance_updates" Receipt.balance_updates_encoding);
        select =
          (function
          | Contents_result (Double_baking_evidence_result _ as op) ->
              Some op
          | _ ->
              None);
        mselect =
          (function
          | Contents_and_result ((Double_baking_evidence _ as op), res) ->
              Some (op, res)
          | _ ->
              None);
        proj = (fun (Double_baking_evidence_result bus) -> bus);
        inj = (fun bus -> Double_baking_evidence_result bus);
      }

  let activate_account_case =
    Case
      {
        op_case = Operation.Encoding.activate_account_case;
        encoding =
          obj1 (req "balance_updates" Receipt.balance_updates_encoding);
        select =
          (function
          | Contents_result (Activate_account_result _ as op) ->
              Some op
          | _ ->
              None);
        mselect =
          (function
          | Contents_and_result ((Activate_account _ as op), res) ->
              Some (op, res)
          | _ ->
              None);
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
          | Contents_and_result ((Proposals _ as op), res) ->
              Some (op, res)
          | _ ->
              None);
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
          | Contents_and_result ((Ballot _ as op), res) ->
              Some (op, res)
          | _ ->
              None);
        proj = (fun Ballot_result -> ());
        inj = (fun () -> Ballot_result);
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
            (req "balance_updates" Receipt.balance_updates_encoding)
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
            | None ->
                None )
          | Contents_result
              (Manager_operation_result
                ({operation_result = Backtracked (res, errs); _} as op)) -> (
            match res_case.select (Successful_manager_result res) with
            | Some res ->
                Some
                  (Manager_operation_result
                     {op with operation_result = Backtracked (res, errs)})
            | None ->
                None )
          | Contents_result
              (Manager_operation_result
                ({operation_result = Skipped kind; _} as op)) -> (
            match equal_manager_kind kind res_case.kind with
            | None ->
                None
            | Some Eq ->
                Some
                  (Manager_operation_result
                     {op with operation_result = Skipped kind}) )
          | Contents_result
              (Manager_operation_result
                ({operation_result = Failed (kind, errs); _} as op)) -> (
            match equal_manager_kind kind res_case.kind with
            | None ->
                None
            | Some Eq ->
                Some
                  (Manager_operation_result
                     {op with operation_result = Failed (kind, errs)}) )
          | Contents_result Ballot_result ->
              None
          | Contents_result (Endorsement_result _) ->
              None
          | Contents_result (Seed_nonce_revelation_result _) ->
              None
          | Contents_result (Endorsement_with_slot_result _) ->
              None
          | Contents_result (Double_endorsement_evidence_result _) ->
              None
          | Contents_result (Double_baking_evidence_result _) ->
              None
          | Contents_result (Activate_account_result _) ->
              None
          | Contents_result Proposals_result ->
              None);
        mselect;
        proj =
          (fun (Manager_operation_result
                 { balance_updates = bus;
                   operation_result = r;
                   internal_operation_results = rs }) ->
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
        | _ ->
            None)

  let transaction_case =
    make_manager_case
      Operation.Encoding.transaction_case
      Manager_result.transaction_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Transaction _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)

  let origination_legacy_case =
    make_manager_case
      Operation.Encoding.origination_legacy_case
      Manager_result.origination_legacy_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Origination_legacy _; _} as op),
              res ) ->
            Some (op, res)
        | _ ->
            None)

  let origination_case =
    make_manager_case
      Operation.Encoding.origination_case
      Manager_result.origination_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Origination _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)

  let delegation_legacy_case =
    make_manager_case
      Operation.Encoding.delegation_legacy_case
      Manager_result.delegation_legacy_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Delegation_legacy _; _} as op),
              res ) ->
            Some (op, res)
        | _ ->
            None)

  let delegation_case =
    make_manager_case
      Operation.Encoding.delegation_case
      Manager_result.delegation_case
      (function
        | Contents_and_result
            ((Manager_operation {operation = Delegation _; _} as op), res) ->
            Some (op, res)
        | _ ->
            None)

  let baker_registration_case =
    make_manager_case
      Operation.Encoding.baker_registration_case
      Manager_result.baker_registration_case
      (function
        | Contents_and_result
            ( (Manager_operation {operation = Baker_registration _; _} as op),
              res ) ->
            Some (op, res)
        | _ ->
            None)
end

let contents_result_encoding =
  let open Encoding in
  let make
      (Case
        { op_case = Operation.Encoding.Case {tag; name; _};
          encoding;
          mselect = _;
          select;
          proj;
          inj }) =
    let proj x =
      match select x with None -> None | Some x -> Some (proj x)
    in
    let inj x = Contents_result (inj x) in
    tagged_case (Tag tag) name encoding proj inj
  in
  def "operation.alpha.contents_result"
  @@ union
       [ make endorsement_case;
         make seed_nonce_revelation_case;
         make endorsement_with_slot_case;
         make double_endorsement_evidence_case;
         make double_baking_evidence_case;
         make activate_account_case;
         make proposals_case;
         make ballot_case;
         make reveal_case;
         make transaction_case;
         make origination_legacy_case;
         make origination_case;
         make delegation_legacy_case;
         make delegation_case;
         make baker_registration_case ]

let contents_and_result_encoding =
  let open Encoding in
  let make
      (Case
        { op_case = Operation.Encoding.Case {tag; name; encoding; proj; inj; _};
          mselect;
          encoding = meta_encoding;
          proj = meta_proj;
          inj = meta_inj;
          _ }) =
    let proj c =
      match mselect c with
      | Some (op, res) ->
          Some (proj op, meta_proj res)
      | _ ->
          None
    in
    let inj (op, res) = Contents_and_result (inj op, meta_inj res) in
    let encoding = merge_objs encoding (obj1 (req "metadata" meta_encoding)) in
    tagged_case (Tag tag) name encoding proj inj
  in
  def "operation.alpha.operation_contents_and_result"
  @@ union
       [ make endorsement_case;
         make seed_nonce_revelation_case;
         make endorsement_with_slot_case;
         make double_endorsement_evidence_case;
         make double_baking_evidence_case;
         make activate_account_case;
         make proposals_case;
         make ballot_case;
         make reveal_case;
         make transaction_case;
         make origination_legacy_case;
         make origination_case;
         make delegation_legacy_case;
         make delegation_case;
         make baker_registration_case ]

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

let contents_result_list_encoding =
  let rec to_list = function
    | Contents_result_list (Single_result o) ->
        [Contents_result o]
    | Contents_result_list (Cons_result (o, os)) ->
        Contents_result o :: to_list (Contents_result_list os)
  in
  let rec of_list = function
    | [] ->
        Pervasives.failwith "cannot decode empty operation result"
    | [Contents_result o] ->
        Contents_result_list (Single_result o)
    | Contents_result o :: os -> (
        let (Contents_result_list os) = of_list os in
        match (o, os) with
        | ( Manager_operation_result _,
            Single_result (Manager_operation_result _) ) ->
            Contents_result_list (Cons_result (o, os))
        | (Manager_operation_result _, Cons_result _) ->
            Contents_result_list (Cons_result (o, os))
        | _ ->
            Pervasives.failwith "cannot decode ill-formed operation result" )
  in
  def "operation.alpha.contents_list_result"
  @@ conv to_list of_list (list contents_result_encoding)

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

let contents_and_result_list_encoding =
  let rec to_list = function
    | Contents_and_result_list (Single_and_result (op, res)) ->
        [Contents_and_result (op, res)]
    | Contents_and_result_list (Cons_and_result (op, res, rest)) ->
        Contents_and_result (op, res)
        :: to_list (Contents_and_result_list rest)
  in
  let rec of_list = function
    | [] ->
        Pervasives.failwith "cannot decode empty combined operation result"
    | [Contents_and_result (op, res)] ->
        Contents_and_result_list (Single_and_result (op, res))
    | Contents_and_result (op, res) :: rest -> (
        let (Contents_and_result_list rest) = of_list rest in
        match (op, rest) with
        | (Manager_operation _, Single_and_result (Manager_operation _, _)) ->
            Contents_and_result_list (Cons_and_result (op, res, rest))
        | (Manager_operation _, Cons_and_result (_, _, _)) ->
            Contents_and_result_list (Cons_and_result (op, res, rest))
        | _ ->
            Pervasives.failwith
              "cannot decode ill-formed combined operation result" )
  in
  conv to_list of_list (Variable.list contents_and_result_encoding)

type 'kind operation_metadata = {contents : 'kind contents_result_list}

type packed_operation_metadata =
  | Operation_metadata : 'kind operation_metadata -> packed_operation_metadata
  | No_operation_metadata : packed_operation_metadata

let operation_metadata_encoding =
  def "operation.alpha.result"
  @@ union
       [ case
           (Tag 0)
           ~title:"Operation_metadata"
           contents_result_list_encoding
           (function
             | Operation_metadata {contents} ->
                 Some (Contents_result_list contents)
             | _ ->
                 None)
           (fun (Contents_result_list contents) ->
             Operation_metadata {contents});
         case
           (Tag 1)
           ~title:"No_operation_metadata"
           empty
           (function No_operation_metadata -> Some () | _ -> None)
           (fun () -> No_operation_metadata) ]

let kind_equal :
    type kind kind2.
    kind contents -> kind2 contents_result -> (kind, kind2) eq option =
 fun op res ->
  match (op, res) with
  | (Endorsement _, Endorsement_result _) ->
      Some Eq
  | (Endorsement _, _) ->
      None
  | (Seed_nonce_revelation _, Seed_nonce_revelation_result _) ->
      Some Eq
  | (Seed_nonce_revelation _, _) ->
      None
  | (Endorsement_with_slot _, Endorsement_with_slot_result _) ->
      Some Eq
  | (Endorsement_with_slot _, _) ->
      None
  | (Double_endorsement_evidence _, Double_endorsement_evidence_result _) ->
      Some Eq
  | (Double_endorsement_evidence _, _) ->
      None
  | (Double_baking_evidence _, Double_baking_evidence_result _) ->
      Some Eq
  | (Double_baking_evidence _, _) ->
      None
  | (Activate_account _, Activate_account_result _) ->
      Some Eq
  | (Activate_account _, _) ->
      None
  | (Proposals _, Proposals_result) ->
      Some Eq
  | (Proposals _, _) ->
      None
  | (Ballot _, Ballot_result) ->
      Some Eq
  | (Ballot _, _) ->
      None
  | (Failing_noop _, _) ->
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
        { operation_result = Failed (Alpha_context.Kind.Reveal_manager_kind, _);
          _ } ) ->
      Some Eq
  | ( Manager_operation {operation = Reveal _; _},
      Manager_operation_result
        {operation_result = Skipped Alpha_context.Kind.Reveal_manager_kind; _}
    ) ->
      Some Eq
  | (Manager_operation {operation = Reveal _; _}, _) ->
      None
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
        { operation_result =
            Failed (Alpha_context.Kind.Transaction_manager_kind, _);
          _ } ) ->
      Some Eq
  | ( Manager_operation {operation = Transaction _; _},
      Manager_operation_result
        { operation_result = Skipped Alpha_context.Kind.Transaction_manager_kind;
          _ } ) ->
      Some Eq
  | (Manager_operation {operation = Transaction _; _}, _) ->
      None
  | ( Manager_operation {operation = Origination_legacy _; _},
      Manager_operation_result
        {operation_result = Applied (Origination_legacy_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Origination_legacy _; _},
      Manager_operation_result
        {operation_result = Backtracked (Origination_legacy_result _, _); _} )
    ->
      Some Eq
  | ( Manager_operation {operation = Origination_legacy _; _},
      Manager_operation_result
        { operation_result =
            Failed (Alpha_context.Kind.Origination_legacy_manager_kind, _);
          _ } ) ->
      Some Eq
  | ( Manager_operation {operation = Origination_legacy _; _},
      Manager_operation_result
        { operation_result =
            Skipped Alpha_context.Kind.Origination_legacy_manager_kind;
          _ } ) ->
      Some Eq
  | (Manager_operation {operation = Origination_legacy _; _}, _) ->
      None
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
        { operation_result =
            Failed (Alpha_context.Kind.Origination_manager_kind, _);
          _ } ) ->
      Some Eq
  | ( Manager_operation {operation = Origination _; _},
      Manager_operation_result
        { operation_result = Skipped Alpha_context.Kind.Origination_manager_kind;
          _ } ) ->
      Some Eq
  | (Manager_operation {operation = Origination _; _}, _) ->
      None
  | ( Manager_operation {operation = Delegation_legacy _; _},
      Manager_operation_result
        {operation_result = Applied (Delegation_legacy_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Delegation_legacy _; _},
      Manager_operation_result
        {operation_result = Backtracked (Delegation_legacy_result _, _); _} )
    ->
      Some Eq
  | ( Manager_operation {operation = Delegation_legacy _; _},
      Manager_operation_result
        { operation_result =
            Failed (Alpha_context.Kind.Delegation_legacy_manager_kind, _);
          _ } ) ->
      Some Eq
  | ( Manager_operation {operation = Delegation_legacy _; _},
      Manager_operation_result
        { operation_result =
            Skipped Alpha_context.Kind.Delegation_legacy_manager_kind;
          _ } ) ->
      Some Eq
  | (Manager_operation {operation = Delegation_legacy _; _}, _) ->
      None
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
        { operation_result =
            Failed (Alpha_context.Kind.Delegation_manager_kind, _);
          _ } ) ->
      Some Eq
  | ( Manager_operation {operation = Delegation _; _},
      Manager_operation_result
        { operation_result = Skipped Alpha_context.Kind.Delegation_manager_kind;
          _ } ) ->
      Some Eq
  | (Manager_operation {operation = Delegation _; _}, _) ->
      None
  | ( Manager_operation {operation = Baker_registration _; _},
      Manager_operation_result
        {operation_result = Applied (Baker_registration_result _); _} ) ->
      Some Eq
  | ( Manager_operation {operation = Baker_registration _; _},
      Manager_operation_result
        {operation_result = Backtracked (Baker_registration_result _, _); _} )
    ->
      Some Eq
  | ( Manager_operation {operation = Baker_registration _; _},
      Manager_operation_result
        { operation_result =
            Failed (Alpha_context.Kind.Baker_registration_manager_kind, _);
          _ } ) ->
      Some Eq
  | ( Manager_operation {operation = Baker_registration _; _},
      Manager_operation_result
        { operation_result =
            Skipped Alpha_context.Kind.Baker_registration_manager_kind;
          _ } ) ->
      Some Eq
  | (Manager_operation {operation = Baker_registration _; _}, _) ->
      None

let rec kind_equal_list :
    type kind kind2.
    kind contents_list -> kind2 contents_result_list -> (kind, kind2) eq option
    =
 fun contents res ->
  match (contents, res) with
  | (Single op, Single_result res) -> (
    match kind_equal op res with None -> None | Some Eq -> Some Eq )
  | (Cons (op, ops), Cons_result (res, ress)) -> (
    match kind_equal op res with
    | None ->
        None
    | Some Eq -> (
      match kind_equal_list ops ress with None -> None | Some Eq -> Some Eq ) )
  | _ ->
      None

let rec pack_contents_list :
    type kind.
    kind contents_list ->
    kind contents_result_list ->
    kind contents_and_result_list =
 fun contents res ->
  match (contents, res) with
  | (Single op, Single_result res) ->
      Single_and_result (op, res)
  | (Cons (op, ops), Cons_result (res, ress)) ->
      Cons_and_result (op, res, pack_contents_list ops ress)
  | ( Single (Manager_operation _),
      Cons_result (Manager_operation_result _, Single_result _) ) ->
      .
  | ( Cons (_, _),
      Single_result (Manager_operation_result {operation_result = Failed _; _})
    ) ->
      .
  | ( Cons (_, _),
      Single_result
        (Manager_operation_result {operation_result = Skipped _; _}) ) ->
      .
  | ( Cons (_, _),
      Single_result
        (Manager_operation_result {operation_result = Applied _; _}) ) ->
      .
  | ( Cons (_, _),
      Single_result
        (Manager_operation_result {operation_result = Backtracked _; _}) ) ->
      .
  | (Single _, Cons_result _) ->
      .

let rec unpack_contents_list :
    type kind.
    kind contents_and_result_list ->
    kind contents_list * kind contents_result_list = function
  | Single_and_result (op, res) ->
      (Single op, Single_result res)
  | Cons_and_result (op, res, rest) ->
      let (ops, ress) = unpack_contents_list rest in
      (Cons (op, ops), Cons_result (res, ress))

let rec to_list = function
  | Contents_result_list (Single_result o) ->
      [Contents_result o]
  | Contents_result_list (Cons_result (o, os)) ->
      Contents_result o :: to_list (Contents_result_list os)

let rec of_list = function
  | [] ->
      assert false
  | [Contents_result o] ->
      Contents_result_list (Single_result o)
  | Contents_result o :: os -> (
      let (Contents_result_list os) = of_list os in
      match (o, os) with
      | (Manager_operation_result _, Single_result (Manager_operation_result _))
        ->
          Contents_result_list (Cons_result (o, os))
      | (Manager_operation_result _, Cons_result _) ->
          Contents_result_list (Cons_result (o, os))
      | _ ->
          Pervasives.failwith
            "Operation result list of length > 1 should only contains manager \
             operations result." )

let operation_data_and_metadata_encoding =
  def "operation.alpha.operation_with_metadata"
  @@ union
       [ case
           (Tag 0)
           ~title:"Operation_with_metadata"
           (obj2
              (req "contents" (dynamic_size contents_and_result_list_encoding))
              (opt "signature" Signature.encoding))
           (function
             | (Operation_data _, No_operation_metadata) ->
                 None
             | (Operation_data op, Operation_metadata res) -> (
               match kind_equal_list op.contents res.contents with
               | None ->
                   Pervasives.failwith
                     "cannot decode inconsistent combined operation result"
               | Some Eq ->
                   Some
                     ( Contents_and_result_list
                         (pack_contents_list op.contents res.contents),
                       op.signature ) ))
           (fun (Contents_and_result_list contents, signature) ->
             let (op_contents, res_contents) = unpack_contents_list contents in
             ( Operation_data {contents = op_contents; signature},
               Operation_metadata {contents = res_contents} ));
         case
           (Tag 1)
           ~title:"Operation_without_metadata"
           (obj2
              (req "contents" (dynamic_size Operation.contents_list_encoding))
              (opt "signature" Signature.encoding))
           (function
             | (Operation_data op, No_operation_metadata) ->
                 Some (Contents_list op.contents, op.signature)
             | (Operation_data _, Operation_metadata _) ->
                 None)
           (fun (Contents_list contents, signature) ->
             (Operation_data {contents; signature}, No_operation_metadata)) ]

type block_metadata = {
  baker : Signature.Public_key_hash.t;
  level : Level.compat_t;
  level_info : Level.t;
  voting_period_kind : Voting_period.kind;
  voting_period_info : Voting_period.info;
  nonce_hash : Nonce_hash.t option;
  consumed_gas : Gas.Arith.fp;
  deactivated : Signature.Public_key_hash.t list;
  balance_updates : Receipt.balance_updates;
}

let block_metadata_encoding =
  let open Data_encoding in
  def "block_header.alpha.metadata"
  @@ conv
       (fun { baker;
              level;
              level_info;
              voting_period_kind;
              voting_period_info;
              nonce_hash;
              consumed_gas;
              deactivated;
              balance_updates } ->
         ( baker,
           level,
           level_info,
           voting_period_kind,
           voting_period_info,
           nonce_hash,
           consumed_gas,
           deactivated,
           balance_updates ))
       (fun ( baker,
              level,
              level_info,
              voting_period_kind,
              voting_period_info,
              nonce_hash,
              consumed_gas,
              deactivated,
              balance_updates ) ->
         {
           baker;
           level;
           level_info;
           voting_period_kind;
           voting_period_info;
           nonce_hash;
           consumed_gas;
           deactivated;
           balance_updates;
         })
       (obj9
          (req "baker" Signature.Public_key_hash.encoding)
          (req
             ~description:"This field is DEPRECATED: use level_info instead"
             "level"
             Level.compat_encoding)
          (req "level_info" Level.encoding)
          (req
             ~description:
               "This field is DEPRECATED: use voting_period_info instead"
             "voting_period_kind"
             Voting_period.kind_encoding)
          (req "voting_period_info" Voting_period.info_encoding)
          (req "nonce_hash" (option Nonce_hash.encoding))
          (req "consumed_gas" Gas.Arith.n_fp_encoding)
          (req "deactivated" (list Signature.Public_key_hash.encoding))
          (req "balance_updates" Receipt.balance_updates_encoding))
