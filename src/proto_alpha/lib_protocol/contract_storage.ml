(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

type error +=
  | (* `Temporary *)
      Balance_too_low of
      Contract_repr.t * Tez_repr.t * Tez_repr.t
  | (* `Temporary *)
      Counter_in_the_past of {
      contract : Contract_repr.t;
      expected : Manager_counter_repr.t;
      found : Manager_counter_repr.t;
    }
  | (* `Branch *)
      Counter_in_the_future of {
      contract : Contract_repr.t;
      expected : Manager_counter_repr.t;
      found : Manager_counter_repr.t;
    }
  | (* `Temporary *)
      Non_existing_contract of
      Contract_repr.t
  | (* `Branch *)
      Empty_implicit_contract of
      Signature.Public_key_hash.t
  | (* `Branch *)
      Empty_implicit_delegated_contract of
      Signature.Public_key_hash.t
  | (* `Permanent *)
      Inconsistent_public_key of
      Signature.Public_key.t * Signature.Public_key.t
  | (* `Permanent *) Failure of string

type error +=
  | (* `Permanent *)
      Frozen_bonds_must_be_spent_at_once of
      Contract_repr.t * Bond_id_repr.t

let () =
  register_error_kind
    `Temporary
    ~id:"contract.balance_too_low"
    ~title:"Balance too low"
    ~description:"An operation tried to spend more tokens than the contract has"
    ~pp:(fun ppf (c, b, a) ->
      Format.fprintf
        ppf
        "Balance of contract %a too low (%a) to spend %a"
        Contract_repr.pp
        c
        Tez_repr.pp
        b
        Tez_repr.pp
        a)
    Data_encoding.(
      obj3
        (req "contract" Contract_repr.encoding)
        (req "balance" Tez_repr.encoding)
        (req "amount" Tez_repr.encoding))
    (function Balance_too_low (c, b, a) -> Some (c, b, a) | _ -> None)
    (fun (c, b, a) -> Balance_too_low (c, b, a)) ;
  register_error_kind
    `Temporary
    ~id:"contract.counter_in_the_future"
    ~title:"Invalid counter (not yet reached) in a manager operation"
    ~description:"An operation assumed a contract counter in the future"
    ~pp:(fun ppf (contract, exp, found) ->
      Format.fprintf
        ppf
        "Counter %a not yet reached for contract %a (expected %a)"
        Manager_counter_repr.pp
        found
        Contract_repr.pp
        contract
        Manager_counter_repr.pp
        exp)
    Data_encoding.(
      obj3
        (req "contract" Contract_repr.encoding)
        (req "expected" Manager_counter_repr.encoding_for_errors)
        (req "found" Manager_counter_repr.encoding_for_errors))
    (function
      | Counter_in_the_future {contract; expected; found} ->
          Some (contract, expected, found)
      | _ -> None)
    (fun (contract, expected, found) ->
      Counter_in_the_future {contract; expected; found}) ;
  register_error_kind
    `Branch
    ~id:"contract.counter_in_the_past"
    ~title:"Invalid counter (already used) in a manager operation"
    ~description:"An operation assumed a contract counter in the past"
    ~pp:(fun ppf (contract, exp, found) ->
      Format.fprintf
        ppf
        "Counter %a already used for contract %a (expected %a)"
        Manager_counter_repr.pp
        found
        Contract_repr.pp
        contract
        Manager_counter_repr.pp
        exp)
    Data_encoding.(
      obj3
        (req "contract" Contract_repr.encoding)
        (req "expected" Manager_counter_repr.encoding_for_errors)
        (req "found" Manager_counter_repr.encoding_for_errors))
    (function
      | Counter_in_the_past {contract; expected; found} ->
          Some (contract, expected, found)
      | _ -> None)
    (fun (contract, expected, found) ->
      Counter_in_the_past {contract; expected; found}) ;
  register_error_kind
    `Temporary
    ~id:"contract.non_existing_contract"
    ~title:"Non existing contract"
    ~description:
      "A contract handle is not present in the context (either it never was or \
       it has been destroyed)"
    ~pp:(fun ppf contract ->
      Format.fprintf ppf "Contract %a does not exist" Contract_repr.pp contract)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Non_existing_contract c -> Some c | _ -> None)
    (fun c -> Non_existing_contract c) ;
  register_error_kind
    `Permanent
    ~id:"contract.manager.inconsistent_public_key"
    ~title:"Inconsistent public key"
    ~description:
      "A provided manager public key is different with the public key stored \
       in the contract"
    ~pp:(fun ppf (eh, ph) ->
      Format.fprintf
        ppf
        "Expected manager public key %s but %s was provided"
        (Signature.Public_key.to_b58check ph)
        (Signature.Public_key.to_b58check eh))
    Data_encoding.(
      obj2
        (req "public_key" Signature.Public_key.encoding)
        (req "expected_public_key" Signature.Public_key.encoding))
    (function Inconsistent_public_key (eh, ph) -> Some (eh, ph) | _ -> None)
    (fun (eh, ph) -> Inconsistent_public_key (eh, ph)) ;
  register_error_kind
    `Permanent
    ~id:"contract.failure"
    ~title:"Contract storage failure"
    ~description:"Unexpected contract storage error"
    ~pp:(fun ppf s -> Format.fprintf ppf "Contract_storage.Failure %S" s)
    Data_encoding.(obj1 (req "message" @@ string Plain))
    (function Failure s -> Some s | _ -> None)
    (fun s -> Failure s) ;
  register_error_kind
    `Branch
    ~id:"implicit.empty_implicit_contract"
    ~title:"Empty implicit contract"
    ~description:
      "No manager operations are allowed on an empty implicit contract. This \
       account has zero balance. Fund it before using."
    ~pp:(fun ppf implicit ->
      Format.fprintf
        ppf
        "Empty implicit contract (%a)"
        Signature.Public_key_hash.pp
        implicit)
    Data_encoding.(obj1 (req "implicit" Signature.Public_key_hash.encoding))
    (function Empty_implicit_contract c -> Some c | _ -> None)
    (fun c -> Empty_implicit_contract c) ;
  register_error_kind
    `Branch
    ~id:"implicit.empty_implicit_delegated_contract"
    ~title:"Empty implicit delegated contract"
    ~description:"Emptying an implicit delegated account is not allowed."
    ~pp:(fun ppf implicit ->
      Format.fprintf
        ppf
        "Emptying implicit delegated contract (%a)"
        Signature.Public_key_hash.pp
        implicit)
    Data_encoding.(obj1 (req "implicit" Signature.Public_key_hash.encoding))
    (function Empty_implicit_delegated_contract c -> Some c | _ -> None)
    (fun c -> Empty_implicit_delegated_contract c) ;
  register_error_kind
    `Permanent
    ~id:"frozen_bonds.must_be_spent_at_once"
    ~title:"Partial spending of frozen bonds"
    ~description:"Frozen bonds must be spent at once."
    ~pp:(fun ppf (contract, bond_id) ->
      Format.fprintf
        ppf
        "The frozen funds for contract (%a) and bond (%a) are not allowed to \
         be partially withdrawn. The amount withdrawn must be equal to the \
         entire deposit for the said bond."
        Contract_repr.pp
        contract
        Bond_id_repr.pp
        bond_id)
    Data_encoding.(
      obj2
        (req "contract" Contract_repr.encoding)
        (req "bond_id" Bond_id_repr.encoding))
    (function
      | Frozen_bonds_must_be_spent_at_once (c, b) -> Some (c, b) | _ -> None)
    (fun (c, b) -> Frozen_bonds_must_be_spent_at_once (c, b))

let failwith msg = tzfail (Failure msg)

module Legacy_big_map_diff = struct
  (*
    Big_map_diff receipt as it was represented in 006 and earlier.
    It is kept here for now for backward compatibility of tools. *)

  type item =
    | Update of {
        big_map : Z.t;
        diff_key : Script_repr.expr;
        diff_key_hash : Script_expr_hash.t;
        diff_value : Script_repr.expr option;
      }
    | Clear of Z.t
    | Copy of {src : Z.t; dst : Z.t}
    | Alloc of {
        big_map : Z.t;
        key_type : Script_repr.expr;
        value_type : Script_repr.expr;
      }

  type t = item list

  let item_encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"update"
          (obj5
             (req "action" (constant "update"))
             (req "big_map" z)
             (req "key_hash" Script_expr_hash.encoding)
             (req "key" Script_repr.expr_encoding)
             (opt "value" Script_repr.expr_encoding))
          (function
            | Update {big_map; diff_key_hash; diff_key; diff_value} ->
                Some ((), big_map, diff_key_hash, diff_key, diff_value)
            | _ -> None)
          (fun ((), big_map, diff_key_hash, diff_key, diff_value) ->
            Update {big_map; diff_key_hash; diff_key; diff_value});
        case
          (Tag 1)
          ~title:"remove"
          (obj2 (req "action" (constant "remove")) (req "big_map" z))
          (function Clear big_map -> Some ((), big_map) | _ -> None)
          (fun ((), big_map) -> Clear big_map);
        case
          (Tag 2)
          ~title:"copy"
          (obj3
             (req "action" (constant "copy"))
             (req "source_big_map" z)
             (req "destination_big_map" z))
          (function Copy {src; dst} -> Some ((), src, dst) | _ -> None)
          (fun ((), src, dst) -> Copy {src; dst});
        case
          (Tag 3)
          ~title:"alloc"
          (obj4
             (req "action" (constant "alloc"))
             (req "big_map" z)
             (req "key_type" Script_repr.expr_encoding)
             (req "value_type" Script_repr.expr_encoding))
          (function
            | Alloc {big_map; key_type; value_type} ->
                Some ((), big_map, key_type, value_type)
            | _ -> None)
          (fun ((), big_map, key_type, value_type) ->
            Alloc {big_map; key_type; value_type});
      ]

  let encoding = Data_encoding.list item_encoding

  let to_lazy_storage_diff legacy_diffs =
    let rev_head (diffs : (_ * (_, _, _) Lazy_storage_diff.diff) list) =
      match diffs with
      | [] -> []
      | (_, Remove) :: _ -> diffs
      | (id, Update {init; updates}) :: rest ->
          (id, Update {init; updates = List.rev updates}) :: rest
    in
    (* Invariant:
       Updates are collected one by one, in reverse order, on the head diff
       item. So only and exactly the head diff item has its updates reversed.
    *)
    List.fold_left
      (fun (new_diff : (_ * (_, _, _) Lazy_storage_diff.diff) list) item ->
        match item with
        | Clear id -> (id, Lazy_storage_diff.Remove) :: rev_head new_diff
        | Copy {src; dst} ->
            let src =
              Lazy_storage_kind.Big_map.Id
              .of_legacy_USE_ONLY_IN_Legacy_big_map_diff
                src
            in
            (dst, Lazy_storage_diff.Update {init = Copy {src}; updates = []})
            :: rev_head new_diff
        | Alloc {big_map; key_type; value_type} ->
            ( big_map,
              Lazy_storage_diff.(
                Update
                  {
                    init = Alloc Lazy_storage_kind.Big_map.{key_type; value_type};
                    updates = [];
                  }) )
            :: rev_head new_diff
        | Update
            {
              big_map;
              diff_key = key;
              diff_key_hash = key_hash;
              diff_value = value;
            } -> (
            match new_diff with
            | (id, diff) :: rest when Compare.Z.(id = big_map) ->
                let diff =
                  match diff with
                  | Remove -> assert false
                  | Update {init; updates} ->
                      let updates =
                        Lazy_storage_kind.Big_map.{key; key_hash; value}
                        :: updates
                      in
                      Lazy_storage_diff.Update {init; updates}
                in
                (id, diff) :: rest
            | new_diff ->
                let updates =
                  [Lazy_storage_kind.Big_map.{key; key_hash; value}]
                in
                (big_map, Update {init = Existing; updates})
                :: rev_head new_diff))
      []
      legacy_diffs
    |> rev_head
    |> List.rev_map (fun (id, diff) ->
           let id =
             Lazy_storage_kind.Big_map.Id
             .of_legacy_USE_ONLY_IN_Legacy_big_map_diff
               id
           in
           Lazy_storage_diff.make Lazy_storage_kind.Big_map id diff)

  let of_lazy_storage_diff diffs =
    List.fold_left
      (fun legacy_diffs (Lazy_storage_diff.Item (kind, id, diff)) ->
        let diffs =
          match kind with
          | Lazy_storage_kind.Big_map -> (
              let id =
                Lazy_storage_kind.Big_map.Id
                .to_legacy_USE_ONLY_IN_Legacy_big_map_diff
                  id
              in
              match diff with
              | Remove -> [Clear id]
              | Update {init; updates} -> (
                  let updates =
                    List.rev_map
                      (fun {Lazy_storage_kind.Big_map.key; key_hash; value} ->
                        Update
                          {
                            big_map = id;
                            diff_key = key;
                            diff_key_hash = key_hash;
                            diff_value = value;
                          })
                      updates
                  in
                  match init with
                  | Existing -> updates
                  | Copy {src} ->
                      let src =
                        Lazy_storage_kind.Big_map.Id
                        .to_legacy_USE_ONLY_IN_Legacy_big_map_diff
                          src
                      in
                      Copy {src; dst = id} :: updates
                  | Alloc {key_type; value_type} ->
                      Alloc {big_map = id; key_type; value_type} :: updates))
          | _ -> (* Not a Big_map *) []
        in
        diffs :: legacy_diffs)
      []
      diffs
    |> List.rev |> List.flatten
end

let update_script_lazy_storage c = function
  | None -> return (c, Z.zero)
  | Some diffs -> Lazy_storage_diff.apply c diffs

let raw_originate c ~prepaid_bootstrap_storage
    (* Free space for bootstrap contracts *) contract ~script =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Originated contract in
  let* c = Storage.Contract.Spendable_balance.init c contract Tez_repr.zero in
  let {Script_repr.code; storage}, lazy_storage_diff = script in
  let* c, code_size = Storage.Contract.Code.init c contract code in
  let* c, storage_size = Storage.Contract.Storage.init c contract storage in
  let* c, lazy_storage_size = update_script_lazy_storage c lazy_storage_diff in
  let total_size =
    Z.add (Z.add (Z.of_int code_size) (Z.of_int storage_size)) lazy_storage_size
  in
  assert (Compare.Z.(total_size >= Z.zero)) ;
  let prepaid_bootstrap_storage =
    if prepaid_bootstrap_storage then total_size else Z.zero
  in
  let* c =
    Storage.Contract.Paid_storage_space.init
      c
      contract
      prepaid_bootstrap_storage
  in
  Storage.Contract.Used_storage_space.init c contract total_size

let native_originate ctxt contract ~script =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Originated contract in
  let* ctxt =
    Storage.Contract.Spendable_balance.init ctxt contract Tez_repr.zero
  in
  let {Script_native_repr.kind; storage}, lazy_storage_diff = script in
  let* ctxt, kind_size = Storage.Contract.Native.init ctxt contract kind in
  let* ctxt, storage_size =
    Storage.Contract.Storage.init ctxt contract storage
  in
  let* ctxt, lazy_storage_size =
    update_script_lazy_storage ctxt lazy_storage_diff
  in
  let total_size =
    Z.add (Z.add (Z.of_int kind_size) (Z.of_int storage_size)) lazy_storage_size
  in
  assert (Compare.Z.(total_size >= Z.zero)) ;
  let* ctxt =
    Storage.Contract.Paid_storage_space.init ctxt contract total_size
  in
  Storage.Contract.Used_storage_space.init ctxt contract total_size

let create_implicit c manager ~balance =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit manager in
  let* counter = Storage.Contract.Global_counter.get c in
  let* c = Storage.Contract.Counter.init c contract counter in
  let* c = Storage.Contract.Spendable_balance.init c contract balance in
  Contract_manager_storage.init c contract (Manager_repr.Hash manager)

let delete c contract =
  let open Lwt_result_syntax in
  match contract with
  | Contract_repr.Originated _ ->
      (* For non implicit contract Big_map should be cleared *)
      failwith "Non implicit contracts cannot be removed"
  | Implicit _ ->
      (* Implicit contract do not have: [Code], [Storage],
         [Paid_storage_space] and [Used_storage_space]. We do not need
         to delete them. Futhermore, these storages space are
         carbonated, thus, require gas to be deleted (even when they
         do not exist). An implicit contract deletion should not cost
         extra gas. *)
      let* c = Contract_delegate_storage.unlink c contract in
      let update local =
        let* local =
          Storage.Contract.Spendable_balance.Local.remove_existing local
        in
        let* local = Storage.Contract.Manager.Local.remove_existing local in
        Storage.Contract.Counter.Local.remove_existing local
      in
      let+ c, () =
        Storage.Contract.with_local_context c contract (fun local ->
            let+ local = update local in
            (local, ()))
      in
      c

let allocated c contract = Storage.Contract.Spendable_balance.mem c contract

let exists c contract =
  match contract with
  | Contract_repr.Implicit _ -> Lwt.return_true
  | Originated _ -> allocated c contract

let must_exist c contract =
  let open Lwt_syntax in
  let* exists_contract = exists c contract in
  match exists_contract with
  | true -> return_unit
  | false -> tzfail (Non_existing_contract contract)

let must_be_allocated c contract =
  let open Lwt_syntax in
  let* is_allocated = allocated c contract in
  match is_allocated with
  | true -> return_unit
  | false -> (
      match contract with
      | Implicit pkh -> tzfail (Empty_implicit_contract pkh)
      | Originated _ -> tzfail (Non_existing_contract contract))

let fresh_contract_from_current_nonce c =
  let open Result_syntax in
  let+ c, nonce = Raw_context.increment_origination_nonce c in
  (c, Contract_hash.of_nonce nonce)

let originated_from_current_nonce ~since:ctxt_since ~until:ctxt_until =
  let open Lwt_result_syntax in
  let*? since = Raw_context.get_origination_nonce ctxt_since in
  let*? until = Raw_context.get_origination_nonce ctxt_until in
  let*! result =
    List.filter_s
      (fun contract -> exists ctxt_until (Contract_repr.Originated contract))
      (Contract_repr.originated_contracts ~since ~until)
  in
  return result

let check_counter_increment c manager counter =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit manager in
  let* contract_counter = Storage.Contract.Counter.get c contract in
  let expected = Manager_counter_repr.succ contract_counter in
  if Manager_counter_repr.(expected = counter) then return_unit
  else if Manager_counter_repr.(expected > counter) then
    tzfail (Counter_in_the_past {contract; expected; found = counter})
  else tzfail (Counter_in_the_future {contract; expected; found = counter})

let increment_counter c manager =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit manager in
  let* global_counter = Storage.Contract.Global_counter.get c in
  let* c =
    Storage.Contract.Global_counter.update
      c
      (Manager_counter_repr.succ global_counter)
  in
  let* contract_counter = Storage.Contract.Counter.get c contract in
  Storage.Contract.Counter.update
    c
    contract
    (Manager_counter_repr.succ contract_counter)

let get_script_code c contract_hash =
  let contract = Contract_repr.Originated contract_hash in
  Storage.Contract.Code.find c contract

let get_script c contract_hash =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Originated contract_hash in
  let* c, code = Storage.Contract.Code.find c contract in
  let* c, storage = Storage.Contract.Storage.find c contract in
  match (code, storage) with
  | None, None -> return (c, None)
  | Some code, Some storage ->
      return (c, Some (Contract_repr.Script {Script_repr.code; storage}))
  | None, Some storage -> (
      let* c, native_kind = Storage.Contract.Native.find c contract in
      match native_kind with
      | None -> return (c, None)
      | Some native_kind ->
          return (c, Some (Contract_repr.Native {kind = native_kind; storage})))
  (* A contract without storage is an illformed contract. *)
  | Some _, None -> failwith "get_script"

let get_storage ctxt contract_hash =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Originated contract_hash in
  let* result = Storage.Contract.Storage.find ctxt contract in
  match result with
  | ctxt, None -> return (ctxt, None)
  | ctxt, Some storage ->
      let*? ctxt =
        Raw_context.consume_gas ctxt (Script_repr.force_decode_cost storage)
      in
      let*? storage = Script_repr.force_decode storage in
      return (ctxt, Some storage)

let get_counter c manager =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit manager in
  let* counter_opt = Storage.Contract.Counter.find c contract in
  match counter_opt with
  | None -> (
      match contract with
      | Contract_repr.Implicit _ -> Storage.Contract.Global_counter.get c
      | Originated _ -> failwith "get_counter")
  | Some v -> return v

let get_balance c contract =
  let open Lwt_result_syntax in
  let* balance_opt = Storage.Contract.Spendable_balance.find c contract in
  match balance_opt with
  | None -> (
      match contract with
      | Implicit _ -> return Tez_repr.zero
      | Originated _ -> failwith "get_balance")
  | Some v -> return v

let get_balance_carbonated c contract =
  (* Reading an int64 from /contracts/index/<hash>/balance *)
  let open Lwt_result_syntax in
  let*? c =
    Raw_context.consume_gas
      c
      (Storage_costs.read_access ~path_length:4 ~read_bytes:8)
  in
  let* balance = get_balance c contract in
  return (c, balance)

let check_allocated_and_get_balance c pkh =
  let open Lwt_result_syntax in
  let* balance_opt =
    Storage.Contract.Spendable_balance.find c (Contract_repr.Implicit pkh)
  in
  match balance_opt with
  | None -> tzfail (Empty_implicit_contract pkh)
  | Some balance -> return balance

let update_script_storage c contract_hash storage lazy_storage_diff =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Originated contract_hash in
  let storage = Script_repr.lazy_expr storage in
  let* c, lazy_storage_size_diff =
    update_script_lazy_storage c lazy_storage_diff
  in
  let* c, size_diff = Storage.Contract.Storage.update c contract storage in
  let* previous_size = Storage.Contract.Used_storage_space.get c contract in
  let new_size =
    Z.add previous_size (Z.add lazy_storage_size_diff (Z.of_int size_diff))
  in
  Storage.Contract.Used_storage_space.update c contract new_size

let spend_from_balance contract balance amount =
  record_trace
    (Balance_too_low (contract, balance, amount))
    Tez_repr.(balance -? amount)

let check_emptiable c contract =
  let open Lwt_result_syntax in
  match contract with
  | Contract_repr.Originated _ -> return_unit
  | Implicit pkh -> (
      let* delegate = Contract_delegate_storage.find c contract in
      match delegate with
      | Some pkh' ->
          if Signature.Public_key_hash.equal pkh pkh' then return_unit
          else
            (* Delegated implicit accounts cannot be emptied *)
            tzfail (Empty_implicit_delegated_contract pkh)
      | None -> return_unit)

let spend_only_call_from_token c contract amount =
  let open Lwt_result_syntax in
  let* balance = Storage.Contract.Spendable_balance.find c contract in
  let balance = Option.value balance ~default:Tez_repr.zero in
  let*? new_balance = spend_from_balance contract balance amount in
  let* c = Storage.Contract.Spendable_balance.update c contract new_balance in
  let* c = Stake_storage.remove_contract_delegated_stake c contract amount in
  let+ () =
    when_
      Tez_repr.(new_balance <= Tez_repr.zero)
      (fun () -> check_emptiable c contract)
  in
  c

(* [Tez_repr.(amount <> zero)] is a precondition of this function. It ensures that
   no entry associating a null balance to an implicit contract exists in the map
   [Storage.Contract.Spendable_balance]. *)
let credit_only_call_from_token c contract amount =
  let open Lwt_result_syntax in
  let* balance_opt = Storage.Contract.Spendable_balance.find c contract in
  match balance_opt with
  | None -> (
      match contract with
      | Originated _ -> tzfail (Non_existing_contract contract)
      | Implicit manager -> create_implicit c manager ~balance:amount)
  | Some balance ->
      let*? balance = Tez_repr.(amount +? balance) in
      let* c = Storage.Contract.Spendable_balance.update c contract balance in
      Stake_storage.add_contract_delegated_stake c contract amount

let init c =
  let open Lwt_result_syntax in
  let* c = Storage.Contract.Global_counter.init c Manager_counter_repr.init in
  Lazy_storage_diff.init c

let used_storage_space c contract =
  let open Lwt_result_syntax in
  let+ value = Storage.Contract.Used_storage_space.find c contract in
  Option.value ~default:Z.zero value

let paid_storage_space c contract =
  let open Lwt_result_syntax in
  let+ value = Storage.Contract.Paid_storage_space.find c contract in
  Option.value ~default:Z.zero value

let set_paid_storage_space_and_return_fees_to_pay c contract new_storage_space =
  let open Lwt_result_syntax in
  let* already_paid_space =
    Storage.Contract.Paid_storage_space.get c contract
  in
  if Compare.Z.(already_paid_space >= new_storage_space) then return (Z.zero, c)
  else
    let to_pay = Z.sub new_storage_space already_paid_space in
    let+ c =
      Storage.Contract.Paid_storage_space.update c contract new_storage_space
    in
    (to_pay, c)

let increase_paid_storage c contract_hash ~amount_in_bytes:storage_incr =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Originated contract_hash in
  let* already_paid_space =
    Storage.Contract.Paid_storage_space.get c contract
  in
  let new_storage_space = Z.add already_paid_space storage_incr in
  Storage.Contract.Paid_storage_space.update c contract new_storage_space

let get_frozen_bonds ctxt contract =
  let open Lwt_result_syntax in
  let+ value = Storage.Contract.Total_frozen_bonds.find ctxt contract in
  Option.value ~default:Tez_repr.zero value

let get_balance_and_frozen_bonds ctxt contract =
  let open Lwt_result_syntax in
  let* balance = Storage.Contract.Spendable_balance.get ctxt contract in
  let* total_bonds = get_frozen_bonds ctxt contract in
  Lwt.return Tez_repr.(balance +? total_bonds)

let bond_allocated ctxt contract bond_id =
  Storage.Contract.Frozen_bonds.mem (ctxt, contract) bond_id

let find_bond ctxt contract bond_id =
  Storage.Contract.Frozen_bonds.find (ctxt, contract) bond_id

(** PRE : [amount > 0], fulfilled by unique caller [Token.transfer]. *)
let spend_bond_only_call_from_token ctxt contract bond_id amount =
  let open Lwt_result_syntax in
  let* () =
    fail_when Tez_repr.(amount = zero) (Failure "Expecting : [amount > 0]")
  in
  let* ctxt =
    Stake_storage.remove_contract_delegated_stake ctxt contract amount
  in
  let* ctxt, frozen_bonds =
    Storage.Contract.Frozen_bonds.get (ctxt, contract) bond_id
  in
  let*? () =
    error_when
      Tez_repr.(frozen_bonds <> amount)
      (Frozen_bonds_must_be_spent_at_once (contract, bond_id))
  in
  let* ctxt, _ =
    Storage.Contract.Frozen_bonds.remove_existing (ctxt, contract) bond_id
  in
  let* total = Storage.Contract.Total_frozen_bonds.get ctxt contract in
  let*? new_total = Tez_repr.(total -? amount) in
  if Tez_repr.(new_total = zero) then
    Storage.Contract.Total_frozen_bonds.remove_existing ctxt contract
  else Storage.Contract.Total_frozen_bonds.update ctxt contract new_total

(** PRE : [amount > 0], fulfilled by unique caller [Token.transfer]. *)
let credit_bond_only_call_from_token ctxt contract bond_id amount =
  let open Lwt_result_syntax in
  let* () =
    fail_when Tez_repr.(amount = zero) (Failure "Expecting : [amount > 0]")
  in
  let* ctxt = Stake_storage.add_contract_delegated_stake ctxt contract amount in
  let* ctxt, _ =
    let* ctxt, frozen_bonds_opt =
      Storage.Contract.Frozen_bonds.find (ctxt, contract) bond_id
    in
    match frozen_bonds_opt with
    | None -> Storage.Contract.Frozen_bonds.init (ctxt, contract) bond_id amount
    | Some frozen_bonds ->
        let*? new_amount = Tez_repr.(frozen_bonds +? amount) in
        Storage.Contract.Frozen_bonds.update (ctxt, contract) bond_id new_amount
  in
  let* total_opt = Storage.Contract.Total_frozen_bonds.find ctxt contract in
  match total_opt with
  | None -> Storage.Contract.Total_frozen_bonds.init ctxt contract amount
  | Some total ->
      let*? new_total = Tez_repr.(total +? amount) in
      Storage.Contract.Total_frozen_bonds.update ctxt contract new_total

let has_frozen_bonds ctxt contract =
  let open Lwt_result_syntax in
  let*! result = Storage.Contract.Total_frozen_bonds.mem ctxt contract in
  return result

let has_frozen_deposits ctxt contract =
  let open Lwt_result_syntax in
  let* pseudo = Storage.Contract.Staking_pseudotokens.find ctxt contract in
  match pseudo with
  | Some v when not Staking_pseudotoken_repr.(v = zero) -> return_true
  | _ -> (
      let* requests = Storage.Contract.Unstake_requests.find ctxt contract in
      match requests with
      | None | Some {delegate = _; requests = []} -> return_false
      | Some _ -> return_true)

let fold_on_bond_ids ctxt contract =
  Storage.Contract.fold_bond_ids (ctxt, contract)

(** Indicate whether the given implicit contract should avoid deletion
    when it is emptied. *)
let should_keep_empty_implicit_contract ctxt contract =
  let open Lwt_result_syntax in
  let* has_frozen_bonds = has_frozen_bonds ctxt contract in
  let* has_frozen_deposits = has_frozen_deposits ctxt contract in
  if has_frozen_bonds || has_frozen_deposits then return_true
  else
    (* full balance of contract is zero. *)
    let* delegate_opt = Contract_delegate_storage.find ctxt contract in
    match delegate_opt with
    | Some _ ->
        (* Here, we know that the contract delegates to itself.
           Indeed, it does not delegate to a different one, because
           the balance of such contracts cannot be zero (see
           {!spend_only_call_from_token}), hence the stake of such
           contracts cannot be zero either. *)
        return_true
    | None ->
        (* Delete empty implicit contract. *)
        return_false

let ensure_deallocated_if_empty ctxt contract =
  let open Lwt_result_syntax in
  match contract with
  | Contract_repr.Originated _ ->
      return ctxt (* Never delete originated contracts *)
  | Implicit _ -> (
      let* balance_opt =
        Storage.Contract.Spendable_balance.find ctxt contract
      in
      match balance_opt with
      | None ->
          (* Nothing to do, contract is not allocated. *)
          return ctxt
      | Some balance ->
          if Tez_repr.(balance <> zero) then return ctxt
          else
            let* keep_contract =
              should_keep_empty_implicit_contract ctxt contract
            in
            if keep_contract then return ctxt else delete ctxt contract)

let simulate_spending ctxt ~balance ~amount source =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit source in
  let*? new_balance = spend_from_balance contract balance amount in
  let* still_allocated =
    if Tez_repr.(new_balance > zero) then return_true
    else
      let* () = check_emptiable ctxt contract in
      should_keep_empty_implicit_contract ctxt contract
  in
  return (new_balance, still_allocated)

let get_total_supply ctxt = Storage.Contract.Total_supply.get ctxt

module For_RPC = struct
  let list c = Storage.Contract.list c

  let fold c = Storage.Contract.fold c

  let get_staked_balance ctxt =
    let open Lwt_result_syntax in
    function
    | Contract_repr.Originated _ -> return_none
    | Implicit _ as contract -> (
        let* delegate_opt = Storage.Contract.Delegate.find ctxt contract in
        match delegate_opt with
        | None -> return_none
        | Some delegate ->
            let* own_frozen_deposits =
              Staking_pseudotokens_storage.For_RPC.staked_balance
                ctxt
                ~delegate
                ~contract
            in
            return (Some own_frozen_deposits))

  let get_unstaked_balance ctxt =
    let open Lwt_result_syntax in
    function
    | Contract_repr.Originated _ -> return_none
    | Implicit _ as contract -> (
        let* result =
          Unstake_requests_storage.For_RPC.prepare_finalize_unstake
            ctxt
            contract
        in
        match result with
        | None -> return_some (Tez_repr.zero, Tez_repr.zero)
        | Some {finalizable; unfinalizable} ->
            let* unfinalizable_requests =
              Unstake_requests_storage.For_RPC
              .apply_slash_to_unstaked_unfinalizable
                ctxt
                unfinalizable
            in
            let*? sum_unfinalizable =
              List.fold_left_e
                (fun acc (_cycle, tz) -> Tez_repr.(acc +? tz))
                Tez_repr.zero
                unfinalizable_requests
            in
            let*? sum_finalizable =
              List.fold_left_e
                (fun acc (_, _cycle, tz) -> Tez_repr.(acc +? tz))
                Tez_repr.zero
                finalizable
            in
            return_some (sum_unfinalizable, sum_finalizable))

  let get_unstaked_frozen_balance ctxt contract =
    let open Lwt_result_syntax in
    let* balance_opt = get_unstaked_balance ctxt contract in
    match balance_opt with
    | None -> return_none
    | Some (amount, _) -> return_some amount

  let get_unstaked_finalizable_balance ctxt contract =
    let open Lwt_result_syntax in
    let* balance_opt = get_unstaked_balance ctxt contract in
    match balance_opt with
    | None -> return_none
    | Some (_, amount) -> return_some amount

  let get_full_balance ctxt contract =
    let open Lwt_result_syntax in
    let* balance_n_frozen = get_balance_and_frozen_bonds ctxt contract in
    let* s = get_staked_balance ctxt contract in
    let staked = Option.value ~default:Tez_repr.zero s in
    let* us = get_unstaked_balance ctxt contract in
    let u_frozen, u_final =
      Option.value ~default:(Tez_repr.zero, Tez_repr.zero) us
    in
    Tez_repr.(
      let*? x = balance_n_frozen +? staked in
      let*? y = u_frozen +? x in
      let*? z = u_final +? y in
      return z)
end
