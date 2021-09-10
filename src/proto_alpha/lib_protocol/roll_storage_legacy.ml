(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Metastate AG <contact@metastate.ch>                    *)
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
  | (* `Permanent *) Consume_roll_change
  | (* `Permanent *) No_roll_for_delegate
  | (* `Permanent *) No_stake_snapshot_for_cycle of Cycle_repr.t
  | (* `Permanent *) Unregistered_delegate of Signature.Public_key_hash.t

let () =
  let open Data_encoding in
  (* Consume roll change *)
  register_error_kind
    `Permanent
    ~id:"contract.manager.consume_roll_change"
    ~title:"Consume roll change"
    ~description:"Change is not enough to consume a roll."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Not enough change to consume a roll.")
    empty
    (function Consume_roll_change -> Some () | _ -> None)
    (fun () -> Consume_roll_change) ;
  (* No roll for delegate *)
  register_error_kind
    `Permanent
    ~id:"contract.manager.no_roll_for_delegate"
    ~title:"No roll for delegate"
    ~description:"Delegate has no roll."
    ~pp:(fun ppf () -> Format.fprintf ppf "Delegate has no roll.")
    empty
    (function No_roll_for_delegate -> Some () | _ -> None)
    (fun () -> No_roll_for_delegate) ;
  (* No roll snapshot for cycle *)
  register_error_kind
    `Permanent
    ~id:"contract.manager.no_stake_snapshot_for_cycle"
    ~title:"No roll snapshot for cycle"
    ~description:
      "A snapshot of the rolls distribution does not exist for this cycle."
    ~pp:(fun ppf c ->
      Format.fprintf
        ppf
        "A snapshot of the rolls distribution does not exist for cycle %a"
        Cycle_repr.pp
        c)
    (obj1 (req "cycle" Cycle_repr.encoding))
    (function No_stake_snapshot_for_cycle c -> Some c | _ -> None)
    (fun c -> No_stake_snapshot_for_cycle c) ;
  (* Unregistered delegate *)
  register_error_kind
    `Permanent
    ~id:"contract.manager.unregistered_delegate_legacy"
    ~title:"Unregistered delegate"
    ~description:"A contract cannot be delegated to an unregistered delegate"
    ~pp:(fun ppf k ->
      Format.fprintf
        ppf
        "The provided public key (with hash %a) is not registered as valid \
         delegate key."
        Signature.Public_key_hash.pp
        k)
    (obj1 (req "hash" Signature.Public_key_hash.encoding))
    (function Unregistered_delegate k -> Some k | _ -> None)
    (fun k -> Unregistered_delegate k)

let get_contract_delegate ctxt contract =
  Storage.Contract.Delegate.find ctxt contract

let delegate_pubkey ctxt delegate =
  Storage.Contract.Manager.find ctxt (Contract_repr.implicit_contract delegate)
  >>=? function
  | None | Some (Manager_repr.Hash _) -> fail (Unregistered_delegate delegate)
  | Some (Manager_repr.Public_key pk) -> return pk

let fold ctxt ~f init =
  Storage.Roll_legacy.Next.get ctxt >>=? fun last ->
  let[@coq_struct "roll"] rec loop ctxt roll acc =
    if Roll_repr_legacy.(roll = last) then return acc
    else
      Storage.Roll_legacy.Owner.find ctxt roll >>=? function
      | None -> loop ctxt (Roll_repr_legacy.succ roll) acc
      | Some delegate ->
          f roll delegate acc >>=? fun acc ->
          loop ctxt (Roll_repr_legacy.succ roll) acc
  in
  loop ctxt Roll_repr_legacy.first init

let get_change ctxt delegate =
  Storage.Roll_legacy.Delegate_change.find ctxt delegate
  >|=? Option.value ~default:Tez_repr.zero

module Delegate = struct
  let fresh_roll ctxt =
    Storage.Roll_legacy.Next.get ctxt >>=? fun roll ->
    Storage.Roll_legacy.Next.update ctxt (Roll_repr_legacy.succ roll)
    >|=? fun ctxt -> (roll, ctxt)

  let get_limbo_roll ctxt =
    Storage.Roll_legacy.Limbo.find ctxt >>=? function
    | None ->
        fresh_roll ctxt >>=? fun (roll, ctxt) ->
        Storage.Roll_legacy.Limbo.init ctxt roll >|=? fun ctxt -> (roll, ctxt)
    | Some roll -> return (roll, ctxt)

  let consume_roll_change ctxt delegate =
    let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
    Storage.Roll_legacy.Delegate_change.get ctxt delegate >>=? fun change ->
    record_trace Consume_roll_change Tez_repr.(change -? tokens_per_roll)
    >>?= fun new_change ->
    Storage.Roll_legacy.Delegate_change.update ctxt delegate new_change

  let recover_roll_change ctxt delegate =
    let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
    Storage.Roll_legacy.Delegate_change.get ctxt delegate >>=? fun change ->
    Tez_repr.(change +? tokens_per_roll) >>?= fun new_change ->
    Storage.Roll_legacy.Delegate_change.update ctxt delegate new_change

  let pop_roll_from_delegate ctxt delegate =
    recover_roll_change ctxt delegate >>=? fun ctxt ->
    (* beginning:
       delegate : roll -> successor_roll -> ...
       limbo : limbo_head -> ...
    *)
    Storage.Roll_legacy.Limbo.find ctxt >>=? fun limbo_head ->
    Storage.Roll_legacy.Delegate_roll_list.find ctxt delegate >>=? function
    | None -> fail No_roll_for_delegate
    | Some roll ->
        Storage.Roll_legacy.Owner.remove_existing ctxt roll >>=? fun ctxt ->
        Storage.Roll_legacy.Successor.find ctxt roll >>=? fun successor_roll ->
        Storage.Roll_legacy.Delegate_roll_list.add_or_remove
          ctxt
          delegate
          successor_roll
        >>= fun ctxt ->
        (* delegate : successor_roll -> ...
           roll ------^
           limbo : limbo_head -> ... *)
        Storage.Roll_legacy.Successor.add_or_remove ctxt roll limbo_head
        >>= fun ctxt ->
        (* delegate : successor_roll -> ...
           roll ------v
           limbo : limbo_head -> ... *)
        Storage.Roll_legacy.Limbo.add ctxt roll >|= fun ctxt ->
        (* delegate : successor_roll -> ...
           limbo : roll -> limbo_head -> ... *)
        ok (roll, ctxt)

  let create_roll_in_delegate ctxt delegate delegate_pk =
    consume_roll_change ctxt delegate >>=? fun ctxt ->
    (* beginning:
       delegate : delegate_head -> ...
       limbo : roll -> limbo_successor -> ...
    *)
    Storage.Roll_legacy.Delegate_roll_list.find ctxt delegate
    >>=? fun delegate_head ->
    get_limbo_roll ctxt >>=? fun (roll, ctxt) ->
    Storage.Roll_legacy.Owner.init ctxt roll delegate_pk >>=? fun ctxt ->
    Storage.Roll_legacy.Successor.find ctxt roll >>=? fun limbo_successor ->
    Storage.Roll_legacy.Limbo.add_or_remove ctxt limbo_successor >>= fun ctxt ->
    (* delegate : delegate_head -> ...
       roll ------v
       limbo : limbo_successor -> ... *)
    Storage.Roll_legacy.Successor.add_or_remove ctxt roll delegate_head
    >>= fun ctxt ->
    (* delegate : delegate_head -> ...
       roll ------^
       limbo : limbo_successor -> ... *)
    Storage.Roll_legacy.Delegate_roll_list.add ctxt delegate roll
    (* delegate : roll -> delegate_head -> ...
       limbo : limbo_successor -> ... *)
    >|= ok

  let ensure_inited ctxt delegate =
    Storage.Roll_legacy.Delegate_change.mem ctxt delegate >>= function
    | true -> return ctxt
    | false ->
        Storage.Roll_legacy.Delegate_change.init ctxt delegate Tez_repr.zero

  let is_inactive ctxt delegate =
    Storage.Contract.Inactive_delegate.mem
      ctxt
      (Contract_repr.implicit_contract delegate)
    >>= fun inactive ->
    if inactive then return inactive
    else
      Storage.Contract.Delegate_desactivation.find
        ctxt
        (Contract_repr.implicit_contract delegate)
      >|=? function
      | Some last_active_cycle ->
          let ({Level_repr.cycle = current_cycle; _} : Level_repr.t) =
            Raw_context.current_level ctxt
          in
          Cycle_repr.(last_active_cycle < current_cycle)
      | None ->
          (* This case is only when called from `set_active`, when creating
             a contract. *)
          false

  let add_amount ctxt delegate amount =
    ensure_inited ctxt delegate >>=? fun ctxt ->
    let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
    Storage.Roll_legacy.Delegate_change.get ctxt delegate >>=? fun change ->
    Tez_repr.(amount +? change) >>?= fun change ->
    Storage.Roll_legacy.Delegate_change.update ctxt delegate change
    >>=? fun ctxt ->
    delegate_pubkey ctxt delegate >>=? fun delegate_pk ->
    let[@coq_struct "change"] rec loop ctxt change =
      if Tez_repr.(change < tokens_per_roll) then return ctxt
      else
        Tez_repr.(change -? tokens_per_roll) >>?= fun change ->
        create_roll_in_delegate ctxt delegate delegate_pk >>=? fun ctxt ->
        loop ctxt change
    in
    is_inactive ctxt delegate >>=? fun inactive ->
    if inactive then return ctxt
    else
      loop ctxt change >>=? fun ctxt ->
      Storage.Roll_legacy.Delegate_roll_list.find ctxt delegate
      >>=? fun rolls ->
      match rolls with
      | None -> return ctxt
      | Some _ ->
          Storage.Legacy_active_delegates_with_rolls.add ctxt delegate >|= ok

  let remove_amount ctxt delegate amount =
    let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
    let[@coq_struct "change"] rec loop ctxt change =
      if Tez_repr.(amount <= change) then return (ctxt, change)
      else
        pop_roll_from_delegate ctxt delegate >>=? fun (_, ctxt) ->
        Tez_repr.(change +? tokens_per_roll) >>?= fun change -> loop ctxt change
    in
    Storage.Roll_legacy.Delegate_change.get ctxt delegate >>=? fun change ->
    is_inactive ctxt delegate >>=? fun inactive ->
    (if inactive then return (ctxt, change)
    else
      loop ctxt change >>=? fun (ctxt, change) ->
      Storage.Roll_legacy.Delegate_roll_list.find ctxt delegate
      >>=? fun rolls ->
      match rolls with
      | None ->
          Storage.Legacy_active_delegates_with_rolls.remove ctxt delegate
          >|= fun ctxt -> ok (ctxt, change)
      | Some _ -> return (ctxt, change))
    >>=? fun (ctxt, change) ->
    Tez_repr.(change -? amount) >>?= fun change ->
    Storage.Roll_legacy.Delegate_change.update ctxt delegate change

  let set_inactive ctxt delegate =
    ensure_inited ctxt delegate >>=? fun ctxt ->
    let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
    Storage.Roll_legacy.Delegate_change.get ctxt delegate >>=? fun change ->
    Storage.Contract.Inactive_delegate.add
      ctxt
      (Contract_repr.implicit_contract delegate)
    >>= fun ctxt ->
    Storage.Legacy_active_delegates_with_rolls.remove ctxt delegate
    >>= fun ctxt ->
    let[@coq_struct "change"] rec loop ctxt change =
      Storage.Roll_legacy.Delegate_roll_list.find ctxt delegate >>=? function
      | None -> return (ctxt, change)
      | Some _roll ->
          pop_roll_from_delegate ctxt delegate >>=? fun (_, ctxt) ->
          Tez_repr.(change +? tokens_per_roll) >>?= fun change ->
          loop ctxt change
    in
    loop ctxt change >>=? fun (ctxt, change) ->
    Storage.Roll_legacy.Delegate_change.update ctxt delegate change

  let set_active ctxt delegate =
    is_inactive ctxt delegate >>=? fun inactive ->
    let current_cycle = (Raw_context.current_level ctxt).cycle in
    let preserved_cycles = Constants_storage.preserved_cycles ctxt in
    (* When the delegate is new or inactive, she will become active in
       `1+preserved_cycles`, and we allow `preserved_cycles` for the
       delegate to start baking. When the delegate is active, we only
       give her at least `preserved_cycles` after the current cycle
       before to be deactivated. *)
    Storage.Contract.Delegate_desactivation.find
      ctxt
      (Contract_repr.implicit_contract delegate)
    >>=? fun current_expiration ->
    let expiration =
      match current_expiration with
      | None -> Cycle_repr.add current_cycle (1 + (2 * preserved_cycles))
      | Some current_expiration ->
          let delay =
            if inactive then 1 + (2 * preserved_cycles)
            else 1 + preserved_cycles
          in
          let updated = Cycle_repr.add current_cycle delay in
          Cycle_repr.max current_expiration updated
    in
    Storage.Contract.Delegate_desactivation.add
      ctxt
      (Contract_repr.implicit_contract delegate)
      expiration
    >>= fun ctxt ->
    if not inactive then return ctxt
    else
      ensure_inited ctxt delegate >>=? fun ctxt ->
      let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
      Storage.Roll_legacy.Delegate_change.get ctxt delegate >>=? fun change ->
      Storage.Contract.Inactive_delegate.remove
        ctxt
        (Contract_repr.implicit_contract delegate)
      >>= fun ctxt ->
      delegate_pubkey ctxt delegate >>=? fun delegate_pk ->
      let[@coq_struct "change"] rec loop ctxt change =
        if Tez_repr.(change < tokens_per_roll) then return ctxt
        else
          Tez_repr.(change -? tokens_per_roll) >>?= fun change ->
          create_roll_in_delegate ctxt delegate delegate_pk >>=? fun ctxt ->
          loop ctxt change
      in
      loop ctxt change >>=? fun ctxt ->
      Storage.Roll_legacy.Delegate_roll_list.find ctxt delegate
      >>=? fun rolls ->
      match rolls with
      | None -> return ctxt
      | Some _ ->
          Storage.Legacy_active_delegates_with_rolls.add ctxt delegate >|= ok
end

module Contract = struct
  let add_amount c contract amount =
    get_contract_delegate c contract >>=? function
    | None -> return c
    | Some delegate -> Delegate.add_amount c delegate amount

  let remove_amount c contract amount =
    get_contract_delegate c contract >>=? function
    | None -> return c
    | Some delegate -> Delegate.remove_amount c delegate amount
end
