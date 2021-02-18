(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

type frozen_balance = {
  deposit : Tez_repr.t;
  fees : Tez_repr.t;
  rewards : Tez_repr.t;
}

let frozen_balance_encoding =
  let open Data_encoding in
  conv
    (fun {deposit; fees; rewards} -> (deposit, fees, rewards))
    (fun (deposit, fees, rewards) -> {deposit; fees; rewards})
    (obj3
       (req "deposit" Tez_repr.encoding)
       (req "fees" Tez_repr.encoding)
       (req "rewards" Tez_repr.encoding))

type error +=
  | (* `Temporary *)
      Already_active of Baker_hash.t
  | (* `Temporary *)
      Already_inactive of Baker_hash.t
  | (* `Temporary *)
      Balance_too_low_for_deposit of {
      baker : Baker_hash.t;
      deposit : Tez_repr.t;
      balance : Tez_repr.t;
    }
  | (* `Permanent *)
      Unregistered_baker of Baker_hash.t
  | (* `Permanent *)
      Already_registered_baker of Baker_hash.t
  | (* `Temporary *)
      Already_used_consensus_key of Signature.Public_key.t
  | (* `Temporary *)
      Allocated_consensus_key_account of
      Signature.Public_key_hash.t
  | (* `Temporary *)
      Baker_accepts_delegations of Baker_hash.t
  | (* `Temporary *)
      Baker_declines_delegations of Baker_hash.t

let () =
  let open Data_encoding in
  register_error_kind
    `Temporary
    ~id:"baker.already_active"
    ~title:"Baker already active"
    ~description:"Useless baker reactivation"
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "The baker %a is still active, no need to activate it"
        Baker_hash.pp
        hash)
    (obj1 (req "hash" Baker_hash.encoding))
    (function Already_active k -> Some k | _ -> None)
    (fun k -> Already_active k) ;
  register_error_kind
    `Temporary
    ~id:"baker.already_inactive"
    ~title:"Baker already inactive"
    ~description:"Useless baker deactivation"
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "The baker %a is already inactive, no need to deactivate it"
        Baker_hash.pp
        hash)
    (obj1 (req "hash" Baker_hash.encoding))
    (function Already_inactive k -> Some k | _ -> None)
    (fun k -> Already_inactive k) ;
  register_error_kind
    `Temporary
    ~id:"baker.balance_too_low_for_deposit"
    ~title:"Balance too low for deposit"
    ~description:"Cannot freeze deposit when the balance is too low"
    ~pp:(fun ppf (baker, balance, deposit) ->
      Format.fprintf
        ppf
        "Baker %a has a too low balance (%a) to deposit %a"
        Baker_hash.pp
        baker
        Tez_repr.pp
        balance
        Tez_repr.pp
        deposit)
    (obj3
       (req "baker" Baker_hash.encoding)
       (req "balance" Tez_repr.encoding)
       (req "deposit" Tez_repr.encoding))
    (function
      | Balance_too_low_for_deposit {baker; balance; deposit} ->
          Some (baker, balance, deposit)
      | _ ->
          None)
    (fun (baker, balance, deposit) ->
      Balance_too_low_for_deposit {baker; balance; deposit}) ;
  register_error_kind
    `Permanent
    ~id:"baker.unregistered_baker"
    ~title:"Unregistered baker"
    ~description:"A contract cannot be delegated to an unregistered baker"
    ~pp:(fun ppf k ->
      Format.fprintf
        ppf
        "The provided baker hash %a is not registered as valid baker."
        Baker_hash.pp
        k)
    (obj1 (req "hash" Baker_hash.encoding))
    (function Unregistered_baker k -> Some k | _ -> None)
    (fun k -> Unregistered_baker k) ;
  register_error_kind
    `Permanent
    ~id:"baker.already_registered_baker"
    ~title:"Already registered baker"
    ~description:"The baker is already registered"
    ~pp:(fun ppf k ->
      Format.fprintf
        ppf
        "The provided baker hash %a is already registered."
        Baker_hash.pp
        k)
    (obj1 (req "hash" Baker_hash.encoding))
    (function Already_registered_baker k -> Some k | _ -> None)
    (fun k -> Already_registered_baker k) ;
  register_error_kind
    `Permanent
    ~id:"baker.already_used_consensus_key"
    ~title:"Already used consensus key"
    ~description:
      "The given baker consensus key is already being used. A unique \
       consensus key must be used."
    ~pp:(fun ppf k ->
      Format.fprintf
        ppf
        "The given baker consensus key %a is already being used. A unique \
         consensus key must be used."
        Signature.Public_key.pp
        k)
    (obj1 (req "key" Signature.Public_key.encoding))
    (function Already_used_consensus_key k -> Some k | _ -> None)
    (fun k -> Already_used_consensus_key k) ;
  register_error_kind
    `Permanent
    ~id:"baker.allocated_consensus_key_account"
    ~title:"The consensus key must be an empty account"
    ~description:
      "The given baker consensus key in not an empty account. A consensus key \
       with an empty account must be used."
    ~pp:(fun ppf k ->
      Format.fprintf
        ppf
        "The given baker consensus key %a in not an empty account. A \
         consensus key with an empty account must be used. Empty the account \
         first, or provide a different key."
        Signature.Public_key_hash.pp
        k)
    (obj1 (req "key" Signature.Public_key_hash.encoding))
    (function Allocated_consensus_key_account k -> Some k | _ -> None)
    (fun k -> Allocated_consensus_key_account k) ;
  register_error_kind
    `Temporary
    ~id:"baker.baker_accepts_delegations"
    ~title:"Baker already accepts delegations"
    ~description:"Useless baker setting to accept delegations"
    ~pp:(fun ppf baker ->
      Format.fprintf
        ppf
        "The baker %a already accepts delegations, no need to set it again"
        Baker_hash.pp
        baker)
    Data_encoding.(obj1 (req "baker" Baker_hash.encoding))
    (function Baker_accepts_delegations baker -> Some baker | _ -> None)
    (fun baker -> Baker_accepts_delegations baker) ;
  register_error_kind
    `Temporary
    ~id:"baker.baker_declines_delegations"
    ~title:"Baker already declines delegations"
    ~description:"Useless baker setting to decline delegations"
    ~pp:(fun ppf baker ->
      Format.fprintf
        ppf
        "The baker %a already declines delegations, no need to set it again"
        Baker_hash.pp
        baker)
    Data_encoding.(obj1 (req "baker" Baker_hash.encoding))
    (function Baker_declines_delegations baker -> Some baker | _ -> None)
    (fun baker -> Baker_declines_delegations baker)

let fresh_baker_from_current_nonce ctxt =
  Lwt.return (Raw_context.increment_origination_nonce ctxt)
  >>=? fun (ctxt, nonce) -> return (ctxt, Contract_repr.baker_from_nonce nonce)

let registered = Storage.Baker.Registered.mem

(* Check that the given consensus key is not yet being used as either active or
   pending consensus key. *)
let must_be_unique_consensus_key :
    Raw_context.t -> Signature.Public_key.t -> unit tzresult Lwt.t =
 fun ctxt key ->
  let pkh = Signature.Public_key.hash key in
  Storage.Baker.Consensus_key_rev.mem ctxt pkh
  >>= fun is_active ->
  fail_when is_active @@ Already_used_consensus_key key
  >>=? fun () ->
  Storage.Baker.Pending_consensus_key.fold
    ctxt
    ~init:(ok ())
    ~f:(fun _ (pending_key, _) _ ->
      fail_when (Signature.Public_key.equal pending_key key)
      @@ Already_used_consensus_key key)

(** Check that the given consensus key isn't associated with an allocated
    implicit account and that the key is unique (no other baker is using it). *)
let check_consensus_key ctxt key =
  let key_hash = Signature.Public_key.hash key in
  Storage.Contract.Balance.find ctxt
  @@ Contract_repr.implicit_contract key_hash
  >>=? (function
         | None ->
             return_unit
         | Some _balance ->
             fail @@ Allocated_consensus_key_account key_hash)
  >>=? fun () -> must_be_unique_consensus_key ctxt key

let register ?baker_hash ?(prepaid_bootstrap_storage = false) ctxt ~balance
    ~threshold ~owner_keys ~consensus_key =
  check_consensus_key ctxt consensus_key
  >>=? fun () ->
  ( match baker_hash with
  | Some baker_hash -> (
      (* The baker hash is only given for bootstrap baker accounts *)
      registered ctxt baker_hash
      >>= function
      | true ->
          fail @@ Already_registered_baker baker_hash
      | false ->
          return (ctxt, baker_hash) )
  | None ->
      (* Generate a new baker hash *)
      fresh_baker_from_current_nonce ctxt )
  >>=? fun (ctxt, baker_hash) ->
  Storage.Baker.Registered.add ctxt baker_hash
  >>= fun ctxt ->
  Roll_storage.Delegate.set_active ctxt baker_hash
  >>=? fun ctxt ->
  (* Baker's balance counts towards their rolls *)
  Roll_storage.Delegate.add_amount ctxt baker_hash balance
  >>=? fun ctxt ->
  let contract = Contract_repr.baker_contract baker_hash in
  Storage.Contract.Balance.init ctxt contract balance
  >>=? fun ctxt ->
  Storage.Baker.Consensus_key.init ctxt baker_hash consensus_key
  >>=? fun ctxt ->
  let current_cycle = (Level_storage.current ctxt).cycle in
  Storage.Baker.Consensus_key.Snapshot.init
    ctxt
    (current_cycle, baker_hash)
    consensus_key
  >>=? fun ctxt ->
  Storage.Baker.Consensus_key_rev.init
    ctxt
    (Signature.Public_key.hash consensus_key)
    baker_hash
  >>=? fun ctxt ->
  (* Initialize the script's storage only, the code is inlined from
     [Baker_script_repr.code]. *)
  let storage = Baker_script_repr.storage ~threshold ~owner_keys in
  Storage.Contract.Storage.init ctxt contract (Script_repr.lazy_expr storage)
  >>=? fun (ctxt, storage_size) ->
  let total_size = Z.of_int storage_size in
  let prepaid_bootstrap_storage =
    if prepaid_bootstrap_storage then total_size else Z.zero
  in
  Storage.Contract.Paid_storage_space.init
    ctxt
    contract
    prepaid_bootstrap_storage
  >>=? fun ctxt ->
  Storage.Contract.Used_storage_space.init ctxt contract total_size
  >>=? fun ctxt ->
  Storage.Contract.Global_counter.get ctxt
  >>=? fun counter ->
  (* The consensus key may sign operations on the baker contract, using the
   counter of the baker contract. *)
  Storage.Contract.Counter.init ctxt contract counter
  >|=? fun ctxt -> (ctxt, baker_hash)

let must_be_registered ctxt baker =
  registered ctxt baker
  >>= fun is_registered -> fail_unless is_registered (Unregistered_baker baker)

let set_active ctxt baker new_active_state =
  must_be_registered ctxt baker
  >>=? fun () ->
  Roll_storage.Delegate.is_inactive ctxt baker
  >>=? fun inactive ->
  match (not inactive, new_active_state) with
  | (true, true) ->
      fail @@ Already_active baker
  | (false, false) ->
      fail @@ Already_inactive baker
  | (false, true) ->
      Roll_storage.Delegate.set_active ctxt baker
  | (true, false) ->
      Roll_storage.Delegate.set_inactive ctxt baker

let toggle_delegations ctxt baker new_delegations_state =
  registered ctxt baker
  >>= fun is_registered ->
  fail_unless is_registered (Unregistered_baker baker)
  >>=? fun () ->
  Storage.Baker.Delegation_decliners.mem ctxt baker
  >>= fun delegation_decliners ->
  match (new_delegations_state, delegation_decliners) with
  | (true, false) ->
      fail @@ Baker_accepts_delegations baker
  | (false, true) ->
      fail @@ Baker_declines_delegations baker
  | (true, _) ->
      Storage.Baker.Delegation_decliners.remove ctxt baker >|= ok
  | (false, _) ->
      Storage.Baker.Delegation_decliners.add ctxt baker >|= ok

let fold = Storage.Baker.Registered.fold

let list = Storage.Baker.Registered.elements

let delegated_contracts ctxt baker =
  Storage.Baker.Delegators.elements (ctxt, baker)

let get_frozen_deposit ctxt contract cycle =
  Storage.Baker.Frozen_deposits.find (ctxt, contract) cycle
  >|=? Option.value ~default:Tez_repr.zero

let credit_frozen_deposit ctxt baker cycle amount =
  get_frozen_deposit ctxt baker cycle
  >>=? fun old_amount ->
  Tez_repr.(old_amount +? amount)
  >>?= fun new_amount ->
  Storage.Baker.Frozen_deposits.add (ctxt, baker) cycle new_amount
  >>= fun ctxt ->
  Storage.Baker.With_frozen_balance.add (ctxt, cycle) baker >|= ok

let freeze_deposit ctxt baker amount =
  let ({Level_repr.cycle; _} : Level_repr.t) = Level_storage.current ctxt in
  Roll_storage.Delegate.set_active ctxt baker
  >>=? fun ctxt ->
  let contract = Contract_repr.baker_contract baker in
  Storage.Contract.Balance.get ctxt contract
  >>=? fun balance ->
  record_trace
    (Balance_too_low_for_deposit {baker; deposit = amount; balance})
    Tez_repr.(balance -? amount)
  >>?= fun new_balance ->
  Storage.Contract.Balance.update ctxt contract new_balance
  >>=? fun ctxt -> credit_frozen_deposit ctxt baker cycle amount

let get_frozen_fees ctxt baker cycle =
  Storage.Baker.Frozen_fees.find (ctxt, baker) cycle
  >|=? Option.value ~default:Tez_repr.zero

let credit_frozen_fees ctxt baker cycle amount =
  get_frozen_fees ctxt baker cycle
  >>=? fun old_amount ->
  Tez_repr.(old_amount +? amount)
  >>?= fun new_amount ->
  Storage.Baker.Frozen_fees.add (ctxt, baker) cycle new_amount
  >>= fun ctxt ->
  Storage.Baker.With_frozen_balance.add (ctxt, cycle) baker >|= ok

let freeze_fees ctxt baker amount =
  let ({Level_repr.cycle; _} : Level_repr.t) = Level_storage.current ctxt in
  Roll_storage.Delegate.add_amount ctxt baker amount
  >>=? fun ctxt -> credit_frozen_fees ctxt baker cycle amount

let burn_fees ctxt baker cycle amount =
  get_frozen_fees ctxt baker cycle
  >>=? fun old_amount ->
  ( match Tez_repr.(old_amount -? amount) with
  | Ok new_amount ->
      Roll_storage.Delegate.remove_amount ctxt baker amount
      >|=? fun ctxt -> (new_amount, ctxt)
  | Error _ ->
      Roll_storage.Delegate.remove_amount ctxt baker old_amount
      >|=? fun ctxt -> (Tez_repr.zero, ctxt) )
  >>=? fun (new_amount, ctxt) ->
  Storage.Baker.Frozen_fees.add (ctxt, baker) cycle new_amount >|= ok

let get_frozen_rewards ctxt baker cycle =
  Storage.Baker.Frozen_rewards.find (ctxt, baker) cycle
  >|=? Option.value ~default:Tez_repr.zero

let credit_frozen_rewards ctxt baker cycle amount =
  get_frozen_rewards ctxt baker cycle
  >>=? fun old_amount ->
  Tez_repr.(old_amount +? amount)
  >>?= fun new_amount ->
  Storage.Baker.Frozen_rewards.add (ctxt, baker) cycle new_amount
  >>= fun ctxt ->
  Storage.Baker.With_frozen_balance.add (ctxt, cycle) baker >|= ok

let freeze_rewards ctxt baker amount =
  let ({Level_repr.cycle; _} : Level_repr.t) = Level_storage.current ctxt in
  credit_frozen_rewards ctxt baker cycle amount

let burn_rewards ctxt baker cycle amount =
  get_frozen_rewards ctxt baker cycle
  >>=? fun old_amount ->
  let new_amount =
    match Tez_repr.(old_amount -? amount) with
    | Error _ ->
        Tez_repr.zero
    | Ok new_amount ->
        new_amount
  in
  Storage.Baker.Frozen_rewards.add (ctxt, baker) cycle new_amount >|= ok

let unfreeze ctxt baker cycle =
  let contract = Contract_repr.baker_contract baker in
  get_frozen_deposit ctxt baker cycle
  >>=? fun deposit ->
  get_frozen_fees ctxt baker cycle
  >>=? fun fees ->
  get_frozen_rewards ctxt baker cycle
  >>=? fun rewards ->
  Storage.Contract.Balance.get ctxt contract
  >>=? fun balance ->
  Tez_repr.(deposit +? fees)
  >>?= fun unfrozen_amount ->
  Tez_repr.(unfrozen_amount +? rewards)
  >>?= fun unfrozen_amount ->
  Tez_repr.(balance +? unfrozen_amount)
  >>?= fun balance ->
  Storage.Contract.Balance.update ctxt contract balance
  >>=? fun ctxt ->
  Roll_storage.Delegate.add_amount ctxt baker rewards
  >>=? fun ctxt ->
  Storage.Baker.Frozen_deposits.remove (ctxt, baker) cycle
  >>= fun ctxt ->
  Storage.Baker.Frozen_fees.remove (ctxt, baker) cycle
  >>= fun ctxt ->
  Storage.Baker.Frozen_rewards.remove (ctxt, baker) cycle
  >|= fun ctxt ->
  ok
    ( ctxt,
      Receipt_repr.cleanup_balance_updates
        [ (Deposits (baker, cycle), Debited deposit, Block_application);
          (Fees (baker, cycle), Debited fees, Block_application);
          (Rewards (baker, cycle), Debited rewards, Block_application);
          (Contract contract, Credited unfrozen_amount, Block_application) ] )

(* Activate pending keys with activation cycle equal to the given cycle.
   This will delete the record from pending keys. *)
let activate_pending_consensus_keys ctxt cycle =
  Storage.Baker.Pending_consensus_key.fold
    ctxt
    ~init:(ok ctxt)
    ~f:(fun baker_hash (consensus_key, activation_cycle) acc ->
      Lwt.return acc
      >>=? fun ctxt ->
      if Cycle_repr.equal cycle activation_cycle then
        Storage.Baker.Consensus_key.update ctxt baker_hash consensus_key
        >>=? fun ctxt ->
        Storage.Baker.Pending_consensus_key.remove ctxt baker_hash >|= ok
      else return ctxt)

(* Take a new snapshot and update the [Consensus_key_rev] storage with the
   current snapshot of every registered baker's consensus key. *)
let snapshot_and_update_consensus_keys ctxt cycle =
  Storage.Baker.Consensus_key.snapshot ctxt cycle
  >>=? fun ctxt ->
  (* We don't clear the [Consensus_key_rev] storage before update, assuming that
     bakers cannot be unregistered. *)
  Storage.Baker.Registered.fold ctxt ~init:(ok ctxt) ~f:(fun baker acc ->
      Lwt.return acc
      >>=? fun ctxt ->
      Storage.Baker.Consensus_key.Snapshot.get ctxt (cycle, baker)
      >>=? fun consensus_key ->
      Storage.Baker.Consensus_key_rev.update
        ctxt
        (Signature.Public_key.hash consensus_key)
        baker)

let init_first_cycles ctxt =
  let cycle = Cycle_repr.root in
  snapshot_and_update_consensus_keys ctxt cycle

let cycle_end ctxt last_cycle unrevealed =
  let preserved = Constants_storage.preserved_cycles ctxt in
  ( match Cycle_repr.sub last_cycle preserved with
  | None ->
      return ctxt
  | Some cleared_cycle ->
      Storage.Baker.Consensus_key.delete_snapshot ctxt cleared_cycle
      >>= fun ctxt -> return ctxt )
  >>=? fun ctxt ->
  let new_cycle = Cycle_repr.succ last_cycle in
  activate_pending_consensus_keys ctxt new_cycle
  >>=? fun ctxt ->
  snapshot_and_update_consensus_keys ctxt new_cycle
  >>=? fun ctxt ->
  ( match Cycle_repr.pred last_cycle with
  | None ->
      return (ctxt, [])
  | Some revealed_cycle ->
      fold_left_s
        (fun (ctxt, balance_updates) (u : Nonce_storage.unrevealed) ->
          burn_fees ctxt u.baker revealed_cycle u.fees
          >>=? fun ctxt ->
          burn_rewards ctxt u.baker revealed_cycle u.rewards
          >|=? fun ctxt ->
          let bus =
            Receipt_repr.
              [ ( Fees (u.baker, revealed_cycle),
                  Debited u.fees,
                  Block_application );
                ( Rewards (u.baker, revealed_cycle),
                  Debited u.rewards,
                  Block_application ) ]
          in
          (ctxt, bus @ balance_updates))
        (ctxt, [])
        unrevealed )
  >>=? fun (ctxt, balance_updates) ->
  match Cycle_repr.sub last_cycle preserved with
  | None ->
      return (ctxt, balance_updates, [])
  | Some unfrozen_cycle ->
      Storage.Baker.With_frozen_balance.fold
        (ctxt, unfrozen_cycle)
        ~init:(Ok (ctxt, balance_updates))
        ~f:(fun baker acc ->
          acc
          >>?= fun (ctxt, bus) ->
          unfreeze ctxt baker unfrozen_cycle
          >|=? fun (ctxt, balance_updates) -> (ctxt, balance_updates @ bus))
      >>=? fun (ctxt, balance_updates) ->
      Storage.Baker.With_frozen_balance.clear (ctxt, unfrozen_cycle)
      >>= fun ctxt ->
      Storage.Baker.Active_with_rolls.fold
        ctxt
        ~init:(Ok (ctxt, []))
        ~f:(fun baker acc ->
          acc
          >>?= fun (ctxt, deactivated) ->
          Storage.Baker.Deactivation.get ctxt baker
          >>=? fun cycle ->
          if Cycle_repr.(cycle <= last_cycle) then
            Roll_storage.Delegate.set_inactive ctxt baker
            >|=? fun ctxt -> (ctxt, baker :: deactivated)
          else return (ctxt, deactivated))
      >|=? fun (ctxt, deactivated) -> (ctxt, balance_updates, deactivated)

let punish ctxt baker cycle =
  get_frozen_deposit ctxt baker cycle
  >>=? fun deposit ->
  get_frozen_fees ctxt baker cycle
  >>=? fun fees ->
  get_frozen_rewards ctxt baker cycle
  >>=? fun rewards ->
  Roll_storage.Delegate.remove_amount ctxt baker deposit
  >>=? fun ctxt ->
  Roll_storage.Delegate.remove_amount ctxt baker fees
  >>=? fun ctxt ->
  (* Rewards are not accounted in the baker's rolls yet... *)
  Storage.Baker.Frozen_deposits.remove (ctxt, baker) cycle
  >>= fun ctxt ->
  Storage.Baker.Frozen_fees.remove (ctxt, baker) cycle
  >>= fun ctxt ->
  Storage.Baker.Frozen_rewards.remove (ctxt, baker) cycle
  >|= fun ctxt -> ok (ctxt, {deposit; fees; rewards})

let has_frozen_balance ctxt baker cycle =
  get_frozen_deposit ctxt baker cycle
  >>=? fun deposit ->
  if Tez_repr.(deposit <> zero) then return_true
  else
    get_frozen_fees ctxt baker cycle
    >>=? fun fees ->
    if Tez_repr.(fees <> zero) then return_true
    else
      get_frozen_rewards ctxt baker cycle
      >|=? fun rewards -> Tez_repr.(rewards <> zero)

let frozen_balance_by_cycle_encoding =
  let open Data_encoding in
  conv
    Cycle_repr.Map.bindings
    (List.fold_left
       (fun m (c, b) -> Cycle_repr.Map.add c b m)
       Cycle_repr.Map.empty)
    (list
       (merge_objs
          (obj1 (req "cycle" Cycle_repr.encoding))
          frozen_balance_encoding))

let empty_frozen_balance =
  {deposit = Tez_repr.zero; fees = Tez_repr.zero; rewards = Tez_repr.zero}

let frozen_balance_by_cycle ctxt baker =
  let map = Cycle_repr.Map.empty in
  Storage.Baker.Frozen_deposits.fold
    (ctxt, baker)
    ~init:map
    ~f:(fun cycle amount map ->
      Lwt.return
        (Cycle_repr.Map.add
           cycle
           {empty_frozen_balance with deposit = amount}
           map))
  >>= fun map ->
  Storage.Baker.Frozen_fees.fold
    (ctxt, baker)
    ~init:map
    ~f:(fun cycle amount map ->
      let balance =
        match Cycle_repr.Map.find_opt cycle map with
        | None ->
            empty_frozen_balance
        | Some balance ->
            balance
      in
      Lwt.return (Cycle_repr.Map.add cycle {balance with fees = amount} map))
  >>= fun map ->
  Storage.Baker.Frozen_rewards.fold
    (ctxt, baker)
    ~init:map
    ~f:(fun cycle amount map ->
      let balance =
        match Cycle_repr.Map.find_opt cycle map with
        | None ->
            empty_frozen_balance
        | Some balance ->
            balance
      in
      Lwt.return (Cycle_repr.Map.add cycle {balance with rewards = amount} map))

let frozen_balance ctxt baker =
  let balance = Ok Tez_repr.zero in
  Storage.Baker.Frozen_deposits.fold
    (ctxt, baker)
    ~init:balance
    ~f:(fun _cycle amount acc ->
      Lwt.return (acc >>? fun acc -> Tez_repr.(acc +? amount)))
  >>= fun balance ->
  Storage.Baker.Frozen_fees.fold
    (ctxt, baker)
    ~init:balance
    ~f:(fun _cycle amount acc ->
      Lwt.return (acc >>? fun acc -> Tez_repr.(acc +? amount)))
  >>= fun balance ->
  Storage.Baker.Frozen_rewards.fold
    (ctxt, baker)
    ~init:balance
    ~f:(fun _cycle amount acc ->
      Lwt.return (acc >>? fun acc -> Tez_repr.(acc +? amount)))

let full_balance ctxt baker =
  let contract = Contract_repr.baker_contract baker in
  frozen_balance ctxt baker
  >>=? fun frozen_balance ->
  Storage.Contract.Balance.get ctxt contract
  >>=? fun balance -> Lwt.return Tez_repr.(frozen_balance +? balance)

let deactivated = Roll_storage.Delegate.is_inactive

let grace_period = Storage.Baker.Deactivation.get

let staking_balance ctxt baker =
  let token_per_rolls = Constants_storage.tokens_per_roll ctxt in
  Roll_storage.count_rolls ctxt baker
  >>=? fun rolls ->
  Roll_storage.get_change ctxt baker
  >>=? fun change ->
  Lwt.return
    ( Tez_repr.(token_per_rolls *? Int64.of_int rolls)
    >>? fun balance -> Tez_repr.(balance +? change) )

let delegated_balance ctxt baker =
  let contract = Contract_repr.baker_contract baker in
  staking_balance ctxt baker
  >>=? fun staking_balance ->
  Storage.Contract.Balance.get ctxt contract
  >>= fun self_staking_balance ->
  Storage.Baker.Frozen_deposits.fold
    (ctxt, baker)
    ~init:self_staking_balance
    ~f:(fun _cycle amount acc ->
      Lwt.return (acc >>? fun acc -> Tez_repr.(acc +? amount)))
  >>= fun self_staking_balance ->
  Storage.Baker.Frozen_fees.fold
    (ctxt, baker)
    ~init:self_staking_balance
    ~f:(fun _cycle amount acc ->
      Lwt.return (acc >>? fun acc -> Tez_repr.(acc +? amount)))
  >>=? fun self_staking_balance ->
  Lwt.return Tez_repr.(staking_balance -? self_staking_balance)

let is_pending_consensus_key ctxt contract =
  match Contract_repr.is_implicit contract with
  | None ->
      return false
  | Some key ->
      Storage.Baker.Pending_consensus_key.fold
        ctxt
        ~init:(ok false)
        ~f:(fun _ (pending_key, _) acc ->
          Lwt.return acc
          >>=? function
          | false
            when Signature.(
                   Public_key.hash pending_key |> Public_key_hash.equal key) ->
              return_true
          | result ->
              return result)

let is_consensus_key = Storage.Baker.Consensus_key_rev.find

let get_consensus_key ?level ?offset ctxt baker =
  let raw_level =
    Option.value ~default:Level_storage.(current ctxt).level level
  in
  let level = Level_storage.from_raw ctxt raw_level ?offset in
  must_be_registered ctxt baker
  >>=? fun () ->
  Storage.Baker.Consensus_key.Snapshot.get ctxt (level.cycle, baker)

let get_pending_consensus_key ctxt baker =
  must_be_registered ctxt baker
  >>=? fun () -> Storage.Baker.Pending_consensus_key.find ctxt baker

(* Set a pending consensus key to be activated on the beginning of the
   [preserved_cycles + 2] cycle from the current cycle. If there already is a
   pending consensus key for the given baker, it will be overridden and the
   activation cycle will reset. *)
let set_consensus_key ctxt baker key =
  check_consensus_key ctxt key
  >>=? fun () ->
  let level = Level_storage.current ctxt in
  let preserved = Constants_storage.preserved_cycles ctxt in
  let activation_cycle = Cycle_repr.add level.cycle (preserved + 2) in
  Storage.Baker.Pending_consensus_key.update ctxt baker (key, activation_cycle)

let init_set_pvss_key = Storage.Baker.Pvss_key.add

let get_pvss_key = Storage.Baker.Pvss_key.find
