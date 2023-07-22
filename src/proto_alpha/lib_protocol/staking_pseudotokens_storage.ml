(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** {0} Introduction

    This module is responsible for maintaining the
    {!Storage.Contract.Frozen_deposits_pseudotokens} and
    {!Storage.Contract.Costaking_pseudotokens} tables.


    {1} Pseudo-tokens

    These tables are used to keep track of the distribution of frozen
    deposits between a delegate and its costakers. The amounts stored
    in these tables don't have a fixed value in tez, they represent
    shares of the frozen deposits called pseudotokens. Pseudotokens
    are minted when the delegate or a costaker increases its share
    using the stake pseudo-operation; they are burnt when the delegate
    or a costaker decreases its share using the request-unstake
    pseudo-operation. Events which modify uniformly the stake of all
    costakers (reward distribution and slashing) don't lead to minting
    nor burning any pseudotokens; that's the main motivation for using
    these pseudotokens: thanks to them we never need to iterate over
    the costakers (whose number is unbounded).


    {1} Conversion rate:

    The conversion rate between pseudotokens and mutez (the value in
    mutez of a pseudotoken) should be given by the ratio between the
    delegate's current frozen deposits and the total number of
    pseudotokens of the delegate; it's actually the case when this
    total number of pseudotokens is positive. When the total number of
    pseudotokens of a delegate is null, the conversion rate could
    theoretically have any value but extreme values are dangerous
    because of overflows and loss of precision; for these reasons, we
    use one as the conversion rate when the total number of
    pseudotokens is null, which can happen in two situations:

    - the first time a baker modifies its staking balance since the
    migration which created the pseudotoken tables, and

    - when a baker empties its frozen deposits and later receives
    rewards.


    {2} Implementation:

    The {!Storage.Contract.Costaking_pseudotokens} table stores for
    each staker (delegate or costaker) its {i staking balance
    pseudotokens} which is the number of pseudotokens owned by the
    staker.

    The {!Storage.Contract.Frozen_deposits_pseudotokens} table stores
    for each delegate the {i frozen deposits pseudotokens} of the
    delegate which is defined as the sum of all the staking balance
    pseudotokens of the delegate and its costakers.

    For both tables, pseudotokens are represented using the
    [Pseudotoken_repr.t] type which is, like [Tez_repr.t], stored on
    non-negative signed int64.


    {2} Invariants:

    {3} Invariant 1: frozen deposits pseudotokens initialization

      For {!Storage.Contract.Frozen_deposits_pseudotokens}, a missing
      key is equivalent to a value of [0]. This case means that there are
      no pseudotokens, the delegate has no costaker, the conversion rate is [1].

      This state is equivalent to having as many pseudotokens as the tez frozen
      deposits of the delegate, all owned by the delegate.


    {3} Invariant 2: staking balance pseudotokens initialization

      For {!Storage.Contract.Costaking_pseudotokens}, a missing key is
      equivalent to a value of [0].

    {3} Invariant 3: relationship between frozen deposits and staking
    balance pseudotokens

      For a given delegate, their frozen deposits pseudotokens equal
      the sum of all costaking pseudotokens of their delegators
      (including the delegate itself).
*)

(** When a delegate gets totally slashed, the value of its
    pseudotokens becomes 0 and before minting any new token we would
    need to iterate over all costakers to empty their pseudotoken
    balances. We want to avoid iterating over costakers so we forbid
    {b stake} in this case. *)
type error += Cannot_stake_on_fully_slashed_delegate

(** These two types are not exported, they are views to the portions
    of the storage which are relevent in this module when a delegate
    or a staker are considered. *)
type delegate_balances = {
  delegate : Signature.public_key_hash;
  frozen_deposits_tez : Tez_repr.t;
  frozen_deposits_pseudotokens : Staking_pseudotoken_repr.t;
}

type contract_balances = {
  contract : Contract_repr.t;
  pseudotoken_balance : Staking_pseudotoken_repr.t;
  delegate_balances : delegate_balances;
}

(** {0} Functions reading from the storage *)

(** [get_frozen_deposits_tez ctxt delegate] returns the sum of frozen
    deposits, in tez, of the delegate and its costakers.

    Note that [Frozen_deposits_storage.get] is expected to default to
    [0] when the key is missing. *)
let get_frozen_deposits_tez ctxt delegate =
  let open Lwt_result_syntax in
  let+ {current_amount; initial_amount = _} =
    Frozen_deposits_storage.get ctxt (Implicit delegate)
  in
  current_amount

(** [get_frozen_deposits_pseudotokens ctxt delegate] returns the total
    number of pseudotokens in circulation for the given
    [delegate]. This should, by invariant 3 be the sum of the
    costaking balance (in pseudotokens) of the delegate and all its
    delegators.

    To preserve invariant 1, this should be the only function of this
    module reading from the
    {!Storage.Contract.Frozen_deposits_pseudotokens} table. *)
let get_frozen_deposits_pseudotokens ctxt delegate =
  let open Lwt_result_syntax in
  let+ frozen_deposits_pseudotokens_opt =
    Storage.Contract.Frozen_deposits_pseudotokens.find ctxt (Implicit delegate)
  in
  Option.value
    frozen_deposits_pseudotokens_opt
    ~default:Staking_pseudotoken_repr.zero

(** [costaking_pseudotokens_balance ctxt contract] returns
    [contract]'s current costaking balance in pseudotokens.

    [contract] can be either a delegate or a delegator.

    To preserve invariant 2, this should be the only function of this
    module reading from the {!Storage.Contract.Costaking_pseudotokens}
    table.
*)
let costaking_pseudotokens_balance ctxt contract =
  let open Lwt_result_syntax in
  let+ costaking_pseudotokens_opt =
    Storage.Contract.Costaking_pseudotokens.find ctxt contract
  in
  Option.value ~default:Staking_pseudotoken_repr.zero costaking_pseudotokens_opt

(** [get_delegate_balances ctxt delegate] records the frozen deposits
    in tez and pseudotokens of a given delegate.

    Postcondition:
      delegate = result.delegate /\
      get_frozen_deposits_tez ctxt delegate = return result.frozen_deposits_tez /\
      get_frozen_deposits_pseudotokens ctxt delegate = return result.frozen_deposits_pseudotokens
*)
let get_delegate_balances ctxt delegate =
  let open Lwt_result_syntax in
  let* frozen_deposits_tez = get_frozen_deposits_tez ctxt delegate in
  let+ frozen_deposits_pseudotokens =
    get_frozen_deposits_pseudotokens ctxt delegate
  in
  {delegate; frozen_deposits_tez; frozen_deposits_pseudotokens}

(** [get_contract_balances ctxt ~contract ~delegate_balances] enriches
    the [delegate_balances] with [contract]'s pseudotoken balance.

    Precondition:
      unchecked: [contract] delegates to [delegate_balance.delegate]
         (which is considered to be the case when [contract = delegate]) /\
      unchecked: get_delegate_balances ctxt delegate = return delegate_balances
    Postcondition:
      result.contract = contract /\
      result.delegate_balances = delegate_balances /\
      costaking_pseudotoken_balance ctxt contract = return result.pseudotoken_balance
*)
let get_contract_balances ctxt ~contract ~delegate_balances =
  let open Lwt_result_syntax in
  let+ pseudotoken_balance = costaking_pseudotokens_balance ctxt contract in
  {contract; pseudotoken_balance; delegate_balances}

(** [init_pseudotokens ctxt delegate_balances_before] initializes the
    pseudotokens of [delegate_balances_before.delegate] by:
    - assigning the totality of the pseudotokens to the delegate,
    - initializing the conversion rate to 1.

   Precondition:
     unchecked: get_delegate_balances ctxt delegate_balances_before.delegate = return delegate_balances_before /\
     unchecked: invariant3(ctxt) /\
     asserted: delegate_balances_before.frozen_deposits_pseudotokens = 0
   Postcondition:
     result.frozen_deposits_pseudotokens = delegate_balances_before.frozen_deposits_tez /\
     other fields unmodified /\
     get_delegate_balances ctxt result.delegate = return result /\
     invariant3(ctxt)
*)
let init_pseudotokens ctxt delegate_balances_before =
  let open Lwt_result_syntax in
  assert (
    Staking_pseudotoken_repr.(
      delegate_balances_before.frozen_deposits_pseudotokens = zero)) ;
  if Tez_repr.(delegate_balances_before.frozen_deposits_tez = zero) then
    (* Nothing to do in this case, all balances set to 0 is a valid
       init state for pseudotokens. *)
    return (ctxt, delegate_balances_before)
  else
    (* Since delegate_balances_before.frozen_deposits_pseudotokens is
       0, invariant 3 tells us that all pseudotoken balances are empty
       so we don't need to read them. *)
    let initial_frozen_deposits_pseudotokens =
      Staking_pseudotoken_repr.init_of_tez
        delegate_balances_before.frozen_deposits_tez
    in
    let result =
      {
        delegate_balances_before with
        frozen_deposits_pseudotokens = initial_frozen_deposits_pseudotokens;
      }
    in
    let contract = Contract_repr.Implicit delegate_balances_before.delegate in
    let*! ctxt =
      Storage.Contract.Costaking_pseudotokens.add
        ctxt
        contract
        initial_frozen_deposits_pseudotokens
    in
    let*! ctxt =
      Storage.Contract.Frozen_deposits_pseudotokens.add
        ctxt
        contract
        initial_frozen_deposits_pseudotokens
    in
    return (ctxt, result)

(** [mint_pseudotokens ctxt contract_balances_before
    pseudotokens_to_mint] mints [pseudotokens_to_mint] pseudotokens
    and assign them to [contract_balances_before.contract]. Both
    tables are updated to maintain invariant 3.

   Precondition:
     unchecked: get_contract_balances ctxt contract_balances_before.contract = return contract_balances_before /\
     unchecked: invariant3(ctxt)
   Postcondition:
     get_contract_balances ctxt contract_balances_before.contract =
       return {contract_balances_before with
                pseudotoken_balance += pseudotokens_to_mint;
                delegate_balances.frozen_deposits_pseudotokens += pseudotokens_to_mint} /\
     invariant3(ctxt)
*)
let mint_pseudotokens ctxt (contract_balances_before : contract_balances)
    pseudotokens_to_mint =
  let open Lwt_result_syntax in
  let*? new_pseudotoken_balance =
    Staking_pseudotoken_repr.(
      contract_balances_before.pseudotoken_balance +? pseudotokens_to_mint)
  in
  let*? new_delegate_total_frozen_deposits_pseudotokens =
    Staking_pseudotoken_repr.(
      contract_balances_before.delegate_balances.frozen_deposits_pseudotokens
      +? pseudotokens_to_mint)
  in
  let*! ctxt =
    Storage.Contract.Costaking_pseudotokens.add
      ctxt
      contract_balances_before.contract
      new_pseudotoken_balance
  in
  let*! ctxt =
    Storage.Contract.Frozen_deposits_pseudotokens.add
      ctxt
      (Implicit contract_balances_before.delegate_balances.delegate)
      new_delegate_total_frozen_deposits_pseudotokens
  in
  return ctxt

(** [burn_pseudotokens ctxt contract_balances_before
    pseudotokens_to_burn] burns [pseudotokens_to_burn] pseudotokens
    from the balance of [contract_balances_before.contract]. Both
    tables are updated to maintain invariant 3.

   Precondition:
     unchecked: get_contract_balances ctxt contract_balances_before.contract = return contract_balances_before /\
     unchecked: invariant3(ctxt)
   Postcondition:
     get_contract_balances ctxt contract_balances_before.contract =
       return {contract_balances_before with
                pseudotoken_balance -= pseudotokens_to_mint;
                delegate_balances.frozen_deposits_pseudotokens -= pseudotokens_to_mint} /\
     invariant3(ctxt)
*)
let burn_pseudotokens ctxt (contract_balances_before : contract_balances)
    pseudotokens_to_burn =
  let open Lwt_result_syntax in
  let*? new_pseudotoken_balance =
    Staking_pseudotoken_repr.(
      contract_balances_before.pseudotoken_balance -? pseudotokens_to_burn)
  in
  let*? new_delegate_total_frozen_deposits_pseudotokens =
    Staking_pseudotoken_repr.(
      contract_balances_before.delegate_balances.frozen_deposits_pseudotokens
      -? pseudotokens_to_burn)
  in
  let*! ctxt =
    Storage.Contract.Costaking_pseudotokens.add
      ctxt
      contract_balances_before.contract
      new_pseudotoken_balance
  in
  let*! ctxt =
    Storage.Contract.Frozen_deposits_pseudotokens.add
      ctxt
      (Implicit contract_balances_before.delegate_balances.delegate)
      new_delegate_total_frozen_deposits_pseudotokens
  in
  return ctxt

(** {0} Conversion between tez and pseudotokens *)

(** Tez -> pseudotokens conversion.
    Precondition:
      tez_amount <> 0 /\
      delegate_balances.frozen_deposits_pseudotokens <> 0 /\
      delegate_balances.frozen_deposits_tez <> 0.
    Postcondition:
      result <> 0.
*)
let pseudotokens_of (delegate_balances : delegate_balances) tez_amount =
  assert (
    Staking_pseudotoken_repr.(
      delegate_balances.frozen_deposits_pseudotokens <> zero)) ;
  assert (Tez_repr.(delegate_balances.frozen_deposits_tez <> zero)) ;
  assert (Tez_repr.(tez_amount <> zero)) ;
  let frozen_deposits_tez_z =
    Z.of_int64 (Tez_repr.to_mutez delegate_balances.frozen_deposits_tez)
  in
  let frozen_deposits_pseudotokens_z =
    Staking_pseudotoken_repr.to_z delegate_balances.frozen_deposits_pseudotokens
  in
  let tez_amount_z = Z.of_int64 (Tez_repr.to_mutez tez_amount) in
  let res_z =
    Z.div
      (Z.mul tez_amount_z frozen_deposits_pseudotokens_z)
      frozen_deposits_tez_z
  in
  Staking_pseudotoken_repr.of_z_exn res_z

(** Pseudotokens -> tez conversion.
    Precondition:
      delegate_balances.frozen_deposits_pseudotokens <> 0.
*)
let tez_of (delegate_balances : delegate_balances) pseudotoken_amount =
  assert (
    Staking_pseudotoken_repr.(
      delegate_balances.frozen_deposits_pseudotokens <> zero)) ;
  let frozen_deposits_tez_z =
    Z.of_int64 (Tez_repr.to_mutez delegate_balances.frozen_deposits_tez)
  in
  let frozen_deposits_pseudotokens_z =
    Staking_pseudotoken_repr.to_z delegate_balances.frozen_deposits_pseudotokens
  in
  let pseudotoken_amount_z = Staking_pseudotoken_repr.to_z pseudotoken_amount in
  let res_z =
    Z.div
      (Z.mul frozen_deposits_tez_z pseudotoken_amount_z)
      frozen_deposits_pseudotokens_z
  in
  Tez_repr.of_mutez_exn (Z.to_int64 res_z)

(** [compute_pseudotoken_credit_for_tez_amount delegate_balances
    tez_amount] is a safe wrapper around [pseudotokens_of
    delegate_balances tez_amount].
*)
let compute_pseudotoken_credit_for_tez_amount delegate_balances tez_amount =
  let open Result_syntax in
  if Tez_repr.(tez_amount = zero) then
    (* This is dead code because Apply.apply_stake already forbids the
       amount=0 case. We keep this dead code here to avoid putting too
       many preconditions on the usage of this module. *)
    return Staking_pseudotoken_repr.zero
  else if
    Staking_pseudotoken_repr.(
      delegate_balances.frozen_deposits_pseudotokens = zero)
  then
    (* Pseudotokens are not yet initialized, the conversion rate is
       1. *)
    return @@ Staking_pseudotoken_repr.init_of_tez tez_amount
  else if Tez_repr.(delegate_balances.frozen_deposits_tez = zero) then
    (* Can only happen in an attempt to stake after a full
       slashing. We forbid this case to avoid having to iterate over
       all costakers to reset their pseudotoken balances. *)
    tzfail Cannot_stake_on_fully_slashed_delegate
  else return @@ pseudotokens_of delegate_balances tez_amount

(** {0} Exported functions, see the mli file. *)
let stake ctxt ~contract ~delegate tez_amount =
  let open Lwt_result_syntax in
  let* delegate_balances = get_delegate_balances ctxt delegate in
  let* ctxt, delegate_balances =
    if
      Staking_pseudotoken_repr.(
        delegate_balances.frozen_deposits_pseudotokens = zero)
    then init_pseudotokens ctxt delegate_balances
    else return (ctxt, delegate_balances)
  in
  let*? pseudotokens_to_credit =
    compute_pseudotoken_credit_for_tez_amount delegate_balances tez_amount
  in
  let* contract_balances =
    get_contract_balances ctxt ~contract ~delegate_balances
  in
  mint_pseudotokens ctxt contract_balances pseudotokens_to_credit

let request_unstake ctxt ~contract ~delegate requested_amount =
  let open Lwt_result_syntax in
  if Tez_repr.(requested_amount = zero) then return (ctxt, Tez_repr.zero)
  else
    let* delegate_balances = get_delegate_balances ctxt delegate in
    if Tez_repr.(delegate_balances.frozen_deposits_tez = zero) then
      return (ctxt, Tez_repr.zero)
    else
      let* contract_balances =
        get_contract_balances ctxt ~contract ~delegate_balances
      in
      if
        Staking_pseudotoken_repr.(
          delegate_balances.frozen_deposits_pseudotokens = zero)
      then (
        (* [delegate] must be non-costaked and have their pseudotokens
           non-initialized.
           Either the request is from a delegator with zero costake (hence
           nothing to unstake) or from the delegate themself and there is no
           need to initialize their pseudotokens. *)
        assert (
          Staking_pseudotoken_repr.(
            contract_balances.pseudotoken_balance = zero)) ;
        if Contract_repr.(contract = Implicit delegate) then
          return
            ( ctxt,
              Tez_repr.min
                delegate_balances.frozen_deposits_tez
                requested_amount )
        else return (ctxt, Tez_repr.zero))
      else if
        Staking_pseudotoken_repr.(contract_balances.pseudotoken_balance = zero)
      then return (ctxt, Tez_repr.zero)
      else
        let pseudotokens_to_unstake, tez_to_unstake =
          if
            Tez_repr.(requested_amount >= delegate_balances.frozen_deposits_tez)
          then
            (* definitely a full unstake, make sure we can empty the costaking
               balance *)
            ( contract_balances.pseudotoken_balance,
              if
                Staking_pseudotoken_repr.(
                  delegate_balances.frozen_deposits_pseudotokens
                  = contract_balances.pseudotoken_balance)
              then
                (* ...and the frozen deposits if from last staker *)
                delegate_balances.frozen_deposits_tez
              else
                tez_of delegate_balances contract_balances.pseudotoken_balance
            )
          else
            let requested_pseudotokens =
              pseudotokens_of delegate_balances requested_amount
            in
            assert (Staking_pseudotoken_repr.(requested_pseudotokens <> zero)) ;
            (* by postcondition of pseudotokens_of *)
            if
              Staking_pseudotoken_repr.(
                requested_pseudotokens < contract_balances.pseudotoken_balance)
            then (requested_pseudotokens, requested_amount)
            else
              ( contract_balances.pseudotoken_balance,
                tez_of delegate_balances contract_balances.pseudotoken_balance
              )
        in
        let+ ctxt =
          burn_pseudotokens ctxt contract_balances pseudotokens_to_unstake
        in
        (ctxt, tez_to_unstake)

let costaking_balance_as_tez ctxt ~contract ~delegate =
  let open Lwt_result_syntax in
  let* delegate_balances = get_delegate_balances ctxt delegate in
  let* contract_balances =
    get_contract_balances ctxt ~contract ~delegate_balances
  in
  if
    Staking_pseudotoken_repr.(
      delegate_balances.frozen_deposits_pseudotokens <> zero)
  then return @@ tez_of delegate_balances contract_balances.pseudotoken_balance
  else (
    assert (
      Staking_pseudotoken_repr.(contract_balances.pseudotoken_balance = zero)) ;
    (* By invariant 3, pseudotoken_balance <= frozen_deposits_pseudotokens = 0 *)
    (* There are no pseudotokens, the delegate owns the totality of
       the frozen deposits. *)
    if Contract_repr.(contract = Implicit delegate) then
      return delegate_balances.frozen_deposits_tez
    else return Tez_repr.zero)
