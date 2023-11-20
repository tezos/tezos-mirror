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
    {!Storage.Contract.Staking_pseudotokens} tables.

    {1} Terminology

    Even so a registered delegate is always technically in a delegation
    relation with itself, in this module, when we use the word
    "delegator", we always mean a delegator different from the
    delegate itself. The word "staker" means for a delegator who
    participates in staking. The word "staker" means any participant
    in staking, either a delegate or a staker. In this module, we
    use the word "contract" to mean either a delegate or a delegator.

    {1} Full staking balance of a delegate

    For each delegate, the {!Stake_storage} module is responsible to
    track three tez quantities which can be requested with the
    {!Stake_storage.get_full_staking_balance} function: [own_frozen]
    is the frozen deposits of the delegate, [staked_frozen] is the
    sum of all frozen deposits of its stakers, and [delegate] is the
    sum of all tez delegated to the delegate (some of which may belong
    to the delegate itself). This module is in charge of tracking the
    frozen deposits of each staker. Since we already have access to
    their sum ([staked_frozen]) we only need to track the proportion
    of this sum owned by each staker.

    {1} Pseudo-tokens

    The {!Storage.Contract.Frozen_deposits_pseudotokens} and
    {!Storage.Contract.Staking_pseudotokens} tables are used to keep
    track of this proportion. The amounts stored in these tables don't
    have a fixed value in tez, they can be seen as shares of the total
    frozen deposits of a delegate's stakers, we call them
    pseudotokens. Pseudotokens are minted when a staker increases
    its share using the stake pseudo-operation; they are burnt when a
    staker decreases its share using the request-unstake
    pseudo-operation. Events which modify uniformly the frozen
    deposits of all the stakers of a delegate (reward distribution
    and slashing) don't lead to minting nor burning any pseudotokens;
    that's the main motivation for using these pseudotokens: thanks to
    them we never need to iterate over the stakers of a delegate
    (whose number is unbounded).


    {1} Conversion rate:

    The conversion rate between pseudotokens and mutez (the value in
    mutez of a pseudotoken) should be given by the ratio between the
    delegate's current staked frozen deposits and the total number
    of pseudotokens of the delegate; it's actually the case when this
    total number of pseudotokens is positive. When the total number of
    pseudotokens of a delegate is null, the conversion rate could
    theoretically have any value but extreme values are dangerous
    because of overflows and loss of precision; for these reasons, we
    use one as the conversion rate when the total number of
    pseudotokens is null, which can happen in two situations:

    - the first time a delegator stakes since the
    migration which created the pseudotoken tables, and

    - when stakers empty their delegate's staked frozen deposits and later
    receive rewards.


    {2} Implementation:

    The {!Storage.Contract.Staking_pseudotokens} table stores for
    each staker its {i staking balance pseudotokens} which is the
    number of pseudotokens owned by the staker.

    The {!Storage.Contract.Frozen_deposits_pseudotokens} table stores
    for each delegate the {i frozen deposits pseudotokens} of the
    delegate which is defined as the sum of all the staking balance
    pseudotokens of its stakers.

    For both tables, pseudotokens are represented using the
    [Pseudotoken_repr.t] type which is, like [Tez_repr.t], stored on
    non-negative signed int64.


    {2} Invariants:

    {3} Invariant 1: frozen deposits pseudotokens initialization

      For {!Storage.Contract.Frozen_deposits_pseudotokens}, a missing
      key is equivalent to a value of [0]. This case means that there are
      no pseudotokens, the delegate has no staker, the conversion rate is [1].

    {3} Invariant 2: staking balance pseudotokens initialization

      For {!Storage.Contract.Staking_pseudotokens}, a missing key is
      equivalent to a value of [0].

    {3} Invariant 3: relationship between frozen deposits and staking
    balance pseudotokens

      For a given delegate, their frozen deposits pseudotokens equal
      the sum of all staking pseudotokens of their delegators.

    {3} Invariant 4: delegates have no staking pseudotokens.
*)

(** When a delegate gets totally slashed, the value of its
    pseudotokens becomes 0 and before minting any new token we would
    need to iterate over all stakers to empty their pseudotoken
    balances. We want to avoid iterating over stakers so we forbid
    {b stake} in this case. *)
type error += Cannot_stake_on_fully_slashed_delegate

(** These two types are not exported, they are views to the portions
    of the storage which are relevant in this module when a delegate
    or a staker are considered. *)
type delegate_balances = {
  delegate : Signature.public_key_hash;
  frozen_deposits_staked_tez : Tez_repr.t;
  frozen_deposits_pseudotokens : Staking_pseudotoken_repr.t;
}

type delegator_balances = {
  delegator : Contract_repr.t;
  pseudotoken_balance : Staking_pseudotoken_repr.t;
  delegate_balances : delegate_balances;
}

(** {0} Functions reading from the storage *)

(** [get_frozen_deposits_staked_tez ctxt ~delegate] returns the sum of frozen
    deposits, in tez, of the delegate's stakers. *)
let get_frozen_deposits_staked_tez ctxt ~delegate =
  let open Lwt_result_syntax in
  let+ {staked_frozen; delegated = _; own_frozen = _} =
    Stake_storage.get_full_staking_balance ctxt delegate
  in
  staked_frozen

let get_own_frozen_deposits ctxt ~delegate =
  let open Lwt_result_syntax in
  let+ {own_frozen; delegated = _; staked_frozen = _} =
    Stake_storage.get_full_staking_balance ctxt delegate
  in
  own_frozen

(** [get_frozen_deposits_pseudotokens ctxt ~delegate] returns the total
    number of pseudotokens in circulation for the given
    [delegate]. This should, by invariant 3 be the sum of the
    staking balance (in pseudotokens) of all its delegators.

    To preserve invariant 1, this should be the only function of this
    module reading from the
    {!Storage.Contract.Frozen_deposits_pseudotokens} table. *)
let get_frozen_deposits_pseudotokens ctxt ~delegate =
  let open Lwt_result_syntax in
  let+ frozen_deposits_pseudotokens_opt =
    Storage.Contract.Frozen_deposits_pseudotokens.find ctxt (Implicit delegate)
  in
  Option.value
    frozen_deposits_pseudotokens_opt
    ~default:Staking_pseudotoken_repr.zero

(** [staking_pseudotokens_balance ctxt ~delegator] returns
    [delegator]'s current staking balance in pseudotokens.

    To preserve invariant 2, this should be the only function of this
    module reading from the {!Storage.Contract.Staking_pseudotokens}
    table.
*)
let staking_pseudotokens_balance ctxt ~delegator =
  let open Lwt_result_syntax in
  let+ staking_pseudotokens_opt =
    Storage.Contract.Staking_pseudotokens.find ctxt delegator
  in
  Option.value ~default:Staking_pseudotoken_repr.zero staking_pseudotokens_opt

(** [get_delegate_balances ctxt ~delegate] records the staked frozen deposits
    in tez and pseudotokens of a given delegate.

    Postcondition:
      delegate = result.delegate /\
      get_frozen_deposits_staked_tez ctxt ~delegate = return result.frozen_deposits_staked_tez /\
      get_frozen_deposits_pseudotokens ctxt ~delegate = return result.frozen_deposits_pseudotokens
*)
let get_delegate_balances ctxt ~delegate =
  let open Lwt_result_syntax in
  let* frozen_deposits_staked_tez =
    get_frozen_deposits_staked_tez ctxt ~delegate
  in
  let+ frozen_deposits_pseudotokens =
    get_frozen_deposits_pseudotokens ctxt ~delegate
  in
  {delegate; frozen_deposits_staked_tez; frozen_deposits_pseudotokens}

(** [get_delegator_balances ctxt ~delegator ~delegate_balances] enriches
    the [delegate_balances] with [delegator]'s pseudotoken balance.

    Precondition:
      unchecked: [delegator != delegate_balance.delegate] /\
      unchecked: [delegator] delegates to [delegate_balance.delegate]
      unchecked: get_delegate_balances ctxt ~delegate = return delegate_balances
    Postcondition:
      result.delegator = delegator /\
      result.delegate_balances = delegate_balances /\
      staking_pseudotokens_balance ctxt ~delegator = return result.pseudotoken_balance
*)
let get_delegator_balances ctxt ~delegator ~delegate_balances =
  let open Lwt_result_syntax in
  let+ pseudotoken_balance = staking_pseudotokens_balance ctxt ~delegator in
  {delegator; pseudotoken_balance; delegate_balances}

(** [mint_pseudotokens ctxt delegator_balances_before
    pseudotokens_to_mint] mints [pseudotokens_to_mint] pseudotokens
    and assign them to [delegator_balances_before.delegator]. Both
    tables are updated to maintain invariant 3.

   Precondition:
     unchecked: get_delegator_balances ctxt delegator_balances_before.delegator = return delegator_balances_before /\
     unchecked: invariant3(ctxt)
   Postcondition:
     get_delegator_balances ctxt delegator_balances_before.delegator =
       return {delegator_balances_before with
                pseudotoken_balance += pseudotokens_to_mint;
                delegate_balances.frozen_deposits_pseudotokens += pseudotokens_to_mint} /\
     invariant3(ctxt)
*)
let mint_pseudotokens ctxt (delegator_balances_before : delegator_balances)
    pseudotokens_to_mint =
  let open Lwt_result_syntax in
  let delegator = delegator_balances_before.delegator in
  let delegate = delegator_balances_before.delegate_balances.delegate in
  let*? new_pseudotoken_balance =
    Staking_pseudotoken_repr.(
      delegator_balances_before.pseudotoken_balance +? pseudotokens_to_mint)
  in
  let*? new_delegate_total_frozen_deposits_pseudotokens =
    Staking_pseudotoken_repr.(
      delegator_balances_before.delegate_balances.frozen_deposits_pseudotokens
      +? pseudotokens_to_mint)
  in
  let*! ctxt =
    Storage.Contract.Staking_pseudotokens.add
      ctxt
      delegator
      new_pseudotoken_balance
  in
  let*! ctxt =
    Storage.Contract.Frozen_deposits_pseudotokens.add
      ctxt
      (Implicit delegate)
      new_delegate_total_frozen_deposits_pseudotokens
  in
  let balance_updates =
    Receipt_repr.
      [
        item
          (Staking_delegator_numerator {delegator})
          (Credited pseudotokens_to_mint)
          Block_application;
        item
          (Staking_delegate_denominator {delegate})
          (Credited pseudotokens_to_mint)
          Block_application;
      ]
  in
  return (ctxt, balance_updates)

(** [burn_pseudotokens ctxt delegator_balances_before
    pseudotokens_to_burn] burns [pseudotokens_to_burn] pseudotokens
    from the balance of [delegator_balances_before.delegator]. Both
    tables are updated to maintain invariant 3.

   Precondition:
     unchecked: get_delegator_balances ctxt delegator_balances_before.delegator = return delegator_balances_before /\
     unchecked: invariant3(ctxt)
   Postcondition:
     get_delegator_balances ctxt delegator_balances_before.delegator =
       return {delegator_balances_before with
                pseudotoken_balance -= pseudotokens_to_mint;
                delegate_balances.frozen_deposits_pseudotokens -= pseudotokens_to_mint} /\
     invariant3(ctxt)
*)
let burn_pseudotokens ctxt (delegator_balances_before : delegator_balances)
    pseudotokens_to_burn =
  let open Lwt_result_syntax in
  let delegator = delegator_balances_before.delegator in
  let delegate = delegator_balances_before.delegate_balances.delegate in
  let*? new_pseudotoken_balance =
    Staking_pseudotoken_repr.(
      delegator_balances_before.pseudotoken_balance -? pseudotokens_to_burn)
  in
  let*? new_delegate_total_frozen_deposits_pseudotokens =
    Staking_pseudotoken_repr.(
      delegator_balances_before.delegate_balances.frozen_deposits_pseudotokens
      -? pseudotokens_to_burn)
  in
  let*! ctxt =
    Storage.Contract.Staking_pseudotokens.add
      ctxt
      delegator
      new_pseudotoken_balance
  in
  let*! ctxt =
    Storage.Contract.Frozen_deposits_pseudotokens.add
      ctxt
      (Implicit delegate)
      new_delegate_total_frozen_deposits_pseudotokens
  in
  let balance_updates =
    Receipt_repr.
      [
        item
          (Staking_delegate_denominator {delegate})
          (Debited pseudotokens_to_burn)
          Block_application;
        item
          (Staking_delegator_numerator {delegator})
          (Debited pseudotokens_to_burn)
          Block_application;
      ]
  in
  return (ctxt, balance_updates)

(** {0} Conversion between tez and pseudotokens *)

(** Tez -> pseudotokens conversion.
    Precondition:
      tez_amount <> 0 /\
      delegate_balances.frozen_deposits_pseudotokens <> 0 /\
      delegate_balances.frozen_deposits_staked_tez <> 0.
    Postcondition:
      result <> 0.
*)
let pseudotokens_of ~rounding (delegate_balances : delegate_balances) tez_amount
    =
  assert (
    Staking_pseudotoken_repr.(
      delegate_balances.frozen_deposits_pseudotokens <> zero)) ;
  assert (Tez_repr.(delegate_balances.frozen_deposits_staked_tez <> zero)) ;
  assert (Tez_repr.(tez_amount <> zero)) ;
  Staking_pseudotoken_repr.mul_ratio
    ~rounding
    delegate_balances.frozen_deposits_pseudotokens
    ~num:(Tez_repr.to_mutez tez_amount)
    ~den:(Tez_repr.to_mutez delegate_balances.frozen_deposits_staked_tez)

(** Pseudotokens -> tez conversion.
    Precondition:
      delegate_balances.frozen_deposits_pseudotokens <> 0.
*)
let tez_of ~rounding (delegate_balances : delegate_balances) pseudotoken_amount
    =
  assert (
    Staking_pseudotoken_repr.(
      delegate_balances.frozen_deposits_pseudotokens <> zero)) ;
  Tez_repr.mul_ratio
    ~rounding
    delegate_balances.frozen_deposits_staked_tez
    ~num:(Staking_pseudotoken_repr.to_int64 pseudotoken_amount)
    ~den:
      (Staking_pseudotoken_repr.to_int64
         delegate_balances.frozen_deposits_pseudotokens)

(** [compute_pseudotoken_credit_for_tez_amount delegate_balances
    tez_amount] is a safe wrapper around [pseudotokens_of
    delegate_balances tez_amount].

    Rounding disadvantages newcomer (gets less pseudotokens for the same tez
    value).
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
  else if Tez_repr.(delegate_balances.frozen_deposits_staked_tez = zero) then
    (* Can only happen in an attempt to stake after a full
       slashing. We forbid this case to avoid having to iterate over
       all stakers to reset their pseudotoken balances. *)
    tzfail Cannot_stake_on_fully_slashed_delegate
  else pseudotokens_of ~rounding:`Down delegate_balances tez_amount

let stake ctxt ~delegator ~delegate tez_amount =
  let open Lwt_result_syntax in
  let* delegate_balances = get_delegate_balances ctxt ~delegate in
  let*? pseudotokens_to_credit =
    compute_pseudotoken_credit_for_tez_amount delegate_balances tez_amount
  in
  let* delegator_balances =
    get_delegator_balances ctxt ~delegator ~delegate_balances
  in
  mint_pseudotokens ctxt delegator_balances pseudotokens_to_credit

(** {0} Exported functions, see the mli file. *)
let stake ctxt ~contract ~delegate tez_amount =
  if Contract_repr.(contract = Implicit delegate) then
    (* No pseudotokens for delegates. *)
    Lwt_result_syntax.return (ctxt, [])
  else stake ctxt ~delegator:contract ~delegate tez_amount

(*
   Rounding disadvantages unstaker. *)
let request_unstake ctxt ~delegator ~delegate requested_amount =
  let open Lwt_result_syntax in
  let* delegate_balances = get_delegate_balances ctxt ~delegate in
  if Tez_repr.(delegate_balances.frozen_deposits_staked_tez = zero) then
    return (ctxt, Tez_repr.zero, [])
  else
    let* delegator_balances =
      get_delegator_balances ctxt ~delegator ~delegate_balances
    in
    if Staking_pseudotoken_repr.(delegator_balances.pseudotoken_balance = zero)
    then return (ctxt, Tez_repr.zero, [])
    else (
      assert (
        Staking_pseudotoken_repr.(
          delegate_balances.frozen_deposits_pseudotokens <> zero)) ;
      let*? pseudotokens_to_unstake, tez_to_unstake =
        let open Result_syntax in
        if
          Tez_repr.(
            requested_amount >= delegate_balances.frozen_deposits_staked_tez)
        then
          (* definitely a full unstake, make sure we can empty the staking
             balance *)
          let+ tez_to_unstake =
            if
              Staking_pseudotoken_repr.(
                delegate_balances.frozen_deposits_pseudotokens
                = delegator_balances.pseudotoken_balance)
            then
              (* ...and the frozen deposits if from last staker *)
              Ok delegate_balances.frozen_deposits_staked_tez
            else
              tez_of
                ~rounding:`Down
                delegate_balances
                delegator_balances.pseudotoken_balance
          in
          (delegator_balances.pseudotoken_balance, tez_to_unstake)
        else
          let* requested_pseudotokens =
            pseudotokens_of ~rounding:`Up delegate_balances requested_amount
          in
          assert (Staking_pseudotoken_repr.(requested_pseudotokens <> zero)) ;
          (* by postcondition of pseudotokens_of *)
          let pseudotokens_to_unstake =
            Staking_pseudotoken_repr.min
              requested_pseudotokens
              delegator_balances.pseudotoken_balance
          in
          let+ tez_to_unstake =
            tez_of ~rounding:`Down delegate_balances pseudotokens_to_unstake
          in
          (pseudotokens_to_unstake, tez_to_unstake)
      in
      let+ ctxt, balance_updates =
        burn_pseudotokens ctxt delegator_balances pseudotokens_to_unstake
      in
      (ctxt, tez_to_unstake, balance_updates))

let request_unstake ctxt ~contract ~delegate requested_amount =
  let open Lwt_result_syntax in
  if Tez_repr.(requested_amount = zero) then return (ctxt, Tez_repr.zero, [])
  else if Contract_repr.(contract = Implicit delegate) then
    let+ delegate_own_frozen_deposits =
      get_own_frozen_deposits ctxt ~delegate
    in
    (ctxt, Tez_repr.min delegate_own_frozen_deposits requested_amount, [])
  else request_unstake ctxt ~delegator:contract ~delegate requested_amount

module For_RPC = struct
  let staked_balance ctxt ~delegator ~delegate =
    let open Lwt_result_syntax in
    let* delegate_balances = get_delegate_balances ctxt ~delegate in
    let* delegator_balances =
      get_delegator_balances ctxt ~delegator ~delegate_balances
    in
    if
      Staking_pseudotoken_repr.(
        delegate_balances.frozen_deposits_pseudotokens <> zero)
    then
      Lwt.return
      @@ tez_of
           ~rounding:`Down
           delegate_balances
           delegator_balances.pseudotoken_balance
    else (
      assert (
        Staking_pseudotoken_repr.(delegator_balances.pseudotoken_balance = zero)) ;
      return Tez_repr.zero)

  let staked_balance ctxt ~contract ~delegate =
    if Contract_repr.(contract = Implicit delegate) then
      get_own_frozen_deposits ctxt ~delegate
    else staked_balance ctxt ~delegator:contract ~delegate

  let staking_pseudotokens_balance = staking_pseudotokens_balance

  let get_frozen_deposits_pseudotokens = get_frozen_deposits_pseudotokens

  let get_frozen_deposits_staked_tez = get_frozen_deposits_staked_tez
end
