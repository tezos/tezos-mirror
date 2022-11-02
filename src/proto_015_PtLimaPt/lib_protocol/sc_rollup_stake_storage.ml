(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Sc_rollup_errors
module Store = Storage.Sc_rollup
module Commitment_storage = Sc_rollup_commitment_storage
module Commitment = Sc_rollup_commitment_repr
module Commitment_hash = Commitment.Hash

let find_staker_unsafe ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let* ctxt, res = Store.Stakers.find (ctxt, rollup) staker in
  match res with
  | None -> fail Sc_rollup_not_staked
  | Some branch -> return (branch, ctxt)

let find_staker ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let* ctxt, res = Store.Last_cemented_commitment.mem ctxt rollup in
  if not res then fail (Sc_rollup_does_not_exist rollup)
  else find_staker_unsafe ctxt rollup staker

let modify_staker_count ctxt rollup f =
  let open Lwt_tzresult_syntax in
  let* ctxt, maybe_count = Store.Staker_count.find ctxt rollup in
  let count = Option.value ~default:0l maybe_count in
  let* ctxt, size_diff, _was_bound =
    Store.Staker_count.add ctxt rollup (f count)
  in
  assert (Compare.Int.(size_diff = 0)) ;
  return ctxt

let get_contract_and_stake ctxt staker =
  let staker_contract = Contract_repr.Implicit staker in
  let stake = Constants_storage.sc_rollup_stake_amount ctxt in
  (staker_contract, stake)

(** Warning: must be called only if [rollup] exists and [staker] is not to be
    found in {!Store.Stakers.} *)
let deposit_stake ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let* lcc, ctxt = Commitment_storage.last_cemented_commitment ctxt rollup in
  let staker_contract, stake = get_contract_and_stake ctxt staker in
  let* ctxt, staker_balance = Token.balance ctxt (`Contract staker_contract) in
  let* () =
    fail_when
      Tez_repr.(staker_balance < stake)
      (Sc_rollup_staker_funds_too_low
         {
           staker;
           sc_rollup = rollup;
           staker_balance;
           min_expected_balance = stake;
         })
  in
  let bond_id = Bond_id_repr.Sc_rollup_bond_id rollup in
  let* ctxt, balance_updates =
    Token.transfer
      ctxt
      (`Contract staker_contract)
      (`Frozen_bonds (staker_contract, bond_id))
      stake
  in
  let* ctxt, _size = Store.Stakers.init (ctxt, rollup) staker lcc in
  let* ctxt = modify_staker_count ctxt rollup Int32.succ in
  return (ctxt, balance_updates, lcc)

let withdraw_stake ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let* lcc, ctxt = Commitment_storage.last_cemented_commitment ctxt rollup in
  let* ctxt, res = Store.Stakers.find (ctxt, rollup) staker in
  match res with
  | None -> fail Sc_rollup_not_staked
  | Some staked_on_commitment ->
      let* () =
        fail_unless
          Commitment_hash.(staked_on_commitment = lcc)
          Sc_rollup_not_staked_on_lcc
      in
      let staker_contract, stake = get_contract_and_stake ctxt staker in
      let bond_id = Bond_id_repr.Sc_rollup_bond_id rollup in
      let* ctxt, balance_updates =
        Token.transfer
          ctxt
          (`Frozen_bonds (staker_contract, bond_id))
          (`Contract staker_contract)
          stake
      in
      let* ctxt, _size_freed =
        Store.Stakers.remove_existing (ctxt, rollup) staker
      in
      let+ ctxt = modify_staker_count ctxt rollup Int32.pred in
      (ctxt, balance_updates)

let assert_commitment_not_too_far_ahead ctxt rollup lcc commitment =
  let open Lwt_tzresult_syntax in
  let* lcc, ctxt = Commitment_storage.get_commitment_unsafe ctxt rollup lcc in
  let min_level = Commitment.(lcc.inbox_level) in
  let max_level = Commitment.(commitment.inbox_level) in
  let* () =
    fail_when
      (let sc_rollup_max_lookahead =
         Constants_storage.sc_rollup_max_lookahead_in_blocks ctxt
       in
       Compare.Int32.(
         sc_rollup_max_lookahead < Raw_level_repr.diff max_level min_level))
      Sc_rollup_too_far_ahead
  in
  return ctxt

(** Enfore that a commitment's inbox level increases by an exact fixed amount over its predecessor.
    This property is used in several places - not obeying it causes severe breakage.
*)
let assert_commitment_period ctxt rollup commitment =
  let open Lwt_tzresult_syntax in
  let pred_hash = Commitment.(commitment.predecessor) in
  let* pred, ctxt =
    Commitment_storage.get_commitment_unsafe ctxt rollup pred_hash
  in
  let pred_level = Commitment.(pred.inbox_level) in
  (* We want to check the following inequalities on [commitment.inbox_level],
     [commitment.predecessor.inbox_level] and the constant [sc_rollup_commitment_period].

     - Greater-than-or-equal (>=), to ensure inbox_levels are monotonically
     increasing along each branch of commitments. Together with
     [assert_commitment_not_too_far_ahead] this is sufficient to limit the
     depth of the commitment tree, which is also the number of commitments stored
     per staker. This constraint must be enforced at submission time.

     - Equality (=), so that L2 blocks are produced at a regular rate.  This
     ensures that there is only ever one branch of correct commitments,
     simplifying refutation logic. This could also be enforced at refutation time
     rather than submission time, but doing it here works too.

     Because [a >= b && a = b] is equivalent to [a = b], we can just keep the latter as
     an optimization.
  *)
  let sc_rollup_commitment_period =
    Constants_storage.sc_rollup_commitment_period_in_blocks ctxt
  in
  let* () =
    fail_unless
      Raw_level_repr.(
        commitment.inbox_level = add pred_level sc_rollup_commitment_period)
      Sc_rollup_bad_inbox_level
  in
  return ctxt

let assert_same_hash_as_predecessor ctxt rollup (commitment : Commitment.t) =
  let open Lwt_tzresult_syntax in
  let* pred, ctxt =
    Commitment_storage.get_commitment_unsafe ctxt rollup commitment.predecessor
  in
  if
    Sc_rollup_repr.State_hash.equal
      pred.compressed_state
      commitment.compressed_state
  then return ctxt
  else fail Sc_rollup_state_change_on_zero_tick_commitment

(** Check invariants on [inbox_level], enforcing overallocation of storage and
    regularity of block production.

    The constants used by [assert_refine_conditions_met] must be chosen such
    that the maximum cost of storage allocated by each staker is at most the size
    of their deposit.
 *)
let assert_refine_conditions_met ctxt rollup lcc commitment =
  let open Lwt_tzresult_syntax in
  let* ctxt = assert_commitment_not_too_far_ahead ctxt rollup lcc commitment in
  let* ctxt = assert_commitment_period ctxt rollup commitment in
  if
    Sc_rollup_repr.Number_of_ticks.equal
      Commitment.(commitment.number_of_ticks)
      Sc_rollup_repr.Number_of_ticks.zero
  then assert_same_hash_as_predecessor ctxt rollup commitment
  else return ctxt

let get_commitment_stake_count ctxt rollup node =
  let open Lwt_tzresult_syntax in
  let* ctxt, maybe_staked_on_commitment =
    Store.Commitment_stake_count.find (ctxt, rollup) node
  in
  return (Option.value ~default:0l maybe_staked_on_commitment, ctxt)

let modify_commitment_stake_count ctxt rollup node f =
  let open Lwt_tzresult_syntax in
  let* count, ctxt = get_commitment_stake_count ctxt rollup node in
  let new_count = f count in
  let* ctxt, size_diff, _was_bound =
    Store.Commitment_stake_count.add (ctxt, rollup) node new_count
  in
  return (new_count, size_diff, ctxt)

let deallocate_commitment ctxt rollup node =
  let open Lwt_tzresult_syntax in
  if Commitment_hash.(node = zero) then return ctxt
  else
    let* ctxt, _size_freed =
      Store.Commitments.remove_existing (ctxt, rollup) node
    in
    return ctxt

let deallocate_commitment_metadata ctxt rollup node =
  let open Lwt_tzresult_syntax in
  if Commitment_hash.(node = zero) then return ctxt
  else
    let* ctxt, _size_freed =
      Store.Commitment_added.remove_existing (ctxt, rollup) node
    in
    let* ctxt, _size_freed =
      Store.Commitment_stake_count.remove_existing (ctxt, rollup) node
    in
    return ctxt

let deallocate ctxt rollup node =
  let open Lwt_tzresult_syntax in
  let* ctxt = deallocate_commitment_metadata ctxt rollup node in
  deallocate_commitment ctxt rollup node

let find_commitment_to_deallocate ctxt rollup commitment_hash
    ~num_commitments_to_keep =
  let open Lwt_result_syntax in
  let rec aux ctxt commitment_hash n =
    if Compare.Int.(n = 0) then return (Some commitment_hash, ctxt)
    else
      let* pred_hash, ctxt =
        Commitment_storage.get_predecessor_opt_unsafe
          ctxt
          rollup
          commitment_hash
      in
      match pred_hash with
      | None -> return (None, ctxt)
      | Some pred_hash -> (aux [@ocaml.tailcall]) ctxt pred_hash (n - 1)
  in
  aux ctxt commitment_hash num_commitments_to_keep

let decrease_commitment_stake_count ctxt rollup node =
  let open Lwt_tzresult_syntax in
  let* new_count, _size_diff, ctxt =
    modify_commitment_stake_count ctxt rollup node Int32.pred
  in
  if Compare.Int32.(new_count <= 0l) then deallocate ctxt rollup node
  else return ctxt

let increase_commitment_stake_count ctxt rollup node =
  let open Lwt_tzresult_syntax in
  let* _new_count, size_diff, ctxt =
    modify_commitment_stake_count ctxt rollup node Int32.succ
  in
  return (size_diff, ctxt)

(* 77 for Commitments entry
   + 4 for Commitment_stake_count entry
   + 4 for Commitment_added entry
   + 0 for Staker_count_update entry *)
let commitment_storage_size_in_bytes = 85

let refine_stake ctxt rollup staker staked_on commitment =
  let open Lwt_tzresult_syntax in
  let* lcc, ctxt = Commitment_storage.last_cemented_commitment ctxt rollup in
  let* ctxt = assert_refine_conditions_met ctxt rollup lcc commitment in
  let*? ctxt, new_hash = Sc_rollup_commitment_storage.hash ctxt commitment in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2559
     Add a test checking that L2 nodes can catch up after going offline. *)
  let rec go node ctxt =
    (* WARNING: Do NOT reorder this sequence of ifs.
       we must check for staked_on before LCC, since refining
       from the LCC to another commit is a valid operation. *)
    if Commitment_hash.(node = staked_on) then (
      (* Previously staked commit found:
         Insert new commitment if not existing *)
      let* ctxt, commitment_size_diff, _was_bound =
        Store.Commitments.add (ctxt, rollup) new_hash commitment
      in
      let level = (Raw_context.current_level ctxt).level in
      let* commitment_added_size_diff, commitment_added_level, ctxt =
        Commitment_storage.set_commitment_added ctxt rollup new_hash level
      in
      let* ctxt, staker_count_diff =
        Store.Stakers.update (ctxt, rollup) staker new_hash
      in
      let* stake_count_size_diff, ctxt =
        increase_commitment_stake_count ctxt rollup new_hash
      in
      (* WARNING: [commitment_storage_size] is a defined constant, and used
         to set a bound on the relationship between [max_lookahead],
         [commitment_period] and [stake_amount].  Be careful changing this
         calculation. *)
      let size_diff =
        commitment_size_diff + commitment_added_size_diff
        + stake_count_size_diff + staker_count_diff
      in
      let expected_size_diff = commitment_storage_size_in_bytes in
      (* First submission adds [commitment_storage_size_in_bytes] to storage.
         Later submission adds 0 due to content-addressing. *)
      assert (Compare.Int.(size_diff = 0 || size_diff = expected_size_diff)) ;
      return (new_hash, commitment_added_level, ctxt)
      (* See WARNING above. *))
    else
      let* () =
        (* We reached the LCC, but [staker] is not staked directly on it.
           Thus, we backtracked. Note that everyone is staked indirectly on
           the LCC. *)
        fail_when Commitment_hash.(node = lcc) Sc_rollup_staker_backtracked
      in
      let* pred, ctxt =
        Commitment_storage.get_predecessor_unsafe ctxt rollup node
      in
      let* _size, ctxt = increase_commitment_stake_count ctxt rollup node in
      (go [@ocaml.tailcall]) pred ctxt
  in
  go Commitment.(commitment.predecessor) ctxt

let publish_commitment ctxt rollup staker commitment =
  let open Lwt_tzresult_syntax in
  let* ctxt, staked_on_opt = Store.Stakers.find (ctxt, rollup) staker in
  let* ctxt, balance_updates, staked_on =
    match staked_on_opt with
    | Some staked_on -> return (ctxt, [], staked_on)
    | None -> deposit_stake ctxt rollup staker
  in
  let+ commitment_hash, ctxt, level =
    refine_stake ctxt rollup staker staked_on commitment
  in
  (commitment_hash, ctxt, level, balance_updates)

let cement_commitment ctxt rollup new_lcc =
  let open Lwt_tzresult_syntax in
  let refutation_deadline_blocks =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  (* Calling [last_final_commitment] first to trigger failure in case of
     non-existing rollup. *)
  let* old_lcc, ctxt =
    Commitment_storage.last_cemented_commitment ctxt rollup
  in
  (* Get is safe, as [Stakers_size] is initialized on origination. *)
  let* ctxt, total_staker_count = Store.Staker_count.get ctxt rollup in
  let* () =
    fail_when Compare.Int32.(total_staker_count <= 0l) Sc_rollup_no_stakers
  in
  let* new_lcc_commitment, ctxt =
    Commitment_storage.get_commitment_unsafe ctxt rollup new_lcc
  in
  let* () =
    fail_when
      Commitment_hash.(new_lcc_commitment.predecessor <> old_lcc)
      Sc_rollup_parent_not_lcc
  in
  let* new_lcc_stake_count, ctxt =
    get_commitment_stake_count ctxt rollup new_lcc
  in
  let* () =
    fail_when
      Compare.Int32.(total_staker_count <> new_lcc_stake_count)
      Sc_rollup_disputed
  in
  let* ctxt, new_lcc_added =
    Store.Commitment_added.get (ctxt, rollup) new_lcc
  in
  let* () =
    let current_level = (Raw_context.current_level ctxt).level in
    let min_level =
      Raw_level_repr.add new_lcc_added refutation_deadline_blocks
    in
    fail_when
      Raw_level_repr.(current_level < min_level)
      (Sc_rollup_commitment_too_recent {current_level; min_level})
  in
  (* update LCC *)
  let* ctxt, lcc_size_diff =
    Store.Last_cemented_commitment.update ctxt rollup new_lcc
  in
  assert (Compare.Int.(lcc_size_diff = 0)) ;
  (* At this point we know that all stakers are implicitly staked on the new
     LCC, and no one is directly staked on the old LCC. Therefore we can safely
     deallocate the metadata ([Commitment_added] and [Commitment_stake_count])
     of the old LCC.
     However, we must not remove the commitment itself as we need it to allow
     executing outbox messages for a limited period. The maximum number of
     active cemented commitments available for execution is specified in
     [ctxt.sc_rollup.max_number_of_stored_cemented_commitments].
     Instead, we remove the oldest cemented commitment that would exceed
     [max_number_of_cemented_commitments], if such exist.
  *)
  let* ctxt = deallocate_commitment_metadata ctxt rollup old_lcc in
  (* Decrease max_number_of_stored_cemented_commitments by one because
     we start counting commitments from old_lcc, rather than from new_lcc. *)
  let num_commitments_to_keep =
    (Raw_context.constants ctxt).sc_rollup
      .max_number_of_stored_cemented_commitments - 1
  in
  let* commitment_to_deallocate, ctxt =
    find_commitment_to_deallocate ~num_commitments_to_keep ctxt rollup old_lcc
  in
  match commitment_to_deallocate with
  | None -> return (ctxt, new_lcc_commitment)
  | Some old_lcc ->
      let+ ctxt = deallocate_commitment ctxt rollup old_lcc in
      (ctxt, new_lcc_commitment)

let remove_staker ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let* lcc, ctxt = Commitment_storage.last_cemented_commitment ctxt rollup in
  let* ctxt, res = Store.Stakers.find (ctxt, rollup) staker in
  match res with
  | None -> fail Sc_rollup_not_staked
  | Some staked_on ->
      let* () =
        fail_when Commitment_hash.(staked_on = lcc) Sc_rollup_remove_lcc
      in
      let staker_contract, stake = get_contract_and_stake ctxt staker in
      let bond_id = Bond_id_repr.Sc_rollup_bond_id rollup in
      let* ctxt, balance_updates =
        Token.transfer
          ctxt
          (`Frozen_bonds (staker_contract, bond_id))
          `Sc_rollup_refutation_punishments
          stake
      in
      let* ctxt, _size_diff =
        Store.Stakers.remove_existing (ctxt, rollup) staker
      in
      let* ctxt = modify_staker_count ctxt rollup Int32.pred in
      let rec go node ctxt =
        if Commitment_hash.(node = lcc) then return ctxt
        else
          let* pred, ctxt =
            Commitment_storage.get_predecessor_unsafe ctxt rollup node
          in
          let* ctxt = decrease_commitment_stake_count ctxt rollup node in
          (go [@ocaml.tailcall]) pred ctxt
      in
      let+ ctxt = go staked_on ctxt in
      (ctxt, balance_updates)

module Internal_for_tests = struct
  let deposit_stake = deposit_stake

  let refine_stake ctxt rollup staker ?staked_on commitment =
    let open Lwt_tzresult_syntax in
    match staked_on with
    | Some staked_on -> refine_stake ctxt rollup staker staked_on commitment
    | None ->
        (* This allows to call {!refine_stake} without explicitely passing the
           staked_on parameter, it's more convenient for tests. However,
           it still enforce that {!deposit_stake} was called before. *)
        let* _ctxt, staked_on = Store.Stakers.get (ctxt, rollup) staker in
        refine_stake ctxt rollup staker staked_on commitment
end
