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

(** Returns the proposal submitted by the most delegates.
    Returns None in case of a tie, if proposal quorum is below required
    minimum or if there are no proposals. *)
let select_winning_proposal ctxt =
  Vote.get_proposals ctxt >>=? fun proposals ->
  let merge proposal vote winners =
    match winners with
    | None -> Some ([proposal], vote)
    | Some (winners, winners_vote) as previous ->
        if Compare.Int64.(vote = winners_vote) then
          Some (proposal :: winners, winners_vote)
        else if Compare.Int64.(vote > winners_vote) then Some ([proposal], vote)
        else previous
  in
  match Protocol_hash.Map.fold merge proposals None with
  | Some ([proposal], vote) ->
      Vote.get_total_voting_power_free ctxt >>=? fun max_vote ->
      let min_proposal_quorum =
        Z.of_int32 (Constants.min_proposal_quorum ctxt)
      in
      let min_vote_to_pass =
        Z.(
          to_int64
            (div (mul min_proposal_quorum (of_int64 max_vote)) (of_int 100_00)))
      in
      if Compare.Int64.(vote >= min_vote_to_pass) then return_some proposal
      else return_none
  | _ -> return_none

(* in case of a tie, let's do nothing. *)

(** A proposal is approved if it has supermajority and the participation reaches
    the current quorum.
    Supermajority means the yays are more 8/10 of casted votes.
    The participation is the ratio of all received votes, including passes, with
    respect to the number of possible votes.
    The participation EMA (exponential moving average) uses the last
    participation EMA and the current participation./
    The expected quorum is calculated using the last participation EMA, capped
    by the min/max quorum protocol constants. *)
let approval_and_participation_ema (ballots : Vote.ballots) ~maximum_vote
    ~participation_ema ~expected_quorum =
  (* Note overflows: considering a maximum of 1e9 tokens (around 2^30),
     hence 1e15 mutez (around 2^50)
     In 'participation' a Z is used because in the worst case 'all_votes is
     1e15 and after the multiplication is 1e19 (around 2^64).
  *)
  let casted_votes = Int64.add ballots.yay ballots.nay in
  let all_votes = Int64.add casted_votes ballots.pass in
  let supermajority = Int64.div (Int64.mul 8L casted_votes) 10L in
  let participation =
    (* in centile of percentage *)
    Z.(
      to_int32
        (div
           (mul (Z.of_int64 all_votes) (Z.of_int 100_00))
           (Z.of_int64 maximum_vote)))
  in
  let approval =
    Compare.Int32.(participation >= expected_quorum)
    && Compare.Int64.(ballots.yay >= supermajority)
  in
  let new_participation_ema =
    Int32.(div (add (mul 8l participation_ema) (mul 2l participation)) 10l)
  in
  (approval, new_participation_ema)

let get_approval_and_update_participation_ema ctxt =
  Vote.get_ballots ctxt >>=? fun ballots ->
  Vote.get_total_voting_power_free ctxt >>=? fun maximum_vote ->
  Vote.get_participation_ema ctxt >>=? fun participation_ema ->
  Vote.get_current_quorum ctxt >>=? fun expected_quorum ->
  Vote.clear_ballots ctxt >>= fun ctxt ->
  let approval, new_participation_ema =
    approval_and_participation_ema
      ballots
      ~maximum_vote
      ~participation_ema
      ~expected_quorum
  in
  Vote.set_participation_ema ctxt new_participation_ema >|=? fun ctxt ->
  (ctxt, approval)

(** Implements the state machine of the amendment procedure. Note that
   [update_listings], that computes the vote weight of each delegate, is run at
   the end of each voting period. This state-machine prepare the voting_period
   for the next block. *)
let start_new_voting_period ctxt =
  (* any change related to the storage in this function must probably
     be replicated in `record_testnet_dictator_proposals` *)
  Voting_period.get_current_kind ctxt >>=? fun kind ->
  (match kind with
  | Proposal -> (
      select_winning_proposal ctxt >>=? fun proposal ->
      Vote.clear_proposals ctxt >>= fun ctxt ->
      match proposal with
      | None -> Voting_period.reset ctxt
      | Some proposal ->
          Vote.init_current_proposal ctxt proposal >>=? Voting_period.succ)
  | Exploration ->
      get_approval_and_update_participation_ema ctxt
      >>=? fun (ctxt, approved) ->
      if approved then Voting_period.succ ctxt
      else
        Vote.clear_current_proposal ctxt >>= fun ctxt ->
        Voting_period.reset ctxt
  | Cooldown -> Voting_period.succ ctxt
  | Promotion ->
      get_approval_and_update_participation_ema ctxt
      >>=? fun (ctxt, approved) ->
      if approved then Voting_period.succ ctxt
      else Vote.clear_current_proposal ctxt >>= Voting_period.reset
  | Adoption ->
      Vote.get_current_proposal ctxt >>=? fun proposal ->
      activate ctxt proposal >>= fun ctxt ->
      Vote.clear_current_proposal ctxt >>= Voting_period.reset)
  >>=? fun ctxt -> Vote.update_listings ctxt

let may_start_new_voting_period ctxt =
  Voting_period.is_last_block ctxt >>=? fun is_last ->
  if is_last then start_new_voting_period ctxt else return ctxt

(** {2 Validation and application of voting operations} *)

open Validate_errors.Voting

(** This state is maintained in memory during the validation of a
    block, or until a change of head block in mempool mode. This
    state's purpose is to detect potential conflicts between the new
    voting operation to validate, and all the voting operations that
    have already been validated in the current block or mempool. *)
module Validation_state = struct
  (** Summary of previously validated Proposals operations by a given
      proposer in the current block/mempool. *)
  type proposer_history = {
    count : int;
        (** Total number of protocols submitted by the proposer in
            previously validated operations. *)
    operations : Operation_hash.t list;
        (** Hashes of the previously validated Proposals operations from
            the proposer. *)
    proposed : Operation_hash.t Protocol_hash.Map.t;
        (** A map indexed by the protocols that have been submitted by the
            proposer in previously validated operations. Each protocol
            points to the operation in which it was proposed. *)
  }

  type t = {
    proposals_validated : proposer_history Signature.Public_key_hash.Map.t;
        (** Summary of all Proposals operations validated in the current
            block/mempool, indexed by the operation's source aka
            proposer. *)
    dictator_proposals_validated : Operation_hash.t option;
        (** If a testnet dictotor Proposals operation has been validated
            in the current block/mempool, then its hash is recorded
            here. Since such an operation can change the voting period
            kind, it is mutually exclusive with any other voting operation
            in a single block (otherwise we would loose the commutativity
            of validated operation application: see
            {!Validate_operation}). *)
    ballots_validated : Operation_hash.t Signature.Public_key_hash.Map.t;
        (** To each delegate that has submitted a ballot in a previously
            validated operation, associates the hash of this operation.  *)
  }

  let empty =
    {
      proposals_validated = Signature.Public_key_hash.Map.empty;
      dictator_proposals_validated = None;
      ballots_validated = Signature.Public_key_hash.Map.empty;
    }

  let check_count_conflict ~count_previous_blocks ~count_operation
      proposer_history =
    let max_allowed = Constants.max_proposals_per_delegate in
    let count_before_op = count_previous_blocks + proposer_history.count in
    (* [count_before_op] should never have been increased above
       [max_proposals_per_delegate]. *)
    assert (Compare.Int.(count_before_op <= max_allowed)) ;
    error_unless
      Compare.Int.(count_before_op + count_operation <= max_allowed)
      (Conflict_too_many_proposals
         {
           max_allowed;
           count_previous_blocks;
           count_current_block = proposer_history.count;
           count_operation;
           conflicting_operations = proposer_history.operations;
         })

  (** Check that a regular (ie. non-dictator) Proposals operation is
      compatible with previously validated voting operations in the
      current block/mempool, and update the [state] with this
      operation.

      @return [Error Conflict_too_many_proposals] if the total number
      of proposals by [proposer] in previously applied operations in
      [ctxt], in previously validated operations in the current
      block/mempool, and in the operation to validate, exceeds
      {!Constants.max_proposals_per_delegate}.

      @return [Error Conflict_already_proposed] if one of the
      operation's [proposals] has already been submitted by [proposer]
      in the current block/mempool.

      @return [Error Conflicting_dictator_proposals] if the current
      block/mempool already contains a testnet dictator Proposals
      operation (see {!recfield:dictator_proposals_validated}).

      Note that this function is designed to be called in addition to
      {!check_proposal_list_sanity} and {!check_count} further below,
      not instead of them: that's why nothing is done when the
      [proposer] is not in {!recfield:proposals_validated}. More
      precisely, this function should be called {e after} the
      aforementioned functions, whose potential errors
      e.g. [Proposals_contain_duplicate] or [Too_many_proposals] should
      take precedence because they are independent from the validation
      [state]. *)
  let check_proposals_and_update (state : t) oph proposer proposals
      ~count_in_ctxt ~proposals_length =
    let open Tzresult_syntax in
    let* new_proposer_history =
      match
        Signature.Public_key_hash.Map.find proposer state.proposals_validated
      with
      | None ->
          let proposed =
            List.fold_left
              (fun acc proposal -> Protocol_hash.Map.add proposal oph acc)
              Protocol_hash.Map.empty
              proposals
          in
          return {count = proposals_length; operations = [oph]; proposed}
      | Some proposer_history ->
          let* () =
            check_count_conflict
              ~count_previous_blocks:count_in_ctxt
              ~count_operation:proposals_length
              proposer_history
          in
          let add_proposal proposed_map proposal =
            match Protocol_hash.Map.find proposal proposer_history.proposed with
            | Some conflicting_operation ->
                error
                  (Conflict_already_proposed {proposal; conflicting_operation})
            | None -> ok (Protocol_hash.Map.add proposal oph proposed_map)
          in
          let* proposed =
            List.fold_left_e add_proposal proposer_history.proposed proposals
          in
          return
            {
              count = proposer_history.count + proposals_length;
              operations = oph :: proposer_history.operations;
              proposed;
            }
    in
    let* () =
      match state.dictator_proposals_validated with
      | None -> ok ()
      | Some dictator_oph -> error (Conflicting_dictator_proposals dictator_oph)
    in
    let proposals_validated =
      Signature.Public_key_hash.Map.add
        proposer
        new_proposer_history
        state.proposals_validated
    in
    return {state with proposals_validated}

  (** Check that a Proposals operation from a testnet dictator is
      compatible with previously validated voting operations in the
      current block/mempool (ie. that no other voting operation has
      been validated), and update the [state] with this operation.

      @return [Error Testnet_dictator_conflicting_operation] if the
      current block or mempool already contains any validated voting
      operation. *)
  let check_dictator_proposals_and_update state oph =
    let open Tzresult_syntax in
    let* () =
      error_unless
        (Signature.Public_key_hash.Map.is_empty state.proposals_validated
        && Option.is_none state.dictator_proposals_validated
        && Signature.Public_key_hash.Map.is_empty state.ballots_validated)
        Testnet_dictator_conflicting_operation
    in
    return {state with dictator_proposals_validated = Some oph}

  (** Check that a Ballot operation is compatible with previously
      validated voting operations in the current block/mempool.

      @return [Error Conflicting_ballot] if the [delegate] has already
      submitted a ballot in the current block/mempool.

      @return [Error Conflicting_dictator_proposals] if the current
      block/mempool already contains a testnet dictator Proposals
      operation (see {!recfield:dictator_proposals_validated}). *)
  let check_ballot state voter =
    let open Tzresult_syntax in
    let* () =
      match
        Signature.Public_key_hash.Map.find voter state.ballots_validated
      with
      | None -> ok ()
      | Some conflicting_operation ->
          error (Conflicting_ballot {conflicting_operation})
    in
    match state.dictator_proposals_validated with
    | None -> ok ()
    | Some dictator_oph -> error (Conflicting_dictator_proposals dictator_oph)

  (** Update the [state] when a Ballot operation is validated. *)
  let update_on_ballot state oph voter =
    let ballots_validated =
      Signature.Public_key_hash.Map.add voter oph state.ballots_validated
    in
    {state with ballots_validated}
end

(** Helpers to validate and apply Proposals operations from a
    registered dictator of a test chain. These operations let the
    dictator immediately change the current voting period's kind, and
    the current proposal if applicable. Of course, there must never be
    such a dictator on mainnet. *)
module Testnet_dictator = struct
  let is_testnet_dictator ctxt chain_id delegate =
    (* This function should always, ALWAYS, return false on mainnet!!!! *)
    match Constants.testnet_dictator ctxt with
    | Some pkh when Chain_id.(chain_id <> Constants.mainnet_id) ->
        Signature.Public_key_hash.equal pkh delegate
    | _ -> false

  (** Check that [record_proposals] below will not fail.

      This function is designed to be exclusively called by
      [validate_proposals] further down this file.

      @return [Error Multiple_proposals] if [proposals] has more than
      one element. *)
  let check_proposals chain_id proposals =
    (* This assertion should be ensured by the fact that
       {!is_testnet_dictator} cannot be [true] on mainnet, but we
       double check it because it is critical. *)
    assert (Chain_id.(chain_id <> Constants.mainnet_id)) ;
    match proposals with
    | [] | [_] ->
        (* In [record_proposals] below, the call to
           {!Vote.init_current_proposal} (in the singleton list case)
           cannot fail because {!Vote.clear_current_proposal} is called
           right before.

           The calls to
           {!Voting_period.Testnet_dictator.overwrite_current_kind} may
           usually fail when the voting period is not
           initialized. However, this cannot happen because the current
           function is only called in [validate_proposals] after a
           successful call to {!Voting_period.get_current}. *)
        ok ()
    | _ :: _ :: _ -> error Testnet_dictator_multiple_proposals

  (** Forcibly update the voting period according to a voting
      dictator's Proposals operation.

      {!check_proposals} should guarantee that this function cannot
      return an error. *)
  let record_proposals ctxt chain_id proposals =
    let open Lwt_tzresult_syntax in
    let*! ctxt = Vote.clear_ballots ctxt in
    let*! ctxt = Vote.clear_proposals ctxt in
    let*! ctxt = Vote.clear_current_proposal ctxt in
    match proposals with
    | [] ->
        Voting_period.Testnet_dictator.overwrite_current_kind
          ctxt
          chain_id
          Proposal
    | [proposal] ->
        let* ctxt = Vote.init_current_proposal ctxt proposal in
        Voting_period.Testnet_dictator.overwrite_current_kind
          ctxt
          chain_id
          Adoption
    | _ :: _ :: _ -> fail Testnet_dictator_multiple_proposals
end

let check_period_index ~expected period_index =
  error_unless
    Compare.Int32.(expected = period_index)
    (Wrong_voting_period_index {expected; provided = period_index})

(** Check that the list of proposals is not empty and does not contain
    duplicates. *)
let check_proposal_list_sanity proposals =
  let open Tzresult_syntax in
  let* () =
    match proposals with [] -> error Empty_proposals | _ :: _ -> ok ()
  in
  let* (_ : Protocol_hash.Set.t) =
    List.fold_left_e
      (fun previous_elements proposal ->
        let* () =
          error_when
            (Protocol_hash.Set.mem proposal previous_elements)
            (Proposals_contain_duplicate {proposal})
        in
        return (Protocol_hash.Set.add proposal previous_elements))
      Protocol_hash.Set.empty
      proposals
  in
  return_unit

let check_period_kind_for_proposals current_period =
  match current_period.Voting_period.kind with
  | Proposal -> ok ()
  | (Exploration | Cooldown | Promotion | Adoption) as current ->
      error (Wrong_voting_period_kind {current; expected = [Proposal]})

let check_in_listings ctxt source =
  let open Lwt_tzresult_syntax in
  let*! in_listings = Vote.in_listings ctxt source in
  fail_unless in_listings Source_not_in_vote_listings

let check_count ~count_in_ctxt ~proposals_length =
  (* The proposal count of the proposer in the context should never
     have been increased above [max_proposals_per_delegate]. *)
  assert (Compare.Int.(count_in_ctxt <= Constants.max_proposals_per_delegate)) ;
  error_unless
    Compare.Int.(
      count_in_ctxt + proposals_length <= Constants.max_proposals_per_delegate)
    Too_many_proposals

let check_already_proposed ctxt proposer proposals =
  let open Lwt_tzresult_syntax in
  List.iter_es
    (fun proposal ->
      let*! already_proposed = Vote.has_proposed ctxt proposer proposal in
      fail_when already_proposed (Already_proposed {proposal}))
    proposals

let validate_proposals ctxt chain_id state ~should_check_signature oph
    (operation : Kind.proposals operation) =
  let open Lwt_tzresult_syntax in
  let (Single (Proposals {source; period; proposals})) =
    operation.protocol_data.contents
  in
  let* current_period = Voting_period.get_current ctxt in
  let*? () = check_period_index ~expected:current_period.index period in
  let* state =
    if Testnet_dictator.is_testnet_dictator ctxt chain_id source then
      let*? () = Testnet_dictator.check_proposals chain_id proposals in
      Lwt.return
        (Validation_state.check_dictator_proposals_and_update state oph)
    else
      let*? () = check_proposal_list_sanity proposals in
      let*? () = check_period_kind_for_proposals current_period in
      let* () = check_in_listings ctxt source in
      let* count_in_ctxt = Vote.get_delegate_proposal_count ctxt source in
      let proposals_length = List.length proposals in
      let*? () = check_count ~count_in_ctxt ~proposals_length in
      let* () = check_already_proposed ctxt source proposals in
      Lwt.return
        (Validation_state.check_proposals_and_update
           state
           oph
           source
           proposals
           ~count_in_ctxt
           ~proposals_length)
  in
  (* The signature check is done last because it is more costly than
     most checks. *)
  let* () =
    when_ should_check_signature (fun () ->
        (* Retrieving the public key cannot fail. Indeed, we have
           already checked that the delegate is in the vote listings
           (or is a testnet dictator), which implies that it is a
           manager with a revealed key. *)
        let* public_key = Contract.get_manager_key ctxt source in
        Lwt.return (Operation.check_signature public_key chain_id operation))
  in
  return state

let apply_proposals ctxt chain_id (Proposals {source; period = _; proposals}) =
  let open Lwt_tzresult_syntax in
  let* ctxt =
    if Testnet_dictator.is_testnet_dictator ctxt chain_id source then
      Testnet_dictator.record_proposals ctxt chain_id proposals
    else
      let* count = Vote.get_delegate_proposal_count ctxt source in
      let new_count = count + List.length proposals in
      let*! ctxt = Vote.set_delegate_proposal_count ctxt source new_count in
      let*! ctxt =
        List.fold_left_s
          (fun ctxt proposal -> Vote.add_proposal ctxt source proposal)
          ctxt
          proposals
      in
      return ctxt
  in
  return (ctxt, Apply_results.Single_result Proposals_result)

let check_period_kind_for_ballot current_period =
  match current_period.Voting_period.kind with
  | Exploration | Promotion -> ok ()
  | (Cooldown | Proposal | Adoption) as current ->
      error
        (Wrong_voting_period_kind {current; expected = [Exploration; Promotion]})

let check_current_proposal ctxt op_proposal =
  let open Lwt_tzresult_syntax in
  let* current_proposal = Vote.get_current_proposal ctxt in
  fail_unless
    (Protocol_hash.equal op_proposal current_proposal)
    (Ballot_for_wrong_proposal
       {current = current_proposal; submitted = op_proposal})

let check_source_has_not_already_voted ctxt source =
  let open Lwt_tzresult_syntax in
  let*! has_ballot = Vote.has_recorded_ballot ctxt source in
  fail_when has_ballot Already_submitted_a_ballot

let validate_ballot ctxt chain_id state ~should_check_signature oph
    (operation : Kind.ballot operation) =
  let open Lwt_tzresult_syntax in
  let (Single (Ballot {source; period; proposal; ballot = _})) =
    operation.protocol_data.contents
  in
  let*? () = Validation_state.check_ballot state source in
  let* current_period = Voting_period.get_current ctxt in
  let*? () = check_period_index ~expected:current_period.index period in
  let*? () = check_period_kind_for_ballot current_period in
  let* () = check_current_proposal ctxt proposal in
  let* () = check_source_has_not_already_voted ctxt source in
  let* () = check_in_listings ctxt source in
  (* The signature check is done last because it is more costly than
     most checks. *)
  let* () =
    when_ should_check_signature (fun () ->
        (* Retrieving the public key cannot fail. Indeed, we have
           already checked that the delegate is in the vote listings,
           which implies that it is a manager with a revealed key. *)
        let* public_key = Contract.get_manager_key ctxt source in
        Lwt.return (Operation.check_signature public_key chain_id operation))
  in
  return (Validation_state.update_on_ballot state oph source)

let apply_ballot ctxt (Ballot {source; period = _; proposal = _; ballot}) =
  let open Lwt_tzresult_syntax in
  let* ctxt = Vote.record_ballot ctxt source ballot in
  return (ctxt, Apply_results.Single_result Ballot_result)
