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

let check_count ctxt proposer proposals =
  let open Lwt_tzresult_syntax in
  let* count = Vote.get_delegate_proposal_count ctxt proposer in
  (* [count] should never have been increased above
     [max_proposals_per_delegate]. *)
  assert (Compare.Int.(count <= Constants.max_proposals_per_delegate)) ;
  fail_unless
    Compare.List_length_with.(
      proposals <= Constants.max_proposals_per_delegate - count)
    Too_many_proposals

let check_already_proposed ctxt proposer proposals =
  let open Lwt_tzresult_syntax in
  List.iter_es
    (fun proposal ->
      let*! already_proposed = Vote.has_proposed ctxt proposer proposal in
      fail_when already_proposed (Already_proposed {proposal}))
    proposals

(* For now, this function is still called directly by
   [apply_proposals], and so [ctxt] is still the context at
   application time (where previous operations from the current block
   have been applied).

   In the next commit, this function will be called by
   [Validate_operation.validate_operation] instead (and will need to
   be updated because [ctxt] will not be the same). *)
let validate_proposals ctxt chain_id ~should_check_signature
    (operation : Kind.proposals operation) =
  let open Lwt_tzresult_syntax in
  let (Single (Proposals {source; period; proposals})) =
    operation.protocol_data.contents
  in
  let* current_period = Voting_period.get_current ctxt in
  let*? () = check_period_index ~expected:current_period.index period in
  let* () =
    if Testnet_dictator.is_testnet_dictator ctxt chain_id source then
      Lwt.return (Testnet_dictator.check_proposals chain_id proposals)
    else
      let*? () = check_proposal_list_sanity proposals in
      let*? () = check_period_kind_for_proposals current_period in
      let* () = check_in_listings ctxt source in
      let* () = check_count ctxt source proposals in
      check_already_proposed ctxt source proposals
  in
  (* The signature check is done last because it is more costly than
     most checks. *)
  when_ should_check_signature (fun () ->
      (* Retrieving the public key cannot fail. Indeed, we have
         already checked that the delegate is in the vote listings
         (or is a testnet dictator), which implies that it is a
         manager with a revealed key. *)
      let* public_key = Delegate.pubkey ctxt source in
      Lwt.return (Operation.check_signature public_key chain_id operation))

let record_proposals ctxt proposer proposals =
  let open Lwt_tzresult_syntax in
  let* count = Vote.get_delegate_proposal_count ctxt proposer in
  let new_count = count + List.length proposals in
  let*! ctxt = Vote.set_delegate_proposal_count ctxt proposer new_count in
  let*! ctxt =
    List.fold_left_s
      (fun ctxt proposal -> Vote.add_proposal ctxt proposer proposal)
      ctxt
      proposals
  in
  return ctxt

let apply_proposals ctxt chain_id (operation : Kind.proposals operation) =
  let open Lwt_tzresult_syntax in
  let* () =
    validate_proposals ctxt chain_id ~should_check_signature:true operation
  in
  let (Single (Proposals {source; period = _; proposals})) =
    operation.protocol_data.contents
  in
  let* ctxt =
    if Testnet_dictator.is_testnet_dictator ctxt chain_id source then
      Testnet_dictator.record_proposals ctxt chain_id proposals
    else record_proposals ctxt source proposals
  in
  return (ctxt, Apply_results.Single_result Proposals_result)

let record_ballot ctxt delegate proposal ballot =
  Voting_period.get_current_kind ctxt >>=? function
  | Exploration | Promotion ->
      Vote.get_current_proposal ctxt >>=? fun current_proposal ->
      error_unless
        (Protocol_hash.equal proposal current_proposal)
        (Ballot_for_wrong_proposal
           {current = current_proposal; submitted = proposal})
      >>?= fun () ->
      Vote.has_recorded_ballot ctxt delegate >>= fun has_ballot ->
      error_when has_ballot Already_submitted_a_ballot >>?= fun () ->
      Vote.in_listings ctxt delegate >>= fun in_listings ->
      if in_listings then Vote.record_ballot ctxt delegate ballot
      else fail Source_not_in_vote_listings
  | (Cooldown | Proposal | Adoption) as current ->
      fail
        (Wrong_voting_period_kind {current; expected = [Exploration; Promotion]})

let apply_ballot ctxt chain_id (operation : Kind.ballot operation) =
  let (Single (Ballot {source; period; proposal; ballot})) =
    operation.protocol_data.contents
  in
  Delegate.pubkey ctxt source >>=? fun delegate ->
  Operation.check_signature delegate chain_id operation >>?= fun () ->
  Voting_period.get_current ctxt >>=? fun {index = current_period; _} ->
  error_unless
    Compare.Int32.(current_period = period)
    (Wrong_voting_period_index {expected = current_period; provided = period})
  >>?= fun () ->
  record_ballot ctxt source proposal ballot >|=? fun ctxt ->
  (ctxt, Apply_results.Single_result Ballot_result)
