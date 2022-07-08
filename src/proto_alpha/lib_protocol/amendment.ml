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
        Vote.clear_current_proposal ctxt >>=? fun ctxt ->
        Voting_period.reset ctxt
  | Cooldown -> Voting_period.succ ctxt
  | Promotion ->
      get_approval_and_update_participation_ema ctxt
      >>=? fun (ctxt, approved) ->
      if approved then Voting_period.succ ctxt
      else Vote.clear_current_proposal ctxt >>=? Voting_period.reset
  | Adoption ->
      Vote.get_current_proposal ctxt >>=? fun proposal ->
      activate ctxt proposal >>= fun ctxt ->
      Vote.clear_current_proposal ctxt >>=? Voting_period.reset)
  >>=? fun ctxt -> Vote.update_listings ctxt

let may_start_new_voting_period ctxt =
  Voting_period.is_last_block ctxt >>=? fun is_last ->
  if is_last then start_new_voting_period ctxt else return ctxt

(** {2 Validation and application of voting operations} *)

open Validate_errors.Voting

let check_no_duplicates proposals =
  let open Tzresult_syntax in
  let* (_ : Protocol_hash.Set.t) =
    List.fold_left_e
      (fun previous_proposals proposal ->
        let* () =
          error_when
            (Protocol_hash.Set.mem proposal previous_proposals)
            (Proposals_contain_duplicate {proposal})
        in
        return (Protocol_hash.Set.add proposal previous_proposals))
      Protocol_hash.Set.empty
      proposals
  in
  return_unit

let record_delegate_proposals ctxt delegate proposals =
  (match proposals with
  | [] -> error Empty_proposals
  | _ :: _ -> Result.return_unit)
  >>?= fun () ->
  check_no_duplicates proposals >>?= fun () ->
  Voting_period.get_current_kind ctxt >>=? function
  | Proposal ->
      Vote.in_listings ctxt delegate >>= fun in_listings ->
      if in_listings then (
        Vote.get_delegate_proposal_count ctxt delegate >>=? fun count ->
        assert (Compare.Int.(Constants.max_proposals_per_delegate >= count)) ;
        error_when
          Compare.Int.(
            List.compare_length_with
              proposals
              (Constants.max_proposals_per_delegate - count)
            > 0)
          Too_many_proposals
        >>?= fun () ->
        (* The mix of syntactic styles will go away in the next commit *)
        let open Lwt_tzresult_syntax in
        let* ctxt, count =
          List.fold_left_es
            (fun (ctxt, count) proposal ->
              let* () =
                let*! already_proposed =
                  Vote.has_proposed ctxt delegate proposal
                in
                fail_when already_proposed (Already_proposed {proposal})
              in
              let*! ctxt = Vote.add_proposal ctxt delegate proposal in
              return (ctxt, count + 1))
            (ctxt, count)
            proposals
        in
        let*! ctxt = Vote.set_delegate_proposal_count ctxt delegate count in
        return ctxt)
      else fail Source_not_in_vote_listings
  | (Exploration | Cooldown | Promotion | Adoption) as current ->
      fail (Wrong_voting_period_kind {current; expected = [Proposal]})

let record_testnet_dictator_proposals ctxt chain_id proposals =
  Vote.clear_ballots ctxt >>= fun ctxt ->
  Vote.clear_proposals ctxt >>= fun ctxt ->
  Vote.clear_current_proposal ctxt >>=? fun ctxt ->
  match proposals with
  | [] ->
      Voting_period.Testnet_dictator.overwrite_current_kind
        ctxt
        chain_id
        Proposal
  | [proposal] ->
      Vote.init_current_proposal ctxt proposal >>=? fun ctxt ->
      Voting_period.Testnet_dictator.overwrite_current_kind
        ctxt
        chain_id
        Adoption
  | _ :: _ :: _ -> fail Testnet_dictator_multiple_proposals

let is_testnet_dictator ctxt chain_id delegate =
  (* This function should always, ALWAYS, return false on mainnet!!!! *)
  match Constants.testnet_dictator ctxt with
  | Some pkh when Chain_id.(chain_id <> Constants_repr.mainnet_id) ->
      Signature.Public_key_hash.equal pkh delegate
  | _ -> false

let record_proposals ctxt chain_id delegate proposals =
  if is_testnet_dictator ctxt chain_id delegate then
    record_testnet_dictator_proposals ctxt chain_id proposals
  else record_delegate_proposals ctxt delegate proposals

let apply_proposals ctxt chain_id (operation : Kind.proposals operation) =
  let (Single (Proposals {source; period; proposals})) =
    operation.protocol_data.contents
  in
  Delegate.pubkey ctxt source >>=? fun delegate ->
  Operation.check_signature delegate chain_id operation >>?= fun () ->
  Voting_period.get_current ctxt >>=? fun {index = current_period; _} ->
  error_unless
    Compare.Int32.(current_period = period)
    (Wrong_voting_period_index {expected = current_period; provided = period})
  >>?= fun () ->
  record_proposals ctxt chain_id source proposals >|=? fun ctxt ->
  (ctxt, Apply_results.Single_result Proposals_result)

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
