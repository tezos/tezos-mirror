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
  let (approval, new_participation_ema) =
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

type error +=
  | (* `Branch *)
      Invalid_proposal
  | Unexpected_proposal
  | Unauthorized_proposal
  | Too_many_proposals
  | Empty_proposal
  | Unexpected_ballot
  | Unauthorized_ballot
  | Duplicate_ballot

let () =
  let open Data_encoding in
  (* Invalid proposal *)
  register_error_kind
    `Branch
    ~id:"invalid_proposal"
    ~title:"Invalid proposal"
    ~description:"Ballot provided for a proposal that is not the current one."
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid proposal")
    empty
    (function Invalid_proposal -> Some () | _ -> None)
    (fun () -> Invalid_proposal) ;
  (* Unexpected proposal *)
  register_error_kind
    `Branch
    ~id:"unexpected_proposal"
    ~title:"Unexpected proposal"
    ~description:"Proposal recorded outside of a proposal period."
    ~pp:(fun ppf () -> Format.fprintf ppf "Unexpected proposal")
    empty
    (function Unexpected_proposal -> Some () | _ -> None)
    (fun () -> Unexpected_proposal) ;
  (* Unauthorized proposal *)
  register_error_kind
    `Branch
    ~id:"unauthorized_proposal"
    ~title:"Unauthorized proposal"
    ~description:
      "The delegate provided for the proposal is not in the voting listings."
    ~pp:(fun ppf () -> Format.fprintf ppf "Unauthorized proposal")
    empty
    (function Unauthorized_proposal -> Some () | _ -> None)
    (fun () -> Unauthorized_proposal) ;
  (* Unexpected ballot *)
  register_error_kind
    `Branch
    ~id:"unexpected_ballot"
    ~title:"Unexpected ballot"
    ~description:"Ballot recorded outside of a voting period."
    ~pp:(fun ppf () -> Format.fprintf ppf "Unexpected ballot")
    empty
    (function Unexpected_ballot -> Some () | _ -> None)
    (fun () -> Unexpected_ballot) ;
  (* Unauthorized ballot *)
  register_error_kind
    `Branch
    ~id:"unauthorized_ballot"
    ~title:"Unauthorized ballot"
    ~description:
      "The delegate provided for the ballot is not in the voting listings."
    ~pp:(fun ppf () -> Format.fprintf ppf "Unauthorized ballot")
    empty
    (function Unauthorized_ballot -> Some () | _ -> None)
    (fun () -> Unauthorized_ballot) ;
  (* Duplicate ballot *)
  register_error_kind
    `Branch
    ~id:"duplicate_ballot"
    ~title:"Duplicate ballot"
    ~description:"The delegate has already submitted a ballot."
    ~pp:(fun ppf () -> Format.fprintf ppf "Duplicate ballot")
    empty
    (function Duplicate_ballot -> Some () | _ -> None)
    (fun () -> Duplicate_ballot) ;
  (* Too many proposals *)
  register_error_kind
    `Branch
    ~id:"too_many_proposals"
    ~title:"Too many proposals"
    ~description:"The delegate reached the maximum number of allowed proposals."
    ~pp:(fun ppf () -> Format.fprintf ppf "Too many proposals")
    empty
    (function Too_many_proposals -> Some () | _ -> None)
    (fun () -> Too_many_proposals) ;
  (* Empty proposal *)
  register_error_kind
    `Branch
    ~id:"empty_proposal"
    ~title:"Empty proposal"
    ~description:"Proposal lists cannot be empty."
    ~pp:(fun ppf () -> Format.fprintf ppf "Empty proposal")
    empty
    (function Empty_proposal -> Some () | _ -> None)
    (fun () -> Empty_proposal)

let record_proposals ctxt delegate proposals =
  (match proposals with
  | [] -> error Empty_proposal
  | _ :: _ -> Result.return_unit)
  >>?= fun () ->
  Voting_period.get_current_kind ctxt >>=? function
  | Proposal ->
      Vote.in_listings ctxt delegate >>= fun in_listings ->
      if in_listings then (
        Vote.recorded_proposal_count_for_delegate ctxt delegate
        >>=? fun count ->
        assert (Compare.Int.(Constants.max_proposals_per_delegate >= count)) ;
        error_when
          Compare.Int.(
            List.compare_length_with
              proposals
              (Constants.max_proposals_per_delegate - count)
            > 0)
          Too_many_proposals
        >>?= fun () ->
        List.fold_left_es
          (fun ctxt proposal -> Vote.record_proposal ctxt proposal delegate)
          ctxt
          proposals)
      else fail Unauthorized_proposal
  | Exploration | Cooldown | Promotion | Adoption -> fail Unexpected_proposal

let record_ballot ctxt delegate proposal ballot =
  Voting_period.get_current_kind ctxt >>=? function
  | Exploration | Promotion ->
      Vote.get_current_proposal ctxt >>=? fun current_proposal ->
      error_unless
        (Protocol_hash.equal proposal current_proposal)
        Invalid_proposal
      >>?= fun () ->
      Vote.has_recorded_ballot ctxt delegate >>= fun has_ballot ->
      error_when has_ballot Duplicate_ballot >>?= fun () ->
      Vote.in_listings ctxt delegate >>= fun in_listings ->
      if in_listings then Vote.record_ballot ctxt delegate ballot
      else fail Unauthorized_ballot
  | Cooldown | Proposal | Adoption -> fail Unexpected_ballot

let may_start_new_voting_period ctxt =
  Voting_period.is_last_block ctxt >>=? fun is_last ->
  if is_last then start_new_voting_period ctxt else return ctxt
