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

let get_delegate_proposal_count ctxt proposer =
  Storage.Vote.Proposals_count.find ctxt proposer >|=? Option.value ~default:0

let set_delegate_proposal_count ctxt proposer count =
  Storage.Vote.Proposals_count.add ctxt proposer count

let has_proposed ctxt proposer proposal =
  Storage.Vote.Proposals.mem ctxt (proposal, proposer)

let add_proposal ctxt proposer proposal =
  Storage.Vote.Proposals.add ctxt (proposal, proposer)

let get_proposals ctxt =
  Storage.Vote.Proposals.fold
    ctxt
    ~order:`Sorted
    ~init:(ok Protocol_hash.Map.empty)
    ~f:(fun (proposal, delegate) acc ->
      (* Assuming the same listings is used at votings *)
      Storage.Vote.Listings.get ctxt delegate >>=? fun weight ->
      Lwt.return
        ( acc >|? fun acc ->
          let previous =
            match Protocol_hash.Map.find proposal acc with
            | None -> 0L
            | Some x -> x
          in
          Protocol_hash.Map.add proposal (Int64.add weight previous) acc ))

let clear_proposals ctxt =
  Storage.Vote.Proposals_count.clear ctxt >>= fun ctxt ->
  Storage.Vote.Proposals.clear ctxt

type ballots = {yay : int64; nay : int64; pass : int64}

let ballots_zero = {yay = 0L; nay = 0L; pass = 0L}

let ballots_encoding =
  let open Data_encoding in
  conv
    (fun {yay; nay; pass} -> (yay, nay, pass))
    (fun (yay, nay, pass) -> {yay; nay; pass})
  @@ obj3 (req "yay" int64) (req "nay" int64) (req "pass" int64)

let equal_ballots b1 b2 =
  Int64.(equal b1.yay b2.yay && equal b1.nay b2.nay && equal b1.pass b2.pass)

let pp_ballots ppf b =
  Format.fprintf ppf "{ yay = %Ld; nay = %Ld; pass = %Ld }" b.yay b.nay b.pass

let has_recorded_ballot = Storage.Vote.Ballots.mem

let record_ballot = Storage.Vote.Ballots.init

let get_ballots ctxt =
  Storage.Vote.Ballots.fold
    ctxt
    ~order:`Sorted
    ~f:(fun delegate ballot (ballots : ballots tzresult) ->
      (* Assuming the same listings is used at votings *)
      Storage.Vote.Listings.get ctxt delegate >>=? fun weight ->
      let count = Int64.add weight in
      Lwt.return
        ( ballots >|? fun ballots ->
          match ballot with
          | Yay -> {ballots with yay = count ballots.yay}
          | Nay -> {ballots with nay = count ballots.nay}
          | Pass -> {ballots with pass = count ballots.pass} ))
    ~init:(ok ballots_zero)

let get_ballot_list = Storage.Vote.Ballots.bindings

let clear_ballots = Storage.Vote.Ballots.clear

let listings_encoding =
  Data_encoding.(
    list
      (obj2
         (req "pkh" Signature.Public_key_hash.encoding)
         (req "voting_power" int64)))

let update_listings ctxt =
  let open Lwt_result_syntax in
  let*! ctxt = Storage.Vote.Listings.clear ctxt in
  let* ctxt, total =
  Stake_storage.fold
    ctxt
    (ctxt, 0L)
    ~order:`Sorted
    ~f:(fun (delegate, stake) (ctxt, total) ->
      let weight = Tez_repr.to_mutez stake in
      Storage.Vote.Listings.init ctxt delegate weight >>=? fun ctxt ->
      return (ctxt, Int64.add total weight))
  in
  let*! ctxt = Storage.Vote.Voting_power_in_listings.add ctxt total in
  return ctxt

type delegate_info = {
  voting_power : Int64.t option;
  current_ballot : Vote_repr.ballot option;
  current_proposals : Protocol_hash.t list;
  remaining_proposals : int;
}

let pp_delegate_info ppf info =
  match info.voting_power with
  | None -> Format.fprintf ppf "Voting power: none"
  | Some p -> (
      Format.fprintf
        ppf
        "Voting power: %a"
        Tez_repr.pp
        (Tez_repr.of_mutez_exn p) ;
      (match info.current_ballot with
      | None -> ()
      | Some ballot ->
          Format.fprintf ppf "@,Current ballot: %a" Vote_repr.pp_ballot ballot) ;
      match info.current_proposals with
      | [] ->
          if Compare.Int.(info.remaining_proposals <> 0) then
            Format.fprintf
              ppf
              "@,Remaining proposals: %d"
              info.remaining_proposals
      | proposals ->
          Format.fprintf ppf "@,@[<v 2>Current proposals:" ;
          List.iter
            (fun p -> Format.fprintf ppf "@,- %a" Protocol_hash.pp p)
            proposals ;
          Format.fprintf ppf "@]" ;
          Format.fprintf
            ppf
            "@,Remaining proposals: %d"
            info.remaining_proposals)

let delegate_info_encoding =
  let open Data_encoding in
  conv
    (fun {voting_power; current_ballot; current_proposals; remaining_proposals} ->
      (voting_power, current_ballot, current_proposals, remaining_proposals))
    (fun (voting_power, current_ballot, current_proposals, remaining_proposals) ->
      {voting_power; current_ballot; current_proposals; remaining_proposals})
    (obj4
       (opt "voting_power" int64)
       (opt "current_ballot" Vote_repr.ballot_encoding)
       (dft "current_proposals" (list Protocol_hash.encoding) [])
       (dft "remaining_proposals" int31 0))

let in_listings = Storage.Vote.Listings.mem

let get_listings = Storage.Vote.Listings.bindings

let get_delegate_info ctxt delegate =
  Storage.Vote.Listings.find ctxt delegate >>=? fun voting_power ->
  match voting_power with
  | None ->
      return
        {
          voting_power;
          current_proposals = [];
          current_ballot = None;
          remaining_proposals = 0;
        }
  | Some _ ->
      Voting_period_storage.get_current_kind ctxt >>=? fun period ->
      (match period with
      | Exploration | Promotion -> Storage.Vote.Ballots.find ctxt delegate
      | Proposal | Cooldown | Adoption -> return None)
      >>=? fun current_ballot ->
      (match period with
      | Exploration | Promotion | Cooldown | Adoption -> Lwt.return []
      | Proposal ->
          Storage.Vote.Proposals.fold
            ctxt
            ~order:`Undefined
            ~init:[]
            ~f:(fun (h, d) acc ->
              if Signature.Public_key_hash.equal d delegate then
                Lwt.return (h :: acc)
              else Lwt.return acc))
      >>= fun current_proposals ->
      let remaining_proposals =
        match period with
        | Proposal ->
            Constants_repr.max_proposals_per_delegate
            - List.length current_proposals
        | _ -> 0
      in
      return
        {voting_power; current_ballot; current_proposals; remaining_proposals}

let get_voting_power_free ctxt owner =
  Storage.Vote.Listings.find ctxt owner >|=? Option.value ~default:0L

(* This function bypasses the carbonated functors to account for gas consumption.
   This is a temporary situation intended to be fixed by adding the right
   carbonated functors in a future amendment *)
let get_voting_power ctxt owner =
  let open Raw_context in
  (* Always consume read access to memory *)
  (* Accessing an int64 at /votes/listings/<KeyKind>/<hash> *)
  consume_gas ctxt (Storage_costs.read_access ~path_length:4 ~read_bytes:8)
  >>?= fun ctxt ->
  Storage.Vote.Listings.find ctxt owner >|=? function
  | None -> (ctxt, 0L)
  | Some power -> (ctxt, power)

let get_total_voting_power_free = Storage.Vote.Voting_power_in_listings.get

(* This function bypasses the carbonated functors to account for gas consumption.
   This is a temporary situation intended to be fixed by adding the right
   carbonated functors in a future amendment *)
let get_total_voting_power ctxt =
  let open Raw_context in
  (* Accessing an int64 at /votes/total_voting_power *)
  consume_gas ctxt (Storage_costs.read_access ~path_length:2 ~read_bytes:8)
  >>?= fun ctxt ->
  get_total_voting_power_free ctxt >|=? fun total_voting_power ->
  (ctxt, total_voting_power)

let get_current_quorum ctxt =
  Storage.Vote.Participation_ema.get ctxt >|=? fun participation_ema ->
  let quorum_min = Constants_storage.quorum_min ctxt in
  let quorum_max = Constants_storage.quorum_max ctxt in
  let quorum_diff = Int32.sub quorum_max quorum_min in
  Int32.(add quorum_min (div (mul participation_ema quorum_diff) 100_00l))

let get_participation_ema = Storage.Vote.Participation_ema.get

let set_participation_ema = Storage.Vote.Participation_ema.update

let current_proposal_exists = Storage.Vote.Current_proposal.mem

let get_current_proposal = Storage.Vote.Current_proposal.get

let find_current_proposal = Storage.Vote.Current_proposal.find

let init_current_proposal = Storage.Vote.Current_proposal.init

let clear_current_proposal = Storage.Vote.Current_proposal.remove

let init ctxt ~start_position =
  (* participation EMA is in centile of a percentage *)
  let participation_ema = Constants_storage.quorum_max ctxt in
  Storage.Vote.Participation_ema.init ctxt participation_ema >>=? fun ctxt ->
  Voting_period_storage.init_first_period ctxt ~start_position
