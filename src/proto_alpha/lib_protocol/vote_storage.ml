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

let recorded_proposal_count_for_baker ctxt proposer =
  Storage.Vote.Proposals_count.get_option ctxt proposer
  >|=? Option.value ~default:0

let record_proposal ctxt proposal proposer =
  recorded_proposal_count_for_baker ctxt proposer
  >>=? fun count ->
  Storage.Vote.Proposals_count.init_set ctxt proposer (count + 1)
  >>= fun ctxt -> Storage.Vote.Proposals.add ctxt (proposal, proposer) >|= ok

let get_proposals ctxt =
  Storage.Vote.Proposals.fold
    ctxt
    ~init:(ok Protocol_hash.Map.empty)
    ~f:(fun (proposal, baker) acc ->
      (* Assuming the same listings is used at votings *)
      Storage.Vote.Listings.get ctxt baker
      >>=? fun weight ->
      Lwt.return
        ( acc
        >|? fun acc ->
        let previous =
          match Protocol_hash.Map.find_opt proposal acc with
          | None ->
              0l
          | Some x ->
              x
        in
        Protocol_hash.Map.add proposal (Int32.add weight previous) acc ))

let clear_proposals ctxt =
  Storage.Vote.Proposals_count.clear ctxt
  >>= fun ctxt -> Storage.Vote.Proposals.clear ctxt

let has_recorded_ballot = Storage.Vote.Ballots.mem

let record_ballot = Storage.Vote.Ballots.init

let try_get_ballot ctxt contract =
  let try_get_delegate_ballot contract =
    Storage.Contract.Delegate.get_option ctxt contract
    >>=? function
    | Some baker_hash ->
        Contract_repr.baker_contract baker_hash
        |> Storage.Vote.Ballots.get_option ctxt
    | None ->
        return None
  in
  Storage.Vote.Ballots.get_option ctxt contract
  >>=? function
  | Some contract_ballot ->
      return_some contract_ballot
  | None ->
      try_get_delegate_ballot contract

type ballots = {yay : int32; nay : int32; pass : int32}

let zero_ballots = {yay = 0l; nay = 0l; pass = 0l}

let ballots_encoding =
  let open Data_encoding in
  conv
    (fun {yay; nay; pass} -> (yay, nay, pass))
    (fun (yay, nay, pass) -> {yay; nay; pass})
  @@ obj3 (req "yay" int32) (req "nay" int32) (req "pass" int32)

(* get a map associating contracts to their total votes *)
let get_votes_map ctxt =
  let open Int64 in
  let constants = Raw_context.constants ctxt in
  let tokens_per_roll = Tez_repr.to_int64 constants.tokens_per_roll in
  let incr contract x =
    Contract_repr.Map.update contract (fun maybe_voting_balance ->
        Option.value ~default:0L maybe_voting_balance |> add x |> Option.some)
  in
  Storage.Contract.Balance.fold
    ctxt
    ~init:(ok (ctxt, Contract_repr.Map.empty))
    ~f:(fun contract balance acc ->
      Lwt.return acc
      >>=? fun (ctxt, votes_map) ->
      let balance = Tez_repr.to_int64 balance in
      let change = rem balance tokens_per_roll in
      let voting_balance = sub balance change in
      let votes_map = incr contract voting_balance votes_map in
      (* If there is a delegate, assign the change to the delegate *)
      Storage.Contract.Delegate.get_option ctxt contract
      >|=? Option.map Contract_repr.baker_contract
      >>=? function
      | None ->
          return (ctxt, votes_map)
      | Some baker ->
          incr baker change votes_map
          |> fun votes_map -> return (ctxt, votes_map))
  (* Add frozen balances *)
  >>=? fun (ctxt, votes_map) ->
  Storage.Baker.Registered.fold
    ctxt
    ~init:(ok (ctxt, votes_map))
    ~f:(fun baker_hash acc ->
      Lwt.return acc
      >>=? fun (ctxt, votes_map) ->
      Baker_storage.frozen_balance ctxt baker_hash
      >>=? fun frozen_balance ->
      let baker = Contract_repr.baker_contract baker_hash in
      incr baker (Tez_repr.to_int64 frozen_balance) votes_map
      |> fun votes_map -> return (ctxt, votes_map))
  (* Divide all balances by tokens_per_roll *)
  >>=? fun (_, votes_map) ->
  Contract_repr.Map.map
    (fun votes -> to_int32 (div votes tokens_per_roll))
    votes_map
  (* Remove zeros *)
  |> Contract_repr.Map.filter (fun _ -> Compare.Int32.( <> ) 0l)
  |> return

let count (ballot : Vote_repr.ballot) weight ballots =
  let allocate fraction total =
    Int32.(add (mul weight (of_int fraction)) total)
  in
  {
    yay = allocate ballot.yays_per_roll ballots.yay;
    nay = allocate ballot.nays_per_roll ballots.nay;
    pass = allocate ballot.passes_per_roll ballots.pass;
  }

let get_ballots ctxt =
  get_votes_map ctxt
  >>=? fun votes_map ->
  Contract_repr.Map.fold
    (fun contract votes ballots ->
      ballots
      >>=? fun ballots ->
      try_get_ballot ctxt contract
      >>=? function
      | None ->
          return ballots
      | Some ballot ->
          count ballot votes ballots |> return)
    votes_map
    (return zero_ballots)

let get_ballot_list = Storage.Vote.Ballots.bindings

let clear_ballots = Storage.Vote.Ballots.clear

let listings_encoding =
  Data_encoding.(
    list (obj2 (req "contract" Contract_repr.encoding) (req "votes" int32)))

let update_listings ctxt =
  Storage.Vote.Listings.clear ctxt
  >>= fun ctxt ->
  Roll_storage.fold ctxt (ctxt, 0l) ~f:(fun _roll baker (ctxt, total) ->
      (* TODO use snapshots *)
      Storage.Vote.Listings.get_option ctxt baker
      >|=? Option.value ~default:0l
      >>=? fun count ->
      Storage.Vote.Listings.init_set ctxt baker (Int32.succ count)
      >|= fun ctxt -> ok (ctxt, Int32.succ total))
  >>=? fun (ctxt, total) ->
  Storage.Vote.Listings_size.init_set ctxt total >|= ok

let listing_size = Storage.Vote.Listings_size.get

let in_listings ctxt contract =
  match Contract_repr.is_baker contract with
  | Some baker_hash ->
      Storage.Vote.Listings.mem ctxt baker_hash
  | None ->
      Lwt.return false

let get_listings ctxt =
  Storage.Vote.Listings.bindings ctxt
  >|= List.map (fun (baker_hash, votes) ->
          (Contract_repr.baker_contract baker_hash, votes))

let get_voting_power_free ctxt owner =
  Storage.Vote.Listings.get_option ctxt owner >|=? Option.value ~default:0l

(* This function bypases the carbonated functors to account for gas consumption.
   This is a temporary situation intended to be fixed by adding the right
   carbonated functors in version 008 *)
let get_voting_power ctxt owner =
  let open Raw_context in
  let open Gas_limit_repr in
  (* Always consume read access to memory *)
  consume_gas ctxt (read_bytes_cost Z.zero)
  >>?= fun ctxt ->
  Storage.Vote.Listings.get_option ctxt owner
  >>=? function
  | None ->
      return (ctxt, 0l)
  | Some power ->
      (* If some power is returned, consume read size_of(int32) = 4 bytes *)
      Lwt.return
        ( consume_gas ctxt (read_bytes_cost (Z.of_int 4))
        >|? fun ctxt -> (ctxt, power) )

let get_total_voting_power_free = listing_size

(* This function bypases the carbonated functors to account for gas consumption.
   This is a temporary situation intended to be fixed by adding the right
   carbonated functors in version 008 *)
let get_total_voting_power ctxt =
  let open Raw_context in
  let open Gas_limit_repr in
  listing_size ctxt
  >>=? fun total_power ->
  (* Consume access to memory and read size_of(int32) = 4 bytes *)
  Lwt.return
    ( consume_gas ctxt (read_bytes_cost Z.zero)
    >>? fun ctxt ->
    consume_gas ctxt (read_bytes_cost (Z.of_int 4))
    >|? fun ctxt -> (ctxt, total_power) )

let get_current_period_kind = Storage.Vote.Current_period_kind.get

let set_current_period_kind = Storage.Vote.Current_period_kind.set

let get_current_quorum ctxt =
  Storage.Vote.Participation_ema.get ctxt
  >|=? fun participation_ema ->
  let quorum_min = Constants_storage.quorum_min ctxt in
  let quorum_max = Constants_storage.quorum_max ctxt in
  let quorum_diff = Int32.sub quorum_max quorum_min in
  Int32.(add quorum_min (div (mul participation_ema quorum_diff) 100_00l))

let get_participation_ema = Storage.Vote.Participation_ema.get

let set_participation_ema = Storage.Vote.Participation_ema.set

let get_current_proposal = Storage.Vote.Current_proposal.get

let init_current_proposal = Storage.Vote.Current_proposal.init

let clear_current_proposal = Storage.Vote.Current_proposal.delete

let init ctxt =
  (* participation EMA is in centile of a percentage *)
  let participation_ema = Constants_storage.quorum_max ctxt in
  Storage.Vote.Participation_ema.init ctxt participation_ema
  >>=? fun ctxt -> Storage.Vote.Current_period_kind.init ctxt Proposal
