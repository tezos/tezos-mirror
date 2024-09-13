(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

(** Testing
    -------
    Component:  Protocol (voting)
    Invocation: dune exec src/proto_021_PsquebeC/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_voting.ml
    Subject:    On the voting process.

*)

open Protocol
open Alpha_context

(** {2 Constants and ratios used in voting}

   percent_mul denotes the percent multiplier
   initial_participation is 7000 that is, 7/10 * percent_mul
   the participation EMA ratio pr_ema_weight / den = 7 / 10
   the participation ratio pr_num / den = 2 / 10
   note: we use the same denominator for both participation EMA and participation rate.
   supermajority rate is s_num / s_den = 8 / 10 *)
let percent_mul = 100_00

let den = 10

let initial_participation_num = 7

let initial_participation = initial_participation_num * percent_mul / den

let pr_ema_weight = 8

let pr_num = den - pr_ema_weight

let s_num = 8

let s_den = 10

let qr_min_num = 2

let qr_max_num = 7

let expected_qr_num participation_ema =
  let participation_ema = Int32.to_int participation_ema in
  let participation_ema = participation_ema * den / percent_mul in
  Float.(
    of_int qr_min_num
    +. of_int participation_ema
       *. (of_int qr_max_num -. of_int qr_min_num)
       /. of_int den)

(* Protocol_hash.zero is "PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i" *)
let protos =
  Array.map
    (fun s -> Protocol_hash.of_b58check_exn s)
    [|
      "ProtoALphaALphaALphaALphaALphaALphaALpha61322gcLUGH";
      "ProtoALphaALphaALphaALphaALphaALphaALphabc2a7ebx6WB";
      "ProtoALphaALphaALphaALphaALphaALphaALpha84efbeiF6cm";
      "ProtoALphaALphaALphaALphaALphaALphaALpha91249Z65tWS";
      "ProtoALphaALphaALphaALphaALphaALphaALpha537f5h25LnN";
      "ProtoALphaALphaALphaALphaALphaALphaALpha5c8fefgDYkr";
      "ProtoALphaALphaALphaALphaALphaALphaALpha3f31feSSarC";
      "ProtoALphaALphaALphaALphaALphaALphaALphabe31ahnkxSC";
      "ProtoALphaALphaALphaALphaALphaALphaALphabab3bgRb7zQ";
      "ProtoALphaALphaALphaALphaALphaALphaALphaf8d39cctbpk";
      "ProtoALphaALphaALphaALphaALphaALphaALpha3b981byuYxD";
      "ProtoALphaALphaALphaALphaALphaALphaALphaa116bccYowi";
      "ProtoALphaALphaALphaALphaALphaALphaALphacce68eHqboj";
      "ProtoALphaALphaALphaALphaALphaALphaALpha225c7YrWwR7";
      "ProtoALphaALphaALphaALphaALphaALphaALpha58743cJL6FG";
      "ProtoALphaALphaALphaALphaALphaALphaALphac91bcdvmJFR";
      "ProtoALphaALphaALphaALphaALphaALphaALpha1faaadhV7oW";
      "ProtoALphaALphaALphaALphaALphaALphaALpha98232gD94QJ";
      "ProtoALphaALphaALphaALphaALphaALphaALpha9d1d8cijvAh";
      "ProtoALphaALphaALphaALphaALphaALphaALphaeec52dKF6Gx";
      "ProtoALphaALphaALphaALphaALphaALphaALpha841f2cQqajX";
    |]

(** {2 Helper functions} *)

let assert_period_kinds expected_kinds kind loc =
  let open Lwt_result_syntax in
  if
    List.exists
      (fun expected_kind -> Stdlib.(expected_kind = kind))
      expected_kinds
  then return_unit
  else
    Alcotest.failf
      "%s - Unexpected voting period kind - expected %a, got %a"
      loc
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
         Voting_period.pp_kind)
      expected_kinds
      Voting_period.pp_kind
      kind

let assert_period_kind expected_kind = assert_period_kinds [expected_kind]

let assert_period_index expected_index index loc =
  let open Lwt_result_syntax in
  if expected_index = index then return_unit
  else
    Alcotest.failf
      "%s - Unexpected voting period index - expected %ld, got %ld"
      loc
      expected_index
      index

let assert_period_position expected_position position loc =
  let open Lwt_result_syntax in
  if position = expected_position then return_unit
  else
    Alcotest.failf
      "%s - Unexpected voting period position blocks - expected %ld, got %ld"
      loc
      expected_position
      position

let assert_period_remaining expected_remaining remaining loc =
  let open Lwt_result_syntax in
  if remaining = expected_remaining then return_unit
  else
    Alcotest.failf
      "%s - Unexpected voting period remaining blocks - expected %ld, got %ld"
      loc
      expected_remaining
      remaining

let assert_period ?expected_kind ?expected_kinds ?expected_index
    ?expected_position ?expected_remaining b loc =
  let open Lwt_result_syntax in
  let* {voting_period; position; remaining} =
    Context.Vote.get_current_period (B b)
  in
  let* () =
    match (expected_kind, expected_kinds) with
    | None, None -> return_unit
    | Some expected_kind, None ->
        assert_period_kind expected_kind voting_period.kind loc
    | None, Some expected_kinds ->
        assert_period_kinds expected_kinds voting_period.kind loc
    | Some _, Some _ ->
        invalid_arg
          "assert_period: arguments expected_kind and expected_kinds should \
           not both be provided."
  in
  let* () =
    match expected_index with
    | Some expected_index ->
        assert_period_index expected_index voting_period.index loc
    | None -> return_unit
  in
  let* () =
    match expected_position with
    | Some expected_position ->
        assert_period_position expected_position position loc
    | None -> return_unit
  in
  match expected_remaining with
  | Some expected_remaining ->
      assert_period_remaining expected_remaining remaining loc
  | None -> return_unit

let assert_ballots expected_ballots b loc =
  let open Lwt_result_syntax in
  let* ballots = Context.Vote.get_ballots (B b) in
  Assert.equal
    ~loc
    Vote.equal_ballots
    "Unexpected ballots"
    Vote.pp_ballots
    ballots
    expected_ballots

let assert_empty_ballots b loc =
  let open Lwt_result_syntax in
  let* () = assert_ballots Vote.ballots_zero b loc in
  let* l = Context.Vote.get_ballot_list (B b) in
  match l with
  | [] -> return_unit
  | _ -> failwith "%s - Unexpected ballot list" loc

let mk_contracts_from_pkh pkh_list =
  List.map (fun c -> Contract.Implicit c) pkh_list

(* get the list of delegates and the list of their voting power from listings *)
let get_delegates_and_power_from_listings b =
  let open Lwt_result_syntax in
  let+ l = Context.Vote.get_listings (B b) in
  (mk_contracts_from_pkh (List.map fst l), List.map snd l)

(* compute the voting power of each delegate *)
let get_power b delegates loc =
  let open Lwt_result_syntax in
  List.map_es
    (fun delegate ->
      let pkh = Context.Contract.pkh delegate in
      let* info = Context.Delegate.voting_info (B b) pkh in
      match info.voting_power with
      | None -> failwith "%s - Missing delegate" loc
      | Some power -> return power)
    delegates

(* Checks that the listings are populated *)
let assert_listings_not_empty b ~loc =
  let open Lwt_result_syntax in
  let* l = Context.Vote.get_listings (B b) in
  match l with
  | [] -> failwith "Unexpected empty listings (%s)" loc
  | _ -> return_unit

let equal_delegate_info a b =
  Option.equal Int64.equal a.Vote.voting_power b.Vote.voting_power
  && Option.equal Vote.equal_ballot a.current_ballot b.current_ballot
  && List.equal
       Protocol_hash.equal
       (List.sort Protocol_hash.compare a.current_proposals)
       (List.sort Protocol_hash.compare b.current_proposals)
  && Int.equal a.remaining_proposals b.remaining_proposals

let assert_equal_info ~loc a b =
  Assert.equal
    ~loc
    equal_delegate_info
    "delegate_info"
    Vote.pp_delegate_info
    a
    b

let bake_until_first_block_of_next_period ?policy b =
  let open Lwt_result_syntax in
  let* {remaining; _} = Context.Vote.get_current_period (B b) in
  Block.bake_n ?policy Int32.(add remaining one |> to_int) b

let context_init_tup tup ?(blocks_per_cycle = 4l) =
  (* Note that some of these tests assume (more or less) that the
     accounts remain active during a voting period, which roughly
     translates to the following condition being assumed to hold:
     `blocks_per_voting_period <= consensus_rights_delay * blocks_per_cycle.`
     We also set baking and attesting rewards to zero in order to
     ease accounting of exact baker stake. *)
  Context.init_gen
    tup
    ~blocks_per_cycle
    ~cycles_per_voting_period:1l
    ~consensus_threshold:0
    ~issuance_weights:
      {
        base_total_issued_per_minute = Tez.zero;
        attesting_reward_weight = 1;
        baking_reward_bonus_weight = 1;
        baking_reward_fixed_portion_weight = 1;
        seed_nonce_revelation_tip_weight = 1;
        vdf_revelation_tip_weight = 1;
      }
    ~nonce_revelation_threshold:2l

(** [context_init n ()] returns [(block, contracts)] where [block] is
    an initial block with [n] bootstrap accounts, and [contracts] is
    the list of associated implicit contracts.

    See {!context_init_tup} and {!Context.init_gen} for optional
    arguments. *)
let context_init n = context_init_tup (Context.TList n)

(** [context_init1 ()] returns [(block, contract)] where [block] is an
    initial block with one bootstrap account, and [contract] is the
    associated implicit contract. *)
let context_init1 = context_init_tup Context.T1

(** [context_init2 ()] returns [(block, contracts)] where [block] is
    an initial block with two bootstrap accounts, and [contracts] is
    the pair of associated implicit contracts. *)
let context_init2 = context_init_tup Context.T2

(** Call {!context_init2}, then inject a Proposals operation and bake
    blocks in order to move on to an Exploration period. Return a
    block, a delegate (distinct from the one who submitted the
    Proposals), and the current proposal. *)
let context_init_exploration ?(proposal = protos.(0)) ?blocks_per_cycle () =
  let open Lwt_result_syntax in
  let* block, (proposer, other_delegate) = context_init2 ?blocks_per_cycle () in
  let* operation = Op.proposals (B block) proposer [proposal] in
  let* block = Block.bake block ~operation in
  let* block = bake_until_first_block_of_next_period block in
  let* () = assert_period ~expected_kind:Exploration block __LOC__ in
  return (block, other_delegate, proposal)

let append_loc ~caller_loc loc =
  Format.sprintf "%s@.Called from %s" loc caller_loc

(** {3 Expected protocol errors} *)

let wrong_error expected_error_name actual_error_trace loc =
  failwith
    "%s:@,Expected error trace [%s], but got:@,%a"
    loc
    expected_error_name
    Error_monad.pp_print_trace
    actual_error_trace

let missing_signature loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error Operation.Missing_signature] -> return_unit
  | err -> wrong_error "Missing_signature" err loc

let invalid_signature loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error Operation.Invalid_signature] -> return_unit
  | err -> wrong_error "Invalid_signature" err loc

open Validate_errors.Voting

let wrong_voting_period_index ~current_index ~op_index loc = function
  | [
      Environment.Ecoproto_error (Wrong_voting_period_index {expected; provided});
    ] ->
      let open Lwt_result_syntax in
      let make_loc = append_loc ~caller_loc:loc in
      let* () =
        Assert.equal_int32 ~loc:(make_loc __LOC__) expected current_index
      in
      Assert.equal_int32 ~loc:(make_loc __LOC__) provided op_index
  | err -> wrong_error "Wrong_voting_period_index" err loc

let wrong_voting_period_kind loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error (Wrong_voting_period_kind _)] -> return_unit
  | err -> wrong_error "Wrong_voting_period_kind" err loc

let proposals_from_unregistered_delegate loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error (Proposals_from_unregistered_delegate _)] ->
      return_unit
  | err -> wrong_error "Proposals_from_unregistered_delegate" err loc

let ballot_from_unregistered_delegate loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error (Ballot_from_unregistered_delegate _)] ->
      return_unit
  | err -> wrong_error "Ballot_from_unregistered_delegate" err loc

let source_not_in_vote_listings loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error Source_not_in_vote_listings] -> return_unit
  | err -> wrong_error "Source_not_in_vote_listings" err loc

let empty_proposals loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error Empty_proposals] -> return_unit
  | err -> wrong_error "Empty_proposals" err loc

let proposals_contain_duplicate duplicate_proposal loc = function
  | [Environment.Ecoproto_error (Proposals_contain_duplicate {proposal})] ->
      Assert.equal_protocol_hash
        ~loc:(append_loc ~caller_loc:loc __LOC__)
        proposal
        duplicate_proposal
  | err -> wrong_error "Proposals_contain_duplicate" err loc

let too_many_proposals loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error (Too_many_proposals _)] -> return_unit
  | err -> wrong_error "Too_many_proposals" err loc

let already_proposed already_proposed_proposal loc = function
  | [Environment.Ecoproto_error (Already_proposed {proposal; _})] ->
      Assert.equal_protocol_hash
        ~loc:(append_loc ~caller_loc:loc __LOC__)
        proposal
        already_proposed_proposal
  | err -> wrong_error "Already_proposed" err loc

let conflicting_proposals loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error (Conflicting_proposals _)] -> return_unit
  | err -> wrong_error "Conflicting_proposals" err loc

let ballot_for_wrong_proposal ~current_proposal ~op_proposal loc = function
  | [
      Environment.Ecoproto_error (Ballot_for_wrong_proposal {current; submitted});
    ] ->
      let open Lwt_result_syntax in
      let* () =
        Assert.equal_protocol_hash
          ~loc:(append_loc ~caller_loc:loc __LOC__)
          current_proposal
          current
      in
      Assert.equal_protocol_hash
        ~loc:(append_loc ~caller_loc:loc __LOC__)
        op_proposal
        submitted
  | err -> wrong_error "Ballot_for_wrong_proposal" err loc

let already_submitted_a_ballot loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error Already_submitted_a_ballot] -> return_unit
  | err -> wrong_error "Already_submitted_a_ballot" err loc

let conflicting_ballot loc =
  let open Lwt_result_syntax in
  function
  | [Environment.Ecoproto_error (Conflicting_ballot _)] -> return_unit
  | err -> wrong_error "Conflicting_ballot" err loc

let assert_validate_proposals_fails ~expected_error ~proposer ~proposals ?period
    block loc =
  let open Lwt_result_syntax in
  let* operation = Op.proposals (B block) proposer ?period proposals in
  Incremental.assert_validate_operation_fails
    (expected_error loc)
    operation
    block

let assert_validate_ballot_fails ~expected_error ~voter ~proposal ~ballot
    ?period block loc =
  let open Lwt_result_syntax in
  let* operation = Op.ballot (B block) voter ?period proposal ballot in
  Incremental.assert_validate_operation_fails
    (expected_error loc)
    operation
    block

(** {2 Scenarized tests} *)

(** A normal and successful vote sequence. *)
let test_successful_vote num_delegates () =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  let* b, (_ : Contract.t list) =
    context_init ~min_proposal_quorum num_delegates ()
  in
  (* no ballots in proposal period *)
  let* () = assert_empty_ballots b __LOC__ in
  (* Last baked block is first block of period Proposal *)
  let* () =
    assert_period
      ~expected_kind:Proposal
      ~expected_index:0l
      ~expected_position:0l
      b
      __LOC__
  in
  let* () = assert_listings_not_empty b ~loc:__LOC__ in
  (* participation EMA starts at initial_participation *)
  let* v = Context.Vote.get_participation_ema b in
  let* () =
    Assert.equal_int ~loc:__LOC__ initial_participation (Int32.to_int v)
  in
  (* listings must be populated in proposal period *)
  let* () = assert_listings_not_empty b ~loc:__LOC__ in
  (* beginning of proposal, denoted by _p1;
     take a snapshot of the active delegates and their voting power from listings *)
  let* delegates_p1, power_p1 = get_delegates_and_power_from_listings b in
  (* no proposals at the beginning of proposal period *)
  let* ps = Context.Vote.get_proposals (B b) in
  let* () =
    if Environment.Protocol_hash.Map.is_empty ps then return_unit
    else failwith "%s - Unexpected proposals" __LOC__
  in
  (* no current proposal during proposal period *)
  let* () =
    let* proposal = Context.Vote.get_current_proposal (B b) in
    match proposal with
    | None -> return_unit
    | Some _ -> failwith "%s - Unexpected proposal" __LOC__
  in
  let del1 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates_p1 0
  in
  let del2 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates_p1 1
  in
  let pkh1 = Context.Contract.pkh del1 in
  let pkh2 = Context.Contract.pkh del2 in
  let pow1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth power_p1 0 in
  let pow2 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth power_p1 1 in
  let props =
    List.map (fun i -> protos.(i)) (2 -- Constants.max_proposals_per_delegate)
  in
  let* ops1 = Op.proposals (B b) del1 (Protocol_hash.zero :: props) in
  let* ops2 = Op.proposals (B b) del2 [Protocol_hash.zero] in
  let* b = Block.bake ~operations:[ops1; ops2] b in
  let* info1 = Context.Delegate.voting_info (B b) pkh1 in
  let* info2 = Context.Delegate.voting_info (B b) pkh2 in
  let* () =
    assert_equal_info
      ~loc:__LOC__
      info1
      {
        voting_power = Some pow1;
        current_ballot = None;
        current_proposals = Protocol_hash.zero :: props;
        remaining_proposals = 0;
      }
  in
  let* () =
    assert_equal_info
      ~loc:__LOC__
      info2
      {
        voting_power = Some pow2;
        current_ballot = None;
        current_proposals = [Protocol_hash.zero];
        remaining_proposals = Constants.max_proposals_per_delegate - 1;
      }
  in
  (* proposals are now populated *)
  let* ps = Context.Vote.get_proposals (B b) in
  (* correctly count the double proposal for zero *)
  let* () =
    let weight =
      Int64.add
        (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth power_p1 0)
        (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth power_p1 1)
    in
    match Environment.Protocol_hash.(Map.find zero ps) with
    | Some v ->
        if v = weight then return_unit
        else failwith "%s - Wrong count %Ld is not %Ld" __LOC__ v weight
    | None -> failwith "%s - Missing proposal" __LOC__
  in
  (* proposing more than maximum_proposals fails *)
  let* () =
    assert_validate_proposals_fails
      ~expected_error:too_many_proposals
      ~proposer:del1
      ~proposals:(Protocol_hash.zero :: props)
      b
      __LOC__
  in
  (* proposing less than one proposal fails *)
  let* () =
    assert_validate_proposals_fails
      ~expected_error:empty_proposals
      ~proposer:del1
      ~proposals:[]
      b
      __LOC__
  in
  (* first block of exploration period *)
  let* b = bake_until_first_block_of_next_period b in
  (* next block is first block of exploration *)
  let* () =
    assert_period ~expected_kind:Exploration ~expected_index:1l b __LOC__
  in
  let* () = assert_listings_not_empty b ~loc:__LOC__ in
  (* listings must be populated in proposal period before moving to exploration period *)
  let* () = assert_listings_not_empty b ~loc:__LOC__ in
  (* beginning of exploration period, denoted by _p2;
     take a snapshot of the active delegates and their voting power from listings *)
  let* delegates_p2, power_p2 = get_delegates_and_power_from_listings b in
  (* no proposals during exploration period *)
  let* ps = Context.Vote.get_proposals (B b) in
  let* () =
    if Environment.Protocol_hash.Map.is_empty ps then return_unit
    else failwith "%s - Unexpected proposals" __LOC__
  in
  (* current proposal must be set during exploration period *)
  let* () =
    let* v_opt = Context.Vote.get_current_proposal (B b) in
    match v_opt with
    | Some v ->
        if Protocol_hash.(equal zero v) then return_unit
        else failwith "%s - Wrong proposal" __LOC__
    | None -> failwith "%s - Missing proposal" __LOC__
  in
  (* unanimous vote: all delegates --active when p2 started-- vote *)
  let* operations =
    List.map_es
      (fun del -> Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
      delegates_p2
  in
  let* b = Block.bake ~operations b in
  let* info1 = Context.Delegate.voting_info (B b) pkh1 in
  let* () =
    assert_equal_info
      ~loc:__LOC__
      info1
      {
        voting_power = Some pow1;
        current_ballot = Some Yay;
        current_proposals = [];
        remaining_proposals = 0;
      }
  in
  (* Submitting a second ballot for [del1] fails (indeed, [del1]
     belongs to [delegates_p2], so they have already sent a ballot
     during the unanimous vote right above). *)
  let* () =
    assert_validate_ballot_fails
      ~expected_error:already_submitted_a_ballot
      ~voter:del1
      ~proposal:Protocol_hash.zero
      ~ballot:Vote.Nay
      b
      __LOC__
  in
  (* Allocate votes from weight of active delegates *)
  List.fold_left (fun acc v -> Int64.(add v acc)) 0L power_p2
  |> fun power_sum ->
  (* # of Yay in ballots matches votes of the delegates *)
  let* () =
    assert_ballots Vote.{yay = power_sum; nay = 0L; pass = 0L} b __LOC__
  in
  (* One Yay ballot per delegate *)
  let* () =
    let* l_opt = Context.Vote.get_ballot_list (B b) in
    match l_opt with
    | [] -> failwith "%s - Unexpected empty ballot list" __LOC__
    | l ->
        List.iter_es
          (fun delegate ->
            let pkh = Context.Contract.pkh delegate in
            match List.find_opt (fun (del, _) -> del = pkh) l with
            | None -> failwith "%s - Missing delegate" __LOC__
            | Some (_, Vote.Yay) -> return_unit
            | Some _ -> failwith "%s - Wrong ballot" __LOC__)
          delegates_p2
  in
  (* skip to cooldown period *)
  let* b = bake_until_first_block_of_next_period b in
  let* () =
    assert_period ~expected_index:2l ~expected_kind:Cooldown b __LOC__
  in
  (* no ballots in cooldown period *)
  let* () = assert_empty_ballots b __LOC__ in
  (* listings must be populated in cooldown period before moving to promotion_vote period *)
  let* () = assert_listings_not_empty b ~loc:__LOC__ in
  (* skip to promotion period *)
  let* b = bake_until_first_block_of_next_period b in
  let* () =
    assert_period ~expected_kind:Promotion ~expected_index:3l b __LOC__
  in
  let* () = assert_listings_not_empty b ~loc:__LOC__ in
  (* period 3 *)
  (* listings must be populated in promotion period *)
  let* () = assert_listings_not_empty b ~loc:__LOC__ in
  (* beginning of promotion period, denoted by _p4;
     take a snapshot of the active delegates and their voting power from listings *)
  let* delegates_p4, power_p4 = get_delegates_and_power_from_listings b in
  (* no proposals during promotion period *)
  let* ps = Context.Vote.get_proposals (B b) in
  let* () =
    if Environment.Protocol_hash.Map.is_empty ps then return_unit
    else failwith "%s - Unexpected proposals" __LOC__
  in
  (* current proposal must be set during promotion period *)
  let* () =
    let* v_opt = Context.Vote.get_current_proposal (B b) in
    match v_opt with
    | Some v ->
        if Protocol_hash.(equal zero v) then return_unit
        else failwith "%s - Wrong proposal" __LOC__
    | None -> failwith "%s - Missing proposal" __LOC__
  in
  (* unanimous vote: all delegates --active when p4 started-- vote *)
  let* operations =
    List.map_es
      (fun del -> Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
      delegates_p4
  in
  let* b = Block.bake ~operations b in
  List.fold_left (fun acc v -> Int64.(add v acc)) 0L power_p4
  |> fun power_sum ->
  (* # of Yays in ballots matches voting power of the delegate *)
  let* () =
    assert_ballots Vote.{yay = power_sum; nay = 0L; pass = 0L} b __LOC__
  in
  (* One Yay ballot per delegate *)
  let* () =
    let* l_opt = Context.Vote.get_ballot_list (B b) in
    match l_opt with
    | [] -> failwith "%s - Unexpected empty ballot list" __LOC__
    | l ->
        List.iter_es
          (fun delegate ->
            let pkh = Context.Contract.pkh delegate in
            match List.find_opt (fun (del, _) -> del = pkh) l with
            | None -> failwith "%s - Missing delegate" __LOC__
            | Some (_, Vote.Yay) -> return_unit
            | Some _ -> failwith "%s - Wrong ballot" __LOC__)
          delegates_p4
  in
  (* skip to end of promotion period and activation*)
  let* b = bake_until_first_block_of_next_period b in
  let* () =
    assert_period ~expected_kind:Adoption ~expected_index:4l b __LOC__
  in
  (* skip to end of Adoption period and bake 1 more to activate *)
  let* b = bake_until_first_block_of_next_period b in
  let* () =
    assert_period ~expected_kind:Proposal ~expected_index:5l b __LOC__
  in
  let* () = assert_listings_not_empty b ~loc:__LOC__ in
  (* zero is the new protocol (before the vote this value is unset) *)
  let*! p = Context.Vote.get_protocol b in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Protocol_hash.equal
      "Unexpected proposal"
      Protocol_hash.pp
      p
      Protocol_hash.zero
  in
  return_unit

(* given a list of active delegates,
   return the first k active delegates with which one can have quorum, that is:
   their voting power divided by the total voting power is bigger than pr_ema_weight/den *)
let get_smallest_prefix_voters_for_quorum active_delegates active_power
    participation_ema =
  let expected_quorum = expected_qr_num participation_ema in
  List.fold_left (fun acc v -> Int64.(add v acc)) 0L active_power
  |> fun active_power_sum ->
  let rec loop delegates power sum selected =
    match (delegates, power) with
    | [], [] -> selected
    | del :: delegates, del_power :: power ->
        if
          den * sum
          < Float.to_int (expected_quorum *. Int64.to_float active_power_sum)
        then
          loop delegates power (sum + Int64.to_int del_power) (del :: selected)
        else selected
    | _, _ -> []
  in
  loop active_delegates active_power 0 []

let get_expected_participation_ema power voter_power old_participation_ema =
  (* formula to compute the updated participation_ema *)
  let get_updated_participation_ema old_participation_ema participation =
    ((pr_ema_weight * Int32.to_int old_participation_ema)
    + (pr_num * participation))
    / den
  in
  List.fold_left (fun acc v -> Int64.(add v acc)) 0L power |> fun power_sum ->
  List.fold_left (fun acc v -> Int64.(add v acc)) 0L voter_power
  |> fun voter_power_sum ->
  let participation =
    Int64.(to_int (div (mul voter_power_sum (of_int percent_mul)) power_sum))
  in
  get_updated_participation_ema old_participation_ema participation

(** If not enough quorum
    -- get_updated_participation_ema < pr_ema_weight/den --
    in exploration, go back to proposal period. *)
let test_not_enough_quorum_in_exploration num_delegates () =
  let open Lwt_result_syntax in
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  let* b, delegates = context_init ~min_proposal_quorum num_delegates () in
  (* proposal period *)
  let open Alpha_context in
  let* () = assert_period ~expected_kind:Proposal b __LOC__ in
  let proposer =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0
  in
  let* operation = Op.proposals (B b) proposer [Protocol_hash.zero] in
  let* b = Block.bake ~operation b in
  (* skip to exploration period *)
  let* b = bake_until_first_block_of_next_period b in
  (* we moved to an exploration period with one proposal *)
  let* () = assert_period ~expected_kind:Exploration b __LOC__ in
  let* initial_participation_ema = Context.Vote.get_participation_ema b in
  (* beginning of exploration period, denoted by _p2;
     take a snapshot of the active delegates and their voting power from listings *)
  let* delegates_p2, power_p2 = get_delegates_and_power_from_listings b in
  let* participation_ema = Context.Vote.get_participation_ema b in
  get_smallest_prefix_voters_for_quorum delegates_p2 power_p2 participation_ema
  |> fun voters ->
  (* take the first two voters out so there cannot be quorum *)
  let voters_without_quorum =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.tl voters
  in
  let* voters_power_in_exploration =
    get_power b voters_without_quorum __LOC__
  in
  (* all voters_without_quorum vote, for yays;
     no nays, so supermajority is satisfied *)
  let* operations =
    List.map_es
      (fun del -> Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
      voters_without_quorum
  in
  let* b = Block.bake ~operations b in
  (* bake to next period *)
  let* b = bake_until_first_block_of_next_period b in
  (* we move back to the proposal period because not enough quorum *)
  let* () = assert_period ~expected_kind:Proposal b __LOC__ in
  (* check participation_ema update *)
  get_expected_participation_ema
    power_p2
    voters_power_in_exploration
    initial_participation_ema
  |> fun expected_participation_ema ->
  let* new_participation_ema = Context.Vote.get_participation_ema b in
  (* assert the formula to calculate participation_ema is correct *)
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      expected_participation_ema
      (Int32.to_int new_participation_ema)
  in
  return_unit

(** If not enough quorum
   -- get_updated_participation_ema < pr_ema_weight/den --
   In promotion period, go back to proposal period. *)
let test_not_enough_quorum_in_promotion num_delegates () =
  let open Lwt_result_syntax in
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  let* b, delegates = context_init ~min_proposal_quorum num_delegates () in
  let* () = assert_period ~expected_kind:Proposal b __LOC__ in
  let proposer =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0
  in
  let* operation = Op.proposals (B b) proposer [Protocol_hash.zero] in
  let* b = Block.bake ~operation b in
  (* skip to exploration period *)
  let* b = bake_until_first_block_of_next_period b in
  (* we moved to an exploration period with one proposal *)
  let* () = assert_period ~expected_kind:Exploration b __LOC__ in
  (* beginning of exploration period, denoted by _p2;
     take a snapshot of the active delegates and their voting power from listings *)
  let* delegates_p2, power_p2 = get_delegates_and_power_from_listings b in
  let* participation_ema = Context.Vote.get_participation_ema b in
  get_smallest_prefix_voters_for_quorum delegates_p2 power_p2 participation_ema
  |> fun voters ->
  let open Alpha_context in
  (* all voters vote, for yays;
       no nays, so supermajority is satisfied *)
  let* operations =
    List.map_es
      (fun del -> Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
      voters
  in
  let* b = Block.bake ~operations b in
  (* skip to first block cooldown period *)
  let* b = bake_until_first_block_of_next_period b in
  (* we move to cooldown because we have supermajority and enough quorum *)
  let* () = assert_period ~expected_kind:Cooldown b __LOC__ in
  (* skip to first block of promotion period *)
  let* b = bake_until_first_block_of_next_period b in
  let* () =
    assert_period ~expected_kind:Promotion b __LOC__
    (* let* b = bake_until_first_block_of_next_period ~offset:1l b
     * in assert_period ~expected_kind:Promotion b __LOC__ *)
  in
  let* initial_participation_ema = Context.Vote.get_participation_ema b in
  (* beginning of promotion period, denoted by _p4;
     take a snapshot of the active delegates and their voting power from listings *)
  let* delegates_p4, power_p4 = get_delegates_and_power_from_listings b in
  let* participation_ema = Context.Vote.get_participation_ema b in
  get_smallest_prefix_voters_for_quorum delegates_p4 power_p4 participation_ema
  |> fun voters ->
  (* take the first voter out so there cannot be quorum *)
  let voters_without_quorum =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.tl voters
  in
  let* voter_power = get_power b voters_without_quorum __LOC__ in
  (* all voters_without_quorum vote, for yays;
     no nays, so supermajority is satisfied *)
  let* operations =
    List.map_es
      (fun del -> Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
      voters_without_quorum
  in
  let* b = Block.bake ~operations b in
  (* skip to end of promotion period *)
  let* b = bake_until_first_block_of_next_period b in
  get_expected_participation_ema power_p4 voter_power initial_participation_ema
  |> fun expected_participation_ema ->
  let* new_participation_ema = Context.Vote.get_participation_ema b in
  (* assert the formula to calculate participation_ema is correct *)
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      expected_participation_ema
      (Int32.to_int new_participation_ema)
  in
  (* we move back to the proposal period because not enough quorum *)
  let* () = assert_period ~expected_kind:Proposal b __LOC__ in
  let* () = assert_listings_not_empty b ~loc:__LOC__ in
  return_unit

(** Assume the initial balance of accounts allocated by Context.init_n is at
    least 4 times the value of the minimal_stake constant. *)
let test_supermajority_in_proposal there_is_a_winner () =
  let open Lwt_result_syntax in
  let min_proposal_quorum = 0l in
  let initial_balance = 1L in
  let* b, delegates =
    context_init
      ~min_proposal_quorum
      ~bootstrap_balances:[initial_balance; initial_balance; initial_balance]
      10
      ()
  in
  let* {
         parametric =
           {
             minimal_stake;
             minimal_frozen_stake;
             adaptive_issuance = {autostaking_enable; _};
             _;
           };
         _;
       } =
    Context.get_constants (B b)
  in
  let del1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0 in
  let del2 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 1 in
  let del3 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 2 in
  let pkhs =
    List.map (fun del -> Context.Contract.pkh del) [del1; del2; del3]
  in
  let policy = Block.Excluding pkhs in
  let* op1 =
    Op.transaction
      (B b)
      (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 3)
      del1
      minimal_stake
  in
  let* op2 =
    Op.transaction
      (B b)
      (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 4)
      del2
      minimal_stake
  in
  let*? bal3 =
    if there_is_a_winner then Tez_helpers.( *? ) minimal_stake 3L
    else
      let open Result_syntax in
      let* t = Tez_helpers.( *? ) minimal_stake 2L in
      Tez_helpers.( +? ) (Tez_helpers.of_mutez initial_balance) t
  in
  let* op3 =
    Op.transaction
      (B b)
      (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 5)
      del3
      bal3
  in
  let* staking_ops =
    if autostaking_enable then return_nil
    else
      let* op4 =
        Adaptive_issuance_helpers.stake (B b) del1 minimal_frozen_stake
      in
      let* op5 =
        Adaptive_issuance_helpers.stake (B b) del2 minimal_frozen_stake
      in
      let* op6 =
        Adaptive_issuance_helpers.stake (B b) del3 minimal_frozen_stake
      in
      return [op4; op5; op6]
  in
  let* b = Block.bake ~policy ~operations:([op1; op2; op3] @ staking_ops) b in
  let* b = bake_until_first_block_of_next_period ~policy b in
  (* make the proposals *)
  let* ops1 = Op.proposals (B b) del1 [protos.(0)] in
  let* ops2 = Op.proposals (B b) del2 [protos.(0)] in
  let* ops3 = Op.proposals (B b) del3 [protos.(1)] in
  let* b = Block.bake ~policy ~operations:[ops1; ops2; ops3] b in
  let* b = bake_until_first_block_of_next_period ~policy b in
  (* we remain in the proposal period when there is no winner,
     otherwise we move to the exploration period *)
  let* () =
    if there_is_a_winner then assert_period ~expected_kind:Exploration b __LOC__
    else assert_period ~expected_kind:Proposal b __LOC__
  in
  return_unit

(** After one voting period, if [has_quorum] then the period kind must
    have been the cooldown vote. Otherwise, it should have remained in
    place in the proposal period. *)
let test_quorum_in_proposal has_quorum () =
  let open Lwt_result_syntax in
  let total_tokens = 32_000_000_000_000L in
  let half_tokens = Int64.div total_tokens 2L in
  let* b, delegates =
    context_init ~bootstrap_balances:[1L; half_tokens; half_tokens] 3 ()
  in
  let* {
         parametric =
           {min_proposal_quorum; adaptive_issuance = {autostaking_enable; _}; _};
         _;
       } =
    Context.get_constants (B b)
  in
  let del1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0 in
  let del2 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 1 in
  let pkhs = List.map (fun del -> Context.Contract.pkh del) [del1; del2] in
  let policy = Block.Excluding pkhs in
  let quorum =
    if has_quorum then Int64.of_int32 min_proposal_quorum
    else Int64.(sub (of_int32 min_proposal_quorum) 10L)
  in
  let bal =
    Int64.(div (mul total_tokens quorum) 100_00L) |> Tez_helpers.of_mutez
  in
  let* op2 = Op.transaction (B b) del2 del1 bal in
  let* b = Block.bake ~policy ~operation:op2 b in
  let* b =
    if autostaking_enable then return b
    else
      let* stake = Adaptive_issuance_helpers.stake (B b) del1 bal in
      Block.bake ~policy ~operation:stake b
  in
  let* b = bake_until_first_block_of_next_period b in
  (* make the proposal *)
  let* operation = Op.proposals (B b) del1 [protos.(0)] in
  let* b = Block.bake ~policy ~operation b in
  let* b = bake_until_first_block_of_next_period b in
  (* we remain in the proposal period when there is no quorum,
     otherwise we move to the cooldown vote period *)
  let* () =
    if has_quorum then assert_period ~expected_kind:Exploration b __LOC__
    else assert_period ~expected_kind:Proposal b __LOC__
  in
  return_unit

(** If a supermajority is reached, then the voting period must be
    reached. Otherwise, it remains in proposal period. *)
let test_supermajority_in_exploration supermajority () =
  let open Lwt_result_syntax in
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / 100)) in
  let* b, delegates = context_init ~min_proposal_quorum 100 () in
  let del1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0 in
  let proposal = protos.(0) in
  let* operation = Op.proposals (B b) del1 [proposal] in
  let* b = Block.bake ~operation b in
  let* b = bake_until_first_block_of_next_period b in
  (* move to exploration *)
  let* () = assert_period ~expected_kind:Exploration b __LOC__ in
  (* assert our proposal won *)
  let* () =
    let* v_opt = Context.Vote.get_current_proposal (B b) in
    match v_opt with
    | Some v ->
        if Protocol_hash.(equal proposal v) then return_unit
        else failwith "%s - Wrong proposal" __LOC__
    | None -> failwith "%s - Missing proposal" __LOC__
  in
  (* beginning of exploration period, denoted by _p2;
     take a snapshot of the active delegates and their voting power from listings *)
  let* delegates_p2, _power_p2 = get_delegates_and_power_from_listings b in
  (* supermajority means [num_yays / (num_yays + num_nays) >= s_num / s_den],
     which is equivalent with [num_yays >= num_nays * s_num / (s_den - s_num)] *)
  let num_delegates = List.length delegates_p2 in
  let num_nays = num_delegates / 5 in
  (* any smaller number will do as well *)
  let num_yays = num_nays * s_num / (s_den - s_num) in
  (* majority/minority vote depending on the [supermajority] parameter *)
  let num_yays = if supermajority then num_yays else num_yays - 1 in
  let open Alpha_context in
  let nays_delegates, rest = List.split_n num_nays delegates_p2 in
  let yays_delegates, _ = List.split_n num_yays rest in
  let* operations_yays =
    List.map_es
      (fun del -> Op.ballot (B b) del proposal Vote.Yay)
      yays_delegates
  in
  let* operations_nays =
    List.map_es
      (fun del -> Op.ballot (B b) del proposal Vote.Nay)
      nays_delegates
  in
  let operations = operations_yays @ operations_nays in
  let* b = Block.bake ~operations b in
  let* b = bake_until_first_block_of_next_period b in
  let* () =
    if supermajority then assert_period ~expected_kind:Cooldown b __LOC__
    else assert_period ~expected_kind:Proposal b __LOC__
  in
  return_unit

(** Test also how the selection scales: all delegates propose max
    proposals. *)
let test_no_winning_proposal num_delegates () =
  let open Lwt_result_syntax in
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  let* b, _ = context_init ~min_proposal_quorum num_delegates () in
  (* beginning of proposal, denoted by _p1;
     take a snapshot of the active delegates and their voting power from listings *)
  let* delegates_p1, _power_p1 = get_delegates_and_power_from_listings b in
  let open Alpha_context in
  let props =
    List.map (fun i -> protos.(i)) (1 -- Constants.max_proposals_per_delegate)
  in
  (* all delegates active in p1 propose the same proposals *)
  let* operations =
    List.map_es (fun del -> Op.proposals (B b) del props) delegates_p1
  in
  let* b = Block.bake ~operations b in
  (* skip to exploration period *)
  let* b = bake_until_first_block_of_next_period b in
  (* we stay in the same proposal period because no winning proposal *)
  let* () = assert_period ~expected_kind:Proposal b __LOC__ in
  return_unit

(** Vote to pass with maximum possible participation_ema (100%), it is
    sufficient for the vote quorum to be equal or greater than the
    maximum quorum cap. *)
let test_quorum_capped_maximum num_delegates () =
  let open Lwt_result_syntax in
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  let* b, delegates = context_init ~min_proposal_quorum num_delegates () in
  (* set the participation EMA to 100% *)
  let*! b = Context.Vote.set_participation_ema b 100_00l in
  let* {parametric = {quorum_max; _}; _} = Context.get_constants (B b) in
  (* proposal period *)
  let open Alpha_context in
  let* () = assert_period ~expected_kind:Proposal b __LOC__ in
  (* propose a new protocol *)
  let protocol = Protocol_hash.zero in
  let proposer =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0
  in
  let* operation = Op.proposals (B b) proposer [protocol] in
  let* b = Block.bake ~operation b in
  (* skip to exploration period *)
  let* b = bake_until_first_block_of_next_period b in
  (* we moved to an exploration period with one proposal *)
  let* () = assert_period ~expected_kind:Exploration b __LOC__ in
  (* take percentage of the delegates equal or greater than quorum_max *)
  let minimum_to_pass =
    Float.of_int (List.length delegates)
    *. Int32.(to_float quorum_max)
    /. 100_00.
    |> Float.ceil |> Float.to_int
  in
  let voters = List.take_n minimum_to_pass delegates in
  (* all voters vote for yays; no nays, so supermajority is satisfied *)
  let* operations =
    List.map_es (fun del -> Op.ballot (B b) del protocol Vote.Yay) voters
  in
  let* b = Block.bake ~operations b in
  (* skip to next period *)
  let* b = bake_until_first_block_of_next_period b in
  (* expect to move to cooldown because we have supermajority and enough quorum *)
  assert_period ~expected_kind:Cooldown b __LOC__

(** Vote to pass with minimum possible participation_ema (0%), it is
    sufficient for the vote quorum to be equal or greater than the
    minimum quorum cap. *)
let test_quorum_capped_minimum num_delegates () =
  let open Lwt_result_syntax in
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  let* b, delegates = context_init ~min_proposal_quorum num_delegates () in
  (* set the participation EMA to 0% *)
  let*! b = Context.Vote.set_participation_ema b 0l in
  let* {parametric = {quorum_min; _}; _} = Context.get_constants (B b) in
  (* proposal period *)
  let open Alpha_context in
  let* () = assert_period ~expected_kind:Proposal b __LOC__ in
  (* propose a new protocol *)
  let protocol = Protocol_hash.zero in
  let proposer =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0
  in
  let* operation = Op.proposals (B b) proposer [protocol] in
  let* b = Block.bake ~operation b in
  (* skip to exploration period *)
  let* b = bake_until_first_block_of_next_period b in
  (* we moved to an exploration period with one proposal *)
  let* () = assert_period ~expected_kind:Exploration b __LOC__ in
  (* take percentage of the delegates equal or greater than quorum_min *)
  let minimum_to_pass =
    Float.of_int (List.length delegates)
    *. Int32.(to_float quorum_min)
    /. 100_00.
    |> Float.ceil |> Float.to_int
  in
  let voters = List.take_n minimum_to_pass delegates in
  (* all voters vote for yays; no nays, so supermajority is satisfied *)
  let* operations =
    List.map_es (fun del -> Op.ballot (B b) del protocol Vote.Yay) voters
  in
  let* b = Block.bake ~operations b in
  (* skip to next period *)
  let* b = bake_until_first_block_of_next_period b in
  (* expect to move to cooldown because we have supermajority and enough quorum *)
  assert_period ~expected_kind:Cooldown b __LOC__

(* gets the voting power *)
let get_voting_power block pkhash =
  let ctxt = Context.B block in
  Context.get_voting_power ctxt pkhash

(** Test that the voting power changes if the balance between bakers changes
    and the blockchain moves to the next voting period. It also checks that
    the total voting power coincides with the addition of the voting powers
    of bakers *)
let test_voting_power_updated_each_voting_period () =
  let open Lwt_result_syntax in
  let init_bal1 = 80_000_000_000L in
  let init_bal2 = 48_000_000_000L in
  let init_bal3 = 40_000_000_000L in
  (* Create three accounts with different amounts *)
  let* genesis, contracts =
    context_init ~bootstrap_balances:[init_bal1; init_bal2; init_bal3] 3 ()
  in
  let con1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0 in
  let con2 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 1 in
  let con3 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 2 in
  (* Get the key hashes of the bakers *)
  let baker1 = Context.Contract.pkh con1 in
  let baker2 = Context.Contract.pkh con2 in
  let baker3 = Context.Contract.pkh con3 in
  (* Retrieve balance of con1 *)
  let open Tez_helpers in
  let* balance1 = Context.Contract.balance (B genesis) con1 in
  let* frozen_deposits1 =
    Context.Delegate.current_frozen_deposits (B genesis) baker1
  in
  let*? full_balance1 = balance1 +? frozen_deposits1 in
  let* () = Assert.equal_tez ~loc:__LOC__ full_balance1 (of_mutez init_bal1) in
  (* Retrieve balance of con2 *)
  let* balance2 = Context.Contract.balance (B genesis) con2 in
  let* frozen_deposits2 =
    Context.Delegate.current_frozen_deposits (B genesis) baker2
  in
  let*? full_balance2 = balance2 +? frozen_deposits2 in
  let* () = Assert.equal_tez ~loc:__LOC__ full_balance2 (of_mutez init_bal2) in
  (* Retrieve balance of con3 *)
  let* balance3 = Context.Contract.balance (B genesis) con3 in
  let* frozen_deposits3 =
    Context.Delegate.current_frozen_deposits (B genesis) baker3
  in
  let*? full_balance3 = balance3 +? frozen_deposits3 in
  let* () = Assert.equal_tez ~loc:__LOC__ full_balance3 (of_mutez init_bal3) in
  (* Auxiliary assert_voting_power *)
  let assert_voting_power ~loc n block baker =
    let* voting_power = get_voting_power block baker in
    Assert.equal_int64 ~loc n voting_power
  in
  (* Auxiliary assert_total_voting_power *)
  let assert_total_voting_power ~loc n block =
    let* total_voting_power = Context.get_total_voting_power (B block) in
    Assert.equal_int64 ~loc n total_voting_power
  in
  let expected_power_of_baker_1 = Tez.to_mutez full_balance1 in
  let* () =
    assert_voting_power ~loc:__LOC__ expected_power_of_baker_1 genesis baker1
  in
  let expected_power_of_baker_2 = Tez.to_mutez full_balance2 in
  let* () =
    assert_voting_power ~loc:__LOC__ expected_power_of_baker_2 genesis baker2
  in
  (* Assert total voting power *)
  let expected_power_of_baker_3 = Tez.to_mutez full_balance3 in
  let* () =
    assert_total_voting_power
      ~loc:__LOC__
      Int64.(
        add
          (add expected_power_of_baker_1 expected_power_of_baker_2)
          expected_power_of_baker_3)
      genesis
  in
  (* Create policy that excludes baker1 and baker2 from baking *)
  let policy = Block.Excluding [baker1; baker2] in
  (* Transfer 30,000 tez from baker1 to baker2 *)
  let amount = Tez.of_mutez_exn 30_000_000_000L in
  let* operation = Op.transaction (B genesis) con1 con2 amount in
  (* Bake the block containing the transaction *)
  let* block = Block.bake ~policy ~operation genesis in
  (* Retrieve balance of con1 *)
  let* balance1 = Context.Contract.balance (B block) con1 in
  (* Assert balance has changed by deducing the amount *)
  let*? balance1_after_deducing_amount = of_mutez init_bal1 -? amount in
  let* frozen_deposit1 =
    Context.Delegate.current_frozen_deposits (B block) baker1
  in
  let* () =
    let*? t = balance1_after_deducing_amount -? frozen_deposit1 in
    Assert.equal_tez ~loc:__LOC__ balance1 t
  in
  (* Retrieve balance of con2 *)
  let* balance2 = Context.Contract.balance (B block) con2 in
  (* Assert balance has changed by adding amount *)
  let*? balance2_after_adding_amount = of_mutez init_bal2 +? amount in
  let* frozen_deposit2 =
    Context.Delegate.current_frozen_deposits (B block) baker2
  in
  let* () =
    let*? t = balance2_after_adding_amount -? frozen_deposit2 in
    Assert.equal_tez ~loc:__LOC__ balance2 t
  in
  let* block = Block.bake ~policy block in
  (* Assert voting power (and total) remains the same before next voting period *)
  let* () =
    assert_voting_power ~loc:__LOC__ expected_power_of_baker_1 block baker1
  in
  let* () =
    assert_voting_power ~loc:__LOC__ expected_power_of_baker_2 block baker2
  in
  let* () =
    assert_voting_power ~loc:__LOC__ expected_power_of_baker_3 block baker3
  in
  let* () =
    assert_total_voting_power
      ~loc:__LOC__
      Int64.(
        add
          (add expected_power_of_baker_1 expected_power_of_baker_2)
          expected_power_of_baker_3)
      block
  in
  let* block = bake_until_first_block_of_next_period block in
  (* Assert voting power of baker1 has decreased by [amount] *)
  let expected_power_of_baker_1 =
    Int64.sub expected_power_of_baker_1 (Tez.to_mutez amount)
  in
  let* () =
    assert_voting_power ~loc:__LOC__ expected_power_of_baker_1 block baker1
  in
  (* Assert voting power of baker2 has increased by [amount] *)
  let expected_power_of_baker_2 =
    Int64.add expected_power_of_baker_2 (Tez.to_mutez amount)
  in
  let* () =
    assert_voting_power ~loc:__LOC__ expected_power_of_baker_2 block baker2
  in
  (* Retrieve voting power of baker3 *)
  let* power = get_voting_power block baker3 in
  let power_of_baker_3 = power in
  (* Assert total voting power *)
  assert_total_voting_power
    ~loc:__LOC__
    Int64.(
      add
        (add expected_power_of_baker_1 expected_power_of_baker_2)
        power_of_baker_3)
    block

let test_voting_period_pp () =
  let vp =
    Voting_period_repr.
      {
        index = Int32.of_int 123;
        kind = Proposal;
        start_position = Int32.of_int 321;
      }
  in
  Assert.equal
    ~loc:__LOC__
    ( = )
    "Unexpected pretty printing of voting period"
    Format.pp_print_string
    (Format.asprintf "%a" Voting_period_repr.pp vp)
    "index: 123, kind:proposal, start_position: 321"

(** {2 Validity tests}

    For each vote operation (Proposals and Ballot), we define a serie
    of negative tests and a positive test.

    Negative tests target errors that can occur during
    application. They check that the appropriate error is triggered.

    If the operation is valid, then its application must succeed when
    it is baked into a block. Positive tests observe the effects of the
    operation application by comparing the states before and after the
    block. *)

(** {3 Proposal -- Negative tests} *)

(** Test that a Proposals operation fails when it is unsigned. *)
let test_proposals_missing_signature () =
  let open Lwt_result_syntax in
  let* block, proposer = context_init1 () in
  let* contents = Op.proposals_contents (B block) proposer [protos.(0)] in
  let op = Op.pack_operation (B block) None contents in
  Incremental.assert_validate_operation_fails
    (missing_signature __LOC__)
    op
    block

(** Test that a Proposals operation fails when its signature is invalid. *)
let test_proposals_invalid_signature () =
  let open Lwt_result_syntax in
  let* block, proposer = context_init1 () in
  let* contents = Op.proposals_contents (B block) proposer [protos.(0)] in
  let op = Op.pack_operation (B block) (Some Signature.zero) contents in
  Incremental.assert_validate_operation_fails
    (invalid_signature __LOC__)
    op
    block

(** Test that a Proposals operation fails when the period index
    provided in the operation is not the current voting period index. *)
let test_proposals_wrong_voting_period_index () =
  let open Lwt_result_syntax in
  let* block, proposer = context_init1 () in
  let* current_period = Context.Vote.get_current_period (B block) in
  let current_index = current_period.voting_period.index in
  let op_index = Int32.succ current_index in
  assert_validate_proposals_fails
    ~expected_error:(wrong_voting_period_index ~current_index ~op_index)
    ~proposer
    ~proposals:[Protocol_hash.zero]
    ~period:op_index
    block
    __LOC__

(** Test that a Proposals operation fails when it occurs in a
    non-Proposal voting period. *)
let test_proposals_wrong_voting_period_kind () =
  let open Lwt_result_syntax in
  let* block, proposer = context_init1 () in
  let proposal = protos.(0) in
  let assert_proposals_fails_with_unexpected_proposal =
    assert_validate_proposals_fails
      ~expected_error:wrong_voting_period_kind
      ~proposer
      ~proposals:[proposal]
  in
  (* End the initial Proposals period with a submitted
     proposal, to move on to an Exploration period. *)
  let* operation = Op.proposals (B block) proposer [proposal] in
  let* block = Block.bake block ~operation in
  let* block = bake_until_first_block_of_next_period block in
  (* Proposals during Exploration. *)
  let* () = assert_period ~expected_kind:Exploration block __LOC__ in
  let* () = assert_proposals_fails_with_unexpected_proposal block __LOC__ in
  (* End the Exploration period with enough votes to move on to a
     Cooldown period. *)
  let* operation = Op.ballot (B block) proposer proposal Vote.Yay in
  let* block = Block.bake ~operation block in
  let* block = bake_until_first_block_of_next_period block in
  (* Proposals during Cooldown. *)
  let* () = assert_period ~expected_kind:Cooldown block __LOC__ in
  let* () = assert_proposals_fails_with_unexpected_proposal block __LOC__ in
  (* Proposals during Promotion. *)
  let* block = bake_until_first_block_of_next_period block in
  let* () = assert_period ~expected_kind:Promotion block __LOC__ in
  let* () = assert_proposals_fails_with_unexpected_proposal block __LOC__ in
  (* End the Promotion period with enough votes to move on to an
     Adoption period. *)
  let* operation = Op.ballot (B block) proposer proposal Vote.Yay in
  let* block = Block.bake ~operation block in
  let* block = bake_until_first_block_of_next_period block in
  (* Proposals during Adoption. *)
  let* () = assert_period ~expected_kind:Adoption block __LOC__ in
  assert_proposals_fails_with_unexpected_proposal block __LOC__

(** Test that a Proposals operation fails when the proposer is not in
    the vote listings (with the same error, no matter how far the
    source is from being a delegate with voting rights). *)
let test_proposals_source_not_in_vote_listings () =
  let open Lwt_result_syntax in
  (* The chosen [blocks_per_cycle] is an arbitrary value that we will
     not reach with the blocks baked in this test. *)
  let* block, funder = context_init1 ~blocks_per_cycle:10l () in
  let fresh_account = Account.new_account () in
  let proposer = Contract.Implicit fresh_account.pkh in
  let assert_fails_with_unregistered_delegate block =
    assert_validate_proposals_fails
      ~expected_error:proposals_from_unregistered_delegate
      ~proposer
      ~proposals:[Protocol_hash.zero]
      block
  in
  let assert_fails_with_source_not_in_vote_listings block =
    assert_validate_proposals_fails
      ~expected_error:source_not_in_vote_listings
      ~proposer
      ~proposals:[Protocol_hash.zero]
      block
  in
  (* Fail when the source has no contract in the storage. *)
  let* () = assert_fails_with_unregistered_delegate block __LOC__ in
  let* operation = Op.transaction (B block) funder proposer Tez.one in
  let* block = Block.bake block ~operation in
  (* Fail when the contract's public key is unreavealed. *)
  let* () = assert_fails_with_unregistered_delegate block __LOC__ in
  let* operation = Op.revelation (B block) fresh_account.pk in
  let* block = Block.bake block ~operation in
  (* Fail when the source is not a delegate. *)
  let* () = assert_fails_with_unregistered_delegate block __LOC__ in
  let* operation = Op.delegation (B block) proposer (Some fresh_account.pkh) in
  let* block = Block.bake block ~operation in
  (* Fail when the source is a delegate, but not yet in the vote listings. *)
  assert_fails_with_source_not_in_vote_listings block __LOC__

(** Test that a Proposals operation fails when its proposal list is
    empty. *)
let test_empty_proposals () =
  let open Lwt_result_syntax in
  let* block, proposer = context_init1 () in
  assert_validate_proposals_fails
    ~expected_error:empty_proposals
    ~proposer
    ~proposals:[]
    block
    __LOC__

(** Test that a Proposals operation fails when its proposal list
    contains multiple occurrences of the same proposal. *)
let test_proposals_contain_duplicate () =
  let open Lwt_result_syntax in
  let* block, proposer = context_init1 () in
  assert_validate_proposals_fails
    ~expected_error:(proposals_contain_duplicate protos.(1))
    ~proposer
    ~proposals:[protos.(0); protos.(1); protos.(2); protos.(1); protos.(3)]
    block
    __LOC__

(** Test that a Proposals operation fails when it would make the total
    count of proposals submitted by the proposer exceed the
    [max_proposals_per_delegate] protocol constant. *)
let test_too_many_proposals () =
  let open Lwt_result_syntax in
  let* block, proposer = context_init1 () in
  assert (Array.length protos >= Constants.max_proposals_per_delegate + 1) ;
  let proposals =
    List.map (Array.get protos) (1 -- Constants.max_proposals_per_delegate)
  in
  let* operation = Op.proposals (B block) proposer proposals in
  let* block = Block.bake block ~operation in
  assert_validate_proposals_fails
    ~expected_error:too_many_proposals
    ~proposer
    ~proposals:[protos.(0)]
    block
    __LOC__

(** Test that a Proposals operation fails when one of its proposals has
    already been submitted by the same proposer in an earlier block. *)
let test_already_proposed () =
  let open Lwt_result_syntax in
  let* block, proposer = context_init1 () in
  let* operation = Op.proposals (B block) proposer [protos.(0); protos.(1)] in
  let* block = Block.bake block ~operation in
  (* The [proposer] cannot submit protocol [0] again. *)
  let* () =
    assert_validate_proposals_fails
      ~expected_error:(already_proposed protos.(0))
      ~proposer
      ~proposals:[protos.(0)]
      block
      __LOC__
  in
  (* The [proposer] cannot submit protocol [1] again, even among other
     new proposals. *)
  let* () =
    assert_validate_proposals_fails
      ~expected_error:(already_proposed protos.(1))
      ~proposer
      ~proposals:[protos.(2); protos.(1); protos.(3)]
      block
      __LOC__
  in
  (* The initial [operation] cannot be replayed. *)
  let* () =
    Incremental.assert_validate_operation_fails
      (already_proposed protos.(0) __LOC__)
      operation
      block
  in
  let* block = bake_until_first_block_of_next_period block in
  Incremental.assert_validate_operation_fails
    (wrong_voting_period_index ~current_index:1l ~op_index:0l __LOC__)
    operation
    block

(** Test that a Proposals operation fails when it would make the total
    count of proposals submitted by the proposer exceed the
    [max_proposals_per_delegate] protocol constant, because of
    previously validated operations in the current block/mempool. *)
let test_conflict_too_many_proposals () =
  let open Lwt_result_syntax in
  let* block, proposer = context_init1 () in
  let n_proposals_in_previous_blocks = 5 in
  assert (Array.length protos >= Constants.max_proposals_per_delegate + 1) ;
  let proposals_in_previous_blocks =
    List.map (Array.get protos) (1 -- n_proposals_in_previous_blocks)
  in
  let* operation =
    Op.proposals (B block) proposer proposals_in_previous_blocks
  in
  let* block = Block.bake block ~operation in
  let* current_block_state = Incremental.begin_construction block in
  let proposals_in_current_block =
    List.map
      (Array.get protos)
      (n_proposals_in_previous_blocks + 1
     -- Constants.max_proposals_per_delegate)
  in
  let* op_in_current_block =
    Op.proposals (B block) proposer proposals_in_current_block
  in
  let* current_block_state =
    Incremental.validate_operation current_block_state op_in_current_block
  in
  let* op = Op.proposals (B block) proposer [protos.(0)] in
  let* (_i : Incremental.t) =
    Incremental.validate_operation
      ~expect_failure:(conflicting_proposals __LOC__)
      current_block_state
      op
  in
  return_unit

(** Test that a Proposals operation fails when its source has already
    submitted a Proposals operation in the current block/mempool. *)
let test_conflicting_proposal () =
  let open Lwt_result_syntax in
  let* block, proposer = context_init1 () in
  let proposal = protos.(0) in
  let* current_block_state = Incremental.begin_construction block in
  let* op_in_current_block = Op.proposals (B block) proposer [proposal] in
  let* current_block_state =
    Incremental.validate_operation current_block_state op_in_current_block
  in
  let* op = Op.proposals (B block) proposer [proposal] in
  let* (_i : Incremental.t) =
    Incremental.validate_operation
      ~expect_failure:(conflicting_proposals __LOC__)
      current_block_state
      op
  in
  let proposal' = protos.(1) in
  let* op' = Op.proposals (B block) proposer [proposal'] in
  let* (_i : Incremental.t) =
    Incremental.validate_operation
      ~expect_failure:(conflicting_proposals __LOC__)
      current_block_state
      op'
  in
  return_unit

(** {3 Proposals -- Positive test}

    A Proposals operation is valid when:

    - its source is a registered delegate and belongs to the voting
      listings,

    - the current voting period is a Proposal period and has the same
      index as the period provided in the operation,

    - its list of proposals is not empty,

    - it won't make the total proposal count of the proposer exceed
      the [max_proposals_per_delegate] protocol constant, and

    - its signature is valid.

    We can observe the successful application of the Proposals
    operation from a pre-state to a post-state as follows:

    - the proposal count of the proposer has been incremented by the
      number of proposals in the operation,

    - the operation proposals have been added to the recorded
      proposals of the proposer, and

    - the total weight supporting each of the proposals has been
      incremented by the voting power of the proposer. *)

let observe_proposals pre_state post_state op caller_loc =
  let open Lwt_result_syntax in
  let make_loc = append_loc ~caller_loc in
  let* (Proposals {source; period; proposals}) =
    let (Operation_data {contents; _}) = op.protocol_data in
    match contents with
    | Single (Proposals _ as contents) -> return contents
    | _ -> failwith "%s - Expected a Proposals operation" (make_loc __LOC__)
  in

  (* Validity conditions *)
  let proposals_num = List.length proposals in
  let* () = Assert.not_equal_int ~loc:(make_loc __LOC__) 0 proposals_num in
  let* () =
    assert_period ~expected_kind:Proposal pre_state (make_loc __LOC__)
  in
  let* pre_period = Context.Vote.get_current_period (B pre_state) in
  let* () =
    Assert.equal_int32
      ~loc:(make_loc __LOC__)
      period
      pre_period.voting_period.index
  in
  let* del =
    Context.Contract.delegate (B pre_state) (Contract.Implicit source)
  in
  let* () = Assert.equal_pkh ~loc:(make_loc __LOC__) source del in
  let* dels, _powers = get_delegates_and_power_from_listings pre_state in
  assert (List.mem ~equal:Contract.equal (Contract.Implicit source) dels) ;
  let* pre_voting_infos = Context.Delegate.voting_info (B pre_state) source in
  let* () =
    Assert.not_equal_int
      ~loc:(make_loc __LOC__)
      0
      pre_voting_infos.remaining_proposals
  in
  let* () =
    Assert.leq_int
      ~loc:(make_loc __LOC__)
      proposals_num
      pre_voting_infos.remaining_proposals
  in

  (* Observations *)
  (* Check [voting_info] update. *)
  let* post_voting_infos = Context.Delegate.voting_info (B post_state) source in
  let* () =
    Assert.equal_int
      ~loc:(make_loc __LOC__)
      post_voting_infos.remaining_proposals
      (pre_voting_infos.remaining_proposals - proposals_num)
  in
  assert (
    List.for_all
      (fun a -> Stdlib.List.mem a post_voting_infos.current_proposals)
      proposals) ;
  (* Check [Storage.Vote.Proposals_count] update. *)
  let* proposal_count_pre =
    Context.Vote.get_delegate_proposal_count (B pre_state) source
  in
  let* proposal_count_post =
    Context.Vote.get_delegate_proposal_count (B post_state) source
  in
  let* () =
    Assert.equal_int
      ~loc:(make_loc __LOC__)
      (proposal_count_pre + proposals_num)
      proposal_count_post
  in
  (* Check the update of the total weight of supporters for each proposal. *)
  let* proposal_weights_pre = Context.Vote.get_proposals (B pre_state) in
  let* proposal_weights_post = Context.Vote.get_proposals (B post_state) in
  let* source_power =
    Assert.get_some ~loc:(make_loc __LOC__) pre_voting_infos.voting_power
  in
  List.iter_es
    (fun proposal ->
      let weight_pre =
        Environment.Protocol_hash.Map.find proposal proposal_weights_pre
        |> Option.value ~default:Int64.zero
      in
      let* weight_post =
        Assert.get_some
          ~loc:(make_loc __LOC__)
          (Environment.Protocol_hash.Map.find proposal proposal_weights_post)
      in
      Assert.equal_int64
        ~loc:(make_loc __LOC__)
        weight_post
        (Int64.add weight_pre source_power))
    proposals

let test_too_many_proposals_in_one_operation () =
  let open Lwt_result_syntax in
  let* b0, proposer0 = context_init1 () in
  let protos = Array.to_list protos in
  Lwt.catch
    (fun () ->
      let* (_ : packed_operation) = Op.proposals (B b0) proposer0 protos in
      failwith
        "Encoding of proposals operation with too many proposals should fail")
    (function
      | Data_encoding.Binary.(Write_error List_invalid_length) -> return_unit
      | exn -> Lwt.reraise exn)

(* Bake blocks with various valid Proposals operations, and observe
   that their effects are correctly applied. *)
let test_valid_proposals () =
  let open Lwt_result_syntax in
  (* We use a higher [blocks_per_cycle] than the
     {!default_blocks_per_cycle} (which is [4l]), so that we can bake
     each operation in a separate block without reaching the end of
     the voting cycle. *)
  let* b0, (proposer0, proposer1) = context_init2 ~blocks_per_cycle:10l () in
  let* op0 = Op.proposals (B b0) proposer0 [protos.(0)] in
  let* b1 = Block.bake b0 ~operation:op0 in
  let* () = observe_proposals b0 b1 op0 __LOC__ in
  let* op1 =
    Op.proposals (B b1) proposer0 [protos.(1); protos.(2); protos.(3)]
  in
  let* b2 = Block.bake b1 ~operation:op1 in
  let* () = observe_proposals b1 b2 op1 __LOC__ in
  let* op2 =
    Op.proposals
      (B b2)
      proposer1
      [protos.(0); protos.(2); protos.(4); protos.(5)]
  in
  let* b3 = Block.bake b2 ~operation:op2 in
  let* () = observe_proposals b2 b3 op2 __LOC__ in
  let* op3 = Op.proposals (B b3) proposer0 [protos.(5); protos.(6)] in
  let* b4 = Block.bake b3 ~operation:op3 in
  observe_proposals b3 b4 op3 __LOC__

(** {3 Ballot -- Negative tests} *)

(** Test that a Ballot operation fails when it is unsigned. *)
let test_ballot_missing_signature () =
  let open Lwt_result_syntax in
  let* block, voter, proposal = context_init_exploration () in
  let* contents = Op.ballot_contents (B block) voter proposal Vote.Yay in
  let op = Op.pack_operation (B block) None contents in
  Incremental.assert_validate_operation_fails
    (missing_signature __LOC__)
    op
    block

(** Test that a Ballot operation fails when its signature is invalid. *)
let test_ballot_invalid_signature () =
  let open Lwt_result_syntax in
  let* block, voter, proposal = context_init_exploration () in
  let* contents = Op.ballot_contents (B block) voter proposal Vote.Yay in
  let op = Op.pack_operation (B block) (Some Signature.zero) contents in
  Incremental.assert_validate_operation_fails
    (invalid_signature __LOC__)
    op
    block

(** Test that a Ballot operation fails when the period index provided
    in the operation is not the current voting period index. *)
let test_ballot_wrong_voting_period_index () =
  let open Lwt_result_syntax in
  let* block, voter = context_init1 () in
  let* current_period = Context.Vote.get_current_period (B block) in
  let current_index = current_period.voting_period.index in
  let op_index = Int32.succ current_index in
  assert_validate_ballot_fails
    ~expected_error:(wrong_voting_period_index ~current_index ~op_index)
    ~voter
    ~proposal:protos.(0)
    ~ballot:Vote.Yay
    ~period:op_index
    block
    __LOC__

(** Test that a Ballot operation fails when it occurs outside of an
    Exploration or Promotion voting period. *)
let test_ballot_wrong_voting_period_kind () =
  let open Lwt_result_syntax in
  let* block, voter = context_init1 () in
  let proposal = protos.(0) in
  let assert_ballot_fails_with_unexpected_ballot =
    assert_validate_ballot_fails
      ~expected_error:wrong_voting_period_kind
      ~voter
      ~proposal
      ~ballot:Vote.Nay
  in
  (* Ballot during Proposals. *)
  let* () = assert_period ~expected_kind:Proposal block __LOC__ in
  let* () = assert_ballot_fails_with_unexpected_ballot block __LOC__ in
  (* End the Proposals period with a submitted proposal, to move on to
     an Exploration period. *)
  let* operation = Op.proposals (B block) voter [proposal] in
  let* block = Block.bake block ~operation in
  let* block = bake_until_first_block_of_next_period block in
  (* End the Exploration period with enough votes to move on to a
     Cooldown period. *)
  let* operation = Op.ballot (B block) voter proposal Vote.Yay in
  let* block = Block.bake block ~operation in
  let* block = bake_until_first_block_of_next_period block in
  (* Ballot during Cooldown. *)
  let* () = assert_period ~expected_kind:Cooldown block __LOC__ in
  let* () = assert_ballot_fails_with_unexpected_ballot block __LOC__ in
  (* End the Cooldown period, then end the Promotion period with
     enough votes to move on to an Adoption period. *)
  let* block = bake_until_first_block_of_next_period block in
  let* operation = Op.ballot (B block) voter proposal Vote.Yay in
  let* block = Block.bake ~operation block in
  let* block = bake_until_first_block_of_next_period block in
  (* Ballot during Adoption. *)
  let* () = assert_period ~expected_kind:Adoption block __LOC__ in
  assert_ballot_fails_with_unexpected_ballot block __LOC__

(** Test that a Ballot operation fails when its proposal is not the
    current proposal. *)
let test_ballot_for_wrong_proposal () =
  let open Lwt_result_syntax in
  let* block, voter, current_proposal =
    context_init_exploration ~proposal:protos.(0) ()
  in
  let op_proposal = protos.(1) in
  assert_validate_ballot_fails
    ~expected_error:(ballot_for_wrong_proposal ~current_proposal ~op_proposal)
    ~voter
    ~proposal:op_proposal
    ~ballot:Vote.Yay
    block
    __LOC__

(** Test that a Ballot operation fails when its source has already
    submitted a Ballot. *)
let test_already_submitted_a_ballot () =
  let open Lwt_result_syntax in
  let* block, voter, proposal = context_init_exploration () in
  let* operation = Op.ballot (B block) voter proposal Vote.Yay in
  let* block = Block.bake ~operation block in
  assert_validate_ballot_fails
    ~expected_error:already_submitted_a_ballot
    ~voter
    ~proposal
    ~ballot:Vote.Nay
    block
    __LOC__

(** Test that a Ballot operation fails when its source is not in the
    vote listings (with the same error, no matter how far the source is
    from being a delegate with voting rights). *)
let test_ballot_source_not_in_vote_listings () =
  let open Lwt_result_syntax in
  let* block, funder, proposal =
    (* The chosen [blocks_per_cycle] is an arbitrary value that we
       will not reach with the blocks baked in this test. *)
    context_init_exploration ~blocks_per_cycle:10l ()
  in
  let fresh_account = Account.new_account () in
  let voter = Contract.Implicit fresh_account.pkh in
  let assert_fails_with_source_not_in_vote_listings block =
    assert_validate_ballot_fails
      ~expected_error:source_not_in_vote_listings
      ~voter
      ~proposal
      ~ballot:Vote.Yay
      block
  in
  let assert_fails_with_unregistered_delegate block =
    assert_validate_ballot_fails
      ~expected_error:ballot_from_unregistered_delegate
      ~voter
      ~proposal
      ~ballot:Vote.Yay
      block
  in
  (* Fail when the source has no contract in the storage. *)
  let* () = assert_fails_with_unregistered_delegate block __LOC__ in
  let* operation = Op.transaction (B block) funder voter Tez.one in
  let* block = Block.bake block ~operation in
  (* Fail when the contract's public key is unreavealed. *)
  let* () = assert_fails_with_unregistered_delegate block __LOC__ in
  let* operation = Op.revelation (B block) fresh_account.pk in
  let* block = Block.bake block ~operation in
  (* Fail when the source is not a delegate. *)
  let* () = assert_fails_with_unregistered_delegate block __LOC__ in
  let* operation = Op.delegation (B block) voter (Some fresh_account.pkh) in
  let* block = Block.bake block ~operation in
  (* Fail when the source is a delegate, but not yet in the vote listings. *)
  assert_fails_with_source_not_in_vote_listings block __LOC__

(** Test that a Ballot operation fails when its source has already
    submitted a Ballot in a previously validated operation of the
    current block. *)
let test_conflicting_ballot () =
  let open Lwt_result_syntax in
  let* block, voter, proposal = context_init_exploration () in
  let* current_block_state = Incremental.begin_construction block in
  let* op_in_current_block = Op.ballot (B block) voter proposal Vote.Yay in
  let* current_block_state =
    Incremental.validate_operation current_block_state op_in_current_block
  in
  let* op = Op.ballot (B block) voter proposal Vote.Nay in
  let* (_i : Incremental.t) =
    Incremental.validate_operation
      ~expect_failure:(conflicting_ballot __LOC__)
      current_block_state
      op
  in
  return_unit

(** {3 Ballot -- Positive test}

    A Ballot operation is valid when:

    - its source is a registered delegate and belongs to the voting
      listings,

    - the current voting period is an Exploration or Promotion period,
      and has the same index as the period provided in the operation,

    - its proposal is the current proposal in the context,

    - the voter had submitted no ballot in the current voting period
      yet, and

    - its signature is valid.

    We can observe the successful application of a Ballot operation by
    checking that:

    - the ballot has been recorded for the voter in the post-state,
      and

    - the score of the ballot's vote (yay/nay/pass) has been
      incremented by the voting power of the source. *)

let observe_ballot pre_state post_state op caller_loc =
  let open Lwt_result_syntax in
  let make_loc = append_loc ~caller_loc in
  let* (Ballot {source; period; proposal; ballot}) =
    let (Operation_data {contents; _}) = op.protocol_data in
    match contents with
    | Single (Ballot _ as contents) -> return contents
    | _ -> failwith "%s - Expected a Ballot operation" (make_loc __LOC__)
  in
  (* Validity conditions *)
  let* () =
    assert_period
      ~expected_kinds:[Exploration; Promotion]
      pre_state
      (make_loc __LOC__)
  in
  let* pre_period = Context.Vote.get_current_period (B pre_state) in
  let* () =
    Assert.equal_int32
      ~loc:(make_loc __LOC__)
      period
      pre_period.voting_period.index
  in
  let* del =
    Context.Contract.delegate (B pre_state) (Contract.Implicit source)
  in
  let* () = Assert.equal_pkh ~loc:(make_loc __LOC__) source del in
  let* dels, _powers = get_delegates_and_power_from_listings pre_state in
  assert (List.mem ~equal:Contract.equal (Contract.Implicit source) dels) ;
  let* pre_voting_infos = Context.Delegate.voting_info (B pre_state) source in
  let* () =
    Assert.is_none
      ~loc:(make_loc __LOC__)
      ~pp:(fun fmt _ -> Format.fprintf fmt "Voter already has a ballot.@.")
      pre_voting_infos.current_ballot
  in
  let* current_proposal = Context.Vote.get_current_proposal (B pre_state) in
  let* current_proposal =
    Assert.get_some ~loc:(make_loc __LOC__) current_proposal
  in
  assert (Protocol_hash.equal proposal current_proposal) ;
  (* Observations *)
  let* post_voting_infos = Context.Delegate.voting_info (B post_state) source in
  let* recorded_ballot =
    Assert.get_some ~loc:(make_loc __LOC__) post_voting_infos.current_ballot
  in
  let* () =
    Assert.equal
      ~loc:(make_loc __LOC__)
      Vote.equal_ballot
      "Wrong ballot recorded"
      Vote.pp_ballot
      ballot
      recorded_ballot
  in
  let* ballots_pre = Context.Vote.get_ballots (B pre_state) in
  let* source_power =
    Assert.get_some ~loc:(make_loc __LOC__) pre_voting_infos.voting_power
  in
  let expected_ballots_post =
    match ballot with
    | Yay -> {ballots_pre with yay = Int64.add ballots_pre.yay source_power}
    | Nay -> {ballots_pre with nay = Int64.add ballots_pre.nay source_power}
    | Pass -> {ballots_pre with pass = Int64.add ballots_pre.pass source_power}
  in
  assert_ballots expected_ballots_post post_state (make_loc __LOC__)

let test_valid_ballot () =
  let open Lwt_result_syntax in
  (* The chosen [blocks_per_cycle] is an arbitrary value that we will
     not reach with the blocks baked in this test. *)
  let* block, delegates = context_init ~blocks_per_cycle:10l 4 () in
  let* proposer, voter1, voter2, voter3 =
    match delegates with
    | [a; b; c; d] -> return (a, b, c, d)
    | _ -> failwith "%s@,[context_init n] should return [n] delegates" __LOC__
  in
  let proposal = protos.(0) in
  let* operation = Op.proposals (B block) proposer [proposal] in
  let* block = Block.bake block ~operation in
  let* b0 = bake_until_first_block_of_next_period block in
  let* operation = Op.ballot (B b0) voter1 proposal Vote.Yay in
  let* b1 = Block.bake b0 ~operation in
  let* () = observe_ballot b0 b1 operation __LOC__ in
  let* operation = Op.ballot (B b1) voter2 proposal Vote.Nay in
  let* b2 = Block.bake b1 ~operation in
  let* () = observe_ballot b1 b2 operation __LOC__ in
  let* operation = Op.ballot (B b2) voter3 proposal Vote.Pass in
  let* b3 = Block.bake b2 ~operation in
  observe_ballot b2 b3 operation __LOC__

let tests =
  [
    (* Scenarized tests *)
    Tztest.tztest "voting successful_vote" `Quick (test_successful_vote 137);
    Tztest.tztest
      "voting cooldown, not enough quorum"
      `Quick
      (test_not_enough_quorum_in_exploration 245);
    Tztest.tztest
      "voting promotion, not enough quorum"
      `Quick
      (test_not_enough_quorum_in_promotion 432);
    Tztest.tztest
      "voting proposal, with supermajority"
      `Quick
      (test_supermajority_in_proposal true);
    Tztest.tztest
      "voting proposal, without supermajority"
      `Quick
      (test_supermajority_in_proposal false);
    Tztest.tztest
      "voting proposal, with quorum"
      `Quick
      (test_quorum_in_proposal true);
    Tztest.tztest
      "voting proposal, without quorum"
      `Quick
      (test_quorum_in_proposal false);
    Tztest.tztest
      "voting cooldown, with supermajority"
      `Quick
      (test_supermajority_in_exploration true);
    Tztest.tztest
      "voting cooldown, without supermajority"
      `Quick
      (test_supermajority_in_exploration false);
    Tztest.tztest
      "voting proposal, no winning proposal"
      `Quick
      (test_no_winning_proposal 400);
    Tztest.tztest
      "voting quorum, quorum capped maximum"
      `Quick
      (test_quorum_capped_maximum 400);
    Tztest.tztest
      "voting quorum, quorum capped minimum"
      `Quick
      (test_quorum_capped_minimum 401);
    Tztest.tztest
      "voting power updated in each voting period"
      `Quick
      test_voting_power_updated_each_voting_period;
    Tztest.tztest "voting period pretty print" `Quick test_voting_period_pp;
    (* Validity tests on Proposals *)
    Tztest.tztest
      "Proposals missing signature"
      `Quick
      test_proposals_missing_signature;
    Tztest.tztest
      "Proposals invalid signature"
      `Quick
      test_proposals_invalid_signature;
    Tztest.tztest
      "Proposals wrong voting period index"
      `Quick
      test_proposals_wrong_voting_period_index;
    Tztest.tztest
      "Proposals wrong voting period kind"
      `Quick
      test_proposals_wrong_voting_period_kind;
    Tztest.tztest
      "Proposals source not in vote listings"
      `Quick
      test_proposals_source_not_in_vote_listings;
    Tztest.tztest "Empty proposals" `Quick test_empty_proposals;
    Tztest.tztest
      "Proposals contain a duplicate proposal"
      `Quick
      test_proposals_contain_duplicate;
    Tztest.tztest
      "Too many proposals (over one operation)"
      `Quick
      test_too_many_proposals_in_one_operation;
    Tztest.tztest
      "Too many proposals (over two operations)"
      `Quick
      test_too_many_proposals;
    Tztest.tztest
      "A proposal had already been proposed"
      `Quick
      test_already_proposed;
    Tztest.tztest
      "Conflict: too many proposals in current block/mempool"
      `Quick
      test_conflict_too_many_proposals;
    Tztest.tztest
      "Conflicting proposals in current block/mempool"
      `Quick
      test_conflicting_proposal;
    Tztest.tztest "Valid Proposals operations" `Quick test_valid_proposals;
    (* Validity tests on Ballot *)
    Tztest.tztest
      "Ballot missing signature"
      `Quick
      test_ballot_missing_signature;
    Tztest.tztest
      "Ballot invalid signature"
      `Quick
      test_ballot_invalid_signature;
    Tztest.tztest
      "Ballot wrong voting period index"
      `Quick
      test_ballot_wrong_voting_period_index;
    Tztest.tztest
      "Ballot wrong voting period kind"
      `Quick
      test_ballot_wrong_voting_period_kind;
    Tztest.tztest
      "Ballot for wrong proposal"
      `Quick
      test_ballot_for_wrong_proposal;
    Tztest.tztest
      "Delegate has already submitted a ballot"
      `Quick
      test_already_submitted_a_ballot;
    Tztest.tztest
      "Ballot source not in vote listings"
      `Quick
      test_ballot_source_not_in_vote_listings;
    Tztest.tztest
      "Conflicting ballot in current block/mempool"
      `Quick
      test_conflicting_ballot;
    Tztest.tztest "Valid Ballot operations" `Quick test_valid_ballot;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("voting", tests)] |> Lwt_main.run
