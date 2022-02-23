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

(** Testing
    -------
    Component:  Protocol (voting)
    Invocation: dune exec src/proto_011_PtHangz2/lib_protocol/test/main.exe -- test "^voting$"
    Subject:    On the voting process.
*)

open Protocol

(* missing stuff in Alpha_context.Vote *)
let ballots_zero = Alpha_context.Vote.{yay = 0l; nay = 0l; pass = 0l}

let ballots_equal b1 b2 =
  Alpha_context.Vote.(b1.yay = b2.yay && b1.nay = b2.nay && b1.pass = b2.pass)

let ballots_pp ppf v =
  Alpha_context.Vote.(
    Format.fprintf ppf "{ yay = %ld ; nay = %ld ; pass = %ld" v.yay v.nay v.pass)

(* constants and ratios used in voting:
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

(** helper functions *)

let assert_period_kind expected_kind kind loc =
  if Stdlib.(expected_kind = kind) then return_unit
  else
    Alcotest.failf
      "%s - Unexpected voting period kind - expected %a, got %a"
      loc
      Alpha_context.Voting_period.pp_kind
      expected_kind
      Alpha_context.Voting_period.pp_kind
      kind

let assert_period_index expected_index index loc =
  if expected_index = index then return_unit
  else
    Alcotest.failf
      "%s - Unexpected voting period index - expected %ld, got %ld"
      loc
      expected_index
      index

let assert_period_position expected_position position loc =
  if position = expected_position then return_unit
  else
    Alcotest.failf
      "%s - Unexpected voting period position blocks - expected %ld, got %ld"
      loc
      expected_position
      position

let assert_period_remaining expected_remaining remaining loc =
  if remaining = expected_remaining then return_unit
  else
    Alcotest.failf
      "%s - Unexpected voting period remaining blocks - expected %ld, got %ld"
      loc
      expected_remaining
      remaining

let assert_period ?expected_kind ?expected_index ?expected_position
    ?expected_remaining b loc =
  Context.Vote.get_current_period (B b)
  >>=? fun {voting_period; position; remaining} ->
  (if Option.is_some expected_kind then
   assert_period_kind
     (WithExceptions.Option.get ~loc:__LOC__ expected_kind)
     voting_period.kind
     loc
  else return_unit)
  >>=? fun () ->
  (if Option.is_some expected_index then
   assert_period_index
     (WithExceptions.Option.get ~loc:__LOC__ expected_index)
     voting_period.index
     loc
  else return_unit)
  >>=? fun () ->
  (if Option.is_some expected_position then
   assert_period_position
     (WithExceptions.Option.get ~loc:__LOC__ expected_position)
     position
     loc
  else return_unit)
  >>=? fun () ->
  if Option.is_some expected_remaining then
    assert_period_remaining
      (WithExceptions.Option.get ~loc:__LOC__ expected_remaining)
      remaining
      loc
  else return_unit

let mk_contracts_from_pkh pkh_list =
  List.map Alpha_context.Contract.implicit_contract pkh_list

(* get the list of delegates and the list of their rolls from listings *)
let get_delegates_and_rolls_from_listings b =
  Context.Vote.get_listings (B b) >|=? fun l ->
  (mk_contracts_from_pkh (List.map fst l), List.map snd l)

(* compute the rolls of each delegate *)
let get_rolls b delegates loc =
  Context.Vote.get_listings (B b) >>=? fun l ->
  List.map_es
    (fun delegate ->
      Context.Contract.pkh delegate >>=? fun pkh ->
      match List.find_opt (fun (del, _) -> del = pkh) l with
      | None -> failwith "%s - Missing delegate" loc
      | Some (_, rolls) -> return rolls)
    delegates

(* Checks that the listings are populated *)
let assert_listings_not_empty b ~loc =
  Context.Vote.get_listings (B b) >>=? function
  | [] -> failwith "Unexpected empty listings (%s)" loc
  | _ -> return_unit

let bake_until_first_block_of_next_period b =
  Context.Vote.get_current_period (B b) >>=? fun {remaining; _} ->
  Block.bake_n Int32.(add remaining one |> to_int) b

(** A normal and successful vote sequence. *)
let test_successful_vote num_delegates () =
  let open Alpha_context in
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  Context.init ~min_proposal_quorum num_delegates >>=? fun (b, _) ->
  (* no ballots in proposal period *)
  Context.Vote.get_ballots (B b) >>=? fun v ->
  Assert.equal
    ~loc:__LOC__
    ballots_equal
    "Unexpected ballots"
    ballots_pp
    v
    ballots_zero
  >>=? fun () ->
  (* no ballots in proposal period *)
  (Context.Vote.get_ballot_list (B b) >>=? function
   | [] -> return_unit
   | _ -> failwith "%s - Unexpected ballot list" __LOC__)
  >>=? fun () ->
  (* Last baked block is first block of period Proposal *)
  assert_period
    ~expected_kind:Proposal
    ~expected_index:0l
    ~expected_position:0l
    b
    __LOC__
  >>=? fun () ->
  assert_listings_not_empty b ~loc:__LOC__ >>=? fun () ->
  (* participation EMA starts at initial_participation *)
  Context.Vote.get_participation_ema b >>=? fun v ->
  Assert.equal_int ~loc:__LOC__ initial_participation (Int32.to_int v)
  >>=? fun () ->
  (* listings must be populated in proposal period *)
  assert_listings_not_empty b ~loc:__LOC__ >>=? fun () ->
  (* beginning of proposal, denoted by _p1;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p1, rolls_p1) ->
  (* no proposals at the beginning of proposal period *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->
  (if Environment.Protocol_hash.Map.is_empty ps then return_unit
  else failwith "%s - Unexpected proposals" __LOC__)
  >>=? fun () ->
  (* no current proposal during proposal period *)
  (Context.Vote.get_current_proposal (B b) >>=? function
   | None -> return_unit
   | Some _ -> failwith "%s - Unexpected proposal" __LOC__)
  >>=? fun () ->
  let del1 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates_p1 0
  in
  let del2 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates_p1 1
  in
  let props =
    List.map (fun i -> protos.(i)) (2 -- Constants.max_proposals_per_delegate)
  in
  Op.proposals (B b) del1 (Protocol_hash.zero :: props) >>=? fun ops1 ->
  Op.proposals (B b) del2 [Protocol_hash.zero] >>=? fun ops2 ->
  Block.bake ~operations:[ops1; ops2] b >>=? fun b ->
  (* proposals are now populated *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->
  (* correctly count the double proposal for zero *)
  (let weight =
     Int32.add
       (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth rolls_p1 0)
       (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth rolls_p1 1)
   in
   match Environment.Protocol_hash.(Map.find zero ps) with
   | Some v ->
       if v = weight then return_unit
       else failwith "%s - Wrong count %ld is not %ld" __LOC__ v weight
   | None -> failwith "%s - Missing proposal" __LOC__)
  >>=? fun () ->
  (* proposing more than maximum_proposals fails *)
  Op.proposals (B b) del1 (Protocol_hash.zero :: props) >>=? fun ops ->
  Block.bake ~operations:[ops] b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Amendment.Too_many_proposals -> true
      | _ -> false)
  >>=? fun () ->
  (* proposing less than one proposal fails *)
  Op.proposals (B b) del1 [] >>=? fun ops ->
  Block.bake ~operations:[ops] b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Amendment.Empty_proposal -> true
      | _ -> false)
  >>=? fun () ->
  (* first block of exploration period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* next block is first block of exploration *)
  assert_period ~expected_kind:Exploration ~expected_index:1l b __LOC__
  >>=? fun () ->
  assert_listings_not_empty b ~loc:__LOC__ >>=? fun () ->
  (* listings must be populated in proposal period before moving to exploration period *)
  assert_listings_not_empty b ~loc:__LOC__ >>=? fun () ->
  (* beginning of exploration period, denoted by _p2;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p2, rolls_p2) ->
  (* no proposals during exploration period *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->
  (if Environment.Protocol_hash.Map.is_empty ps then return_unit
  else failwith "%s - Unexpected proposals" __LOC__)
  >>=? fun () ->
  (* current proposal must be set during exploration period *)
  (Context.Vote.get_current_proposal (B b) >>=? function
   | Some v ->
       if Protocol_hash.(equal zero v) then return_unit
       else failwith "%s - Wrong proposal" __LOC__
   | None -> failwith "%s - Missing proposal" __LOC__)
  >>=? fun () ->
  (* unanimous vote: all delegates --active when p2 started-- vote *)
  List.map_es
    (fun del -> Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    delegates_p2
  >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->
  Op.ballot (B b) del1 Protocol_hash.zero Vote.Nay >>=? fun op ->
  Block.bake ~operations:[op] b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Amendment.Unauthorized_ballot -> true
      | _ -> false)
  >>=? fun () ->
  (* Allocate votes from weight (rolls) of active delegates *)
  List.fold_left (fun acc v -> Int32.(add v acc)) 0l rolls_p2
  |> fun rolls_sum ->
  (* # of Yay rolls in ballots matches votes of the delegates *)
  Context.Vote.get_ballots (B b) >>=? fun v ->
  Assert.equal
    ~loc:__LOC__
    ballots_equal
    "Unexpected ballots"
    ballots_pp
    v
    Vote.{yay = rolls_sum; nay = 0l; pass = 0l}
  >>=? fun () ->
  (* One Yay ballot per delegate *)
  (Context.Vote.get_ballot_list (B b) >>=? function
   | [] -> failwith "%s - Unexpected empty ballot list" __LOC__
   | l ->
       List.iter_es
         (fun delegate ->
           Context.Contract.pkh delegate >>=? fun pkh ->
           match List.find_opt (fun (del, _) -> del = pkh) l with
           | None -> failwith "%s - Missing delegate" __LOC__
           | Some (_, Vote.Yay) -> return_unit
           | Some _ -> failwith "%s - Wrong ballot" __LOC__)
         delegates_p2)
  >>=? fun () ->
  (* skip to cooldown period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  assert_period ~expected_index:2l ~expected_kind:Cooldown b __LOC__
  >>=? fun () ->
  (* no ballots in cooldown period *)
  Context.Vote.get_ballots (B b) >>=? fun v ->
  Assert.equal
    ~loc:__LOC__
    ballots_equal
    "Unexpected ballots"
    ballots_pp
    v
    ballots_zero
  >>=? fun () ->
  (* listings must be populated in cooldown period before moving to promotion_vote period *)
  assert_listings_not_empty b ~loc:__LOC__ >>=? fun () ->
  (* skip to promotion period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  assert_period ~expected_kind:Promotion ~expected_index:3l b __LOC__
  >>=? fun () ->
  assert_listings_not_empty b ~loc:__LOC__ >>=? fun () ->
  (* period 3 *)
  (* listings must be populated in promotion period *)
  assert_listings_not_empty b ~loc:__LOC__ >>=? fun () ->
  (* beginning of promotion period, denoted by _p4;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p4, rolls_p4) ->
  (* no proposals during promotion period *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->
  (if Environment.Protocol_hash.Map.is_empty ps then return_unit
  else failwith "%s - Unexpected proposals" __LOC__)
  >>=? fun () ->
  (* current proposal must be set during promotion period *)
  (Context.Vote.get_current_proposal (B b) >>=? function
   | Some v ->
       if Protocol_hash.(equal zero v) then return_unit
       else failwith "%s - Wrong proposal" __LOC__
   | None -> failwith "%s - Missing proposal" __LOC__)
  >>=? fun () ->
  (* unanimous vote: all delegates --active when p4 started-- vote *)
  List.map_es
    (fun del -> Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    delegates_p4
  >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->
  List.fold_left (fun acc v -> Int32.(add v acc)) 0l rolls_p4
  |> fun rolls_sum ->
  (* # of Yays in ballots matches rolls of the delegate *)
  Context.Vote.get_ballots (B b) >>=? fun v ->
  Assert.equal
    ~loc:__LOC__
    ballots_equal
    "Unexpected ballots"
    ballots_pp
    v
    Vote.{yay = rolls_sum; nay = 0l; pass = 0l}
  >>=? fun () ->
  (* One Yay ballot per delegate *)
  (Context.Vote.get_ballot_list (B b) >>=? function
   | [] -> failwith "%s - Unexpected empty ballot list" __LOC__
   | l ->
       List.iter_es
         (fun delegate ->
           Context.Contract.pkh delegate >>=? fun pkh ->
           match List.find_opt (fun (del, _) -> del = pkh) l with
           | None -> failwith "%s - Missing delegate" __LOC__
           | Some (_, Vote.Yay) -> return_unit
           | Some _ -> failwith "%s - Wrong ballot" __LOC__)
         delegates_p4)
  >>=? fun () ->
  (* skip to end of promotion period and activation*)
  bake_until_first_block_of_next_period b >>=? fun b ->
  assert_period ~expected_kind:Adoption ~expected_index:4l b __LOC__
  >>=? fun () ->
  (* skip to end of Adoption period and bake 1 more to activate *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  assert_period ~expected_kind:Proposal ~expected_index:5l b __LOC__
  >>=? fun () ->
  assert_listings_not_empty b ~loc:__LOC__ >>=? fun () ->
  (* zero is the new protocol (before the vote this value is unset) *)
  Context.Vote.get_protocol b >>= fun p ->
  Assert.equal
    ~loc:__LOC__
    Protocol_hash.equal
    "Unexpected proposal"
    Protocol_hash.pp
    p
    Protocol_hash.zero
  >>=? fun () -> return_unit

(* given a list of active delegates,
   return the first k active delegates with which one can have quorum, that is:
   their roll sum divided by the total roll sum is bigger than pr_ema_weight/den *)
let get_smallest_prefix_voters_for_quorum active_delegates active_rolls
    participation_ema =
  let expected_quorum = expected_qr_num participation_ema in
  List.fold_left (fun acc v -> Int32.(add v acc)) 0l active_rolls
  |> fun active_rolls_sum ->
  let rec loop delegates rolls sum selected =
    match (delegates, rolls) with
    | ([], []) -> selected
    | (del :: delegates, del_rolls :: rolls) ->
        if
          den * sum
          < Float.to_int (expected_quorum *. Int32.to_float active_rolls_sum)
        then
          loop delegates rolls (sum + Int32.to_int del_rolls) (del :: selected)
        else selected
    | (_, _) -> []
  in
  loop active_delegates active_rolls 0 []

let get_expected_participation_ema rolls voter_rolls old_participation_ema =
  (* formula to compute the updated participation_ema *)
  let get_updated_participation_ema old_participation_ema participation =
    ((pr_ema_weight * Int32.to_int old_participation_ema)
    + (pr_num * participation))
    / den
  in
  List.fold_left (fun acc v -> Int32.(add v acc)) 0l rolls |> fun rolls_sum ->
  List.fold_left (fun acc v -> Int32.(add v acc)) 0l voter_rolls
  |> fun voter_rolls_sum ->
  let participation =
    Int32.to_int voter_rolls_sum * percent_mul / Int32.to_int rolls_sum
  in
  get_updated_participation_ema old_participation_ema participation

(** If not enough quorum
    -- get_updated_participation_ema < pr_ema_weight/den --
    in exploration, go back to proposal period. *)
let test_not_enough_quorum_in_exploration num_delegates () =
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  Context.init ~min_proposal_quorum num_delegates >>=? fun (b, delegates) ->
  (* proposal period *)
  let open Alpha_context in
  assert_period ~expected_kind:Proposal b __LOC__ >>=? fun () ->
  let proposer =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0
  in
  Op.proposals (B b) proposer [Protocol_hash.zero] >>=? fun ops ->
  Block.bake ~operations:[ops] b >>=? fun b ->
  (* skip to exploration period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* we moved to an exploration period with one proposal *)
  assert_period ~expected_kind:Exploration b __LOC__ >>=? fun () ->
  Context.Vote.get_participation_ema b >>=? fun initial_participation_ema ->
  (* beginning of exploration period, denoted by _p2;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p2, rolls_p2) ->
  Context.Vote.get_participation_ema b >>=? fun participation_ema ->
  get_smallest_prefix_voters_for_quorum delegates_p2 rolls_p2 participation_ema
  |> fun voters ->
  (* take the first two voters out so there cannot be quorum *)
  let voters_without_quorum =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.tl voters
  in
  get_rolls b voters_without_quorum __LOC__
  >>=? fun voters_rolls_in_exploration ->
  (* all voters_without_quorum vote, for yays;
     no nays, so supermajority is satisfied *)
  List.map_es
    (fun del -> Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    voters_without_quorum
  >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->
  (* bake to next period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* we move back to the proposal period because not enough quorum *)
  assert_period ~expected_kind:Proposal b __LOC__ >>=? fun () ->
  (* check participation_ema update *)
  get_expected_participation_ema
    rolls_p2
    voters_rolls_in_exploration
    initial_participation_ema
  |> fun expected_participation_ema ->
  Context.Vote.get_participation_ema b >>=? fun new_participation_ema ->
  (* assert the formula to calculate participation_ema is correct *)
  Assert.equal_int
    ~loc:__LOC__
    expected_participation_ema
    (Int32.to_int new_participation_ema)
  >>=? fun () -> return_unit

(** If not enough quorum
   -- get_updated_participation_ema < pr_ema_weight/den --
   In promotion period, go back to proposal period. *)
let test_not_enough_quorum_in_promotion num_delegates () =
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  Context.init ~min_proposal_quorum num_delegates >>=? fun (b, delegates) ->
  assert_period ~expected_kind:Proposal b __LOC__ >>=? fun () ->
  let proposer =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0
  in
  Op.proposals (B b) proposer [Protocol_hash.zero] >>=? fun ops ->
  Block.bake ~operations:[ops] b >>=? fun b ->
  (* skip to exploration period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* we moved to an exploration period with one proposal *)
  assert_period ~expected_kind:Exploration b __LOC__ >>=? fun () ->
  (* beginning of exploration period, denoted by _p2;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p2, rolls_p2) ->
  Context.Vote.get_participation_ema b >>=? fun participation_ema ->
  get_smallest_prefix_voters_for_quorum delegates_p2 rolls_p2 participation_ema
  |> fun voters ->
  let open Alpha_context in
  (* all voters vote, for yays;
       no nays, so supermajority is satisfied *)
  List.map_es
    (fun del -> Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    voters
  >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->
  (* skip to first block cooldown period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* we move to cooldown because we have supermajority and enough quorum *)
  assert_period ~expected_kind:Cooldown b __LOC__ >>=? fun () ->
  (* skip to first block of promotion period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  assert_period ~expected_kind:Promotion b __LOC__
  (* bake_until_first_block_of_next_period ~offset:1l b
   * >>=? fun b ->
   * assert_period ~expected_kind:Promotion b __LOC__ *)
  >>=?
  fun () ->
  Context.Vote.get_participation_ema b >>=? fun initial_participation_ema ->
  (* beginning of promotion period, denoted by _p4;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p4, rolls_p4) ->
  Context.Vote.get_participation_ema b >>=? fun participation_ema ->
  get_smallest_prefix_voters_for_quorum delegates_p4 rolls_p4 participation_ema
  |> fun voters ->
  (* take the first voter out so there cannot be quorum *)
  let voters_without_quorum =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.tl voters
  in
  get_rolls b voters_without_quorum __LOC__ >>=? fun voter_rolls ->
  (* all voters_without_quorum vote, for yays;
     no nays, so supermajority is satisfied *)
  List.map_es
    (fun del -> Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    voters_without_quorum
  >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->
  (* skip to end of promotion period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  get_expected_participation_ema rolls_p4 voter_rolls initial_participation_ema
  |> fun expected_participation_ema ->
  Context.Vote.get_participation_ema b >>=? fun new_participation_ema ->
  (* assert the formula to calculate participation_ema is correct *)
  Assert.equal_int
    ~loc:__LOC__
    expected_participation_ema
    (Int32.to_int new_participation_ema)
  >>=? fun () ->
  (* we move back to the proposal period because not enough quorum *)
  assert_period ~expected_kind:Proposal b __LOC__ >>=? fun () ->
  assert_listings_not_empty b ~loc:__LOC__ >>=? fun () -> return_unit

(** Identical proposals (identified by their hash) must be counted as
    one. *)
let test_multiple_identical_proposals_count_as_one () =
  Context.init 1 >>=? fun (b, delegates) ->
  assert_period ~expected_kind:Proposal b __LOC__ >>=? fun () ->
  let proposer = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd delegates in
  Op.proposals (B b) proposer [Protocol_hash.zero; Protocol_hash.zero]
  >>=? fun ops ->
  Block.bake ~operations:[ops] b >>=? fun b ->
  (* compute the weight of proposals *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->
  (* compute the rolls of proposer *)
  Context.Contract.pkh proposer >>=? fun pkh ->
  Context.Vote.get_listings (B b) >>=? fun l ->
  (match List.find_opt (fun (del, _) -> del = pkh) l with
  | None -> failwith "%s - Missing delegate" __LOC__
  | Some (_, proposer_rolls) -> return proposer_rolls)
  >>=? fun proposer_rolls ->
  (* correctly count the double proposal for zero as one proposal *)
  let expected_weight_proposer = proposer_rolls in
  match Environment.Protocol_hash.(Map.find zero ps) with
  | Some v ->
      if v = expected_weight_proposer then return_unit
      else
        failwith
          "%s - Wrong count %ld is not %ld; identical proposals count as one"
          __LOC__
          v
          expected_weight_proposer
  | None -> failwith "%s - Missing proposal" __LOC__

(** Assume the initial balance of allocated by Context.init is at
    least 4 times the value of the tokens_per_roll constant. *)
let test_supermajority_in_proposal there_is_a_winner () =
  let min_proposal_quorum = 0l in
  Context.init ~min_proposal_quorum ~initial_balances:[1L; 1L; 1L] 10
  >>=? fun (b, delegates) ->
  Context.get_constants (B b)
  >>=? fun {
             parametric =
               {blocks_per_cycle; tokens_per_roll; blocks_per_voting_period; _};
             _;
           } ->
  let del1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0 in
  let del2 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 1 in
  let del3 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 2 in
  List.map_es (fun del -> Context.Contract.pkh del) [del1; del2; del3]
  >>=? fun pkhs ->
  let policy = Block.Excluding pkhs in
  Op.transaction
    (B b)
    (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 3)
    del1
    tokens_per_roll
  >>=? fun op1 ->
  Op.transaction
    (B b)
    (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 4)
    del2
    tokens_per_roll
  >>=? fun op2 ->
  (if there_is_a_winner then Test_tez.Tez.( *? ) tokens_per_roll 3L
  else Test_tez.Tez.( *? ) tokens_per_roll 2L)
  >>?= fun bal3 ->
  Op.transaction
    (B b)
    (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 5)
    del3
    bal3
  >>=? fun op3 ->
  Block.bake ~policy ~operations:[op1; op2; op3] b >>=? fun b ->
  (* we let one voting period pass; we make sure that:
     - the three selected delegates remain active by re-registering as delegates
     - their number of rolls do not change *)
  List.fold_left_es
    (fun b _ ->
      List.map_es
        (fun del ->
          Context.Contract.pkh del >>=? fun pkh ->
          Op.delegation (B b) del (Some pkh))
        delegates
      >>=? fun ops ->
      Block.bake ~policy ~operations:ops b >>=? fun b ->
      Block.bake_until_cycle_end ~policy b)
    b
    (1 -- Int32.to_int (Int32.div blocks_per_voting_period blocks_per_cycle))
  >>=? fun b ->
  (* make the proposals *)
  Op.proposals (B b) del1 [protos.(0)] >>=? fun ops1 ->
  Op.proposals (B b) del2 [protos.(0)] >>=? fun ops2 ->
  Op.proposals (B b) del3 [protos.(1)] >>=? fun ops3 ->
  Block.bake ~policy ~operations:[ops1; ops2; ops3] b >>=? fun b ->
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* we remain in the proposal period when there is no winner,
     otherwise we move to the exploration period *)
  (if there_is_a_winner then assert_period ~expected_kind:Exploration b __LOC__
  else assert_period ~expected_kind:Proposal b __LOC__)
  >>=? fun () -> return_unit

(** After one voting period, if [has_quorum] then the period kind must
    have been the cooldown vote. Otherwise, it should have remained in
    place in the proposal period. *)
let test_quorum_in_proposal has_quorum () =
  let total_tokens = 32_000_000_000_000L in
  let half_tokens = Int64.div total_tokens 2L in
  Context.init ~initial_balances:[1L; half_tokens; half_tokens] 3
  >>=? fun (b, delegates) ->
  Context.get_constants (B b)
  >>=? fun {
             parametric =
               {
                 blocks_per_cycle;
                 min_proposal_quorum;
                 blocks_per_voting_period;
                 _;
               };
             _;
           } ->
  let del1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0 in
  let del2 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 1 in
  List.map_es (fun del -> Context.Contract.pkh del) [del1; del2]
  >>=? fun pkhs ->
  let policy = Block.Excluding pkhs in
  let quorum =
    if has_quorum then Int64.of_int32 min_proposal_quorum
    else Int64.(sub (of_int32 min_proposal_quorum) 10L)
  in
  let bal =
    Int64.(div (mul total_tokens quorum) 100_00L) |> Test_tez.Tez.of_mutez_exn
  in
  Op.transaction (B b) del2 del1 bal >>=? fun op2 ->
  Block.bake ~policy ~operations:[op2] b >>=? fun b ->
  (* we let one voting period pass; we make sure that:
     - the two selected delegates remain active by re-registering as delegates
     - their number of rolls do not change *)
  List.fold_left_es
    (fun b _ ->
      List.map_es
        (fun del ->
          Context.Contract.pkh del >>=? fun pkh ->
          Op.delegation (B b) del (Some pkh))
        [del1; del2]
      >>=? fun ops ->
      Block.bake ~policy ~operations:ops b >>=? fun b ->
      Block.bake_until_cycle_end ~policy b)
    b
    (1 -- Int32.to_int (Int32.div blocks_per_voting_period blocks_per_cycle))
  >>=? fun b ->
  (* make the proposal *)
  Op.proposals (B b) del1 [protos.(0)] >>=? fun ops ->
  Block.bake ~policy ~operations:[ops] b >>=? fun b ->
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* we remain in the proposal period when there is no quorum,
     otherwise we move to the cooldown vote period *)
  (if has_quorum then assert_period ~expected_kind:Exploration b __LOC__
  else assert_period ~expected_kind:Proposal b __LOC__)
  >>=? fun () -> return_unit

(** If a supermajority is reached, then the voting period must be
    reached. Otherwise, it remains in proposal period. *)
let test_supermajority_in_exploration supermajority () =
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / 100)) in
  Context.init ~min_proposal_quorum 100 >>=? fun (b, delegates) ->
  let del1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0 in
  let proposal = protos.(0) in
  Op.proposals (B b) del1 [proposal] >>=? fun ops1 ->
  Block.bake ~operations:[ops1] b >>=? fun b ->
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* move to exploration *)
  assert_period ~expected_kind:Exploration b __LOC__ >>=? fun () ->
  (* assert our proposal won *)
  (Context.Vote.get_current_proposal (B b) >>=? function
   | Some v ->
       if Protocol_hash.(equal proposal v) then return_unit
       else failwith "%s - Wrong proposal" __LOC__
   | None -> failwith "%s - Missing proposal" __LOC__)
  >>=? fun () ->
  (* beginning of exploration period, denoted by _p2;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p2, _rolls_p2) ->
  (* supermajority means [num_yays / (num_yays + num_nays) >= s_num / s_den],
     which is equivalent with [num_yays >= num_nays * s_num / (s_den - s_num)] *)
  let num_delegates = List.length delegates_p2 in
  let num_nays = num_delegates / 5 in
  (* any smaller number will do as well *)
  let num_yays = num_nays * s_num / (s_den - s_num) in
  (* majority/minority vote depending on the [supermajority] parameter *)
  let num_yays = if supermajority then num_yays else num_yays - 1 in
  let open Alpha_context in
  let (nays_delegates, rest) = List.split_n num_nays delegates_p2 in
  let (yays_delegates, _) = List.split_n num_yays rest in
  List.map_es (fun del -> Op.ballot (B b) del proposal Vote.Yay) yays_delegates
  >>=? fun operations_yays ->
  List.map_es (fun del -> Op.ballot (B b) del proposal Vote.Nay) nays_delegates
  >>=? fun operations_nays ->
  let operations = operations_yays @ operations_nays in
  Block.bake ~operations b >>=? fun b ->
  bake_until_first_block_of_next_period b >>=? fun b ->
  (if supermajority then assert_period ~expected_kind:Cooldown b __LOC__
  else assert_period ~expected_kind:Proposal b __LOC__)
  >>=? fun () -> return_unit

(** Test also how the selection scales: all delegates propose max
    proposals. *)
let test_no_winning_proposal num_delegates () =
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  Context.init ~min_proposal_quorum num_delegates >>=? fun (b, _) ->
  (* beginning of proposal, denoted by _p1;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p1, _rolls_p1) ->
  let open Alpha_context in
  let props =
    List.map (fun i -> protos.(i)) (1 -- Constants.max_proposals_per_delegate)
  in
  (* all delegates active in p1 propose the same proposals *)
  List.map_es (fun del -> Op.proposals (B b) del props) delegates_p1
  >>=? fun ops_list ->
  Block.bake ~operations:ops_list b >>=? fun b ->
  (* skip to exploration period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* we stay in the same proposal period because no winning proposal *)
  assert_period ~expected_kind:Proposal b __LOC__ >>=? fun () -> return_unit

(** Vote to pass with maximum possible participation_ema (100%), it is
    sufficient for the vote quorum to be equal or greater than the
    maximum quorum cap. *)
let test_quorum_capped_maximum num_delegates () =
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  Context.init ~min_proposal_quorum num_delegates >>=? fun (b, delegates) ->
  (* set the participation EMA to 100% *)
  Context.Vote.set_participation_ema b 100_00l >>= fun b ->
  Context.get_constants (B b) >>=? fun {parametric = {quorum_max; _}; _} ->
  (* proposal period *)
  let open Alpha_context in
  assert_period ~expected_kind:Proposal b __LOC__ >>=? fun () ->
  (* propose a new protocol *)
  let protocol = Protocol_hash.zero in
  let proposer =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0
  in
  Op.proposals (B b) proposer [protocol] >>=? fun ops ->
  Block.bake ~operations:[ops] b >>=? fun b ->
  (* skip to exploration period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* we moved to an exploration period with one proposal *)
  assert_period ~expected_kind:Exploration b __LOC__ >>=? fun () ->
  (* take percentage of the delegates equal or greater than quorum_max *)
  let minimum_to_pass =
    Float.of_int (List.length delegates)
    *. Int32.(to_float quorum_max)
    /. 100_00.
    |> Float.ceil |> Float.to_int
  in
  let voters = List.take_n minimum_to_pass delegates in
  (* all voters vote for yays; no nays, so supermajority is satisfied *)
  List.map_es (fun del -> Op.ballot (B b) del protocol Vote.Yay) voters
  >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->
  (* skip to next period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* expect to move to cooldown because we have supermajority and enough quorum *)
  assert_period ~expected_kind:Cooldown b __LOC__

(** Vote to pass with minimum possible participation_ema (0%), it is
    sufficient for the vote quorum to be equal or greater than the
    minimum quorum cap. *)
let test_quorum_capped_minimum num_delegates () =
  let min_proposal_quorum = Int32.(of_int @@ (100_00 / num_delegates)) in
  Context.init ~min_proposal_quorum num_delegates >>=? fun (b, delegates) ->
  (* set the participation EMA to 0% *)
  Context.Vote.set_participation_ema b 0l >>= fun b ->
  Context.get_constants (B b) >>=? fun {parametric = {quorum_min; _}; _} ->
  (* proposal period *)
  let open Alpha_context in
  assert_period ~expected_kind:Proposal b __LOC__ >>=? fun () ->
  (* propose a new protocol *)
  let protocol = Protocol_hash.zero in
  let proposer =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth delegates 0
  in
  Op.proposals (B b) proposer [protocol] >>=? fun ops ->
  Block.bake ~operations:[ops] b >>=? fun b ->
  (* skip to exploration period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
  (* we moved to an exploration period with one proposal *)
  assert_period ~expected_kind:Exploration b __LOC__ >>=? fun () ->
  (* take percentage of the delegates equal or greater than quorum_min *)
  let minimum_to_pass =
    Float.of_int (List.length delegates)
    *. Int32.(to_float quorum_min)
    /. 100_00.
    |> Float.ceil |> Float.to_int
  in
  let voters = List.take_n minimum_to_pass delegates in
  (* all voters vote for yays; no nays, so supermajority is satisfied *)
  List.map_es (fun del -> Op.ballot (B b) del protocol Vote.Yay) voters
  >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->
  (* skip to next period *)
  bake_until_first_block_of_next_period b >>=? fun b ->
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
  let open Test_tez.Tez in
  (* Create three accounts with different amounts *)
  Context.init
    ~initial_balances:[80_000_000_000L; 48_000_000_000L; 4_000_000_000_000L]
    3
  >>=? fun (block, contracts) ->
  let con1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0 in
  let con2 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 1 in
  let con3 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 2 in
  (* Retrieve balance of con1 *)
  Context.Contract.balance (B block) con1 >>=? fun balance1 ->
  Assert.equal_tez ~loc:__LOC__ balance1 (of_mutez_exn 80_000_000_000L)
  >>=? fun _ ->
  (* Retrieve balance of con2 *)
  Context.Contract.balance (B block) con2 >>=? fun balance2 ->
  Assert.equal_tez ~loc:__LOC__ balance2 (of_mutez_exn 48_000_000_000L)
  >>=? fun _ ->
  (* Retrieve balance of con3 *)
  Context.Contract.balance (B block) con3 >>=? fun balance3 ->
  (* Retrieve constants blocks_per_voting_period and tokens_per_roll *)
  Context.get_constants (B block)
  >>=? fun {parametric = {tokens_per_roll; _}; _} ->
  (* Get the key hashes of the bakers *)
  Context.get_bakers (B block) >>=? fun bakers ->
  (* [Context.init] and [Context.get_bakers] store the accounts in reversed orders *)
  let baker1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bakers 2 in
  let baker2 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bakers 1 in
  let baker3 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bakers 0 in
  (* Auxiliary assert_voting_power *)
  let assert_voting_power ~loc n block baker =
    get_voting_power block baker >>=? fun voting_power ->
    Assert.equal_int ~loc n (Int32.to_int voting_power)
  in
  (* Auxiliary assert_total_voting_power *)
  let assert_total_voting_power ~loc n block =
    Context.get_total_voting_power (B block) >>=? fun total_voting_power ->
    Assert.equal_int ~loc n (Int32.to_int total_voting_power)
  in
  (* Assert voting power is equal to the balance divided by tokens_per_roll *)
  let expected_power_of_baker_1 =
    Int64.(to_int (div (to_mutez balance1) (to_mutez tokens_per_roll)))
  in
  assert_voting_power ~loc:__LOC__ expected_power_of_baker_1 block baker1
  >>=? fun _ ->
  (* Assert voting power is equal to the balance divided by tokens_per_roll *)
  let expected_power_of_baker_2 =
    Int64.(to_int (div (to_mutez balance2) (to_mutez tokens_per_roll)))
  in
  assert_voting_power ~loc:__LOC__ expected_power_of_baker_2 block baker2
  >>=? fun _ ->
  (* Assert total voting power *)
  let expected_power_of_baker_3 =
    Int64.(to_int (div (to_mutez balance3) (to_mutez tokens_per_roll)))
  in
  assert_total_voting_power
    ~loc:__LOC__
    Int.(
      add
        (add expected_power_of_baker_1 expected_power_of_baker_2)
        expected_power_of_baker_3)
    block
  >>=? fun _ ->
  (* Create policy that excludes baker1 and baker2 from baking *)
  let policy = Block.Excluding [baker1; baker2] in
  (* Transfer tokens_per_roll * num_rolls from baker1 to baker2 *)
  let num_rolls = 5L in
  tokens_per_roll *? num_rolls >>?= fun amount ->
  Op.transaction (B block) con1 con2 amount >>=? fun op ->
  (* Bake the block containing the transaction *)
  Block.bake ~policy ~operations:[op] block >>=? fun block ->
  (* Retrieve balance of con1 *)
  Context.Contract.balance (B block) con1 >>=? fun balance1 ->
  (* Assert balance has changed by tokens_per_roll * num_rolls *)
  tokens_per_roll *? num_rolls >>?= fun rolls ->
  of_mutez_exn 80_000_000_000L -? rolls
  >>?= Assert.equal_tez ~loc:__LOC__ balance1
  >>=? fun _ ->
  (* Retrieve balance of con2 *)
  Context.Contract.balance (B block) con2 >>=? fun balance2 ->
  (* Assert balance has changed by tokens_per_roll * num_rolls *)
  tokens_per_roll *? num_rolls >>?= fun rolls ->
  of_mutez_exn 48_000_000_000L +? rolls
  >>?= Assert.equal_tez ~loc:__LOC__ balance2
  >>=? fun () ->
  Block.bake block >>=? fun block ->
  (* Assert voting power (and total) remains the same before next voting period *)
  assert_voting_power ~loc:__LOC__ expected_power_of_baker_1 block baker1
  >>=? fun () ->
  assert_voting_power ~loc:__LOC__ expected_power_of_baker_2 block baker2
  >>=? fun () ->
  assert_voting_power ~loc:__LOC__ expected_power_of_baker_3 block baker3
  >>=? fun () ->
  assert_total_voting_power
    ~loc:__LOC__
    Int.(
      add
        (add expected_power_of_baker_1 expected_power_of_baker_2)
        expected_power_of_baker_3)
    block
  >>=? fun _ ->
  bake_until_first_block_of_next_period block >>=? fun block ->
  (* Assert voting power of baker1 has decreased by num_rolls *)
  let expected_power_of_baker_1 =
    Int.sub expected_power_of_baker_1 (Int64.to_int num_rolls)
  in
  assert_voting_power ~loc:__LOC__ expected_power_of_baker_1 block baker1
  >>=? fun _ ->
  (* Assert voting power of baker2 has increased by num_rolls *)
  let expected_power_of_baker_2 =
    Int.add expected_power_of_baker_2 (Int64.to_int num_rolls)
  in
  assert_voting_power ~loc:__LOC__ expected_power_of_baker_2 block baker2
  >>=? fun _ ->
  (* Retrieve voting power of baker3 *)
  get_voting_power block baker3 >>=? fun power ->
  let power_of_baker_3 = Int32.to_int power in
  (* Assert total voting power *)
  assert_total_voting_power
    ~loc:__LOC__
    Int.(
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

let tests =
  [
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
      "voting counting double proposal"
      `Quick
      test_multiple_identical_proposals_count_as_one;
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
  ]
