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
    Component:  Protocol (endorsement)
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_endorsement.ml
    Subject:    Endorsing a block adds an extra layer of confidence
                to the Tezos' PoS algorithm. The block endorsing
                operation must be included in the following block.
*)

open Protocol
open Alpha_context

let init_genesis ?policy () =
  Context.init_n ~consensus_threshold:0 5 () >>=? fun (genesis, _contracts) ->
  Block.bake ?policy genesis >>=? fun b -> return (genesis, b)

(** inject an endorsement and return the block with the endorsement and its
   parent. *)
let inject_the_first_endorsement () =
  init_genesis () >>=? fun (_genesis, b) ->
  Op.endorsement b >>=? fun operation ->
  Block.bake ~operation b >>=? fun b' -> return (b', b)

(****************************************************************)
(*                      Tests                                   *)
(****************************************************************)

(** Apply a single endorsement from the slot 0 endorser. *)
let test_simple_endorsement () =
  inject_the_first_endorsement () >>=? fun (_, _) -> return_unit

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Apply an endorsement with a negative slot. *)
let test_negative_slot () =
  Context.init_n 5 () >>=? fun (genesis, _contracts) ->
  Block.bake genesis >>=? fun b ->
  Context.get_endorser (B b) >>=? fun (delegate, _slots) ->
  Lwt.catch
    (fun () ->
      Op.endorsement
        ~delegate
        ~slot:(Slot.of_int_do_not_use_except_for_parameters (-1))
        b
      >>=? fun (_ : packed_operation) ->
      failwith "negative slot should not be accepted by the binary format")
    (function
      | Data_encoding.Binary.Write_error _ -> return_unit | e -> Lwt.fail e)

(** Apply an endorsement with a non-normalized slot (that is, not the smallest
   possible). *)
let test_non_normalized_slot () =
  Context.init_n 5 () >>=? fun (genesis, _contracts) ->
  Block.bake genesis >>=? fun b ->
  Context.get_endorsers (B b) >>=? fun endorsers_list ->
  (* find an endorsers with more than 1 slot *)
  List.find_map
    (function
      | {Plugin.RPC.Validators.delegate; slots; _} ->
          if Compare.List_length_with.(slots > 1) then Some (delegate, slots)
          else None)
    endorsers_list
  |> function
  | None -> assert false
  | Some (delegate, slots) ->
      let set_slots = Slot.Set.of_list slots in
      (* no duplicated slots *)
      Assert.equal_int
        ~loc:__LOC__
        (Slot.Set.cardinal set_slots)
        (List.length slots)
      >>=? fun () ->
      (* the first slot should be the smallest slot *)
      Assert.equal
        ~loc:__LOC__
        (fun x y -> Slot.compare x y = 0)
        "the first slot is not the smallest"
        Slot.pp
        (WithExceptions.Option.get ~loc:__LOC__ @@ List.hd slots)
        (WithExceptions.Option.get ~loc:__LOC__ @@ Slot.Set.min_elt set_slots)
      >>=? fun () ->
      let slot =
        match List.hd (List.rev slots) with None -> assert false | Some s -> s
      in
      Op.endorsement ~delegate ~slot b >>=? fun operation ->
      let policy = Block.Excluding [delegate] in
      Block.bake ~policy ~operation b >>= fun res ->
      Assert.proto_error ~loc:__LOC__ res (function
          | Validate_errors.Consensus.Wrong_slot_used_for_consensus_operation
              {kind}
            when kind = Validate_errors.Consensus.Endorsement ->
              true
          | _ -> false)

(** Wrong endorsement predecessor : apply an endorsement with an
    incorrect block predecessor. *)
let test_wrong_endorsement_predecessor () =
  init_genesis () >>=? fun (_genesis, b) ->
  Op.endorsement ~pred_branch:(Context.branch (B b)) b >>=? fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Wrong_consensus_operation_branch {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)

(** Invalid_endorsement_level: apply an endorsement with an incorrect
    level (i.e. the predecessor level). *)
let test_invalid_endorsement_level () =
  init_genesis () >>=? fun (genesis, b) ->
  Context.get_level (B genesis) >>?= fun genesis_level ->
  Op.endorsement ~level:genesis_level b >>=? fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)

(** Duplicate endorsement : apply an endorsement that has already been applied. *)
let test_duplicate_endorsement () =
  init_genesis () >>=? fun (_genesis, b) ->
  Incremental.begin_construction b >>=? fun inc ->
  Op.endorsement b >>=? fun operation ->
  Incremental.add_operation inc operation >>=? fun inc ->
  Op.endorsement b >>=? fun operation ->
  Incremental.add_operation inc operation >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Conflicting_consensus_operation {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)

(** Consensus operation for future level : apply an endorsement with a level in the future *)
let test_consensus_operation_endorsement_for_future_level () =
  init_genesis () >>=? fun (_genesis, pred) ->
  let raw_level = Raw_level.of_int32 (Int32.of_int 10) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~is_preendorsement:false
    ~endorsed_block:pred
    ~level
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_future_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    ~construction_mode:(pred, None)
    ()

(** Consensus operation for old level : apply an endorsement one level in the past *)
let test_consensus_operation_endorsement_for_predecessor_level () =
  init_genesis () >>=? fun (_genesis, pred) ->
  let raw_level = Raw_level.of_int32 (Int32.of_int 0) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~is_preendorsement:false
    ~endorsed_block:pred
    ~level
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    ~construction_mode:(pred, None)
    ()

(** Consensus operation for old level : apply an endorsement with more than one level in the past *)
let test_consensus_operation_endorsement_for_old_level () =
  init_genesis () >>=? fun (genesis, pred) ->
  Block.bake genesis >>=? fun _next_block ->
  let raw_level = Raw_level.of_int32 (Int32.of_int 0) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~is_preendorsement:false
    ~endorsed_block:pred
    ~level
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    ~construction_mode:(pred, None)
    ()

(** Consensus operation for future round : apply an endorsement with a round in the future *)
let test_consensus_operation_endorsement_for_future_round () =
  init_genesis () >>=? fun (_genesis, pred) ->
  Environment.wrap_tzresult (Round.of_int 21) >>?= fun round ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~is_preendorsement:false
    ~endorsed_block:pred
    ~round
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_future_round {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    ~construction_mode:(pred, None)
    ()

(** Consensus operation for old round : apply an endorsement with a round in the past *)
let test_consensus_operation_endorsement_for_old_round () =
  init_genesis ~policy:(By_round 10) () >>=? fun (_genesis, pred) ->
  Environment.wrap_tzresult (Round.of_int 0) >>?= fun round ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~is_preendorsement:false
    ~endorsed_block:pred
    ~round
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_old_round {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    ~construction_mode:(pred, None)
    ()

(** Consensus operation on competing proposal : apply an endorsement on a competing proposal *)
let test_consensus_operation_endorsement_on_competing_proposal () =
  init_genesis () >>=? fun (_genesis, pred) ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~is_preendorsement:false
    ~endorsed_block:pred
    ~block_payload_hash:Block_payload_hash.zero
    ~error:(function
      | Validate_errors.Consensus.Wrong_payload_hash_for_consensus_operation
          {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    ~construction_mode:(pred, None)
    ()

(** Wrong round : apply an endorsement with an incorrect round *)
let test_wrong_round () =
  init_genesis () >>=? fun (_genesis, b) ->
  Environment.wrap_tzresult (Round.of_int 2) >>?= fun round ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~is_preendorsement:false
    ~endorsed_block:b
    ~round
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_future_round {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    ()

(** Wrong level : apply an endorsement with an incorrect level *)
let test_wrong_level () =
  init_genesis () >>=? fun (_genesis, b) ->
  (* let context = Context.B genesis in*)
  let raw_level = Raw_level.of_int32 (Int32.of_int 0) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~is_preendorsement:false
    ~endorsed_block:b
    ~level
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    ()

(** Wrong payload hash : apply an endorsement with an incorrect payload hash *)
let test_wrong_payload_hash () =
  init_genesis () >>=? fun (_genesis, b) ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~is_preendorsement:false
    ~endorsed_block:b
    ~block_payload_hash:Block_payload_hash.zero
    ~error:(function
      | Validate_errors.Consensus.Wrong_payload_hash_for_consensus_operation
          {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    ()

let test_wrong_slot_used () =
  init_genesis () >>=? fun (_genesis, b) ->
  Context.get_endorser (B b) >>=? fun (_, slots) ->
  (match slots with
  | _x :: y :: _ -> return y
  | _ -> failwith "Slots size should be at least of 2 ")
  >>=? fun slot ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~is_preendorsement:false
    ~endorsed_block:b
    ~slot
    ~error:(function
      | Validate_errors.Consensus.Wrong_slot_used_for_consensus_operation
          {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    ()

(** Check that:
    - a block with not enough endorsement cannot be baked;
    - a block with enough endorsement is baked. *)
let test_endorsement_threshold ~sufficient_threshold () =
  (* We choose a relative large number of accounts so that the probability that
     any delegate has [consensus_threshold] slots is low and most delegates have
     about 1 slot so we can get closer to the limit of [consensus_threshold]: we
     check that a block with endorsing power [consensus_threshold - 1] won't be
     baked. *)
  Context.init_n 10 () >>=? fun (genesis, _contracts) ->
  Block.bake genesis >>=? fun b ->
  Context.get_constants (B b)
  >>=? fun {parametric = {consensus_threshold; _}; _} ->
  Context.get_endorsers (B b) >>=? fun endorsers_list ->
  Block.get_round b >>?= fun round ->
  List.fold_left_es
    (fun (counter, endos) {Plugin.RPC.Validators.delegate; slots; _} ->
      let new_counter = counter + List.length slots in
      if
        (sufficient_threshold && counter < consensus_threshold)
        || ((not sufficient_threshold) && new_counter < consensus_threshold)
      then
        Op.endorsement ~round ~delegate b >>=? fun endo ->
        return (new_counter, endo :: endos)
      else return (counter, endos))
    (0, [])
    endorsers_list
  >>=? fun (_, endos) ->
  Block.bake ~operations:endos b >>= fun b ->
  if sufficient_threshold then return_unit
  else Assert.proto_error_with_info ~loc:__LOC__ b "Not enough endorsements"

(** Fitness gap: this is a straightforward update from Emmy to Tenderbake, that
    is, check that the level is incremented in a child block. *)
let test_fitness_gap () =
  inject_the_first_endorsement () >>=? fun (b, pred_b) ->
  let fitness =
    match Fitness.from_raw b.header.shell.fitness with
    | Ok fitness -> fitness
    | _ -> assert false
  in
  let pred_fitness =
    match Fitness.from_raw pred_b.header.shell.fitness with
    | Ok fitness -> fitness
    | _ -> assert false
  in
  let level = Fitness.level fitness in
  let pred_level = Fitness.level pred_fitness in
  let level_diff =
    Int32.sub (Raw_level.to_int32 level) (Raw_level.to_int32 pred_level)
  in
  Assert.equal_int32 ~loc:__LOC__ level_diff 1l

let test_preendorsement_endorsement_same_level () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b1 ->
  Incremental.begin_construction ~mempool_mode:true ~policy:(By_round 2) b1
  >>=? fun i ->
  Op.endorsement b1 >>=? fun op_endo ->
  Incremental.add_operation i op_endo >>=? fun (_i : Incremental.t) ->
  Op.preendorsement b1 >>=? fun op_preendo ->
  Incremental.add_operation i op_preendo >>=? fun (_i : Incremental.t) ->
  return_unit

(** Test for endorsement injection with wrong slot in mempool mode. This
    test is expected to fail *)
let test_wrong_endorsement_slot_in_mempool_mode () =
  Context.init_n ~consensus_threshold:1 5 () >>=? fun (genesis, _) ->
  Block.bake genesis >>=? fun b1 ->
  let module V = Plugin.RPC.Validators in
  (Context.get_endorsers (B b1) >>=? function
   | {V.slots = _ :: non_canonical_slot :: _; _} :: _ ->
       (* we didn't use min slot for the injection. It's bad !*)
       return (Some non_canonical_slot)
   | _ -> assert false)
  >>=? fun slot ->
  Op.endorsement ?slot b1 >>=? fun endo ->
  Incremental.begin_construction ~mempool_mode:true b1 >>=? fun i ->
  Incremental.add_operation i endo >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Wrong_slot_used_for_consensus_operation {kind}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)

(** Endorsement for next level *)
let test_endorsement_for_next_level () =
  init_genesis () >>=? fun (genesis, _) ->
  Consensus_helpers.test_consensus_op_for_next
    ~genesis
    ~kind:`Endorsement
    ~next:`Level

(** Endorsement for next round *)
let test_endorsement_for_next_round () =
  init_genesis () >>=? fun (genesis, _) ->
  Consensus_helpers.test_consensus_op_for_next
    ~genesis
    ~kind:`Endorsement
    ~next:`Round

(** Endorsement of grandparent  *)
let test_endorsement_grandparent () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b_gp ->
  Block.bake b_gp >>=? fun b ->
  Incremental.begin_construction ~mempool_mode:true b >>=? fun i ->
  (* Endorsement on grandparent *)
  Op.endorsement b_gp >>=? fun op1 ->
  (* Endorsement on parent *)
  Op.endorsement b >>=? fun op2 ->
  (* Both should be accepted by the mempool *)
  Incremental.add_operation i op1 >>=? fun i ->
  Incremental.add_operation i op2 >>=? fun (_i : Incremental.t) -> return_unit

(** Double inclusion of grandparent endorsement *)
let test_double_endorsement_grandparent () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b_gp ->
  Block.bake b_gp >>=? fun b ->
  Incremental.begin_construction ~mempool_mode:true b >>=? fun i ->
  (* Endorsement on grandparent *)
  Op.endorsement b_gp >>=? fun op1 ->
  (* Endorsement on parent *)
  Op.endorsement b >>=? fun op2 ->
  (* The first grand parent endorsement should be accepted by the
     mempool but the second rejected. *)
  Incremental.add_operation i op1 >>=? fun i ->
  Incremental.add_operation i op1 >>= fun res ->
  Assert.proto_error_with_info
    ~loc:__LOC__
    res
    "Double inclusion of consensus operation"
  >>=? fun () ->
  Incremental.add_operation i op2 >>=? fun (_i : Incremental.t) -> return_unit

(** Endorsement of grandparent on same slot as parent *)
let test_endorsement_grandparent_same_slot () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b_gp ->
  Block.bake b_gp >>=? fun b ->
  Incremental.begin_construction ~mempool_mode:true b >>=? fun i ->
  (* Endorsement on parent *)
  Consensus_helpers.delegate_of_first_slot (B b) >>=? fun (delegate, slot) ->
  Op.endorsement ~delegate b >>=? fun op2 ->
  (* Endorsement on grandparent *)
  Consensus_helpers.delegate_of_slot slot (B b_gp) >>=? fun delegate ->
  Op.endorsement ~delegate b_gp >>=? fun op1 ->
  (* Both should be accepted by the mempool *)
  Incremental.add_operation i op1 >>=? fun i ->
  Incremental.add_operation i op2 >>=? fun (_i : Incremental.t) -> return_unit

(** Endorsement of grandparent in application mode should be rejected *)
let test_endorsement_grandparent_application () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b_gp ->
  Block.bake b_gp >>=? fun b ->
  Op.endorsement b_gp >>=? fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)

(** Endorsement of grandparent in full construction mode should be rejected *)
let test_endorsement_grandparent_full_construction () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b_gp ->
  Block.bake b_gp >>=? fun b ->
  Incremental.begin_construction b >>=? fun i ->
  (* Endorsement on grandparent *)
  Op.endorsement b_gp >>=? fun op1 ->
  Incremental.add_operation i op1 >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)

let tests =
  [
    Tztest.tztest "Simple endorsement" `Quick test_simple_endorsement;
    Tztest.tztest "Endorsement with slot -1" `Quick test_negative_slot;
    Tztest.tztest
      "Endorsement wrapped with non-normalized slot"
      `Quick
      test_non_normalized_slot;
    Tztest.tztest "Fitness gap" `Quick test_fitness_gap;
    (* Fail scenarios *)
    Tztest.tztest
      "Wrong endorsement predecessor"
      `Quick
      test_wrong_endorsement_predecessor;
    Tztest.tztest
      "Invalid endorsement level"
      `Quick
      test_invalid_endorsement_level;
    Tztest.tztest "Duplicate endorsement" `Quick test_duplicate_endorsement;
    Tztest.tztest
      "Endorsement for future level"
      `Quick
      test_consensus_operation_endorsement_for_future_level;
    Tztest.tztest
      "Endorsement for predecessor level"
      `Quick
      test_consensus_operation_endorsement_for_old_level;
    Tztest.tztest
      "Endorsement for old level"
      `Quick
      test_consensus_operation_endorsement_for_old_level;
    Tztest.tztest
      "Endorsement for future round"
      `Quick
      test_consensus_operation_endorsement_for_future_round;
    Tztest.tztest
      "Endorsement for old round"
      `Quick
      test_consensus_operation_endorsement_for_old_round;
    Tztest.tztest
      "Endorsement on competing proposal"
      `Quick
      test_consensus_operation_endorsement_on_competing_proposal;
    Tztest.tztest "Wrong level for consensus operation" `Quick test_wrong_level;
    Tztest.tztest "Wrong round for consensus operation" `Quick test_wrong_round;
    Tztest.tztest
      "Wrong payload hash for consensus operation"
      `Quick
      test_wrong_payload_hash;
    Tztest.tztest
      "Wrong slot used for consensus operation"
      `Quick
      test_wrong_slot_used;
    Tztest.tztest
      "sufficient endorsement threshold"
      `Quick
      (test_endorsement_threshold ~sufficient_threshold:true);
    Tztest.tztest
      "insufficient endorsement threshold"
      `Quick
      (test_endorsement_threshold ~sufficient_threshold:false);
    Tztest.tztest
      "Endorsement/Preendorsement at same level"
      `Quick
      test_preendorsement_endorsement_same_level;
    Tztest.tztest
      "Wrong endorsement slot in mempool mode"
      `Quick
      test_wrong_endorsement_slot_in_mempool_mode;
    Tztest.tztest
      "Endorsement for next level"
      `Quick
      test_endorsement_for_next_level;
    Tztest.tztest
      "Endorsement for next round"
      `Quick
      test_endorsement_for_next_round;
    Tztest.tztest
      "Endorsement for grandparent"
      `Quick
      test_endorsement_grandparent;
    Tztest.tztest
      "Double endorsement of grandparent"
      `Quick
      test_double_endorsement_grandparent;
    Tztest.tztest
      "Endorsement for grandparent on same slot as parent"
      `Quick
      test_endorsement_grandparent_same_slot;
    Tztest.tztest
      "Endorsement for grandparent in application mode"
      `Quick
      test_endorsement_grandparent_application;
    Tztest.tztest
      "Endorsement for grandparent in full construction mode"
      `Quick
      test_endorsement_grandparent_full_construction;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("endorsement", tests)]
  |> Lwt_main.run
