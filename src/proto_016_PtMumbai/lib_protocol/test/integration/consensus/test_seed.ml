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
    Component:    Protocol (seed)
    Invocation:   dune exec src/proto_016_PtMumbai/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_seed.ml
    Subject:      - seed_nonce_hash included in some blocks
                  - revelation operation of seed_nonce that should correspond
                  to each seed_nonce_hash
*)

open Protocol
open Lwt_result_syntax

(** Checking that, in the absence of nonce revelations and VDF computation,
    the seed of each cycle is correctly computed based on the seed of
    the previous cycle. *)
let test_seed_no_commitment () =
  let n_cycles = 15 in
  let (Hash initial_seed) =
    let empty_bytes = Bytes.(copy empty) in
    Tezos_crypto.Hacl.Blake2b.direct empty_bytes Nonce_hash.size
  in
  let seeds =
    (* compute the first `n_cycles` expected seeds *)
    let zero_bytes = Bytes.make Nonce_hash.size '\000' in
    let rec make_seeds s = function
      | 0 -> []
      | n ->
          let (Hash h) =
            Tezos_crypto.Hacl.Blake2b.direct
              (Bytes.cat s zero_bytes)
              Nonce_hash.size
          in
          h :: make_seeds h (n - 1)
    in
    make_seeds initial_seed n_cycles
  in
  let check_seed b expected_seed =
    let open Alpha_context in
    let* s = Context.get_seed (B b) in
    let seed_bytes = Data_encoding.Binary.to_bytes_exn Seed.seed_encoding s in
    (if expected_seed <> seed_bytes then
     let seed_pp =
       Hex.show
         (Hex.of_string
            (Data_encoding.Binary.to_string_exn Seed.seed_encoding s))
     in
     let expected_seed_pp = Hex.show (Hex.of_bytes expected_seed) in
     Stdlib.failwith
       (Format.sprintf "Seed: %s\nExpected: %s\n\n" seed_pp expected_seed_pp)) ;
    return b
  in
  let rec bake_and_check_seed b = function
    | [] -> return b
    | s :: seeds ->
        let* b = Block.bake_until_cycle_end b in
        let* b = check_seed b s in
        let* b = Block.bake_n 2 b in
        bake_and_check_seed b seeds
  in
  let* b, _delegates =
    Context.init3
      ~blocks_per_cycle:8l
      ~consensus_threshold:0
      ~nonce_revelation_threshold:2l
      ()
  in
  let* b = check_seed b initial_seed in
  let* (_ : Block.t) = bake_and_check_seed b seeds in
  return_unit

(** Baking [blocks_per_commitment] blocks without a [seed_nonce_hash]
    commitment fails with an "Invalid commitment in block header" error. *)
let test_no_commitment () =
  let* b, _contracts = Context.init_n ~consensus_threshold:0 5 () in
  let* {parametric = {blocks_per_commitment; _}; _} =
    Context.get_constants (B b)
  in
  let blocks_per_commitment = Int32.to_int blocks_per_commitment in
  (* Bake normally until before the commitment *)
  let* b = Block.bake_n (blocks_per_commitment - 2) b in
  (* Forge a block with empty commitment and apply it *)
  let* header = Block.Forge.forge_header b in
  let* header =
    Block.Forge.set_seed_nonce_hash None header |> Block.Forge.sign_header
  in
  let*! e = Block.apply header b in
  Assert.proto_error_with_info
    ~loc:__LOC__
    e
    "Invalid commitment in block header"

(** Choose a baker, denote it by id. In the first cycle, make id bake only once.
    Check that:
    - when id reveals the nonce too early, there's an error
    - when id reveals at the right time but the wrong value, there's an error
    - when another baker reveals correctly, it receives the tip
    - revealing twice produces an error *)
let test_revelation_early_wrong_right_twice () =
  let open Assert in
  let* b, _contracts = Context.init_n ~consensus_threshold:0 5 () in
  let* csts = Context.get_constants (B b) in
  let tip = csts.parametric.seed_nonce_revelation_tip in
  let blocks_per_commitment =
    Int32.to_int csts.parametric.blocks_per_commitment
  in
  let baking_reward_fixed_portion =
    csts.parametric.baking_reward_fixed_portion
  in
  (* get the pkh of a baker *)
  let* pkh, _, _, _ = Block.get_next_baker b in
  let id = Alpha_context.Contract.Implicit pkh in
  let policy = Block.Excluding [pkh] in
  (* bake until commitment - 2, excluding id *)
  let* b = Block.bake_n ~policy (blocks_per_commitment - 2) b in
  let* bal_main = Context.Contract.balance (B b) id in
  (* the baker [id] will include a seed_nonce commitment *)
  let* b = Block.bake ~policy:(Block.By_account pkh) b in
  let*? level_commitment = Context.get_level (B b) in
  let* committed_hash = Context.get_seed_nonce_hash (B b) in
  (* test that the baking reward is received *)
  let* () =
    balance_was_credited
      ~loc:__LOC__
      (B b)
      id
      bal_main
      baking_reward_fixed_portion
  in
  (* test that revealing too early produces an error *)
  let operation =
    Op.seed_nonce_revelation
      (B b)
      level_commitment
      (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get committed_hash)
  in
  let*! e = Block.bake ~policy ~operation b in
  let* () =
    Assert.proto_error ~loc:__LOC__ e (function
        | Nonce_storage.Too_early_revelation -> true
        | _ -> false)
  in
  (* finish the cycle excluding the committing baker, id *)
  let* b = Block.bake_until_cycle_end ~policy b in
  (* test that revealing at the right time but the wrong value
     produces an error *)
  let wrong_hash, _ = Nonce.generate () in
  let operation =
    Op.seed_nonce_revelation
      (B b)
      level_commitment
      (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get wrong_hash)
  in
  let*! e = Block.bake ~operation b in
  let* () =
    Assert.proto_error ~loc:__LOC__ e (function
        | Nonce_storage.Inconsistent_nonce -> true
        | _ -> false)
  in
  (* reveals correctly *)
  let operation =
    Op.seed_nonce_revelation
      (B b)
      level_commitment
      (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get committed_hash)
  in
  let* baker_pkh, _, _, _ = Block.get_next_baker ~policy b in
  let baker = Alpha_context.Contract.Implicit baker_pkh in
  let* baker_bal = Context.Contract.balance (B b) baker in
  (* test that revealing twice in a block produces an error *)
  let*! e =
    Block.bake
      ~policy:(Block.By_account baker_pkh)
      ~operations:[operation; operation]
      b
  in
  let* () =
    Assert.proto_error ~loc:__LOC__ e (function
        | Validate_errors.Anonymous.Conflicting_nonce_revelation _ -> true
        | _ -> false)
  in
  let* b = Block.bake ~policy:(Block.By_account baker_pkh) ~operation b in
  (* test that the baker gets the tip reward plus the baking reward*)
  let* () =
    balance_was_credited
      ~loc:__LOC__
      (B b)
      baker
      baker_bal
      Test_tez.(tip +! baking_reward_fixed_portion)
  in
  (* test that revealing twice produces an error *)
  let operation =
    Op.seed_nonce_revelation
      (B b)
      level_commitment
      (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get wrong_hash)
  in
  let*! e = Block.bake ~operation ~policy b in
  Assert.proto_error ~loc:__LOC__ e (function
      | Nonce_storage.Already_revealed_nonce -> true
      | _ -> false)

(** Test that revealing too late produces an error. Note that a
    committer who doesn't reveal at cycle 1 is not punished.*)
let test_revelation_missing_and_late () =
  let open Context in
  let open Assert in
  let* b, _contracts = Context.init_n ~consensus_threshold:0 5 () in
  let* csts = get_constants (B b) in
  let blocks_per_commitment =
    Int32.to_int csts.parametric.blocks_per_commitment
  in
  let nonce_revelation_threshold =
    Int32.to_int csts.parametric.nonce_revelation_threshold
  in
  (* bake until commitment *)
  let* b = Block.bake_n (blocks_per_commitment - 2) b in
  (* the next baker [id] will include a seed_nonce commitment *)
  let* pkh, _, _, _ = Block.get_next_baker b in
  let* b = Block.bake b in
  let*? level_commitment = Context.get_level (B b) in
  let* committed_hash = Context.get_seed_nonce_hash (B b) in
  (* finish cycle 0 excluding the committing baker [id] *)
  let policy = Block.Excluding [pkh] in
  let* b = Block.bake_until_cycle_end ~policy b in
  (* test that revealing after revelation period produces an error *)
  let* b = Block.bake_n (nonce_revelation_threshold - 1) b in
  let operation =
    Op.seed_nonce_revelation
      (B b)
      level_commitment
      (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get committed_hash)
  in
  let*! e = Block.bake ~operation ~policy b in
  let* () =
    Assert.proto_error ~loc:__LOC__ e (function
        | Nonce_storage.Too_late_revelation -> true
        | _ -> false)
  in
  (* finish cycle 1 excluding the committing baker [id] *)
  let* b = Block.bake_until_cycle_end ~policy b in
  (* test that revealing too late after cycle 1 produces an error *)
  let operation =
    Op.seed_nonce_revelation
      (B b)
      level_commitment
      (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get committed_hash)
  in
  let*! e = Block.bake ~operation b in
  Assert.proto_error ~loc:__LOC__ e (function
      | Nonce_storage.Too_late_revelation -> true
      | _ -> false)

let wrap e = e >|= Environment.wrap_tzresult

(** Test that we do not distribute endorsing rewards if the nonce was
    not revealed. *)
let test_unrevealed () =
  let open Alpha_context in
  let constants =
    {
      Default_parameters.constants_test with
      endorsing_reward_per_slot = Tez.one_mutez;
      baking_reward_bonus_per_slot = Tez.zero;
      baking_reward_fixed_portion = Tez.zero;
      seed_nonce_revelation_tip = Tez.zero;
      consensus_threshold = 0;
      minimal_participation_ratio = Ratio.{numerator = 0; denominator = 1};
    }
  in
  let* b, (_account1, account2) = Context.init_with_constants2 constants in
  let delegate2 = Context.Contract.pkh account2 in
  (* Delegate 2 will add a nonce but never reveals it *)
  let* csts = Context.get_constants (B b) in
  let blocks_per_commitment =
    Int32.to_int csts.parametric.blocks_per_commitment
  in
  let bake_and_endorse_block ?policy (_pred_b, b) =
    let* slots = Context.get_endorsers (B b) in
    let* endorsements =
      List.map_es
        (fun {Plugin.RPC.Validators.consensus_key; _} ->
          Op.endorsement ~delegate:consensus_key b)
        slots
    in
    Block.bake ?policy ~operations:endorsements b
  in
  (* Bake until commitment *)
  let* b = Block.bake_n (blocks_per_commitment - 2) b in
  (* Baker delegate 2 will include a seed_nonce commitment *)
  let policy = Block.By_account delegate2 in
  let* b = Block.bake_until_cycle_end ~policy b in
  let* info_before = Context.Delegate.info (B b) delegate2 in
  let* b' = Block.bake ~policy b in
  let* b = bake_and_endorse_block ~policy (b, b') in
  (* Finish cycle 1 excluding the first baker *)
  let* b = Block.bake_until_cycle_end ~policy b in
  let* info_after = Context.Delegate.info (B b) delegate2 in
  (* Assert that we did not received a reward because we didn't
     reveal the nonce. *)
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      info_before.full_balance
      info_after.full_balance
  in
  return_unit

let test_vdf_status () =
  let* b, _ = Context.init3 ~consensus_threshold:0 () in
  let* b = Block.bake b in
  let* status = Context.get_seed_computation (B b) in
  assert (status = Alpha_context.Seed.Nonce_revelation_stage) ;
  let* constants = Context.get_constants (B b) in
  let* b =
    Block.bake_n
      (Int32.to_int constants.parametric.nonce_revelation_threshold)
      b
  in
  let* status = Context.get_seed_computation (B b) in
  assert (
    match status with
    | Alpha_context.Seed.Vdf_revelation_stage _ -> true
    | _ -> false) ;
  return_unit

(** Choose a baker, denote it by id. In the first cycle, make id bake only once.
  Check that:
  - when the vdf is revealed too early, there's an error
  - when the vdf is revealed at the right time but the wrong value, there's an error
  - when the vdf is revealed at the right time and the correct value,
    - the baker receives a reward
    - the VDF status is updated to "Computation_finished"
    - the seed is updated with the vdf solution
  - another vdf revelation produces an error *)
let test_early_incorrect_unverified_correct_already_vdf () =
  let open Assert in
  let* b, _ = Context.init3 ~consensus_threshold:0 () in
  let* csts = Context.get_constants (B b) in
  let blocks_per_commitment =
    Int32.to_int csts.parametric.blocks_per_commitment
  in
  let nonce_revelation_threshold =
    Int32.to_int csts.parametric.nonce_revelation_threshold
  in
  let baking_reward_fixed_portion =
    csts.parametric.baking_reward_fixed_portion
  in
  let seed_nonce_revelation_tip = csts.parametric.seed_nonce_revelation_tip in
  let vdf_nonce_revelation_tip = csts.parametric.seed_nonce_revelation_tip in
  (* get the pkh of a baker *)
  let* pkh, _, _, _ = Block.get_next_baker b in
  let id = Alpha_context.Contract.Implicit pkh in
  let policy = Block.Excluding [pkh] in
  (* bake until commitment - 2, excluding id *)
  let* b = Block.bake_n ~policy (blocks_per_commitment - 2) b in
  let* bal_main = Context.Contract.balance (B b) id in
  (* the baker [id] will include a seed_nonce commitment *)
  let* b = Block.bake ~policy:(Block.By_account pkh) b in
  let*? level_commitment = Context.get_level (B b) in
  let* committed_hash = Context.get_seed_nonce_hash (B b) in
  (* test that the baking reward is received *)
  let* () =
    balance_was_credited
      ~loc:__LOC__
      (B b)
      id
      bal_main
      baking_reward_fixed_portion
  in
  (* finish the cycle excluding the committing baker, id *)
  let* b = Block.bake_until_cycle_end ~policy b in
  (* reveals correctly *)
  let operation =
    Op.seed_nonce_revelation
      (B b)
      level_commitment
      (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get committed_hash)
  in
  let* baker_pkh, _, _, _ = Block.get_next_baker ~policy b in
  let baker = Alpha_context.Contract.Implicit baker_pkh in
  let* baker_bal = Context.Contract.balance (B b) baker in
  let* b = Block.bake ~policy:(Block.By_account baker_pkh) ~operation b in
  (* test that the baker gets the tip reward plus the baking reward*)
  let* () =
    balance_was_credited
      ~loc:__LOC__
      (B b)
      baker
      baker_bal
      Test_tez.(seed_nonce_revelation_tip +! baking_reward_fixed_portion)
  in
  (* test that revealing the VDF early produces an error *)
  let dummy_solution =
    let open Environment.Vdf in
    let dummy = Bytes.create Environment.Vdf.form_size_bytes in
    let result = Stdlib.Option.get @@ result_of_bytes_opt dummy in
    let proof = Stdlib.Option.get @@ proof_of_bytes_opt dummy in
    (result, proof)
  in
  let operation = Op.vdf_revelation (B b) dummy_solution in
  let*! e = Block.bake ~operation b in
  let* () =
    Assert.proto_error ~loc:__LOC__ e (function
        | Seed_storage.Too_early_revelation -> true
        | _ -> false)
  in
  (* bake until nonce reveal period finishes *)
  let* b = Block.bake_n ~policy nonce_revelation_threshold b in
  (* test that revealing non group elements produces an error *)
  let operation = Op.vdf_revelation (B b) dummy_solution in
  let*! e = Block.bake ~operation b in
  let* () =
    Assert.proto_error ~loc:__LOC__ e (function
        | Seed_storage.Unverified_vdf -> true
        | _ -> false)
  in
  let* seed_status = Context.get_seed_computation (B b) in
  match seed_status with
  | Nonce_revelation_stage -> assert false
  | Computation_finished -> assert false
  | Vdf_revelation_stage info -> (
      (* generate the VDF discriminant and challenge *)
      let discriminant, challenge =
        Alpha_context.Seed.generate_vdf_setup
          ~seed_discriminant:info.seed_discriminant
          ~seed_challenge:info.seed_challenge
      in
      (* test that revealing wrong VDF produces an error *)
      let wrong_solution =
        let open Environment.Vdf in
        let f = challenge_to_bytes challenge in
        let result = Stdlib.Option.get @@ result_of_bytes_opt f in
        let proof = Stdlib.Option.get @@ proof_of_bytes_opt f in
        (result, proof)
      in
      let operation = Op.vdf_revelation (B b) wrong_solution in
      let*! e = Block.bake ~operation b in
      let* () =
        Assert.proto_error ~loc:__LOC__ e (function
            | Seed_storage.Unverified_vdf -> true
            | _ -> false)
      in
      (* test with correct input *)
      (* compute the VDF solution (the result and the proof ) *)
      let solution =
        (* generate the result and proof *)
        Environment.Vdf.prove
          discriminant
          challenge
          csts.parametric.vdf_difficulty
      in
      let* baker_bal = Context.Contract.balance (B b) baker in
      let operation = Op.vdf_revelation (B b) solution in
      let*! e =
        Block.bake
          ~policy:(Block.By_account baker_pkh)
          ~operations:[operation; operation]
          b
      in
      let* () =
        Assert.proto_error ~loc:__LOC__ e (function
            | Validate_errors.Anonymous.Conflicting_vdf_revelation _ -> true
            | _ -> false)
      in
      (* verify the balance was credited following operation inclusion *)
      let* b = Block.bake ~policy:(Block.By_account baker_pkh) ~operation b in
      let* () =
        balance_was_credited
          ~loc:__LOC__
          (B b)
          baker
          baker_bal
          Test_tez.(vdf_nonce_revelation_tip +! baking_reward_fixed_portion)
      in
      (* verify the seed status has changed *)
      let* seed_status = Context.get_seed_computation (B b) in
      match seed_status with
      | Nonce_revelation_stage -> assert false
      | Vdf_revelation_stage _ -> assert false
      | Computation_finished ->
          (* test than sending another VDF reveal produces an error *)
          let operation = Op.vdf_revelation (B b) solution in
          let*! e = Block.bake ~operation b in
          let* () =
            Assert.proto_error ~loc:__LOC__ e (function
                | Seed_storage.Already_accepted -> true
                | _ -> false)
          in
          (* verify the stored seed has the expected value *)
          let open Data_encoding.Binary in
          let open Alpha_context in
          (* retrieving & converting seed stored in cycle n + preserved_cycle + 1 *)
          let* b =
            Block.bake_until_n_cycle_end
              ~policy
              (csts.parametric.preserved_cycles + 1)
              b
          in
          let* stored_seed = Context.get_seed (B b) in
          let vdf_stored_seed = to_bytes_exn Seed.seed_encoding stored_seed in
          (* recomputing seed with randao output and vdf solution *)
          let vdf_expected_seed =
            let randao_seed =
              to_bytes_exn Seed.seed_encoding info.seed_challenge
              |> of_bytes_exn Seed_repr.seed_encoding
            in
            Seed_repr.vdf_to_seed randao_seed solution
            |> to_bytes_exn Seed_repr.seed_encoding
          in
          assert (Bytes.(equal vdf_expected_seed vdf_stored_seed)) ;
          return_unit)

(* We check that bounds used in [Seed_storage.for_cycle] are as expected. *)
let test_cycle_bounds () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, _accounts) ->
  Context.get_constants (B b) >>=? fun csts ->
  let past_offset = csts.parametric.max_slashing_period - 1 in
  let future_offset = csts.parametric.preserved_cycles in
  let open Alpha_context.Cycle in
  let expected_error_message direction current_cycle =
    match direction with
    | `Past ->
        let oldest_cycle = Stdlib.Option.get (sub current_cycle past_offset) in
        let older_cycle = Stdlib.Option.get (sub oldest_cycle 1) in
        Format.asprintf
          "The seed for cycle %a has been cleared from the context  (oldest \
           known seed is for cycle %a)"
          pp
          older_cycle
          pp
          oldest_cycle
    | `Future ->
        let latest_cycle = add current_cycle future_offset in
        let later_cycle = add latest_cycle 1 in
        Format.asprintf
          "The seed for cycle %a has not been computed yet  (latest known seed \
           is for cycle %a)"
          pp
          later_cycle
          pp
          latest_cycle
    | `Missing_sampler_state cycle ->
        Format.asprintf
          "Storage error:\n  Missing key 'cycle/%a/delegate_sampler_state'."
          pp
          cycle
  in
  let cycle = root in
  Context.get_bakers ~cycle:(add cycle future_offset) (B b)
  >>=? fun (_ : _ list) ->
  let future_cycle = add cycle (future_offset + 1) in
  Context.get_bakers ~cycle:future_cycle (B b) >>= fun res ->
  (* the first cycle is special *)
  Assert.proto_error_with_info
    ~loc:__LOC__
    ~error_info_field:`Message
    res
    (expected_error_message (`Missing_sampler_state future_cycle) cycle)
  >>=? fun () ->
  Block.bake_until_cycle_end b >>=? fun b ->
  let cycle = add cycle 1 in
  Context.get_bakers ~cycle:root (B b) >>=? fun (_ : _ list) ->
  Context.get_bakers ~cycle:(add cycle future_offset) (B b)
  >>=? fun (_ : _ list) ->
  Context.get_bakers ~cycle:(add cycle (future_offset + 1)) (B b) >>= fun res ->
  Assert.proto_error_with_info
    ~loc:__LOC__
    res
    ~error_info_field:`Message
    (expected_error_message `Future cycle)
  >>=? fun () ->
  Block.bake_until_n_cycle_end past_offset b >>=? fun b ->
  let cycle = add cycle past_offset in
  Context.get_bakers ~cycle:(Stdlib.Option.get (sub cycle past_offset)) (B b)
  >>=? fun (_ : _ list) ->
  Context.get_bakers
    ~cycle:(Stdlib.Option.get (sub cycle (past_offset + 1)))
    (B b)
  >>= fun res ->
  Assert.proto_error_with_info
    ~loc:__LOC__
    res
    ~error_info_field:`Message
    (expected_error_message `Past cycle)
  >>=? fun () ->
  Context.get_bakers ~cycle:(add cycle future_offset) (B b)
  >>=? fun (_ : _ list) ->
  Context.get_bakers ~cycle:(add cycle (future_offset + 1)) (B b) >>= fun res ->
  Assert.proto_error_with_info
    ~loc:__LOC__
    res
    ~error_info_field:`Message
    (expected_error_message `Future cycle)

let tests =
  [
    Tztest.tztest
      "seed computation (no commitment)"
      `Quick
      test_seed_no_commitment;
    Tztest.tztest "no commitment" `Quick test_no_commitment;
    Tztest.tztest
      "revelation_early_wrong_right_twice"
      `Quick
      test_revelation_early_wrong_right_twice;
    Tztest.tztest
      "revelation_missing_and_late"
      `Quick
      test_revelation_missing_and_late;
    Tztest.tztest "unrevealed" `Quick test_unrevealed;
    Tztest.tztest
      "early_incorrect_unverified_correct_already_vdf"
      `Quick
      test_early_incorrect_unverified_correct_already_vdf;
    Tztest.tztest "VDF status" `Quick test_vdf_status;
    Tztest.tztest "for_cycle cycle bounds" `Quick test_cycle_bounds;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("seed", tests)] |> Lwt_main.run
