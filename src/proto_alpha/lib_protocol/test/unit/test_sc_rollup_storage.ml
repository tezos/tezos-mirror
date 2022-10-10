(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** Testing
    -------
    Component:  Protocol Sc_rollup_storage
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
      -- test "^\[Unit\] Sc_rollup_storage.ml$"
    Subject:    Tests for the SCORU storage module
*)

open Protocol
open Lwt_result_syntax
module Commitment_repr = Sc_rollup_commitment_repr

(** Lift a computation using using environment errors to use shell errors. *)
let lift k = Lwt.map Environment.wrap_tzresult k

(** [new_context_with_stakers n] creates a context with [n] stakers initially
    credited with 100 000 tez. *)
let new_context_with_stakers nb_stakers =
  let initial_balance = Int64.of_string "100_000_000_000" in
  let*? bootstrap_balances =
    List.init ~when_negative_length:[] nb_stakers (fun _ -> initial_balance)
  in
  let sc_rollup_max_number_of_messages_per_commitment_period =
    (* The default value is too large for testing. *)
    1000
  in
  let* b, contracts =
    Context.init_n
      ~bootstrap_balances
      nb_stakers
      ~sc_rollup_max_number_of_messages_per_commitment_period
      ()
  in
  let+ inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  (* Necessary to originate rollups. *)
  let ctxt = Alpha_context.Origination_nonce.init ctxt Operation_hash.zero in
  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in
  let stakers =
    List.map
      (function
        | Alpha_context.Contract.Implicit key -> key | _ -> assert false)
      contracts
  in
  (ctxt, stakers)

let initial_staker_balance = Tez_repr.of_mutez_exn 100_000_000_000L

let new_context () =
  let* ctxt, _stakers = new_context_with_stakers 1 in
  (* Mint some tez for staker accounts. *)
  let mint_tez_for ctxt pkh_str =
    let pkh = Signature.Public_key_hash.of_b58check_exn pkh_str in
    let contract = Contract_repr.Implicit pkh in
    let+ ctxt, _ =
      lift
      @@ Token.transfer ctxt `Minted (`Contract contract) initial_staker_balance
    in
    ctxt
  in
  let* ctxt = mint_tez_for ctxt "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG" in
  let* ctxt = mint_tez_for ctxt "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo" in
  mint_tez_for ctxt "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

let new_sc_rollup ctxt =
  let+ rollup, _size, genesis_hash, ctxt =
    let {Michelson_v1_parser.expanded; _}, _ =
      Michelson_v1_parser.parse_expression "unit"
    in
    let parameters_ty = Alpha_context.Script.lazy_expr expanded in
    let boot_sector = "" in
    let kind = Sc_rollups.Kind.Example_arith in
    let*! genesis_commitment =
      Sc_rollup_helpers.genesis_commitment_raw
        ~boot_sector
        ~origination_level:(Raw_context.current_level ctxt).level
        kind
    in
    Sc_rollup_storage.originate
      ctxt
      ~kind
      ~boot_sector
      ~parameters_ty
      ~genesis_commitment
  in
  (rollup, genesis_hash, ctxt)

let new_context_with_stakers_and_rollup nb_stakers =
  let* ctxt, stakers = new_context_with_stakers nb_stakers in
  let+ rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  (ctxt, rollup, genesis_hash, stakers)

let new_context_with_rollup () =
  let* ctxt = new_context () in
  let+ rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  (ctxt, rollup, genesis_hash)

let equal_tez ~loc =
  Assert.equal ~loc Tez_repr.( = ) "Tez aren't equal" Tez_repr.pp

let assert_balance_changed op ctxt ctxt' account amount =
  let* _, balance = lift @@ Token.balance ctxt account in
  let* _, balance' = lift @@ Token.balance ctxt' account in
  let* balance_op_amount = lift @@ op balance amount in
  equal_tez balance' ~loc:__LOC__ balance_op_amount

let assert_balance_increased ctxt ctxt' account amount =
  let ( +? ) t1 t2 = Lwt.return Tez_repr.(t1 +? t2) in
  assert_balance_changed ( +? ) ctxt ctxt' account amount

let assert_balance_decreased ctxt ctxt' account amount =
  let ( -? ) t1 t2 = Lwt.return Tez_repr.(t1 -? t2) in
  assert_balance_changed ( -? ) ctxt ctxt' account amount

let perform_staking_action_and_check ctxt rollup staker do_and_check =
  let staker_contract = Contract_repr.Implicit staker in
  let stake = Constants_storage.sc_rollup_stake_amount ctxt in
  do_and_check ctxt rollup staker_contract stake

let deposit_stake_and_check_balances ctxt rollup staker =
  perform_staking_action_and_check
    ctxt
    rollup
    staker
    (fun ctxt rollup staker_contract stake ->
      let* ctxt', _, _ =
        lift
        @@ Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
             ctxt
             rollup
             staker
      in
      let* () =
        assert_balance_decreased ctxt ctxt' (`Contract staker_contract) stake
      in
      let bond_id = Bond_id_repr.Sc_rollup_bond_id rollup in
      let bonds_account = `Frozen_bonds (staker_contract, bond_id) in
      let+ () = assert_balance_increased ctxt ctxt' bonds_account stake in
      ctxt')

(** Originate a rollup with [nb_stakers] stakers and make a deposit to the
    initial LCC. *)
let originate_rollup_and_deposit_with_n_stakers nb_stakers =
  let* ctxt, rollup, genesis_hash, stakers =
    new_context_with_stakers_and_rollup nb_stakers
  in
  let deposit ctxt staker =
    deposit_stake_and_check_balances ctxt rollup staker
  in
  let+ ctxt = List.fold_left_es deposit ctxt stakers in
  (ctxt, rollup, genesis_hash, stakers)

(** Originate a rollup with one staker and make a deposit to the initial LCC. *)
let originate_rollup_and_deposit_with_one_staker () =
  let* ctxt = new_context () in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let+ ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  (ctxt, rollup, genesis_hash, staker)

(** Originate a rollup with two stakers and make a deposit to the initial LCC.
*)
let originate_rollup_and_deposit_with_two_stakers () =
  let* ctxt = new_context () in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker1 =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let staker2 =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker1 in
  let+ ctxt = deposit_stake_and_check_balances ctxt rollup staker2 in
  (ctxt, rollup, genesis_hash, staker1, staker2)

(** Originate a rollup with three stakers and make a deposit to the initial LCC.
*)
let originate_rollup_and_deposit_with_three_stakers () =
  let+ ctxt, rollup, genesis_hash, stakers =
    originate_rollup_and_deposit_with_n_stakers 3
  in
  match stakers with
  | [staker1; staker2; staker3] ->
      (ctxt, rollup, genesis_hash, staker1, staker2, staker3)
  | _ -> assert false

(** Trivial assertion.

    By convention, context is passed linearly as [ctxt].  This takes a context
    argument to allow this.
*)
let assert_true _ctxt = return ()

(** Assert that the computation fails with the given message. *)
let assert_fails_with ~loc k expected_err =
  let open Lwt_result_syntax in
  let*! res = k in
  let res = Environment.wrap_tzresult res in
  Assert.proto_error ~loc res (( = ) expected_err)

(** Assert operation fails because of missing rollup *)
let assert_fails_with_missing_rollup ~loc op =
  let* ctxt = new_context () in
  let rollup = Sc_rollup_repr.Address.hash_bytes [] in
  assert_fails_with
    ~loc
    (op ctxt rollup) (* Hash of empty sequence *)
    (Sc_rollup_errors.Sc_rollup_does_not_exist rollup)

(** Assert commitment hash equality.

    By convention, context is passed linearly as [ctxt].  This takes a context
    argument to allow this.
    *)
let assert_commitment_hash_equal ~loc _ctxt x y =
  Assert.equal
    ~loc
    Commitment_repr.Hash.equal
    "Compare commitment hash"
    Commitment_repr.Hash.pp
    x
    y

let assert_commitment_equal ~loc x y =
  Assert.equal
    ~loc
    (fun Commitment_repr.
           {
             compressed_state = c1;
             inbox_level = l1;
             predecessor = p1;
             number_of_ticks = n1;
           }
         Commitment_repr.
           {
             compressed_state = c2;
             inbox_level = l2;
             predecessor = p2;
             number_of_ticks = n2;
           } ->
      Sc_rollup_repr.State_hash.equal c1 c2
      && Raw_level_repr.equal l1 l2
      && Commitment_repr.Hash.equal p1 p2
      && Sc_rollup_repr.Number_of_ticks.equal n1 n2)
    "Compare commitment"
    Commitment_repr.pp
    x
    y

let assert_commitments_with_levels_equal ~loc cs1 cs2 =
  let commitment_with_level_pp ppf (hash, level) =
    Format.fprintf
      ppf
      "(%a, %a)"
      Sc_rollup_commitment_repr.Hash.pp
      hash
      Raw_level_repr.pp
      level
  in
  Assert.assert_equal_list
    ~loc
    (fun (commitment1, level1) (commitment2, level2) ->
      Sc_rollup_commitment_repr.Hash.(commitment1 = commitment2)
      && Raw_level_repr.(level1 = level2))
    "Unexpected list of cemented commitments"
    commitment_with_level_pp
    cs1
    cs2

let assert_kinds_are_equal ~loc x y =
  Assert.equal
    ~loc
    (Option.equal Sc_rollups.Kind.equal)
    "Compare optional kind"
    (Format.pp_print_option Sc_rollups.Kind.pp)
    x
    y

let test_deposit_to_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
        ctxt
        rollup
        Sc_rollup_repr.Staker.zero)

let test_last_cemented_commitment_hash_with_level_when_genesis () =
  let* ctxt = new_context () in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let* c1, inbox_level, ctxt =
    lift
    @@ Sc_rollup_commitment_storage.last_cemented_commitment_hash_with_level
         ctxt
         rollup
  in
  let* () = assert_commitment_hash_equal ~loc:__LOC__ ctxt genesis_hash c1 in
  Assert.equal_int32
    ~loc:__LOC__
    (Raw_level_repr.to_int32 (Raw_context.current_level ctxt).level)
    (Raw_level_repr.to_int32 inbox_level)

let test_deposit_by_underfunded_staker () =
  let* ctxt, sc_rollup, _genesis_hash = new_context_with_rollup () in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1hhNZvjed6McQQLWtR7MRzPHpgSFZTXxdW"
  in
  let stake = Constants_storage.sc_rollup_stake_amount ctxt in
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
         ctxt
         sc_rollup
         staker)
      (Sc_rollup_errors.Sc_rollup_staker_funds_too_low
         {
           staker;
           sc_rollup;
           staker_balance = Tez_repr.zero;
           min_expected_balance = stake;
         })
  in
  let staker_balance = Tez_repr.div_exn stake 2 in
  let staker_contract = Contract_repr.Implicit staker in
  let* ctxt, _ =
    lift
    @@ Token.transfer ctxt `Minted (`Contract staker_contract) staker_balance
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
       ctxt
       sc_rollup
       staker)
    (Sc_rollup_errors.Sc_rollup_staker_funds_too_low
       {staker; sc_rollup; staker_balance; min_expected_balance = stake})

let test_initial_state_is_pre_boot () =
  let* ctxt, rollup, genesis_hash = new_context_with_rollup () in
  let* lcc, ctxt =
    lift @@ Sc_rollup_commitment_storage.last_cemented_commitment ctxt rollup
  in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt lcc genesis_hash

let test_deposit_to_existing_rollup () =
  let* ctxt, _rollup, _genesis_hash, _staker =
    originate_rollup_and_deposit_with_one_staker ()
  in
  assert_true ctxt

let assert_balance_unchanged ctxt ctxt' account =
  let* _, balance = lift @@ Token.balance ctxt account in
  let* _, balance' = lift @@ Token.balance ctxt' account in
  equal_tez ~loc:__LOC__ balance' balance

let remove_staker_and_check_balances ctxt rollup staker =
  perform_staking_action_and_check
    ctxt
    rollup
    staker
    (fun ctxt rollup staker_contract stake ->
      let* ctxt', _ =
        lift @@ Sc_rollup_stake_storage.remove_staker ctxt rollup staker
      in
      let* () =
        assert_balance_unchanged ctxt ctxt' (`Contract staker_contract)
      in
      let bond_id = Bond_id_repr.Sc_rollup_bond_id rollup in
      let bonds_account = `Frozen_bonds (staker_contract, bond_id) in
      let+ () = assert_balance_decreased ctxt ctxt' bonds_account stake in
      ctxt')

let test_removing_staker_from_lcc_fails () =
  let* ctxt, rollup, _genesis_hash, staker =
    originate_rollup_and_deposit_with_one_staker ()
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.remove_staker ctxt rollup staker)
    Sc_rollup_errors.Sc_rollup_remove_lcc

let withdraw_stake_and_check_balances ctxt rollup staker =
  perform_staking_action_and_check
    ctxt
    rollup
    staker
    (fun ctxt rollup staker_contract stake ->
      let* ctxt', _ =
        lift @@ Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker
      in
      let* () =
        assert_balance_increased ctxt ctxt' (`Contract staker_contract) stake
      in
      let bond_id = Bond_id_repr.Sc_rollup_bond_id rollup in
      let bonds_account = `Frozen_bonds (staker_contract, bond_id) in
      let+ () = assert_balance_decreased ctxt ctxt' bonds_account stake in
      ctxt')

let test_deposit_then_withdraw () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Signature.Public_key_hash.of_b58check_exn
      "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let* ctxt = withdraw_stake_and_check_balances ctxt rollup staker in
  assert_true ctxt

let test_withdrawal_from_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_stake_storage.withdraw_stake
        ctxt
        rollup
        Sc_rollup_repr.Staker.zero)

let test_withdraw_when_not_staked () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Signature.Public_key_hash.of_b58check_exn
      "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker)
    Sc_rollup_errors.Sc_rollup_not_staked

let test_withdrawing_twice () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Signature.Public_key_hash.of_b58check_exn
      "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let* ctxt = withdraw_stake_and_check_balances ctxt rollup staker in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker)
    Sc_rollup_errors.Sc_rollup_not_staked

let number_of_ticks_exn n =
  match Sc_rollup_repr.Number_of_ticks.of_value n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_ticks"

let dal_slot_index_of_int_exn n =
  match Dal_slot_repr.Index.of_int n with
  | Some n -> n
  | None -> Stdlib.failwith "Bad slot index"

let valid_inbox_level ctxt =
  let root_level = Raw_level_repr.to_int32 Level_storage.(current ctxt).level in
  let commitment_freq =
    Constants_storage.sc_rollup_commitment_period_in_blocks ctxt
  in
  fun i ->
    Raw_level_repr.of_int32_exn
      (Int32.add root_level (Int32.mul (Int32.of_int commitment_freq) i))

let produce_and_refine ctxt ~number_of_commitments ?(start_at_level = 1l)
    ~predecessor staker rollup =
  let rec aux ctxt n l predecessor result =
    if n = 0 then return @@ (List.rev result, ctxt)
    else
      let commitment =
        Commitment_repr.
          {
            predecessor;
            inbox_level = valid_inbox_level ctxt l;
            number_of_ticks = number_of_ticks_exn 1232909L;
            compressed_state = Sc_rollup_repr.State_hash.zero;
          }
      in
      let* c, _level, ctxt =
        Sc_rollup_stake_storage.Internal_for_tests.refine_stake
          ctxt
          rollup
          staker
          commitment
      in
      aux ctxt (n - 1) (Int32.succ l) c (c :: result)
  in
  aux ctxt number_of_commitments start_at_level predecessor []

let rec cement_commitments ctxt commitments rollup =
  match commitments with
  | [] -> return ctxt
  | c :: commitments ->
      let* ctxt, _commitment =
        Sc_rollup_stake_storage.cement_commitment ctxt rollup c
      in
      cement_commitments ctxt commitments rollup

let test_deposit_then_refine () =
  let* ctxt = new_context () in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state =
          Sc_rollup_repr.State_hash.zero (* genesis.compressed_state; *);
      }
  in
  let* _node, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment
  in
  assert_true ctxt

let test_deposit_then_refine_bad_inbox () =
  let* ctxt = new_context () in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = Raw_level_repr.of_int32_exn 22l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.Internal_for_tests.refine_stake
       ctxt
       rollup
       staker
       commitment)
    Sc_rollup_errors.Sc_rollup_bad_inbox_level

let test_publish () =
  let* ctxt = new_context () in
  lift
  @@ let* rollup, genesis_hash, ctxt = new_sc_rollup ctxt in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let commitment =
       Commitment_repr.
         {
           predecessor = genesis_hash;
           inbox_level = valid_inbox_level ctxt 1l;
           number_of_ticks = number_of_ticks_exn 152231L;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* _node, _level, ctxt, _balance_updates =
       Sc_rollup_stake_storage.publish_commitment ctxt rollup staker commitment
     in
     assert_true ctxt

let test_publish_returns_oldest_publish_level () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 152231L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _node, level1, ctxt, _balance_updates =
    lift
    @@ Sc_rollup_stake_storage.publish_commitment ctxt rollup staker1 commitment
  in
  let current_level =
    Raw_level_repr.to_int32 (Raw_context.current_level ctxt).level
  in
  let* () =
    Assert.equal_int32
      ~loc:__LOC__
      (Raw_level_repr.to_int32 level1)
      current_level
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 10 in
  let* _node, level2, _ctxt, _balance_updates =
    lift
    @@ Sc_rollup_stake_storage.publish_commitment ctxt rollup staker2 commitment
  in
  Assert.equal_int32
    ~loc:__LOC__
    (Raw_level_repr.to_int32 level1)
    (Raw_level_repr.to_int32 level2)

let test_withdraw_and_cement () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment
  in
  let* ctxt = withdraw_stake_and_check_balances ctxt rollup staker2 in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
  in
  assert_true ctxt

let test_refine_commitment_different_stakers () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment2
  in
  assert_true ctxt

let test_refine_stake_twice_different_stakers () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _c, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment
  in
  let* ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment
  in
  assert_true ctxt

let test_refine_stake_twice_same_staker () =
  let* ctxt, rollup, genesis_hash, staker =
    originate_rollup_and_deposit_with_one_staker ()
  in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.Internal_for_tests.refine_stake
       ctxt
       rollup
       staker
       commitment)
    Sc_rollup_errors.Sc_rollup_staker_backtracked

let test_deposit_then_publish () =
  let* ctxt = new_context () in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 152231L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _node, _level, ctxt, _balance_updates =
    lift
    @@ Sc_rollup_stake_storage.publish_commitment ctxt rollup staker commitment
  in
  assert_true ctxt

let test_publish_missing_rollup () =
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let commitment ctxt =
    Commitment_repr.
      {
        predecessor = Commitment_repr.Hash.zero;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_stake_storage.publish_commitment
        ctxt
        rollup
        staker
        (commitment ctxt))

let test_cement () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
  in
  assert_true ctxt

(* Create and cement three commitments:

   [c3 -> c2 -> c1 -> Commitment_hash.zero]

   This is useful to catch potential issues with de-allocation of [c2],
   as we deallocate the old LCC when a new LCC is cemented.
*)
let test_cement_three_commitments () =
  let* ctxt, rollup, genesis_hash, staker =
    originate_rollup_and_deposit_with_one_staker ()
  in
  let level = valid_inbox_level ctxt in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c2, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment2
  in
  let commitment3 =
    Commitment_repr.
      {
        predecessor = c2;
        inbox_level = level 3l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c3, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment3
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt, commitment1' =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
  in
  let* ctxt, commitment2' =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c2
  in
  let* ctxt, commitment3' =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c3
  in
  let* () = assert_commitment_equal ~loc:__LOC__ commitment1' commitment1 in
  let* () = assert_commitment_equal ~loc:__LOC__ commitment2' commitment2 in
  let* () = assert_commitment_equal ~loc:__LOC__ commitment3' commitment3 in
  assert_true ctxt

let test_cement_then_remove () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt, _ =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.remove_staker ctxt rollup staker)
    Sc_rollup_errors.Sc_rollup_remove_lcc

let test_cement_unknown_commitment_fails () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.cement_commitment
       ctxt
       rollup
       Commitment_repr.Hash.zero)
    (Sc_rollup_errors.Sc_rollup_unknown_commitment Commitment_repr.Hash.zero)

let test_cement_with_zero_stakers_fails () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt = remove_staker_and_check_balances ctxt rollup staker in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.cement_commitment ctxt rollup c1)
    Sc_rollup_errors.Sc_rollup_no_stakers

let test_cement_fail_too_recent () =
  let* ctxt = new_context () in
  let level = valid_inbox_level ctxt in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment
  in
  let min_cementation_level = Raw_level_repr.add level challenge_window in
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.cement_commitment ctxt rollup c1)
      (Sc_rollup_errors.Sc_rollup_commitment_too_recent
         {current_level = level; min_level = min_cementation_level})
  in
  let ctxt =
    Raw_context.Internal_for_tests.add_level ctxt (challenge_window - 1)
  in
  let level = (Raw_context.current_level ctxt).level in
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.cement_commitment ctxt rollup c1)
      (Sc_rollup_errors.Sc_rollup_commitment_too_recent
         {current_level = level; min_level = min_cementation_level})
  in
  assert_true ctxt

let test_cement_deadline_uses_oldest_add_time () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in

  let* c2, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment
  in
  let* ctxt =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
  in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt c1 c2

let test_last_cemented_commitment_hash_with_level () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let inbox_level = valid_inbox_level ctxt 1l in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt, _ =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
  in
  let* c1', inbox_level', ctxt =
    lift
    @@ Sc_rollup_commitment_storage.last_cemented_commitment_hash_with_level
         ctxt
         rollup
  in
  let* () = assert_commitment_hash_equal ~loc:__LOC__ ctxt c1 c1' in
  Assert.equal_int32
    ~loc:__LOC__
    (Raw_level_repr.to_int32 inbox_level)
    (Raw_level_repr.to_int32 inbox_level')

let test_withdrawal_fails_when_not_staked_on_lcc () =
  let* ctxt = new_context () in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _node, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker)
    Sc_rollup_errors.Sc_rollup_not_staked_on_lcc

let test_genesis_info_of_rollup () =
  let* ctxt = new_context () in
  let level_before_rollup = (Raw_context.current_level ctxt).level in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 10 in
  let* _ctxt, genesis_info =
    lift @@ Sc_rollup_storage.genesis_info ctxt rollup
  in
  let initial_level = genesis_info.level in
  Assert.equal_int32
    ~loc:__LOC__
    (Raw_level_repr.to_int32 level_before_rollup)
    (Raw_level_repr.to_int32 initial_level)

let test_stake_on_existing_node () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  lift
  @@ let* _node, _level, ctxt =
       Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment
     in
     let* _node, _level, ctxt =
       Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment
     in
     assert_true ctxt

let test_cement_with_two_stakers () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  lift
  @@ let* c1, _level, ctxt =
       Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
     in
     let commitment2 =
       Commitment_repr.
         {
           predecessor = c1;
           inbox_level = level 2l;
           number_of_ticks = number_of_ticks_exn 1232909L;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* _node, _level, ctxt =
       Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment2
     in
     let challenge_window =
       Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
     in
     let ctxt =
       Raw_context.Internal_for_tests.add_level ctxt challenge_window
     in

     let* ctxt = Sc_rollup_stake_storage.cement_commitment ctxt rollup c1 in
     assert_true ctxt

let test_can_remove_staker () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _node, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment2
  in
  let* ctxt = remove_staker_and_check_balances ctxt rollup staker1 in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
  in
  assert_true ctxt

let test_can_remove_staker2 () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _node, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment2
  in
  let* ctxt = remove_staker_and_check_balances ctxt rollup staker2 in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
  in
  assert_true ctxt

let test_removed_staker_can_not_withdraw () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _node, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment2
  in
  let* ctxt, _ =
    lift @@ Sc_rollup_stake_storage.remove_staker ctxt rollup staker2
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker2)
    Sc_rollup_errors.Sc_rollup_not_staked

let test_no_cement_on_conflict () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 44L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _node, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment2
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 5000 in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.cement_commitment ctxt rollup c1)
    Sc_rollup_errors.Sc_rollup_disputed

(** Tests that [c1] can not be finalized in the following scenario:
   staker2     staker1
     |           |
     V           V
    LCC      <- [c1]
 *)
let test_no_cement_with_one_staker_at_zero_commitment () =
  let* ctxt, rollup, genesis_hash, staker1, _staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.cement_commitment ctxt rollup c1)
    Sc_rollup_errors.Sc_rollup_disputed

let test_non_cemented_parent () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c2, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment2
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.cement_commitment ctxt rollup c2)
    Sc_rollup_errors.Sc_rollup_parent_not_lcc

let test_finds_conflict_point_at_lcc () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 55L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _c2, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment2
  in
  let* (left, _right), ctxt =
    lift
    @@ Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
         ctxt
         rollup
         staker1
         staker2
  in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt left.hash c1

let test_finds_conflict_point_beneath_lcc () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c2, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment2
  in
  let commitment3 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 7373L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c3, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment3
  in
  let* (left, right), ctxt =
    lift
    @@ Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
         ctxt
         rollup
         staker1
         staker2
  in
  let* () = assert_commitment_hash_equal ~loc:__LOC__ ctxt left.hash c2 in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt right.hash c3

let test_conflict_point_is_first_point_of_disagreement () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c2, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment2
  in
  let commitment3 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 7373L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c3, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment3
  in
  let commitment4 =
    Commitment_repr.
      {
        predecessor = c2;
        inbox_level = level 3l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _c4, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment4
  in
  let* (left, right), ctxt =
    lift
    @@ Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
         ctxt
         rollup
         staker1
         staker2
  in
  let* () = assert_commitment_hash_equal ~loc:__LOC__ ctxt left.hash c2 in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt right.hash c3

let test_conflict_point_computation_fits_in_gas_limit () =
  (* Worst case of conflict point computation: two branches of maximum
     length rooted just after the LCC. *)
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let max_commits =
    let commitment_freq =
      Constants_storage.sc_rollup_commitment_period_in_blocks ctxt
    in
    Int32.div
      (Constants_storage.sc_rollup_max_lookahead_in_blocks ctxt)
      (Int32.of_int commitment_freq)
  in
  let root_commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* root_commitment_hash, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         root_commitment
  in
  let* _node, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         root_commitment
  in
  let rec branch ctxt staker_id staker predecessor i max acc =
    let commitment =
      Commitment_repr.
        {
          predecessor;
          inbox_level = level i;
          number_of_ticks = number_of_ticks_exn staker_id;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let* commitment_hash, _level, ctxt =
      lift
      @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
           ctxt
           rollup
           staker
           commitment
    in
    if i = max then
      return (Array.of_list (List.rev (commitment_hash :: acc)), ctxt)
    else
      branch
        ctxt
        staker_id
        staker
        commitment_hash
        (Int32.succ i)
        max
        (commitment_hash :: acc)
  in
  let* branch_1, ctxt =
    branch ctxt 1L staker1 root_commitment_hash 2l max_commits []
  in
  let* branch_2, ctxt =
    branch ctxt 2L staker2 root_commitment_hash 2l max_commits []
  in
  let ctxt =
    Raw_context.set_gas_limit
      ctxt
      (Constants_storage.hard_gas_limit_per_operation ctxt)
  in
  let* (left, right), ctxt =
    lift
    @@ Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
         ctxt
         rollup
         staker1
         staker2
  in
  let* () =
    assert_commitment_hash_equal ~loc:__LOC__ ctxt left.hash branch_1.(0)
  in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt right.hash branch_2.(0)

let test_no_conflict_point_one_staker_at_lcc_preboot () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
       ctxt
       rollup
       staker1
       staker2)
    Sc_rollup_errors.Sc_rollup_no_conflict

let test_no_conflict_point_both_stakers_at_lcc_preboot () =
  let* ctxt, rollup, _genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
       ctxt
       rollup
       staker1
       staker2)
    Sc_rollup_errors.Sc_rollup_no_conflict

let test_no_conflict_point_one_staker_at_lcc () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _node, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment2
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt, _ =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
       ctxt
       rollup
       staker1
       staker2)
    Sc_rollup_errors.Sc_rollup_no_conflict

let test_no_conflict_point_both_stakers_at_lcc () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let* _node, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment1
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt, _ =
    lift @@ Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
       ctxt
       rollup
       staker1
       staker2)
    Sc_rollup_errors.Sc_rollup_no_conflict

let test_staker_cannot_backtrack () =
  let* ctxt = new_context () in
  let* rollup, genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment2
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.Internal_for_tests.refine_stake
       ctxt
       rollup
       staker
       commitment1)
    Sc_rollup_errors.Sc_rollup_staker_backtracked

let test_staker_cannot_change_branch () =
  let* ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment1
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c2, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment2
  in
  let commitment3 =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 7373L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in

  let* _c3, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker2
         commitment3
  in
  let commitment4 =
    Commitment_repr.
      {
        predecessor = c2;
        inbox_level = level 3l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* _c4, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker1
         commitment4
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_stake_storage.Internal_for_tests.refine_stake
       ctxt
       rollup
       staker2
       commitment4)
    Sc_rollup_errors.Sc_rollup_staker_backtracked

let test_kind_of_missing_rollup () =
  let rollup = Sc_rollup_repr.Address.hash_bytes [] in
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt _ ->
      Sc_rollup_storage.kind ctxt rollup)

let test_refine_stake_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_stake_storage.Internal_for_tests.refine_stake
        ctxt
        rollup
        Sc_rollup_repr.Staker.zero
        ~staked_on:Sc_rollup_commitment_repr.Hash.zero
        Commitment_repr.
          {
            predecessor = Commitment_repr.Hash.zero;
            inbox_level = valid_inbox_level ctxt 1l;
            number_of_ticks = number_of_ticks_exn 1232909L;
            compressed_state = Sc_rollup_repr.State_hash.zero;
          })

let test_last_cemented_commitment_of_missing_rollup () =
  assert_fails_with_missing_rollup
    ~loc:__LOC__
    Sc_rollup_commitment_storage.last_cemented_commitment

let test_last_cemented_commitment_hash_with_level_of_missing_rollup () =
  assert_fails_with_missing_rollup
    ~loc:__LOC__
    Sc_rollup_commitment_storage.last_cemented_commitment_hash_with_level

let test_cement_commitment_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_stake_storage.cement_commitment
        ctxt
        rollup
        Commitment_repr.Hash.zero)

let test_get_conflict_point_on_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
        ctxt
        rollup
        Sc_rollup_repr.Staker.zero
        Sc_rollup_repr.Staker.zero)

let test_get_commitment_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_commitment_storage.get_commitment
        ctxt
        rollup
        Commitment_repr.Hash.zero)

let test_get_missing_commitment () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let commitment_hash = Commitment_repr.Hash.zero in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_commitment_storage.get_commitment ctxt rollup commitment_hash)
    (Sc_rollup_errors.Sc_rollup_unknown_commitment commitment_hash)

let test_remove_staker_from_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_stake_storage.remove_staker
        ctxt
        rollup
        Sc_rollup_repr.Staker.zero)

let test_genesis_info_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ Sc_rollup_storage.genesis_info

let test_concurrent_refinement_point_of_conflict () =
  let* before_ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let level = valid_inbox_level before_ctxt in
  let commitment1 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let commitment2 =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 7373L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, c2), _ctxt =
    lift
    @@ let* _c1, _level, ctxt =
         Sc_rollup_stake_storage.Internal_for_tests.refine_stake
           before_ctxt
           rollup
           staker1
           commitment1
       in
       let* _c2, _level, ctxt =
         Sc_rollup_stake_storage.Internal_for_tests.refine_stake
           ctxt
           rollup
           staker2
           commitment2
       in
       Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
         ctxt
         rollup
         staker1
         staker2
  in
  let* (c1', c2'), ctxt =
    lift
    @@ let* _c2, _level, ctxt =
         Sc_rollup_stake_storage.Internal_for_tests.refine_stake
           before_ctxt
           rollup
           staker2
           commitment2
       in
       let* _c1, _level, ctxt =
         Sc_rollup_stake_storage.Internal_for_tests.refine_stake
           ctxt
           rollup
           staker1
           commitment1
       in
       Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
         ctxt
         rollup
         staker1
         staker2
  in
  let* () = assert_commitment_hash_equal ~loc:__LOC__ ctxt c1.hash c1'.hash in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt c2.hash c2'.hash

let test_concurrent_refinement_cement () =
  let* before_ctxt, rollup, genesis_hash, staker1, staker2 =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = valid_inbox_level before_ctxt 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _ctxt =
    lift
    @@ let* c1, _level, ctxt =
         Sc_rollup_stake_storage.Internal_for_tests.refine_stake
           before_ctxt
           rollup
           staker1
           commitment
       in
       let* _c2, _level, ctxt =
         Sc_rollup_stake_storage.Internal_for_tests.refine_stake
           ctxt
           rollup
           staker2
           commitment
       in
       let challenge_window =
         Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
       in
       let ctxt =
         Raw_context.Internal_for_tests.add_level ctxt challenge_window
       in
       let* ctxt, _ =
         Sc_rollup_stake_storage.cement_commitment ctxt rollup c1
       in
       Sc_rollup_commitment_storage.last_cemented_commitment ctxt rollup
  in
  let* c2, ctxt =
    lift
    @@ let* c2, _level, ctxt =
         Sc_rollup_stake_storage.Internal_for_tests.refine_stake
           before_ctxt
           rollup
           staker2
           commitment
       in
       let* _c1, _level, ctxt =
         Sc_rollup_stake_storage.Internal_for_tests.refine_stake
           ctxt
           rollup
           staker1
           commitment
       in
       let challenge_window =
         Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
       in
       let ctxt =
         Raw_context.Internal_for_tests.add_level ctxt challenge_window
       in
       let* ctxt, _ =
         Sc_rollup_stake_storage.cement_commitment ctxt rollup c2
       in
       Sc_rollup_commitment_storage.last_cemented_commitment ctxt rollup
  in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt c1 c2

let test_zero_tick_commitment_cannot_change_state () =
  let* ctxt, rollup, genesis_hash, staker =
    originate_rollup_and_deposit_with_one_staker ()
  in
  let level = valid_inbox_level ctxt in
  let commitment =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level 1l;
        number_of_ticks = number_of_ticks_exn 1232909L;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* c1, _level, ctxt =
    lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment
  in
  let commitment =
    Commitment_repr.
      {
        predecessor = c1;
        inbox_level = level 2l;
        number_of_ticks = number_of_ticks_exn 0L;
        compressed_state =
          Sc_rollup_repr.State_hash.context_hash_to_state_hash
            (Context_hash.hash_string ["wxyz"]);
      }
  in
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         staker
         commitment)
      Sc_rollup_errors.Sc_rollup_state_change_on_zero_tick_commitment
  in
  assert_true ctxt

let check_gas_consumed ~since ~until =
  let open Raw_context in
  let as_cost = Gas_limit_repr.cost_of_gas @@ gas_consumed ~since ~until in
  Saturation_repr.to_int as_cost

let test_limit_on_number_of_messages_during_commitment_period with_gap () =
  let* ctxt = new_context () in
  let* _rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let commitment_period =
    Constants_storage.sc_rollup_commitment_period_in_blocks ctxt
  in
  let max_number =
    Constants_storage.sc_rollup_max_number_of_messages_per_commitment_period
      ctxt
  in
  let*? payload =
    List.init
      ~when_negative_length:[]
      (1 + (max_number / (commitment_period - 1)))
    @@ fun _ -> "a"
  in
  let*! add_too_many_messages =
    List.fold_left_es
      (fun ctxt i ->
        let ctxt =
          if with_gap && i = commitment_period / 2 then
            Raw_context.Internal_for_tests.add_level ctxt commitment_period
          else ctxt
        in
        let* _inbox, _size_diff, ctxt =
          lift @@ Sc_rollup_inbox_storage.add_external_messages ctxt payload
        in
        return ctxt)
      ctxt
      (1 -- (commitment_period - 1))
  in
  if with_gap then
    (* Changing the commitment period is enough to accept that many messages... *)
    let*? (_r : Raw_context.t) = add_too_many_messages in
    return ()
  else
    (* ... but if we stay in the same commitment period, it fails. *)
    Assert.proto_error ~loc:__LOC__ add_too_many_messages @@ function
    | Sc_rollup_errors
      .Sc_rollup_max_number_of_messages_reached_for_commitment_period ->
        true
    | _ -> false

let record ctxt rollup level message_index =
  Sc_rollup_outbox_storage.record_applied_message
    ctxt
    rollup
    (Raw_level_repr.of_int32_exn @@ Int32.of_int level)
    ~message_index

(* Recreating the indexing logic to make sure messages are applied. *)
let assert_is_already_applied ~loc ctxt rollup level message_index =
  let level = Raw_level_repr.of_int32_exn (Int32.of_int level) in
  let level_index =
    let max_active_levels =
      Constants_storage.sc_rollup_max_active_outbox_levels ctxt
    in
    Int32.rem (Raw_level_repr.to_int32 level) max_active_levels
  in
  let* _ctxt, level_and_bitset_opt =
    lift
    @@ Storage.Sc_rollup.Applied_outbox_messages.find (ctxt, rollup) level_index
  in
  match level_and_bitset_opt with
  | Some (existing_level, bitset) when Raw_level_repr.(existing_level = level)
    ->
      let*? is_set =
        Environment.wrap_tzresult @@ Bitset.mem bitset message_index
      in
      Assert.equal_bool ~loc is_set true
  | _ -> Stdlib.failwith "Expected a bitset and a matching level."

(** Test outbox for applied messages. *)
let test_storage_outbox () =
  let* ctxt = new_context () in
  let* rollup1, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let level1 = 100 in
  (* Test that is-applied is false for non-recorded messages. *)
  let* _size_diff, ctxt = lift @@ record ctxt rollup1 level1 1 in
  let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup1 level1 1 in
  (* Record the same level and message twice should fail. *)
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (record ctxt rollup1 level1 1)
      Sc_rollup_errors.Sc_rollup_outbox_message_already_applied
  in
  let* _size_diff, ctxt = lift @@ record ctxt rollup1 level1 2 in
  let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup1 level1 2 in
  (* Record messages for new level. *)
  let level2 = level1 + 3 in
  let* _size_diff, ctxt = lift @@ record ctxt rollup1 level2 47 in
  let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup1 level2 47 in
  let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup1 level1 1 in
  (* Record for a new rollup. *)
  let* rollup2, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let* _size_diff, ctxt = lift @@ record ctxt rollup2 level1 1 in
  let* _size_diff, ctxt = lift @@ record ctxt rollup2 level1 3 in
  let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup2 level1 1 in
  let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup2 level1 3 in
  assert_is_already_applied ~loc:__LOC__ ctxt rollup1 level1 1

(** Test limits for applied outbox messages. *)
let test_storage_outbox_exceed_limits () =
  let level = 1234 in
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  (* Assert that recording a message index that exceeds max outbox messages per
     level fails. *)
  let* () =
    let max_message_index =
      Constants_storage.sc_rollup_max_outbox_messages_per_level ctxt
    in
    assert_fails_with
      ~loc:__LOC__
      (record ctxt rollup level max_message_index)
      Sc_rollup_errors.Sc_rollup_invalid_outbox_message_index
  in
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (record ctxt rollup level (-1))
      Sc_rollup_errors.Sc_rollup_invalid_outbox_message_index
  in
  let max_active_levels =
    Int32.to_int @@ Constants_storage.sc_rollup_max_active_outbox_levels ctxt
  in
  (* Record message 42 at level 15 *)
  let* _size_diff, ctxt = lift @@ record ctxt rollup 15 42 in
  let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup 15 42 in
  (* Record message 42 at level [max_active_levels + 15] *)
  let* _size_diff, ctxt =
    lift @@ record ctxt rollup (max_active_levels + 15) 42
  in
  (* Record message 42 at level 15 again should fail as it's expired. *)
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (record ctxt rollup 15 42)
      Sc_rollup_errors.Sc_rollup_outbox_level_expired
  in
  return ()

(** Test storage outbox size diff. Note that these tests depend on the constant.
    [sc_rollup_max_outbox_messages_per_level] which controls the maximum size
    of bitsets required to store applied messages per level.

    Here's a breakdown of the size for applied-outbox-messages storage:
    - [size_of_level = 4]
    - [max_size_per_level < (2 * (sc_rollup_max_outbox_messages_per_level / 8))]
    - [max_size_per_level < size_of_level + size_of_bitset]
    - [total_size < sc_rollup_max_active_outbox_levels * max_size_per_level]
  *)
let test_storage_outbox_size_diff () =
  (* This is the maximum additional storage space required to store one message.
     It depends on [sc_rollup_max_outbox_messages_per_level]. *)
  let max_size_diff = 19 in
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let level = 15 in
  let max_message_index =
    Constants_storage.sc_rollup_max_outbox_messages_per_level ctxt - 1
  in
  let max_active_levels =
    Int32.to_int @@ Constants_storage.sc_rollup_max_active_outbox_levels ctxt
  in
  (* Record a new message. *)
  let* size_diff, ctxt = lift @@ record ctxt rollup level 1 in
  (* Size diff is 11 bytes. 4 bytes for level and 7 bytes for a new Z.t *)
  let* () = Assert.equal_int ~loc:__LOC__ (Z.to_int size_diff) 5 in
  let* size_diff, ctxt = lift @@ record ctxt rollup level 2 in
  (* Recording a new message in the bitset at a lower index does not occupy
     any additional space. *)
  let* () = Assert.equal_int ~loc:__LOC__ (Z.to_int size_diff) 0 in
  (* Record a new message at the highest index at an existing level. This
     expands the bitset but does not charge for the level. *)
  let* size_diff, ctxt = lift @@ record ctxt rollup level max_message_index in
  let* () = Assert.equal_int ~loc:__LOC__ (Z.to_int size_diff) 14 in
  (* Record a new message at the highest index at a new level. This charges for
     space for level and maximum bitset. *)
  let* size_diff, ctxt =
    lift @@ record ctxt rollup (level + 1) max_message_index
  in
  let* () = Assert.equal_int ~loc:__LOC__ (Z.to_int size_diff) max_size_diff in
  (* Record a new message for a level that resets an index. This replaces the
     bitset with a smaller one. Hence we get a negative size diff. *)
  let* size_diff, _ctxt =
    lift @@ record ctxt rollup (level + max_active_levels) 0
  in
  let* () = Assert.equal_int ~loc:__LOC__ (Z.to_int size_diff) (-14) in
  return ()

let test_subscribe_slot_to_rollup () =
  let* ctxt = new_context () in
  let level = (Raw_context.current_level ctxt).level in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let slot_index = dal_slot_index_of_int_exn 0 in
  let* index, subscribed_level, _ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index
  in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      (Dal_slot_repr.Index.to_int index)
      (Dal_slot_repr.Index.to_int slot_index)
  in
  Assert.equal_int32
    ~loc:__LOC__
    (Raw_level_repr.to_int32 subscribed_level)
    (Raw_level_repr.to_int32 level)

let test_subscribe_slot_twice_at_same_level () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let slot_index = dal_slot_index_of_int_exn 0 in
  let* _index, _subscribed_level, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index)
    (Dal_errors_repr.Dal_rollup_already_registered_to_slot_index
       (rollup, slot_index))

let test_subscribe_slot_twice_at_different_levels () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let slot_index = dal_slot_index_of_int_exn 0 in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 10 in
  let* _index, _subscribed_level, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index)
    (Dal_errors_repr.Dal_rollup_already_registered_to_slot_index
       (rollup, slot_index))

let test_subscribe_different_slots_at_same_level () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let slot_0 = dal_slot_index_of_int_exn 0 in
  let slot_1 = dal_slot_index_of_int_exn 1 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_0
  in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_1
  in
  assert_true ctxt

let test_subscribe_different_slots_at_different_levels () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let slot_0 = dal_slot_index_of_int_exn 0 in
  let slot_1 = dal_slot_index_of_int_exn 1 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_0
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 10 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_1
  in
  assert_true ctxt

let test_subscribe_slot_to_missing_rollup () =
  let slot_index = dal_slot_index_of_int_exn 0 in
  assert_fails_with_missing_rollup
    ~loc:__LOC__
    (Sc_rollup_storage.Dal_slot.subscribe ~slot_index)

let test_subscribe_to_slot_with_index_out_of_bounds () =
  let* ctxt = new_context () in
  (* Ensure that the maximum number of slots is not above the hard limit *)
  let number_of_slots =
    Dal_slot_repr.Index.to_int Dal_slot_repr.Index.max_value
  in
  let* ctxt =
    Lwt.map (fun ctxt -> Ok ctxt)
    @@ Raw_context.patch_constants ctxt (fun constants ->
           {constants with dal = {constants.dal with number_of_slots}})
  in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let out_of_bounds_index = dal_slot_index_of_int_exn number_of_slots in
  let maximum = dal_slot_index_of_int_exn (number_of_slots - 1) in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.Dal_slot.subscribe
       ctxt
       rollup
       ~slot_index:out_of_bounds_index)
    (Dal_errors_repr.Dal_subscribe_rollup_invalid_slot_index
       {given = out_of_bounds_index; maximum})

let test_subscribed_slots_no_entries () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let current_level = (Raw_context.current_level ctxt).level in
  let* subscribed_slots =
    lift
    @@ Sc_rollup_storage.Dal_slot.subscribed_slot_indices
         ctxt
         rollup
         current_level
  in
  Assert.assert_equal_list
    ~loc:__LOC__
    (fun lhs rhs ->
      Dal_slot_repr.Index.to_int lhs = Dal_slot_repr.Index.to_int rhs)
    "Unexpected slot indices"
    Dal_slot_repr.Index.pp
    subscribed_slots
    []

let test_subscribed_slots_returns_overall_slot_subscriptions () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let slot_0 = dal_slot_index_of_int_exn 0 in
  let slot_1 = dal_slot_index_of_int_exn 1 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_0
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 1 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_1
  in
  let current_level = (Raw_context.current_level ctxt).level in
  let* subscribed_slots =
    lift
    @@ Sc_rollup_storage.Dal_slot.subscribed_slot_indices
         ctxt
         rollup
         current_level
  in
  Assert.assert_equal_list
    ~loc:__LOC__
    (fun lhs rhs ->
      Dal_slot_repr.Index.to_int lhs = Dal_slot_repr.Index.to_int rhs)
    "Unexpected slot indices"
    Dal_slot_repr.Index.pp
    subscribed_slots
    [slot_0; slot_1]

let test_subscribed_slots_no_entry_at_current_level () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let slot_0 = dal_slot_index_of_int_exn 0 in
  let slot_1 = dal_slot_index_of_int_exn 1 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_0
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 1 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_1
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 1 in
  let current_level = (Raw_context.current_level ctxt).level in
  let* subscribed_slots =
    lift
    @@ Sc_rollup_storage.Dal_slot.subscribed_slot_indices
         ctxt
         rollup
         current_level
  in
  Assert.assert_equal_list
    ~loc:__LOC__
    (fun lhs rhs ->
      Dal_slot_repr.Index.to_int lhs = Dal_slot_repr.Index.to_int rhs)
    "Unexpected slot indices"
    Dal_slot_repr.Index.pp
    subscribed_slots
    [slot_0; slot_1]

let test_subscribed_slots_at_level_past_context_level () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let slot_0 = dal_slot_index_of_int_exn 0 in
  let slot_1 = dal_slot_index_of_int_exn 1 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_0
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 1 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_1
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 1 in
  let current_level = (Raw_context.current_level ctxt).level in
  let*? future_level =
    Environment.wrap_tzresult @@ Raw_level_repr.of_int32
    @@ Int32.succ (Raw_level_repr.to_int32 current_level)
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.Dal_slot.subscribed_slot_indices
       ctxt
       rollup
       future_level)
    (Dal_errors_repr.Dal_requested_subscriptions_at_future_level
       (current_level, future_level))

let test_subscribed_slots_entries_at_future_level () =
  let* ctxt = new_context () in
  let* rollup, _genesis_hash, ctxt = lift @@ new_sc_rollup ctxt in
  let slot_0 = dal_slot_index_of_int_exn 0 in
  let slot_1 = dal_slot_index_of_int_exn 1 in
  let slot_2 = dal_slot_index_of_int_exn 2 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_0
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 1 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_1
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 1 in
  let query_level = (Raw_context.current_level ctxt).level in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 1 in
  let* _, _, ctxt =
    lift @@ Sc_rollup_storage.Dal_slot.subscribe ctxt rollup ~slot_index:slot_2
  in
  let* subscribed_slots =
    lift
    @@ Sc_rollup_storage.Dal_slot.subscribed_slot_indices
         ctxt
         rollup
         query_level
  in
  Assert.assert_equal_list
    ~loc:__LOC__
    (fun lhs rhs ->
      Dal_slot_repr.Index.to_int lhs = Dal_slot_repr.Index.to_int rhs)
    "Unexpected slot indices"
    Dal_slot_repr.Index.pp
    subscribed_slots
    [slot_0; slot_1]

let test_subscribed_slots_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_storage.Dal_slot.subscribed_slot_indices
        ctxt
        rollup
        (Raw_context.current_level ctxt).level)

let test_get_cemented_commitments_with_levels_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_commitment_storage.Internal_for_tests
      .get_cemented_commitments_with_levels
        ctxt
        rollup)

let test_get_cemented_commitments_with_levels () =
  let* ctxt, rollup, c0, staker =
    originate_rollup_and_deposit_with_one_staker ()
  in
  let level = valid_inbox_level ctxt in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  (* Produce and stake on n commitments, each on top of the other. *)
  (* Fetch number of stored commitments in context. *)
  let max_num_stored_cemented_commitments =
    (Raw_context.constants ctxt).sc_rollup
      .max_number_of_stored_cemented_commitments
  in
  (* Produce and stake more commitments than the number of cemented
     commitments that can be stored. *)
  let number_of_commitments = max_num_stored_cemented_commitments + 5 in
  let* commitments, ctxt =
    lift
    @@ produce_and_refine
         ~number_of_commitments
         ~predecessor:c0
         ctxt
         staker
         rollup
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  (* Cement all commitments that have been produced. *)
  let* ctxt = lift @@ cement_commitments ctxt commitments rollup in
  (* Add genesis commitment to list of produced commitments. *)
  let commitments = c0 :: commitments in
  let number_of_cemented_commitments = List.length commitments in
  (* Fetch cemented commitments that are kept in context. *)
  let* cemented_commitments_with_levels, _ctxt =
    lift
    @@ Sc_rollup_commitment_storage.Internal_for_tests
       .get_cemented_commitments_with_levels
         ctxt
         rollup
  in
  (* Check that only ctxt.sc_rollup.max_number_of_stored_cemented_commitments
     are kept in context. *)
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      (List.length cemented_commitments_with_levels)
      max_num_stored_cemented_commitments
  in
  (* Check that the commitments that are kept in context are the
     last [ctxt.sc_rollup.max_number_of_stored_cemented_commitments].
     commitments that have been cemented. *)
  let dropped_commitments =
    number_of_cemented_commitments - max_num_stored_cemented_commitments
  in
  let expected_commitments_with_levels =
    commitments
    |> List.drop_n dropped_commitments
    |> List.mapi (fun i c ->
           (c, level @@ Int32.of_int (i + dropped_commitments)))
  in
  assert_commitments_with_levels_equal
    ~loc:__LOC__
    cemented_commitments_with_levels
    expected_commitments_with_levels

let tests =
  [
    Tztest.tztest
      "deposit to missing rollup fails"
      `Quick
      test_deposit_to_missing_rollup;
    Tztest.tztest
      "deposit by underfunded staker"
      `Quick
      test_deposit_by_underfunded_staker;
    Tztest.tztest
      "deposit to existing rollup"
      `Quick
      test_deposit_to_existing_rollup;
    Tztest.tztest "deposit, then withdraw" `Quick test_deposit_then_withdraw;
    Tztest.tztest
      "cement with zero stakers fails"
      `Quick
      test_cement_with_zero_stakers_fails;
    Tztest.tztest
      "withdrawing when not staked fails"
      `Quick
      test_withdraw_when_not_staked;
    Tztest.tztest "withdrawing twice fails" `Quick test_withdrawing_twice;
    Tztest.tztest "stake on new node" `Quick test_deposit_then_refine;
    Tztest.tztest
      "Do not refine with wrong inbox level"
      `Quick
      test_deposit_then_refine_bad_inbox;
    Tztest.tztest "stake on existing node" `Quick test_stake_on_existing_node;
    Tztest.tztest "publish commitment" `Quick test_publish;
    Tztest.tztest
      "publish commitment returns level when commitment was first published"
      `Quick
      test_publish_returns_oldest_publish_level;
    Tztest.tztest
      "withdraw stake of another staker before cementing"
      `Quick
      test_withdraw_and_cement;
    Tztest.tztest
      "refine a commitment published by another staker is allowed"
      `Quick
      test_refine_stake_twice_different_stakers;
    Tztest.tztest
      "Different stakers staking on same commitment is allowed"
      `Quick
      test_refine_commitment_different_stakers;
    Tztest.tztest
      "staking twice on same commitment from same staker is not allowed"
      `Quick
      test_refine_stake_twice_same_staker;
    Tztest.tztest "stake then publish" `Quick test_deposit_then_publish;
    Tztest.tztest "publish with no rollup" `Quick test_publish_missing_rollup;
    Tztest.tztest
      "withdrawal from missing rollup fails"
      `Quick
      test_withdrawal_from_missing_rollup;
    Tztest.tztest
      "withdrawal fails when not staked on LCC"
      `Quick
      test_withdrawal_fails_when_not_staked_on_lcc;
    Tztest.tztest
      "initial_level returns correct level"
      `Quick
      test_genesis_info_of_rollup;
    Tztest.tztest
      "rollup starts in pre-boot state"
      `Quick
      test_initial_state_is_pre_boot;
    Tztest.tztest "cement" `Quick test_cement;
    Tztest.tztest
      "cement three commitments"
      `Quick
      test_cement_three_commitments;
    Tztest.tztest "cannot unstake staker at LCC" `Quick test_cement_then_remove;
    Tztest.tztest
      "cement unknown commitment fails"
      `Quick
      test_cement_unknown_commitment_fails;
    Tztest.tztest
      "cement fails when too recent"
      `Quick
      test_cement_fail_too_recent;
    Tztest.tztest
      "cement deadline uses oldest add time"
      `Quick
      test_cement_deadline_uses_oldest_add_time;
    Tztest.tztest
      "last cemented commitment hash and level returns correct information"
      `Quick
      test_last_cemented_commitment_hash_with_level;
    Tztest.tztest "cement with two stakers" `Quick test_cement_with_two_stakers;
    Tztest.tztest "no cement on conflict" `Quick test_no_cement_on_conflict;
    Tztest.tztest
      "refuse cementing when one staker is at zero commitment"
      `Quick
      test_no_cement_with_one_staker_at_zero_commitment;
    Tztest.tztest
      "refuse cementing when parent commitment is not the LCC"
      `Quick
      test_non_cemented_parent;
    Tztest.tztest
      "finds conflict point at LCC"
      `Quick
      test_finds_conflict_point_at_lcc;
    Tztest.tztest
      "finds conflict point beneath LCC"
      `Quick
      test_finds_conflict_point_beneath_lcc;
    Tztest.tztest
      "finds first point of disagreement when as point of conflict"
      `Quick
      test_conflict_point_is_first_point_of_disagreement;
    Tztest.tztest
      "finds no conflict point with two stakers, one of which is at LCC (PVM \
       in preboot)"
      `Quick
      test_no_conflict_point_one_staker_at_lcc_preboot;
    Tztest.tztest
      "finds no conflict point when both stakers commit to LCC (PVM in preboot)"
      `Quick
      test_no_conflict_point_both_stakers_at_lcc_preboot;
    Tztest.tztest
      "finds no conflict point with two stakers, one of which is at LCC"
      `Quick
      test_no_conflict_point_one_staker_at_lcc;
    Tztest.tztest
      "finds no conflict point when both stakers commit to LCC"
      `Quick
      test_no_conflict_point_both_stakers_at_lcc;
    Tztest.tztest
      "test_conflict_point_computation_fits_in_gas_limit"
      `Quick
      test_conflict_point_computation_fits_in_gas_limit;
    Tztest.tztest "staker cannot backtrack" `Quick test_staker_cannot_backtrack;
    Tztest.tztest
      "staker cannot change branch"
      `Quick
      test_staker_cannot_change_branch;
    Tztest.tztest "can remove staker 1" `Quick test_can_remove_staker;
    Tztest.tztest "can remove staker 2" `Quick test_can_remove_staker2;
    Tztest.tztest
      "removed staker can not withdraw"
      `Quick
      test_removed_staker_can_not_withdraw;
    Tztest.tztest
      "removing staker from the LCC fails"
      `Quick
      test_removing_staker_from_lcc_fails;
    Tztest.tztest
      "kind of missing rollup is None"
      `Quick
      test_kind_of_missing_rollup;
    Tztest.tztest
      "refining stake of missing rollup fails"
      `Quick
      test_refine_stake_of_missing_rollup;
    Tztest.tztest
      "fetching last final commitment of missing rollup fails"
      `Quick
      test_last_cemented_commitment_of_missing_rollup;
    Tztest.tztest
      "fetching last final commitment hash and level of missing rollup fails"
      `Quick
      test_last_cemented_commitment_hash_with_level_of_missing_rollup;
    Tztest.tztest
      "Finalizing commitment of missing rollup fails"
      `Quick
      test_cement_commitment_of_missing_rollup;
    Tztest.tztest
      "fetching conflict point of missing rollup fails"
      `Quick
      test_get_conflict_point_on_missing_rollup;
    Tztest.tztest
      "fetching commitment of missing rollup fails"
      `Quick
      test_get_commitment_of_missing_rollup;
    Tztest.tztest
      "fetching non-existing commitment of rollup fails"
      `Quick
      test_get_missing_commitment;
    Tztest.tztest
      "removing staker from missing rollup fails"
      `Quick
      test_remove_staker_from_missing_rollup;
    Tztest.tztest
      "initial level of missing rollup fails"
      `Quick
      test_genesis_info_of_missing_rollup;
    Tztest.tztest
      "Refinement operations are commutative (point of conflict)"
      `Quick
      test_concurrent_refinement_point_of_conflict;
    Tztest.tztest
      "Refinement operations are commutative (cement)"
      `Quick
      test_concurrent_refinement_cement;
    Tztest.tztest
      "A commitment with zero ticks shouldn't change the state"
      `Quick
      test_zero_tick_commitment_cannot_change_state;
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3978

       The number of messages during commitment period is broken with the
       unique inbox. *)
    (* Tztest.tztest
     *   "The number of messages pushed during commitment period stays under \
     *    limit (without gap)"
     *   `Quick
     *   (test_limit_on_number_of_messages_during_commitment_period false);
     * Tztest.tztest
     *   "The number of messages pushed during commitment period stays under \
     *    limit (with gap)"
     *   `Quick
     *   (test_limit_on_number_of_messages_during_commitment_period true);
     * Tztest.tztest "Record messages in storage outbox" `Quick test_storage_outbox; *)
    Tztest.tztest
      "Record messages in storage outbox limits"
      `Quick
      test_storage_outbox_exceed_limits;
    Tztest.tztest
      "Record messages size diffs"
      `Quick
      test_storage_outbox_size_diff;
    Tztest.tztest
      "Slot subscription to rollup is successful"
      `Quick
      test_subscribe_slot_to_rollup;
    Tztest.tztest
      "Subscribing twice to same slot at the same level fails"
      `Quick
      test_subscribe_slot_twice_at_same_level;
    Tztest.tztest
      "Subscribing to same slot at different levels fails"
      `Quick
      test_subscribe_slot_twice_at_different_levels;
    Tztest.tztest
      "Subscribe to different slots at same level is allowed"
      `Quick
      test_subscribe_different_slots_at_same_level;
    Tztest.tztest
      "Subscribe to different slots at different level is allowed"
      `Quick
      test_subscribe_different_slots_at_different_levels;
    Tztest.tztest
      "Subscribe missing rollup to slot fails"
      `Quick
      test_subscribe_slot_to_missing_rollup;
    Tztest.tztest
      "Subscribe to nonexisting slot fails"
      `Quick
      test_subscribe_to_slot_with_index_out_of_bounds;
    Tztest.tztest
      "Fetching slot subscriptions when there are no entries returns the empty \
       list"
      `Quick
      test_subscribed_slots_no_entries;
    Tztest.tztest
      "Fetching slot subscriptions returns overall slot subscriptions"
      `Quick
      test_subscribed_slots_returns_overall_slot_subscriptions;
    Tztest.tztest
      "Fetching slot subscriptions returns information for most recent entry"
      `Quick
      test_subscribed_slots_no_entry_at_current_level;
    Tztest.tztest
      "Fetching slot subscriptions at level higher than current level fails"
      `Quick
      test_subscribed_slots_at_level_past_context_level;
    Tztest.tztest
      "Fetching slot subscriptions ignores entries at future levels"
      `Quick
      test_subscribed_slots_entries_at_future_level;
    Tztest.tztest
      "Fetching slot subscriptions of missing rollup fails"
      `Quick
      test_subscribed_slots_of_missing_rollup;
    Tztest.tztest
      "Originating a rollup creates a genesis commitment"
      `Quick
      test_last_cemented_commitment_hash_with_level_when_genesis;
    Tztest.tztest
      "Getting cemented commitments with levels of missing rollups fails"
      `Quick
      test_get_cemented_commitments_with_levels_of_missing_rollup;
    Tztest.tztest
      "Getting cemented commitments returns multiple cemented commitments"
      `Quick
      test_get_cemented_commitments_with_levels;
  ]

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2460
   Further tests to be added.
*)
