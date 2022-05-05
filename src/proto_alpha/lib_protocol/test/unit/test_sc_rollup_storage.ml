(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** Lift a computation using using environment errors to use shell errors. *)
let lift k = Lwt.map Environment.wrap_tzresult k

let new_context () =
  let* (b, _contract) = Context.init1 () in
  Incremental.begin_construction b >|=? fun inc ->
  let state = Incremental.validation_state inc in
  let ctxt = state.ctxt in
  (* Necessary to originate rollups. *)
  let ctxt = Alpha_context.Origination_nonce.init ctxt Operation_hash.zero in
  Alpha_context.Internal_for_tests.to_raw ctxt

let new_sc_rollup ctxt =
  let+ (rollup, _size, ctxt) =
    Sc_rollup_storage.originate ctxt ~kind:Example_arith ~boot_sector:""
  in
  (rollup, ctxt)

(** Originate a rollup with one staker and make a deposit to the initial LCC *)
let originate_rollup_and_deposit_with_one_staker () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let+ ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  (ctxt, rollup, staker)

(** Originate a rollup with two stakers and make a deposit to the initial LCC *)
let originate_rollup_and_deposit_with_two_stakers () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker1 =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let staker2 =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
  let+ ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
  (ctxt, rollup, staker1, staker2)

(** Trivial assertion.

    By convention, context is passed linearly as [ctxt].  This takes a context
    argument to allow this.
*)
let assert_true _ctxt = return ()

(** Assert that the computation fails with the given message. *)
let assert_fails_with ~loc k msg =
  let expected_error_msg = "Error:\n  " ^ msg ^ "\n" in
  k >>= function
  | Ok _ -> Stdlib.failwith "Expected failure"
  | Error err ->
      let actual_error_msg : string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace err
      in
      Assert.equal_string ~loc expected_error_msg actual_error_msg

(** Assert operation fails because of missing rollup *)
let assert_fails_with_missing_rollup ~loc op =
  let* ctxt = new_context () in
  let rollup = Sc_rollup_repr.Address.hash_bytes [] in
  assert_fails_with
    ~loc
    (op ctxt rollup)
    (* Hash of empty sequence *)
    "Rollup scr1Ew52VCdi6nF1JuokRGMqfmSeiAEXymW2m does not exist"

(** Assert commitment hash equality.

    By convention, context is passed linearly as [ctxt].  This takes a context
    argument to allow this.
    *)
let assert_commitment_hash_equal ~loc _ctxt x y =
  Assert.equal
    ~loc
    Sc_rollup_repr.Commitment_hash.equal
    "Compare commitment hash"
    Sc_rollup_repr.Commitment_hash.pp
    x
    y

let assert_kinds_are_equal ~loc x y =
  Assert.equal
    ~loc
    (Option.equal Sc_rollup_repr.Kind.equal)
    "Compare optional kind"
    (Format.pp_print_option Sc_rollup_repr.Kind.pp)
    x
    y

let test_deposit_to_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_storage.deposit_stake ctxt rollup Sc_rollup_repr.Staker.zero)

let test_initial_state_is_pre_boot () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let* (lcc, ctxt) =
    lift @@ Sc_rollup_storage.last_cemented_commitment ctxt rollup
  in
  assert_commitment_hash_equal
    ~loc:__LOC__
    ctxt
    lcc
    Sc_rollup_repr.Commitment_hash.zero

let test_deposit_to_existing_rollup () =
  let* ctxt = new_context () in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker =
       Signature.Public_key_hash.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     assert_true ctxt

let test_removing_staker_from_lcc_fails () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Signature.Public_key_hash.of_b58check_exn
      "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.remove_staker ctxt rollup staker)
    "Can not remove a cemented commitment."

let test_deposit_then_withdraw () =
  let* ctxt = new_context () in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker =
       Signature.Public_key_hash.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     let* ctxt = Sc_rollup_storage.withdraw_stake ctxt rollup staker in
     assert_true ctxt

let test_can_not_stake_twice () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Signature.Public_key_hash.of_b58check_exn
      "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.deposit_stake ctxt rollup staker)
    "Already staked."

let test_withdrawal_from_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_storage.withdraw_stake ctxt rollup Sc_rollup_repr.Staker.zero)

let test_withdraw_when_not_staked () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Signature.Public_key_hash.of_b58check_exn
      "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.withdraw_stake ctxt rollup staker)
    "Unknown staker."

let test_withdrawing_twice () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Signature.Public_key_hash.of_b58check_exn
      "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let* ctxt = lift @@ Sc_rollup_storage.withdraw_stake ctxt rollup staker in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.withdraw_stake ctxt rollup staker)
    "Unknown staker."

let number_of_messages_exn n =
  match Sc_rollup_repr.Number_of_messages.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_messages"

let number_of_ticks_exn n =
  match Sc_rollup_repr.Number_of_ticks.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_ticks"

let test_deposit_then_refine () =
  let* ctxt = new_context () in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     let commitment =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 21l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup staker commitment
     in
     assert_true ctxt

let test_deposit_then_refine_bad_inbox () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 22l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.refine_stake ctxt rollup staker commitment)
    "Attempted to commit to a bad inbox level."

let test_publish () =
  let* ctxt = new_context () in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let commitment =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 21l;
           number_of_messages = number_of_messages_exn 5l;
           number_of_ticks = number_of_ticks_exn 152231l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.publish_commitment ctxt rollup staker commitment
     in
     assert_true ctxt

let test_deposit_then_publish () =
  let* ctxt = new_context () in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     let commitment =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 21l;
           number_of_messages = number_of_messages_exn 5l;
           number_of_ticks = number_of_ticks_exn 152231l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.publish_commitment ctxt rollup staker commitment
     in
     assert_true ctxt

let test_publish_missing_rollup () =
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_storage.publish_commitment ctxt rollup staker commitment)

let test_cement () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     let commitment =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 21l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (c1, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup staker commitment
     in
     let ctxt =
       Raw_context.Internal_for_tests.add_level ctxt challenge_window
     in
     let* ctxt = Sc_rollup_storage.cement_commitment ctxt rollup c1 in
     assert_true ctxt

(* Create and cement three commitments:

   [c3 -> c2 -> c1 -> Commitment_hash.zero]

   This is useful to catch potential issues with de-allocation of [c2],
   as we deallocate the old LCC when a new LCC is cemented.
 *)
let test_cement_three_commitments () =
  let* (ctxt, rollup, staker) =
    originate_rollup_and_deposit_with_one_staker ()
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  lift
  @@
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    Sc_rollup_storage.refine_stake ctxt rollup staker commitment
  in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c2, ctxt) =
    Sc_rollup_storage.refine_stake ctxt rollup staker commitment
  in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c2;
        inbox_level = Raw_level_repr.of_int32_exn 61l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c3, ctxt) =
    Sc_rollup_storage.refine_stake ctxt rollup staker commitment
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt = Sc_rollup_storage.cement_commitment ctxt rollup c1 in
  let* ctxt = Sc_rollup_storage.cement_commitment ctxt rollup c2 in
  let* ctxt = Sc_rollup_storage.cement_commitment ctxt rollup c3 in
  assert_true ctxt

let test_cement_then_remove () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker commitment
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt = lift @@ Sc_rollup_storage.cement_commitment ctxt rollup c1 in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.remove_staker ctxt rollup staker)
    "Can not remove a cemented commitment."

let test_cement_consumes_available_messages () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let* (inbox, _n, ctxt) =
    lift @@ Sc_rollup_storage.add_messages ctxt rollup ["one"; "two"; "three"]
  in
  let available_messages =
    Sc_rollup_inbox_repr.number_of_available_messages inbox
  in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 1l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker commitment
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt = lift @@ Sc_rollup_storage.cement_commitment ctxt rollup c1 in
  let* (new_inbox, _ctxt) = lift @@ Sc_rollup_storage.inbox ctxt rollup in
  let new_available_messages =
    Sc_rollup_inbox_repr.number_of_available_messages new_inbox
  in
  let consumed_messages =
    Z.of_int32
    @@ Sc_rollup_repr.Number_of_messages.to_int32 commitment.number_of_messages
  in
  Assert.equal
    ~loc:__LOC__
    Z.equal
    "Compare consumed messages"
    Z.pp_print
    Z.(available_messages - new_available_messages)
    consumed_messages

let test_cement_unknown_commitment_fails () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.cement_commitment
       ctxt
       rollup
       Sc_rollup_repr.Commitment_hash.zero)
    "Commitment scc12XhSULdV8bAav21e99VYLTpqAjTd7NU8Mn4zFdKPSA8auMbggG does \
     not exist"

let test_cement_with_zero_stakers_fails () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker commitment
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in

  let* ctxt = lift @@ Sc_rollup_storage.remove_staker ctxt rollup staker in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.cement_commitment ctxt rollup c1)
    "No stakers."

let test_cement_fail_too_recent () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker commitment
  in
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_storage.cement_commitment ctxt rollup c1)
      "Attempted to cement a commitment before its refutation deadline."
  in
  let ctxt =
    Raw_context.Internal_for_tests.add_level ctxt (challenge_window - 1)
  in
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_storage.cement_commitment ctxt rollup c1)
      "Attempted to cement a commitment before its refutation deadline."
  in
  assert_true ctxt

let test_cement_deadline_uses_oldest_add_time () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in

  let* (c2, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment
  in
  let* ctxt = lift @@ Sc_rollup_storage.cement_commitment ctxt rollup c1 in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt c1 c2

let test_last_cemented_commitment_hash_with_level () =
  let* ctxt = new_context () in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let inbox_level = Raw_level_repr.of_int32_exn 21l in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker commitment
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt = lift @@ Sc_rollup_storage.cement_commitment ctxt rollup c1 in
  let* (c1', inbox_level', ctxt) =
    lift
    @@ Sc_rollup_storage.last_cemented_commitment_hash_with_level ctxt rollup
  in
  let* () = assert_commitment_hash_equal ~loc:__LOC__ ctxt c1 c1' in
  Assert.equal_int32
    ~loc:__LOC__
    (Raw_level_repr.to_int32 inbox_level)
    (Raw_level_repr.to_int32 inbox_level')

let test_withdrawal_fails_when_not_staked_on_lcc () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_node, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker commitment
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.withdraw_stake ctxt rollup staker)
    "Attempted to withdraw while not staked on the last cemented commitment."

let test_initial_level_of_rollup () =
  let* ctxt = new_context () in
  let level_before_rollup = (Raw_context.current_level ctxt).level in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 10 in
  let* initial_level = lift @@ Sc_rollup_storage.initial_level ctxt rollup in
  Assert.equal_int32
    ~loc:__LOC__
    (Raw_level_repr.to_int32 level_before_rollup)
    (Raw_level_repr.to_int32 initial_level)

let test_stake_on_existing_node () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  lift
  @@ let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment
     in
     assert_true ctxt

let test_cement_with_two_stakers () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  lift
  @@ let* (c1, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
     in
     let commitment2 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = c1;
           inbox_level = Raw_level_repr.of_int32_exn 41l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment2
     in
     let challenge_window =
       Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
     in
     let ctxt =
       Raw_context.Internal_for_tests.add_level ctxt challenge_window
     in

     let* ctxt = Sc_rollup_storage.cement_commitment ctxt rollup c1 in
     assert_true ctxt

let test_can_remove_staker () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  lift
  @@ let* (c1, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
     in
     let commitment2 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = c1;
           inbox_level = Raw_level_repr.of_int32_exn 41l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment2
     in
     let* ctxt = Sc_rollup_storage.remove_staker ctxt rollup staker1 in
     let challenge_window =
       Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
     in
     let ctxt =
       Raw_context.Internal_for_tests.add_level ctxt challenge_window
     in
     let* ctxt = Sc_rollup_storage.cement_commitment ctxt rollup c1 in
     assert_true ctxt

let test_can_remove_staker2 () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  lift
  @@ let* (c1, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
     in
     let commitment2 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = c1;
           inbox_level = Raw_level_repr.of_int32_exn 41l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment2
     in
     let* ctxt = Sc_rollup_storage.remove_staker ctxt rollup staker2 in
     let challenge_window =
       Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
     in
     let ctxt =
       Raw_context.Internal_for_tests.add_level ctxt challenge_window
     in

     let* ctxt = Sc_rollup_storage.cement_commitment ctxt rollup c1 in
     assert_true ctxt

let test_removed_staker_can_not_withdraw () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_node, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment2
  in
  let* ctxt = lift @@ Sc_rollup_storage.remove_staker ctxt rollup staker2 in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.withdraw_stake ctxt rollup staker2)
    "Unknown staker."

let test_no_cement_on_conflict () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 44l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_node, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment2
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt 5000 in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.cement_commitment ctxt rollup c1)
    "Attempted to cement a disputed commitment."

(** Tests that [c1] can not be finalized in the following scenario:
   staker2     staker1
     |           |
     V           V
    LCC      <- [c1]
 *)
let test_no_cement_with_one_staker_at_zero_commitment () =
  let* (ctxt, rollup, staker1, _staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.cement_commitment ctxt rollup c1)
    "Attempted to cement a disputed commitment."

let test_non_cemented_parent () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c2, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment2
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.cement_commitment ctxt rollup c2)
    "Parent is not cemented."

let test_finds_conflict_point_at_lcc () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 55l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_c2, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment2
  in
  let* ((left, _right), ctxt) =
    lift @@ Sc_rollup_storage.get_conflict_point ctxt rollup staker1 staker2
  in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt left c1

let test_finds_conflict_point_beneath_lcc () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c2, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment2
  in
  let commitment3 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 4l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c3, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment3
  in
  let* ((left, right), ctxt) =
    lift @@ Sc_rollup_storage.get_conflict_point ctxt rollup staker1 staker2
  in
  let* () = assert_commitment_hash_equal ~loc:__LOC__ ctxt left c2 in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt right c3

let test_conflict_point_is_first_point_of_disagreement () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c2, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment2
  in
  let commitment3 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 4l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c3, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment3
  in
  let commitment4 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c2;
        inbox_level = Raw_level_repr.of_int32_exn 61l;
        number_of_messages = number_of_messages_exn 4l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_c4, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment4
  in
  let* ((left, right), ctxt) =
    lift @@ Sc_rollup_storage.get_conflict_point ctxt rollup staker1 staker2
  in
  let* () = assert_commitment_hash_equal ~loc:__LOC__ ctxt left c2 in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt right c3

let test_no_conflict_point_one_staker_at_lcc_preboot () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.get_conflict_point ctxt rollup staker1 staker2)
    "No conflict."

let test_no_conflict_point_both_stakers_at_lcc_preboot () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.get_conflict_point ctxt rollup staker1 staker2)
    "No conflict."

let test_no_conflict_point_one_staker_at_lcc () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_node, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment2
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt = lift @@ Sc_rollup_storage.cement_commitment ctxt rollup c1 in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.get_conflict_point ctxt rollup staker1 staker2)
    "No conflict."

let test_no_conflict_point_both_stakers_at_lcc () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
  in
  let* (_node, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment1
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
  let* ctxt = lift @@ Sc_rollup_storage.cement_commitment ctxt rollup c1 in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.get_conflict_point ctxt rollup staker1 staker2)
    "No conflict."

let test_staker_cannot_backtrack () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker commitment2
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.refine_stake ctxt rollup staker commitment1)
    "Staker backtracked."

let test_staker_cannot_change_branch () =
  let* (ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c2, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment2
  in
  let commitment3 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 41l;
        number_of_messages = number_of_messages_exn 4l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in

  let* (_c3, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment3
  in
  let commitment4 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c2;
        inbox_level = Raw_level_repr.of_int32_exn 61l;
        number_of_messages = number_of_messages_exn 4l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_c4, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment4
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment4)
    "Staker backtracked."

let test_kind_of_missing_rollup () =
  let* ctxt = new_context () in
  let rollup = Sc_rollup_repr.Address.hash_bytes [] in
  let* kind = lift @@ Sc_rollup_storage.kind ctxt rollup in
  assert_kinds_are_equal ~loc:__LOC__ Option.None kind

let test_add_messages_from_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_storage.add_messages ctxt rollup ["Dummy message"])

let test_inbox_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ Sc_rollup_storage.inbox

let test_refine_stake_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_storage.refine_stake
        ctxt
        rollup
        Sc_rollup_repr.Staker.zero
        Sc_rollup_repr.Commitment.
          {
            predecessor = Sc_rollup_repr.Commitment_hash.zero;
            inbox_level = Raw_level_repr.of_int32_exn 21l;
            number_of_messages = number_of_messages_exn 3l;
            number_of_ticks = number_of_ticks_exn 1232909l;
            compressed_state = Sc_rollup_repr.State_hash.zero;
          })

let test_last_cemented_commitment_of_missing_rollup () =
  assert_fails_with_missing_rollup
    ~loc:__LOC__
    Sc_rollup_storage.last_cemented_commitment

let test_last_cemented_commitment_hash_with_level_of_missing_rollup () =
  assert_fails_with_missing_rollup
    ~loc:__LOC__
    Sc_rollup_storage.last_cemented_commitment_hash_with_level

let test_cement_commitment_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_storage.cement_commitment
        ctxt
        rollup
        Sc_rollup_repr.Commitment_hash.zero)

let test_get_conflict_point_on_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_storage.get_conflict_point
        ctxt
        rollup
        Sc_rollup_repr.Staker.zero
        Sc_rollup_repr.Staker.zero)

let test_get_commitment_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_storage.get_commitment
        ctxt
        rollup
        Sc_rollup_repr.Commitment_hash.zero)

let test_get_missing_commitment () =
  let* ctxt = new_context () in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let commitment_hash = Sc_rollup_repr.Commitment_hash.zero in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.get_commitment ctxt rollup commitment_hash)
    "Commitment scc12XhSULdV8bAav21e99VYLTpqAjTd7NU8Mn4zFdKPSA8auMbggG does \
     not exist"

let test_remove_staker_from_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
      Sc_rollup_storage.remove_staker ctxt rollup Sc_rollup_repr.Staker.zero)

let test_initial_level_of_missing_rollup () =
  assert_fails_with_missing_rollup ~loc:__LOC__ Sc_rollup_storage.initial_level

let test_concurrent_refinement_point_of_conflict () =
  let* (before_ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 10l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* ((c1, c2), _ctxt) =
    lift
    @@ let* (_c1, ctxt) =
         Sc_rollup_storage.refine_stake before_ctxt rollup staker1 commitment1
       in
       let* (_c2, ctxt) =
         Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment2
       in
       Sc_rollup_storage.get_conflict_point ctxt rollup staker1 staker2
  in
  let* ((c1', c2'), ctxt) =
    lift
    @@ let* (_c2, ctxt) =
         Sc_rollup_storage.refine_stake before_ctxt rollup staker2 commitment2
       in
       let* (_c1, ctxt) =
         Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment1
       in
       Sc_rollup_storage.get_conflict_point ctxt rollup staker1 staker2
  in
  let* () = assert_commitment_hash_equal ~loc:__LOC__ ctxt c1 c1' in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt c2 c2'

let test_concurrent_refinement_cement () =
  let* (before_ctxt, rollup, staker1, staker2) =
    originate_rollup_and_deposit_with_two_stakers ()
  in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, _ctxt) =
    lift
    @@ let* (c1, ctxt) =
         Sc_rollup_storage.refine_stake before_ctxt rollup staker1 commitment
       in
       let* (_c2, ctxt) =
         Sc_rollup_storage.refine_stake ctxt rollup staker2 commitment
       in
       let challenge_window =
         Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
       in
       let ctxt =
         Raw_context.Internal_for_tests.add_level ctxt challenge_window
       in
       let* ctxt = Sc_rollup_storage.cement_commitment ctxt rollup c1 in
       Sc_rollup_storage.last_cemented_commitment ctxt rollup
  in
  let* (c2, ctxt) =
    lift
    @@ let* (c2, ctxt) =
         Sc_rollup_storage.refine_stake before_ctxt rollup staker2 commitment
       in
       let* (_c1, ctxt) =
         Sc_rollup_storage.refine_stake ctxt rollup staker1 commitment
       in
       let challenge_window =
         Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
       in
       let ctxt =
         Raw_context.Internal_for_tests.add_level ctxt challenge_window
       in
       let* ctxt = Sc_rollup_storage.cement_commitment ctxt rollup c2 in
       Sc_rollup_storage.last_cemented_commitment ctxt rollup
  in
  assert_commitment_hash_equal ~loc:__LOC__ ctxt c1 c2

let check_gas_consumed ~since ~until =
  let open Raw_context in
  let as_cost = Gas_limit_repr.cost_of_gas @@ gas_consumed ~since ~until in
  Saturation_repr.to_int as_cost

(* Cost of compare key is currently free, which means that the lookup operation  
   on a map of size 1 will consume 50 gas units (base cost), plus 2 for the 
   traversal overhead, plus 15 for comparing the key, for a total of 67 gas units.
*)
let test_carbonated_memory_inbox_retrieval () =
  let open Raw_context in
  let* ctxt = new_context () in
  let ctxt =
    set_gas_limit ctxt (Gas_limit_repr.Arith.integral_of_int_exn 20_000)
  in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let*? (_, ctxt') =
    Environment.wrap_tzresult
    @@ Sc_rollup_in_memory_inbox.current_messages ctxt rollup
  in
  let consumed_gas = check_gas_consumed ~since:ctxt ~until:ctxt' in
  Assert.equal_int ~loc:__LOC__ consumed_gas 67

(* A bit ugly, as we repeat the logic for setting messages 
   defined in `Sc_rollup_storage`. However, this is necessary 
   since we want to capture the context before and after performing 
   the `set_current_messages` operation on the in-memory map of messages.

   Assuming that the cost of compare key is free, 
   we expect set_messages to consume 67 gas units for finding the key, 
   and 134 gas units for performing the update, for a total of 201 gas units.
*)
let test_carbonated_memory_inbox_set_messages () =
  let open Raw_context in
  let* ctxt = new_context () in
  let ctxt =
    set_gas_limit ctxt (Gas_limit_repr.Arith.integral_of_int_exn 20_000)
  in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let* (inbox, ctxt) = lift @@ Sc_rollup_storage.inbox ctxt rollup in
  let*? (current_messages, ctxt) =
    Environment.wrap_tzresult
    @@ Sc_rollup_in_memory_inbox.current_messages ctxt rollup
  in
  let {Level_repr.level; _} = Raw_context.current_level ctxt in
  let* (current_messages, _) =
    lift
    @@ Sc_rollup_inbox_repr.(
         add_messages_no_history
           inbox
           level
           ["CAFEBABE"; "CAFEBABE"; "CAFEBABE"]
           current_messages)
  in
  let*? ctxt' =
    Environment.wrap_tzresult
    @@ Sc_rollup_in_memory_inbox.set_current_messages
         ctxt
         rollup
         current_messages
  in
  let consumed_gas = check_gas_consumed ~since:ctxt ~until:ctxt' in
  Assert.equal_int ~loc:__LOC__ consumed_gas 201

let tests =
  [
    Tztest.tztest
      "deposit to missing rollup fails"
      `Quick
      test_deposit_to_missing_rollup;
    Tztest.tztest
      "deposit to existing rollup"
      `Quick
      test_deposit_to_existing_rollup;
    Tztest.tztest "can not deposit twice" `Quick test_can_not_stake_twice;
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
      test_initial_level_of_rollup;
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
      "cement consumes available messages"
      `Quick
      test_cement_consumes_available_messages;
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
      "adding messages to missing rollup fails"
      `Quick
      test_add_messages_from_missing_rollup;
    Tztest.tztest
      "fetching inbox of missing rollup fails"
      `Quick
      test_inbox_of_missing_rollup;
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
      test_initial_level_of_missing_rollup;
    Tztest.tztest
      "Refinement operations are commutative (point of conflict)"
      `Quick
      test_concurrent_refinement_point_of_conflict;
    Tztest.tztest
      "Refinement operations are commutative (cement)"
      `Quick
      test_concurrent_refinement_cement;
    Tztest.tztest
      "Retrieval of in-memory message inbox consumes gas"
      `Quick
      test_carbonated_memory_inbox_retrieval;
    Tztest.tztest
      "Setting messages in in-memory message inbox consumes gas"
      `Quick
      test_carbonated_memory_inbox_set_messages;
  ]

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2460
   Further tests to be added.
   *)
