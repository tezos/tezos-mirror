(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Rollup layer 1 logic
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                  -- test "^sc rollup$"
    Subject:      Test smart contract rollup
*)

open Protocol
open Alpha_context
open Lwt_result_syntax

exception Sc_rollup_test_error of string

let err x = Exn (Sc_rollup_test_error x)

(** [context_init tup] initializes a context for testing in which the
  [sc_rollup_enable] constant is set to true. It returns the created
  context and contracts. *)
let context_init tup =
  Context.init_with_constants_gen
    tup
    {
      Context.default_test_constants with
      sc_rollup_enable = true;
      consensus_threshold = 0;
      sc_rollup_challenge_window_in_blocks = 10;
    }

(** [test_disable_feature_flag ()] tries to originate a smart contract
   rollup when the feature flag is deactivated and checks that it
   fails. *)
let test_disable_feature_flag () =
  let* (b, contract) = Context.init1 () in
  let* i = Incremental.begin_construction b in
  let kind = Sc_rollup.Kind.Example_arith in
  let* (op, _) = Op.sc_rollup_origination (I i) contract kind "" in
  let expect_failure = function
    | Environment.Ecoproto_error (Apply.Sc_rollup_feature_disabled as e) :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ ->
        failwith
          "It should not be possible to send a smart contract rollup operation \
           when the feature flag is disabled."
  in
  let*! _ = Incremental.add_operation ~expect_failure i op in
  return_unit

(** [test_sc_rollups_all_well_defined] checks that [Sc_rollups.all]
    contains all the constructors of [Sc_rollup.Kind.t] and that
    the [kind_of_string] is consistent with the names declared in
    the PVM implementations. *)
let test_sc_rollups_all_well_defined () =
  let all_contains_all_constructors () =
    let tickets = ref ["Example_arith"] in
    let burn x = tickets := List.filter (( <> ) x) !tickets in
    let pick = function
      | Sc_rollup.Kind.Example_arith -> burn "Example_arith"
    in
    List.iter pick Sc_rollups.all ;
    if !tickets <> [] then
      failwith
        "The following smart-contract rollup kinds should occur in \
         [Sc_rollups.all]: %s\n"
        (String.concat ", " !tickets)
    else return_unit
  in
  let all_names_are_valid () =
    List.iter_es
      (fun k ->
        let (module P : Sc_rollups.PVM.S) = Sc_rollups.of_kind k in
        fail_unless
          (Sc_rollups.kind_of_string P.name = Some k)
          (err (Printf.sprintf "PVM name `%s' is not a valid kind name" P.name)))
      Sc_rollups.all
  in
  let* _ = all_contains_all_constructors () in
  all_names_are_valid ()

(** Initializes the context and originates a SCORU. *)
let init_and_originate tup =
  let* (ctxt, contracts) = context_init tup in
  let contract = Context.tup_hd tup contracts in
  let kind = Sc_rollup.Kind.Example_arith in
  let* (operation, rollup) =
    Op.sc_rollup_origination (B ctxt) contract kind ""
  in
  let* b = Block.bake ~operation ctxt in
  return (b, contracts, rollup)

let number_of_messages_exn n =
  match Sc_rollup.Number_of_messages.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_messages"

let number_of_ticks_exn n =
  match Sc_rollup.Number_of_ticks.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_ticks"

let rec bake_until i top =
  let level = Incremental.level i in
  if level >= top then return i
  else
    Incremental.finalize_block i >>=? fun b ->
    Incremental.begin_construction b >>=? fun i -> bake_until i top

let dummy_commitment ctxt rollup =
  let ctxt = Incremental.alpha_ctxt ctxt in
  let*! root_level = Sc_rollup.initial_level ctxt rollup in
  let root_level =
    match root_level with Ok v -> v | Error _ -> assert false
  in
  let inbox_level =
    let commitment_freq =
      Constants_storage.sc_rollup_commitment_frequency_in_blocks
        (Alpha_context.Internal_for_tests.to_raw ctxt)
    in

    Raw_level.of_int32_exn
      (Int32.add (Raw_level.to_int32 root_level) (Int32.of_int commitment_freq))
  in
  return
    Sc_rollup.Commitment.
      {
        predecessor = Sc_rollup.Commitment_hash.zero;
        inbox_level;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 3000l;
        compressed_state = Sc_rollup.State_hash.zero;
      }

(** [test_publish_and_cement] creates a rollup, publishes a
    commitment and then [commitment_freq] blocks later cements that commitment *)
let test_publish_and_cement () =
  let* (ctxt, contracts, rollup) = init_and_originate Context.T2 in
  let (_, contract) = contracts in
  let* i = Incremental.begin_construction ctxt in
  let* c = dummy_commitment i rollup in
  let* operation = Op.sc_rollup_publish (B ctxt) contract rollup c in
  let* i = Incremental.add_operation i operation in
  let* b = Incremental.finalize_block i in
  let* i = Incremental.begin_construction b in
  let* i = bake_until i 20l in
  let hash = Sc_rollup.Commitment.hash c in
  let* cement_op = Op.sc_rollup_cement (I i) contract rollup hash in
  let* _ = Incremental.add_operation i cement_op in
  return_unit

(** [test_cement_fails_if_premature] creates a rollup, publishes a
    commitment and then tries to cement the commitment immediately
    without waiting for the challenge period to elapse. We check that
    this fails with the correct error. *)
let test_cement_fails_if_premature () =
  let* (ctxt, contracts, rollup) = init_and_originate Context.T2 in
  let (_, contract) = contracts in
  let* i = Incremental.begin_construction ctxt in
  let* c = dummy_commitment i rollup in
  let* operation = Op.sc_rollup_publish (B ctxt) contract rollup c in
  let* i = Incremental.add_operation i operation in
  let* b = Incremental.finalize_block i in
  let* i = Incremental.begin_construction b in
  let hash = Sc_rollup.Commitment.hash c in
  let* cement_op = Op.sc_rollup_cement (I i) contract rollup hash in
  let expect_failure = function
    | Environment.Ecoproto_error (Sc_rollup_storage.Sc_rollup_too_recent as e)
      :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ ->
        failwith "It should not be possible to cement a commitment prematurely."
  in
  let* _ = Incremental.add_operation ~expect_failure i cement_op in
  return_unit

(** [test_publish_fails_on_backtrack] creates a rollup and then
    publishes two different commitments with the same staker. We check
    that the second publish fails. *)
let test_publish_fails_on_backtrack () =
  let* (ctxt, contracts, rollup) = init_and_originate Context.T2 in
  let (_, contract) = contracts in
  let* i = Incremental.begin_construction ctxt in
  let* commitment1 = dummy_commitment i rollup in
  let commitment2 =
    {commitment1 with number_of_ticks = number_of_ticks_exn 3001l}
  in
  let* operation1 = Op.sc_rollup_publish (B ctxt) contract rollup commitment1 in
  let* i = Incremental.add_operation i operation1 in
  let* b = Incremental.finalize_block i in
  let* operation2 = Op.sc_rollup_publish (B b) contract rollup commitment2 in
  let* i = Incremental.begin_construction b in
  let expect_failure = function
    | Environment.Ecoproto_error
        (Sc_rollup_storage.Sc_rollup_staker_backtracked as e)
      :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ -> failwith "It should not be possible for a staker to backtrack."
  in
  let* _ = Incremental.add_operation ~expect_failure i operation2 in
  return_unit

(** [test_cement_fails_on_conflict] creates a rollup and then publishes
    two different commitments. It waits 20 blocks and then attempts to
    cement one of the commitments; it checks that this fails because the
    commitment is contested. *)
let test_cement_fails_on_conflict () =
  let* (ctxt, contracts, rollup) = init_and_originate Context.T3 in
  let (_, contract1, contract2) = contracts in
  let* i = Incremental.begin_construction ctxt in
  let* commitment1 = dummy_commitment i rollup in
  let commitment2 =
    {commitment1 with number_of_ticks = number_of_ticks_exn 3001l}
  in
  let* operation1 =
    Op.sc_rollup_publish (B ctxt) contract1 rollup commitment1
  in
  let* i = Incremental.add_operation i operation1 in
  let* b = Incremental.finalize_block i in
  let* operation2 = Op.sc_rollup_publish (B b) contract2 rollup commitment2 in
  let* i = Incremental.begin_construction b in
  let* i = Incremental.add_operation i operation2 in
  let* b = Incremental.finalize_block i in
  let* i = Incremental.begin_construction b in
  let* i = bake_until i 20l in
  let hash = Sc_rollup.Commitment.hash commitment1 in
  let* cement_op = Op.sc_rollup_cement (I i) contract1 rollup hash in
  let expect_failure = function
    | Environment.Ecoproto_error (Sc_rollup_storage.Sc_rollup_disputed as e)
      :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ ->
        failwith "It should not be possible to cement a contested commitment."
  in
  let* _ = Incremental.add_operation ~expect_failure i cement_op in
  return_unit

let tests =
  [
    Tztest.tztest
      "check effect of disabled feature flag"
      `Quick
      test_disable_feature_flag;
    Tztest.tztest
      "check that all rollup kinds are correctly enumerated"
      `Quick
      test_sc_rollups_all_well_defined;
    Tztest.tztest
      "can publish a commit and then cement it"
      `Quick
      test_publish_and_cement;
    Tztest.tztest
      "cement will fail if it is too soon"
      `Quick
      test_cement_fails_if_premature;
    Tztest.tztest
      "publish will fail if staker is backtracking"
      `Quick
      test_publish_fails_on_backtrack;
    Tztest.tztest
      "cement will fail if commitment is contested"
      `Quick
      test_cement_fails_on_conflict;
  ]
