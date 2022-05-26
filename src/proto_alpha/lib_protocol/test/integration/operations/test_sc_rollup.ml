(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

let wrap k = Lwt.map Environment.wrap_tzresult k

let assert_fails ~loc ?error m =
  let open Lwt_result_syntax in
  let*! res = m in
  match res with
  | Ok _ -> Stdlib.failwith "Expected failure"
  | Error err_res -> (
      match (err_res, error) with
      | Environment.Ecoproto_error err' :: _, Some err when err = err' ->
          (* Matched exact error. *)
          return_unit
      | _, Some _ ->
          (* Expected a different error. *)
          let msg =
            Printf.sprintf "Expected a different error at location %s" loc
          in
          Stdlib.failwith msg
      | _, None ->
          (* Any error is ok. *)
          return ())

(** [context_init tup] initializes a context for testing in which the
  [sc_rollup_enable] constant is set to true. It returns the created
  context and contracts. *)
let context_init ?(sc_rollup_challenge_window_in_blocks = 10) tup =
  Context.init_with_constants_gen
    tup
    {
      Context.default_test_constants with
      sc_rollup_enable = true;
      consensus_threshold = 0;
      sc_rollup_challenge_window_in_blocks;
    }

(** [test_disable_feature_flag ()] tries to originate a smart contract
    rollup when the feature flag is deactivated and checks that it
    fails. *)
let test_disable_feature_flag () =
  let* b, contract = Context.init1 () in
  let* i = Incremental.begin_construction b in
  let kind = Sc_rollup.Kind.Example_arith in
  let* op, _ =
    let parameters_ty = Script.lazy_expr @@ Expr.from_string "unit" in
    Op.sc_rollup_origination (I i) contract kind "" parameters_ty
  in

  let expect_failure = function
    | Environment.Ecoproto_error (Apply.Sc_rollup_feature_disabled as e) :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ -> failwith "It should have failed with [Sc_rollup_feature_disabled]"
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
let sc_originate block contract parameters_ty =
  let kind = Sc_rollup.Kind.Example_arith in
  let* operation, rollup =
    Op.sc_rollup_origination
      ~counter:(Z.of_int 0)
      (B block)
      contract
      kind
      ""
      (Script.lazy_expr @@ Expr.from_string parameters_ty)
  in
  let* incr = Incremental.begin_construction block in
  let* incr = Incremental.add_operation incr operation in
  let* block = Incremental.finalize_block incr in
  return (block, rollup)

(** Initializes the context and originates a SCORU. *)
let init_and_originate ?sc_rollup_challenge_window_in_blocks tup parameters_ty =
  let* block, contracts =
    context_init ?sc_rollup_challenge_window_in_blocks tup
  in
  let contract = Context.tup_hd tup contracts in
  let* block, rollup = sc_originate block contract parameters_ty in
  return (block, contracts, rollup)

let number_of_messages_exn n =
  match Sc_rollup.Number_of_messages.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_messages"

let number_of_ticks_exn n =
  match Sc_rollup.Number_of_ticks.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_ticks"

let dummy_commitment ctxt rollup =
  let ctxt = Incremental.alpha_ctxt ctxt in
  let*! root_level = Sc_rollup.initial_level ctxt rollup in
  let root_level =
    match root_level with Ok v -> v | Error _ -> assert false
  in
  let inbox_level =
    let commitment_freq =
      Constants_storage.sc_rollup_commitment_period_in_blocks
        (Alpha_context.Internal_for_tests.to_raw ctxt)
    in

    Raw_level.of_int32_exn
      (Int32.add (Raw_level.to_int32 root_level) (Int32.of_int commitment_freq))
  in
  return
    Sc_rollup.Commitment.
      {
        predecessor = Sc_rollup.Commitment.Hash.zero;
        inbox_level;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 3000l;
        compressed_state = Sc_rollup.State_hash.zero;
      }

(** [test_publish_and_cement] creates a rollup, publishes a
    commitment and then [commitment_freq] blocks later cements that commitment *)
let test_publish_and_cement () =
  let* ctxt, contracts, rollup = init_and_originate Context.T2 "unit" in
  let _, contract = contracts in
  let* i = Incremental.begin_construction ctxt in
  let* c = dummy_commitment i rollup in
  let* operation = Op.sc_rollup_publish (B ctxt) contract rollup c in
  let* i = Incremental.add_operation i operation in
  let* b = Incremental.finalize_block i in
  let* constants = Context.get_constants (B b) in
  let* b =
    Block.bake_n constants.parametric.sc_rollup_challenge_window_in_blocks b
  in
  let* i = Incremental.begin_construction b in
  let hash = Sc_rollup.Commitment.hash c in
  let* cement_op = Op.sc_rollup_cement (I i) contract rollup hash in
  let* _ = Incremental.add_operation i cement_op in
  return_unit

(** [test_publish_fails_on_backtrack] creates a rollup and then
    publishes two different commitments with the same staker. We check
    that the second publish fails. *)
let test_publish_fails_on_backtrack () =
  let* ctxt, contracts, rollup = init_and_originate Context.T2 "unit" in
  let _, contract = contracts in
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
        (Sc_rollup_errors.Sc_rollup_staker_backtracked as e)
      :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ -> failwith "It should have failed with [Sc_rollup_staker_backtracked]"
  in
  let* _ = Incremental.add_operation ~expect_failure i operation2 in
  return_unit

(** [test_cement_fails_on_conflict] creates a rollup and then publishes
    two different commitments. It waits 20 blocks and then attempts to
    cement one of the commitments; it checks that this fails because the
    commitment is contested. *)
let test_cement_fails_on_conflict () =
  let* ctxt, contracts, rollup = init_and_originate Context.T3 "unit" in
  let _, contract1, contract2 = contracts in
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
  let* constants = Context.get_constants (B b) in
  let* b =
    Block.bake_n constants.parametric.sc_rollup_challenge_window_in_blocks b
  in
  let* i = Incremental.begin_construction b in
  let hash = Sc_rollup.Commitment.hash commitment1 in
  let* cement_op = Op.sc_rollup_cement (I i) contract1 rollup hash in
  let expect_failure = function
    | Environment.Ecoproto_error (Sc_rollup_errors.Sc_rollup_disputed as e) :: _
      ->
        Assert.test_error_encodings e ;
        return_unit
    | _ -> failwith "It should have failed with [Sc_rollup_disputed]"
  in

  let* _ = Incremental.add_operation ~expect_failure i cement_op in
  return_unit

let commit_and_cement_after_n_bloc ?expect_failure ctxt contract rollup n =
  let* i = Incremental.begin_construction ctxt in
  let* commitment = dummy_commitment i rollup in
  let* operation = Op.sc_rollup_publish (B ctxt) contract rollup commitment in
  let* i = Incremental.add_operation i operation in
  let* b = Incremental.finalize_block i in
  (* This pattern would add an additional block, so we decrement [n] by one. *)
  let* b = Block.bake_n (n - 1) b in
  let* i = Incremental.begin_construction b in
  let hash = Sc_rollup.Commitment.hash commitment in
  let* cement_op = Op.sc_rollup_cement (I i) contract rollup hash in
  let* _ = Incremental.add_operation ?expect_failure i cement_op in
  return_unit

(** [test_challenge_window_period_boundaries] checks that cementing a commitment
    without waiting for the whole challenge window period fails. Whereas,
    succeeds when the period is over. *)
let test_challenge_window_period_boundaries () =
  let sc_rollup_challenge_window_in_blocks = 10 in
  let* ctxt, contract, rollup =
    init_and_originate ~sc_rollup_challenge_window_in_blocks Context.T1 "unit"
  in
  (* Should fail because the waiting period is not strictly greater than the
     challenge window period. *)
  let* () =
    let expect_failure = function
      | Environment.Ecoproto_error (Sc_rollup_errors.Sc_rollup_too_recent as e)
        :: _ ->
          Assert.test_error_encodings e ;
          return_unit
      | _ -> failwith "It should have failed with [Sc_rollup_too_recent]"
    in
    commit_and_cement_after_n_bloc
      ~expect_failure
      ctxt
      contract
      rollup
      (sc_rollup_challenge_window_in_blocks - 1)
  in
  (* Succeeds because the challenge period is over. *)
  let* () =
    commit_and_cement_after_n_bloc
      ctxt
      contract
      rollup
      sc_rollup_challenge_window_in_blocks
  in
  return_unit

(** Test originating with bad type. *)
let test_originating_with_invalid_types () =
  let* block, (contract, _, _) = context_init Context.T3 in
  let assert_fails_for_type parameters_type =
    assert_fails
      ~loc:__LOC__
      ~error:Sc_rollup_operations.Sc_rollup_invalid_parameters_type
      (sc_originate block contract parameters_type)
  in
  (* Following types fail at validation time. *)
  let* () =
    [
      "mutez";
      "big_map string nat";
      "contract string";
      "sapling_state 2";
      "sapling_transaction 2";
      "lambda string nat";
    ]
    |> List.iter_es assert_fails_for_type
  in
  (* Operation fails with a different error as it's not "passable". *)
  assert_fails ~loc:__LOC__ (sc_originate block contract "operation")

let assert_equal_expr ~loc e1 e2 =
  let s1 = Format.asprintf "%a" Michelson_v1_printer.print_expr e1 in
  let s2 = Format.asprintf "%a" Michelson_v1_printer.print_expr e2 in
  Assert.equal_string ~loc s1 s2

let test_originating_with_valid_type () =
  let* block, contract = context_init Context.T1 in
  let assert_parameters_ty parameters_ty =
    let* block, rollup = sc_originate block contract parameters_ty in
    let* incr = Incremental.begin_construction block in
    let ctxt = Incremental.alpha_ctxt incr in
    let* expr, _ctxt = wrap @@ Sc_rollup.parameters_type ctxt rollup in
    let*? expr, _ctxt =
      Environment.wrap_tzresult
      @@ Script.force_decode_in_context
           ~consume_deserialization_gas:When_needed
           ctxt
           expr
    in
    assert_equal_expr ~loc:__LOC__ (Expr.from_string parameters_ty) expr
  in
  [
    "unit";
    "int";
    "nat";
    "signature";
    "string";
    "bytes";
    "key_hash";
    "key";
    "timestamp";
    "address";
    "bls12_381_fr";
    "bls12_381_g1";
    "bls12_381_g2";
    "bool";
    "never";
    "tx_rollup_l2_address";
    "chain_id";
    "ticket string";
    "set nat";
    "option (ticket string)";
    "list nat";
    "pair nat unit";
    "or nat string";
    "map string int";
    "map (option (pair nat string)) (list (ticket nat))";
    "or (nat %deposit) (string %name)";
  ]
  |> List.iter_es assert_parameters_ty

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
      "publish will fail if staker is backtracking"
      `Quick
      test_publish_fails_on_backtrack;
    Tztest.tztest
      "cement will fail if commitment is contested"
      `Quick
      test_cement_fails_on_conflict;
    Tztest.tztest
      "check the challenge window period boundaries"
      `Quick
      test_challenge_window_period_boundaries;
    Tztest.tztest
      "originating with invalid types"
      `Quick
      test_originating_with_invalid_types;
    Tztest.tztest
      "originating with valid type"
      `Quick
      test_originating_with_valid_type;
  ]
