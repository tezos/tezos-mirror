(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
      | Environment.Ecoproto_error err' :: _, Some err ->
          let msg =
            Format.asprintf
              "Expected error [%a] but got [%a] at location %s"
              Environment.Error_monad.pp
              err'
              Environment.Error_monad.pp
              err
              loc
          in
          Stdlib.failwith msg
      | _, Some _ ->
          (* Expected a different error. *)
          let msg =
            Printf.sprintf "Expected a different error at location %s" loc
          in
          Stdlib.failwith msg
      | _, None ->
          (* Any error is ok. *)
          return ())

let assert_equal_z ~loc x y =
  Assert.equal ~loc Z.equal "Compare Z.t" Z.pp_print x y

(** [context_init tup] initializes a context for testing in which the
  [sc_rollup_enable] constant is set to true. It returns the created
  context and contracts. *)
let context_init ?(sc_rollup_challenge_window_in_blocks = 10) tup =
  Context.init_with_constants_gen
    tup
    {
      Context.default_test_constants with
      consensus_threshold = 0;
      sc_rollup =
        {
          Context.default_test_constants.sc_rollup with
          enable = true;
          challenge_window_in_blocks = sc_rollup_challenge_window_in_blocks;
        };
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
  let expect_apply_failure = function
    | Environment.Ecoproto_error
        (Validate_operation.Manager.Sc_rollup_feature_disabled as e)
      :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ -> failwith "It should have failed with [Sc_rollup_feature_disabled]"
  in
  let*! _ = Incremental.add_operation ~expect_apply_failure i op in
  return_unit

(** [test_sc_rollups_all_well_defined] checks that the [kind_of_string] is
    consistent with the names declared in the PVM implementations. *)
let test_sc_rollups_all_well_defined () =
  let all_names_are_valid () =
    List.iter_es
      (fun k ->
        let (module P : Sc_rollup.PVM.S) = Sc_rollup.Kind.pvm_of k in
        fail_unless
          (Sc_rollup.Kind.of_name P.name = Some k)
          (err (Printf.sprintf "PVM name `%s' is not a valid kind name" P.name)))
      Sc_rollup.Kind.all
  in
  all_names_are_valid ()

(** Initializes the context and originates a SCORU. *)
let sc_originate block contract parameters_ty =
  let kind = Sc_rollup.Kind.Example_arith in
  let* operation, rollup =
    Op.sc_rollup_origination
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

(* Verify that parameters and unparsed parameters match. *)
let verify_params ctxt ~parameters_ty ~parameters ~unparsed_parameters =
  let show_stripped exp =
    Expr.to_string @@ Environment.Micheline.strip_locations exp
  in
  let unparse ctxt parameters =
    wrap
      (Script_ir_translator.unparse_data
         ctxt
         Script_ir_translator.Optimized
         parameters_ty
         parameters)
  in
  let* unparsed_parameters, ctxt =
    (* Make sure we can parse the unparsed-parameters with the given parameters
       type. *)
    let* parsed_unparsed_parameters, ctxt =
      wrap
        (Script_ir_translator.parse_data
           ctxt
           ~legacy:true
           ~allow_forged:true
           parameters_ty
           (Environment.Micheline.root unparsed_parameters))
    in
    (* Un-parse again to get back to Micheline. *)
    unparse ctxt parsed_unparsed_parameters
  in
  (* Un-parse the parsed parameters. *)
  let* expected_unparsed_parameters, _ctxt = unparse ctxt parameters in
  (* Verify that both version match. *)
  Assert.equal_string
    ~loc:__LOC__
    (show_stripped unparsed_parameters)
    (show_stripped expected_unparsed_parameters)

(* Verify that the given list of transactions and transaction operations match.
   Also checks each transaction operation for type mismatches etc. *)
let verify_execute_outbox_message_operations incr ~loc ~source ~operations
    ~expected_transactions =
  let ctxt = Incremental.alpha_ctxt incr in
  let validate_and_extract_operation_params ctxt op =
    match op with
    | Script_typed_ir.Internal_operation
        {
          source = op_source;
          operation =
            Transaction_to_contract
              {
                destination;
                amount;
                entrypoint;
                location = _;
                parameters_ty;
                parameters;
                unparsed_parameters;
              };
          nonce = _;
        } ->
        (* Check that the parameters match. *)
        let* () =
          verify_params ctxt ~parameters_ty ~parameters ~unparsed_parameters
        in
        let* () =
          (* Check that the sources match. *)
          Assert.equal_string
            ~loc
            (Contract.to_b58check (Contract.Implicit source))
            (Contract.to_b58check op_source)
        in
        (* Assert that the amount is 0. *)
        let* () = Assert.equal_tez ~loc amount Tez.zero in
        (* Load the arg-type and entrypoints of the destination script. *)
        let* ( Script_ir_translator.Ex_script (Script {arg_type; entrypoints; _}),
               ctxt ) =
          let* contract_hash =
            match destination with
            | Contract.Originated ch -> return ch
            | _ -> failwith "Expected originated contract at %s" loc
          in
          let* ctxt, _cache_key, cached =
            wrap @@ Script_cache.find ctxt contract_hash
          in
          match cached with
          | Some (_script, ex_script) -> return (ex_script, ctxt)
          | None -> failwith "Could not load script at %s" loc
        in
        (* Find the script parameters ty of the script. *)
        let*? entrypoint_res, ctxt =
          Environment.wrap_tzresult
            (Gas_monad.run
               ctxt
               (Script_ir_translator.find_entrypoint
                  ~error_details:(Informative ())
                  arg_type
                  entrypoints
                  entrypoint))
        in
        let*? (Ex_ty_cstr {ty = script_parameters_ty; _}) =
          Environment.wrap_tzresult entrypoint_res
        in
        (* Check that the script parameters type matches the one from the
           transaction. *)
        let*? ctxt =
          Environment.wrap_tzresult
            (let open Result_syntax in
            let* eq, ctxt =
              Gas_monad.run
                ctxt
                (Script_ir_translator.ty_eq
                   ~error_details:(Informative (-1))
                   script_parameters_ty
                   parameters_ty)
            in
            let+ Eq = eq in
            ctxt)
        in
        return (ctxt, (destination, entrypoint, unparsed_parameters))
    | _ ->
        failwith
          "Expected an internal transaction operation, called from %s"
          loc
  in
  let* _ctxt, operations_data =
    List.fold_left_map_es validate_and_extract_operation_params ctxt operations
  in
  let compare_data (d1, e1, p1) (d2, e2, p2) =
    Contract.equal d1 d2
    && Entrypoint_repr.(e1 = e2)
    && String.equal (Expr.to_string p1) (Expr.to_string p2)
  in
  let pp_data fmt (d, e, p) =
    Format.fprintf
      fmt
      "(%a, %a, %s)"
      Contract.pp
      d
      Entrypoint_repr.pp
      e
      (Expr.to_string p)
  in
  let transactions_data =
    let data_of_transaction (ty, entrypoint, params) =
      let params = Expr.from_string params in
      (ty, entrypoint, params)
    in
    List.map data_of_transaction expected_transactions
  in
  Assert.assert_equal_list
    ~loc
    compare_data
    "Compare operations data"
    pp_data
    operations_data
    transactions_data

(* Helper function to create output used for executing outbox messages. *)
let make_output ~outbox_level ~message_index transactions =
  let transactions =
    List.map
      (fun (destination, entrypoint, parameters) ->
        let destination =
          match destination with
          | Contract.Originated ch -> ch
          | Contract.Implicit _ ->
              Stdlib.failwith "Expected an originated contract."
        in
        let unparsed_parameters = Expr.from_string parameters in
        {Sc_rollup.Outbox.Message.unparsed_parameters; destination; entrypoint})
      transactions
  in
  let message =
    Sc_rollup.Outbox.Message.Atomic_transaction_batch {transactions}
  in
  let outbox_level = Raw_level.of_int32_exn (Int32.of_int outbox_level) in
  let message_index = Z.of_int message_index in
  Sc_rollup.{outbox_level; message_index; message}

let string_ticket_token ticketer content =
  let open Lwt_result_syntax in
  let contents =
    Result.value_f ~default:(fun _ -> assert false)
    @@ Script_string.of_string content
  in
  let*? ticketer = Environment.wrap_tzresult @@ Contract.of_b58check ticketer in
  return
    (Ticket_token.Ex_token
       {ticketer; contents_type = Script_typed_ir.string_t; contents})

let originate_contract incr ~script ~baker ~storage ~source_contract =
  let* block = Incremental.finalize_block incr in
  let* contract, _, block =
    Contract_helpers.originate_contract_from_string
      ~script
      ~storage
      ~source_contract
      ~baker
      block
  in
  let* incr = Incremental.begin_construction block in
  return (contract, incr)

let publish_and_cement_commitment incr ~baker ~originator rollup commitment =
  let* operation = Op.sc_rollup_publish (I incr) originator rollup commitment in
  let* incr = Incremental.add_operation incr operation in
  let* block = Incremental.finalize_block incr in
  let* constants = Context.get_constants (B block) in
  let* block =
    Block.bake_n constants.parametric.sc_rollup.challenge_window_in_blocks block
  in
  let hash = Sc_rollup.Commitment.hash commitment in
  let* incr =
    Incremental.begin_construction ~policy:Block.(By_account baker) block
  in
  let* cement_op = Op.sc_rollup_cement (I incr) originator rollup hash in
  let* incr = Incremental.add_operation incr cement_op in
  let* block = Incremental.finalize_block incr in
  let* incr =
    Incremental.begin_construction ~policy:Block.(By_account baker) block
  in
  return (hash, incr)

let publish_and_cement_dummy_commitment incr ~baker ~originator rollup =
  let* commitment = dummy_commitment incr rollup in
  publish_and_cement_commitment incr ~baker ~originator rollup commitment

(* Publishes repeated cemented commitments until a commitment with
   [inbox_level >= min_inbox_level] is found (such a commitment
   is also published and cemented). *)
let publish_commitments_until_min_inbox_level incr rollup ~baker ~originator
    ~min_inbox_level ~cemented_commitment_hash ~cemented_commitment =
  let commitment_freq =
    Constants_storage.sc_rollup_commitment_period_in_blocks
      (Alpha_context.Internal_for_tests.to_raw @@ Incremental.alpha_ctxt incr)
  in
  let rec aux incr hash ({Sc_rollup.Commitment.inbox_level; _} as commitment) =
    let level = Int32.to_int @@ Raw_level.to_int32 inbox_level in
    if level >= min_inbox_level then return (hash, incr)
    else
      let next_inbox_level =
        Raw_level.of_int32_exn
          (Int32.add
             (Raw_level.to_int32 inbox_level)
             (Int32.of_int commitment_freq))
      in
      let commitment =
        {commitment with predecessor = hash; inbox_level = next_inbox_level}
      in
      let* hash, incr =
        publish_and_cement_commitment incr ~baker ~originator rollup commitment
      in
      aux incr hash commitment
  in
  aux incr cemented_commitment_hash cemented_commitment

let deposit_ticket_token incr rollup ticket_token delta =
  wrap
    (let ctxt = Incremental.alpha_ctxt incr in
     let* rollup_red_token_hash, ctxt =
       Ticket_balance_key.of_ex_token
         ctxt
         ~owner:(Destination.Sc_rollup rollup)
         ticket_token
     in
     let* _, ctxt =
       Ticket_balance.adjust_balance ctxt rollup_red_token_hash ~delta
     in
     let incr = Incremental.set_alpha_ctxt incr ctxt in
     return incr)

(** A version of execute outbox message that output ignores proof validation. *)
let execute_outbox_message_without_proof_validation incr rollup
    ~cemented_commitment ~source outbox_message =
  let* res, ctxt =
    wrap
      (Sc_rollup_operations.Internal_for_tests.execute_outbox_message
         (Incremental.alpha_ctxt incr)
         ~validate_and_decode_output_proof:
           (fun ctxt ~cemented_commitment:_ _rollup ~output_proof:_ ->
           return (outbox_message, ctxt))
         rollup
         ~cemented_commitment
         ~source
         ~output_proof:"Not used")
  in
  return (res, Incremental.set_alpha_ctxt incr ctxt)

let execute_outbox_message incr ~originator rollup ~output_proof
    ~commitment_hash =
  let* batch_op =
    Op.sc_rollup_execute_outbox_message
      (I incr)
      originator
      rollup
      commitment_hash
      ~output_proof
  in
  let* incr = Incremental.add_operation incr batch_op in
  let* block = Incremental.finalize_block incr in
  Incremental.begin_construction block

let get_balance ctxt ~token ~owner =
  let* key_hash, ctxt =
    wrap @@ Ticket_balance_key.of_ex_token ctxt ~owner token
  in
  wrap (Ticket_balance.get_balance ctxt key_hash)

let assert_ticket_token_balance ~loc incr token owner expected =
  let ctxt = Incremental.alpha_ctxt incr in
  let* balance, _ = get_balance ctxt ~token ~owner in
  match (balance, expected) with
  | Some b, Some e -> Assert.equal_int ~loc (Z.to_int b) e
  | Some b, None ->
      failwith "%s: Expected no balance but got some %d" loc (Z.to_int b)
  | None, Some b -> failwith "%s: Expected balance %d but got none" loc b
  | None, None -> return ()

(** Assert that the computation fails with the given message. *)
let assert_fails_with ~__LOC__ k expected_err =
  let*! res = k in
  Assert.proto_error ~loc:__LOC__ res (( = ) expected_err)

type balances = {liquid : Tez.t; frozen : Tez.t}

let balances ctxt contract =
  let* liquid = Context.Contract.balance ctxt contract in
  let* frozen = Context.Contract.frozen_bonds ctxt contract in
  return {liquid; frozen}

let check_balances_evolution bal_before {liquid; frozen} ~action =
  let open Lwt_result_syntax in
  let wret x = wrap @@ Lwt.return x in
  let* {liquid = expected_liquid; frozen = expected_frozen} =
    match action with
    | `Freeze amount ->
        let* liquid = wret @@ Tez.( -? ) bal_before.liquid amount in
        let* frozen = wret @@ Tez.( +? ) bal_before.frozen amount in
        return {liquid; frozen}
    | `Unfreeze amount ->
        let* liquid = wret @@ Tez.( +? ) bal_before.liquid amount in
        let* frozen = wret @@ Tez.( -? ) bal_before.frozen amount in
        return {liquid; frozen}
  in
  let* () = Assert.equal_tez ~loc:__LOC__ expected_liquid liquid in
  let* () = Assert.equal_tez ~loc:__LOC__ expected_frozen frozen in
  return ()

let attempt_to_recover_bond i contract rollup =
  let* recover_bond_op = Op.sc_rollup_recover_bond (I i) contract rollup in
  let* i = Incremental.add_operation i recover_bond_op in
  let* b = Incremental.finalize_block i in
  return b

let recover_bond_not_lcc i contract rollup =
  assert_fails_with
    ~__LOC__
    (attempt_to_recover_bond i contract rollup)
    Sc_rollup_errors.Sc_rollup_not_staked_on_lcc

let recover_bond_not_staked i contract rollup =
  assert_fails_with
    ~__LOC__
    (attempt_to_recover_bond i contract rollup)
    Sc_rollup_errors.Sc_rollup_not_staked

let recover_bond_with_success i contract rollup =
  let* bal_before = balances (I i) contract in
  let* b = attempt_to_recover_bond i contract rollup in
  let* bal_after = balances (B b) contract in
  let* constants = Context.get_constants (I i) in
  let* () =
    check_balances_evolution
      bal_before
      bal_after
      ~action:(`Unfreeze constants.parametric.sc_rollup.stake_amount)
  in
  return b

(** [test_publish_cement_and_recover_bond] creates a rollup, publishes a
    commitment and then [challenge_window_in_blocks] blocks later cements
    that commitment.
    The comitter tries to withdraw stake before and after cementing. Only the
    second attempt is expected to succeed. *)
let test_publish_cement_and_recover_bond () =
  let* ctxt, contracts, rollup = init_and_originate Context.T2 "unit" in
  let _, contract = contracts in
  let* i = Incremental.begin_construction ctxt in
  (* not staked yet *)
  let* () = recover_bond_not_staked i contract rollup in
  let* c = dummy_commitment i rollup in
  let* operation = Op.sc_rollup_publish (B ctxt) contract rollup c in
  let* i = Incremental.add_operation i operation in
  let* b = Incremental.finalize_block i in
  let* constants = Context.get_constants (B b) in
  let* b =
    Block.bake_n constants.parametric.sc_rollup.challenge_window_in_blocks b
  in
  let* i = Incremental.begin_construction b in
  let hash = Sc_rollup.Commitment.hash c in
  (* stake not on LCC *)
  let* () = recover_bond_not_lcc i contract rollup in
  let* cement_op = Op.sc_rollup_cement (I i) contract rollup hash in
  let* i = Incremental.add_operation i cement_op in
  let* b = Incremental.finalize_block i in
  let* i =
    let pkh =
      (* We forbid the stake owner from baker to correctly check the unfrozen
         amount below. *)
      match contract with Implicit pkh -> pkh | Originated _ -> assert false
    in
    Incremental.begin_construction b ~policy:(Excluding [pkh])
  in
  (* recover bond should succeed *)
  let* b = recover_bond_with_success i contract rollup in
  let* i = Incremental.begin_construction b in
  (* not staked anymore *)
  let* () = recover_bond_not_staked i contract rollup in
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
  let expect_apply_failure = function
    | Environment.Ecoproto_error
        (Sc_rollup_errors.Sc_rollup_staker_backtracked as e)
      :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ -> failwith "It should have failed with [Sc_rollup_staker_backtracked]"
  in
  let* _ = Incremental.add_operation ~expect_apply_failure i operation2 in
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
    Block.bake_n constants.parametric.sc_rollup.challenge_window_in_blocks b
  in
  let* i = Incremental.begin_construction b in
  let hash = Sc_rollup.Commitment.hash commitment1 in
  let* cement_op = Op.sc_rollup_cement (I i) contract1 rollup hash in
  let expect_apply_failure = function
    | Environment.Ecoproto_error (Sc_rollup_errors.Sc_rollup_disputed as e) :: _
      ->
        Assert.test_error_encodings e ;
        return_unit
    | _ -> failwith "It should have failed with [Sc_rollup_disputed]"
  in
  let* _ = Incremental.add_operation ~expect_apply_failure i cement_op in
  return_unit

let commit_and_cement_after_n_bloc ?expect_apply_failure ctxt contract rollup n
    =
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
  let* _ = Incremental.add_operation ?expect_apply_failure i cement_op in
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
    let expect_apply_failure = function
      | Environment.Ecoproto_error (Sc_rollup_errors.Sc_rollup_too_recent as e)
        :: _ ->
          Assert.test_error_encodings e ;
          return_unit
      | _ -> failwith "It should have failed with [Sc_rollup_too_recent]"
    in
    commit_and_cement_after_n_bloc
      ~expect_apply_failure
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
    let expr = WithExceptions.Option.get ~loc:__LOC__ expr in
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

(* A contract that receives a pair of nat and a ticket and stores the ticket
   with previously stored tickets. *)
let ticket_receiver =
  {|
      { parameter (pair nat (ticket string));
        storage (list (ticket string));
        code { UNPAIR;          # [(nat, ticket) ; list]
               CDR;             # [ticket ; list]
               CONS;            # [ticket :: list]
               NIL operation ;  # [[] ;  ticket :: list]
               PAIR;            # [([], ticket :: list)]
              }
      }
    |}

(* A contract that receives a string. *)
let string_receiver =
  {|
      { parameter string;
        storage string;
        code { CDR ; NIL operation; PAIR } }
  |}

(* A contract that receives a mutez. *)
let mutez_receiver =
  {|
      { parameter mutez;
        storage mutez;
        code { CDR ; NIL operation; PAIR } }
  |}

let test_single_transaction_batch () =
  let* block, (baker, originator) = context_init Context.T2 in
  let source = Context.Contract.pkh originator in
  let baker = Context.Contract.pkh baker in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup = sc_originate block originator "list (ticket string)" in
  let* incr = Incremental.begin_construction block in
  (* Originate a contract that accepts a pair of nat and ticket string input.  *)
  let* ticket_receiver, incr =
    originate_contract
      incr
      ~script:ticket_receiver
      ~storage:"{}"
      ~source_contract:originator
      ~baker
  in
  (* Ticket-token with content "red". *)
  let* red_token =
    string_ticket_token "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red"
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, incr =
    publish_and_cement_dummy_commitment incr ~baker ~originator rollup
  in
  (* Create an atomic batch message. *)
  let transactions =
    [
      ( ticket_receiver,
        Entrypoint.default,
        {|Pair 42 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1)|} );
    ]
  in
  let output = make_output ~outbox_level:0 ~message_index:0 transactions in
  (* Set up the balance so that the self contract owns one ticket. *)
  let* incr = deposit_ticket_token incr rollup red_token Z.one in
  let* Sc_rollup_operations.{operations; _}, incr =
    execute_outbox_message_without_proof_validation
      incr
      rollup
      ~cemented_commitment
      ~source
      output
  in
  (* Confirm that each transaction maps to one operation. *)
  let* () =
    verify_execute_outbox_message_operations
      ~loc:__LOC__
      incr
      ~source
      ~operations
      ~expected_transactions:transactions
  in
  (* Verify that the balance has moved to ticket-receiver. *)
  let* () =
    assert_ticket_token_balance
      ~loc:__LOC__
      incr
      red_token
      (Destination.Sc_rollup rollup)
      None
  in
  assert_ticket_token_balance
    ~loc:__LOC__
    incr
    red_token
    (Destination.Contract ticket_receiver)
    (Some 1)

let test_multi_transaction_batch () =
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  let source = Context.Contract.pkh originator in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup = sc_originate block originator "list (ticket string)" in
  let* incr = Incremental.begin_construction block in
  (* Originate a contract that accepts a pair of nat and ticket string input. *)
  let* ticket_receiver, incr =
    originate_contract
      incr
      ~script:ticket_receiver
      ~storage:"{}"
      ~source_contract:originator
      ~baker
  in
  (* Originate a contract that accepts a string as input. *)
  let* string_receiver, incr =
    originate_contract
      incr
      ~script:string_receiver
      ~storage:{|""|}
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, incr =
    publish_and_cement_dummy_commitment incr ~baker ~originator rollup
  in
  (* Ticket-token with content "red". *)
  let* red_token =
    string_ticket_token "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red"
  in
  let transactions =
    [
      (* A transaction to the ticket-receiver contract. *)
      ( ticket_receiver,
        Entrypoint.default,
        {|Pair 1 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 4)|} );
      (* Another transaction to the ticket-receiver contract. *)
      ( ticket_receiver,
        Entrypoint.default,
        {|Pair 2 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 6)|} );
      (* A transaction to the string-receiver contract. *)
      (string_receiver, Entrypoint.default, {|"Hello"|});
      (* Another transaction to the string-receiver contract. *)
      (string_receiver, Entrypoint.default, {|"Hello again"|});
    ]
  in
  (* Create an atomic batch message. *)
  let output = make_output ~outbox_level:0 ~message_index:0 transactions in
  (* Set up the balance so that the rollup owns 10 units of red tokens. *)
  let* incr = deposit_ticket_token incr rollup red_token (Z.of_int 10) in
  let* Sc_rollup_operations.{operations; _}, incr =
    execute_outbox_message_without_proof_validation
      incr
      rollup
      ~cemented_commitment
      ~source
      output
  in
  (* Confirm that each transaction maps to one operation. *)
  let* () =
    verify_execute_outbox_message_operations
      ~loc:__LOC__
      incr
      ~source
      ~operations
      ~expected_transactions:transactions
  in
  (* Verify that the balance has moved to ticket-receiver. *)
  let* () =
    assert_ticket_token_balance
      ~loc:__LOC__
      incr
      red_token
      (Destination.Sc_rollup rollup)
      None
  in
  assert_ticket_token_balance
    ~loc:__LOC__
    incr
    red_token
    (Destination.Contract ticket_receiver)
    (Some 10)

(** Test that executing an L2 to L1 transaction that involves an invalid
    parameter (mutez) fails. *)
let test_transaction_with_invalid_type () =
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  let source = Context.Contract.pkh originator in
  let* block, rollup = sc_originate block originator "list (ticket string)" in
  let* incr = Incremental.begin_construction block in
  let* mutez_receiver, incr =
    originate_contract
      incr
      ~script:mutez_receiver
      ~storage:"0"
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, incr =
    publish_and_cement_dummy_commitment incr ~baker ~originator rollup
  in
  let transactions = [(mutez_receiver, Entrypoint.default, "12")] in
  (* Create an atomic batch message. *)
  let output = make_output ~outbox_level:0 ~message_index:1 transactions in
  assert_fails
    ~loc:__LOC__
    ~error:Sc_rollup_operations.Sc_rollup_invalid_parameters_type
    (execute_outbox_message_without_proof_validation
       incr
       rollup
       ~cemented_commitment
       ~source
       output)

(** Test that executing the same outbox message for the same twice fails. *)
let test_execute_message_twice () =
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  let source = Context.Contract.pkh originator in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup = sc_originate block originator "list (ticket string)" in
  let* incr = Incremental.begin_construction block in
  (* Originate a contract that accepts a pair of nat and ticket string input.  *)
  let* string_receiver, incr =
    originate_contract
      incr
      ~script:string_receiver
      ~storage:{|""|}
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, incr =
    publish_and_cement_dummy_commitment incr ~baker ~originator rollup
  in
  (* Create an atomic batch message. *)
  let transactions = [(string_receiver, Entrypoint.default, {|"Hello"|})] in
  let output = make_output ~outbox_level:0 ~message_index:1 transactions in
  (* Execute the message once - should succeed. *)
  let* Sc_rollup_operations.{operations; _}, incr =
    execute_outbox_message_without_proof_validation
      incr
      rollup
      ~cemented_commitment
      ~source
      output
  in
  (* Confirm that each transaction maps to one operation. *)
  let* () =
    verify_execute_outbox_message_operations
      ~loc:__LOC__
      incr
      ~source
      ~operations
      ~expected_transactions:transactions
  in
  (* Execute the same message again should fail. *)
  assert_fails
    ~loc:__LOC__
    ~error:Sc_rollup_errors.Sc_rollup_outbox_message_already_applied
    (execute_outbox_message_without_proof_validation
       incr
       rollup
       ~cemented_commitment
       ~source
       output)

let test_zero_amount_ticket () =
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  let source = Context.Contract.pkh originator in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup = sc_originate block originator "list (ticket string)" in
  let* incr = Incremental.begin_construction block in
  (* Originate a contract that accepts a pair of nat and ticket string input. *)
  let* ticket_receiver, incr =
    originate_contract
      incr
      ~script:ticket_receiver
      ~storage:"{}"
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, incr =
    publish_and_cement_dummy_commitment incr ~baker ~originator rollup
  in
  (* Create an atomic batch message. *)
  let transactions =
    [
      ( ticket_receiver,
        Entrypoint.default,
        {|Pair 42 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 0)|} );
    ]
  in
  let output = make_output ~outbox_level:0 ~message_index:0 transactions in
  assert_fails
    ~loc:__LOC__
    ~error:Ticket_scanner.Forbidden_zero_ticket_quantity
    (execute_outbox_message_without_proof_validation
       incr
       rollup
       ~cemented_commitment
       ~source
       output)

(* Check that executing an outbox message fails when the inclusion proof in
   invalid. *)
let test_invalid_output_proof () =
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup = sc_originate block originator "list (ticket string)" in
  let* incr = Incremental.begin_construction block in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, incr =
    publish_and_cement_dummy_commitment incr ~baker ~originator rollup
  in
  assert_fails
    ~loc:__LOC__
    ~error:Sc_rollup_operations.Sc_rollup_invalid_output_proof
    (execute_outbox_message
       incr
       rollup
       ~originator
       ~output_proof:"No good"
       ~commitment_hash:cemented_commitment)

let test_execute_message_override_applied_messages_slot () =
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  let source = Context.Contract.pkh originator in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup = sc_originate block originator "list (ticket string)" in
  let* incr = Incremental.begin_construction block in
  (* Originate a contract that accepts a pair of nat and ticket string input.  *)
  let* string_receiver, incr =
    originate_contract
      incr
      ~script:string_receiver
      ~storage:{|""|}
      ~source_contract:originator
      ~baker
  in
  let max_active_levels =
    Int32.to_int
      (Constants_storage.sc_rollup_max_active_outbox_levels
         (Alpha_context.Internal_for_tests.to_raw @@ Incremental.alpha_ctxt incr))
  in
  let execute_message incr ~outbox_level ~message_index
      ~cemented_commitment_hash =
    let transactions = [(string_receiver, Entrypoint.default, {|"Hello"|})] in
    let output = make_output ~outbox_level ~message_index transactions in
    let* Sc_rollup_operations.{operations = _; paid_storage_size_diff}, incr =
      execute_outbox_message_without_proof_validation
        incr
        rollup
        ~cemented_commitment:cemented_commitment_hash
        ~source
        output
    in
    return (paid_storage_size_diff, incr)
  in
  let* cemented_commitment = dummy_commitment incr rollup in
  let* cemented_commitment_hash, incr =
    publish_and_cement_commitment
      incr
      rollup
      ~baker
      ~originator
      cemented_commitment
  in
  (* Execute a message. *)
  let* _, incr =
    execute_message
      incr
      ~outbox_level:0
      ~message_index:0
      ~cemented_commitment_hash
  in
  (* Publish a bunch of commitments until the inbox level of the lcc is greater
     than [max_active_levels]. *)
  let* cemented_commitment_hash, incr =
    publish_commitments_until_min_inbox_level
      incr
      rollup
      ~baker
      ~originator
      ~min_inbox_level:(max_active_levels + 10)
      ~cemented_commitment_hash
      ~cemented_commitment
  in
  (* Execute the message again but at [max_active_levels] outbox-level. *)
  let* paid_storage_size_diff, incr =
    execute_message
      incr
      ~outbox_level:max_active_levels
      ~message_index:1
      ~cemented_commitment_hash
  in
  (* Since bitset has already been created for the slot, there should be no
     extra storage space. *)
  let* () = assert_equal_z ~loc:__LOC__ paid_storage_size_diff Z.zero in
  (* Execute a message at index 99. *)
  let* paid_storage_size_diff, incr =
    execute_message
      incr
      ~outbox_level:max_active_levels
      ~message_index:99
      ~cemented_commitment_hash
  in
  (* A message at slot 99 is now recorded which expands the size of the bitset.
     We therefore see an increase in size.
  *)
  let* () = assert_equal_z ~loc:__LOC__ paid_storage_size_diff (Z.of_int 14) in
  (* Execute at index 98. *)
  let* paid_storage_size_diff, incr =
    execute_message
      incr
      ~outbox_level:max_active_levels
      ~message_index:98
      ~cemented_commitment_hash
  in
  (* The bitset is not expanded so we don't pay anything. *)
  let* () = assert_equal_z ~loc:__LOC__ paid_storage_size_diff Z.zero in
  (* If we now try to record a message at level 0 it should fail since it
     expired. *)
  let* () =
    assert_fails
      ~loc:__LOC__
      ~error:Sc_rollup_operations.Sc_rollup_invalid_outbox_level
      (execute_message
         incr
         ~outbox_level:0
         ~message_index:0
         ~cemented_commitment_hash)
  in
  let* _paid_storage_size_diff, _incr =
    execute_message
      incr
      ~outbox_level:(max_active_levels + 5)
      ~message_index:0
      ~cemented_commitment_hash
  in
  (* This should fail even if no message exists for the corresponding slot.
     The reason is that outbox-level is smaller than the minimum level:
     [last-cemented-commitment-level - max-active-levels].
  *)
  let* () =
    assert_fails
      ~loc:__LOC__
      ~error:Sc_rollup_operations.Sc_rollup_invalid_outbox_level
      (execute_message
         incr
         ~outbox_level:1
         ~message_index:0
         ~cemented_commitment_hash)
  in
  return_unit

(** Test that a transaction fails if it attempts to transfer more tickets than
    allowed. *)
let test_insufficient_ticket_balances () =
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  let source = Context.Contract.pkh originator in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup = sc_originate block originator "list (ticket string)" in
  let* incr = Incremental.begin_construction block in
  (* Originate a contract that accepts a pair of nat and ticket string input. *)
  let* ticket_receiver, incr =
    originate_contract
      incr
      ~script:ticket_receiver
      ~storage:"{}"
      ~source_contract:originator
      ~baker
  in
  (* Originate a contract that accepts a string as input. *)
  let* string_receiver, incr =
    originate_contract
      incr
      ~script:string_receiver
      ~storage:{|""|}
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, incr =
    publish_and_cement_dummy_commitment incr ~baker ~originator rollup
  in
  (* Ticket-token with content "red". *)
  let* red_token =
    string_ticket_token "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red"
  in
  let transactions =
    [
      (* A transaction to the ticket-receiver contract. *)
      ( ticket_receiver,
        Entrypoint.default,
        {|Pair 1 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 4)|} );
      (* Another transaction to the ticket-receiver contract. *)
      ( ticket_receiver,
        Entrypoint.default,
        {|Pair 2 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 6)|} );
      (* A transaction to the string-receiver contract. *)
      (string_receiver, Entrypoint.default, {|"Hello"|});
      (* Another transaction to the string-receiver contract. *)
      (string_receiver, Entrypoint.default, {|"Hello again"|});
    ]
  in
  (* Create an atomic batch message. *)
  let output = make_output ~outbox_level:0 ~message_index:0 transactions in
  (* Set up the balance so that the rollup owns 7 units of red tokens.
     This is insufficient wrt the set of transactions above.
  *)
  let* incr = deposit_ticket_token incr rollup red_token (Z.of_int 7) in
  let* key, ctxt =
    wrap
      (Ticket_balance_key.of_ex_token
         (Incremental.alpha_ctxt incr)
         ~owner:(Destination.Sc_rollup rollup)
         red_token)
  in
  let incr = Incremental.set_alpha_ctxt incr ctxt in
  (* Executing the batch fails because the rollup only has 7 units of tickets
     but attempts to transfer 10 units. *)
  assert_fails
    ~loc:__LOC__
    ~error:
      (Ticket_balance.Negative_ticket_balance {key; balance = Z.of_int (-3)})
    (execute_outbox_message_without_proof_validation
       incr
       rollup
       ~cemented_commitment
       ~source
       output)

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
      "can publish a commit, cement it and withdraw stake"
      `Quick
      test_publish_cement_and_recover_bond;
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
    Tztest.tztest
      "originating with invalid types"
      `Quick
      test_originating_with_invalid_types;
    Tztest.tztest
      "originating with valid type"
      `Quick
      test_originating_with_valid_type;
    Tztest.tztest
      "single transaction atomic batch"
      `Quick
      test_single_transaction_batch;
    Tztest.tztest
      "multi-transaction atomic batch"
      `Quick
      test_multi_transaction_batch;
    Tztest.tztest
      "transaction with invalid type"
      `Quick
      test_transaction_with_invalid_type;
    Tztest.tztest "execute same message twice" `Quick test_execute_message_twice;
    Tztest.tztest
      "transaction with zero amount ticket"
      `Quick
      test_zero_amount_ticket;
    Tztest.tztest "invalid output proof" `Quick test_invalid_output_proof;
    Tztest.tztest
      "outbox message that overrides an old slot"
      `Quick
      test_execute_message_override_applied_messages_slot;
    Tztest.tztest
      "insufficient ticket balances"
      `Quick
      test_insufficient_ticket_balances;
  ]
