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
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_sc_rollup.ml
    Subject:      Test smart contract rollup
*)

open Protocol
open Alpha_context

exception Sc_rollup_test_error of string

let err x = Exn (Sc_rollup_test_error x)

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
          return_unit)

let assert_equal_z ~loc x y =
  Assert.equal ~loc Z.equal "Compare Z.t" Z.pp_print x y

let get_game_status_result incr =
  match Incremental.rev_tickets incr with
  | [] ->
      Stdlib.failwith
        "Failed to find an applied operation result in the metadata"
  | operations -> (
      List.find_map
        (function
          | Apply_results.Operation_metadata
              {
                contents =
                  Single_result
                    (Manager_operation_result {operation_result = Applied op; _});
              } -> (
              match op with
              | Sc_rollup_refute_result {game_status; _} ->
                  Some (game_status, `Refute)
              | Sc_rollup_timeout_result {game_status; _} ->
                  Some (game_status, `Timeout)
              | _ -> None)
          | _ -> None)
        operations
      |> function
      | None ->
          Stdlib.failwith
            "No operation [Sc_rollup_refute_result] or \
             [Sc_rollup_timeout_result] found"
      | Some x -> x)

let assert_equal_game_status ?game_status actual_game_status =
  let open Lwt_result_syntax in
  match game_status with
  | None -> return_unit
  | Some game_status ->
      if game_status = actual_game_status then return_unit
      else
        let msg =
          Format.asprintf
            "Expected game status [%a] but got [%a]"
            Sc_rollup.Game.pp_status
            game_status
            Sc_rollup.Game.pp_status
            actual_game_status
        in
        Stdlib.failwith msg

let assert_game_exists incr rollup game_index ~exists =
  let open Lwt_result_wrap_syntax in
  let ctxt = Incremental.alpha_ctxt incr in
  let*@ _ctxt, game_opt =
    Sc_rollup.Refutation_storage.find_game ctxt rollup game_index
  in
  match game_opt with
  | Some _ when exists -> return_unit
  | Some _ -> Stdlib.failwith "Found game but expected no game."
  | None when not exists -> return_unit
  | None -> Stdlib.failwith "No game game but expected one."

let assert_refute_result ?game_status incr rollup game_index =
  let open Lwt_result_syntax in
  let* () = assert_game_exists incr rollup game_index ~exists:false in
  let actual_game_status, op_type = get_game_status_result incr in
  assert (op_type = `Refute) ;
  assert_equal_game_status ?game_status actual_game_status

let assert_timeout_result ?game_status incr rollup game_index =
  let open Lwt_result_syntax in
  let* () = assert_game_exists incr rollup game_index ~exists:false in
  let actual_game_status, op_type = get_game_status_result incr in
  assert (op_type = `Timeout) ;
  assert_equal_game_status ?game_status actual_game_status

let bake_timeout_period ?timeout_period_in_blocks block =
  let open Lwt_result_syntax in
  let* timeout_period_in_blocks =
    match timeout_period_in_blocks with
    | Some v -> return v
    | None ->
        let* constants = Context.get_constants (B block) in
        let Constants.Parametric.{timeout_period_in_blocks; _} =
          constants.parametric.sc_rollup
        in
        return timeout_period_in_blocks
  in
  Block.bake_n timeout_period_in_blocks block

(** [context_init tup] initializes a context and returns the created
    context and contracts. *)
let context_init ?commitment_period_in_blocks
    ?(sc_rollup_challenge_window_in_blocks = 10)
    ?sc_rollup_max_active_outbox_levels ?(timeout_period_in_blocks = 10)
    ?hard_gas_limit_per_operation ?hard_gas_limit_per_block tup =
  Context.init_with_constants_gen
    tup
    {
      Context.default_test_constants with
      consensus_threshold_size = 0;
      hard_gas_limit_per_operation =
        Option.value
          hard_gas_limit_per_operation
          ~default:Context.default_test_constants.hard_gas_limit_per_operation;
      hard_gas_limit_per_block =
        Option.value
          hard_gas_limit_per_block
          ~default:Context.default_test_constants.hard_gas_limit_per_block;
      sc_rollup =
        {
          Context.default_test_constants.sc_rollup with
          arith_pvm_enable = true;
          private_enable = true;
          challenge_window_in_blocks = sc_rollup_challenge_window_in_blocks;
          max_active_outbox_levels =
            Option.value
              ~default:
                Context.default_test_constants.sc_rollup
                  .max_active_outbox_levels
              sc_rollup_max_active_outbox_levels;
          commitment_period_in_blocks =
            Option.value
              ~default:
                Context.default_test_constants.sc_rollup
                  .commitment_period_in_blocks
              commitment_period_in_blocks;
          timeout_period_in_blocks;
        };
    }

let bake_until_refutation_game_can_start block =
  (* A refutation game must not start too early: the protocol requires waiting
     for one commitment period to ensure sufficient time has passed since the
     commitment being challenged was published. This allows for potential
     successor commitments to be observed and provides a more stable foundation
     for dispute resolution. *)
  let open Lwt_result_syntax in
  let* constants = Context.get_constants (B block) in
  let commitment_period_in_blocks =
    constants.parametric.sc_rollup.commitment_period_in_blocks
  in
  Block.bake_n (commitment_period_in_blocks - 1) block

(** [test_disable_arith_pvm_feature_flag ()] tries to originate a Arith smart
    rollup when the Arith PVM feature flag is deactivated and checks that it
    fails. *)
let test_disable_arith_pvm_feature_flag () =
  let open Lwt_result_syntax in
  let* b, contract = Context.init1 ~sc_rollup_arith_pvm_enable:false () in
  let* i = Incremental.begin_construction b in
  let kind = Sc_rollup.Kind.Example_arith in
  let* op, _ = Sc_rollup_helpers.origination_op (B b) contract kind in
  let expect_failure = function
    | Environment.Ecoproto_error
        (Validate_errors.Manager.Sc_rollup_arith_pvm_disabled as e)
      :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ -> failwith "It should have failed with [Sc_rollup_arith_pvm_disabled]"
  in
  let* (_ : Incremental.t) = Incremental.add_operation ~expect_failure i op in
  return_unit

(** [test_disable_riscv_pvm_feature_flag ()] tries to originate a Riscv smart
    rollup when the Riscv PVM feature flag is deactivated and checks that it
    fails. *)
let test_disable_riscv_pvm_feature_flag () =
  let open Lwt_result_syntax in
  let* b, contract = Context.init1 ~sc_rollup_riscv_pvm_enable:false () in
  let* i = Incremental.begin_construction b in
  let kind = Sc_rollup.Kind.Riscv in
  let* op, _ = Sc_rollup_helpers.origination_op (B b) contract kind in
  let expect_failure = function
    | Environment.Ecoproto_error
        (Validate_errors.Manager.Sc_rollup_riscv_pvm_disabled as e)
      :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ -> failwith "It should have failed with [Sc_rollup_riscv_pvm_disabled]"
  in
  let* (_ : Incremental.t) = Incremental.add_operation ~expect_failure i op in
  return_unit

(** Initializes the context and originates a SCORU. *)
let sc_originate ?boot_sector ?parameters_ty ?whitelist block contract =
  let open Lwt_result_syntax in
  let kind = Sc_rollup.Kind.Example_arith in
  let* operation, rollup =
    Sc_rollup_helpers.origination_op
      ?boot_sector
      ?parameters_ty
      ?whitelist
      (B block)
      contract
      kind
  in
  let* block = Block.bake ~operation block in
  return (block, rollup)

(** Initializes the context and originates a SCORU. *)
let init_and_originate ?boot_sector ?parameters_ty ?commitment_period_in_blocks
    ?sc_rollup_challenge_window_in_blocks tup =
  let open Lwt_result_syntax in
  let* block, contracts =
    context_init
      ?commitment_period_in_blocks
      ?sc_rollup_challenge_window_in_blocks
      tup
  in
  let contract = Context.tup_hd tup contracts in
  let* block, rollup =
    sc_originate ?boot_sector ?parameters_ty block contract
  in
  return (block, contracts, rollup)

let number_of_ticks_exn n =
  match Sc_rollup.Number_of_ticks.of_value n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_ticks"

let next_inbox_level ?predecessor ctxt rollup =
  let open Lwt_result_syntax in
  let* genesis_info = Context.Sc_rollup.genesis_info ctxt rollup in
  let+ constants = Context.get_constants ctxt in
  let commitment_freq =
    constants.parametric.sc_rollup.commitment_period_in_blocks
  in
  let pred_level =
    Option.fold
      ~none:genesis_info.level
      ~some:(fun pred -> pred.Sc_rollup.Commitment.inbox_level)
      predecessor
  in
  Raw_level.add pred_level commitment_freq

let dummy_commitment ?predecessor ?compressed_state ?(number_of_ticks = 3000L)
    ?inbox_level ctxt rollup =
  let open Lwt_result_syntax in
  let* genesis_info = Context.Sc_rollup.genesis_info ctxt rollup in
  let predecessor_hash =
    match predecessor with
    | Some pred -> Sc_rollup.Commitment.hash_uncarbonated pred
    | None -> genesis_info.commitment_hash
  in
  let* compressed_state =
    match compressed_state with
    | None ->
        let* {compressed_state; _} =
          Context.Sc_rollup.commitment ctxt rollup predecessor_hash
        in
        return compressed_state
    | Some compressed_state -> return compressed_state
  in
  let* inbox_level =
    match inbox_level with
    | Some inbox_level -> return inbox_level
    | None -> next_inbox_level ?predecessor ctxt rollup
  in
  return
    Sc_rollup.Commitment.
      {
        predecessor = predecessor_hash;
        inbox_level;
        number_of_ticks = number_of_ticks_exn number_of_ticks;
        compressed_state;
      }

(* Bakes blocks to satisfy requirement of next_commitment.inbox_level <= current_level *)
let bake_blocks_until_next_inbox_level ?predecessor block rollup =
  let open Lwt_result_syntax in
  let* next_level = next_inbox_level ?predecessor (B block) rollup in
  Block.bake_until_level next_level block

let bake_blocks_until_inbox_level block commitment =
  Block.bake_until_level commitment.Sc_rollup.Commitment.inbox_level block

let publish_op_and_dummy_commitment ~sender ?compressed_state ?predecessor
    rollup block =
  let open Lwt_result_syntax in
  let compressed_state =
    Option.map
      (fun s ->
        Sc_rollup.State_hash.context_hash_to_state_hash
          (Context_hash.hash_string [s]))
      compressed_state
  in
  let* commitment =
    dummy_commitment ?compressed_state ?predecessor (B block) rollup
  in
  let* publish = Op.sc_rollup_publish (B block) sender rollup commitment in
  return (publish, commitment)

(* Verify that parameters and unparsed parameters match. *)
let verify_params ctxt ~parameters_ty ~parameters ~unparsed_parameters =
  let open Lwt_result_wrap_syntax in
  let show exp = Expr.to_string @@ exp in
  let unparse ctxt parameters =
    Script_ir_translator.unparse_data
      ctxt
      Script_ir_unparser.Optimized
      parameters_ty
      parameters
  in
  let*@ unparsed_parameters, ctxt =
    (* Make sure we can parse the unparsed-parameters with the given parameters
       type. *)
    let* parsed_unparsed_parameters, ctxt =
      Script_ir_translator.parse_data
        ctxt
        ~elab_conf:Script_ir_translator_config.(make ~legacy:true ())
        ~allow_forged_tickets:true
        ~allow_forged_lazy_storage_id:true
        parameters_ty
        (Environment.Micheline.root unparsed_parameters)
    in
    (* Un-parse again to get back to Micheline. *)
    unparse ctxt parsed_unparsed_parameters
  in
  (* Un-parse the parsed parameters. *)
  let*@ expected_unparsed_parameters, _ctxt = unparse ctxt parameters in
  (* Verify that both version match. *)
  Assert.equal_string
    ~loc:__LOC__
    (show unparsed_parameters)
    (show expected_unparsed_parameters)

(* Verify that the given list of transactions and transaction operations match.
   Also checks each transaction operation for type mismatches etc. *)
let verify_execute_outbox_message_operations ctxt rollup ~loc ~operations
    ~expected_transactions =
  let open Lwt_result_wrap_syntax in
  let* incr =
    Context.(
      match ctxt with
      | I incr -> return incr
      | B block -> Incremental.begin_construction block)
  in
  let alpha_ctxt = Incremental.alpha_ctxt incr in
  let validate_and_extract_operation_params ctxt op =
    match op with
    | Script_typed_ir.Internal_operation
        {
          sender = op_sender;
          operation =
            Transaction_to_smart_contract
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
          (* Check that the senders match. *)
          Assert.equal_string
            ~loc
            (Destination.to_b58check (Sc_rollup rollup))
            (Destination.to_b58check op_sender)
        in
        (* Assert that the amount is 0. *)
        let* () = Assert.equal_tez ~loc amount Tez.zero in
        (* Load the arg-type and entrypoints of the destination script. *)
        let* ( Script_ir_translator.Ex_script (Script {arg_type; entrypoints; _}),
               ctxt ) =
          let*@ ctxt, _cache_key, cached = Script_cache.find ctxt destination in
          match cached with
          | Some (_script, ex_script) -> return (ex_script, ctxt)
          | None -> failwith "Could not load script at %s" loc
        in
        (* Find the script parameters ty of the script. *)
        let*?@ entrypoint_res, ctxt =
          Gas_monad.run
            ctxt
            (Script_ir_translator.find_entrypoint
               ~error_details:(Informative ())
               arg_type
               entrypoints
               entrypoint)
        in
        let*?@ (Ex_ty_cstr {ty = script_parameters_ty; _}) = entrypoint_res in
        (* Check that the script parameters type matches the one from the
           transaction. *)
        let*?@ ctxt =
          let open Result_syntax in
          let* eq, ctxt =
            Gas_monad.run
              ctxt
              (Script_ir_translator.ty_eq
                 ~error_details:(Informative (-1))
                 script_parameters_ty
                 parameters_ty)
          in
          let+ Eq = eq in
          ctxt
        in
        return (ctxt, (destination, entrypoint, unparsed_parameters))
    | _ ->
        failwith
          "Expected an internal transaction operation to a smart-contract, \
           called from %s"
          loc
  in
  let* _alpha_ctxt, operations_data =
    List.fold_left_map_es
      validate_and_extract_operation_params
      alpha_ctxt
      operations
  in
  let compare_data (d1, e1, p1) (d2, e2, p2) =
    Contract_hash.equal d1 d2
    && Entrypoint_repr.(e1 = e2)
    && String.equal (Expr.to_string p1) (Expr.to_string p2)
  in
  let pp_data fmt (d, e, p) =
    Format.fprintf
      fmt
      "(%a, %a, %s)"
      Contract_hash.pp
      d
      Entrypoint_repr.pp
      e
      (Expr.to_string p)
  in
  let transactions_data =
    let data_of_transaction (contract, entrypoint, params) =
      let params = Expr.from_string params in
      (contract, entrypoint, params)
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

let verify_whitelist ~loc ~expected_whitelist rollup ctxt =
  let open Lwt_result_syntax in
  let* found_whitelist = Context.Sc_rollup.whitelist ctxt rollup in
  let sort_whitelist =
    Option.map (List.sort Signature.Public_key_hash.compare)
  in
  let found_sorted = sort_whitelist found_whitelist in
  let expected_sorted = sort_whitelist expected_whitelist in
  Assert.(assert_equal_list_opt ~loc Signature.Public_key_hash.equal)
    "whitelist"
    Signature.Public_key_hash.pp
    expected_sorted
    found_sorted

(* Helper functions to create output used for executing outbox messages. *)
let make_output ~outbox_level ~message_index message =
  let outbox_level = Raw_level.of_int32_exn (Int32.of_int outbox_level) in
  let message_index = Z.of_int message_index in
  Sc_rollup.{output_info = {outbox_level; message_index}; message}

let make_transaction_output ~outbox_level ~message_index transactions =
  let transactions =
    List.map
      (fun (destination, entrypoint, parameters) ->
        let unparsed_parameters = Expr.from_string parameters in
        {Sc_rollup.Outbox.Message.unparsed_parameters; destination; entrypoint})
      transactions
  in
  let message =
    Sc_rollup.Outbox.Message.Atomic_transaction_batch {transactions}
  in
  make_output ~outbox_level ~message_index message

let make_whitelist_update_output ~outbox_level ~message_index
    (whitelist_opt : Sc_rollup.Whitelist.t option) =
  make_output ~outbox_level ~message_index
  @@ Sc_rollup.Outbox.Message.Whitelist_update whitelist_opt

let string_ticket_token ticketer content =
  let open Lwt_result_wrap_syntax in
  let contents =
    Result.value_f ~default:(fun _ -> assert false)
    @@ Script_string.of_string content
  in
  let*?@ ticketer = Contract.of_b58check ticketer in
  return
    (Ticket_token.Ex_token
       {ticketer; contents_type = Script_typed_ir.string_t; contents})

let originate_contract block ~script ~baker ~storage ~source_contract =
  let open Lwt_result_syntax in
  let* contract, _, block =
    Contract_helpers.originate_contract_from_string_hash
      ~script
      ~storage
      ~source_contract
      ~baker
      block
  in
  return (contract, block)

let hash_commitment commitment =
  Sc_rollup.Commitment.hash_uncarbonated commitment

let publish_commitment source rollup block commitment =
  let open Lwt_result_syntax in
  let* block = bake_blocks_until_inbox_level block commitment in
  let* operation = Op.sc_rollup_publish (B block) source rollup commitment in
  Block.bake ~operation block

let publish_commitments block source rollup commitments =
  List.fold_left_es (publish_commitment source rollup) block commitments

let cement_commitment ?challenge_window_in_blocks block rollup staker =
  let open Lwt_result_syntax in
  let* challenge_window_in_blocks =
    match challenge_window_in_blocks with
    | Some x -> return x
    | None ->
        let* constants = Context.get_constants (B block) in
        return constants.parametric.sc_rollup.challenge_window_in_blocks
  in
  let* block = Block.bake_n challenge_window_in_blocks block in
  let* cement = Op.sc_rollup_cement (B block) staker rollup in
  Block.bake ~operation:cement block

let cement_commitments ?challenge_window_in_blocks block rollup staker hashes =
  (* [hashes] is useful to know the number of commitments we expect to cement. *)
  List.fold_left_es
    (fun block _hash ->
      cement_commitment ?challenge_window_in_blocks block rollup staker)
    block
    hashes

let publish_and_cement_commitment block ~originator rollup commitment =
  let open Lwt_result_wrap_syntax in
  let* block = publish_commitment originator rollup block commitment in
  let* constants = Context.get_constants (B block) in
  let* block =
    Block.bake_n constants.parametric.sc_rollup.challenge_window_in_blocks block
  in
  let hash = hash_commitment commitment in
  let* cement_op = Op.sc_rollup_cement (B block) originator rollup in
  let* block = Block.bake ~operation:cement_op block in
  return (hash, block)

let publish_and_cement_commitments block ~originator rollup commitments =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun block commitment ->
      let* _hash, block =
        publish_and_cement_commitment block ~originator rollup commitment
      in
      return block)
    block
    commitments

let publish_and_cement_dummy_commitment block ~originator rollup =
  let open Lwt_result_syntax in
  let* commitment = dummy_commitment (B block) rollup in
  publish_and_cement_commitment block ~originator rollup commitment

(* Publishes repeated cemented commitments until a commitment with
   [inbox_level >= min_inbox_level] is found (such a commitment
   is also published and cemented). *)
let publish_commitments_until_min_inbox_level block rollup ~originator
    ~min_inbox_level ~cemented_commitment_hash ~cemented_commitment =
  let rec aux block hash ({Sc_rollup.Commitment.inbox_level; _} as commitment) =
    let open Lwt_result_syntax in
    let level = Raw_level.to_int32 inbox_level in
    if level >= Int32.of_int min_inbox_level then return (hash, block)
    else
      let* commitment =
        dummy_commitment ~predecessor:commitment (B block) rollup
      in
      let* hash, block =
        publish_and_cement_commitment block ~originator rollup commitment
      in
      aux block hash commitment
  in
  aux block cemented_commitment_hash cemented_commitment

let adjust_ticket_token_balance_of_rollup block rollup ticket_token ~delta =
  let open Lwt_result_syntax in
  let* incr = Incremental.begin_construction block in
  let alpha_ctxt = Incremental.alpha_ctxt incr in
  let* hash, alpha_ctxt =
    Ticket_helpers.adjust_ticket_token_balance
      alpha_ctxt
      (Destination.Sc_rollup rollup)
      ticket_token
      ~delta
  in
  let incr = Incremental.set_alpha_ctxt incr alpha_ctxt in
  let* block = Incremental.finalize_block incr in
  return (hash, block)

(** A version of execute outbox message that output ignores proof validation. *)
let execute_outbox_message_without_proof_validation block rollup
    ~cemented_commitment outbox_message =
  let open Lwt_result_wrap_syntax in
  let* incr = Incremental.begin_construction block in
  let*@ res, alpha_ctxt =
    Sc_rollup_operations.Internal_for_tests.execute_outbox_message
      (Incremental.alpha_ctxt incr)
      ~validate_and_decode_output_proof:(fun
          ctxt ~cemented_commitment:_ _rollup ~output_proof:_ ->
        return (outbox_message, ctxt))
      rollup
      ~cemented_commitment
      ~output_proof:"Not used"
  in
  let incr = Incremental.set_alpha_ctxt incr alpha_ctxt in
  let* block = Incremental.finalize_block incr in
  return (res, block)

let execute_outbox_message block ~originator rollup ~output_proof
    ~commitment_hash =
  let open Lwt_result_syntax in
  let* batch_op =
    Op.sc_rollup_execute_outbox_message
      (B block)
      originator
      rollup
      commitment_hash
      ~output_proof
  in
  Block.bake ~operation:batch_op block

let assert_ticket_token_balance ~loc ctxt token owner expected =
  let open Lwt_result_wrap_syntax in
  let* incr =
    Context.(
      match ctxt with
      | I incr -> return incr
      | B block -> Incremental.begin_construction block)
  in
  let alpha_ctxt = Incremental.alpha_ctxt incr in
  let*@ balance, _ =
    let* key_hash, ctxt =
      Ticket_balance_key.of_ex_token alpha_ctxt ~owner token
    in
    Ticket_balance.get_balance ctxt key_hash
  in
  match (balance, expected) with
  | Some b, Some e -> Assert.equal_int ~loc (Z.to_int b) e
  | Some b, None ->
      failwith "%s: Expected no balance but got some %d" loc (Z.to_int b)
  | None, Some b -> failwith "%s: Expected balance %d but got none" loc b
  | None, None -> return_unit

(** Assert that the computation fails with the given message. *)
let assert_fails_with ~__LOC__ k expected_err =
  let open Lwt_result_syntax in
  let*! res = k in
  Assert.proto_error ~loc:__LOC__ res (( = ) expected_err)

let verify_can_publish_commit ~__LOC__ ~succeed rollup account block =
  let open Lwt_result_syntax in
  let* dummy_commitment = dummy_commitment (B block) rollup in
  let block_res = publish_commitment account rollup block dummy_commitment in
  if succeed then
    let* _block = block_res in
    return_unit
  else
    assert_fails_with
      ~__LOC__
      block_res
      Sc_rollup_errors.Sc_rollup_staker_not_in_whitelist

type balances = {liquid : Tez.t; frozen : Tez.t}

let balances ctxt contract =
  let open Lwt_result_syntax in
  let* liquid = Context.Contract.balance ctxt contract in
  let* frozen = Context.Contract.frozen_bonds ctxt contract in
  return {liquid; frozen}

let check_balances_evolution bal_before {liquid; frozen} ~action =
  let open Lwt_result_wrap_syntax in
  let* {liquid = expected_liquid; frozen = expected_frozen} =
    match action with
    | `Freeze amount ->
        let*?@ liquid = Tez.( -? ) bal_before.liquid amount in
        let*?@ frozen = Tez.( +? ) bal_before.frozen amount in
        return {liquid; frozen}
    | `Unfreeze amount ->
        let*?@ liquid = Tez.( +? ) bal_before.liquid amount in
        let*?@ frozen = Tez.( -? ) bal_before.frozen amount in
        return {liquid; frozen}
  in
  let* () = Assert.equal_tez ~loc:__LOC__ expected_liquid liquid in
  let* () = Assert.equal_tez ~loc:__LOC__ expected_frozen frozen in
  return_unit

(* Generates a list of cemented dummy commitments. *)
let gen_commitments ctxt rollup ~predecessor ~num_commitments =
  let open Lwt_result_syntax in
  let* constants = Context.get_constants ctxt in
  let delta = constants.parametric.sc_rollup.commitment_period_in_blocks in
  let rec aux predecessor n acc =
    if n <= 0 then return (List.rev acc)
    else
      let inbox_level =
        Raw_level.add predecessor.Sc_rollup.Commitment.inbox_level delta
      in
      let* commitment =
        dummy_commitment
          ~predecessor
          ~inbox_level
          ~compressed_state:predecessor.compressed_state
          ctxt
          rollup
      in
      let hash = Sc_rollup.Commitment.hash_uncarbonated commitment in
      aux commitment (n - 1) ((commitment, hash) :: acc)
  in
  aux predecessor num_commitments []

let attempt_to_recover_bond ?policy block contract ?staker rollup =
  let open Lwt_result_syntax in
  (* Recover its own bond by default. *)
  let staker =
    match staker with
    | Some staker -> staker
    | None -> Account.pkh_of_contract_exn contract
  in
  let* recover_bond_op =
    Op.sc_rollup_recover_bond (B block) contract rollup staker
  in
  Block.bake ?policy ~operation:recover_bond_op block

let recover_bond_not_lcc block contract rollup =
  assert_fails_with
    ~__LOC__
    (attempt_to_recover_bond block contract rollup)
    Sc_rollup_errors.Sc_rollup_not_staked_on_lcc_or_ancestor

let recover_bond_not_staked block contract rollup =
  assert_fails_with
    ~__LOC__
    (attempt_to_recover_bond block contract rollup)
    Sc_rollup_errors.Sc_rollup_not_staked

let recover_bond_with_success ?policy block contract rollup =
  let open Lwt_result_syntax in
  let* bal_before = balances (B block) contract in
  let* b = attempt_to_recover_bond ?policy block contract rollup in
  let* bal_after = balances (B b) contract in
  let* constants = Context.get_constants (B b) in
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
  let open Lwt_result_wrap_syntax in
  let* block, contracts, rollup = init_and_originate Context.T2 in
  let _, contract = contracts in
  let* block = bake_blocks_until_next_inbox_level block rollup in
  (* not staked yet *)
  let* () = recover_bond_not_staked block contract rollup in
  let* c = dummy_commitment (B block) rollup in
  let* operation = Op.sc_rollup_publish (B block) contract rollup c in
  let* b = Block.bake ~operation block in
  let* constants = Context.get_constants (B b) in
  let* b =
    Block.bake_n constants.parametric.sc_rollup.challenge_window_in_blocks b
  in
  (* stake not on LCC *)
  let* () = recover_bond_not_lcc b contract rollup in
  let* cement_op = Op.sc_rollup_cement (B b) contract rollup in
  let* b = Block.bake ~operation:cement_op b in
  (* recover bond should succeed *)
  let* b =
    recover_bond_with_success
    (* We forbid the stake owner from baker to correctly check the
       unfrozen amount below. *)
      ~policy:(Excluding [Account.pkh_of_contract_exn contract])
      b
      contract
      rollup
  in
  (* not staked anymore *)
  let* () = recover_bond_not_staked b contract rollup in
  return_unit

(** [test_publish_fails_on_double_stake] creates a rollup and then
    publishes two different commitments with the same staker. We check
    that the second publish fails. *)
let test_publish_fails_on_double_stake () =
  let open Lwt_result_syntax in
  let* block, contracts, rollup = init_and_originate Context.T2 in
  let* block = bake_blocks_until_next_inbox_level block rollup in
  let _, contract = contracts in
  let* commitment1 = dummy_commitment (B block) rollup in
  let commitment2 =
    {commitment1 with number_of_ticks = number_of_ticks_exn 3001L}
  in
  let* operation1 =
    Op.sc_rollup_publish (B block) contract rollup commitment1
  in
  let* block = Block.bake ~operation:operation1 block in
  let* operation2 =
    Op.sc_rollup_publish (B block) contract rollup commitment2
  in
  assert_fails_with
    ~__LOC__
    (Block.bake ~operation:operation2 block)
    Sc_rollup_errors.Sc_rollup_staker_double_stake

(** [test_cement_fails_on_conflict] creates a rollup and then publishes
    two different commitments. It waits 20 blocks and then attempts to
    cement one of the commitments; it checks that this fails because the
    commitment is contested. *)
let test_cement_fails_on_conflict () =
  let open Lwt_result_wrap_syntax in
  let* b, contracts, rollup = init_and_originate Context.T3 in
  let* b = bake_blocks_until_next_inbox_level b rollup in
  let _, contract1, contract2 = contracts in
  let* commitment1 = dummy_commitment (B b) rollup in
  let commitment2 =
    {commitment1 with number_of_ticks = number_of_ticks_exn 3001L}
  in
  let* operation1 = Op.sc_rollup_publish (B b) contract1 rollup commitment1 in
  let* b = Block.bake ~operation:operation1 b in
  let* operation2 = Op.sc_rollup_publish (B b) contract2 rollup commitment2 in
  let* b = Block.bake ~operation:operation2 b in
  let* constants = Context.get_constants (B b) in
  let* b =
    Block.bake_n constants.parametric.sc_rollup.challenge_window_in_blocks b
  in
  let* cement_op = Op.sc_rollup_cement (B b) contract1 rollup in
  let block_res = Block.bake ~operation:cement_op b in
  assert_fails_with ~__LOC__ block_res Sc_rollup_errors.Sc_rollup_disputed

let commit_and_cement_after_n_bloc ?expected_error b contract rollup n =
  let open Lwt_result_wrap_syntax in
  let* b = bake_blocks_until_next_inbox_level b rollup in
  let* commitment = dummy_commitment (B b) rollup in
  let* operation = Op.sc_rollup_publish (B b) contract rollup commitment in
  let* b = Block.bake ~operation b in
  (* This pattern would add an additional block, so we decrement [n] by one. *)
  let* b = Block.bake_n (n - 1) b in
  let* cement_op = Op.sc_rollup_cement (B b) contract rollup in
  let block = Block.bake ~operation:cement_op b in
  match expected_error with
  | Some error -> assert_fails_with ~__LOC__ block error
  | None ->
      let* _block = block in
      return_unit

(** [test_challenge_window_period_boundaries] checks that cementing a commitment
    without waiting for the whole challenge window period fails. Whereas,
    succeeds when the period is over. *)
let test_challenge_window_period_boundaries () =
  let commitment_period_in_blocks = 10 in
  let sc_rollup_challenge_window_in_blocks = 10 in
  let open Lwt_result_syntax in
  let* block, contract, rollup =
    init_and_originate
      ~commitment_period_in_blocks
      ~sc_rollup_challenge_window_in_blocks
      Context.T1
  in
  (* Should fail because the waiting period is not strictly greater than the
     challenge window period. *)
  let* () =
    let*? current_level = Context.get_level (B block) in
    let level_of_cement_submit =
      Int32.to_int (Raw_level.to_int32 current_level)
      + commitment_period_in_blocks + sc_rollup_challenge_window_in_blocks
      |> Int32.of_int |> Raw_level_repr.of_int32_exn
    in
    let min_level = Raw_level_repr.succ level_of_cement_submit in
    commit_and_cement_after_n_bloc
      ~expected_error:
        (Sc_rollup_errors.Sc_rollup_commitment_too_recent
           {current_level = level_of_cement_submit; min_level})
      block
      contract
      rollup
      (sc_rollup_challenge_window_in_blocks - 1)
  in
  (* Succeeds because the challenge period is over. *)
  let* () =
    commit_and_cement_after_n_bloc
      block
      contract
      rollup
      sc_rollup_challenge_window_in_blocks
  in
  return_unit

(** Test originating with bad type. *)
let test_originating_with_invalid_types () =
  let open Lwt_result_syntax in
  let* block, (contract, _, _) = context_init Context.T3 in
  let assert_fails_for_type parameters_ty =
    assert_fails
      ~loc:__LOC__
      ~error:Sc_rollup_operations.Sc_rollup_invalid_parameters_type
      (sc_originate block contract ~parameters_ty)
  in
  (* Following types fail at validation time. *)
  let* () =
    [
      "mutez";
      "big_map string nat";
      "sapling_state 2";
      "sapling_transaction 2";
      "lambda string nat";
      "or (nat %deposit) (string %name)";
    ]
    |> List.iter_es assert_fails_for_type
  in
  (* Operation fails with a different error as it's not "passable". *)
  assert_fails
    ~loc:__LOC__
    (sc_originate block contract ~parameters_ty:"operation")

let assert_equal_expr ~loc e1 e2 =
  let s1 = Format.asprintf "%a" Michelson_v1_printer.print_expr e1 in
  let s2 = Format.asprintf "%a" Michelson_v1_printer.print_expr e2 in
  Assert.equal_string ~loc s1 s2

let test_originating_with_valid_type () =
  let open Lwt_result_wrap_syntax in
  let* block, contract = context_init Context.T1 in
  let assert_parameters_ty parameters_ty =
    let* block, rollup = sc_originate block contract ~parameters_ty in
    let* incr = Incremental.begin_construction block in
    let ctxt = Incremental.alpha_ctxt incr in
    let*@ expr, _ctxt = Sc_rollup.parameters_type ctxt rollup in
    let expr = WithExceptions.Option.get ~loc:__LOC__ expr in
    let*?@ expr, _ctxt =
      Script.force_decode_in_context
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
    "chain_id";
    "ticket string";
    "set nat";
    "option (ticket string)";
    "list nat";
    "pair nat unit";
    "or nat string";
    "map string int";
    "map (option (pair nat string)) (list (ticket nat))";
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
  let open Lwt_result_syntax in
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup =
    sc_originate block originator ~parameters_ty:"list (ticket string)"
  in
  (* Originate a contract that accepts a pair of nat and ticket string input.  *)
  let* ticket_receiver, block =
    originate_contract
      block
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
  let* cemented_commitment, block =
    publish_and_cement_dummy_commitment block ~originator rollup
  in
  (* Create an atomic batch message. *)
  let transactions =
    [
      ( ticket_receiver,
        Entrypoint.default,
        {|Pair 42 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1)|} );
    ]
  in
  let output =
    make_transaction_output ~outbox_level:0 ~message_index:0 transactions
  in
  (* Set up the balance so that the self contract owns one ticket. *)
  let* _ticket_hash, block =
    adjust_ticket_token_balance_of_rollup block rollup red_token ~delta:Z.one
  in
  let* Sc_rollup_operations.{operations; _}, block =
    execute_outbox_message_without_proof_validation
      block
      rollup
      ~cemented_commitment
      output
  in
  (* Confirm that each transaction maps to one operation. *)
  let* () =
    verify_execute_outbox_message_operations
      ~loc:__LOC__
      (B block)
      rollup
      ~operations
      ~expected_transactions:transactions
  in
  (* Verify that the balance has moved to ticket-receiver. *)
  let* () =
    assert_ticket_token_balance
      ~loc:__LOC__
      (B block)
      red_token
      (Destination.Sc_rollup rollup)
      None
  in
  assert_ticket_token_balance
    ~loc:__LOC__
    (B block)
    red_token
    (Destination.Contract (Originated ticket_receiver))
    (Some 1)

(** Test that checks that an outbox message can be executed against all stored
    cemented commitments but not against an outdated one. *)
let test_older_cemented_commitment () =
  let open Lwt_result_syntax in
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup =
    sc_originate block originator ~parameters_ty:"list (ticket string)"
  in
  (* Originate a contract that accepts a pair of nat and ticket string input.  *)
  let* ticket_receiver, block =
    originate_contract
      block
      ~script:ticket_receiver
      ~storage:"{}"
      ~source_contract:originator
      ~baker
  in
  (* Ticket-token with content "red". *)
  let* red_token =
    string_ticket_token "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red"
  in
  let verify_outbox_message_execution block cemented_commitment =
    (* Set up the balance so that the self contract owns one ticket. *)
    let* _ticket_hash, block =
      adjust_ticket_token_balance_of_rollup block rollup red_token ~delta:Z.one
    in
    (* Create an atomic batch message. *)
    let transactions =
      [
        ( ticket_receiver,
          Entrypoint.default,
          {|Pair 42 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1)|} );
      ]
    in
    let output =
      make_transaction_output ~outbox_level:0 ~message_index:0 transactions
    in
    let* Sc_rollup_operations.{operations; _}, block =
      execute_outbox_message_without_proof_validation
        block
        rollup
        ~cemented_commitment
        output
    in
    (* Confirm that each transaction maps to one operation. *)
    let* () =
      verify_execute_outbox_message_operations
        ~loc:__LOC__
        (B block)
        rollup
        ~operations
        ~expected_transactions:transactions
    in
    (* Verify that the balance has moved to ticket-receiver. *)
    let* () =
      assert_ticket_token_balance
        ~loc:__LOC__
        (B block)
        red_token
        (Destination.Sc_rollup rollup)
        None
    in
    assert_ticket_token_balance
      ~loc:__LOC__
      (B block)
      red_token
      (Destination.Contract (Originated ticket_receiver))
      (Some 1)
  in
  let* max_num_stored_cemented_commitments =
    let* constants = Context.get_constants (B block) in
    return
      constants.parametric.sc_rollup.max_number_of_stored_cemented_commitments
  in
  (* Publish and cement a commitment. *)
  let* first_commitment_hash, block =
    publish_and_cement_dummy_commitment block ~originator rollup
  in
  let* first_commitment =
    Context.Sc_rollup.commitment (B block) rollup first_commitment_hash
  in
  (* Generate a list of commitments that exceed the maximum number of stored
     ones by one. *)
  let* commitments_and_hashes =
    gen_commitments
      (B block)
      rollup
      ~predecessor:first_commitment
      ~num_commitments:(max_num_stored_cemented_commitments + 1)
  in
  let commitments, commitment_hashes = List.split commitments_and_hashes in
  let* block =
    publish_and_cement_commitments block ~originator rollup commitments
  in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4469
     The test actually do not test the good "too old" commitment. *)
  let commitment_hashes = first_commitment_hash :: commitment_hashes in
  match commitment_hashes with
  | too_old_commitment :: stored_hashes ->
      (* Executing outbox message for the old non-stored commitment should fail. *)
      let* () =
        assert_fails
          ~loc:__LOC__
          ~error:Sc_rollup_operations.Sc_rollup_invalid_last_cemented_commitment
          (verify_outbox_message_execution block too_old_commitment)
      in
      (* Executing outbox message for the recent ones should succeed. *)
      List.iter_es (verify_outbox_message_execution block) stored_hashes
  | _ -> failwith "Expected non-empty list of commitment hashes."

let test_multi_transaction_batch () =
  let open Lwt_result_syntax in
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup =
    sc_originate block originator ~parameters_ty:"list (ticket string)"
  in
  (* Originate a contract that accepts a pair of nat and ticket string input. *)
  let* ticket_receiver, block =
    originate_contract
      block
      ~script:ticket_receiver
      ~storage:"{}"
      ~source_contract:originator
      ~baker
  in
  (* Originate a contract that accepts a string as input. *)
  let* string_receiver, block =
    originate_contract
      block
      ~script:string_receiver
      ~storage:{|""|}
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, block =
    publish_and_cement_dummy_commitment block ~originator rollup
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
  let output =
    make_transaction_output ~outbox_level:0 ~message_index:0 transactions
  in
  (* Set up the balance so that the rollup owns 10 units of red tokens. *)
  let* _ticket_hash, block =
    adjust_ticket_token_balance_of_rollup
      block
      rollup
      red_token
      ~delta:(Z.of_int 10)
  in
  let* Sc_rollup_operations.{operations; _}, block =
    execute_outbox_message_without_proof_validation
      block
      rollup
      ~cemented_commitment
      output
  in
  (* Confirm that each transaction maps to one operation. *)
  let* () =
    verify_execute_outbox_message_operations
      ~loc:__LOC__
      (B block)
      rollup
      ~operations
      ~expected_transactions:transactions
  in
  (* Verify that the balance has moved to ticket-receiver. *)
  let* () =
    assert_ticket_token_balance
      ~loc:__LOC__
      (B block)
      red_token
      (Destination.Sc_rollup rollup)
      None
  in
  assert_ticket_token_balance
    ~loc:__LOC__
    (B block)
    red_token
    (Destination.Contract (Originated ticket_receiver))
    (Some 10)

(** Test that executing an L2 to L1 transaction that involves an invalid
    parameter (mutez) fails. *)
let test_transaction_with_invalid_type () =
  let open Lwt_result_syntax in
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  let* block, rollup =
    sc_originate block originator ~parameters_ty:"list (ticket string)"
  in
  let* mutez_receiver, block =
    originate_contract
      block
      ~script:mutez_receiver
      ~storage:"0"
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, block =
    publish_and_cement_dummy_commitment block ~originator rollup
  in
  let transactions = [(mutez_receiver, Entrypoint.default, "12")] in
  (* Create an atomic batch message. *)
  let output =
    make_transaction_output ~outbox_level:0 ~message_index:1 transactions
  in
  assert_fails
    ~loc:__LOC__
    ~error:Sc_rollup_operations.Sc_rollup_invalid_parameters_type
    (execute_outbox_message_without_proof_validation
       block
       rollup
       ~cemented_commitment
       output)

(** Test that executing the same outbox message for the same twice fails. *)
let test_execute_message_twice () =
  let open Lwt_result_syntax in
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup =
    sc_originate block originator ~parameters_ty:"list (ticket string)"
  in
  (* Originate a contract that accepts a pair of nat and ticket string input.  *)
  let* string_receiver, block =
    originate_contract
      block
      ~script:string_receiver
      ~storage:{|""|}
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, block =
    publish_and_cement_dummy_commitment block ~originator rollup
  in
  (* Create an atomic batch message. *)
  let transactions = [(string_receiver, Entrypoint.default, {|"Hello"|})] in
  let output =
    make_transaction_output ~outbox_level:0 ~message_index:1 transactions
  in
  (* Execute the message once - should succeed. *)
  let* Sc_rollup_operations.{operations; _}, block =
    execute_outbox_message_without_proof_validation
      block
      rollup
      ~cemented_commitment
      output
  in
  (* Confirm that each transaction maps to one operation. *)
  let* () =
    verify_execute_outbox_message_operations
      ~loc:__LOC__
      (B block)
      rollup
      ~operations
      ~expected_transactions:transactions
  in
  (* Execute the same message again should fail. *)
  assert_fails
    ~loc:__LOC__
    ~error:Sc_rollup_errors.Sc_rollup_outbox_message_already_applied
    (execute_outbox_message_without_proof_validation
       block
       rollup
       ~cemented_commitment
       output)

(** Verifies that it is not possible to execute the same message twice from
    different commitments. *)
let test_execute_message_twice_different_cemented_commitments () =
  let open Lwt_result_syntax in
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup =
    sc_originate block originator ~parameters_ty:"list (ticket string)"
  in
  (* Originate a contract that accepts a pair of nat and ticket string input.  *)
  let* string_receiver, block =
    originate_contract
      block
      ~script:string_receiver
      ~storage:{|""|}
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* first_cemented_commitment, block =
    publish_and_cement_dummy_commitment block ~originator rollup
  in
  let* predecessor =
    Context.Sc_rollup.commitment (B block) rollup first_cemented_commitment
  in
  let* commitment = dummy_commitment ~predecessor (B block) rollup in
  let* second_cemented_commitment, block =
    publish_and_cement_commitment block ~originator rollup commitment
  in
  (* Create an atomic batch message. *)
  let transactions = [(string_receiver, Entrypoint.default, {|"Hello"|})] in
  let output =
    make_transaction_output ~outbox_level:0 ~message_index:1 transactions
  in
  (* Execute the message once - should succeed. *)
  let* Sc_rollup_operations.{operations; _}, block =
    execute_outbox_message_without_proof_validation
      block
      rollup
      ~cemented_commitment:first_cemented_commitment
      output
  in
  (* Confirm that each transaction maps to one operation. *)
  let* () =
    verify_execute_outbox_message_operations
      ~loc:__LOC__
      (B block)
      rollup
      ~operations
      ~expected_transactions:transactions
  in
  (* Execute the same message again should fail. *)
  assert_fails
    ~loc:__LOC__
    ~error:Sc_rollup_errors.Sc_rollup_outbox_message_already_applied
    (execute_outbox_message_without_proof_validation
       block
       rollup
       ~cemented_commitment:second_cemented_commitment
       output)

let test_zero_amount_ticket () =
  let open Lwt_result_syntax in
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup =
    sc_originate block originator ~parameters_ty:"list (ticket string)"
  in
  (* Originate a contract that accepts a pair of nat and ticket string input. *)
  let* ticket_receiver, block =
    originate_contract
      block
      ~script:ticket_receiver
      ~storage:"{}"
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, block =
    publish_and_cement_dummy_commitment block ~originator rollup
  in
  (* Create an atomic batch message. *)
  let transactions =
    [
      ( ticket_receiver,
        Entrypoint.default,
        {|Pair 42 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 0)|} );
    ]
  in
  let output =
    make_transaction_output ~outbox_level:0 ~message_index:0 transactions
  in
  let*! result =
    execute_outbox_message_without_proof_validation
      block
      rollup
      ~cemented_commitment
      output
  in
  match result with
  | Error e ->
      if
        Option.is_some
        @@ List.find
             (function
               | Environment.Ecoproto_error
                   Script_tc_errors.Forbidden_zero_ticket_quantity ->
                   true
               | _ -> false)
             e
      then return_unit
      else Stdlib.failwith "Expected failure"
  | Ok _ -> Stdlib.failwith "Expected failure"

(* Check that executing an outbox message fails when the inclusion proof in
   invalid. *)
let test_invalid_output_proof () =
  let open Lwt_result_syntax in
  let* block, originator = context_init Context.T1 in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup =
    sc_originate block originator ~parameters_ty:"list (ticket string)"
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, block =
    publish_and_cement_dummy_commitment block ~originator rollup
  in
  assert_fails
    ~loc:__LOC__
    ~error:Sc_rollup_operations.Sc_rollup_invalid_output_proof
    (execute_outbox_message
       block
       rollup
       ~originator
       ~output_proof:"No good"
       ~commitment_hash:cemented_commitment)

let test_execute_message_override_applied_messages_slot () =
  let open Lwt_result_syntax in
  (* Since we will create more blocks than the [max_active_outbox_levels]
     parametric constant, we initialize it with a small enough value. *)
  let* block, (baker, originator) =
    context_init
      ~sc_rollup_max_active_outbox_levels:100l
      ~commitment_period_in_blocks:50
      Context.T2
  in
  let baker = Context.Contract.pkh baker in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup =
    sc_originate block originator ~parameters_ty:"list (ticket string)"
  in
  (* Originate a contract that accepts a pair of nat and ticket string input.  *)
  let* string_receiver, block =
    originate_contract
      block
      ~script:string_receiver
      ~storage:{|""|}
      ~source_contract:originator
      ~baker
  in
  let* constants = Context.get_constants (B block) in
  let max_active_levels =
    constants.parametric.sc_rollup.max_active_outbox_levels |> Int32.to_int
  in
  let execute_message incr ~outbox_level ~message_index
      ~cemented_commitment_hash =
    let transactions = [(string_receiver, Entrypoint.default, {|"Hello"|})] in
    let output =
      make_transaction_output ~outbox_level ~message_index transactions
    in
    let* ( Sc_rollup_operations.
             {
               operations = _;
               ticket_receipt = _;
               whitelist_update = _;
               paid_storage_size_diff;
             },
           incr ) =
      execute_outbox_message_without_proof_validation
        incr
        rollup
        ~cemented_commitment:cemented_commitment_hash
        output
    in
    return (paid_storage_size_diff, incr)
  in
  let* cemented_commitment = dummy_commitment (B block) rollup in
  let* cemented_commitment_hash, block =
    publish_and_cement_commitment block rollup ~originator cemented_commitment
  in
  (* Execute a message. *)
  let* _, block =
    execute_message
      block
      ~outbox_level:0
      ~message_index:0
      ~cemented_commitment_hash
  in
  (* Publish a bunch of commitments until the inbox level of the lcc is greater
     than [max_active_levels]. *)
  let* cemented_commitment_hash, block =
    publish_commitments_until_min_inbox_level
      block
      rollup
      ~originator
      ~min_inbox_level:(max_active_levels + 10)
      ~cemented_commitment_hash
      ~cemented_commitment
  in
  (* Execute the message again but at [max_active_levels] outbox-level. *)
  let* paid_storage_size_diff, incr =
    execute_message
      block
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
  let open Lwt_result_syntax in
  let* block, (baker, originator) = context_init Context.T2 in
  let baker = Context.Contract.pkh baker in
  (* Originate a rollup that accepts a list of string tickets as input. *)
  let* block, rollup =
    sc_originate block originator ~parameters_ty:"list (ticket string)"
  in
  (* Originate a contract that accepts a pair of nat and ticket string input. *)
  let* ticket_receiver, block =
    originate_contract
      block
      ~script:ticket_receiver
      ~storage:"{}"
      ~source_contract:originator
      ~baker
  in
  (* Originate a contract that accepts a string as input. *)
  let* string_receiver, block =
    originate_contract
      block
      ~script:string_receiver
      ~storage:{|""|}
      ~source_contract:originator
      ~baker
  in
  (* Publish and cement a commitment. *)
  let* cemented_commitment, block =
    publish_and_cement_dummy_commitment block ~originator rollup
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
  let output =
    make_transaction_output ~outbox_level:0 ~message_index:0 transactions
  in
  (* Set up the balance so that the rollup owns 7 units of red tokens.
     This is insufficient wrt the set of transactions above.
  *)
  let* ticket_hash, incr =
    adjust_ticket_token_balance_of_rollup
      block
      rollup
      red_token
      ~delta:(Z.of_int 7)
  in
  (* Executing the batch fails because the rollup only has 7 units of tickets
     but attempts to transfer 10 units. *)
  assert_fails
    ~loc:__LOC__
    ~error:
      (Ticket_balance.Negative_ticket_balance
         {key = ticket_hash; balance = Z.of_int (-3)})
    (execute_outbox_message_without_proof_validation
       incr
       rollup
       ~cemented_commitment
       output)

let test_inbox_max_number_of_messages_per_level () =
  let open Lwt_result_syntax in
  let* block, (account1, account2) =
    (* set sort of unlimited gas or we are going to hit gas exhaustion. *)
    context_init
      ~hard_gas_limit_per_operation:(Gas.Arith.integral_of_int_exn 100_000_000)
      ~hard_gas_limit_per_block:
        (Gas.Arith.integral_of_int_exn Int.(max_int / 1000))
      Context.T2
  in
  (* we need to bake block because we are at the migration level and there is an additional msg in the inbox *)
  let* block = Block.bake block in
  let max_number_of_messages_per_level =
    Constants.sc_rollup_max_number_of_messages_per_level
  in
  let* incr = Incremental.begin_construction block in
  (* This just one message below the limit *)
  let messages =
    List.repeat (Z.to_int max_number_of_messages_per_level) "foo"
  in
  let* op =
    Op.sc_rollup_add_messages ~gas_limit:Max (I incr) account1 messages
  in
  let* incr = Incremental.add_operation ~check_size:false incr op in
  (* This break the limit *)
  let* op = Op.sc_rollup_add_messages (I incr) account2 ["foo"] in
  let* (_incr : Incremental.t) =
    let expect_apply_failure = function
      | Environment.Ecoproto_error
          (Sc_rollup_inbox_repr.Inbox_level_reached_messages_limit as e)
        :: _ ->
          Assert.test_error_encodings e ;
          return_unit
      | _ ->
          failwith
            "It should have failed with [Inbox_level_reached_messages_limit]"
    in

    Incremental.add_operation ~expect_apply_failure incr op
  in
  return_unit

let add_publish ~rollup block account commitment =
  let open Lwt_result_syntax in
  let* block = bake_blocks_until_inbox_level block commitment in
  let* publish = Op.sc_rollup_publish (B block) account rollup commitment in
  Block.bake ~operation:publish block

(** [test_number_of_parallel_games_bounded] checks that one cannot
    play an arbitrary number of games. *)
let test_number_of_parallel_games_bounded () =
  let open Lwt_result_syntax in
  let max_number_of_parallel_games =
    Context.default_test_constants.sc_rollup.max_number_of_parallel_games
  in
  let nb_accounts = max_number_of_parallel_games + 2 in
  let* block, accounts =
    context_init
      ~sc_rollup_challenge_window_in_blocks:100
      ~hard_gas_limit_per_block:(Gas.Arith.integral_of_int_exn 1_000_000_000)
      (Context.TList nb_accounts)
  in
  let* block, rollup = sc_originate block (Stdlib.List.hd accounts) in
  let* dummy_commitment = dummy_commitment (B block) rollup in

  let commitments =
    List.mapi
      (fun i _ ->
        {
          dummy_commitment with
          number_of_ticks = number_of_ticks_exn (Int64.of_int (i + 1));
        })
      accounts
  in
  let* block =
    List.fold_left2_es
      ~when_different_lengths:[]
      (fun block account commitment ->
        add_publish ~rollup block account commitment)
      block
      accounts
      commitments
  in
  let staker, opponents =
    match accounts with
    | staker :: opponents -> (staker, opponents)
    | [] ->
        (* Because [max_number_of_parallel_games] is strictly positive. *)
        assert false
  in
  let staker_commitment, opponents_commitments =
    match commitments with
    | staker_commitment :: opponents_commitments ->
        (staker_commitment, opponents_commitments)
    | [] ->
        (* Because [max_number_of_parallel_games] is strictly positive. *)
        assert false
  in
  let expect_apply_failure = function
    | Environment.Ecoproto_error
        (Sc_rollup_errors.Sc_rollup_max_number_of_parallel_games_reached xstaker)
      :: _ ->
        assert (
          Signature.Public_key_hash.(
            xstaker = Account.pkh_of_contract_exn staker)) ;
        return_unit
    | _ ->
        failwith
          "It should have failed with \
           [Sc_rollup_max_number_of_parallel_games_reached]"
  in
  let* block = bake_until_refutation_game_can_start block in
  let* incr = Incremental.begin_construction block in
  let* _block, _counter =
    List.fold_left2_es
      ~when_different_lengths:[]
      (fun (block, counter) opponent opponent_commitment ->
        let addr = Account.pkh_of_contract_exn staker in
        let refutation =
          Sc_rollup.Game.Start
            {
              player_commitment_hash = hash_commitment opponent_commitment;
              opponent_commitment_hash = hash_commitment staker_commitment;
            }
        in
        let* op =
          Op.sc_rollup_refute (I block) opponent rollup addr refutation
        in
        let* incr =
          if counter = max_number_of_parallel_games then
            Incremental.add_operation ~expect_apply_failure block op
          else Incremental.add_operation block op
        in
        return (incr, counter + 1))
      (incr, 0)
      opponents
      opponents_commitments
  in
  return_unit

(** [test_timeout] test multiple cases of the timeout logic.
- Test to timeout a player before it's allowed and fails.
- Test that the timeout left by player decreases as expected.
- Test another account can timeout a late player.
*)
let test_timeout () =
  let open Lwt_result_syntax in
  let* block, (account1, account2, account3) = context_init Context.T3 in
  let pkh1 = Account.pkh_of_contract_exn account1 in
  let pkh2 = Account.pkh_of_contract_exn account2 in
  let* block, rollup = sc_originate block account1 in
  let* constants = Context.get_constants (B block) in
  let Constants.Parametric.{timeout_period_in_blocks; _} =
    constants.parametric.sc_rollup
  in
  let* genesis_info = Context.Sc_rollup.genesis_info (B block) rollup in
  let* dummy_commitment = dummy_commitment (B block) rollup in
  let commitment1 =
    {
      dummy_commitment with
      number_of_ticks = number_of_ticks_exn 4L;
      compressed_state =
        Sc_rollup.State_hash.context_hash_to_state_hash
          (Context_hash.hash_string ["first"]);
    }
  in
  let commitment2 =
    {
      dummy_commitment with
      number_of_ticks = number_of_ticks_exn 4L;
      compressed_state =
        Sc_rollup.State_hash.context_hash_to_state_hash
          (Context_hash.hash_string ["second"]);
    }
  in

  let* block = add_publish ~rollup block account1 commitment1 in
  let* block = add_publish ~rollup block account2 commitment2 in
  let* block = bake_until_refutation_game_can_start block in
  let refutation =
    Sc_rollup.Game.Start
      {
        player_commitment_hash =
          Sc_rollup.Commitment.hash_uncarbonated commitment1;
        opponent_commitment_hash =
          Sc_rollup.Commitment.hash_uncarbonated commitment2;
      }
  in
  let* start_game_op =
    Op.sc_rollup_refute (B block) account1 rollup pkh2 refutation
  in
  let* block = Block.bake ~operation:start_game_op block in
  let* block = Block.bake_n (timeout_period_in_blocks - 1) block in
  let game_index = Sc_rollup.Game.Index.make pkh1 pkh2 in
  (* Testing to send a timeout before it's allowed. There is one block left
     before timeout is allowed, that is, the current block. *)
  let* () =
    let blocks_left = 0l in
    let* timeout = Op.sc_rollup_timeout (B block) account3 rollup game_index in
    let block_res = Block.bake ~operation:timeout block in
    assert_fails_with
      ~__LOC__
      block_res
      (Sc_rollup_errors.Sc_rollup_timeout_level_not_reached (blocks_left, pkh1))
  in
  let* refute =
    let tick =
      WithExceptions.Option.get ~loc:__LOC__ (Sc_rollup.Tick.of_int 0)
    in
    let* {compressed_state; _} =
      Context.Sc_rollup.commitment (B block) rollup genesis_info.commitment_hash
    in
    let first_chunk =
      Sc_rollup.Dissection_chunk.{state_hash = Some compressed_state; tick}
    in
    let* rest =
      List.init_es ~when_negative_length:[] 4 (fun i ->
          let state_hash = None in
          let tick =
            WithExceptions.Option.get
              ~loc:__LOC__
              (Sc_rollup.Tick.of_int (i + 1))
          in
          return Sc_rollup.Dissection_chunk.{state_hash; tick})
    in
    let step = Sc_rollup.Game.Dissection (first_chunk :: rest) in
    let refutation = Sc_rollup.Game.(Move {choice = tick; step}) in
    Op.sc_rollup_refute (B block) account1 rollup pkh2 refutation
  in
  let* block = Block.bake ~operation:refute block in
  let* pkh1_timeout, pkh2_timeout =
    let+ timeout = Context.Sc_rollup.timeout (B block) rollup pkh1 pkh2 in
    let timeout = WithExceptions.Option.get ~loc:__LOC__ timeout in
    if game_index.alice = pkh1 then (timeout.alice, timeout.bob)
    else (timeout.bob, timeout.alice)
  in
  let* () = Assert.equal_int ~loc:__LOC__ pkh1_timeout 0 in
  let* () =
    Assert.equal_int ~loc:__LOC__ pkh2_timeout timeout_period_in_blocks
  in
  let* block = Block.bake_n timeout_period_in_blocks block in
  let* timeout = Op.sc_rollup_timeout (B block) account3 rollup game_index in
  let* incr = Incremental.begin_construction block in
  let* incr = Incremental.add_operation incr timeout in
  let expected_game_status : Sc_rollup.Game.status =
    Ended (Loser {reason = Timeout; loser = pkh2})
  in
  assert_timeout_result ~game_status:expected_game_status incr rollup game_index

let start_game block rollup (first_player, commitment1) (pkh2, commitment2) =
  let open Lwt_result_syntax in
  let* block = bake_until_refutation_game_can_start block in
  let refutation =
    Sc_rollup.Game.Start
      {
        player_commitment_hash =
          Sc_rollup.Commitment.hash_uncarbonated commitment1;
        opponent_commitment_hash =
          Sc_rollup.Commitment.hash_uncarbonated commitment2;
      }
  in
  let* start_game_op =
    Op.sc_rollup_refute (B block) first_player rollup pkh2 refutation
  in
  Block.bake ~operation:start_game_op block

let create_conflicting_commitment block rollup first_player second_player =
  let open Lwt_result_syntax in
  let compressed_state =
    Sc_rollup.State_hash.context_hash_to_state_hash
      (Context_hash.hash_string ["first"])
  in
  let* commitment1 =
    dummy_commitment ~compressed_state ~number_of_ticks:1L (B block) rollup
  in
  let compressed_state =
    Sc_rollup.State_hash.context_hash_to_state_hash
      (Context_hash.hash_string ["second"])
  in
  let* commitment2 =
    dummy_commitment ~compressed_state ~number_of_ticks:1L (B block) rollup
  in
  let* block = add_publish ~rollup block first_player commitment1 in
  let* block = add_publish ~rollup block second_player commitment2 in
  return (block, commitment1, commitment2)

let create_conflict block rollup ~first_player ~second_player =
  let open Lwt_result_syntax in
  let pkh2 = Account.pkh_of_contract_exn second_player in
  let* block, commitment1, commitment2 =
    create_conflicting_commitment block rollup first_player second_player
  in
  let* block =
    start_game block rollup (first_player, commitment1) (pkh2, commitment2)
  in
  return block

let init_with_conflict () =
  let open Lwt_result_syntax in
  let* block, (account1, account2) = context_init Context.T2 in
  let pkh1 = Account.pkh_of_contract_exn account1 in
  let pkh2 = Account.pkh_of_contract_exn account2 in
  let* block, rollup = sc_originate block account1 in
  let* block =
    create_conflict block rollup ~first_player:account1 ~second_player:account2
  in
  return (block, (account1, pkh1), (account2, pkh2), rollup)

module Arith_pvm = Sc_rollup_helpers.Arith_pvm

let dumb_proof ~choice =
  let open Lwt_result_wrap_syntax in
  let context_arith_pvm = Arith_pvm.make_empty_context () in
  let empty = Arith_pvm.make_empty_state () in
  let*! arith_state = Arith_pvm.initial_state ~empty in
  let*! arith_state = Arith_pvm.install_boot_sector arith_state "" in
  let input = Sc_rollup_helpers.make_external_input "c4c4" in
  let*@ pvm_step =
    Arith_pvm.produce_proof
      context_arith_pvm
      ~is_reveal_enabled:Sc_rollup_helpers.is_reveal_enabled_default
      (Some input)
      arith_state
  in
  let pvm_step =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Sc_rollup.Proof.serialize_pvm_step ~pvm:(module Arith_pvm) pvm_step
  in
  let inbox_proof =
    Sc_rollup.Proof.Inbox_proof
      {
        level = Raw_level.root;
        message_counter = Z.zero;
        proof =
          Sc_rollup.Inbox.Internal_for_tests.serialized_proof_of_string
            "dummy proof";
      }
  in
  let proof = Sc_rollup.Proof.{pvm_step; input_proof = Some inbox_proof} in
  return Sc_rollup.Game.(Move {choice; step = Proof proof})

(** Test that two invalid proofs from the two players lead to a draw
    in the refutation game. *)
let test_draw_with_two_invalid_moves () =
  let open Lwt_result_syntax in
  let* block, (p1, p1_pkh), (p2, p2_pkh), rollup = init_with_conflict () in

  (* Player1 will play an invalid final move. *)
  let* block =
    let* p1_refutation =
      let choice = Sc_rollup.Tick.initial in
      dumb_proof ~choice
    in
    let* p1_final_move_op =
      Op.sc_rollup_refute (B block) p1 rollup p2_pkh p1_refutation
    in
    Block.bake ~operation:p1_final_move_op block
  in

  (* Get the frozen bonds for the two players before the draw. *)
  let* frozen_bonds_p1 = Context.Contract.frozen_bonds (B block) p1 in
  let* frozen_bonds_p2 = Context.Contract.frozen_bonds (B block) p2 in

  (* Player2 will also send an invalid final move. *)
  let* incr =
    let* p2_refutation =
      let choice = Sc_rollup.Tick.initial in
      dumb_proof ~choice
    in
    let* p2_final_move_op =
      Op.sc_rollup_refute (B block) p2 rollup p1_pkh p2_refutation
    in
    let* incr = Incremental.begin_construction block in
    Incremental.add_operation incr p2_final_move_op
  in

  (* As both players played invalid moves, the game ends in a draw. *)
  let expected_game_status : Sc_rollup.Game.status = Ended Draw in
  let* () =
    assert_refute_result
      ~game_status:expected_game_status
      incr
      rollup
      (Sc_rollup.Game.Index.make p1_pkh p2_pkh)
  in

  (* The two players should have been slashed. *)
  let* constants = Context.get_constants (I incr) in
  let stake_amount = constants.parametric.sc_rollup.stake_amount in
  let* () =
    Assert.frozen_bonds_was_debited
      ~loc:__LOC__
      (I incr)
      p1
      frozen_bonds_p1
      stake_amount
  in
  let* () =
    Assert.frozen_bonds_was_debited
      ~loc:__LOC__
      (I incr)
      p2
      frozen_bonds_p2
      stake_amount
  in
  return_unit

let play_conflict_until_draw block (p1, p1_pkh) p2_pkh rollup =
  let open Lwt_result_syntax in
  (* Player1 will play an invalid final move. *)
  let* block =
    let* p1_refutation =
      let choice = Sc_rollup.Tick.initial in
      dumb_proof ~choice
    in

    let* p1_final_move_op =
      Op.sc_rollup_refute (B block) p1 rollup p2_pkh p1_refutation
    in
    Block.bake ~operation:p1_final_move_op block
  in

  (* Player2 will not play and it will be timeout. *)
  let* incr =
    let* block = bake_timeout_period block in
    let game_index = Sc_rollup.Game.Index.make p1_pkh p2_pkh in
    let* timeout = Op.sc_rollup_timeout (B block) p1 rollup game_index in
    let* incr = Incremental.begin_construction block in
    Incremental.add_operation incr timeout
  in

  (* As the player1 played an invalid move, the timeout is counted
     as an invalid one too. The game ends in a draw. *)
  let expected_game_status : Sc_rollup.Game.status = Ended Draw in
  let* () =
    assert_timeout_result
      ~game_status:expected_game_status
      incr
      rollup
      (Sc_rollup.Game.Index.make p1_pkh p2_pkh)
  in
  Incremental.finalize_block incr

let play_conflict_with_timeout block (p1, p1_pkh) p2_pkh rollup =
  let open Lwt_result_syntax in
  (* Player1 will not play and it will be timeout. *)
  let game_index = Sc_rollup.Game.Index.make p1_pkh p2_pkh in
  let* incr =
    let* block = bake_timeout_period block in
    let* timeout = Op.sc_rollup_timeout (B block) p1 rollup game_index in
    let* incr = Incremental.begin_construction block in
    Incremental.add_operation incr timeout
  in
  (* The game ends with p2 as a winner. *)
  let expected_game_status : Sc_rollup.Game.status =
    Ended (Loser {reason = Timeout; loser = p1_pkh})
  in
  let* () =
    assert_timeout_result
      ~game_status:expected_game_status
      incr
      rollup
      game_index
  in
  let* () = assert_game_exists incr rollup game_index ~exists:false in
  Incremental.finalize_block incr

(** Test that timeout a player during the final move ends the game if
    the other player played. *)
let test_timeout_during_final_move () =
  let open Lwt_result_syntax in
  let* block, (p1, p1_pkh), (_p2, p2_pkh), rollup = init_with_conflict () in
  let* _block = play_conflict_until_draw block (p1, p1_pkh) p2_pkh rollup in
  return_unit

(** Test that timeout a player during the final move ends the game if
    the other player played. *)
let test_draw_with_parallel_game () =
  let open Lwt_result_syntax in
  let* block, (account1, account2, account3) = context_init Context.T3 in
  let pkh1 = Account.pkh_of_contract_exn account1 in
  let pkh2 = Account.pkh_of_contract_exn account2 in
  let pkh3 = Account.pkh_of_contract_exn account3 in
  let* block, rollup = sc_originate block account1 in
  let compressed_state =
    Sc_rollup.State_hash.context_hash_to_state_hash
      (Context_hash.hash_string ["first"])
  in
  let* commitment1 =
    dummy_commitment ~compressed_state ~number_of_ticks:1L (B block) rollup
  in
  let compressed_state =
    Sc_rollup.State_hash.context_hash_to_state_hash
      (Context_hash.hash_string ["second"])
  in
  let* commitment2 =
    dummy_commitment ~compressed_state ~number_of_ticks:1L (B block) rollup
  in
  let compressed_state =
    Sc_rollup.State_hash.context_hash_to_state_hash
      (Context_hash.hash_string ["third"])
  in
  let* commitment3 =
    dummy_commitment ~compressed_state ~number_of_ticks:1L (B block) rollup
  in
  let* block = add_publish ~rollup block account1 commitment1 in
  let* block = add_publish ~rollup block account2 commitment2 in
  let* block = add_publish ~rollup block account3 commitment3 in
  let* block =
    start_game block rollup (account1, commitment1) (pkh2, commitment2)
  in
  let* block =
    start_game block rollup (account3, commitment3) (pkh2, commitment2)
  in
  let* block =
    start_game block rollup (account3, commitment3) (pkh1, commitment1)
  in
  let* block = play_conflict_with_timeout block (account1, pkh1) pkh2 rollup in
  let* block = play_conflict_with_timeout block (account3, pkh3) pkh2 rollup in

  (* [pkh1] and [pkh3] were both slashed, any move in their game will
     result in a draw. *)
  let* incr = Incremental.begin_construction block in
  let* incr =
    let* refutation =
      let choice = Sc_rollup.Tick.initial in
      dumb_proof ~choice
    in
    let* final_move_op =
      Op.sc_rollup_refute (B block) account3 rollup pkh1 refutation
    in
    Incremental.add_operation incr final_move_op
  in
  let expected_game_status : Sc_rollup.Game.status = Ended Draw in
  let game_index = Sc_rollup.Game.Index.make pkh1 pkh3 in
  let* () =
    assert_refute_result
      ~game_status:expected_game_status
      incr
      rollup
      game_index
  in
  let* () = assert_game_exists incr rollup game_index ~exists:false in
  return_unit

(** Test that playing a dissection during a final move is rejected. *)
let test_dissection_during_final_move () =
  let open Lwt_result_syntax in
  let* block, (p1, p1_pkh), (p2, p2_pkh), rollup = init_with_conflict () in

  (* Player1 will play an invalid final move. *)
  let* block =
    let* p1_refutation =
      let choice = Sc_rollup.Tick.initial in
      dumb_proof ~choice
    in

    let* p1_final_move_op =
      Op.sc_rollup_refute (B block) p1 rollup p2_pkh p1_refutation
    in
    Block.bake ~operation:p1_final_move_op block
  in

  (* Player2 will play a dissection. *)
  let dumb_dissection =
    let choice = Sc_rollup.Tick.initial in
    Sc_rollup.Game.(Move {choice; step = Dissection []})
  in
  let* p2_op = Op.sc_rollup_refute (B block) p2 rollup p1_pkh dumb_dissection in
  (* Dissecting is no longer accepted. *)
  let block_res = Block.bake ~operation:p2_op block in
  assert_fails_with
    ~__LOC__
    block_res
    Sc_rollup_game_repr.Dissecting_during_final_move

let init_arith_state ~boot_sector =
  let open Lwt_syntax in
  let context = Arith_pvm.make_empty_context () in
  let empty = Arith_pvm.make_empty_state () in
  let* state = Arith_pvm.initial_state ~empty in
  let* state = Arith_pvm.install_boot_sector state boot_sector in
  return (context, state)

(** [make_arith_state ?boot_sector metadata] initializes an arith PVM
    waiting to read its first input, after evaluating the boot sector
    and the [metadata].

    [boot_sector] defaults to [""].
*)
let make_arith_state ?(boot_sector = "") metadata =
  let open Lwt_syntax in
  let* context, state = init_arith_state ~boot_sector in
  let* state_hash1 = Arith_pvm.state_hash state in

  (* 1. We evaluate the boot sector. *)
  let* input_required =
    Arith_pvm.is_input_state
      ~is_reveal_enabled:Sc_rollup_helpers.is_reveal_enabled_default
      state
  in
  assert (input_required = Sc_rollup.No_input_required) ;
  let* state = Arith_pvm.eval state in
  let* state_hash2 = Arith_pvm.state_hash state in
  (* 2. The state now needs the metadata. *)
  let* input_required =
    Arith_pvm.is_input_state
      ~is_reveal_enabled:Sc_rollup_helpers.is_reveal_enabled_default
      state
  in
  assert (input_required = Sc_rollup.Needs_reveal Reveal_metadata) ;
  (* 3. We feed the state with the metadata. *)
  let input = Sc_rollup.(Reveal (Metadata metadata)) in
  let* state = Arith_pvm.set_input input state in
  let* state_hash3 = Arith_pvm.state_hash state in
  let* input_required =
    Arith_pvm.is_input_state
      ~is_reveal_enabled:Sc_rollup_helpers.is_reveal_enabled_default
      state
  in
  assert (input_required = Sc_rollup.Initial) ;

  return (context, state, state_hash1, state_hash2, state_hash3)

let make_set_input_refutation context state input input_proof =
  let open Lwt_syntax in
  let* proof =
    Arith_pvm.produce_proof
      context
      ~is_reveal_enabled:Sc_rollup_helpers.is_reveal_enabled_default
      (Some input)
      state
  in
  let pvm_step = WithExceptions.Result.get_ok ~loc:__LOC__ proof in
  let pvm_step =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Sc_rollup.Proof.serialize_pvm_step ~pvm:(module Arith_pvm) pvm_step
  in
  let choice = Sc_rollup.Tick.(next initial) in
  let step : Sc_rollup.Game.step =
    Proof {pvm_step; input_proof = Some input_proof}
  in
  return Sc_rollup.Game.(Move {choice; step})

(** [test_refute_set_input p1_info p2_info make_state_before] creates
    a refutation game where the final tick refuted is a [set_input]
    step.  It uses [p1_info] (and respectively [p2_info]) to create
    the input (and the input proof) executed. [make_state_before]
    initializes the context and the state before the divergence
    between the two players.

    The first player will be expected to win the game. *)
let test_refute_set_input
    (p1_info :
      Sc_rollup.t ->
      Sc_rollup.Commitment.genesis_info ->
      Sc_rollup.input * Sc_rollup.Proof.input_proof) p2_info
    (make_state_before :
      Sc_rollup.t ->
      Sc_rollup.Commitment.genesis_info ->
      (Arith_pvm.context * Arith_pvm.state) tzresult Lwt.t) =
  let open Lwt_result_syntax in
  let* block, (p1, p2) = context_init Context.T2 in
  let pkh1 = Account.pkh_of_contract_exn p1 in
  let pkh2 = Account.pkh_of_contract_exn p2 in
  let* block, rollup = sc_originate block p1 in
  let* genesis_info = Context.Sc_rollup.genesis_info (B block) rollup in
  let* genesis_commitment =
    Context.Sc_rollup.commitment (B block) rollup genesis_info.commitment_hash
  in

  let p1_input, p1_input_proof = p1_info rollup genesis_info in
  let p2_input, p2_input_proof = p2_info rollup genesis_info in
  let* context, prior_state = make_state_before rollup genesis_info in

  let post_commitment_from_set_input block account input =
    let* inbox_level = next_inbox_level (B block) rollup in
    let*! state_hash1 = Arith_pvm.state_hash prior_state in
    let*! state = Arith_pvm.set_input input prior_state in
    let*! state_hash2 = Arith_pvm.state_hash state in
    let commitment : Sc_rollup.Commitment.t =
      {
        predecessor = genesis_info.commitment_hash;
        inbox_level;
        number_of_ticks = number_of_ticks_exn 2L;
        compressed_state = state_hash2;
      }
    in
    let* block = add_publish ~rollup block account commitment in
    return (commitment, block, state, state_hash1, state_hash2)
  in

  let* commitment1, block, _state, state_hash1, p1_state_hash2 =
    post_commitment_from_set_input block p1 p1_input
  in
  let* commitment2, block, _, _, _ =
    post_commitment_from_set_input block p2 p2_input
  in

  (* [p1] starts a game.

     The dissection is:
     0 -> predecessor state hash
     1 -> state just before the [set_input]
     2 -> tick in conflict with different evaluations of [set_input]
  *)
  let* block = bake_until_refutation_game_can_start block in
  let* start_game_op =
    let refutation =
      Sc_rollup.Game.Start
        {
          player_commitment_hash =
            Sc_rollup.Commitment.hash_uncarbonated commitment1;
          opponent_commitment_hash =
            Sc_rollup.Commitment.hash_uncarbonated commitment2;
        }
    in
    Op.sc_rollup_refute (B block) p1 rollup pkh2 refutation
  in
  let* dissection_op =
    let dissection : Sc_rollup.Game.refutation =
      let choice = Sc_rollup.Tick.initial in
      let step : Sc_rollup.Game.step =
        let zero = Sc_rollup.Tick.initial in
        let one = Sc_rollup.Tick.next zero in
        let two = Sc_rollup.Tick.next one in
        Dissection
          [
            {tick = zero; state_hash = Some genesis_commitment.compressed_state};
            {tick = one; state_hash = Some state_hash1};
            {tick = two; state_hash = Some p1_state_hash2};
          ]
      in
      Move {choice; step}
    in
    Op.sc_rollup_refute (B block) p1 rollup pkh2 dissection
  in
  let* p1_moves =
    Op.batch_operations
      ~recompute_counters:true
      ~source:p1
      (B block)
      [start_game_op; dissection_op]
  in
  let* block = Block.bake ~operation:p1_moves block in

  (* [p2] plays its [set_input], he is expected to play an invalid one. *)
  let* p2_final_move_op =
    let*! proof =
      make_set_input_refutation context prior_state p2_input p2_input_proof
    in
    Op.sc_rollup_refute (B block) p2 rollup pkh1 proof
  in
  (* [p1] plays it [set_input] too. *)
  let* p1_final_move_op =
    let*! proof =
      make_set_input_refutation context prior_state p1_input p1_input_proof
    in
    Op.sc_rollup_refute (B block) p1 rollup pkh2 proof
  in
  let* incr = Incremental.begin_construction block in
  let* incr = Incremental.add_operation incr p2_final_move_op in
  let* incr = Incremental.add_operation incr p1_final_move_op in
  let expected_game_status : Sc_rollup.Game.status =
    Ended (Loser {reason = Conflict_resolved; loser = pkh2})
  in
  assert_refute_result
    ~game_status:expected_game_status
    incr
    rollup
    (Sc_rollup.Game.Index.make pkh1 pkh2)

let test_refute_invalid_metadata () =
  let open Lwt_result_syntax in
  let p1_info rollup (genesis_info : Sc_rollup.Commitment.genesis_info) =
    let metadata =
      Sc_rollup.Metadata.
        {address = rollup; origination_level = genesis_info.level}
    in
    Sc_rollup.(Reveal (Metadata metadata), Proof.Reveal_proof Metadata_proof)
  in
  let p2_info rollup _genesis_info =
    let invalid_metadata =
      Sc_rollup.Metadata.
        {address = rollup; origination_level = Raw_level.of_int32_exn 42l}
    in
    Sc_rollup.
      (Reveal (Metadata invalid_metadata), Proof.Reveal_proof Metadata_proof)
  in
  let make_state_before _rollup _genesis_info =
    let*! context, state = init_arith_state ~boot_sector:"" in
    let*! state = Arith_pvm.eval state in
    return (context, state)
  in
  test_refute_set_input p1_info p2_info make_state_before

(** [arith_state_before_reveal metadata hash] initializes an arith PVM waiting
    for the data associated to [hash] to be revealed.

    Starts by creating a state with {!make_arith_state}, then triggers the
    [Needs_reveal] state through an external message annoucing the [hash].
*)
let arith_state_before_reveal metadata hash =
  let open Lwt_result_syntax in
  let*! context, state, _, _, _ = make_arith_state metadata in
  let input =
    Sc_rollup_helpers.make_external_input
      ~inbox_level:Raw_level.root
      ~message_counter:Z.zero
      ("hash:" ^ hash)
  in
  let*! state = Arith_pvm.set_input input state in
  let rec eval_until_needs_reveal state =
    let*! input_request =
      Arith_pvm.is_input_state
        ~is_reveal_enabled:Sc_rollup_helpers.is_reveal_enabled_default
        state
    in
    match input_request with
    | Needs_reveal _ -> return state
    | _ ->
        let*! state = Arith_pvm.eval state in
        eval_until_needs_reveal state
  in
  let* state = eval_until_needs_reveal state in
  return (context, state)

let test_refute_invalid_reveal () =
  let data = String.make Constants_repr.sc_rollup_message_size_limit 'f' in
  let invalid_data = "foo" in
  let hash =
    Sc_rollup_reveal_hash.(hash_string ~scheme:Blake2B [data] |> to_hex)
  in
  let p1_info _rollup _genesis_info =
    Sc_rollup.(Reveal (Raw_data data), Proof.Reveal_proof (Raw_data_proof data))
  in
  let p2_info _rollup _genesis_info =
    Sc_rollup.
      ( Reveal (Raw_data invalid_data),
        Proof.Reveal_proof (Raw_data_proof invalid_data) )
  in
  let make_state_before rollup
      (genesis_info : Sc_rollup.Commitment.genesis_info) =
    let metadata =
      Sc_rollup.Metadata.
        {address = rollup; origination_level = genesis_info.level}
    in
    arith_state_before_reveal metadata hash
  in
  test_refute_set_input p1_info p2_info make_state_before

let full_history_inbox (genesis_predecessor_timestamp, genesis_predecessor)
    all_external_messages =
  let open Sc_rollup_helpers in
  let payloads_per_levels =
    List.map
      (fun (pred_info, level, external_messages) ->
        wrap_messages ~pred_info level external_messages)
      all_external_messages
  in
  Sc_rollup_helpers.Node_inbox.construct_inbox
    ~genesis_predecessor_timestamp
    ~genesis_predecessor
    payloads_per_levels

let input_included ~snapshot ~full_history_inbox (l, n) =
  let open Lwt_result_wrap_syntax in
  let open Sc_rollup_helpers in
  let Sc_rollup_helpers.Node_inbox.{payloads_histories; history; inbox} =
    full_history_inbox
  in
  let history_proof = Sc_rollup.Inbox.old_levels_messages inbox in
  (* Create an inclusion proof of the inbox message at [(l, n)]. *)
  let*@ proof, _ =
    Sc_rollup.Inbox.produce_proof
      ~get_payloads_history:(get_payloads_history payloads_histories)
      ~get_history:(get_history history)
      history_proof
      (l, n)
  in
  let*?@ inbox_message_verified =
    Sc_rollup.Inbox.verify_proof (l, n) snapshot proof
  in
  return
  @@ Option.map
       (fun inbox_message -> Sc_rollup.Inbox_message inbox_message)
       inbox_message_verified

(** Test that the protocol adds a [SOL], [Info_per_level] and [EOL] for each
    Tezos level, even if no messages are added to the inbox. *)
let test_automatically_added_internal_messages () =
  let open Lwt_result_syntax in
  let assert_input_included ~__LOC__ ~snapshot ~full_history_inbox (l, n) input
      =
    let* input_verified = input_included ~snapshot ~full_history_inbox (l, n) in
    Assert.equal
      ~loc:__LOC__
      (Option.equal Sc_rollup.input_equal)
      "Input found with the proof is different from input provided"
      (fun ppf v ->
        match v with
        | None -> Format.pp_print_string ppf "None"
        | Some v -> Sc_rollup.pp_input ppf v)
      input_verified
      input
  in

  let assert_sol ~snapshot ~full_history_inbox ~inbox_level =
    let sol = Sc_rollup_helpers.make_sol ~inbox_level in
    assert_input_included
      ~snapshot
      ~full_history_inbox
      (inbox_level, Z.zero)
      (Some sol)
  in

  let assert_ipl ~snapshot ~full_history_inbox ~level_info ~inbox_level =
    let predecessor_timestamp, predecessor = level_info in
    let info_per_level =
      Sc_rollup_helpers.make_info_per_level
        ~inbox_level
        ~predecessor_timestamp
        ~predecessor
    in
    assert_input_included
      ~snapshot
      ~full_history_inbox
      (inbox_level, Z.one)
      (Some info_per_level)
  in

  let assert_protocol_migration ~snapshot ~full_history_inbox ~inbox_level =
    let protocol_migration =
      Sc_rollup_helpers.make_protocol_migration ~inbox_level
    in
    assert_input_included
      ~snapshot
      ~full_history_inbox
      (inbox_level, Z.(succ one))
      (Some protocol_migration)
  in

  let assert_eol ~snapshot ~full_history_inbox ~inbox_level ~message_counter =
    let eol = Sc_rollup_helpers.make_eol ~inbox_level ~message_counter in
    assert_input_included
      ~snapshot
      ~full_history_inbox
      (inbox_level, message_counter)
      (Some eol)
  in

  let assert_no_message ~snapshot ~full_history_inbox ~inbox_level
      ~message_counter =
    assert_input_included
      ~snapshot
      ~full_history_inbox
      (inbox_level, message_counter)
      None
  in

  let info_per_block (block : Block.t) =
    (block.header.shell.timestamp, block.hash)
  in

  (* Create the first block. *)
  let* block, account = context_init Context.T1 in

  let level_zero_info =
    ( Time.Protocol.epoch,
      Block_hash.of_b58check_exn
        "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU" )
  in

  let level_one_info = info_per_block block in
  (* Bake a second block. *)
  let* block = Block.bake block in

  let level_two_info = info_per_block block in
  (* Bake a third block where a message is added. *)
  let* operation = Op.sc_rollup_add_messages (B block) account ["foo"] in
  let* block = Block.bake ~operation block in

  let* inbox = Context.Sc_rollup.inbox (B block) in
  let snapshot = Sc_rollup.Inbox.take_snapshot inbox in

  let level_zero = Raw_level.of_int32_exn 0l in
  let level_one = Raw_level.of_int32_exn 1l in
  let level_two = Raw_level.of_int32_exn 2l in
  let*? ({inbox; _} as full_history_inbox) =
    full_history_inbox
      level_zero_info
      [(level_one_info, level_one, []); (level_two_info, level_two, ["foo"])]
  in

  (* Assertions about level 0. *)
  let* () =
    assert_sol ~__LOC__ ~snapshot ~full_history_inbox ~inbox_level:level_zero
  in
  let* () =
    assert_ipl
      ~__LOC__
      ~snapshot
      ~full_history_inbox
      ~inbox_level:level_zero
      ~level_info:level_zero_info
  in
  let* () =
    assert_protocol_migration
      ~__LOC__
      ~snapshot
      ~full_history_inbox
      ~inbox_level:level_zero
  in
  let* () =
    assert_eol
      ~__LOC__
      ~snapshot
      ~full_history_inbox
      ~inbox_level:level_zero
      ~message_counter:(Z.of_int 3)
  in

  (* Assertions about level 1. *)
  let* () =
    assert_sol ~__LOC__ ~snapshot ~full_history_inbox ~inbox_level:level_one
  in
  let* () =
    assert_ipl
      ~__LOC__
      ~snapshot
      ~full_history_inbox
      ~inbox_level:level_one
      ~level_info:level_one_info
  in
  let* () =
    assert_protocol_migration
      ~__LOC__
      ~snapshot
      ~full_history_inbox
      ~inbox_level:level_zero
  in
  let* () =
    assert_eol
      ~__LOC__
      ~snapshot
      ~full_history_inbox
      ~inbox_level:level_one
      ~message_counter:(Z.of_int 3)
  in

  (* Assertions about level 2. *)
  let* () =
    assert_sol ~__LOC__ ~snapshot ~full_history_inbox ~inbox_level:level_two
  in
  let* () =
    assert_ipl
      ~__LOC__
      ~snapshot
      ~full_history_inbox
      ~inbox_level:level_two
      ~level_info:level_two_info
  in
  let* () =
    assert_eol
      ~__LOC__
      ~snapshot
      ~full_history_inbox
      ~inbox_level:level_two
      ~message_counter:(Z.of_int 3)
  in
  let* () =
    assert_no_message
      ~__LOC__
      ~snapshot
      ~full_history_inbox
      ~inbox_level:level_two
      ~message_counter:(Z.of_int 4)
  in

  (* Assert the computed inbox and protocol's inbox are equal. *)
  let history_proof = Sc_rollup.Inbox.old_levels_messages inbox in
  Assert.equal
    ~loc:__LOC__
    Sc_rollup.Inbox.equal_history_proof
    "Computed and protocol inboxes aren't equal"
    Sc_rollup.Inbox.pp_history_proof
    snapshot
    history_proof

(** With [Start_of_level] and [End_of_level] inbox messages in each inbox level,
    it's impossible to give a valid commitment with 0 ticks. *)
let test_zero_tick_commitment_fails () =
  let open Lwt_result_syntax in
  let* block, contract, rollup = init_and_originate Context.T1 in
  let* commitment = dummy_commitment (B block) rollup in
  let commitment = {commitment with number_of_ticks = number_of_ticks_exn 0L} in
  let* publish_commitment =
    Op.sc_rollup_publish (B block) contract rollup commitment
  in
  let block_res = Block.bake ~operation:publish_commitment block in
  assert_fails_with
    ~__LOC__
    block_res
    Sc_rollup_errors.Sc_rollup_zero_tick_commitment

(** [test_curfew] creates a rollup, publishes two conflicting
    commitments. Branches are expected to continue (commitment are able to be
    published). Tries to publish another commitment at the same initial
    `inbox_level` after [challenge_window_in_blocks - 1] and after
    [challenge_window_in_blocks] blocks. Only the first attempt is expected to
    succeed. *)
let test_curfew () =
  let open Lwt_result_syntax in
  let* block, (account1, account2, account3), rollup =
    (* sc_rollup_challenge_window_in_blocks should be at least commitment period *)
    init_and_originate ~sc_rollup_challenge_window_in_blocks:150 Context.T3
  in
  let* constants = Context.get_constants (B block) in
  let challenge_window =
    constants.parametric.sc_rollup.challenge_window_in_blocks
  in
  let* publish1, commitment1 =
    publish_op_and_dummy_commitment
      ~sender:account1
      ~compressed_state:"first"
      rollup
      block
  in
  let* publish2, commitment2 =
    publish_op_and_dummy_commitment
      ~sender:account2
      ~compressed_state:"second"
      rollup
      block
  in
  let* block = bake_blocks_until_inbox_level block commitment1 in
  let* block = Block.bake ~operations:[publish1; publish2] block in
  let* block = Block.bake_n (challenge_window - 1) block in

  let* publish11, commitment11 =
    publish_op_and_dummy_commitment
      ~sender:account1
      ~predecessor:commitment1
      rollup
      block
  in
  let* publish21, commitment21 =
    publish_op_and_dummy_commitment
      ~sender:account2
      ~predecessor:commitment2
      rollup
      block
  in
  let* publish3, _commitment3 =
    publish_op_and_dummy_commitment
      ~sender:account3
      ~compressed_state:"third"
      rollup
      block
  in
  let* block = bake_blocks_until_inbox_level block commitment11 in
  let* block = Block.bake ~operations:[publish11; publish21; publish3] block in
  let* publish111, commitment111 =
    publish_op_and_dummy_commitment
      ~sender:account1
      ~predecessor:commitment11
      rollup
      block
  in
  let* publish211, _commitment211 =
    publish_op_and_dummy_commitment
      ~sender:account2
      ~predecessor:commitment21
      rollup
      block
  in
  let* publish4, _commitment4 =
    publish_op_and_dummy_commitment
      ~sender:account3
      ~compressed_state:"fourth"
      rollup
      block
  in
  let* block = bake_blocks_until_inbox_level block commitment111 in
  let* incr = Incremental.begin_construction block in
  let* incr = Incremental.add_operation incr publish111 in
  let* incr = Incremental.add_operation incr publish211 in
  let expect_apply_failure = function
    | Environment.Ecoproto_error
        (Sc_rollup_errors.Sc_rollup_commitment_past_curfew as e)
      :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ ->
        failwith "It should have failed with [Sc_rollup_commitment_past_curfew]"
  in
  let* _incr = Incremental.add_operation ~expect_apply_failure incr publish4 in
  return_unit

(** [test_curfew_period_is_started_only_after_first_publication checks that
    publishing the first commitment of a given [inbox_level] after
    [inbox_level + challenge_window] is still possible. *)
let test_curfew_period_is_started_only_after_first_publication () =
  let open Lwt_result_syntax in
  let* block, account1, rollup = init_and_originate Context.T1 in
  let* constants = Context.get_constants (B block) in
  let challenge_window =
    constants.parametric.sc_rollup.challenge_window_in_blocks
  in
  let commitment_period =
    constants.parametric.sc_rollup.commitment_period_in_blocks
  in
  let* block = Block.bake_n commitment_period block in
  let* block = Block.bake_n challenge_window block in
  let* commitment = dummy_commitment (B block) rollup in
  let* operation = Op.sc_rollup_publish (B block) account1 rollup commitment in
  let* _block = Block.bake ~operation block in
  return_unit

let test_offline_staker_does_not_prevent_cementation () =
  let open Lwt_result_syntax in
  let* ctxt, contracts, rollup = init_and_originate Context.T2 in
  let contract1, contract2 = contracts in
  let* ctxt = bake_blocks_until_next_inbox_level ctxt rollup in
  (* A publishes a commitment on C1. *)
  let* commitment1 = dummy_commitment (B ctxt) rollup in
  let* operation = Op.sc_rollup_publish (B ctxt) contract1 rollup commitment1 in
  let* b = Block.bake ~operation ctxt in

  (* We cement C1. *)
  let* constants = Context.get_constants (B b) in
  let* b =
    Block.bake_n constants.parametric.sc_rollup.challenge_window_in_blocks b
  in
  let* cement_op = Op.sc_rollup_cement (B b) contract1 rollup in
  let* b = Block.bake ~operation:cement_op b in

  (* A now goes offline, B takes over. *)
  let* commitment2 = dummy_commitment ~predecessor:commitment1 (B b) rollup in
  let* operation2 =
    Op.sc_rollup_publish (B ctxt) contract2 rollup commitment2
  in
  let* b = bake_blocks_until_inbox_level b commitment2 in
  let* b = Block.bake ~operation:operation2 b in

  (* We cement C2. *)
  let* constants = Context.get_constants (B b) in
  let* b =
    Block.bake_n constants.parametric.sc_rollup.challenge_window_in_blocks b
  in
  let* cement_op = Op.sc_rollup_cement (B b) contract2 rollup in
  let* _b = Block.bake ~operation:cement_op b in
  return_unit

let init_with_4_conflicts () =
  let open Lwt_result_syntax in
  let dumb_compressed_state s =
    Sc_rollup.State_hash.context_hash_to_state_hash
      (Context_hash.hash_string [s])
  in
  let* block, players = context_init (Context.TList 4) in
  let pA, pB, pC, pD =
    match players with
    | [pA; pB; pC; pD] -> (pA, pB, pC, pD)
    | _ -> assert false
  in
  let pA_pkh = Account.pkh_of_contract_exn pA in
  let pB_pkh = Account.pkh_of_contract_exn pB in
  let pC_pkh = Account.pkh_of_contract_exn pC in
  let pD_pkh = Account.pkh_of_contract_exn pD in
  let* block, rollup = sc_originate block pA in

  (* The four players stake on a conflicting commitment. *)
  let* pA_commitment =
    dummy_commitment
      ~number_of_ticks:1L
      ~compressed_state:(dumb_compressed_state "A")
      (B block)
      rollup
  in
  let* pB_commitment =
    dummy_commitment
      ~number_of_ticks:1L
      ~compressed_state:(dumb_compressed_state "B")
      (B block)
      rollup
  in
  let* pC_commitment =
    dummy_commitment
      ~number_of_ticks:1L
      ~compressed_state:(dumb_compressed_state "C")
      (B block)
      rollup
  in
  let* pD_commitment =
    dummy_commitment
      ~number_of_ticks:1L
      ~compressed_state:(dumb_compressed_state "D")
      (B block)
      rollup
  in
  let* block =
    List.fold_left_es
      (fun block (player, commitment) ->
        add_publish ~rollup block player commitment)
      block
      [
        (pA, pA_commitment);
        (pB, pB_commitment);
        (pC, pC_commitment);
        (pD, pD_commitment);
      ]
  in
  return (block, rollup, (pA, pA_pkh), (pB, pB_pkh), (pC, pC_pkh), (pD, pD_pkh))

let start_refutation_game_op block rollup (p1, p1_pkh) p2_pkh =
  let open Lwt_result_wrap_syntax in
  let* ctxt =
    let+ incr = Incremental.begin_construction block in
    Incremental.alpha_ctxt incr
  in
  let*@ (p1_point, p2_point), _ctxt =
    Sc_rollup.Refutation_storage.Internal_for_tests.get_conflict_point
      ctxt
      rollup
      p1_pkh
      p2_pkh
  in
  let refutation =
    Sc_rollup.Game.Start
      {
        player_commitment_hash = p1_point.hash;
        opponent_commitment_hash = p2_point.hash;
      }
  in
  Op.sc_rollup_refute (B block) p1 rollup p2_pkh refutation

(** Test that when A plays against B, C, D and if A losts the game against
    one of them, the others can win against A for free. *)
let test_winner_by_forfeit () =
  let open Lwt_result_syntax in
  let* block, rollup, (pA, pA_pkh), (pB, pB_pkh), (pC, pC_pkh), (pD, pD_pkh) =
    init_with_4_conflicts ()
  in
  let* block = bake_until_refutation_game_can_start block in

  (* Refutation game starts: A against B, C and D. *)
  (* A starts against B and D so it can be timeouted. *)
  let* pA_against_pB_op =
    start_refutation_game_op block rollup (pA, pA_pkh) pB_pkh
  in
  let* pA_against_pD_op =
    start_refutation_game_op block rollup (pA, pA_pkh) pD_pkh
  in
  let* pA_op =
    Op.batch_operations
      ~recompute_counters:true
      ~source:pA
      (B block)
      [pA_against_pB_op; pA_against_pD_op]
  in
  (* C starts against A so it can win through a move. *)
  let* pC_against_pA_op =
    start_refutation_game_op block rollup (pC, pC_pkh) pA_pkh
  in
  let* block = Block.bake block ~operations:[pA_op; pC_against_pA_op] in
  let* block = bake_timeout_period block in

  (* B timeouts A. *)
  let game_index = Sc_rollup.Game.Index.make pA_pkh pB_pkh in
  let* pB_timeout = Op.sc_rollup_timeout (B block) pB rollup game_index in
  let* block = Block.bake block ~operation:pB_timeout in

  (* C sends a dumb move but A was already slashed. *)
  let dumb_dissection =
    let choice = Sc_rollup.Tick.initial in
    Sc_rollup.Game.(Move {choice; step = Dissection []})
  in
  let* pC_move =
    Op.sc_rollup_refute (B block) pC rollup pA_pkh dumb_dissection
  in

  (* D timeouts A. *)
  let game_index = Sc_rollup.Game.Index.make pA_pkh pD_pkh in
  let* pD_timeout = Op.sc_rollup_timeout (B block) pD rollup game_index in

  (* Both operation fails with [Unknown staker], because pA was removed when
     it lost against B. *)
  let* _block = Block.bake ~operations:[pC_move; pD_timeout] block in
  return_unit

(** Test the same property as in {!test_winner_by_forfeit} but where two
    players slashed eachother with a draw. *)
let test_winner_by_forfeit_with_draw () =
  let open Lwt_result_syntax in
  let* block, rollup, (pA, pA_pkh), (pB, pB_pkh), (pC, pC_pkh), (_pD, _pD_pkh) =
    init_with_4_conflicts ()
  in
  let* constants = Context.get_constants (B block) in
  let Constants.Parametric.{timeout_period_in_blocks; stake_amount; _} =
    constants.parametric.sc_rollup
  in
  let* block = bake_until_refutation_game_can_start block in

  (* A and B starts a refutation game against C. *)
  let* pA_against_pC_op =
    start_refutation_game_op block rollup (pA, pA_pkh) pC_pkh
  in
  let* pB_against_pC_op =
    start_refutation_game_op block rollup (pB, pB_pkh) pC_pkh
  in

  let* block = Block.bake block ~operation:pA_against_pC_op in
  let* block = Block.bake block ~operation:pB_against_pC_op in

  (* A starts a refutation against B.  *)
  let* frozen_bonds_pA = Context.Contract.frozen_bonds (B block) pA in
  let* frozen_bonds_pB = Context.Contract.frozen_bonds (B block) pB in
  let* pA_against_pB_op =
    start_refutation_game_op block rollup (pA, pA_pkh) pB_pkh
  in
  let* block = Block.bake block ~operation:pA_against_pB_op in

  (* A and B will both make an invalid move and ends up in a draw. *)
  let* dumb_move =
    let choice = Sc_rollup.Tick.initial in
    dumb_proof ~choice
  in
  let* pA_dumb_move_op =
    Op.sc_rollup_refute (B block) pA rollup pB_pkh dumb_move
  in
  let* block = Block.bake block ~operation:pA_dumb_move_op in
  let* pB_dumb_move_op =
    Op.sc_rollup_refute (B block) pB rollup pA_pkh dumb_move
  in
  let* block = Block.bake block ~operation:pB_dumb_move_op in

  (* Assert the draw by checking the frozen bonds. *)
  let* () =
    Assert.frozen_bonds_was_debited
      ~loc:__LOC__
      (B block)
      pA
      frozen_bonds_pA
      stake_amount
  in
  let* () =
    Assert.frozen_bonds_was_debited
      ~loc:__LOC__
      (B block)
      pB
      frozen_bonds_pB
      stake_amount
  in

  (* Now C will win the game against A and B with a timeout. *)
  let* block = bake_timeout_period ~timeout_period_in_blocks block in

  (* C timeouts A. *)
  let game_index = Sc_rollup.Game.Index.make pC_pkh pA_pkh in
  let* pC_timeout_pA = Op.sc_rollup_timeout (B block) pC rollup game_index in
  let* block = Block.bake block ~operation:pC_timeout_pA in

  (* C timeouts B. *)
  let game_index = Sc_rollup.Game.Index.make pC_pkh pB_pkh in
  let* pC_timeout_pB = Op.sc_rollup_timeout (B block) pC rollup game_index in
  let* _block = Block.bake block ~operation:pC_timeout_pB in

  return_unit

let test_conflict_point_on_a_branch () =
  let open Lwt_result_wrap_syntax in
  let* block, (pA, pB), rollup =
    init_and_originate ~sc_rollup_challenge_window_in_blocks:1000 Context.T2
  in
  let pA_pkh = Account.pkh_of_contract_exn pA in
  let pB_pkh = Account.pkh_of_contract_exn pB in
  (* pA stakes on a whole branch. *)
  let* genesis_info = Context.Sc_rollup.genesis_info (B block) rollup in
  let* predecessor =
    Context.Sc_rollup.commitment (B block) rollup genesis_info.commitment_hash
  in
  let* commitments_and_hashes =
    gen_commitments (B block) rollup ~predecessor ~num_commitments:10
  in
  let commitments, _ = List.split commitments_and_hashes in
  let* block = publish_commitments block pA rollup commitments in
  (* pB stakes on only one commitment. *)
  let pA_commitment, pB_commitment =
    let commitment = Stdlib.List.nth commitments 8 in
    ( commitment,
      {
        commitment with
        compressed_state =
          Sc_rollup.State_hash.context_hash_to_state_hash
            (Context_hash.hash_string ["foo"]);
      } )
  in
  let* block = publish_commitments block pB rollup [pB_commitment] in
  let* ctxt =
    let+ incr = Incremental.begin_construction block in
    Incremental.alpha_ctxt incr
  in
  let*@ ( ( {commitment = _; hash = conflict_pA_hash},
            {commitment = _; hash = conflict_pB_hash} ),
          _ctxt ) =
    Sc_rollup.Refutation_storage.Internal_for_tests.get_conflict_point
      ctxt
      rollup
      pA_pkh
      pB_pkh
  in
  let pA_hash = hash_commitment pA_commitment in
  let pB_hash = hash_commitment pB_commitment in
  let expected_conflict =
    Sc_rollup.Commitment.Hash.(
      equal conflict_pA_hash pA_hash && equal conflict_pB_hash pB_hash)
  in
  Assert.equal_bool ~loc:__LOC__ true expected_conflict

let test_agreeing_stakers_cannot_play () =
  let open Lwt_result_syntax in
  let* block, (pA, pB), rollup =
    init_and_originate ~sc_rollup_challenge_window_in_blocks:1351 Context.T2
  in
  let pB_pkh = Account.pkh_of_contract_exn pB in
  (* pA stakes on a whole branch. *)
  let* genesis_info = Context.Sc_rollup.genesis_info (B block) rollup in
  let* predecessor =
    Context.Sc_rollup.commitment (B block) rollup genesis_info.commitment_hash
  in
  let* commitments_and_hashes =
    gen_commitments (B block) rollup ~predecessor ~num_commitments:10
  in
  let commitments, _ = List.split commitments_and_hashes in
  let* block = publish_commitments block pA rollup commitments in
  let* block = publish_commitments block pB rollup commitments in
  let* block = bake_until_refutation_game_can_start block in
  let _, agreed_commitment_hash =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.last_opt commitments_and_hashes
  in
  let refutation =
    Sc_rollup.Game.Start
      {
        player_commitment_hash = agreed_commitment_hash;
        opponent_commitment_hash = agreed_commitment_hash;
      }
  in
  let* operation = Op.sc_rollup_refute (B block) pA rollup pB_pkh refutation in
  let block_res = Block.bake ~operation block in
  assert_fails_with ~__LOC__ block_res Sc_rollup_errors.Sc_rollup_no_conflict

let test_start_game_on_cemented_commitment () =
  let open Lwt_result_syntax in
  let* block, (pA, pB), rollup =
    init_and_originate ~sc_rollup_challenge_window_in_blocks:1351 Context.T2
  in
  let* constants = Context.get_constants (B block) in
  let pA_pkh = Account.pkh_of_contract_exn pA in
  let pB_pkh = Account.pkh_of_contract_exn pB in
  let* genesis_info = Context.Sc_rollup.genesis_info (B block) rollup in
  let* predecessor =
    Context.Sc_rollup.commitment (B block) rollup genesis_info.commitment_hash
  in
  let* commitments_and_hashes =
    gen_commitments (B block) rollup ~predecessor ~num_commitments:10
  in
  (* pA and pB publishes and cements 10 commitments. *)
  let commitments, hashes = List.split commitments_and_hashes in
  let* block = publish_commitments block pA rollup commitments in
  let* block = publish_commitments block pB rollup commitments in
  let* block =
    cement_commitments
      ~challenge_window_in_blocks:
        constants.parametric.sc_rollup.challenge_window_in_blocks
      block
      rollup
      pA
      hashes
  in

  let* block = bake_until_refutation_game_can_start block in
  (* We now check that pA and pB cannot start a refutation against on
     cemented commitments. *)
  List.iter_es
    (fun hash ->
      (* The refutation game checks that [pA] stakes on [hash] and
         [pB] on [hash]. As the storage keeps in the storage only
         the metadata for active commitments, any game started on a cemented
         commitment will fail with "<tz1> not staked on <hash>". *)
      let refutation =
        Sc_rollup.Game.Start
          {player_commitment_hash = hash; opponent_commitment_hash = hash}
      in
      let* pA_against_pB =
        Op.sc_rollup_refute (B block) pA rollup pB_pkh refutation
      in
      let* pB_against_pA =
        Op.sc_rollup_refute (B block) pB rollup pA_pkh refutation
      in
      (* Even if there is no conflict, the refutation game will reject
         it before that. This test behaves as a regression test to prevent
         to break this property. *)
      let wrong_staker_error pkh =
        Sc_rollup_errors.Sc_rollup_wrong_staker_for_conflict_commitment
          (pkh, hash)
      in
      let* () =
        let block_res = Block.bake ~operation:pA_against_pB block in
        assert_fails_with ~__LOC__ block_res (wrong_staker_error pA_pkh)
      in
      let* () =
        let block_res = Block.bake ~operation:pB_against_pA block in
        assert_fails_with ~__LOC__ block_res (wrong_staker_error pB_pkh)
      in
      return_unit)
    hashes

let test_origination_fails_with_empty_whitelist () =
  let open Lwt_result_syntax in
  let* b, contract =
    Context.init1
      ~sc_rollup_arith_pvm_enable:true
      ~sc_rollup_private_enable:true
      ()
  in
  let kind = Sc_rollup.Kind.Example_arith in
  let* operation, _rollup =
    Sc_rollup_helpers.origination_op (B b) contract kind ~whitelist:[]
  in
  let*! b = Block.bake ~operation b in
  Assert.proto_error_with_info
    ~loc:__LOC__
    b
    "Invalid whitelist: whitelist cannot be empty"

let test_private_rollup_can_be_deactivated () =
  let open Lwt_result_syntax in
  let* b, contract =
    Context.init1
      ~sc_rollup_arith_pvm_enable:true
      ~sc_rollup_private_enable:false
      ()
  in
  let kind = Sc_rollup.Kind.Example_arith in
  let* operation, _rollup =
    Sc_rollup_helpers.origination_op (B b) contract kind ~whitelist:[]
  in
  let*! b = Block.bake ~operation b in
  Assert.proto_error_with_info
    ~loc:__LOC__
    b
    "Invalid whitelist: must be None when the feature is deactivated"

let test_private_rollup_publish_succeeds_with_whitelisted_staker () =
  let open Lwt_result_syntax in
  let* b, contract =
    Context.init1
      ~sc_rollup_arith_pvm_enable:true
      ~sc_rollup_private_enable:true
      ()
  in
  let kind = Sc_rollup.Kind.Example_arith in
  let staker_pkh = Account.pkh_of_contract_exn contract in
  let* operation, rollup =
    Sc_rollup_helpers.origination_op (B b) contract kind ~whitelist:[staker_pkh]
  in
  let* b = Block.bake ~operation b in
  let* commitment = dummy_commitment (B b) rollup in
  let* operation = Op.sc_rollup_publish (B b) contract rollup commitment in
  let*! _b = Block.bake ~operation b in
  return_unit

let test_private_rollup_publish_fails_with_non_whitelisted_staker () =
  let open Lwt_result_syntax in
  let* b, (contract1, contract2) =
    Context.init2
      ~sc_rollup_arith_pvm_enable:true
      ~sc_rollup_private_enable:true
      ()
  in
  let kind = Sc_rollup.Kind.Example_arith in
  let* operation, rollup =
    Sc_rollup_helpers.origination_op
      (B b)
      contract1
      kind
      ~whitelist:[Context.Contract.pkh contract2]
  in
  let* b = Block.bake ~operation b in
  let* commitment = dummy_commitment (B b) rollup in
  let* operation = Op.sc_rollup_publish (B b) contract1 rollup commitment in
  let*! b = Block.bake ~operation b in
  let* () =
    Assert.proto_error
      ~loc:__LOC__
      b
      (( = ) Sc_rollup_errors.Sc_rollup_staker_not_in_whitelist)
  in
  return_unit

let test_private_rollup_whitelist_cannot_contain_key_duplication () =
  let open Lwt_result_syntax in
  let* block, (account1, account2) = context_init Context.T2 in
  let account2_pkh = Account.pkh_of_contract_exn account2 in
  let originate_with_whitelist ~whitelist block =
    sc_originate ?whitelist block account1
  in
  let whitelist = Some [account2_pkh; account2_pkh] in
  let block_rollup_res = originate_with_whitelist ~whitelist block in
  assert_fails_with
    ~__LOC__
    block_rollup_res
    Sc_rollup_errors.Sc_rollup_duplicated_key_in_whitelist

let update_whitelist ?(message_index = 1)
    ~(genesis_info : Sc_rollup.Commitment.genesis_info) block rollup
    updated_whitelist =
  let open Lwt_result_syntax in
  let output =
    make_whitelist_update_output
      ~outbox_level:Raw_level.(Int32.to_int @@ to_int32 @@ genesis_info.level)
      ~message_index
      updated_whitelist
  in
  let* _res, block =
    execute_outbox_message_without_proof_validation
      block
      rollup
      ~cemented_commitment:genesis_info.commitment_hash
      output
  in
  return block

let verify_whitelist ~__LOC__ block rollup expected_whitelist =
  verify_whitelist ~loc:__LOC__ rollup (B block) ~expected_whitelist

let verify_can_publish_commit_accounts block rollup accounts =
  Tezos_base.TzPervasives.List.iter_es
    (fun (account, succeed) ->
      verify_can_publish_commit ~__LOC__ ~succeed rollup account block)
    accounts

let test_check_initial_whitelist () =
  let open Lwt_result_syntax in
  let* block, (account1, account2, account3) = context_init Context.T3 in
  let account1_pkh = Account.pkh_of_contract_exn account1 in
  let whitelist = Some [account1_pkh] in
  let* block, rollup = sc_originate ?whitelist block account1 in
  (* check initial whitelist *)
  let* () = verify_whitelist ~__LOC__ block rollup whitelist in
  verify_can_publish_commit_accounts
    block
    rollup
    [(account1, true); (account2, false); (account3, false)]

let test_whitelist_update_duplicated_keys () =
  let open Lwt_result_syntax in
  let* block, (account1, account2) = context_init Context.T2 in
  let account1_pkh = Account.pkh_of_contract_exn account1 in
  let account2_pkh = Account.pkh_of_contract_exn account2 in
  let whitelist = Some [account1_pkh] in
  let* block, rollup = sc_originate ?whitelist block account1 in
  let* genesis_info = Context.Sc_rollup.genesis_info (B block) rollup in
  (* replace whitelist with twice the same keys fails *)
  let updated_whitelist = Some [account2_pkh; account2_pkh] in
  let block_rollup_res =
    update_whitelist ~genesis_info block rollup updated_whitelist
  in
  assert_fails_with
    ~__LOC__
    block_rollup_res
    Sc_rollup_errors.Sc_rollup_duplicated_key_in_whitelist

let test_whitelist_update_empty_list () =
  let open Lwt_result_syntax in
  let* block, account = context_init Context.T1 in
  let account_pkh = Account.pkh_of_contract_exn account in
  let whitelist = Some [account_pkh] in
  let* block, rollup = sc_originate ?whitelist block account in
  let* genesis_info = Context.Sc_rollup.genesis_info (B block) rollup in
  (* update to empty list fails *)
  let updated_whitelist = Some [] in
  let block_res =
    update_whitelist ~genesis_info block rollup updated_whitelist
  in
  assert_fails_with
    ~__LOC__
    block_res
    Sc_rollup_errors.Sc_rollup_empty_whitelist

let test_whitelist_update_two_keys () =
  let open Lwt_result_syntax in
  let* block, (account1, account2, account3) = context_init Context.T3 in
  let account1_pkh = Account.pkh_of_contract_exn account1 in
  let account2_pkh = Account.pkh_of_contract_exn account2 in
  let account3_pkh = Account.pkh_of_contract_exn account3 in
  let whitelist = Some [account1_pkh] in
  let* block, rollup = sc_originate ?whitelist block account1 in
  let* genesis_info = Context.Sc_rollup.genesis_info (B block) rollup in

  (* replace whitelist with two keys succeed *)
  let updated_whitelist = Some [account2_pkh; account3_pkh] in
  let* block = update_whitelist ~genesis_info block rollup updated_whitelist in
  let* () = verify_whitelist ~__LOC__ block rollup updated_whitelist in
  verify_can_publish_commit_accounts
    block
    rollup
    [(account1, false); (account2, true); (account3, true)]

let test_whitelist_update_make_rollup_public () =
  let open Lwt_result_syntax in
  let* block, (account1, account2, account3) = context_init Context.T3 in
  let account1_pkh = Account.pkh_of_contract_exn account1 in
  let account2_pkh = Account.pkh_of_contract_exn account2 in
  let account3_pkh = Account.pkh_of_contract_exn account3 in
  let whitelist = Some [account1_pkh] in
  let* block, rollup = sc_originate ?whitelist block account1 in
  let* genesis_info = Context.Sc_rollup.genesis_info (B block) rollup in

  let* block =
    (* replace whitelist with two keys succeed *)
    let updated_whitelist = Some [account2_pkh; account3_pkh] in
    let* block =
      update_whitelist ~genesis_info block rollup updated_whitelist
    in
    let* () = verify_whitelist ~__LOC__ block rollup updated_whitelist in
    let* () =
      verify_can_publish_commit_accounts
        block
        rollup
        [(account1, false); (account2, true); (account3, true)]
    in
    return block
  in
  (* second update succeed and make the rollup public *)
  let updated_whitelist = None in
  let* block =
    update_whitelist
      ~genesis_info
      ~message_index:2
      block
      rollup
      updated_whitelist
  in
  let* () = verify_whitelist ~__LOC__ block rollup updated_whitelist in
  let* () =
    verify_can_publish_commit_accounts
      block
      rollup
      [(account1, true); (account2, true); (account3, true)]
  in
  return_unit

let tests =
  [
    Tztest.tztest
      "check effect of disabled arith pvm flag"
      `Quick
      test_disable_arith_pvm_feature_flag;
    Tztest.tztest
      "check effect of disabled RISC-V pvm flag"
      `Quick
      test_disable_riscv_pvm_feature_flag;
    Tztest.tztest
      "can publish a commit, cement it and withdraw stake"
      `Quick
      test_publish_cement_and_recover_bond;
    Tztest.tztest
      "publish will fail if staker is double staking"
      `Quick
      test_publish_fails_on_double_stake;
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
      "single transaction atomic batch"
      `Quick
      test_single_transaction_batch;
    Tztest.tztest
      "execute outbox message against older cemented commitment"
      `Quick
      test_older_cemented_commitment;
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
      "execute same message twice against different cemented commitments"
      `Quick
      test_execute_message_twice_different_cemented_commitments;
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
    Tztest.tztest
      "inbox max number of messages per inbox level"
      `Quick
      test_inbox_max_number_of_messages_per_level;
    Tztest.tztest
      "a player can't timeout another player before timeout period and related \
       timeout value."
      `Quick
      test_timeout;
    Tztest.tztest
      "a player cannot play more than max_number_of_parallel_games games in \
       parallel."
      `Quick
      test_number_of_parallel_games_bounded;
    Tztest.tztest
      "Two invalid final moves end the game in a draw situation"
      `Quick
      test_draw_with_two_invalid_moves;
    Tztest.tztest
      "Timeout during the final move can end the game in a draw situation"
      `Quick
      test_timeout_during_final_move;
    Tztest.tztest
      "Multiple draw in parallel game are valid"
      `Quick
      test_draw_with_parallel_game;
    Tztest.tztest
      "Cannot play a dissection when the final move has started"
      `Quick
      test_dissection_during_final_move;
    Tztest.tztest
      "Invalid metadata initialization can be refuted"
      `Quick
      test_refute_invalid_metadata;
    Tztest.tztest
      "Invalid reveal can be refuted"
      `Quick
      test_refute_invalid_reveal;
    Tztest.tztest
      "SOL/Info_per_level/EOL are added in the inbox"
      `Quick
      test_automatically_added_internal_messages;
    Tztest.tztest
      "0-tick commitments are forbidden"
      `Quick
      test_zero_tick_commitment_fails;
    Tztest.tztest "the curfew functionality" `Quick test_curfew;
    Tztest.tztest
      "a commitment can be published after the inbox_level + challenge window \
       is passed."
      `Quick
      test_curfew_period_is_started_only_after_first_publication;
    Tztest.tztest
      "An offline staker should not prevent cementation"
      `Quick
      test_offline_staker_does_not_prevent_cementation;
    Tztest.tztest "win refutation game by forfeit" `Quick test_winner_by_forfeit;
    Tztest.tztest
      "win refutation game by forfeit with draw"
      `Quick
      test_winner_by_forfeit_with_draw;
    Tztest.tztest
      "cannot start a game with agreeing stakers"
      `Quick
      test_agreeing_stakers_cannot_play;
    Tztest.tztest
      "find conflict point with incomplete branch"
      `Quick
      test_conflict_point_on_a_branch;
    Tztest.tztest
      "cannot start a game on a cemented commitment"
      `Quick
      test_start_game_on_cemented_commitment;
    Tztest.tztest
      "Origination fails with empty whitelist"
      `Quick
      test_origination_fails_with_empty_whitelist;
    Tztest.tztest
      "Origination can be deactivated"
      `Quick
      test_private_rollup_can_be_deactivated;
    Tztest.tztest
      "Submit a commitment with a whitelisted staker"
      `Quick
      test_private_rollup_publish_succeeds_with_whitelisted_staker;
    Tztest.tztest
      "Submit a commitment with a non-whitelisted staker"
      `Quick
      test_private_rollup_publish_fails_with_non_whitelisted_staker;
    Tztest.tztest
      "Originate a rollup with duplicated key in the whitelist fails"
      `Quick
      test_private_rollup_whitelist_cannot_contain_key_duplication;
    Tztest.tztest "Check initial whitelist" `Quick test_check_initial_whitelist;
    Tztest.tztest
      "Update the whitelist with duplicated keys"
      `Quick
      test_whitelist_update_duplicated_keys;
    Tztest.tztest
      "Update the whitelist with an empty list"
      `Quick
      test_whitelist_update_empty_list;
    Tztest.tztest
      "Update the whitelist with two distinct keys"
      `Quick
      test_whitelist_update_two_keys;
    Tztest.tztest
      "Update the whitelist to make the rollup public"
      `Quick
      test_whitelist_update_make_rollup_public;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("sc rollup", tests)]
  |> Lwt_main.run
