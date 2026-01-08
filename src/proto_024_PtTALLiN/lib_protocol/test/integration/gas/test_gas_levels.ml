(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:  Protocol (Gas levels)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/gas/main.exe \
                  -- --file test_gas_levels.ml
    Subject:    On gas consumption and exhaustion.
*)

open Protocol
open Raw_context
module S = Saturation_repr

(* This value is supposed to be larger than the block gas level limit
   but not saturated. *)
let opg = max_int / 10000

exception Gas_levels_test_error of string

let err x = Exn (Gas_levels_test_error x)

let succeed x = match x with Ok _ -> true | _ -> false

let failed x = not (succeed x)

let dummy_context () =
  let open Lwt_result_wrap_syntax in
  let* block, _contract = Context.init1 ~consensus_threshold_size:0 () in
  let*@ ctxt =
    Raw_context.prepare
      ~level:Int32.zero
      ~predecessor_timestamp:Time.Protocol.epoch
      ~timestamp:Time.Protocol.epoch
      (* ~fitness:[] *)
      (block.context : Tezos_protocol_environment.Context.t)
      ~all_bakers_attest_first_level:None
  in
  return ctxt

let consume_gas_lwt context gas =
  let open Lwt_result_wrap_syntax in
  let*?@ ctxt = consume_gas context (S.safe_int gas) in
  return ctxt

let consume_gas_limit_in_block_lwt context gas =
  let open Lwt_result_wrap_syntax in
  let*?@ ctxt = consume_gas_limit_in_block context gas in
  return ctxt

let test_detect_gas_exhaustion_in_fresh_context () =
  let open Lwt_result_syntax in
  let* context = dummy_context () in
  fail_unless
    (consume_gas context (S.safe_int opg) |> succeed)
    (err "In a fresh context, gas consumption is unlimited.")

(** Create a context with a given block gas level, capped at the
    hard gas limit per block *)
let make_context remaining_block_gas =
  let open Lwt_result_syntax in
  let open Gas_limit_repr in
  let* context = dummy_context () in
  let hard_limit = Arith.fp (constants context).hard_gas_limit_per_operation in
  let hard_limit_block =
    Arith.fp (constants context).hard_gas_limit_per_block
  in
  let block_gas = Arith.(unsafe_fp (Z.of_int remaining_block_gas)) in
  let rec aux context to_consume =
    (* Because of saturated arithmetic, [to_consume] should never be negative. *)
    assert (Arith.(to_consume >= zero)) ;
    if Arith.(to_consume = zero) then return context
    else if Arith.(to_consume <= hard_limit) then
      consume_gas_limit_in_block_lwt context to_consume
    else
      let* context = consume_gas_limit_in_block_lwt context hard_limit in
      aux context (Arith.sub to_consume hard_limit)
  in
  aux context Arith.(sub hard_limit_block block_gas)

(** Test operation gas exhaustion. Should pass when remaining gas is 0,
    and fail when it goes over *)
let test_detect_gas_exhaustion_when_operation_gas_hits_zero () =
  let open Lwt_result_syntax in
  let gas_op = 100000 in
  let* context = dummy_context () in
  set_gas_limit context (Gas_limit_repr.Arith.unsafe_fp (Z.of_int gas_op))
  |> fun context ->
  let* () =
    fail_unless
      (consume_gas context (S.safe_int gas_op) |> succeed)
      (err "Succeed when consuming exactly the remaining operation gas.")
  in
  fail_unless
    (consume_gas context (S.safe_int (gas_op + 1)) |> failed)
    (err "Fail when consuming more than the remaining operation gas.")

(** Test block gas exhaustion *)
let test_detect_gas_exhaustion_when_block_gas_hits_zero () =
  let open Lwt_result_syntax in
  let gas k = Gas_limit_repr.Arith.unsafe_fp (Z.of_int k) in
  let remaining_gas = gas 100000 and too_much = gas (100000 + 1) in
  let* context = make_context 100000 in
  let* () =
    fail_unless
      (consume_gas_limit_in_block context remaining_gas |> succeed)
      (err "Succeed when consuming exactly the remaining block gas.")
  in
  fail_unless
    (consume_gas_limit_in_block context too_much |> failed)
    (err "Fail when consuming more than the remaining block gas.")

(** Test invalid gas limit. Should fail when limit is above the hard gas limit per
    operation *)
let test_detect_gas_limit_consumption_above_hard_gas_operation_limit () =
  let open Lwt_result_syntax in
  let* context = dummy_context () in
  fail_unless
    (consume_gas_limit_in_block
       context
       (Gas_limit_repr.Arith.unsafe_fp (Z.of_int opg))
    |> failed)
    (err
       "Fail when consuming gas above the hard limit per operation in the \
        block.")

(** For a given [context], check if its levels match those given in [block_level] and
    [operation_level] *)
let check_context_levels context block_level operation_level =
  let open Lwt_result_syntax in
  let op_check =
    match gas_level context with
    | Unaccounted -> true
    | Limited {remaining} ->
        Gas_limit_repr.Arith.(unsafe_fp (Z.of_int operation_level) = remaining)
  in
  let block_check =
    Gas_limit_repr.Arith.(
      unsafe_fp (Z.of_int block_level) = block_gas_level context)
  in
  let* () =
    fail_unless
      (op_check || block_check)
      (err "Unexpected block and operation gas levels")
  in
  let* () = fail_unless op_check (err "Unexpected operation gas level") in
  fail_unless block_check (err "Unexpected block gas level")

let monitor remaining_block_gas initial_operation_level consumed_gas () =
  let open Lwt_result_syntax in
  let op_limit =
    Gas_limit_repr.Arith.unsafe_fp (Z.of_int initial_operation_level)
  in
  let* context = make_context remaining_block_gas in
  let* context = consume_gas_limit_in_block_lwt context op_limit in
  set_gas_limit context op_limit |> fun context ->
  let* context = consume_gas_lwt context consumed_gas in
  check_context_levels
    context
    (remaining_block_gas - initial_operation_level)
    (initial_operation_level - consumed_gas)

let test_monitor_gas_level = monitor 1000 100 10

(** Test cas consumption mode switching (limited -> unlimited) *)
let test_set_gas_unlimited () =
  let open Lwt_result_syntax in
  let init_block_gas = 100000 in
  let op_limit_int = 10000 in
  let op_limit = Gas_limit_repr.Arith.unsafe_fp (Z.of_int op_limit_int) in
  let* context = make_context init_block_gas in
  set_gas_limit context op_limit |> set_gas_unlimited |> fun context ->
  let* context = consume_gas_lwt context opg in
  check_context_levels context init_block_gas (-1)

(** Test cas consumption mode switching (unlimited -> limited) *)
let test_set_gas_limited () =
  let open Lwt_result_syntax in
  let init_block_gas = 100000 in
  let op_limit_int = 10000 in
  let op_limit = Gas_limit_repr.Arith.unsafe_fp (Z.of_int op_limit_int) in
  let op_gas = 100 in
  let* context = make_context init_block_gas in
  set_gas_unlimited context |> fun context ->
  set_gas_limit context op_limit |> fun context ->
  let* context = consume_gas_lwt context op_gas in
  check_context_levels context init_block_gas (op_limit_int - op_gas)

(*** Tests with blocks ***)

let begin_validation_and_application ctxt chain_id mode ~predecessor =
  let open Lwt_result_syntax in
  let* validation_state = begin_validation ctxt chain_id mode ~predecessor in
  let* application_state = begin_application ctxt chain_id mode ~predecessor in
  return (validation_state, application_state)

let validate_and_apply_operation (validation_state, application_state) op =
  let open Lwt_result_syntax in
  let oph = Alpha_context.Operation.hash_packed op in
  let* validation_state = validate_operation validation_state oph op in
  let* application_state, receipt = apply_operation application_state oph op in
  return ((validation_state, application_state), receipt)

let finalize_validation_and_application (validation_state, application_state)
    shell_header =
  let open Lwt_result_syntax in
  let* () = finalize_validation validation_state in
  finalize_application application_state shell_header

let apply_with_gas header ?(operations = []) (pred : Block.t) =
  let open Lwt_result_wrap_syntax in
  let open Alpha_context in
  let+@ context, consumed_gas =
    let* vstate =
      begin_validation_and_application
        pred.context
        Chain_id.zero
        (Application header)
        ~predecessor:pred.header.shell
    in
    let* vstate =
      List.fold_left_es
        (fun vstate op ->
          let+ state, _result = validate_and_apply_operation vstate op in
          state)
        vstate
        operations
    in
    let+ validation, result =
      finalize_validation_and_application vstate (Some header.shell)
    in
    (validation.context, result.consumed_gas)
  in
  let hash = Block_header.hash header in
  ( {Block.hash; header; operations; context; constants = pred.constants},
    consumed_gas )

let bake_with_gas ?policy ?timestamp ?operation ?operations pred =
  let open Lwt_result_syntax in
  let operations =
    match (operation, operations) with
    | Some op, Some ops -> Some (op :: ops)
    | Some op, None -> Some [op]
    | None, Some ops -> Some ops
    | None, None -> None
  in
  let* header = Block.Forge.forge_header ?timestamp ?policy ?operations pred in
  let* header = Block.Forge.sign_header header in
  apply_with_gas header ?operations pred

let check_consumed_gas consumed expected =
  fail_unless
    Alpha_context.Gas.Arith.(consumed = expected)
    (err
       (Format.asprintf
          "Gas discrepancy: consumed gas : %a | expected : %a\n"
          Alpha_context.Gas.Arith.pp
          consumed
          Alpha_context.Gas.Arith.pp
          expected))

let lazy_unit = Alpha_context.Script.lazy_expr (Expr.from_string "Unit")

let prepare_origination block source script =
  let code = Expr.toplevel_from_string script in
  let script =
    Alpha_context.Script.{code = lazy_expr code; storage = lazy_unit}
  in
  Op.contract_origination (B block) source ~script

let originate_contract block source script =
  let open Lwt_result_syntax in
  let* operation, dst = prepare_origination block source script in
  let* block = Block.bake ~operation block in
  return (block, dst)

let init_block n to_originate =
  let open Lwt_result_syntax in
  let* block, src_list = Context.init_n n ~consensus_threshold_size:0 () in
  match src_list with
  | [] -> assert false
  | src :: _ ->
      (*** originate contracts ***)
      let rec full_originate block originated = function
        | [] -> return (block, List.rev originated)
        | h :: t ->
            let* block, ct = originate_contract block src h in
            full_originate block (ct :: originated) t
      in
      let* block, originated = full_originate block [] to_originate in
      return (block, src_list, originated)

let nil_contract =
  "parameter unit;\n\
   storage unit;\n\
   code {\n\
  \       DROP;\n\
  \       UNIT; NIL operation; PAIR\n\
  \     }\n"

let fail_contract = "parameter unit; storage unit; code { FAIL }"

let loop_contract =
  "parameter unit;\n\
   storage unit;\n\
   code {\n\
  \       DROP;\n\
  \       PUSH bool True;\n\
  \       LOOP {\n\
  \              PUSH string \"GASGASGAS\";\n\
  \              PACK;\n\
  \              SHA3;\n\
  \              DROP;\n\
  \              PUSH bool True\n\
  \            };\n\
  \       UNIT; NIL operation; PAIR\n\
  \     }\n"

let block_with_one_origination n contract =
  let open Lwt_result_syntax in
  let* block, srcs, originated = init_block n [contract] in
  match originated with [dst] -> return (block, srcs, dst) | _ -> assert false

let full_block n () =
  let open Lwt_result_syntax in
  let* block, src_list, originated =
    init_block n [nil_contract; fail_contract; loop_contract]
  in
  let dst_nil, dst_fail, dst_loop =
    match originated with [c1; c2; c3] -> (c1, c2, c3) | _ -> assert false
  in
  return (block, src_list, dst_nil, dst_fail, dst_loop)

(** Combine a list of operations into an operation list. Also returns
    the sum of their gas limits.*)
let combine_operations_with_gas block list_dst =
  let open Lwt_result_syntax in
  let rec make_op_list src full_gas op_list = function
    | [] -> return (src, full_gas, List.rev op_list)
    | (src, dst, gas_limit) :: t ->
        let* op =
          Op.transaction
            ~gas_limit:(Custom_gas gas_limit)
            (B block)
            src
            dst
            Alpha_context.Tez.zero
        in
        make_op_list
          (Some src)
          (Alpha_context.Gas.Arith.add full_gas gas_limit)
          (op :: op_list)
          t
  in
  let* src, full_gas, op_list =
    make_op_list None Alpha_context.Gas.Arith.zero [] list_dst
  in
  match src with
  | None -> assert false
  | Some source ->
      let* operation =
        Op.batch_operations ~recompute_counters:true ~source (B block) op_list
      in
      return (operation, full_gas)

(** Applies [combine_operations_with_gas] to lists in a list, then bake a block
    with this list of operations. Also returns the sum of all gas limits *)
let bake_operations_with_gas block list_list_dst =
  let open Lwt_result_syntax in
  let rec make_list full_gas op_list = function
    | [] -> return (full_gas, List.rev op_list)
    | list_dst :: t ->
        let* op, gas = combine_operations_with_gas block list_dst in
        make_list (Alpha_context.Gas.Arith.add full_gas gas) (op :: op_list) t
  in
  let* gas_limit_total, operations =
    make_list Alpha_context.Gas.Arith.zero [] list_list_dst
  in
  let* block, consumed_gas = bake_with_gas ~operations block in
  return (block, consumed_gas, gas_limit_total)

(* A sampler for gas limits, the returned value should always be high
   enough to apply a simple manager operation but lower than the
   operation gas limit. *)
let basic_gas_sampler () =
  Alpha_context.Gas.Arith.integral_of_int_exn
    (Michelson_v1_gas.Internal_for_tests.int_cost_of_manager_operation + 1000
   + Random.int 900)

let generic_test_block_one_origination contract gas_sampler structure =
  let open Lwt_result_syntax in
  let sources_number = List.length structure in
  let* block, src_list, dst =
    block_with_one_origination sources_number contract
  in
  let lld =
    List.mapi
      (fun i t ->
        match List.nth src_list i with
        | None -> assert false
        | Some src -> (List.map (fun _ -> (src, dst, gas_sampler ()))) t)
      structure
  in
  let* _block, consumed_gas, gas_limit_total =
    bake_operations_with_gas block lld
  in
  check_consumed_gas consumed_gas gas_limit_total

let make_batch_test_block_one_origination name contract gas_sampler =
  let test = generic_test_block_one_origination contract gas_sampler in
  let test_one_operation () = test [[()]] in
  let test_one_operation_list () = test [[(); (); ()]] in
  let test_many_single_operations () = test [[()]; [()]; [()]] in
  let test_mixed_operations () = test [[(); ()]; [()]; [(); (); ()]] in
  let app_n = List.map (fun (x, y) -> (x ^ " with contract " ^ name, y)) in
  app_n
    [
      ("bake one operation", test_one_operation);
      ("bake one operation list", test_one_operation_list);
      ("multiple single operations", test_many_single_operations);
      ("both lists and single operations", test_mixed_operations);
    ]

let hard_gas_limit_per_operation = 1_040_000

let hard_gas_limit_per_block = 1_386_666

(** Tests the consumption of all gas in a block, should pass *)
let test_consume_exactly_all_block_gas () =
  let open Lwt_result_syntax in
  let number_of_ops = 1 in
  let* block, src_list, dst =
    block_with_one_origination number_of_ops nil_contract
  in
  let lld =
    List.map
      (fun src ->
        [
          ( src,
            dst,
            Alpha_context.Gas.Arith.integral_of_int_exn
              hard_gas_limit_per_operation );
        ])
      src_list
  in
  let* _, _, _ = bake_operations_with_gas block lld in
  return_unit

(** Tests the consumption of more than the block gas level with many single
    operations, should fail *)
let test_malformed_block_max_limit_reached () =
  let open Lwt_result_syntax in
  let number_of_ops = 2 in
  let* block, src_list, dst =
    block_with_one_origination number_of_ops nil_contract
  in
  let lld =
    List.mapi
      (fun i src ->
        [
          ( src,
            dst,
            Alpha_context.Gas.Arith.integral_of_int_exn
              (if i = number_of_ops - 1 then
                 hard_gas_limit_per_block - hard_gas_limit_per_operation + 1
               else hard_gas_limit_per_operation) );
        ])
      src_list
  in
  let*! result = bake_operations_with_gas block lld in
  match result with
  | Error _ -> return_unit
  | Ok _ ->
      tzfail
        (err
           "Invalid block: sum of operation gas limits exceeds hard gas limit \
            per block")

(** Tests the consumption of more than the block gas level with one big
    operation list, should fail *)
let test_malformed_block_max_limit_reached' () =
  let open Lwt_result_syntax in
  let number_of_ops = 2 in
  let* block, src_list, dst =
    block_with_one_origination number_of_ops nil_contract
  in
  let lld =
    List.mapi
      (fun i src ->
        [
          ( src,
            dst,
            Alpha_context.Gas.Arith.integral_of_int_exn
              (if i = number_of_ops - 1 then
                 hard_gas_limit_per_block - hard_gas_limit_per_operation + 1
               else hard_gas_limit_per_operation) );
        ])
      src_list
  in
  let*! result = bake_operations_with_gas block lld in
  match result with
  | Error _ -> return_unit
  | Ok _ ->
      tzfail
        (err
           "Invalid block: sum of gas limits in operation list exceeds hard \
            gas limit per block")

let test_block_mixed_operations () =
  let open Lwt_result_syntax in
  let number_of_ops = 4 in
  let* block, src_list, dst_nil, dst_fail, dst_loop =
    full_block number_of_ops ()
  in
  let l = [[dst_nil]; [dst_nil; dst_fail; dst_nil]; [dst_loop]; [dst_nil]] in
  let*? lld =
    List.map2
      ~when_different_lengths:[]
      (fun src l -> (List.map (fun x -> (src, x, basic_gas_sampler ()))) l)
      src_list
      l
  in
  let* _block, consumed_gas, gas_limit_total =
    bake_operations_with_gas block lld
  in
  check_consumed_gas consumed_gas gas_limit_total

(** Test that emptying an account does not cost extra-gas *)
let test_emptying_account_gas () =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let bootstrap_pkh = Context.Contract.pkh bootstrap in
  let {Account.pkh; pk; _} = Account.new_account () in
  let contract = Contract.Implicit pkh in
  let amount = Tez_helpers.of_int 10 in
  let* op1 = Op.transaction (B b) bootstrap contract amount in
  let* b = Block.bake ~operation:op1 b in
  let* op2 = Op.revelation ~fee:Tez.zero (B b) pk in
  let* b = Block.bake ~operation:op2 b in
  let gas_limit = Op.Low in
  let* op =
    Op.delegation ~fee:amount ~gas_limit (B b) contract (Some bootstrap_pkh)
  in
  let* i = Incremental.begin_construction b in
  (* The delegation operation should be valid as the operation effect
     would be to remove [contract] and should not generate any extra
     gas cost. *)
  let expect_apply_failure = function
    | [Environment.Ecoproto_error (Storage_error (Raw_context.Missing_key _))]
      ->
        (* The delegation is expected to fail in the apply part as the
           contract was emptied when fees were retrieved. *)
        return_unit
    | err -> failwith "got unexpected error: %a" pp_print_trace err
  in
  let* (_i : Incremental.t) =
    Incremental.add_operation ~expect_apply_failure i op
  in
  return_unit

let quick (what, how) = Tztest.tztest what `Quick how

let tests =
  List.map
    quick
    ([
       ( "Detect gas exhaustion in fresh context",
         test_detect_gas_exhaustion_in_fresh_context );
       ( "Detect gas exhaustion when operation gas as hits zero",
         test_detect_gas_exhaustion_when_operation_gas_hits_zero );
       ( "Detect gas exhaustion when block gas as hits zero",
         test_detect_gas_exhaustion_when_block_gas_hits_zero );
       ( "Detect gas limit consumption when it is above the hard gas operation \
          limit",
         test_detect_gas_limit_consumption_above_hard_gas_operation_limit );
       ( "Each new operation impacts block gas level, each gas consumption \
          impacts operation gas level",
         test_monitor_gas_level );
       ( "Switches operation gas consumption from limited to unlimited",
         test_set_gas_unlimited );
       ( "Switches operation gas consumption from unlimited to limited",
         test_set_gas_limited );
       ( "Accepts a block that consumes all of its gas",
         test_consume_exactly_all_block_gas );
       ( "Detect when the sum of all operation gas limits exceeds the hard gas \
          limit per block",
         test_malformed_block_max_limit_reached );
       ( "Detect when gas limit of operation list exceeds the hard gas limit \
          per block",
         test_malformed_block_max_limit_reached' );
       ("the gas consumption of various operations", test_block_mixed_operations);
       ("emptying an account costs gas", test_emptying_account_gas);
     ]
    @ make_batch_test_block_one_origination "nil" nil_contract basic_gas_sampler
    @ make_batch_test_block_one_origination
        "fail"
        fail_contract
        basic_gas_sampler
    @ make_batch_test_block_one_origination
        "infinite loop"
        loop_contract
        basic_gas_sampler)

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("gas levels", tests)]
  |> Lwt_main.run
