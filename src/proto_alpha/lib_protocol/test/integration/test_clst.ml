(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol (CLST)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                  -- --file test_clst.ml
    Subject:      CLST contract
*)

open Protocol
open Alpha_context

let register_test ~title =
  Tezt_helpers.register_test_es
    ~__FILE__
    ~file_tags:["clst"]
    ~title:("CLST: " ^ title)

let run_view ~contract ~view_name ~input (block : Block.t) =
  let chain_id = Chain_id.of_block_hash block.hash in
  Plugin.RPC.Scripts.run_script_view
    ~gas:None
    ~other_contracts:None
    ~extra_big_maps:None
    ~contract
    ~view:view_name
    ~input
    ~unlimited_gas:true
    ~now:None
    ~chain_id
    ~level:None
    ~sender:None
    ~payer:None
    ~unparsing_mode:Script_ir_unparser.Readable
    Block.rpc_ctxt
    block

let run_tzip4_view ~contract ~entrypoint_name ~input (block : Block.t) =
  let chain_id = Chain_id.of_block_hash block.hash in
  Plugin.RPC.Scripts.run_tzip4_view
    ~gas:None
    ~other_contracts:None
    ~extra_big_maps:None
    ~contract
    ~entrypoint:entrypoint_name
    ~input
    ~now:None
    ~chain_id
    ~level:None
    ~sender:None
    ~payer:None
    ~unparsing_mode:Script_ir_unparser.Readable
    Block.rpc_ctxt
    block

let get_clst_hash ctxt =
  let open Lwt_result_wrap_syntax in
  let* alpha_ctxt = Context.get_alpha_ctxt ctxt in
  let*@ hash = Contract.get_clst_contract_hash alpha_ctxt in
  return hash

let total_amount_of_tez ctxt =
  let open Lwt_result_wrap_syntax in
  let* alpha_ctxt = Context.get_alpha_ctxt ctxt in
  let*@ amount = Clst.total_amount_of_tez alpha_ctxt in
  return amount

let get_allowance ~owner ~spender b =
  let open Lwt_result_wrap_syntax in
  let* clst_hash = get_clst_hash (B b) in
  let* allowance =
    run_view
      ~contract:clst_hash
      ~view_name:"get_allowance"
      ~input:
        Environment.Micheline.(
          Prim
            ( dummy_location,
              Script.D_Pair,
              [
                String (dummy_location, Contract.to_b58check owner);
                String (dummy_location, Contract.to_b58check spender);
                Int (dummy_location, Z.zero);
              ],
              [] )
          |> strip_locations)
      b
  in
  let allowance =
    match allowance |> Environment.Micheline.root with
    | Environment.Micheline.Int (_, allowance_z) -> allowance_z |> Z.to_int64
    | _ -> Test.fail "Unexpected output"
  in
  return allowance

let is_operator_view ~owner ~operator b =
  let open Lwt_result_wrap_syntax in
  let* clst_hash = get_clst_hash (B b) in
  let* result =
    run_view
      ~contract:clst_hash
      ~view_name:"is_operator"
      ~input:
        Environment.Micheline.(
          Prim
            ( dummy_location,
              Script.D_Pair,
              [
                String (dummy_location, Contract.to_b58check owner);
                String (dummy_location, Contract.to_b58check operator);
                Int (dummy_location, Z.zero);
              ],
              [] )
          |> strip_locations)
      b
  in
  let is_op =
    match result |> Environment.Micheline.root with
    | Environment.Micheline.Prim (_, Script.D_True, [], []) -> true
    | Environment.Micheline.Prim (_, Script.D_False, [], []) -> false
    | _ -> Test.fail "Unexpected output"
  in
  return is_op

let create_funded_account ~funder ~amount_mutez b =
  let open Lwt_result_wrap_syntax in
  let account = Account.new_account () in
  let account = Contract.Implicit account.Account.pkh in
  let* op =
    Op.transaction (B b) funder account (Tez.of_mutez_exn amount_mutez)
  in
  let* b = Block.bake ~operation:op b in
  return (account, b)

(* Checks the difference [diff] between an initial [init] and a
   current CLST balances. Both [init] and [diff] are given in
   mutez. *)
let check_clst_balance_diff ~loc ~init ~diff b account =
  let open Lwt_result_syntax in
  let* balance =
    Plugin.Contract_services.stez_balance Block.rpc_ctxt b account
  in
  let balance =
    Option.value_f
      ~default:(fun () -> assert false)
      (Script_int.to_int64 balance)
  in
  let expected_balance = Int64.add init diff in
  Assert.equal_int64 ~loc expected_balance balance

(* Checks the block's balance updates contains the given balance updates, in the
   given order. *)
let check_balance_updates full_metadata expected_balance_updates =
  let balance_updates = Block.get_balance_updates_from_metadata full_metadata in
  (* The encoding for the single balance update is not exported, so
     balance_update_item are wrapped as a singleton. *)
  let pp_balance_update ppf bal =
    Format.fprintf
      ppf
      "%s"
      Data_encoding.Json.(
        to_string (construct Receipt.balance_updates_encoding [bal]))
  in
  Assert.assert_is_subset_list
    ~loc:__LOC__
    ( = )
    "Unexpected balance update"
    pp_balance_update
    expected_balance_updates
    balance_updates

let check_expected_parameters ~loc expected parameters =
  let open Lwt_result_syntax in
  let* () =
    Assert.equal_int32
      ~loc
      expected
        .Clst_delegates_parameters_repr
         .edge_of_clst_staking_over_baking_millionth
      parameters
        .Clst_delegates_parameters_repr
         .edge_of_clst_staking_over_baking_millionth
  in
  Assert.equal_int32
    ~loc
    expected.ratio_of_clst_staking_over_direct_staking_billionth
    parameters
      .Clst_delegates_parameters_repr
       .ratio_of_clst_staking_over_direct_staking_billionth

let check_expected_pending_parameters ~loc expected parameters =
  let open Lwt_result_syntax in
  match (expected, parameters) with
  | ( Clst_delegates_parameters_repr.Update expected,
      Clst_delegates_parameters_repr.Update parameters ) ->
      check_expected_parameters ~loc expected parameters
  | ( Clst_delegates_parameters_repr.Unregister,
      Clst_delegates_parameters_repr.Unregister ) ->
      return_unit
  | _, _ ->
      let parameter_to_string parameters =
        Data_encoding.Json.construct
          Clst_delegates_parameters_repr.update_encoding
          parameters
        |> Data_encoding.Json.to_string
      in
      Test.fail
        ~__LOC__:loc
        "Expected parameter:\n%s\ngot:\n%s"
        (parameter_to_string expected)
        (parameter_to_string parameters)

(* Returns a list of event type names extracted from internal event
   operations of packed operation metadata *)
let extract_events (metadata : Apply_results.packed_operation_metadata) =
  let extract_from_internal_ops internal_operation_results =
    List.filter_map
      (fun (Apply_internal_results.Internal_operation_result (op, _result)) ->
        match op.operation with
        | Event {tag = _; payload = _; ty} -> (
            match ty |> Environment.Micheline.root with
            | Environment.Micheline.Prim (_, _, _, [annot]) -> Some annot
            | _ -> None)
        | _ -> None)
      internal_operation_results
  in
  let extract_from_result : type a. a Apply_results.contents_result -> _ =
    function
    | Manager_operation_result {internal_operation_results; _} ->
        extract_from_internal_ops internal_operation_results
    | _ -> []
  in
  let rec extract_from_contents_result_list : type a.
      a Apply_results.contents_result_list -> _ = function
    | Single_result r -> extract_from_result r
    | Cons_result (r, rest) ->
        extract_from_result r @ extract_from_contents_result_list rest
  in
  match metadata with
  | Apply_results.Operation_metadata {contents} ->
      extract_from_contents_result_list contents
  | Apply_results.No_operation_metadata -> []

let get_events_from_metadata metadata =
  let _header_metadata, operation_receipts = metadata in
  List.concat_map extract_events operation_receipts

let check_number_events ~loc ~expected event_type events =
  let count = List.length (List.filter (String.equal event_type) events) in
  Assert.equal_int ~loc count expected

let test_deposit =
  register_test ~title:"Test deposit of a non null amount" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let amount = Tez.of_mutez_exn 100000000L in
  let* deposit_tx = Op.clst_deposit (Context.B b) sender amount in
  let* b, full_metadata = Block.bake_with_metadata ~operation:deposit_tx b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:0L
      ~diff:(Tez.to_mutez amount)
      b
      sender
  in
  let* clst_contract_hash = get_clst_hash (Context.B b) in
  let expected_balance_updates =
    [
      Alpha_context.Receipt.(
        item
          (Contract (Originated clst_contract_hash))
          (Debited amount)
          Block_application);
      Receipt.(item CLST_deposits (Credited amount) Block_application);
    ]
  in
  check_balance_updates full_metadata expected_balance_updates

let test_deposit_zero =
  register_test ~title:"Test depositing 0 tez amount is forbidden" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let amount = Tez.of_mutez_exn 0L in
  let* deposit_tx = Op.clst_deposit (Context.B b) sender amount in
  let*! b = Block.bake ~operation:deposit_tx b in
  match b with
  | Ok _ ->
      Test.fail "Empty deposits on CLST are forbidden and expected to fail"
  | Error trace -> Error_helpers.expect_clst_empty_transfer ~loc:__LOC__ trace

(*
   Contract taking a contract (or <contract>%<entrypoint>) as parameter, and
   transfers it any tez it has received with the initial contract call.

{ storage unit;
  parameter (contract unit);
  code
    {
      UNPAIR ;
      AMOUNT;
      UNIT;
      TRANSFER_TOKENS ;
      NIL operation ;
      SWAP ;
      CONS ;
      PAIR ;
    };
}
*)
let proxy_contract =
  let open Environment.Micheline in
  let open Script in
  {
    code =
      lazy_expr
        (strip_locations
           (Seq
              ( (),
                [
                  Prim ((), K_storage, [Prim ((), T_unit, [], [])], []);
                  Prim
                    ( (),
                      K_parameter,
                      [Prim ((), T_contract, [Prim ((), T_unit, [], [])], [])],
                      [] );
                  Prim
                    ( (),
                      K_code,
                      [
                        Seq
                          ( (),
                            [
                              Prim ((), I_UNPAIR, [], []);
                              Prim ((), I_AMOUNT, [], []);
                              Prim ((), I_UNIT, [], []);
                              Prim ((), I_TRANSFER_TOKENS, [], []);
                              Prim
                                ((), I_NIL, [Prim ((), T_operation, [], [])], []);
                              Prim ((), I_SWAP, [], []);
                              Prim ((), I_CONS, [], []);
                              Prim ((), I_PAIR, [], []);
                            ] );
                      ],
                      [] );
                ] )));
    storage = lazy_expr (strip_locations (Prim ((), D_Unit, [], [])));
  }

let test_deposit_from_originated_contract =
  register_test ~title:"Test contract depositing is forbidden" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let* origination_op, proxy_hash =
    Op.contract_origination ~script:proxy_contract (Context.B b) sender
  in
  let* b = Block.bake ~operation:origination_op b in
  let* clst_hash = get_clst_hash (Context.B b) in
  let* operation =
    let open Environment.Micheline in
    let open Script in
    Op.transaction
      ~parameters:
        (lazy_expr
           (strip_locations
              (String ((), Contract_hash.to_b58check clst_hash ^ "%deposit"))))
      (Context.B b)
      sender
      proxy_hash
      Tez.one
  in
  let*! b = Block.bake ~operation b in
  match b with
  | Ok _ ->
      Test.fail
        "Deposits from smart contracts are forbidden and expected to fail"
  | Error trace ->
      Error_helpers.expect_clst_non_implicit_depositer ~loc:__LOC__ trace

let () =
  register_test ~title:"Test simple redeem" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, funder = Context.init1 ~consensus_threshold_size:0 () in
  let initial_bal_mutez = 300_000_000L in
  let* account, b =
    create_funded_account ~funder ~amount_mutez:initial_bal_mutez b
  in
  let initial_clst_bal_mutez = 100_000_000L in
  let* deposit_tx =
    Op.clst_deposit
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      account
      (Tez.of_mutez_exn initial_clst_bal_mutez)
  in
  let* b = Block.bake ~operation:deposit_tx b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:0L
      ~diff:initial_clst_bal_mutez
      b
      account
  in

  let* frozen_redeemed_balance_before =
    Context.CLST.redeemed_frozen_balance (B b) account
  in
  let frozen_redeemed_balance_before =
    Option.value ~default:Tez.zero frozen_redeemed_balance_before
  in
  let redeemed_amount_mutez = 30_000_000L in
  let redeemed_amount = Tez.of_mutez_exn redeemed_amount_mutez in
  let* redeem_tx =
    Op.clst_redeem ~fee:Tez.zero (B b) account redeemed_amount_mutez
  in
  let* b, full_metadata = Block.bake_with_metadata ~operation:redeem_tx b in
  let* () =
    Assert.clst_frozen_redeemed_balance_was_credited
      ~loc:__LOC__
      (B b)
      account
      frozen_redeemed_balance_before
      redeemed_amount
  in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:initial_clst_bal_mutez
      ~diff:(Int64.neg redeemed_amount_mutez)
      b
      account
  in
  let cycle = Block.current_cycle b in
  let expected_balance_updates =
    [
      Alpha_context.Receipt.(
        item CLST_deposits (Debited redeemed_amount) Block_application);
      Receipt.(
        item
          (CLST_redeemed_deposits (account, cycle))
          (Credited redeemed_amount)
          Block_application);
    ]
  in
  let* () = check_balance_updates full_metadata expected_balance_updates in
  return_unit

let () =
  register_test ~title:"Test overredeem" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, funder = Context.init1 ~consensus_threshold_size:0 () in
  let initial_bal_mutez = 300_000_000L in
  let* account, b =
    create_funded_account ~funder ~amount_mutez:initial_bal_mutez b
  in
  let redeemed_amount_mutez = 30_000_000L in
  let* redeem_tx =
    Op.clst_redeem
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      account
      redeemed_amount_mutez
  in
  let*! b = Block.bake ~operation:redeem_tx b in
  match b with
  | Ok _ ->
      Test.fail "Redeeming more clst tokens than the contract has is forbidden"
  | Error trace -> Error_helpers.expect_clst_balance_too_low ~loc:__LOC__ trace

let () =
  register_test ~title:"Test zero redeem" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, funder = Context.init1 ~consensus_threshold_size:0 () in
  let initial_bal_mutez = 300_000_000L in
  let* account, b =
    create_funded_account ~funder ~amount_mutez:initial_bal_mutez b
  in
  let redeemed_amount_mutez = 0L in
  let* redeem_tx =
    Op.clst_redeem
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      account
      redeemed_amount_mutez
  in
  let*! b = Block.bake ~operation:redeem_tx b in
  match b with
  | Ok _ -> Test.fail "Redeeming 0 tez is forbidden"
  | Error trace -> Error_helpers.expect_clst_empty_transfer ~loc:__LOC__ trace

let () =
  register_test ~title:"Test redeem with non-zero transfer" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, funder = Context.init1 ~consensus_threshold_size:0 () in
  let initial_bal_mutez = 300_000_000L in
  let* account, b =
    create_funded_account ~funder ~amount_mutez:initial_bal_mutez b
  in
  let* clst_hash = get_clst_hash (Context.B b) in
  let redeemed_amount_mutez = 10L in
  let* withdraw_tx =
    Op.transaction
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      account
      (Contract.Originated clst_hash)
      (Tez.of_mutez_exn 30_000L)
      ~entrypoint:(Entrypoint.of_string_strict_exn "redeem")
      ~parameters:
        (Alpha_context.Script.lazy_expr
           (Expr.from_string (Int64.to_string redeemed_amount_mutez)))
  in
  let*! b = Block.bake ~operation:withdraw_tx b in
  match b with
  | Ok _ -> Test.fail "Transferring to redeem is forbidden"
  | Error trace ->
      Error_helpers.expect_clst_non_empty_transfer ~loc:__LOC__ trace

let () =
  register_test ~title:"Test get_balance view" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let* clst_hash = get_clst_hash (B b) in
  let amount = Tez.of_mutez_exn 100000000L in
  let* deposit_tx = Op.clst_deposit (B b) sender amount in
  let* b = Block.bake ~operation:deposit_tx b in
  let* balance =
    run_view
      ~contract:clst_hash
      ~view_name:"get_balance"
      ~input:
        Environment.Micheline.(
          Prim
            ( dummy_location,
              Script.D_Pair,
              [
                String (dummy_location, Contract.to_b58check sender);
                Int (dummy_location, Z.zero);
              ],
              [] )
          |> strip_locations)
      b
  in
  let balance =
    match balance |> Environment.Micheline.root with
    | Environment.Micheline.Int (_, balance_z) -> balance_z |> Z.to_int64
    | _ -> Test.fail "Unexpected output"
  in
  let amount = Tez.to_mutez amount in
  let* () = Assert.equal_int64 ~loc:__LOC__ amount balance in
  return_unit

let test_total_supply (total_supply_f : Block.t -> int64 tzresult Lwt.t) =
  let open Lwt_result_wrap_syntax in
  let check_rpcs ~loc b expected_in_mutez =
    (* total_supply = total_amount_of_tez
       exchange_rate = 1 *)
    let* total_supply = total_supply_f b in
    let* total_amount_of_tez =
      Plugin.Contract_services.stez_total_amount_of_tez Block.rpc_ctxt b
    in
    let* exchange_rate =
      Plugin.Contract_services.stez_exchange_rate Block.rpc_ctxt b
    in
    let* () = Assert.equal_int64 ~loc total_supply expected_in_mutez in
    let* () =
      Assert.equal_int64
        ~loc
        (Tez.to_mutez total_amount_of_tez)
        expected_in_mutez
    in
    let* () = Assert.is_true ~loc (Q.equal exchange_rate Q.one) in
    return_unit
  in
  let* b, funder = Context.init1 ~consensus_threshold_size:0 () in
  let initial_bal_mutez = 300_000_000L in
  let* account_a, b =
    create_funded_account ~funder ~amount_mutez:initial_bal_mutez b
  in
  let* account_b, b =
    create_funded_account ~funder ~amount_mutez:initial_bal_mutez b
  in
  let initial_clst_bal_mutez_a = 200_000_000L in
  let initial_clst_bal_mutez_b = 50_000_000L in
  let* deposit_a_tx =
    Op.clst_deposit
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      account_a
      (Tez.of_mutez_exn initial_clst_bal_mutez_a)
  in
  let* deposit_b_tx =
    Op.clst_deposit
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      account_b
      (Tez.of_mutez_exn initial_clst_bal_mutez_b)
  in
  let* b = Block.bake ~operations:[deposit_a_tx; deposit_b_tx] b in
  let expected_total_supply =
    Int64.add initial_clst_bal_mutez_a initial_clst_bal_mutez_b
  in
  let* () = check_rpcs ~loc:__LOC__ b expected_total_supply in
  let redeemed_amount_mutez = 40_000_000L in
  let* withdraw_tx =
    Op.clst_redeem ~fee:Tez.zero (B b) account_a redeemed_amount_mutez
  in
  let* b = Block.bake ~operation:withdraw_tx b in
  let expected_total_supply =
    Int64.sub expected_total_supply redeemed_amount_mutez
  in
  check_rpcs ~loc:__LOC__ b expected_total_supply

let () =
  register_test ~title:"Test get_total_supply view" @@ fun () ->
  test_total_supply (fun b ->
      let open Lwt_result_wrap_syntax in
      let* clst_hash = get_clst_hash (B b) in
      let* total_supply =
        run_view
          ~contract:clst_hash
          ~view_name:"get_total_supply"
          ~input:
            Environment.Micheline.(
              Int (dummy_location, Z.zero) |> strip_locations)
          b
      in
      let total_supply =
        match total_supply |> Environment.Micheline.root with
        | Environment.Micheline.Int (_, balance_z) -> balance_z |> Z.to_int64
        | _ -> Test.fail "Unexpected output"
      in
      return total_supply)

let () =
  register_test ~title:"Test total_supply RPC" @@ fun () ->
  test_total_supply (fun b ->
      let open Lwt_result_syntax in
      let* total_supply =
        Plugin.Contract_services.stez_total_supply Block.rpc_ctxt b
      in
      let total_supply =
        Option.value_f
          ~default:(fun () -> assert false)
          (Script_int.to_int64 total_supply)
      in
      return total_supply)

let () =
  register_test ~title:"Test is_token view" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, _ = Context.init1 () in
  let* clst_hash = get_clst_hash (B b) in
  let* is_token_0 =
    run_view
      ~contract:clst_hash
      ~view_name:"is_token"
      ~input:
        Environment.Micheline.(Int (dummy_location, Z.zero) |> strip_locations)
      b
  in
  let () =
    match is_token_0 |> Environment.Micheline.root with
    | Environment.Micheline.Prim (_, Script.D_True, [], []) -> ()
    | _ -> Test.fail "Unexpected output"
  in
  let* is_token_positive =
    run_view
      ~contract:clst_hash
      ~view_name:"is_token"
      ~input:
        Environment.Micheline.(Int (dummy_location, Z.one) |> strip_locations)
      b
  in
  let () =
    match is_token_positive |> Environment.Micheline.root with
    | Environment.Micheline.Prim (_, Script.D_False, [], []) -> ()
    | _ -> Test.fail "Unexpected output"
  in
  return_unit

let () =
  register_test ~title:"Deposits are not spendable" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let* clst_contract = get_clst_hash (Context.B b) in
  let amount = Tez.of_mutez_exn 100000000L in
  let* deposit_tx = Op.clst_deposit (Context.B b) sender amount in
  let* b = Block.bake ~operation:deposit_tx b in
  (* The balance as retrieved from the context is the spendable balance, the one
     the contract can transfer directly. Since deposits move all the transferred
     assets to the 'deposits balance', it should always be empty. *)
  let* spendable_balance =
    Context.Contract.balance (B b) (Contract.Originated clst_contract)
  in
  let* () = Assert.equal_tez ~loc:__LOC__ spendable_balance Tez.zero in
  (* The deposits balance should contain the total amount of tez transferred
     during the deposit. *)
  let* deposited_balance = total_amount_of_tez (Context.B b) in
  Assert.equal_tez ~loc:__LOC__ deposited_balance amount

let () =
  register_test ~title:"Initial exchange_rate is one" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, _funder = Context.init1 ~consensus_threshold_size:0 () in
  let* exchange_rate =
    Plugin.Contract_services.stez_exchange_rate Block.rpc_ctxt b
  in
  let* () = Assert.is_true ~loc:__LOC__ (Q.equal exchange_rate Q.one) in
  return_unit

let () =
  register_test ~title:"Simple deposit-redeem-finalize flow" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  (* Ensures that neither storage cost nor issuance will modify the balance,
     making it easier to check. *)
  let* b, sender =
    Context.init1
      ~consensus_threshold_size:0
      ~cost_per_byte:Tez.zero
      ~issuance_weights:
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        }
      ()
  in

  (* First deposit 100_000_000 mutez *)
  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (Context.B b) sender amount in
  let* b = Block.bake ~operation:deposit_tx b in

  (* Then redeem 30_000_000 mutez *)
  let redeemed_amount_mutez = 30_000_000L in
  let redeemed_amount = Tez.of_mutez_exn redeemed_amount_mutez in
  let* redeem_tx =
    Op.clst_redeem ~fee:Tez.zero (B b) sender redeemed_amount_mutez
  in
  let* b = Block.bake ~operation:redeem_tx b in
  let redemption_cycle = Block.current_cycle b in

  (* Wait for the redemption request to be finalizable *)
  let* b = Block.bake_until_cycle_end b in
  let finalization_delay =
    b.constants.consensus_rights_delay + Protocol.Constants_repr.slashing_delay
    + 1
  in
  let* b = Block.bake_until_n_cycle_end finalization_delay b in

  (* Finalize, and look at the balance, and ensure it is:
     <amount before finalization> + <redeemed amount>
  *)
  let* balance_before_finalization = Context.Contract.balance (B b) sender in
  let sender_pkh =
    match sender with Contract.Implicit pkh -> pkh | _ -> assert false
  in
  let* finalize_tx =
    Op.clst_finalize_redeem ~fee:Tez.zero (B b) ~sender ~redeemer:sender_pkh
  in
  let* b, full_metadata = Block.bake_with_metadata b ~operation:finalize_tx in
  let* balance_after_finalization = Context.Contract.balance (B b) sender in

  let*?@ expected_balance =
    Tez.(balance_before_finalization +? redeemed_amount)
  in
  let* () =
    Assert.equal_tez ~loc:__LOC__ expected_balance balance_after_finalization
  in

  (* Check the balance updates, showing that the redeemed tez have been moved to
     CLST balance before being sent to the sender.

     Note that for some reasons, internal operations (that sends the tez from
     the contract to the staker) updates appear before the transfer from the
     redeemed deposits to CLST balance, hence we cannot rely on their order and
     need to check them separately. *)
  let* clst_contract_hash = get_clst_hash (Context.B b) in
  let expected_balance_updates_container_to_contract =
    [
      (* First, tez are moved from the redeemed deposits to CLST balance *)
      Alpha_context.Receipt.(
        item
          (CLST_redeemed_deposits (sender, redemption_cycle))
          (Debited redeemed_amount)
          Block_application);
      Receipt.(
        item
          (Contract (Originated clst_contract_hash))
          (Credited redeemed_amount)
          Block_application);
    ]
  in
  let* () =
    check_balance_updates
      full_metadata
      expected_balance_updates_container_to_contract
  in
  let expected_balance_updates_contract_to_staker =
    [
      (* Then transfered to the staker *)
      Alpha_context.Receipt.(
        item
          (Contract (Originated clst_contract_hash))
          (Debited redeemed_amount)
          Block_application);
      Receipt.(
        item (Contract sender) (Credited redeemed_amount) Block_application);
    ]
  in
  let* () =
    check_balance_updates
      full_metadata
      expected_balance_updates_contract_to_staker
  in
  return_unit

let () =
  register_test ~title:"Finalize redeem with sender different from redeemer"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (staker, sender) =
    Context.init2
      ~consensus_threshold_size:0
      ~cost_per_byte:Tez.zero
      ~issuance_weights:
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        }
      ()
  in
  (* staker deposits 100_000_000 mutez *)
  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) staker amount in
  let* b = Block.bake ~operation:deposit_tx b in

  (* staker redeems 30_000_000 mutez *)
  let redeemed_amount_mutez = 30_000_000L in
  let redeemed_amount = Tez.of_mutez_exn redeemed_amount_mutez in
  let* redeem_tx =
    Op.clst_redeem ~fee:Tez.zero (B b) staker redeemed_amount_mutez
  in
  let* b = Block.bake ~operation:redeem_tx b in

  (* wait for the redemption request to be finalizable *)
  let finalization_delay =
    b.constants.consensus_rights_delay + Protocol.Constants_repr.slashing_delay
    + 1
  in
  let* b = Block.bake_until_n_cycle_end finalization_delay b in

  (* sender finalizes the redemption request of staker *)
  let* staker_balance_before = Context.Contract.balance (B b) staker in
  let* sender_balance_before = Context.Contract.balance (B b) sender in
  let staker_pkh =
    match staker with Contract.Implicit pkh -> pkh | _ -> assert false
  in
  let* finalize_tx =
    Op.clst_finalize_redeem
      ~fee:Tez.zero
      (Context.B b)
      ~sender
      ~redeemer:staker_pkh
  in
  let* b = Block.bake b ~operation:finalize_tx in

  (* staker's spendable balance is increased by the redeemed amount *)
  let* staker_balance_after = Context.Contract.balance (B b) staker in
  let*?@ expected_staker_balance =
    Tez.(staker_balance_before +? redeemed_amount)
  in
  let* () =
    Assert.equal_tez ~loc:__LOC__ expected_staker_balance staker_balance_after
  in
  (* sender's spendable balance is not changed *)
  let* sender_balance_after = Context.Contract.balance (B b) sender in
  let* () =
    Assert.equal_tez ~loc:__LOC__ sender_balance_before sender_balance_after
  in
  return_unit

let () =
  register_test ~title:"Test redeemed balance with non finalizable requests"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  (* Ensures that neither storage cost nor issuance will modify the balance,
     making it easier to check. *)
  let* b, sender =
    Context.init1
      ~consensus_threshold_size:0
      ~cost_per_byte:Tez.zero
      ~issuance_weights:
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        }
      ()
  in

  (* First deposit 100_000_000 mutez *)
  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (Context.B b) sender amount in
  let* b = Block.bake ~operation:deposit_tx b in

  (* Then redeem 30_000_000 mutez *)
  let first_redeemed_amount_mutez = 30_000_000L in
  let first_redeemed_amount = Tez.of_mutez_exn first_redeemed_amount_mutez in
  let* first_redeem_tx =
    Op.clst_redeem ~fee:Tez.zero (B b) sender first_redeemed_amount_mutez
  in
  let* b = Block.bake ~operation:first_redeem_tx b in

  let first_redeem_cycle = Block.current_cycle b in
  let* b = Block.bake_until_cycle_end b in

  (* Now, let's redeem 10_000_000 mutez at the next cycle *)
  let second_redeemed_amount_mutez = 10_000_000L in
  let second_redeemed_amount = Tez.of_mutez_exn second_redeemed_amount_mutez in
  let* second_redeem_tx =
    Op.clst_redeem ~fee:Tez.zero (B b) sender second_redeemed_amount_mutez
  in
  let* b = Block.bake ~operation:second_redeem_tx b in
  let _second_redeem_cycle = Block.current_cycle b in
  let* b = Block.bake_until_cycle_end b in

  (* Wait for the first redemption request to be finalizable *)
  let* b = Block.bake_until_cycle_end b in
  let finalization_delay =
    b.constants.consensus_rights_delay + Protocol.Constants_repr.slashing_delay
    + 1
  in
  let* b =
    Block.bake_until_cycle Cycle.(add first_redeem_cycle finalization_delay) b
  in

  (* Let's check the frozen and finalizable balances *)
  let* () =
    Assert.stez_frozen_redeemed_balance_is
      ~loc:__LOC__
      b
      sender
      second_redeemed_amount
  in
  let* () =
    Assert.stez_finalizable_redeemed_balance_is
      ~loc:__LOC__
      b
      sender
      first_redeemed_amount
  in

  (* Finalize, and look at the balance, and ensure it is:
     <amount before finalization> + <first redeemed amount>
  *)
  let* balance_before_finalization = Context.Contract.balance (B b) sender in
  let sender_pkh =
    match sender with Contract.Implicit pkh -> pkh | _ -> assert false
  in
  let* finalize_tx =
    Op.clst_finalize_redeem ~fee:Tez.zero (B b) ~sender ~redeemer:sender_pkh
  in
  let* b = Block.bake b ~operation:finalize_tx in
  let* balance_after_finalization = Context.Contract.balance (B b) sender in

  let*?@ expected_balance =
    Tez.(balance_before_finalization +? first_redeemed_amount)
  in
  let* () =
    Assert.equal_tez ~loc:__LOC__ expected_balance balance_after_finalization
  in

  (* Let's check the frozen and finalizable balances again, frozen shouldn't
     have changed, but there shouldn't be any finalizable balance anymore. *)
  let* () =
    Assert.stez_frozen_redeemed_balance_is
      ~loc:__LOC__
      b
      sender
      second_redeemed_amount
  in
  Assert.stez_finalizable_redeemed_balance_is ~loc:__LOC__ b sender Tez.zero

let () =
  register_test ~title:"Test allowance entrypoint and get_allowance view"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (owner, spender) = Context.init2 ~consensus_threshold_size:0 () in
  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) owner amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* allowance = get_allowance ~owner ~spender b in
  let* () = Assert.equal_int64 ~loc:__LOC__ 0L allowance in

  let amount_incr_allowance = 40_000L in
  let* approve_tx =
    Op.clst_approve
      (B b)
      ~sender:owner
      [(owner, spender, amount_incr_allowance)]
  in
  let* b = Block.bake ~operation:approve_tx b in
  let* allowance = get_allowance ~owner ~spender b in
  let* () = Assert.equal_int64 ~loc:__LOC__ amount_incr_allowance allowance in

  let amount_decr_allowance = -30_000L in
  let* approve_tx =
    Op.clst_approve
      (B b)
      ~sender:owner
      [(owner, spender, amount_decr_allowance)]
  in
  let* b = Block.bake ~operation:approve_tx b in
  let* allowance = get_allowance ~owner ~spender b in
  let* () = Assert.equal_int64 ~loc:__LOC__ 10_000L allowance in

  let* approve_tx =
    Op.clst_approve
      (B b)
      ~sender:spender
      [(owner, spender, amount_incr_allowance)]
  in
  let*! b = Block.bake ~operation:approve_tx b in
  match b with
  | Ok _ -> Test.fail "Only owner can change the allowance."
  | Error trace ->
      Error_helpers.expect_clst_only_owner_can_change_operator
        ~loc:__LOC__
        trace

let () =
  register_test ~title:"Test update_operator entrypoint and is_operator view"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (owner, operator) = Context.init2 ~consensus_threshold_size:0 () in
  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) owner amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* is_operator = is_operator_view ~owner ~operator b in
  let* () = Assert.equal_bool ~loc:__LOC__ false is_operator in

  let* update_operator_tx =
    Op.clst_update_operator (B b) ~sender:owner [(owner, operator, `Add)]
  in
  let* b = Block.bake ~operation:update_operator_tx b in
  let* is_operator = is_operator_view ~owner ~operator b in
  let* () = Assert.equal_bool ~loc:__LOC__ true is_operator in

  let* update_operator_tx =
    Op.clst_update_operator (B b) ~sender:owner [(owner, operator, `Remove)]
  in
  let* b = Block.bake ~operation:update_operator_tx b in
  let* is_operator = is_operator_view ~owner ~operator b in
  let* () = Assert.equal_bool ~loc:__LOC__ false is_operator in

  let* update_operator_tx =
    Op.clst_update_operator (B b) ~sender:operator [(owner, operator, `Add)]
  in
  let*! b = Block.bake ~operation:update_operator_tx b in

  match b with
  | Ok _ -> Test.fail "Only owner can change the allowance."
  | Error trace ->
      Error_helpers.expect_clst_only_owner_can_change_operator
        ~loc:__LOC__
        trace

let () =
  register_test
    ~title:
      "Test update_operator: remove operator for non-operator has no effect"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (owner, operator) = Context.init2 ~consensus_threshold_size:0 () in
  let* op =
    Op.clst_update_operator (B b) ~sender:owner [(owner, operator, `Remove)]
  in
  let* b, metadata = Block.bake_with_metadata ~operation:op b in
  let events = get_events_from_metadata metadata in
  let* () =
    check_number_events ~loc:__LOC__ ~expected:1 "%operator_update" events
  in
  let* is_operator = is_operator_view ~owner ~operator b in
  Assert.equal_bool ~loc:__LOC__ false is_operator

let () =
  register_test ~title:"Test adding operator on finite allowance" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (owner, account) = Context.init2 ~consensus_threshold_size:0 () in
  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) owner amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* approve_tx =
    Op.clst_approve (B b) ~sender:owner [(owner, account, 50_000L)]
  in
  let* b = Block.bake ~operation:approve_tx b in
  let* allowance = get_allowance ~owner ~spender:account b in
  let* () = Assert.equal_int64 ~loc:__LOC__ 50_000L allowance in
  let* is_operator = is_operator_view ~owner ~operator:account b in
  let* () = Assert.equal_bool ~loc:__LOC__ false is_operator in

  (* Adding an operator to a finite allowance replaces the allowance. *)
  let* update_op =
    Op.clst_update_operator (B b) ~sender:owner [(owner, account, `Add)]
  in
  let* b = Block.bake ~operation:update_op b in
  let* is_operator = is_operator_view ~owner ~operator:account b in
  let* () = Assert.equal_bool ~loc:__LOC__ true is_operator in
  let* allowance = get_allowance ~owner ~spender:account b in
  let* () = Assert.equal_int64 ~loc:__LOC__ 0L allowance in
  return_unit

let () =
  register_test ~title:"Test adding finite allowance on operator" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (owner, account) = Context.init2 ~consensus_threshold_size:0 () in
  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) owner amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* update_op =
    Op.clst_update_operator (B b) ~sender:owner [(owner, account, `Add)]
  in
  let* b = Block.bake ~operation:update_op b in
  let* is_operator = is_operator_view ~owner ~operator:account b in
  let* () = Assert.equal_bool ~loc:__LOC__ true is_operator in

  (* Adding a finite allowance on an operator has no effect. *)
  let* approve_tx =
    Op.clst_approve (B b) ~sender:owner [(owner, account, 50_000L)]
  in
  let* b = Block.bake ~operation:approve_tx b in
  let* is_operator = is_operator_view ~owner ~operator:account b in
  let* () = Assert.equal_bool ~loc:__LOC__ true is_operator in
  let* allowance = get_allowance ~owner ~spender:account b in
  let* () = Assert.equal_int64 ~loc:__LOC__ 0L allowance in
  return_unit

let () =
  register_test ~title:"Test balance_of entrypoint" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let* clst_hash = get_clst_hash (B b) in
  let amount = Tez.of_mutez_exn 100000000L in
  let* deposit_tx = Op.clst_deposit (B b) sender amount in
  let* b = Block.bake ~operation:deposit_tx b in
  let* balance =
    let elm =
      Environment.Micheline.(
        Prim
          ( dummy_location,
            Script.D_Pair,
            [
              String (dummy_location, Contract.to_b58check sender);
              Int (dummy_location, Z.zero);
            ],
            [] ))
    in
    run_tzip4_view
      ~contract:clst_hash
      ~entrypoint_name:(Entrypoint.of_string_strict_exn "balance_of")
      ~input:
        Environment.Micheline.(Seq (dummy_location, [elm]) |> strip_locations)
      b
  in
  let balance =
    match balance |> Environment.Micheline.root with
    | Environment.Micheline.Seq
        (_, [Prim (_, Script.D_Pair, [_; Int (_, balance_z)], [])]) ->
        balance_z |> Z.to_int64
    | _ -> Test.fail "Unexpected output"
  in
  let amount = Tez.to_mutez amount in
  let* () = Assert.equal_int64 ~loc:__LOC__ amount balance in
  return_unit

let () =
  register_test ~title:"Test get_token_metadata view" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, _ = Context.init1 () in
  let* clst_hash = get_clst_hash (B b) in
  let* token_metadata_id0 =
    run_view
      ~contract:clst_hash
      ~view_name:"get_token_metadata"
      ~input:
        Environment.Micheline.(Int (dummy_location, Z.zero) |> strip_locations)
      b
  in
  let* () =
    let open Environment.Micheline in
    let open Michelson_v1_primitives in
    let expected_token_metadata =
      let elt key value =
        Prim
          ( dummy_location,
            D_Elt,
            [String (dummy_location, key); Bytes (dummy_location, value)],
            [] )
      in
      [
        elt "decimals" (Bytes.of_string "6");
        elt "name" (Bytes.of_string "Staked Tez");
        elt "symbol" (Bytes.of_string "sTez");
        elt "tokenKind" (Bytes.of_string "fungible");
      ]
    in
    match token_metadata_id0 |> Environment.Micheline.root with
    | Seq (_, token_metadata) ->
        let is_equal =
          List.equal
            (fun a b ->
              match (a, b) with
              | ( Prim
                    ( _,
                      Script.D_Elt,
                      [String (_, key_a); Bytes (_, value_a)],
                      [] ),
                  Prim
                    ( _,
                      Script.D_Elt,
                      [String (_, key_b); Bytes (_, value_b)],
                      [] ) ) ->
                  String.equal key_a key_b && Bytes.equal value_a value_b
              | _ -> false)
            token_metadata
            expected_token_metadata
        in
        Assert.is_true ~loc:__LOC__ is_equal
    | _ -> Test.fail "Unexpected output"
  in
  let* token_metadata_id1 =
    run_view
      ~contract:clst_hash
      ~view_name:"get_token_metadata"
      ~input:
        Environment.Micheline.(Int (dummy_location, Z.one) |> strip_locations)
      b
  in
  let () =
    match token_metadata_id1 |> Environment.Micheline.root with
    | Environment.Micheline.Seq (_, []) -> ()
    | _ -> Test.fail "Unexpected output"
  in
  return_unit

let () =
  register_test ~title:"Test export_ticket entrypoint" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst) = Context.init2 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let ticket_amount_export = 30_000_000L in
  let* op_export_ticket =
    Op.clst_export_ticket
      (B b)
      ~sender:src
      [(dst, [(src, ticket_amount_export)])]
  in
  let* b = Block.bake ~operation:op_export_ticket b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg ticket_amount_export)
      b
      src
  in
  let* stez_ticket_balance =
    Plugin.Contract_services.stez_ticket_balance Block.rpc_ctxt b dst
  in
  let* () =
    Assert.equal_int64
      ~loc:__LOC__
      (Z.to_int64 stez_ticket_balance)
      ticket_amount_export
  in

  let* op_export_ticket_zero =
    Op.clst_export_ticket (B b) ~sender:src [(dst, [(src, 0L)])]
  in
  let*! b = Block.bake ~operation:op_export_ticket_zero b in
  match b with
  | Ok _ -> Test.fail "Empty tickets are forbidden and expected to fail"
  | Error trace -> Error_helpers.expect_clst_empty_ticket ~loc:__LOC__ trace

let () =
  register_test ~title:"Test import_ticket via export_ticket entrypoint"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst) = Context.init2 ~consensus_threshold_size:0 () in
  let* clst_hash = get_clst_hash (B b) in
  let clst_import_ticket_entrypoint =
    Contract.to_b58check (Contract.Originated clst_hash) ^ "%import_ticket"
  in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_op = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_op b in

  let ticket_amount = 30_000_000L in
  let* op =
    Op.clst_export_ticket
      ~destination_contract:(Some clst_import_ticket_entrypoint)
      (B b)
      ~sender:src
      [(dst, [(src, ticket_amount)])]
  in
  let* b = Block.bake ~operation:op b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg ticket_amount)
      b
      src
  in
  let* stez_balance =
    Plugin.Contract_services.stez_balance Block.rpc_ctxt b dst
  in
  let stez_balance =
    Option.value_f
      ~default:(fun () -> assert false)
      (Script_int.to_int64 stez_balance)
  in
  let* () = Assert.equal_int64 ~loc:__LOC__ stez_balance ticket_amount in

  let* stez_ticket_balance =
    Plugin.Contract_services.stez_ticket_balance Block.rpc_ctxt b dst
  in
  Assert.equal_int64 ~loc:__LOC__ (Z.to_int64 stez_ticket_balance) 0L

let () =
  register_test ~title:"Test lambda_export is not implemented" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, _dst) = Context.init2 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_op = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_op b in

  let ticket_amount_export = 30_000_000L in
  let lambda_action = Expr.from_string "{ DROP ; NIL operation }" in
  let* op =
    Op.clst_lambda_export (B b) ~src ~lambda_action ticket_amount_export
  in
  let*! b = Block.bake ~operation:op b in
  match b with
  | Ok _ -> Test.fail "lambda_export is not implemented and expected to fail"
  | Error trace ->
      Error_helpers.expect_clst_standard_error
        ~loc:__LOC__
        ~str:"FA2.1 lambda_export is not implemented"
        trace

let register_delegate_and_bake
    ?(edge_of_clst_staking_over_baking_millionth = 50_000l)
    ?(ratio_of_clst_staking_over_direct_staking_billionth = 200_000_000l) b
    delegate =
  let open Lwt_result_wrap_syntax in
  let*?@ parameters =
    Clst_delegates_parameters_repr.make
      ~edge_of_clst_staking_over_baking_millionth
      ~ratio_of_clst_staking_over_direct_staking_billionth
  in
  let* register_tx =
    Op.clst_register_delegate
      (Context.B b)
      delegate
      ~edge_of_clst_staking_over_baking_millionth:
        (Z.of_int32 parameters.edge_of_clst_staking_over_baking_millionth)
      ~ratio_of_clst_staking_over_direct_staking_billionth:
        (Z.of_int32
           parameters.ratio_of_clst_staking_over_direct_staking_billionth)
  in
  let* b = Block.bake b ~operation:register_tx in
  return (b, parameters)

let update_delegate_and_bake
    ?(edge_of_clst_staking_over_baking_millionth = 50_000l)
    ?(ratio_of_clst_staking_over_direct_staking_billionth = 200_000_000l) b
    delegate =
  let open Lwt_result_wrap_syntax in
  let*?@ parameters =
    Clst_delegates_parameters_repr.make
      ~edge_of_clst_staking_over_baking_millionth
      ~ratio_of_clst_staking_over_direct_staking_billionth
  in
  let* register_tx =
    Op.clst_update_delegate_parameters
      (Context.B b)
      delegate
      ~edge_of_clst_staking_over_baking_millionth:
        (Z.of_int32 parameters.edge_of_clst_staking_over_baking_millionth)
      ~ratio_of_clst_staking_over_direct_staking_billionth:
        (Z.of_int32
           parameters.ratio_of_clst_staking_over_direct_staking_billionth)
  in
  let* b = Block.bake b ~operation:register_tx in
  return (b, parameters)

let check_pending_parameters_and_return_next_activation_cycle ~loc b
    delegate_pkh expected_parameters_list =
  let open Lwt_result_syntax in
  let* pending_parameters_list =
    Delegate_services.pending_clst_staking_parameters
      Block.rpc_ctxt
      b
      delegate_pkh
  in
  let rec check_and_return_earliest_cycle expected_parameters_list
      pending_parameters_list first_cycle =
    match (pending_parameters_list, expected_parameters_list) with
    | [], [] -> return first_cycle
    | [], _ :: _ -> Test.fail ~loc:__LOC__ "Expected pending CLST parameters"
    | ( (activation_cycle, pending_parameters) :: pending_parameters_list',
        expected_parameters :: expected_parameters_list' ) ->
        let* () =
          check_expected_pending_parameters
            ~loc
            expected_parameters
            pending_parameters
        in
        let first_cycle =
          match first_cycle with
          | None -> Some activation_cycle
          | Some _ -> first_cycle
        in
        check_and_return_earliest_cycle
          expected_parameters_list'
          pending_parameters_list'
          first_cycle
    | _ :: _, [] ->
        Test.fail
          ~loc:__LOC__
          "There shouldn't be more than one pending parameter"
  in
  check_and_return_earliest_cycle
    expected_parameters_list
    pending_parameters_list
    None

let check_delegate_is_eventually_registered ctxt pkh =
  let open Lwt_result_wrap_syntax in
  let* ctxt = Block.get_alpha_ctxt ctxt in
  let*@ registered =
    Clst_contract_storage.is_delegate_eventually_registered ctxt pkh
  in
  return registered

let () =
  register_test ~title:"Test baker registration" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  (* Ensures that neither storage cost nor issuance will modify the balance,
     making it easier to check. *)
  let* b, delegate =
    Context.init1
      ~consensus_threshold_size:0
      ~cost_per_byte:Tez.zero
      ~issuance_weights:
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        }
      ()
  in
  let delegate_pkh =
    match delegate with
    | Contract.Implicit pkh -> pkh
    | Contract.Originated _ -> assert false
  in
  let* registered =
    Delegate_services.clst_registered Block.rpc_ctxt b delegate_pkh
  in
  let* () = Assert.is_true ~loc:__LOC__ (not registered) in
  let* b, parameters = register_delegate_and_bake b delegate in

  (* Check that the baker is considered registered. *)
  let* eventually_registered =
    check_delegate_is_eventually_registered b delegate_pkh
  in
  let* () = Assert.is_true ~loc:__LOC__ eventually_registered in

  (* Check that the pending parameters are the only parameters registered, and
     bake until the cycle where these parameters will be activated. *)
  let* activation_cycle =
    check_pending_parameters_and_return_next_activation_cycle
      ~loc:__LOC__
      b
      delegate_pkh
      [Clst_delegates_parameters_repr.Update parameters]
  in
  let activation_cycle =
    Option.value_f ~default:(fun _ -> assert false) activation_cycle
  in
  let* b = Block.bake_until_cycle activation_cycle b in

  (* Now, check that the parameters have been activated. *)
  let* active_parameters =
    Delegate_services.active_clst_staking_parameters
      Block.rpc_ctxt
      b
      delegate_pkh
  in
  match active_parameters with
  | None -> Test.fail ~loc:__LOC__ "Delegate has no active parameters"
  | Some active_parameters ->
      check_expected_parameters ~loc:__LOC__ parameters active_parameters

let () =
  register_test ~title:"Test non delegate cannot register" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  (* Ensures that neither storage cost nor issuance will modify the balance,
     making it easier to check. *)
  let* b, delegate =
    Context.init1
      ~consensus_threshold_size:0
      ~cost_per_byte:Tez.zero
      ~issuance_weights:
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        }
      ()
  in
  let account = Account.new_account () in
  let pk = account.Account.pk in
  let account = Contract.Implicit account.Account.pkh in
  let* funding =
    Op.transaction
      (Context.B b)
      delegate
      account
      (Tez.of_mutez_exn 1_000_000_000L)
  in
  let* b = Block.bake ~operation:funding b in
  let* reveal = Op.revelation (Context.B b) pk in
  let* b = Block.bake ~operation:reveal b in
  let*! b_and_parameters = register_delegate_and_bake b account in
  match b_and_parameters with
  | Ok _ ->
      Test.fail ~__LOC__ "A non delegate account shouldn't be able to register"
  | Error e -> Error_helpers.expect_clst_contract_is_not_delegate ~loc:__LOC__ e

let () =
  register_test ~title:"Test baker parameters update" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  (* Ensures that neither storage cost nor issuance will modify the balance,
     making it easier to check. *)
  let* b, delegate =
    Context.init1
      ~consensus_threshold_size:0
      ~cost_per_byte:Tez.zero
      ~issuance_weights:
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        }
      ()
  in
  let delegate_pkh =
    match delegate with
    | Contract.Implicit pkh -> pkh
    | Contract.Originated _ -> assert false
  in
  let* b, parameters = register_delegate_and_bake b delegate in

  (* Check that the pending parameters are the only parameters registered, and
     bake until the cycle where these parameters will be activated. *)
  let* first_params_activation_cycle =
    check_pending_parameters_and_return_next_activation_cycle
      ~loc:__LOC__
      b
      delegate_pkh
      [Clst_delegates_parameters_repr.Update parameters]
  in
  let first_params_activation_cycle_minus_one =
    Option.value_f
      ~default:(fun _ -> assert false)
      (Option.bind first_params_activation_cycle Cycle.pred)
  in
  let* b = Block.bake_until_cycle first_params_activation_cycle_minus_one b in

  (* Now let's register with new parameters, before the previous one are activated *)
  let* b, next_parameters =
    update_delegate_and_bake
      ~edge_of_clst_staking_over_baking_millionth:0l
      b
      delegate
  in
  let* first_params_activation_cycle =
    check_pending_parameters_and_return_next_activation_cycle
      ~loc:__LOC__
      b
      delegate_pkh
      [Update parameters; Update next_parameters]
  in
  let first_params_activation_cycle =
    Option.value_f
      ~default:(fun _ -> assert false)
      first_params_activation_cycle
  in
  let* b = Block.bake_until_cycle first_params_activation_cycle b in

  (* Now, check that the parameters have been activated. *)
  let* active_parameters =
    Delegate_services.active_clst_staking_parameters
      Block.rpc_ctxt
      b
      delegate_pkh
  in
  let* () =
    match active_parameters with
    | None -> Test.fail ~loc:__LOC__ "Delegate has no active parameters"
    | Some active_parameters ->
        check_expected_parameters ~loc:__LOC__ parameters active_parameters
  in

  (* Check the new parameters are still pending. *)
  let* next_params_activation_cycle =
    check_pending_parameters_and_return_next_activation_cycle
      ~loc:__LOC__
      b
      delegate_pkh
      [Update next_parameters]
  in
  let next_params_activation_cycle =
    Option.value_f ~default:(fun _ -> assert false) next_params_activation_cycle
  in
  let* b = Block.bake_until_cycle next_params_activation_cycle b in

  (* Finally, check that the second parameters have been activated. *)
  let* active_parameters =
    Delegate_services.active_clst_staking_parameters
      Block.rpc_ctxt
      b
      delegate_pkh
  in
  match active_parameters with
  | None -> Test.fail ~loc:__LOC__ "Delegate has no active parameters"
  | Some active_parameters ->
      check_expected_parameters ~loc:__LOC__ next_parameters active_parameters

let () =
  register_test ~title:"Test simple transfer" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst) = Context.init2 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let transfer_amount = 30_000_000L in
  let* transfer_tx =
    Op.clst_transfer (B b) ~sender:src [(src, [(dst, transfer_amount)])]
  in
  let* b = Block.bake ~operation:transfer_tx b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg transfer_amount)
      b
      src
  in
  let* () =
    check_clst_balance_diff ~loc:__LOC__ ~init:0L ~diff:transfer_amount b dst
  in

  (* Test a zero transfer *)
  let* transfer_tx = Op.clst_transfer (B b) ~sender:src [(src, [(dst, 0L)])] in
  let* b = Block.bake ~operation:transfer_tx b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Int64.sub (Tez.to_mutez amount) transfer_amount)
      ~diff:0L
      b
      src
  in
  let* () =
    check_clst_balance_diff ~loc:__LOC__ ~init:transfer_amount ~diff:0L b dst
  in
  return_unit

let () =
  register_test ~title:"Test simple transfer with finite allowance" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst, sender) = Context.init3 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* approve_tx =
    Op.clst_approve (B b) ~sender:src [(src, sender, 40_000_000L)]
  in
  let* b = Block.bake ~operation:approve_tx b in

  (* Test a zero transfer with an approved sender *)
  let* transfer_tx = Op.clst_transfer (B b) ~sender [(src, [(dst, 0L)])] in
  let* b = Block.bake ~operation:transfer_tx b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:0L
      b
      src
  in
  let* () = check_clst_balance_diff ~loc:__LOC__ ~init:0L ~diff:0L b dst in

  (* Test a non-zero transfer with an approved sender with sufficient
     allowance *)
  let transfer_amount = 30_000_000L in
  let* transfer_tx =
    Op.clst_transfer (B b) ~sender [(src, [(dst, transfer_amount)])]
  in
  let* b = Block.bake ~operation:transfer_tx b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg transfer_amount)
      b
      src
  in
  let* () =
    check_clst_balance_diff ~loc:__LOC__ ~init:0L ~diff:transfer_amount b dst
  in
  let* allowance = get_allowance ~owner:src ~spender:sender b in
  let* () = Assert.equal_int64 ~loc:__LOC__ 10_000_000L allowance in

  (* Test a non-zero transfer with an approved sender with
     insufficient allowance *)
  let* transfer_tx =
    Op.clst_transfer (B b) ~sender [(src, [(dst, transfer_amount)])]
  in
  let*! b_error = Block.bake ~operation:transfer_tx b in
  let* () =
    match b_error with
    | Ok _ -> Test.fail "Expected to fail due to insufficient allowance"
    | Error trace ->
        Error_helpers.expect_clst_standard_error
          ~loc:__LOC__
          ~str:"FA2.1_INSUFFICIENT_ALLOWANCE"
          trace
  in

  (* Test a non-zero transfer with a non-approved sender *)
  let* transfer_tx =
    Op.clst_transfer (B b) ~sender:dst [(src, [(dst, transfer_amount)])]
  in
  let*! b_error = Block.bake ~operation:transfer_tx b in
  let* () =
    match b_error with
    | Ok _ ->
        Test.fail
          "Expected to fail as a sender has no permission on src's tokens"
    | Error trace ->
        Error_helpers.expect_clst_standard_error
          ~loc:__LOC__
          ~str:"FA2_NOT_OPERATOR"
          trace
  in

  (* Test a zero transfer with a non-approved sender *)
  let* transfer_tx = Op.clst_transfer (B b) ~sender:dst [(src, [(dst, 0L)])] in
  let*! b_error = Block.bake ~operation:transfer_tx b in
  let* () =
    match b_error with
    | Ok _ ->
        Test.fail
          "Expected to fail as a sender has no permission on src's tokens"
    | Error trace ->
        Error_helpers.expect_clst_standard_error
          ~loc:__LOC__
          ~str:"FA2_NOT_OPERATOR"
          trace
  in
  return_unit

let () =
  register_test ~title:"Test simple transfer with infinite allowance"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst, sender) = Context.init3 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* update_operator_tx =
    Op.clst_update_operator (B b) ~sender:src [(src, sender, `Add)]
  in
  let* b = Block.bake ~operation:update_operator_tx b in

  (* Test a zero transfer with an approved operator *)
  let* transfer_tx = Op.clst_transfer (B b) ~sender [(src, [(dst, 0L)])] in
  let* b = Block.bake ~operation:transfer_tx b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:0L
      b
      src
  in
  let* () = check_clst_balance_diff ~loc:__LOC__ ~init:0L ~diff:0L b dst in

  (* Test a non-zero transfer with an approved operator *)
  let transfer_amount = 30_000_000L in
  let* transfer_tx =
    Op.clst_transfer (B b) ~sender [(src, [(dst, transfer_amount)])]
  in
  let* b = Block.bake ~operation:transfer_tx b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg transfer_amount)
      b
      src
  in
  let* () =
    check_clst_balance_diff ~loc:__LOC__ ~init:0L ~diff:transfer_amount b dst
  in

  (* Test a second non-zero transfer with an approved operator *)
  let* transfer_tx =
    Op.clst_transfer (B b) ~sender [(src, [(dst, transfer_amount)])]
  in
  let* b = Block.bake ~operation:transfer_tx b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Int64.sub (Tez.to_mutez amount) transfer_amount)
      ~diff:(Int64.neg transfer_amount)
      b
      src
  in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:transfer_amount
      ~diff:transfer_amount
      b
      dst
  in

  (* Test a non-zero transfer with an approved operator but with
     owner's insufficient balance *)
  let* transfer_tx =
    Op.clst_transfer (B b) ~sender [(src, [(dst, Tez.to_mutez amount)])]
  in
  let*! b_error = Block.bake ~operation:transfer_tx b in
  let* () =
    match b_error with
    | Ok _ -> Test.fail "Expected to fail due to insufficient balance"
    | Error trace ->
        Error_helpers.expect_clst_standard_error
          ~loc:__LOC__
          ~str:"FA2_INSUFFICIENT_BALANCE"
          trace
  in
  return_unit

let () =
  register_test ~title:"Test export_ticket entrypoint with finite allowance"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst, sender) = Context.init3 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* approve_tx =
    Op.clst_approve (B b) ~sender:src [(src, sender, 40_000_000L)]
  in
  let* b = Block.bake ~operation:approve_tx b in

  (* Test a non-zero ticket export with an approved sender with
     sufficient allowance *)
  let ticket_amount_export = 30_000_000L in
  let* op_export_ticket =
    Op.clst_export_ticket (B b) ~sender [(dst, [(src, ticket_amount_export)])]
  in
  let* b = Block.bake ~operation:op_export_ticket b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg ticket_amount_export)
      b
      src
  in
  let* stez_ticket_balance =
    Plugin.Contract_services.stez_ticket_balance Block.rpc_ctxt b dst
  in
  let* () =
    Assert.equal_int64
      ~loc:__LOC__
      (Z.to_int64 stez_ticket_balance)
      ticket_amount_export
  in

  (* Test a non-zero ticket export with an approved sender with
     insufficient allowance *)
  let* op_export_ticket =
    Op.clst_export_ticket (B b) ~sender [(dst, [(src, ticket_amount_export)])]
  in
  let*! b_error = Block.bake ~operation:op_export_ticket b in
  let* () =
    match b_error with
    | Ok _ -> Test.fail "Expected to fail due to insufficient allowance"
    | Error trace ->
        Error_helpers.expect_clst_standard_error
          ~loc:__LOC__
          ~str:"FA2.1_INSUFFICIENT_ALLOWANCE"
          trace
  in

  (* Test a non-zero ticket export with a non-approved sender *)
  let* op_export_ticket =
    Op.clst_export_ticket
      (B b)
      ~sender:dst
      [(dst, [(src, ticket_amount_export)])]
  in
  let*! b_error = Block.bake ~operation:op_export_ticket b in
  let* () =
    match b_error with
    | Ok _ ->
        Test.fail
          "Expected to fail as a sender has no permission on src's tokens"
    | Error trace ->
        Error_helpers.expect_clst_standard_error
          ~loc:__LOC__
          ~str:"FA2_NOT_OPERATOR"
          trace
  in

  (* Test a zero ticket export with an approved sender *)
  let* op_export_ticket_zero =
    Op.clst_export_ticket (B b) ~sender [(dst, [(src, 0L)])]
  in
  let*! b_error = Block.bake ~operation:op_export_ticket_zero b in
  let* () =
    match b_error with
    | Ok _ -> Test.fail "Empty tickets are forbidden and expected to fail"
    | Error trace -> Error_helpers.expect_clst_empty_ticket ~loc:__LOC__ trace
  in

  (* Test a zero ticket export with a non-approved sender *)
  let* op_export_ticket_zero =
    Op.clst_export_ticket (B b) ~sender:dst [(dst, [(src, 0L)])]
  in
  let*! b_error = Block.bake ~operation:op_export_ticket_zero b in
  let* () =
    match b_error with
    | Ok _ ->
        Test.fail
          "Expected to fail as a sender has no permission on src's tokens"
    | Error trace ->
        Error_helpers.expect_clst_standard_error
          ~loc:__LOC__
          ~str:"FA2_NOT_OPERATOR"
          trace
  in
  return_unit

let () =
  register_test ~title:"Test export_ticket entrypoint with infinite allowance"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst, sender) = Context.init3 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* update_operator_tx =
    Op.clst_update_operator (B b) ~sender:src [(src, sender, `Add)]
  in
  let* b = Block.bake ~operation:update_operator_tx b in

  (* Test a non-zero ticket export with an approved operator *)
  let ticket_amount_export = 30_000_000L in
  let* op_export_ticket =
    Op.clst_export_ticket (B b) ~sender [(dst, [(src, ticket_amount_export)])]
  in
  let* b = Block.bake ~operation:op_export_ticket b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg ticket_amount_export)
      b
      src
  in
  let* stez_ticket_balance =
    Plugin.Contract_services.stez_ticket_balance Block.rpc_ctxt b dst
  in
  let* () =
    Assert.equal_int64
      ~loc:__LOC__
      (Z.to_int64 stez_ticket_balance)
      ticket_amount_export
  in

  (* Test a second non-zero ticket export with an approved operator *)
  let* op_export_ticket =
    Op.clst_export_ticket (B b) ~sender [(dst, [(src, ticket_amount_export)])]
  in
  let* b = Block.bake ~operation:op_export_ticket b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Int64.sub (Tez.to_mutez amount) ticket_amount_export)
      ~diff:(Int64.neg ticket_amount_export)
      b
      src
  in
  let* stez_ticket_balance =
    Plugin.Contract_services.stez_ticket_balance Block.rpc_ctxt b dst
  in
  let* () =
    Assert.equal_int64
      ~loc:__LOC__
      (Z.to_int64 stez_ticket_balance)
      (Int64.mul ticket_amount_export 2L)
  in

  (* Test a zero ticket export with an approved operator *)
  let* op_export_ticket_zero =
    Op.clst_export_ticket (B b) ~sender [(dst, [(src, 0L)])]
  in
  let*! b_error = Block.bake ~operation:op_export_ticket_zero b in
  let* () =
    match b_error with
    | Ok _ -> Test.fail "Empty tickets are forbidden and expected to fail"
    | Error trace -> Error_helpers.expect_clst_empty_ticket ~loc:__LOC__ trace
  in
  return_unit

let () =
  register_test ~title:"Test zero allowance" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst, sender) = Context.init3 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* approve_tx = Op.clst_approve (B b) ~sender:src [(src, sender, 0L)] in
  let* b = Block.bake ~operation:approve_tx b in

  (* Test a zero transfer with an approved sender *)
  let* transfer_tx = Op.clst_transfer (B b) ~sender [(src, [(dst, 0L)])] in
  let* b = Block.bake ~operation:transfer_tx b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:0L
      b
      src
  in
  let* () = check_clst_balance_diff ~loc:__LOC__ ~init:0L ~diff:0L b dst in

  (* Test a non-zero transfer with an approved sender with insufficient
     allowance *)
  let transfer_amount = 30_000_000L in
  let* transfer_tx =
    Op.clst_transfer (B b) ~sender [(src, [(dst, transfer_amount)])]
  in
  let*! b_error = Block.bake ~operation:transfer_tx b in
  let* () =
    match b_error with
    | Ok _ -> Test.fail "Expected to fail due to insufficient allowance"
    | Error trace ->
        Error_helpers.expect_clst_standard_error
          ~loc:__LOC__
          ~str:"FA2.1_INSUFFICIENT_ALLOWANCE"
          trace
  in

  (* Test a non-zero ticket export with an approved sender with
     insufficient allowance *)
  let* op_export_ticket =
    Op.clst_export_ticket (B b) ~sender [(dst, [(src, transfer_amount)])]
  in
  let*! b_error = Block.bake ~operation:op_export_ticket b in
  let* () =
    match b_error with
    | Ok _ -> Test.fail "Expected to fail due to insufficient allowance"
    | Error trace ->
        Error_helpers.expect_clst_standard_error
          ~loc:__LOC__
          ~str:"FA2.1_INSUFFICIENT_ALLOWANCE"
          trace
  in

  (* Test a zero ticket export with an approved sender *)
  let* op_export_ticket_zero =
    Op.clst_export_ticket (B b) ~sender [(dst, [(src, 0L)])]
  in
  let*! b_error = Block.bake ~operation:op_export_ticket_zero b in
  let* () =
    match b_error with
    | Ok _ -> Test.fail "Empty tickets are forbidden and expected to fail"
    | Error trace -> Error_helpers.expect_clst_empty_ticket ~loc:__LOC__ trace
  in
  return_unit

let () =
  register_test ~title:"Test import_ticket_from_implicit entrypoint"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst) = Context.init2 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_op = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_op b in

  let ticket_amount_export = 30_000_000L in
  let* op_export_ticket =
    Op.clst_export_ticket
      (B b)
      ~sender:src
      [(dst, [(src, ticket_amount_export)])]
  in
  let* b = Block.bake ~operation:op_export_ticket b in

  let ticket_amount_import = 20_000_000L in
  let* op_import_ticket =
    Op.clst_import_ticket_from_implicit (B b) ~src:dst ticket_amount_import
  in
  let* b = Block.bake ~operation:op_import_ticket b in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg ticket_amount_export)
      b
      src
  in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:0L
      ~diff:ticket_amount_import
      b
      dst
  in
  let* stez_ticket_balance =
    Plugin.Contract_services.stez_ticket_balance Block.rpc_ctxt b dst
  in
  let expected_clst_ticked_balance =
    Int64.sub ticket_amount_export ticket_amount_import
  in
  let* () =
    Assert.equal_int64
      ~loc:__LOC__
      (Z.to_int64 stez_ticket_balance)
      expected_clst_ticked_balance
  in

  (* Insufficient ticket balance *)
  let* op_import_ticket =
    Op.clst_import_ticket_from_implicit
      (B b)
      ~src:dst
      (Int64.add expected_clst_ticked_balance 1L)
  in
  let*! b_error = Block.bake ~operation:op_import_ticket b in
  let* () =
    match b_error with
    | Ok _ -> Test.fail "Expected to fail due to an insufficient ticket balance"
    | Error trace ->
        Error_helpers.expect_negative_ticket_balance ~loc:__LOC__ trace
  in
  return_unit

let () =
  register_test ~title:"Test baker unregistration with active parameters"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  (* Ensures that neither storage cost nor issuance will modify the balance,
     making it easier to check. *)
  let* b, delegate =
    Context.init1
      ~consensus_threshold_size:0
      ~cost_per_byte:Tez.zero
      ~issuance_weights:
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        }
      ()
  in
  let delegate_pkh =
    match delegate with
    | Contract.Implicit pkh -> pkh
    | Contract.Originated _ -> assert false
  in
  let* registered =
    Delegate_services.clst_registered Block.rpc_ctxt b delegate_pkh
  in
  let* () = Assert.is_true ~loc:__LOC__ (not registered) in
  let* b, parameters = register_delegate_and_bake b delegate in

  (* Check that the pending parameters are the only parameters registered, and
     bake until the cycle where these parameters will be activated. *)
  let* activation_cycle =
    check_pending_parameters_and_return_next_activation_cycle
      ~loc:__LOC__
      b
      delegate_pkh
      [Clst_delegates_parameters_repr.Update parameters]
  in
  let activation_cycle =
    Option.value_f ~default:(fun _ -> assert false) activation_cycle
  in
  let* b = Block.bake_until_cycle activation_cycle b in

  (* Parameters should have been activated. *)
  let* active_parameters =
    Delegate_services.active_clst_staking_parameters
      Block.rpc_ctxt
      b
      delegate_pkh
  in
  let* () =
    match active_parameters with
    | None -> Test.fail ~loc:__LOC__ "Delegate has no active parameters"
    | Some _ -> return_unit
  in

  (* Let's send an unregister operation *)
  let* unregister_tx = Op.clst_unregister_delegate (Context.B b) delegate in
  let* b = Block.bake ~operation:unregister_tx b in
  let* unregister_activation_cycle =
    check_pending_parameters_and_return_next_activation_cycle
      ~loc:__LOC__
      b
      delegate_pkh
      [Clst_delegates_parameters_repr.Unregister]
  in
  let unregister_activation_cycle =
    Option.value_f ~default:(fun _ -> assert false) unregister_activation_cycle
  in

  (* Check that the delegate will eventually unregister. *)
  let* is_eventually_registered =
    check_delegate_is_eventually_registered b delegate_pkh
  in
  let* () =
    if is_eventually_registered then
      Test.fail
        ~loc:__LOC__
        "Delegate is expected to be eventually unregistered ????"
    else return_unit
  in
  let* b = Block.bake_until_cycle unregister_activation_cycle b in

  (* Check that the delegate is unregistered. *)
  let* is_registered =
    Delegate_services.clst_registered Block.rpc_ctxt b delegate_pkh
  in
  let* () =
    if is_registered then
      Test.fail ~loc:__LOC__ "Delegate is expected to be unregistered"
    else return_unit
  in
  (* Parameters should be unregistered by now, so it doesn't have parameters anymore. *)
  let* active_parameters =
    Delegate_services.active_clst_staking_parameters
      Block.rpc_ctxt
      b
      delegate_pkh
  in
  match active_parameters with
  | None -> return_unit
  | Some _ -> Test.fail ~loc:__LOC__ "Delegate shouldn't have parameters"

let () =
  register_test
    ~title:"Test baker unregistration with pending parameters at the same cycle"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  (* Ensures that neither storage cost nor issuance will modify the balance,
     making it easier to check. *)
  let* b, delegate =
    Context.init1
      ~consensus_threshold_size:0
      ~cost_per_byte:Tez.zero
      ~issuance_weights:
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        }
      ()
  in
  let delegate_pkh =
    match delegate with
    | Contract.Implicit pkh -> pkh
    | Contract.Originated _ -> assert false
  in
  let* b, _parameters = register_delegate_and_bake b delegate in

  (* Check that the delegate will eventually be registered. *)
  let* is_eventually_registered =
    check_delegate_is_eventually_registered b delegate_pkh
  in
  let* () =
    if not is_eventually_registered then
      Test.fail ~loc:__LOC__ "Delegate is expected to be eventually registered"
    else return_unit
  in

  (* Let's send an unregister operation, this shouldn't fail *)
  let* unregister_tx = Op.clst_unregister_delegate (Context.B b) delegate in
  let* b = Block.bake ~operation:unregister_tx b in
  let* unregister_activation_cycle =
    check_pending_parameters_and_return_next_activation_cycle
      ~loc:__LOC__
      b
      delegate_pkh
      [Clst_delegates_parameters_repr.Unregister]
  in
  let unregister_activation_cycle =
    Option.value_f ~default:(fun _ -> assert false) unregister_activation_cycle
  in
  let* b = Block.bake_until_cycle unregister_activation_cycle b in

  (* Check that the delegate will eventually be unregistered. *)
  let* is_eventually_registered =
    check_delegate_is_eventually_registered b delegate_pkh
  in
  let* () =
    if is_eventually_registered then
      Test.fail
        ~loc:__LOC__
        "Delegate is expected to be eventually unregistered"
    else return_unit
  in

  (* Pending parameters shouln't have been registered, as they've been subsumed
     by the unregistration at the same cycle. *)
  let* active_parameters =
    Delegate_services.active_clst_staking_parameters
      Block.rpc_ctxt
      b
      delegate_pkh
  in
  match active_parameters with
  | None -> return_unit
  | Some _ -> Test.fail ~loc:__LOC__ "Delegate shouldn't have parameters"

let () =
  register_test ~title:"Test baker unregistration when not registered"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  (* Ensures that neither storage cost nor issuance will modify the balance,
     making it easier to check. *)
  let* b, delegate =
    Context.init1
      ~consensus_threshold_size:0
      ~cost_per_byte:Tez.zero
      ~issuance_weights:
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        }
      ()
  in
  (* Let's send an unregister operation, which is expected to fail *)
  let* unregister_tx = Op.clst_unregister_delegate (Context.B b) delegate in
  let*! b = Block.bake ~operation:unregister_tx b in
  match b with
  | Ok _ ->
      Test.fail
        ~loc:__LOC__
        "Operation is invalid, a non registered delegate cannot unregister"
  | Error e -> Error_helpers.expect_clst_unregistered_delegate ~loc:__LOC__ e

let () =
  register_test ~title:"Test event deduplication in transfer" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst1, dst2) = Context.init3 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_tx b in

  (* Test the same dst twice *)
  let* transfer_tx =
    Op.clst_transfer
      (B b)
      ~sender:src
      [(src, [(dst1, 10_000_000L); (dst1, 20_000_000L); (dst2, 5_000_000L)])]
  in
  let* b, metadata = Block.bake_with_metadata ~operation:transfer_tx b in
  let events = get_events_from_metadata metadata in
  let* () =
    check_number_events ~loc:__LOC__ ~expected:3 "%balance_update" events
  in
  let* () =
    check_number_events ~loc:__LOC__ ~expected:3 "%transfer_event" events
  in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg 35_000_000L)
      b
      src
  in
  let* () =
    check_clst_balance_diff ~loc:__LOC__ ~init:0L ~diff:30_000_000L b dst1
  in
  let* () =
    check_clst_balance_diff ~loc:__LOC__ ~init:0L ~diff:5_000_000L b dst2
  in

  (* Test self-transfer *)
  let* transfer_tx =
    Op.clst_transfer (B b) ~sender:src [(src, [(src, 10_000_000L)])]
  in
  let* b, metadata = Block.bake_with_metadata ~operation:transfer_tx b in
  let events = get_events_from_metadata metadata in
  let* () =
    check_number_events ~loc:__LOC__ ~expected:1 "%balance_update" events
  in
  let* () =
    check_number_events ~loc:__LOC__ ~expected:1 "%transfer_event" events
  in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Int64.sub (Tez.to_mutez amount) 35_000_000L)
      ~diff:0L
      b
      src
  in
  return_unit

let () =
  register_test ~title:"Test event deduplication in approve" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (owner, spender1, spender2) =
    Context.init3 ~consensus_threshold_size:0 ()
  in
  (* Test the same spender twice *)
  let* approve_tx =
    Op.clst_approve
      (B b)
      ~sender:owner
      [
        (owner, spender1, 10_000L);
        (owner, spender2, 5_000L);
        (owner, spender1, 20_000L);
      ]
  in
  let* b, metadata = Block.bake_with_metadata ~operation:approve_tx b in
  let events = get_events_from_metadata metadata in
  let* () =
    check_number_events ~loc:__LOC__ ~expected:2 "%allowance_update" events
  in
  let* allowance = get_allowance ~owner ~spender:spender1 b in
  let* () = Assert.equal_int64 ~loc:__LOC__ 30_000L allowance in
  let* allowance = get_allowance ~owner ~spender:spender2 b in
  let* () = Assert.equal_int64 ~loc:__LOC__ 5_000L allowance in
  return_unit

let () =
  register_test ~title:"Test event deduplication in update_operators"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (owner, operator1, operator2) =
    Context.init3 ~consensus_threshold_size:0 ()
  in
  (* Test the same operator twice *)
  let* update_tx =
    Op.clst_update_operator
      (B b)
      ~sender:owner
      [
        (owner, operator1, `Add);
        (owner, operator2, `Add);
        (owner, operator1, `Remove);
      ]
  in
  let* b, metadata = Block.bake_with_metadata ~operation:update_tx b in
  let events = get_events_from_metadata metadata in
  let* () =
    check_number_events ~loc:__LOC__ ~expected:2 "%operator_update" events
  in
  let* is_op1 = is_operator_view ~owner ~operator:operator1 b in
  let* () = Assert.equal_bool ~loc:__LOC__ false is_op1 in
  let* is_op2 = is_operator_view ~owner ~operator:operator2 b in
  let* () = Assert.equal_bool ~loc:__LOC__ true is_op2 in
  return_unit

let () =
  register_test ~title:"Test event deduplication in export_ticket" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst1, dst2) = Context.init3 ~consensus_threshold_size:0 () in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* op_export =
    Op.clst_export_ticket
      (B b)
      ~sender:src
      [
        (dst1, [(src, 10_000_000L)]);
        (dst2, [(src, 5_000_000L)]);
        (dst1, [(src, 20_000_000L)]);
      ]
  in
  let* b, metadata = Block.bake_with_metadata ~operation:op_export b in
  let events = get_events_from_metadata metadata in
  let* () =
    check_number_events ~loc:__LOC__ ~expected:1 "%balance_update" events
  in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg 35_000_000L)
      b
      src
  in
  let* stez_ticket_balance_dst1 =
    Plugin.Contract_services.stez_ticket_balance Block.rpc_ctxt b dst1
  in
  let* () =
    Assert.equal_int64
      ~loc:__LOC__
      (Z.to_int64 stez_ticket_balance_dst1)
      30_000_000L
  in
  let* stez_ticket_balance_dst2 =
    Plugin.Contract_services.stez_ticket_balance Block.rpc_ctxt b dst2
  in
  let* () =
    Assert.equal_int64
      ~loc:__LOC__
      (Z.to_int64 stez_ticket_balance_dst2)
      5_000_000L
  in
  return_unit

let () =
  register_test ~title:"Test event deduplication in import_ticket" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, (src, dst1, dst2) = Context.init3 ~consensus_threshold_size:0 () in
  let* clst_hash = get_clst_hash (B b) in
  let clst_import_ticket_entrypoint =
    Contract.to_b58check (Contract.Originated clst_hash) ^ "%import_ticket"
  in

  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_op = Op.clst_deposit (B b) src amount in
  let* b = Block.bake ~operation:deposit_op b in

  let* op =
    Op.clst_export_ticket
      ~destination_contract:(Some clst_import_ticket_entrypoint)
      (B b)
      ~sender:src
      [
        (dst1, [(src, 10_000_000L)]);
        (dst2, [(src, 5_000_000L); (src, 3_000_000L)]);
        (dst1, [(src, 20_000_000L)]);
      ]
  in
  let* b, metadata = Block.bake_with_metadata ~operation:op b in
  let events = get_events_from_metadata metadata in
  let* () =
    (* export_ticket: 1 update_balance event for src + 3 calls to
       import_ticket *)
    check_number_events ~loc:__LOC__ ~expected:4 "%balance_update" events
  in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      ~init:(Tez.to_mutez amount)
      ~diff:(Int64.neg 38_000_000L)
      b
      src
  in
  let* () =
    check_clst_balance_diff ~loc:__LOC__ ~init:0L ~diff:30_000_000L b dst1
  in
  let* () =
    check_clst_balance_diff ~loc:__LOC__ ~init:0L ~diff:8_000_000L b dst2
  in
  return_unit

(* --- Stake allocation tests --- *)

let pkh_of_contract = function
  | Contract.Implicit pkh -> pkh
  | Contract.Originated _ -> assert false

let clst_allocated_tez ctxt delegate_pkh =
  let open Lwt_result_wrap_syntax in
  let* alpha_ctxt = Context.get_alpha_ctxt ctxt in
  let raw_ctxt = Alpha_context.Internal_for_tests.to_raw alpha_ctxt in
  let*@ staking_balance =
    Stake_storage.get_full_staking_balance raw_ctxt delegate_pkh
  in
  let amount = Full_staking_balance_repr.stez_frozen staking_balance in
  (* Convert Tez_repr.t to Alpha_context.Tez.t via mutez *)
  return (Tez.of_mutez_exn (Tez_repr.to_mutez amount))

let delegate_own_frozen ctxt delegate_pkh =
  let open Lwt_result_wrap_syntax in
  let* alpha_ctxt = Context.get_alpha_ctxt ctxt in
  let raw_ctxt = Alpha_context.Internal_for_tests.to_raw alpha_ctxt in
  let*@ staking_balance =
    Stake_storage.get_full_staking_balance raw_ctxt delegate_pkh
  in
  let amount = Full_staking_balance_repr.own_frozen staking_balance in
  return (Tez.of_mutez_exn (Tez_repr.to_mutez amount))

let setup_delegate_with_clst_deposit
    ?(limit_of_staking_over_baking = Q.of_int 5)
    ?(edge_of_clst_staking_over_baking_millionth = 50_000l)
    ?(ratio_of_clst_staking_over_direct_staking_billionth = 200_000_000l)
    ~deposit_mutez () =
  let open Lwt_result_wrap_syntax in
  let* b, delegate =
    Context.init1
      ~consensus_threshold_size:0
      ~cost_per_byte:Tez.zero
      ~issuance_weights:
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        }
      ()
  in
  let delegate_pkh = pkh_of_contract delegate in
  (* Allow external staking — use Op.transaction directly with
     ~force_reveal:true because bootstrap accounts may be unrevealed. *)
  let limit_of_staking_over_baking_millionth =
    Q.mul limit_of_staking_over_baking (Q.of_int 1_000_000) |> Q.to_bigint
  in
  let edge_of_baking_over_staking_billionth = Z.of_int 1_000_000_000 in
  let* op =
    Op.set_delegate_parameters
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      delegate
      ~limit_of_staking_over_baking_millionth
      ~edge_of_baking_over_staking_billionth
  in
  let* b = Block.bake ~operation:op b in
  (* Wait for staking parameters to activate *)
  let* b =
    Block.bake_until_n_cycle_end
      (Default_parameters.constants_test.delegate_parameters_activation_delay
     + 1)
      b
  in
  (* Register the delegate with CLST *)
  let* b, parameters =
    register_delegate_and_bake
      ~edge_of_clst_staking_over_baking_millionth
      ~ratio_of_clst_staking_over_direct_staking_billionth
      b
      delegate
  in
  (* Fund an account and deposit to CLST *)
  let* sender, b =
    create_funded_account ~funder:delegate ~amount_mutez:deposit_mutez b
  in
  let* deposit_tx =
    Op.clst_deposit
      ~force_reveal:true
      (Context.B b)
      sender
      (Tez.of_mutez_exn deposit_mutez)
  in
  let* b = Block.bake ~operation:deposit_tx b in
  let* activation_cycle =
    check_pending_parameters_and_return_next_activation_cycle
      ~loc:__LOC__
      b
      delegate_pkh
      [Clst_delegates_parameters_repr.Update parameters]
  in
  let activation_cycle =
    Option.value_f ~default:(fun _ -> assert false) activation_cycle
  in
  return (b, delegate, delegate_pkh, activation_cycle)

let () =
  register_test ~title:"Test basic stake allocation" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let deposit_mutez = 100_000_000L in
  let* b, _delegate, delegate_pkh, activation_cycle =
    setup_delegate_with_clst_deposit ~deposit_mutez ()
  in
  (* Bake up to activation cycle: parameters are activated, rebalance will
     happen at the end of this cycle. *)
  let* b = Block.bake_until_cycle activation_cycle b in
  (* Record state BEFORE the first rebalance *)
  let* total_before = total_amount_of_tez (B b) in
  let* () =
    Assert.equal_tez ~loc:__LOC__ (Tez.of_mutez_exn deposit_mutez) total_before
  in
  let* alloc_before = clst_allocated_tez (B b) delegate_pkh in
  let* () = Assert.equal_tez ~loc:__LOC__ Tez.zero alloc_before in
  let* power_before = Context.get_current_baking_power (B b) delegate_pkh in
  (* Bake until activation cycle — triggers first rebalance *)
  let* b = Block.bake_until_cycle_end b in
  (* Check: total_amount_of_tez is preserved (pure accounting) *)
  let* total_after = total_amount_of_tez (B b) in
  let* () = Assert.equal_tez ~loc:__LOC__ total_before total_after in
  (* Check: CLST allocated some tez to the delegate *)
  let* alloc_after = clst_allocated_tez (B b) delegate_pkh in
  let* () = Assert.is_true ~loc:__LOC__ Tez.(alloc_after > zero) in
  (* Check: baking power increased after CLST allocation *)
  let* power_after = Context.get_current_baking_power (B b) delegate_pkh in
  let expected_power_after =
    Int64.add power_before (Tez.to_mutez alloc_after)
  in
  let* () =
    Assert.is_true
      ~loc:__LOC__
      Compare.Int64.(power_after = expected_power_after)
  in
  (* Bake another cycle: recomputation should yield same result *)
  let* b = Block.bake_until_cycle_end b in
  let* total_after_2 = total_amount_of_tez (B b) in
  let* () = Assert.equal_tez ~loc:__LOC__ total_before total_after_2 in
  (* Allocation should be the same (idempotent) *)
  let* alloc_after_2 = clst_allocated_tez (B b) delegate_pkh in
  Assert.equal_tez ~loc:__LOC__ alloc_after alloc_after_2

let () =
  register_test ~title:"Test allocation when limit of direct staking is zero"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let deposit_mutez = 100_000_000L in
  (* limit_of_staking_over_baking = 0 means no direct external staking.
     However, CLST allocation uses the global limit, so the delegate should
     still receive an allocation. *)
  let* b, _delegate, delegate_pkh, activation_cycle =
    setup_delegate_with_clst_deposit
      ~limit_of_staking_over_baking:Q.zero
      ~deposit_mutez
      ()
  in
  let* b = Block.bake_until_cycle activation_cycle b in
  let* total_before = total_amount_of_tez (B b) in
  let* power_before = Context.get_current_baking_power (B b) delegate_pkh in
  (* Bake past activation — rebalance runs using global limit *)
  let* b = Block.bake_until_cycle_end b in
  (* total_amount_of_tez is preserved *)
  let* total_after = total_amount_of_tez (B b) in
  let* () = Assert.equal_tez ~loc:__LOC__ total_before total_after in
  (* CLST allocation is non-zero (global limit allows it) *)
  let* alloc = clst_allocated_tez (B b) delegate_pkh in
  let* () = Assert.is_true ~loc:__LOC__ Tez.(alloc > zero) in
  let* power_after = Context.get_current_baking_power (B b) delegate_pkh in
  let expected_power_after = Int64.add power_before (Tez.to_mutez alloc) in
  Assert.is_true ~loc:__LOC__ Compare.Int64.(power_after = expected_power_after)
