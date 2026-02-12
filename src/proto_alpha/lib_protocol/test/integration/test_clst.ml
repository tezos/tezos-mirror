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

let create_funded_account ~funder ~amount_mutez b =
  let open Lwt_result_wrap_syntax in
  let account = Account.new_account () in
  let account = Contract.Implicit account.Account.pkh in
  let* op =
    Op.transaction (B b) funder account (Tez.of_mutez_exn amount_mutez)
  in
  let* b = Block.bake ~operation:op b in
  return (account, b)

let check_clst_balance_diff ~loc initial_balance_mutez diff_mutez b account =
  let open Lwt_result_syntax in
  let* balance =
    Plugin.Contract_services.clst_balance Block.rpc_ctxt b account
  in
  let balance =
    Option.value_f
      ~default:(fun () -> assert false)
      (Script_int.to_int64 balance)
  in
  let expected_balance = Int64.add initial_balance_mutez diff_mutez in
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

let test_deposit =
  register_test ~title:"Test deposit of a non null amount" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let amount = Tez.of_mutez_exn 100000000L in
  let* deposit_tx = Op.clst_deposit (Context.B b) sender amount in
  let* b, full_metadata = Block.bake_with_metadata ~operation:deposit_tx b in
  let* () =
    check_clst_balance_diff ~loc:__LOC__ 0L (Tez.to_mutez amount) b sender
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
    check_clst_balance_diff ~loc:__LOC__ 0L initial_clst_bal_mutez b account
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
      initial_clst_bal_mutez
      (Int64.neg redeemed_amount_mutez)
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
      Plugin.Contract_services.clst_total_amount_of_tez Block.rpc_ctxt b
    in
    let* exchange_rate =
      Plugin.Contract_services.clst_exchange_rate Block.rpc_ctxt b
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
              Prim (dummy_location, Script.D_Unit, [], []) |> strip_locations)
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
        Plugin.Contract_services.clst_total_supply Block.rpc_ctxt b
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
    Plugin.Contract_services.clst_exchange_rate Block.rpc_ctxt b
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
  let* finalize_tx = Op.clst_finalize ~fee:Tez.zero (Context.B b) sender in
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
    Assert.clst_frozen_redeemed_balance_is
      ~loc:__LOC__
      b
      sender
      second_redeemed_amount
  in
  let* () =
    Assert.clst_finalizable_redeemed_balance_is
      ~loc:__LOC__
      b
      sender
      first_redeemed_amount
  in

  (* Finalize, and look at the balance, and ensure it is:
     <amount before finalization> + <first redeemed amount>
  *)
  let* balance_before_finalization = Context.Contract.balance (B b) sender in
  let* finalize_tx = Op.clst_finalize ~fee:Tez.zero (Context.B b) sender in
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
    Assert.clst_frozen_redeemed_balance_is
      ~loc:__LOC__
      b
      sender
      second_redeemed_amount
  in
  Assert.clst_finalizable_redeemed_balance_is ~loc:__LOC__ b sender Tez.zero

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
    Op.clst_approve (B b) ~src:owner ~spender amount_incr_allowance
  in
  let* b = Block.bake ~operation:approve_tx b in
  let* allowance = get_allowance ~owner ~spender b in
  let* () = Assert.equal_int64 ~loc:__LOC__ amount_incr_allowance allowance in

  let amount_decr_allowance = -30_000L in
  let* approve_tx =
    Op.clst_approve (B b) ~src:owner ~spender amount_decr_allowance
  in
  let* b = Block.bake ~operation:approve_tx b in
  let* allowance = get_allowance ~owner ~spender b in
  let* () = Assert.equal_int64 ~loc:__LOC__ 10_000L allowance in

  let* approve_tx =
    Op.clst_approve (B b) ~src:spender ~owner ~spender amount_incr_allowance
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
  let is_operator_view ~owner ~operator b =
    let* clst_hash = get_clst_hash (B b) in
    let* is_operator =
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
    let is_operator =
      match is_operator |> Environment.Micheline.root with
      | Environment.Micheline.Prim (_, Script.D_True, [], []) -> true
      | Environment.Micheline.Prim (_, Script.D_False, [], []) -> false
      | _ -> Test.fail "Unexpected output"
    in
    return is_operator
  in

  let* b, (owner, operator) = Context.init2 ~consensus_threshold_size:0 () in
  let amount = Tez.of_mutez_exn 100_000_000L in
  let* deposit_tx = Op.clst_deposit (B b) owner amount in
  let* b = Block.bake ~operation:deposit_tx b in

  let* is_operator = is_operator_view ~owner ~operator b in
  let* () = Assert.equal_bool ~loc:__LOC__ false is_operator in

  let* update_operator_tx =
    Op.clst_update_operator (B b) ~src:owner ~operator `Add
  in
  let* b = Block.bake ~operation:update_operator_tx b in
  let* is_operator = is_operator_view ~owner ~operator b in
  let* () = Assert.equal_bool ~loc:__LOC__ true is_operator in

  let* update_operator_tx =
    Op.clst_update_operator (B b) ~src:owner ~operator `Remove
  in
  let* b = Block.bake ~operation:update_operator_tx b in
  let* is_operator = is_operator_view ~owner ~operator b in
  let* () = Assert.equal_bool ~loc:__LOC__ false is_operator in

  let* update_operator_tx =
    Op.clst_update_operator (B b) ~src:operator ~owner ~operator `Add
  in
  let*! b = Block.bake ~operation:update_operator_tx b in
  match b with
  | Ok _ -> Test.fail "Only owner can change the allowance."
  | Error trace ->
      Error_helpers.expect_clst_only_owner_can_change_operator
        ~loc:__LOC__
        trace

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
