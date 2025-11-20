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

let get_clst_hash ctxt =
  let open Lwt_result_wrap_syntax in
  let* alpha_ctxt = Context.get_alpha_ctxt ctxt in
  let*@ hash = Contract.get_clst_contract_hash alpha_ctxt in
  return hash

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

let test_deposit =
  register_test ~title:"Test deposit of a non null amount" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let amount = Tez.of_mutez_exn 100000000L in
  let* deposit_tx = Op.clst_deposit (Context.B b) sender amount in
  let* b = Block.bake ~operation:deposit_tx b in
  check_clst_balance_diff ~loc:__LOC__ 0L (Tez.to_mutez amount) b sender

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
  register_test ~title:"Test simple withdraw" @@ fun () ->
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

  let* balance_before = Context.Contract.balance (B b) account in
  let withdrawal_amount_mutez = 30_000_000L in
  let* withdraw_tx =
    Op.clst_withdraw ~fee:Tez.zero (B b) account withdrawal_amount_mutez
  in
  let* b = Block.bake ~operation:withdraw_tx b in
  let* () =
    Assert.balance_was_credited
      ~loc:__LOC__
      (B b)
      account
      balance_before
      (Tez.of_mutez_exn withdrawal_amount_mutez)
  in
  let* () =
    check_clst_balance_diff
      ~loc:__LOC__
      initial_clst_bal_mutez
      (Int64.neg withdrawal_amount_mutez)
      b
      account
  in
  return_unit

let () =
  register_test ~title:"Test overwithdraw" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, funder = Context.init1 ~consensus_threshold_size:0 () in
  let initial_bal_mutez = 300_000_000L in
  let* account, b =
    create_funded_account ~funder ~amount_mutez:initial_bal_mutez b
  in
  let withdrawal_amount_mutez = 30_000_000L in
  let* withdraw_tx =
    Op.clst_withdraw
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      account
      withdrawal_amount_mutez
  in
  let*! b = Block.bake ~operation:withdraw_tx b in
  match b with
  | Ok _ ->
      Test.fail
        "Withdrawing more clst tokens than the contract has is forbidden"
  | Error trace -> Error_helpers.expect_clst_balance_too_low ~loc:__LOC__ trace

let () =
  register_test ~title:"Test zero withdraw" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, funder = Context.init1 ~consensus_threshold_size:0 () in
  let initial_bal_mutez = 300_000_000L in
  let* account, b =
    create_funded_account ~funder ~amount_mutez:initial_bal_mutez b
  in
  let withdrawal_amount_mutez = 0L in
  let* withdraw_tx =
    Op.clst_withdraw
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      account
      withdrawal_amount_mutez
  in
  let*! b = Block.bake ~operation:withdraw_tx b in
  match b with
  | Ok _ -> Test.fail "Withdrawing 0 tez is forbidden"
  | Error trace -> Error_helpers.expect_clst_empty_transfer ~loc:__LOC__ trace

let () =
  register_test ~title:"Test withdraw with non-zero transfer" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, funder = Context.init1 ~consensus_threshold_size:0 () in
  let initial_bal_mutez = 300_000_000L in
  let* account, b =
    create_funded_account ~funder ~amount_mutez:initial_bal_mutez b
  in
  let* clst_hash = get_clst_hash (Context.B b) in
  let withdrawal_amount_mutez = 10L in
  let* withdraw_tx =
    Op.transaction
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      account
      (Contract.Originated clst_hash)
      (Tez.of_mutez_exn 30_000L)
      ~entrypoint:(Entrypoint.of_string_strict_exn "withdraw")
      ~parameters:
        (Alpha_context.Script.lazy_expr
           (Expr.from_string (Int64.to_string withdrawal_amount_mutez)))
  in
  let*! b = Block.bake ~operation:withdraw_tx b in
  match b with
  | Ok _ -> Test.fail "Transferring to withdraw is forbidden"
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
