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

let get_clst_hash ctxt =
  let open Lwt_result_wrap_syntax in
  let* alpha_ctxt = Context.get_alpha_ctxt ctxt in
  let*@ hash = Contract.get_clst_contract_hash alpha_ctxt in
  return hash

let test_deposit =
  register_test ~title:"Test deposit of a non null amount" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let amount = Tez.of_mutez_exn 100000000L in
  let* deposit_tx = Op.clst_deposit (Context.B b) sender amount in
  let* b = Block.bake ~operation:deposit_tx b in
  let* balance =
    Plugin.Contract_services.clst_balance Block.rpc_ctxt b sender
  in
  let amount = Tez.to_mutez amount in
  let balance =
    Option.value_f
      ~default:(fun () -> assert false)
      (Script_int.to_int64 balance)
  in
  Check.((amount = balance) int64 ~error_msg:"Expected %L, got %R") ;
  return_unit

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
  | Error trace -> Error_helpers.expect_clst_empty_deposit ~loc:__LOC__ trace

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
