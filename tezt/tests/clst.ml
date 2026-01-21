(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
   -------
   Component:    Canonical Liquid Staking
   Invocation:   dune exec tezt/tests/main.exe -- --file clst.ml
   Subject:      Test CLST contract
*)

let team = Tag.layer1

(** Tags shared by all tests in this file. *)
let clst_tags = [team; "clst"]

let test_deposit_and_withdraw =
  Protocol.register_test
    ~__FILE__
    ~title:"Test deposit and withdraw"
    ~tags:(clst_tags @ ["deposit"; "withdraw"])
    ~supports:(Protocol.From_protocol 25)
  @@ fun protocol ->
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      [(["native_contracts_enable"], `Bool true)]
  in
  let* _node, client =
    Client.init_with_protocol ~parameter_file ~protocol `Client ()
  in
  let* clst_contract_hash =
    Client.RPC.call client @@ RPC.get_chain_block_context_clst_contract_hash ()
  in
  Log.info ~color:Log.Color.FG.green "CLST contract hash: %s" clst_contract_hash ;
  let src = Constant.bootstrap2 in

  Log.info ~color:Log.Color.FG.green "Deposit 150000 tez to CLST" ;
  let init_amount = Tez.of_int 150_000 in
  let* () =
    Client.clst_deposit ~burn_cap:Tez.one init_amount ~src:src.alias client
  in
  let* () = Client.bake_for_and_wait client in
  let* clst_balance =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_clst_balance
         ~id:src.public_key_hash
         ()
  in
  Check.((JSON.as_int clst_balance = Tez.to_mutez init_amount) ~__LOC__ int)
    ~error_msg:"Expected clst balance %R to be equal to %L" ;
  Log.info
    ~color:Log.Color.FG.green
    "clst balance for %s: %s"
    src.alias
    (JSON.as_string clst_balance) ;

  Log.info ~color:Log.Color.FG.green "Withdraw 30000 tez from CLST" ;
  let withdrawal_amount = Tez.of_int 30_000 in
  let* () =
    Client.clst_withdraw
      ~burn_cap:Tez.one
      withdrawal_amount
      ~src:src.alias
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* clst_balance =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_clst_balance
         ~id:src.public_key_hash
         ()
  in
  Check.(
    (JSON.as_int clst_balance
    = Tez.to_mutez init_amount - Tez.to_mutez withdrawal_amount)
      ~__LOC__
      int)
    ~error_msg:"Expected clst balance %R to be equal to %L" ;
  Log.info
    ~color:Log.Color.FG.green
    "clst balance for %s: %s"
    src.alias
    (JSON.as_string clst_balance) ;
  unit

let register ~protocols = test_deposit_and_withdraw protocols
