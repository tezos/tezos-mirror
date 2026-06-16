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
let clst_tags = [team; "clst"; "stez"]

let test_deposit_and_redeem =
  Protocol.register_test
    ~__FILE__
    ~title:"Test deposit and redeem"
    ~tags:(clst_tags @ ["deposit"; "redeem"])
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
  let* stez_contract_hash =
    Client.RPC.call client @@ RPC.get_chain_block_context_stez_contract_hash ()
  in
  Log.info ~color:Log.Color.FG.green "sTEZ contract hash: %s" stez_contract_hash ;
  let src = Constant.bootstrap2 in

  Log.info ~color:Log.Color.FG.green "Deposit 150000 tez to sTEZ" ;
  let init_amount = Tez.of_int 150_000 in
  let* () =
    Client.stez_deposit ~burn_cap:Tez.one init_amount ~src:src.alias client
  in
  let* () = Client.bake_for_and_wait client in
  let* stez_balance =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_stez_balance
         ~id:src.public_key_hash
         ()
  in
  Check.((JSON.as_int stez_balance = Tez.to_mutez init_amount) ~__LOC__ int)
    ~error_msg:"Expected stez balance %R to be equal to %L" ;
  Log.info
    ~color:Log.Color.FG.green
    "stez balance for %s: %s"
    src.alias
    (JSON.as_string stez_balance) ;

  Log.info ~color:Log.Color.FG.green "Redeem 30000 tez from sTEZ" ;
  let redeemed_amount = Tez.of_int 30_000 in
  let* () =
    Client.stez_redeem ~burn_cap:Tez.one redeemed_amount ~src:src.alias client
  in
  let* () = Client.bake_for_and_wait client in
  let* stez_balance =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_stez_balance
         ~id:src.public_key_hash
         ()
  in
  Check.(
    (JSON.as_int stez_balance
    = Tez.to_mutez init_amount - Tez.to_mutez redeemed_amount)
      ~__LOC__
      int)
    ~error_msg:"Expected stez balance %R to be equal to %L" ;
  Log.info
    ~color:Log.Color.FG.green
    "stez balance for %s: %s"
    src.alias
    (JSON.as_string stez_balance) ;
  unit

let register ~protocols = test_deposit_and_redeem protocols
