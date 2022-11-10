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

(* Testing
   -------
   Component:    Sapling
   Invocation:   dune exec tezt/tests/main.exe -- --file sapling.ml
   Subject:      Tests for the Sapling client and protocol
*)

open Tezt_tezos

let sapling_contract_path ?(script = "sapling_contract.tz") protocol =
  sf
    "./src/%s/lib_protocol/test/integration/michelson/contracts/%s"
    (Protocol.directory protocol)
    script

module Helpers = struct
  let init protocol contract =
    let* node = Node.init [Node.Synchronisation_threshold 0; Connections 0] in
    let* client = Client.init ~endpoint:(Node node) () in
    let* () = Client.activate_protocol ~protocol client in
    Log.info "Activated protocol." ;
    let* contract_address =
      let prg = sapling_contract_path protocol in
      Client.originate_contract
        ~wait:"none"
        ~init:"{}"
        ~alias:contract
        ~amount:Tez.zero
        ~burn_cap:(Tez.of_int 10)
        ~src:Constant.bootstrap1.alias
        ~prg
        client
    in
    let* () = Client.bake_for_and_wait client in
    Log.info "  - Originated %s." contract ;
    return (client, contract_address)

  let gen_user (client, contract) name =
    let* (_ : string list) =
      Client.sapling_gen_key ~name ~unencrypted:true client
    in
    let* () =
      Client.sapling_use_key ~sapling_key:name ~contract ~memo_size:8 client
    in
    let* address, _index = Client.sapling_gen_address ~name client in
    return address

  let check_balance ~__LOC__ (client, contract) name amount =
    let* balance =
      Client.sapling_get_balance ~sapling_key:name ~contract client
    in
    Check.(
      (balance = amount)
        Tez.typ
        ~error_msg:"Expected sapling balance in contract %R, got %L"
        ~__LOC__) ;
    unit

  let shield ?(expect_failure = false) (client, contract) src dst amount_tez =
    if expect_failure then
      let* () =
        Client.spawn_sapling_shield
          ~qty:amount_tez
          ~src_tz:Account.(src.alias)
          ~dst_sap:dst
          ~sapling_contract:contract
          ~burn_cap:(Tez.of_int 2)
          client
        |> Process.check_error
      in
      return Tez.(zero, zero)
    else
      let* balance_diff =
        Client.sapling_shield
          ~qty:amount_tez
          ~src_tz:Account.(src.alias)
          ~dst_sap:dst
          ~sapling_contract:contract
          ~burn_cap:(Tez.of_int 2)
          client
      in
      let* () = Client.bake_for_and_wait client in
      Log.info "shield" ;
      return balance_diff

  let transfer ?(expect_failure = false) (client, contract) src dst amount_tez =
    let temp_sapling_transaction_file = Temp.file "sapling_transaction" in
    if expect_failure then
      Client.spawn_sapling_forge_transaction
        ~qty:amount_tez
        ~src_sap:src
        ~dst_sap:dst
        ~sapling_contract:contract
        ~file:temp_sapling_transaction_file
        client
      |> Process.check_error
    else
      let* () =
        Client.sapling_forge_transaction
          ~qty:amount_tez
          ~src_sap:src
          ~dst_sap:dst
          ~sapling_contract:contract
          ~file:temp_sapling_transaction_file
          client
      in
      let* () =
        Client.sapling_submit
          ~burn_cap:Tez.one
          ~file:temp_sapling_transaction_file
          ~alias_tz:Constant.bootstrap1.alias
          ~sapling_contract:contract
          client
      in
      let* () = Client.bake_for_and_wait client in
      Log.info "transfer" ;
      unit

  let unshield ?(expect_failure = false) (client, contract) src dst amount_tez =
    if expect_failure then
      let* () =
        Client.spawn_sapling_unshield
          ~burn_cap:Tez.one
          ~qty:amount_tez
          ~src_sap:src
          ~dst_tz:Account.(dst.alias)
          ~sapling_contract:contract
          client
        |> Process.check_error
      in
      return Tez.(zero, zero)
    else
      let* balance_diff =
        Client.sapling_unshield
          ~burn_cap:Tez.one
          ~qty:amount_tez
          ~src_sap:src
          ~dst_tz:Account.(dst.alias)
          ~sapling_contract:contract
          client
      in
      let* () = Client.bake_for_and_wait client in
      Log.info "unshield" ;
      return balance_diff

  let balance_tz1 (client, _contract) pkh =
    let* balance_tez =
      RPC.Client.call client
      @@ RPC.get_chain_block_context_contract_balance ~id:pkh ()
    in
    return balance_tez
end

module Insufficient_funds = struct
  let shield =
    Protocol.register_test
      ~__FILE__
      ~title:"insufficient_funds.shield"
      ~tags:["sapling"]
    @@ fun protocol ->
    let open Helpers in
    let* c = init protocol (sapling_contract_path protocol) in
    let alice_tz1 = Constant.bootstrap2 in
    let* alice_address = gen_user c "alice" in
    let* _ =
      shield
        ~expect_failure:true
        c
        alice_tz1
        alice_address
        (Tez.of_int 1_000_000_000)
    in
    unit

  let transfer =
    Protocol.register_test
      ~__FILE__
      ~title:"insufficient_funds.transfer"
      ~tags:["sapling"]
    @@ fun protocol ->
    let open Helpers in
    let* c = init protocol (sapling_contract_path protocol) in
    let alice_tz1 = Constant.bootstrap2 in
    let* alice_address = gen_user c "alice" in
    let* bob_address = gen_user c "bob" in
    let* _ = shield c alice_tz1 alice_address (Tez.of_int 10) in
    let* () =
      transfer ~expect_failure:true c "alice" bob_address (Tez.of_int 11)
    in
    unit

  let unshield =
    Protocol.register_test
      ~__FILE__
      ~title:"insufficient_funds.unshield"
      ~tags:["sapling"]
    @@ fun protocol ->
    let open Helpers in
    let* c = init protocol (sapling_contract_path protocol) in
    let alice_tz1 = Constant.bootstrap2 in
    let* alice_address = gen_user c "alice" in
    let* bob_address = gen_user c "bob" in
    let* _ = shield c alice_tz1 alice_address (Tez.of_int 10) in
    let* () = transfer c "alice" bob_address (Tez.of_int 10) in
    let* _ = unshield ~expect_failure:true c "bob" alice_tz1 (Tez.of_int 11) in
    unit
end

(* Roundtrip of shield, transfer and unshield. At each step we check the
   balances of the Tezos account that submits the operations, the two Sapling
   accounts and the smart contract. *)
let successful_roundtrip =
  Protocol.register_test
    ~__FILE__
    ~title:"successful_roundtrip"
    ~tags:["sapling"]
  @@ fun protocol ->
  let open Helpers in
  let* c = init protocol (sapling_contract_path protocol) in
  let alice_tz1 = Constant.bootstrap2 in
  let* alice_address = gen_user c "alice" in
  let* bob_address = gen_user c "bob" in
  let* () = check_balance ~__LOC__ c "alice" Tez.zero in
  let* () = check_balance ~__LOC__ c "bob" Tez.zero in
  let shield_amount = Tez.of_int 10 in
  let* balance_alice_tz1_before = balance_tz1 c alice_tz1.public_key_hash in
  let* amount, fees = shield c alice_tz1 alice_address shield_amount in
  let* balance_alice_tz1_after = balance_tz1 c alice_tz1.public_key_hash in
  Check.(
    (amount = shield_amount) Tez.typ ~__LOC__ ~error_msg:"Expected %R, got %L") ;
  Check.(
    (balance_alice_tz1_after
    = Tez.(balance_alice_tz1_before - shield_amount - fees))
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* balance_contract = balance_tz1 c (snd c) in
  Check.(
    (balance_contract = shield_amount)
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* () = check_balance ~__LOC__ c "alice" shield_amount in
  let* () = check_balance ~__LOC__ c "bob" Tez.zero in
  let* () = transfer c "alice" bob_address shield_amount in
  let* balance_contract = balance_tz1 c (snd c) in
  Check.(
    (balance_contract = shield_amount)
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* () = check_balance ~__LOC__ c "alice" Tez.zero in
  let* () = check_balance ~__LOC__ c "bob" shield_amount in
  let* balance_alice_tz1_before = balance_tz1 c alice_tz1.public_key_hash in
  let* amount, fees = unshield c "bob" alice_tz1 shield_amount in
  let* balance_alice_tz1_after = balance_tz1 c alice_tz1.public_key_hash in
  Check.(
    (amount = Tez.(to_mutez shield_amount * -1 |> Tez.of_mutez_int))
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  Check.(
    (balance_alice_tz1_after
    = Tez.(balance_alice_tz1_before + shield_amount - fees))
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* balance_contract = balance_tz1 c (snd c) in
  Check.(
    (balance_contract = Tez.zero)
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* () = check_balance ~__LOC__ c "alice" Tez.zero in
  let* () = check_balance ~__LOC__ c "bob" Tez.zero in
  unit

let register ~protocols =
  Insufficient_funds.shield protocols ;
  Insufficient_funds.transfer protocols ;
  Insufficient_funds.unshield protocols ;
  successful_roundtrip protocols
