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

module Helpers = struct
  module Re = struct
    let extract_sapling_address client_output =
      client_output =~* rex ".*?(zet1\\w{65}.*)" |> Option.get

    let extract_sapling_balance client_output =
      client_output
      =~* rex "Total Sapling funds ?(\\d*)ꜩ"
      |> Option.get |> int_of_string

    (* These regexs only work from protocol J, before there is no field
       `storage fees`. *)
    let balance_diff ~dst client_output =
      let fees =
        let re = ".*fees.* \\.* \\+ꜩ?(\\d*[.\\d*]*)" in
        let res = matches client_output (rex re) in
        List.map float_of_string res |> List.fold_left ( +. ) 0.
      in
      let amount_pos =
        let re = dst ^ ".*\\+ꜩ?(\\d*[.\\d*]*)" in
        match matches client_output (rex re) with
        | [s] -> float_of_string s
        | _ -> 0.
      in
      let amount_neg =
        let re = dst ^ ".*\\-ꜩ?(\\d*[.\\d*]*)" in
        match matches client_output (rex re) with
        | [s] -> float_of_string s
        | _ -> 0.
      in
      (amount_pos -. amount_neg, fees)
  end

  let init protocol contract =
    let* node = Node.init [Node.Synchronisation_threshold 0; Connections 0] in
    let* client = Client.init ~endpoint:(Node node) () in
    let* () = Client.activate_protocol ~protocol client in
    Log.info "Activated protocol." ;
    let* contract_address =
      let prg =
        sf
          "./src/%s/lib_protocol/test/integration/michelson/contracts/%s"
          (Protocol.directory protocol)
          contract
      in
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
    let* () =
      Client.spawn_command
        client
        ["sapling"; "gen"; "key"; name; "--unencrypted"]
      |> Process.check
    in
    let* () =
      Client.spawn_command
        client
        [
          "sapling";
          "use";
          "key";
          name;
          "for";
          "contract";
          contract;
          "--memo-size";
          "8";
        ]
      |> Process.check
    in
    let* client_output =
      Client.spawn_command client ["sapling"; "gen"; "address"; name]
      |> Process.check_and_read_stdout
    in
    return @@ Re.extract_sapling_address client_output

  let balance (client, contract) name =
    let* client_output =
      Client.spawn_command
        client
        ["sapling"; "get"; "balance"; "for"; name; "in"; "contract"; contract]
      |> Process.check_and_read_stdout
    in
    return @@ Re.extract_sapling_balance client_output

  let assert_balance client name amount =
    let* balance = balance client name in
    assert (balance = amount) ;
    unit

  let shield ?(expect_failure = false) (client, contract) src dst amount =
    let* client_output =
      Client.spawn_command
        client
        (["--wait"; "none"]
        @ [
            "sapling";
            "shield";
            string_of_int amount;
            "from";
            Account.(src.alias);
            "to";
            dst;
            "using";
            contract;
            "--burn-cap";
            "2";
          ])
      |> Process.check_and_read_stdout ~expect_failure
    in
    if expect_failure then return (0., 0.)
    else
      let* () = Client.bake_for_and_wait client in
      Log.info "shield" ;
      return @@ Re.balance_diff ~dst:contract client_output

  let transfer ?(expect_failure = false) (client, contract) src dst amount =
    let* () =
      Client.spawn_command
        client
        [
          "sapling";
          "forge";
          "transaction";
          string_of_int amount;
          "from";
          src;
          "to";
          dst;
          "using";
          contract;
        ]
      |> Process.check ~expect_failure
    in
    if expect_failure then unit
    else
      let* () =
        Client.spawn_command
          client
          (["--wait"; "none"]
          @ [
              "sapling";
              "submit";
              "sapling_transaction";
              "from";
              Constant.bootstrap1.alias;
              "using";
              contract;
              "--burn-cap";
              "1";
            ])
        |> Process.check
      in
      let* () = Client.bake_for_and_wait client in
      Log.info "transfer" ;
      unit

  let unshield ?(expect_failure = false) (client, contract) src dst amount =
    let* client_output =
      Client.spawn_command
        client
        (["--wait"; "none"]
        @ [
            "sapling";
            "unshield";
            string_of_int amount;
            "from";
            src;
            "to";
            Account.(dst.alias);
            "using";
            contract;
            "--burn-cap";
            "1";
          ])
      |> Process.check_and_read_stdout ~expect_failure
    in
    if expect_failure then return (0., 0.)
    else
      let* () = Client.bake_for_and_wait client in
      Log.info "unshield" ;
      return @@ Re.balance_diff ~dst:contract client_output

  let balance_tz1 (client, _contract) pkh =
    let*! bal = RPC.Contracts.get_balance ~contract_id:pkh client in
    let f = JSON.as_int bal |> float_of_int in
    return (f /. 1_000_000.)
end

let contract = "sapling_contract.tz"

module Insufficient_funds = struct
  let shield =
    Protocol.register_test
      ~__FILE__
      ~title:"insufficient_funds.shield"
      ~tags:["sapling"]
    @@ fun protocol ->
    let open Helpers in
    let* c = init protocol contract in
    let alice_tz1 = Constant.bootstrap2 in
    let* alice_address = gen_user c "alice" in
    let* _ =
      shield ~expect_failure:true c alice_tz1 alice_address 1_000_000_000
    in
    unit

  let transfer =
    Protocol.register_test
      ~__FILE__
      ~title:"insufficient_funds.transfer"
      ~tags:["sapling"]
    @@ fun protocol ->
    let open Helpers in
    let* c = init protocol contract in
    let alice_tz1 = Constant.bootstrap2 in
    let* alice_address = gen_user c "alice" in
    let* bob_address = gen_user c "bob" in
    let* _ = shield c alice_tz1 alice_address 10 in
    let* () = transfer ~expect_failure:true c "alice" bob_address 11 in
    unit

  let unshield =
    Protocol.register_test
      ~__FILE__
      ~title:"insufficient_funds.unshield"
      ~tags:["sapling"]
    @@ fun protocol ->
    let open Helpers in
    let* c = init protocol contract in
    let alice_tz1 = Constant.bootstrap2 in
    let* alice_address = gen_user c "alice" in
    let* bob_address = gen_user c "bob" in
    let* _ = shield c alice_tz1 alice_address 10 in
    let* () = transfer c "alice" bob_address 10 in
    let* _ = unshield ~expect_failure:true c "bob" alice_tz1 11 in
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
  let* c = init protocol contract in
  let alice_tz1 = Constant.bootstrap2 in
  let* alice_address = gen_user c "alice" in
  let* bob_address = gen_user c "bob" in
  let* () = assert_balance c "alice" 0 in
  let* () = assert_balance c "bob" 0 in
  let* balance_alice_tz1_before = balance_tz1 c alice_tz1.public_key_hash in
  let* (amount, fees) = shield c alice_tz1 alice_address 10 in
  let* balance_alice_tz1_after = balance_tz1 c alice_tz1.public_key_hash in
  assert (amount = 10.) ;
  assert (balance_alice_tz1_after = balance_alice_tz1_before -. 10. -. fees) ;
  let* balance_contract = balance_tz1 c (snd c) in
  assert (balance_contract = 10.) ;
  let* () = assert_balance c "alice" 10 in
  let* () = assert_balance c "bob" 0 in
  let* () = transfer c "alice" bob_address 10 in
  let* balance_contract = balance_tz1 c (snd c) in
  assert (balance_contract = 10.) ;
  let* () = assert_balance c "alice" 0 in
  let* () = assert_balance c "bob" 10 in
  let* balance_alice_tz1_before = balance_tz1 c alice_tz1.public_key_hash in
  let* (amount, fees) = unshield c "bob" alice_tz1 10 in
  let* balance_alice_tz1_after = balance_tz1 c alice_tz1.public_key_hash in
  assert (amount = -10.) ;
  assert (balance_alice_tz1_after = balance_alice_tz1_before +. 10. -. fees) ;
  let* balance_contract = balance_tz1 c (snd c) in
  assert (balance_contract = 0.) ;
  let* () = assert_balance c "alice" 0 in
  let* () = assert_balance c "bob" 0 in
  unit

let register ~protocols =
  Insufficient_funds.shield protocols ;
  Insufficient_funds.transfer protocols ;
  Insufficient_funds.unshield protocols ;
  successful_roundtrip protocols
