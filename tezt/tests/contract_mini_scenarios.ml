(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_mini_scenarios.ml
   Subject:      Test mini scenarios
*)

let test_replay client ~protocol =
  Log.info "Replay 'originate'" ;
  let* _alias, contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "replay"]
      protocol
  in
  Log.info "Replay transfer fail" ;
  let* () =
    Client.spawn_transfer
      ~amount:(Tez.of_int 10)
      ~giver:"bootstrap1"
      ~receiver:contract
      ~arg:"Unit"
      client
    |> Process.check_error ~msg:(rex "Internal operation replay attempt")
  in
  unit

let check_balance ~__LOC__ ~account client ~amount =
  let* contract_balance = Client.get_balance_for ~account client in
  Check.(contract_balance = Tez.of_int amount)
    Tez.typ
    ~__LOC__
    ~error_msg:"Expected balance %R, got %L" ;
  unit

let test_create_contract_balance client ~contract =
  check_balance ~__LOC__ ~account:contract ~amount:1000 client

let check_contract_storage ~__LOC__ client contract ~expected =
  let* storage = Client.contract_storage contract client in
  Check.(
    (String.trim storage = expected)
      string
      ~__LOC__
      ~error_msg:"Expected storage %R, got %L") ;
  unit

let extract_new_contract client_output =
  match client_output =~* rex "New contract ?(KT1\\w{33})" with
  | None ->
      Test.fail
        "Cannot extract new contract from client_output: %s"
        client_output
  | Some c -> return c

let test_create_contract client ~protocol =
  Log.info "Create contract 'originate'" ;
  let* _alias, create_contract =
    Client.originate_contract_at
      ~amount:(Tez.of_int 1000)
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "create_contract"]
      protocol
  in
  let* () = test_create_contract_balance client ~contract:create_contract in
  Log.info "Test create contract perform creation" ;
  let* () =
    let process =
      Client.spawn_transfer
        ~burn_cap:(Tez.of_int 10)
        ~amount:Tez.zero
        ~giver:"bootstrap1"
        ~receiver:create_contract
        ~arg:"None"
        client
    in
    let* client_output = Process.check_and_read_stdout process in
    let* kt_1 = extract_new_contract client_output in
    let* () =
      check_contract_storage ~__LOC__ client kt_1 ~expected:"\"abcdefg\""
    in
    let* () = check_balance ~__LOC__ ~account:kt_1 client ~amount:100 in
    let* () =
      check_balance ~__LOC__ ~account:create_contract client ~amount:900
    in
    unit
  in
  unit

let test_default_account client ~protocol =
  Log.info "Default account 'originate'" ;
  let* _alias, default_account =
    Client.originate_contract_at
      ~amount:(Tez.of_int 1000)
      ~src:"bootstrap1"
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "default_account"]
      protocol
  in
  Log.info "Test default account transfer" ;
  let* () =
    let tz1 = Constant.bootstrap4.Account.public_key_hash in
    let arg = sf "\"%s\"" tz1 in
    let* () =
      Client.transfer
        ~burn_cap:(Tez.of_int 10)
        ~amount:Tez.zero
        ~giver:"bootstrap1"
        ~receiver:default_account
        ~arg
        client
    in
    let account = "tz1SuakBpFdG9b4twyfrSMqZzruxhpMeSrE5" in
    let arg = sf "\"%s\"" account in
    let* () =
      Client.transfer
        ~burn_cap:(Tez.of_int 10)
        ~amount:Tez.zero
        ~giver:"bootstrap1"
        ~receiver:default_account
        ~arg
        client
    in
    unit
  in
  unit

let test_preimage_and_signature client ~protocol =
  Log.info "Reveal signed preimage 'originate'" ;
  let* _alias, reveal_signed_preimage =
    let byt =
      "0x9995c2ef7bcc7ae3bd15bdd9b02dc6e877c27b26732340d641a4cbc6524813bb"
    in
    let sign = "p2pk66uq221795tFxT7jfNmXtBMdjMf6RAaxRTwv1dbuSHbH6yfqGwz" in
    let init = sf "Pair %s \"%s\"" byt sign in
    Client.originate_contract_at
      ~amount:(Tez.of_int 1000)
      ~src:"bootstrap1"
      ~init
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "reveal_signed_preimage"]
      protocol
  in
  Log.info "Test wrong preimage" ;
  let* () =
    let byte =
      "0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972"
    in
    let sign =
      "p2sigvgDSBnN1bUsfwyMvqpJA1cFhE5s5oi7SetJVQ6LJsbFrU2idPvnvwJhf5v9DhM9ZTX1euS9DgWozVw6BTHiK9VcQVpAU8"
    in
    let arg = sf "Pair %s \"%s\"" byte sign in
    Client.spawn_transfer
      ~amount:Tez.zero
      ~giver:"bootstrap1"
      ~receiver:reveal_signed_preimage
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
    |> Process.check_error ~msg:(rex "At line 8 characters 9 to 21")
  in
  Log.info "Test wrong signature" ;
  let* () =
    let byte =
      "0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972203f"
    in
    let sign =
      "p2sigvgDSBnN1bUsfwyMvqpJA1cFhE5s5oi7SetJVQ6LJsbFrU2idPvnvwJhf5v9DhM9ZTX1euS9DgWozVw6BTHiK9VcQVpAU8"
    in
    let arg = sf "Pair %s \"%s\"" byte sign in
    Client.spawn_transfer
      ~amount:Tez.zero
      ~giver:"bootstrap1"
      ~receiver:reveal_signed_preimage
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
    |> Process.check_error ~msg:(rex "At line 15 characters 9 to 15")
  in
  Log.info "Test good preimage and signature" ;
  let* () =
    let byte =
      "0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972203f"
    in
    let sign =
      "p2sigsceCzcDw2AeYDzUonj4JT341WC9Px4wdhHBxbZcG1FhfqFVuG7f2fGCzrEHSAZgrsrQWpxduDPk9qZRgrpzwJnSHC3gZJ"
    in
    let arg = sf "Pair %s \"%s\"" byte sign in
    let* () =
      Client.transfer
        ~amount:Tez.zero
        ~giver:"bootstrap1"
        ~receiver:reveal_signed_preimage
        ~arg
        ~burn_cap:(Tez.of_int 10)
        client
    in
    unit
  in
  unit

(* Test vote_for_delegate *)
let test_vote_for_delegate client ~protocol =
  Log.info "vote for delegate 'originate" ;
  let* _alias, vote_for_delegate =
    let b_3 = Constant.bootstrap3.Account.public_key_hash in
    let b_4 = Constant.bootstrap4.Account.public_key_hash in
    let init = sf {|(Pair (Pair "%s" None) (Pair "%s" None))|} b_3 b_4 in
    Client.originate_contract_at
      ~amount:(Tez.of_int 1000)
      ~src:"bootstrap1"
      ~init
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "vote_for_delegate"]
      protocol
  in
  let* delegate_opt = Client.get_delegate ~src:vote_for_delegate client in
  let () =
    Check.((delegate_opt = None) (option string))
      ~error_msg:"expected delegate %R, got %L"
  in
  Log.info "Test vote for delegate wrong identity1" ;
  let* () =
    Client.spawn_transfer
      ~amount:Tez.zero
      ~giver:"bootstrap1"
      ~receiver:vote_for_delegate
      ~arg:"None"
      ~burn_cap:(Tez.of_int 10)
      client
    |> Process.check_error ~msg:(rex "At line 14 characters 9 to 12")
  in
  Log.info "Test vote for delegate wrong identity2" ;
  let* () =
    Client.spawn_transfer
      ~amount:Tez.zero
      ~giver:"bootstrap2"
      ~receiver:vote_for_delegate
      ~arg:"None"
      ~burn_cap:(Tez.of_int 10)
      client
    |> Process.check_error ~msg:(rex "At line 14 characters 9 to 12")
  in
  Log.info "Test vote for delegate b3 vote for b5" ;
  let* () =
    let b_5 = Constant.bootstrap5.Account.public_key_hash in
    let arg = sf {|(Some "%s")|} b_5 in
    let* () =
      Client.transfer
        ~amount:Tez.zero
        ~giver:"bootstrap3"
        ~receiver:vote_for_delegate
        ~arg
        ~burn_cap:(Tez.of_int 10)
        client
    in
    let b_3 = Constant.bootstrap3.Account.public_key_hash in
    let b_4 = Constant.bootstrap4.Account.public_key_hash in
    let expected =
      sf
        "Pair (Pair \"%s\"\n           (Some \"%s\"))\n     \"%s\"\n     None"
        b_3
        b_5
        b_4
    in
    let* () =
      check_contract_storage ~__LOC__ client vote_for_delegate ~expected
    in
    unit
  in
  Log.info "Test vote for delegate still no delegate1" ;
  let* () =
    let* delegate_opt = Client.get_delegate ~src:vote_for_delegate client in
    let () =
      Check.((delegate_opt = None) (option string))
        ~error_msg:"expected delegate %R, got %L"
    in
    unit
  in
  Log.info "Test vote for delegate b4 vote for b2" ;
  let* () =
    let b_2 = Constant.bootstrap2.Account.public_key_hash in
    let arg = sf {|(Some "%s")|} b_2 in
    let* () =
      Client.transfer
        ~amount:Tez.zero
        ~giver:"bootstrap4"
        ~receiver:vote_for_delegate
        ~arg
        ~burn_cap:(Tez.of_int 10)
        client
    in
    let b_3 = Constant.bootstrap3.Account.public_key_hash in
    let b_4 = Constant.bootstrap4.Account.public_key_hash in
    let b_5 = Constant.bootstrap5.Account.public_key_hash in
    let expected =
      sf
        "Pair (Pair \"%s\"\n\
        \           (Some \"%s\"))\n\
        \     \"%s\"\n\
        \     (Some \"%s\")"
        b_3
        b_5
        b_4
        b_2
    in
    let* () =
      check_contract_storage ~__LOC__ client vote_for_delegate ~expected
    in
    unit
  in
  Log.info "Test vote for delegate still no delegate2" ;
  let* () =
    let* delegate_opt = Client.get_delegate ~src:vote_for_delegate client in
    let () =
      Check.((delegate_opt = None) (option string))
        ~error_msg:"expected delegate %R, got %L"
    in
    unit
  in
  Log.info "Test vote for delegate b4 vote for b5" ;
  let* () =
    let b_5 = Constant.bootstrap5.Account.public_key_hash in
    let arg = sf {|(Some "%s")|} b_5 in
    let* () =
      Client.transfer
        ~amount:Tez.zero
        ~giver:"bootstrap4"
        ~receiver:vote_for_delegate
        ~arg
        ~burn_cap:(Tez.of_int 10)
        client
    in
    let b_3 = Constant.bootstrap3.Account.public_key_hash in
    let b_4 = Constant.bootstrap4.Account.public_key_hash in
    let expected =
      sf
        "Pair (Pair \"%s\"\n\
        \           (Some \"%s\"))\n\
        \     \"%s\"\n\
        \     (Some \"%s\")"
        b_3
        b_5
        b_4
        b_5
    in
    let* () =
      check_contract_storage ~__LOC__ client vote_for_delegate ~expected
    in
    unit
  in
  Log.info "Test vote for delegate has delegate" ;
  let* () =
    let b_5 = Constant.bootstrap5.Account.public_key_hash in
    let* delegate_opt = Client.get_delegate ~src:vote_for_delegate client in
    let () =
      Check.((delegate_opt = Some b_5) (option string))
        ~error_msg:"expected delegate %R, got %L"
    in
    unit
  in
  unit

let test_multiple_entrypoints_counter client ~protocol =
  let* _alias, contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap5"
      ~init:"None"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "multiple_entrypoints_counter"]
      protocol
  in
  (* call contract: creates the internal contract and calls it. *)
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:"bootstrap1"
      ~receiver:contract
      ~burn_cap:(Tez.of_int 10)
      client
  in
  let* () = check_contract_storage ~__LOC__ client contract ~expected:"None" in
  unit

(* Test that the [noop.tz] contract can be originated, typechecks and can
   receive a transaction. *)
let test_contract_noop client ~protocol =
  let burn_cap = Tez.one in
  let prg = Michelson_script.(find ["opcodes"; "noop"] protocol |> path) in
  let* () = Client.remember_script client ~alias:"noop" ~src:("file:" ^ prg) in
  let* () = Client.typecheck_script client ~scripts:[prg] in
  let* _contract =
    Client.originate_contract
      ~alias:"noop"
      ~amount:(Tez.of_int 1000)
      ~src:"bootstrap1"
      ~prg
      ~init:"Unit"
      ~burn_cap
      client
  in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 10)
      ~giver:"bootstrap1"
      ~receiver:"noop"
      ~burn_cap
      ~arg:"Unit"
      client
  in
  unit

(* Test that [hardlimit.tz] initialized with [3] fails after the
   third transfer *)
let test_contract_hardlimit client ~protocol =
  let burn_cap = Tez.one in
  let* hardlimit, _address =
    Client.originate_contract_at
      ~amount:(Tez.of_int 1000)
      ~src:"bootstrap1"
      ~init:"3"
      ~burn_cap
      client
      ["mini_scenarios"; "hardlimit"]
      protocol
  in
  let spawn_transfer () =
    Client.spawn_transfer
      ~amount:(Tez.of_int 10)
      ~giver:"bootstrap1"
      ~receiver:hardlimit
      ~burn_cap
      ~arg:"Unit"
      client
  in
  let* () = spawn_transfer () |> Process.check in
  let* () = spawn_transfer () |> Process.check in
  let* () = spawn_transfer () |> Process.check in
  let* () =
    spawn_transfer ()
    |> Process.check_error ~msg:(rex "script reached FAILWITH instruction")
  in
  unit

let register ~protocols =
  List.iter
    (fun (title, test_function) ->
      Protocol.register_test
        ~__FILE__
        ~title
        ~tags:["client"; "michelson"; "mini_scenarios"]
        (fun protocol ->
          let* client = Client.init_mockup ~protocol () in
          test_function client ~protocol)
        protocols)
    [
      ("test create contract balance", test_create_contract);
      ("test replay", test_replay);
      ("test default account originate and transfer", test_default_account);
      ("test preimage and signature", test_preimage_and_signature);
      ("test vote for delegate", test_vote_for_delegate);
      ("test multiple entrypoints counter", test_multiple_entrypoints_counter);
      ("test contract noop", test_contract_noop);
      ("test contract hardlimit", test_contract_hardlimit);
    ]
