(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Client commands
   Invocation:   dune exec tezt/tests/main.exe -- --file client_commands.ml
   Subject:      Tests for the Tezos client
*)

module Helpers = struct
  let originate_fail_on_false protocol client =
    let* _alias, contract =
      Client.originate_contract_at
        ~wait:"none"
        ~init:"Unit"
        ~amount:Tez.zero
        ~burn_cap:Tez.one
        ~src:Constant.bootstrap1.alias
        client
        ["mini_scenarios"; "fail_on_false"]
        protocol
    in
    let* () = Client.bake_for_and_wait client in
    return contract

  let get_balance pkh client =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_contract_balance ~id:pkh ()

  let supported_signature_schemes = function
    | Protocol.Alpha | Nairobi | Mumbai ->
        ["ed25519"; "secp256k1"; "p256"; "bls"]

  let airdrop_and_reveal client accounts =
    Log.info "Airdrop 1000tz to each account" ;
    let batches =
      Ezjsonm.list
        (fun account ->
          `O
            [
              ("destination", `String account.Account.public_key_hash);
              ("amount", `String "1000");
            ])
        accounts
    in
    let*! () =
      Client.multiple_transfers
        ~giver:Constant.bootstrap1.public_key_hash
        ~json_batch:(Ezjsonm.to_string batches)
        ~burn_cap:Tez.one
        client
    in
    let* () = Client.bake_for_and_wait client in
    let* balances =
      Lwt_list.map_p
        (fun account ->
          let* balance = get_balance account.Account.public_key_hash client in
          return (account.alias, balance))
        accounts
    in
    List.iter
      (fun (alias, balance) ->
        Check.((Tez.to_string balance = "1000") string)
          ~error_msg:(sf "%s has balance %%L instead of %%R" alias))
      balances ;
    Log.info "Revealing public keys" ;
    let* () =
      Lwt_list.iter_p
        (fun account ->
          let*! () = Client.reveal ~src:account.Account.alias client in
          unit)
        accounts
    in
    Client.bake_for_and_wait client

  let gen_accounts protocol client index =
    Lwt_list.map_s
      (fun sig_alg ->
        Client.gen_and_show_keys
          ~alias:(sf "account_%s_%d" sig_alg index)
          ~sig_alg
          client)
      (supported_signature_schemes protocol)
end

module Simulation = struct
  let transfer ~arg ?simulation ?force k protocol =
    let* _node, client = Client.init_with_protocol `Client ~protocol () in
    let* contract = Helpers.originate_fail_on_false protocol client in
    Client.spawn_transfer
      ~amount:(Tez.of_int 2)
      ~giver:Constant.bootstrap1.public_key_hash
      ~receiver:contract
      ~arg
      ?simulation
      ?force
      client
    |> k

  let multiple_transfers ~args ?simulation ?force k protocol =
    let* _node, client = Client.init_with_protocol `Client ~protocol () in
    let* contract = Helpers.originate_fail_on_false protocol client in
    let batches =
      Ezjsonm.list
        (fun arg ->
          `O
            [
              ("destination", `String contract);
              ("amount", `String "2");
              ("arg", `String arg);
            ])
        args
    in
    let file = Temp.file "batch.json" in
    let oc = open_out file in
    Ezjsonm.to_channel oc batches ;
    close_out oc ;
    Client.multiple_transfers
      ~giver:Constant.bootstrap1.public_key_hash
      ~json_batch:file
      ?simulation
      ?force
      client
    |> k

  let successful =
    Protocol.register_test
      ~__FILE__
      ~title:"Simulation of successful operation"
      ~tags:["client"; "simulation"; "success"]
    @@ transfer ~arg:"True" ~simulation:true Process.check

  let successful_multiple =
    Protocol.register_test
      ~__FILE__
      ~title:"Simulation of successful operation batch"
      ~tags:["client"; "simulation"; "success"; "multiple"; "batch"]
    @@ multiple_transfers
         ~args:["True"; "True"; "True"]
         ~simulation:true
         Runnable.run

  let failing =
    Protocol.register_test
      ~__FILE__
      ~title:"Simulation of failing operation"
      ~tags:["client"; "simulation"; "failing"]
    @@ transfer ~arg:"False" ~simulation:true
    @@ Process.check_error ~exit_code:1 ~msg:(rex "with \"bang\"")

  let failing_force =
    Protocol.register_test
      ~__FILE__
      ~title:"Simulation of failing operation with force"
      ~tags:["client"; "simulation"; "failing"; "force"]
    @@ transfer ~arg:"False" ~simulation:true ~force:true
    @@ fun p ->
    let* stdout = Process.check_and_read_stdout ~expect_failure:false p in
    if stdout =~! rex "This operation FAILED" then
      Test.fail "Did not report operation failure" ;
    unit

  let failing_multiple_force =
    Protocol.register_test
      ~__FILE__
      ~title:"Simulation of failing batch with force"
      ~tags:["client"; "simulation"; "failing"; "multiple"; "batch"; "force"]
    @@ multiple_transfers
         ~args:["True"; "False"; "True"]
         ~simulation:true
         ~force:true
    @@ fun Runnable.{value; _} ->
    let* stdout = Process.check_and_read_stdout ~expect_failure:false value in
    if
      stdout
      =~! rex
            "This transaction was BACKTRACKED[\\S\\s.]*This operation \
             FAILED[\\S\\s.]*This operation was skipped"
    then Test.fail "Did not report operation backtracked/failure/skipping" ;
    unit

  let failing_first_multiple_force =
    Protocol.register_test
      ~__FILE__
      ~title:"Simulation of first failing operation in batch with force"
      ~tags:["client"; "simulation"; "failing"; "multiple"; "batch"; "force"]
    @@ multiple_transfers
         ~args:["False"; "True"; "True"; "True"; "True"; "True"]
         ~simulation:true
         ~force:true
    @@ fun Runnable.{value; _} ->
    let* stdout = Process.check_and_read_stdout ~expect_failure:false value in
    if
      stdout
      =~! rex
            "Simulation result:[\\S\\s.]*This operation FAILED([\\S\\s.]*This \
             operation was skipped){5}"
    then Test.fail "Did not report operation failure/skipping*5" ;
    unit

  let injection_force =
    Protocol.register_test
      ~__FILE__
      ~title:"Injecting of failing operation with force"
      ~tags:["client"; "injection"; "failing"; "force"]
    @@ transfer ~arg:"False" ~force:true
    @@ Process.check_error
         ~exit_code:1
         ~msg:(rex "--gas-limit option is required")

  let injection_multiple_force =
    Protocol.register_test
      ~__FILE__
      ~title:"Injecting of failing operations batch with force"
      ~tags:["client"; "injection"; "failing"; "force"; "multiple"; "batch"]
    @@ multiple_transfers ~args:["True"; "False"; "True"] ~force:true
    @@ fun Runnable.{value; _} ->
    Process.check_error
      value
      ~exit_code:1
      ~msg:(rex "--gas-limit option is required")

  let register protocol =
    successful protocol ;
    successful_multiple protocol ;
    failing protocol ;
    failing_force protocol ;
    failing_multiple_force protocol ;
    failing_first_multiple_force protocol ;
    injection_force protocol ;
    injection_multiple_force protocol
end

module Transfer = struct
  open Helpers

  let alias_pkh_destination =
    Protocol.register_test
      ~__FILE__
      ~title:"Transfer to public key hash alias"
      ~tags:["client"; "alias"; "transfer"]
    @@ fun protocol ->
    let* node, client = Client.init_with_protocol `Client ~protocol () in
    let* client2 = Client.init ~endpoint:(Node node) () in
    let* victim = Client.gen_and_show_keys ~alias:"victim" client in
    let* malicious = Client.gen_and_show_keys ~alias:"malicious" client2 in
    let malicious = {malicious with Account.alias = victim.public_key_hash} in
    Log.info "Importing malicious account whose alias is victim's public key" ;
    let* () = Client.import_secret_key client malicious in
    let amount = Tez.of_int 2 in
    Log.info
      "Transferring to victim's public key hash should not transfer to \
       malicious" ;
    let* () =
      Client.transfer
        ~amount
        ~giver:Constant.bootstrap1.public_key_hash
        ~receiver:victim.public_key_hash
        ~burn_cap:Tez.one
        client
    in
    let* () = Client.bake_for_and_wait client in
    let* balance_victim = get_balance victim.public_key_hash client
    and* balance_malicious = get_balance malicious.public_key_hash client in
    Check.((balance_victim = amount) (convert Tez.to_string string))
      ~error_msg:"Balance of victim should be %R but is %L." ;
    Check.((balance_malicious = Tez.zero) (convert Tez.to_string string))
      ~error_msg:"Balance of malicious should be %R but is %L." ;
    unit

  let alias_pkh_source =
    Protocol.register_test
      ~__FILE__
      ~title:"Transfer from public key hash alias"
      ~tags:["client"; "alias"; "transfer"]
    @@ fun protocol ->
    let* node, client = Client.init_with_protocol `Client ~protocol () in
    let* client2 = Client.init ~endpoint:(Node node) () in
    let* malicious = Client.gen_and_show_keys ~alias:"malicious" client in
    let* victim = Client.gen_and_show_keys ~alias:"victim" client2 in
    let victim = {victim with Account.alias = malicious.public_key_hash} in
    Log.info "Importing victim account whose alias is malicious's public key" ;
    let* () = Client.import_secret_key client victim in
    Log.info "Giving some tokens to victim" ;
    let* () =
      Client.transfer
        ~amount:(Tez.of_int 100)
        ~giver:Constant.bootstrap1.public_key_hash
        ~receiver:("text:" ^ victim.public_key_hash)
        ~burn_cap:Tez.one
        client
    in
    Log.info "Giving some tokens to malicious" ;
    let* () =
      Client.transfer
        ~amount:(Tez.of_int 100)
        ~giver:Constant.bootstrap2.public_key_hash
        ~receiver:("text:" ^ malicious.public_key_hash)
        ~burn_cap:Tez.one
        client
    in
    let* () = Client.bake_for_and_wait client in
    let* prev_balance_victim = get_balance victim.public_key_hash client in
    let amount = Tez.of_int 2 in
    Log.info
      "Transferring from malicious's public key hash should not transfer from \
       victim" ;
    let* () =
      Client.transfer
        ~amount
        ~giver:malicious.public_key_hash
        ~receiver:Constant.bootstrap1.public_key_hash
        ~burn_cap:Tez.one
        client
    in
    let* () = Client.bake_for_and_wait client in
    let* balance_victim = get_balance victim.public_key_hash client in
    Check.(
      (balance_victim = prev_balance_victim) (convert Tez.to_string string))
      ~error_msg:"Balance of victim should be %R but is %L." ;
    unit

  let transfer_tz4 =
    Protocol.register_test
      ~__FILE__
      ~title:"Transfer from and to accounts"
      ~tags:["client"; "transfer"; "bls"; "tz4"]
    @@ fun protocol ->
    let* _node, client = Client.init_with_protocol `Client ~protocol () in
    Log.info "Generating new accounts" ;
    let* accounts1 = gen_accounts protocol client 1 in
    let* accounts2 = gen_accounts protocol client 2 in
    let accounts = accounts1 @ accounts2 in
    let* () = airdrop_and_reveal client accounts in
    let test_transfer (from : Account.key) (dest : Account.key) =
      Log.info "Test transfer from %s to %s" from.alias dest.alias ;
      let amount = Tez.of_int 10 in
      let fee = Tez.of_int 1 in
      let* balance_from0 = get_balance from.public_key_hash client
      and* balance_dest0 = get_balance dest.public_key_hash client in
      let* () =
        Client.transfer
          ~amount
          ~giver:from.public_key_hash
          ~receiver:dest.public_key_hash
          ~fee
          client
      in
      let* () = Client.bake_for_and_wait client in
      let* balance_from = get_balance from.public_key_hash client
      and* balance_dest = get_balance dest.public_key_hash client in
      let expected_balance_from = Tez.(balance_from0 - (amount + fee)) in
      let expected_balance_dest = Tez.(balance_dest0 + amount) in
      Check.(
        (Tez.to_string balance_from = Tez.to_string expected_balance_from)
          string)
        ~error_msg:(sf "Sender %s has balance %%L instead of %%R" from.alias) ;
      Check.(
        (Tez.to_string balance_dest = Tez.to_string expected_balance_dest)
          string)
        ~error_msg:(sf "Receiver %s has balance %%L instead of %%R" dest.alias) ;
      unit
    in
    Lwt_list.iter_s
      (fun from ->
        Lwt_list.iter_s (fun dest -> test_transfer from dest) accounts2)
      accounts1

  let batch_transfers_tz4 =
    Protocol.register_test
      ~__FILE__
      ~title:"Batch transfers"
      ~tags:["client"; "batch"; "transfer"; "bls"; "tz4"]
    @@ fun protocol ->
    let* _node, client = Client.init_with_protocol `Client ~protocol () in
    Log.info "Generating new accounts" ;
    let* accounts = gen_accounts protocol client 1 in
    let* dests = gen_accounts protocol client 2 in
    let* () = airdrop_and_reveal client (accounts @ dests) in
    let test_batch_transfer (from : Account.key) =
      Log.info "Test batch transfer from %s" from.alias ;
      let* balance_from0 = get_balance from.public_key_hash client
      and* balance_dests0 =
        Lwt_list.map_p
          (fun dest -> get_balance dest.Account.public_key_hash client)
          dests
      in
      let amount = Tez.of_int 10 in
      let fee = Tez.of_int 1 in
      let batches =
        Ezjsonm.list
          (fun account ->
            `O
              [
                ("destination", `String account.Account.public_key_hash);
                ("amount", `String (Tez.to_string amount));
                ("fee", `String (Tez.to_string fee));
              ])
          dests
      in
      let*! () =
        Client.multiple_transfers
          ~giver:from.alias
          ~json_batch:(Ezjsonm.to_string batches)
          ~fee_cap:(Tez.of_int 10)
          client
      in
      let* () = Client.bake_for_and_wait client in
      let* balance_from = get_balance from.public_key_hash client
      and* balance_dests =
        Lwt_list.map_p
          (fun dest -> get_balance dest.Account.public_key_hash client)
          dests
      in
      let expected_balance_from =
        let total =
          Tez.(
            mutez_int64 (amount + fee)
            |> Int64.(mul @@ of_int @@ List.length dests)
            |> of_mutez_int64)
        in
        Tez.(balance_from0 - total)
      in
      let expected_balance_dests =
        List.map (fun b -> Tez.(b + amount)) balance_dests0
      in
      Check.(
        (Tez.to_string balance_from = Tez.to_string expected_balance_from)
          string)
        ~error_msg:(sf "Sender %s has balance %%L instead of %%R" from.alias) ;
      List.iter2
        (fun balance_dest expected_balance_dest ->
          Check.(
            (Tez.to_string balance_dest = Tez.to_string expected_balance_dest)
              string)
            ~error_msg:"Receiver has balance %L instead of %R")
        balance_dests
        expected_balance_dests ;
      unit
    in
    Lwt_list.iter_s test_batch_transfer accounts

  let forbidden_set_delegate_tz4 =
    Protocol.register_test
      ~__FILE__
      ~title:"Set delegate forbidden on tz4"
      ~tags:["client"; "set_delegate"; "bls"; "tz4"]
    @@ fun protocol ->
    let* _node, client = Client.init_with_protocol `Client ~protocol () in
    let* () = Client.import_secret_key client Constant.tz4_account in
    let* () = airdrop_and_reveal client [Constant.tz4_account] in
    let*? set_delegate_process =
      Client.set_delegate
        client
        ~src:Constant.tz4_account.public_key_hash
        ~delegate:Constant.tz4_account.public_key_hash
    in
    let msg =
      rex "The delegate tz4.*\\w is forbidden as it is a BLS public key hash"
    in
    Process.check_error set_delegate_process ~exit_code:1 ~msg

  let balance_too_low =
    Protocol.register_test
      ~__FILE__
      ~title:"Test transfer with too low balance"
      ~tags:["client"; "transfer"]
    @@ fun protocol ->
    let* _node, client = Client.init_with_protocol `Client ~protocol () in
    Log.info "Generating new accounts" ;
    let* accounts = gen_accounts protocol client 1 in
    let* () = airdrop_and_reveal client accounts in
    let test_uncovered_transfer (from : Account.key) =
      Log.info "Test uncovered transfer from %s to bootstrap1" from.alias ;
      let* balance_from = get_balance from.public_key_hash client in
      let amount = Tez.(balance_from + one) in
      let fee = Tez.of_int 1 in
      Client.spawn_transfer
        ~amount
        ~giver:from.public_key_hash
        ~receiver:Constant.bootstrap1.public_key_hash
        ~fee
        client
      |> Process.check_error ~msg:(rex "Balance of contract ([^ ]+) too low")
    in
    Lwt_list.iter_s test_uncovered_transfer accounts

  let transfers_bootstraps5_bootstrap1 =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple transfer from bootstrap5 to bootstrap1"
      ~tags:["client"; "transfer"]
    @@ fun protocol ->
    let* _node, client = Client.init_with_protocol `Client ~protocol () in
    let check_balance_and_deposits ~__LOC__ (account : Account.key)
        expected_amount =
      let* all_deposits =
        RPC.Client.call client
        @@ RPC.get_chain_block_context_delegate_frozen_deposits
             account.public_key_hash
      in
      let* balance = get_balance account.public_key_hash client in
      Check.(
        (Tez.(balance + all_deposits) = expected_amount)
          Tez.typ
          ~__LOC__
          ~error_msg:
            ("Expected balance and deposits of " ^ account.alias
           ^ " to be %R, but got %L")) ;
      unit
    in
    let bake () =
      Client.bake_for_and_wait
        ~minimal_fees:0
        ~minimal_nanotez_per_gas_unit:0
        ~minimal_nanotez_per_byte:0
        ~keys:["bootstrap2"; "bootstrap3"; "bootstrap4"]
        client
    in
    let (Account.{public_key_hash = bootstrap1_pkh; _} as bootstrap1) =
      Constant.bootstrap1
    in
    let (Account.{public_key_hash = bootstrap5_pkh; _} as bootstrap5) =
      Constant.bootstrap5
    in
    let fee = Tez.zero in
    let initial_balance = Tez.of_int 4_000_000 in
    let* () = bake () in
    let* () = check_balance_and_deposits ~__LOC__ bootstrap1 initial_balance in
    let* () = check_balance_and_deposits ~__LOC__ bootstrap5 initial_balance in
    let amount = Tez.of_int 400_000 in
    let* () =
      Client.transfer
        client
        ~amount
        ~fee
        ~giver:bootstrap5_pkh
        ~receiver:bootstrap1_pkh
    in
    let* () = bake () in
    let* _ = Client.get_balance_for ~account:"bootstrap5" client in
    let* () =
      check_balance_and_deposits
        ~__LOC__
        bootstrap5
        Tez.(initial_balance - amount)
    in
    let* () =
      check_balance_and_deposits
        ~__LOC__
        bootstrap1
        Tez.(initial_balance + amount)
    in
    let* () =
      Client.transfer
        client
        ~amount
        ~fee
        ~giver:bootstrap1_pkh
        ~receiver:bootstrap5_pkh
    in
    let* () = bake () in
    let* () =
      check_balance_and_deposits ~__LOC__ Constant.bootstrap5 initial_balance
    in
    unit

  let register protocols =
    alias_pkh_destination protocols ;
    alias_pkh_source protocols ;
    transfer_tz4 protocols ;
    batch_transfers_tz4 protocols ;
    forbidden_set_delegate_tz4 protocols ;
    balance_too_low protocols ;
    transfers_bootstraps5_bootstrap1 protocols
end

module Dry_run = struct
  let test_gas_consumed =
    Protocol.register_test
      ~__FILE__
      ~title:"Check consumed gas of origination dry run"
      ~tags:["client"; "gas"; "estimation"; "dryrun"]
    @@ fun protocol ->
    Log.info
      "This test checks that the consumed gas returned by the dry run of a \
       contract origination is sufficient to successfully inject the \
       origination." ;
    Log.info "Initialize a client with protocol %s." (Protocol.name protocol) ;
    let* node, client = Client.init_with_protocol `Client ~protocol () in
    let prg =
      Michelson_script.(
        find ["mini_scenarios"; "large_str_id"] protocol |> path)
    in
    let test_with_signature_scheme sig_alg =
      Log.info
        ~color:Log.Color.bold
        "----------------------------------------\nWith signature scheme: %s"
        sig_alg ;
      let* source = Client.gen_and_show_keys ~sig_alg client in
      Log.info "Fund the source." ;
      let* () =
        Client.transfer
          ~amount:(Tez.of_int 1000)
          ~giver:Constant.bootstrap1.public_key_hash
          ~receiver:source.public_key_hash
          ~burn_cap:Tez.one
          client
      in
      let* () = Client.bake_for_and_wait client in
      Log.info
        "Call the origination command of the client with [--dry-run] to \
         recover the consumed gas." ;
      let call_originate_contract =
        Client.spawn_originate_contract
          ~alias:("originated_contract_" ^ sig_alg)
          ~amount:Tez.zero
          ~src:source.public_key_hash
          ~prg
          ~init:{|Pair "" ""|}
          ~burn_cap:(Tez.of_int 10)
      in
      let dry_run_res = call_originate_contract ~dry_run:true client in
      let* res = Process.check_and_read_stdout dry_run_res in
      let gas_consumed =
        let re =
          Re.Str.regexp "\\(.\\|[ \\\n]\\)*Consumed gas: \\([0-9.]+\\).*"
        in
        if Re.Str.string_match re res 0 then
          float_of_string (Re.Str.matched_group 2 res)
        else
          Test.fail
            "Failed to parse the consumed gas in the following output of the \
             dry run:\n\
             %s"
            res
      in
      let gas_limit = Float.(to_int (ceil gas_consumed)) in
      Log.info
        "Estimated gas consumption is: %f. The gas_limit must be at least of \
         %d gas units for the origination to succeed."
        gas_consumed
        gas_limit ;
      Log.info
        "Try to originate the contract with a gas_limit of %d and check that \
         the origination fails."
        (pred gas_limit) ;
      let originate_res_ko =
        call_originate_contract
          ~gas_limit:(pred gas_limit)
          ~dry_run:false
          client
      in
      let msg = Constant.Error_msg.gas_limit_exceeded in
      let* () = Process.check_error ~msg originate_res_ko in
      Log.info
        "Originate the contract with a gas_limit of %d and check that the \
         origination succeeds."
        gas_limit ;
      let originate_res_ok =
        call_originate_contract
        (* We wait for a new block to force the application of the
           operation, not only its validation. *)
          ~wait:"0"
          ~gas_limit
          ~dry_run:false
          client
      in
      let* () = Node.wait_for_request ~request:`Inject node in
      let* () = Client.bake_for_and_wait client in
      Process.check originate_res_ok
    in
    Lwt_list.iter_s
      test_with_signature_scheme
      (Helpers.supported_signature_schemes protocol)

  let register protocols = test_gas_consumed protocols
end

module Signatures = struct
  open Helpers

  let test_check_signature =
    Protocol.register_test
      ~__FILE__
      ~title:"Test client signatures and on chain check"
      ~tags:["client"; "signature"; "check"; "bls"]
    @@ fun protocol ->
    let* _node, client = Client.init_with_protocol `Client ~protocol () in
    let* contract, _hash =
      Client.originate_contract_at
        ~amount:Tez.zero
        ~src:Constant.bootstrap2.alias
        ~burn_cap:(Tez.of_int 10)
        client
        ["mini_scenarios"; "check_signature"]
        protocol
    in
    Log.info "Generating new accounts" ;
    let* accounts =
      Lwt_list.map_s
        (fun sig_alg ->
          Client.gen_and_show_keys
            ~alias:(sf "account_%s" sig_alg)
            ~sig_alg
            client)
        (supported_signature_schemes protocol)
    in
    let* () = airdrop_and_reveal client accounts in
    let test (account : Account.key) =
      let msg = "0x" ^ Hex.show (Hex.of_string "Some nerdy quote") in
      let* signature =
        Client.sign_bytes ~signer:account.alias ~data:msg client
      in
      Client.transfer
        client
        ~amount:Tez.zero
        ~giver:account.public_key_hash
        ~receiver:contract
        ~arg:(sf "Pair %S %S %s" account.public_key signature msg)
    in
    let* () = Lwt_list.iter_s test accounts in
    let* () = Client.bake_for_and_wait client in
    let* block = RPC.Client.call client @@ RPC.get_chain_block () in
    let ops = JSON.(block |-> "operations" |=> 3 |> as_list) in
    Check.(
      (List.length ops = List.length (supported_signature_schemes protocol)) int)
      ~error_msg:"Block contains %L operations but should have %R" ;
    unit

  let test_check_message_signature =
    Protocol.register_test
      ~__FILE__
      ~title:"Test client message signatures"
      ~tags:["client"; "signature"; "message"; "check"]
    @@ fun protocol ->
    let* _node, client = Client.init_with_protocol ~protocol `Client () in
    [
      ( "bootstrap1",
        "msg1",
        "edsigtz68o4FdbpvycnAMDLaa7hpmmhjDxhx4Zu3QWHLYJtcY1mVhW9m6CCvsciFXwf1zLmah8fJP51cqaeaciBPGy5osH11AnR"
      );
      ( "bootstrap2",
        "msg2",
        "edsigtZqhR5SW6vbRSmqwzfS1KiJZLYLeFhLcCEw7WxjBDxotVx83M2rLe4Baq52SUTjxfXhQ5J3TabCwqt78kNpoU8j42GDEk4"
      );
      ( "bootstrap3",
        "msg3",
        "edsigu2PvAWxVYY3jQFVfBRW2Dg61xZMNesHiNbwCTmpJSyfcJMW8Ch9WABHqsgHQRBaSs6zZNHVGXfHSBnGCxT9x2b49L2zpMW"
      );
      ( "bootstrap4",
        "msg4",
        "edsigu5jieost8eeD3JwVrpPuSnKzLLvR3aqezLPDTvxC3p41qwBEpxuViKriipxig52NQmJ7AFXTzhM3xgKM2ZaADcSMYWztuJ"
      );
    ]
    |> Lwt_list.iter_s @@ fun (src, message, expected_signature) ->
       let* signature = Client.sign_message client ~src message in
       Check.(
         (signature = expected_signature)
           string
           ~__LOC__
           ~error_msg:"Expected signature %R, got %L") ;
       let* () = Client.check_message client ~src ~signature message in
       unit

  let register protocols =
    test_check_signature protocols ;
    test_check_message_signature protocols
end

module Account_activation = struct
  let test_activate_accounts =
    Protocol.register_test
      ~__FILE__
      ~title:"Test account activation"
      ~tags:["client"; "account"; "activation"]
    @@ fun protocol ->
    let parameter_file =
      Protocol.parameter_file ~constants:Constants_test protocol
    in
    let* _node, client =
      Client.init_with_protocol ~parameter_file `Client ~protocol ()
    in
    let* () =
      Client.activate_account
        client
        ~alias:"king"
        ~activation_key:"tezt/tests/account/king_commitment.json"
    in
    let* () = Client.bake_for_and_wait client in
    let* () =
      Client.activate_account
        client
        ~alias:"queen"
        ~activation_key:"tezt/tests/account/queen_commitment.json"
    in
    let* () = Client.bake_for_and_wait client in
    let* king_balance = Client.get_balance_for ~account:"king" client in
    let* queen_balance = Client.get_balance_for ~account:"queen" client in
    Check.(
      (king_balance = Tez.of_mutez_int 23_932_454_669_343)
        Tez.typ
        ~__LOC__
        ~error_msg:"Expected king's balance to be %R, got %L" ;
      (queen_balance = Tez.of_mutez_int 72_954_577_464_032)
        Tez.typ
        ~__LOC__
        ~error_msg:"Expected queen's balance to be %R, got %L") ;
    let* () =
      Client.transfer
        client
        ~amount:Tez.(of_int 10)
        ~giver:"king"
        ~receiver:"queen"
    in
    Client.bake_for_and_wait client

  let register protocols = test_activate_accounts protocols
end

let register ~protocols =
  Simulation.register protocols ;
  Transfer.register protocols ;
  Dry_run.register protocols ;
  Signatures.register protocols ;
  Account_activation.register protocols
