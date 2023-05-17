(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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
   Component: Client - mockup mode
   Invocation: dune exec tezt/tests/main.exe -- --file mockup.ml
   Subject: Tests of the client's --mode mockup.
*)

(* Test.
   Call `octez-client rpc list` and check that return code is 0.
*)
let test_rpc_list =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) RPC list"
    ~tags:["mockup"; "client"; "rpc"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _ = Client.rpc_list client in
  Lwt.return_unit

(* Test.
   Call `octez-client rpc /chains/<chain_id>/blocks/<block_id>/header/shell` and check that return code is 0.
*)
let test_rpc_header_shell =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) RPC header/shell"
    ~tags:["mockup"; "client"; "rpc"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _ = Client.shell_header client in
  Lwt.return_unit

let transfer_data =
  (Constant.bootstrap1.alias, Tez.one, Constant.bootstrap2.alias)

let check_balances_after_transfer giver amount receiver =
  let giver_balance_before, giver_balance_after = giver in
  let receiver_balance_before, receiver_balance_after = receiver in
  if not Tez.(giver_balance_after < giver_balance_before - amount) then
    Test.fail
      "Invalid balance of giver after transfer: %s (before it was %s)"
      (Tez.to_string giver_balance_after)
      (Tez.to_string giver_balance_before) ;
  Log.info
    "Balance of giver after transfer is valid: %s"
    (Tez.to_string giver_balance_after) ;
  let receiver_expected_after = Tez.(receiver_balance_before + amount) in
  if receiver_balance_after <> receiver_expected_after then
    Test.fail
      "Invalid balance of receiver after transfer: %s (expected %s)"
      (Tez.to_string receiver_balance_after)
      (Tez.to_string receiver_expected_after) ;
  Log.info
    "Balance of receiver after transfer is valid: %s"
    (Tez.to_string receiver_balance_after)

(* Test.
   Transfer some tz and check balance changes are as expected.
*)
let test_transfer =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Transfer"
    ~tags:["mockup"; "client"; "transfer"]
  @@ fun protocol ->
  let giver, amount, receiver = transfer_data in
  let* client = Client.init_mockup ~protocol () in
  let* giver_balance_before = Client.get_balance_for ~account:giver client in
  let* receiver_balance_before =
    Client.get_balance_for ~account:receiver client
  in
  Log.info
    "About to transfer %s from %s to %s"
    (Tez.to_string amount)
    giver
    receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  let* giver_balance_after = Client.get_balance_for ~account:giver client in
  let* receiver_balance_after =
    Client.get_balance_for ~account:receiver client
  in
  check_balances_after_transfer
    (giver_balance_before, giver_balance_after)
    amount
    (receiver_balance_before, receiver_balance_after) ;
  return ()

let test_calling_contract_with_global_constant_success =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Calling a contract with a global constant success"
    ~tags:["mockup"; "client"; "global_constant"]
  @@ fun protocol ->
  let src, _, _ = transfer_data in
  let* client = Client.init_mockup ~protocol () in
  let value = "999" in
  let burn_cap = Some (Tez.of_int 1) in
  let* _ = Client.register_global_constant ~src ~value ?burn_cap client in
  let storage = "0" in
  let input = "Unit" in
  let* {storage; _} =
    Client.run_script_at
      ~storage
      ~input
      client
      ["mini_scenarios"; "999_constant"]
      protocol
  in
  Log.info "Contract with constant output storage %s" storage ;
  if storage = value then return ()
  else Test.fail "Expected storage '%s' but got '%s'" value storage

let test_calling_contract_with_global_constant_failure =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Calling a contract with a global constant failure"
    ~tags:["mockup"; "client"; "global_constant"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let storage = "0" in
  let input = "Unit" in
  let process =
    Client.spawn_run_script_at
      ~storage
      ~input
      client
      ["mini_scenarios"; "999_constant"]
      protocol
  in
  Process.check_error
    ~exit_code:1
    ~msg:(rex "No registered global was found")
    process

let test_register_global_constant_success =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Register Global Constant success"
    ~tags:["mockup"; "client"; "global_constant"]
  @@ fun protocol ->
  let src, _, _ = transfer_data in
  let* client = Client.init_mockup ~protocol () in
  let value = "999" in
  let burn_cap = Some (Tez.of_int 1) in
  let* result = Client.register_global_constant ~src ~value ?burn_cap client in
  Log.info "Registered Global Connstant %s with hash %s" value result ;
  return ()

let test_register_global_constant_failure =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Register Global Constant failure"
    ~tags:["mockup"; "client"; "global_constant"]
  @@ fun protocol ->
  let src, _, _ = transfer_data in
  let* client = Client.init_mockup ~protocol () in
  let value = "Pair 1 (constant \"foobar\")" in
  let burn_cap = Some (Tez.of_int 1) in
  let proccess =
    Client.spawn_register_global_constant ~src ~value ?burn_cap client
  in
  Process.check_error
    ~exit_code:1
    ~msg:(rex "register global constant simulation failed")
    proccess

let test_originate_contract_with_global_constant_success =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Originate Contract with Global Constant success"
    ~tags:["mockup"; "client"; "global_constant"]
  @@ fun protocol ->
  let src, _, _ = transfer_data in
  let* client = Client.init_mockup ~protocol () in
  let value = "999" in
  let burn_cap = Some (Tez.of_int 1) in
  let* _ = Client.register_global_constant ~src ~value ?burn_cap client in
  let* _alias, result =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"0"
      ~burn_cap:(Tez.of_int 2)
      client
      ["mini_scenarios"; "999_constant"]
      protocol
  in
  Log.info "result %s" result ;
  return ()

let test_typechecking_and_normalization_work_with_constants =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Typechecking and normalization work with constants"
    ~tags:["mockup"; "client"; "global_constant"]
  @@ fun protocol ->
  let src, _, _ = transfer_data in
  let* client = Client.init_mockup ~protocol () in
  (* Register the type *)
  let value = "unit" in
  let burn_cap = Some (Tez.of_int 1) in
  let* _ = Client.register_global_constant ~src ~value ?burn_cap client in
  (* Register the value *)
  let value = "Unit" in
  let* _ = Client.register_global_constant ~src ~value ?burn_cap client in
  let script =
    Michelson_script.(find ["mini_scenarios"; "constant_unit"] protocol |> path)
  in
  let* _ = Client.normalize_script ~script client in
  let* () = Client.typecheck_script ~scripts:[script] client in
  return ()

let test_simple_baking_event =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Transfer (asynchronous)"
    ~tags:["mockup"; "client"; "transfer"; "asynchronous"]
  @@ fun protocol ->
  let giver, amount, receiver = transfer_data in
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  Log.info "Transferring %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  Log.info "Baking pending operations..." ;
  Client.bake_for ~keys:[giver] client

let transfer_expected_to_fail ~giver ~receiver ~amount client =
  let process = Client.spawn_transfer ~amount ~giver ~receiver client in
  let* status = Process.wait process in
  if status = Unix.WEXITED 0 then
    Test.fail "Last transfer was successful but was expected to fail ..." ;
  return ()

let test_same_transfer_twice =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Same transfer twice (asynchronous)"
    ~tags:["mockup"; "client"; "transfer"; "asynchronous"]
  @@ fun protocol ->
  let giver, amount, receiver = transfer_data in
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  let mempool_file = Client.base_dir client // "mockup" // "mempool.json" in
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  let mempool1 = read_file mempool_file in
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = transfer_expected_to_fail ~amount ~giver ~receiver client in
  let mempool2 = read_file mempool_file in
  Log.info "Checking that mempool is unchanged" ;
  if mempool1 <> mempool2 then
    Test.fail
      "Expected mempool to stay unchanged\n--\n%s--\n %s"
      mempool1
      mempool2 ;
  return ()

let test_transfer_same_participants =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Transfer same participants (asynchronous)"
    ~tags:["mockup"; "client"; "transfer"; "asynchronous"]
  @@ fun protocol ->
  let giver, amount, receiver = transfer_data in
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  let base_dir = Client.base_dir client in
  let mempool_file = base_dir // "mockup" // "mempool.json" in
  let thrashpool_file = base_dir // "mockup" // "trashpool.json" in
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  let mempool1 = read_file mempool_file in
  let amount = Tez.(amount + one) in
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = transfer_expected_to_fail ~amount ~giver ~receiver client in
  let mempool2 = read_file mempool_file in
  Log.info "Checking that mempool is unchanged" ;
  if mempool1 <> mempool2 then
    Test.fail
      "Expected mempool to stay unchanged\n--\n%s\n--\n %s"
      mempool1
      mempool2 ;
  Log.info
    "Checking that last operation was discarded into a newly created trashpool" ;
  let str = read_file thrashpool_file in
  if String.equal str "" then
    Test.fail "Expected thrashpool to have one operation" ;
  return ()

let test_multiple_baking =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Multi transfer/multi baking (asynchronous)"
    ~tags:["mockup"; "client"; "transfer"; "asynchronous"]
  @@ fun protocol ->
  (* For the equality test below to hold, alice, bob and baker must be
     different accounts. Here, alice is bootstrap1, bob is bootstrap2 and
     baker is bootstrap3. *)
  let alice, _amount, bob = transfer_data and baker = "bootstrap3" in
  if String.(equal alice bob || equal bob baker || equal baker alice) then
    Test.fail "alice, bob and baker need to be different accounts" ;
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  Lwt_list.iteri_s
    (fun i amount ->
      let amount = Tez.of_int amount in
      let* () = Client.transfer ~amount ~giver:alice ~receiver:bob client in
      let* () = Client.transfer ~amount ~giver:bob ~receiver:alice client in
      let* () = Client.bake_for ~keys:[baker] client in
      let* alice_balance = Client.get_balance_for ~account:alice client in
      let* bob_balance = Client.get_balance_for ~account:bob client in
      Log.info
        "%d. Balances\n  - Alice :: %s\n  - Bob ::   %s"
        i
        (Tez.to_string alice_balance)
        (Tez.to_string bob_balance) ;
      if alice_balance <> bob_balance then
        Test.fail
          "Unexpected balances for Alice (%s) and Bob (%s). They should be \
           equal."
          (Tez.to_string alice_balance)
          (Tez.to_string bob_balance) ;
      return ())
    (range 1 10)

let perform_migration ~protocol ~next_protocol ~next_constants ~pre_migration
    ~post_migration =
  let* client = Client.init_mockup ~constants:next_constants ~protocol () in
  let* pre_result = pre_migration client in
  Log.info
    "Migrating from %s to %s"
    (Protocol.hash protocol)
    (Protocol.hash next_protocol) ;
  let* () = Client.migrate_mockup ~next_protocol client in
  post_migration client pre_result

let get_candidates_to_migration () =
  let* mockup_protocols =
    let transient = Client.create_with_mode Client.Mockup in
    Client.list_protocols `Mockup transient
  in
  (* Find all registered mockup protocols which declare a next protocol *)
  let result =
    List.filter_map
      (fun (protocol : Protocol.t) ->
        match Protocol.next_protocol protocol with
        | None -> None
        | Some next ->
            let next_hash = Protocol.hash next in
            if
              List.exists
                (String.equal (Protocol.hash protocol))
                mockup_protocols
              && List.exists (String.equal next_hash) mockup_protocols
            then Some (protocol, next)
            else None)
      Protocol.all
  in
  return result

(* Test mockup migration. *)
let test_migration ?(migration_spec : (Protocol.t * Protocol.t) option)
    ~pre_migration ~post_migration ~info () =
  Test.register
    ~__FILE__
    ~title:(sf "(Mockup) Migration (%s)" info)
    ~tags:["mockup"; "migration"]
    (fun () ->
      match migration_spec with
      | None -> (
          Log.info "Searching for protocols to migrate..." ;
          let* protocols = get_candidates_to_migration () in
          match protocols with
          | [] -> Test.fail "No protocol can be tested for migration!"
          | (protocol, next_protocol) :: _ ->
              perform_migration
                ~protocol
                ~next_protocol
                ~next_constants:Protocol.default_constants
                ~pre_migration
                ~post_migration)
      | Some (protocol, next_protocol) ->
          perform_migration
            ~protocol
            ~next_protocol
            ~next_constants:Protocol.default_constants
            ~pre_migration
            ~post_migration)

let test_migration_transfer ?migration_spec () =
  let giver, amount, receiver = ("alice", Tez.of_int 1, "bob") in
  test_migration
    ?migration_spec
    ~pre_migration:(fun client ->
      Log.info
        "Creating two new accounts %s and %s and fund them sufficiently."
        giver
        receiver ;
      let* _ = Client.gen_keys ~alias:giver client in
      let* _ = Client.gen_keys ~alias:receiver client in
      let bigger_amount = Tez.of_int 2 in
      let* () =
        Client.transfer
          ~amount:bigger_amount
          ~giver:Constant.bootstrap1.alias
          ~receiver:giver
          ~burn_cap:Tez.one
          client
      in
      let* () =
        Client.transfer
          ~amount:bigger_amount
          ~giver:Constant.bootstrap1.alias
          ~receiver
          ~burn_cap:Tez.one
          client
      in
      Log.info
        "About to transfer %s from %s to %s"
        (Tez.to_string amount)
        giver
        receiver ;
      let* giver_balance_before =
        Client.get_balance_for ~account:giver client
      in
      let* receiver_balance_before =
        Client.get_balance_for ~account:receiver client
      in
      let* () = Client.transfer ~amount ~giver ~receiver client in
      return (giver_balance_before, receiver_balance_before))
    ~post_migration:
      (fun client (giver_balance_before, receiver_balance_before) ->
      let* giver_balance_after = Client.get_balance_for ~account:giver client in
      let* receiver_balance_after =
        Client.get_balance_for ~account:receiver client
      in
      check_balances_after_transfer
        (giver_balance_before, giver_balance_after)
        amount
        (receiver_balance_before, receiver_balance_after) ;
      return ())
    ~info:"transfer"
    ()

(* Check constants equality between that obtained by directly initializing
   a mockup context at alpha and that obtained by migrating from
   alpha~1 to alpha *)
let test_migration_constants ~migrate_from ~migrate_to =
  Test.register
    ~__FILE__
    ~title:
      (sf
         "(%s -> %s) constant migration"
         (Protocol.name migrate_from)
         (Protocol.name migrate_to))
    ~tags:["mockup"; "migration"]
    (fun () ->
      let constants_path =
        ["chains"; "main"; "blocks"; "head"; "context"; "constants"]
      in
      let* client_to =
        Client.init_mockup
          ~constants:Protocol.Constants_mainnet
          ~protocol:migrate_to
          ()
      in
      let* const_to = Client.(rpc GET constants_path client_to) in
      let* const_migrated =
        perform_migration
          ~protocol:migrate_from
          ~next_protocol:migrate_to
          ~next_constants:Protocol.Constants_mainnet
          ~pre_migration:(fun _ -> return ())
          ~post_migration:(fun client () ->
            Client.(rpc GET constants_path client))
      in
      if const_to = const_migrated then return ()
      else (
        Log.error
          "constants (%s):\n%s\n"
          (Protocol.tag migrate_to)
          (JSON.encode const_to) ;
        Log.error
          "constants (migrated from %s):\n%s\n"
          (Protocol.tag migrate_from)
          (JSON.encode const_migrated) ;
        Test.fail "Protocol constants mismatch"))

(** Test. Reproduce the scenario of https://gitlab.com/tezos/tezos/-/issues/1143 *)
let test_origination_from_unrevealed_fees =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) origination fees from unrevealed"
    ~tags:["mockup"; "client"; "transfer"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* () =
    Client.import_secret_key
      client
      {
        alias = "originator";
        public_key_hash = "";
        public_key = "";
        secret_key =
          Unencrypted
            "edskRiUZpqYpyBCUQmhpfCmzHfYahfiMqkKb9AaYKaEggXKaEKVUWPBz6RkwabTmLHXajbpiytRdMJb4v4f4T8zN9t6QCHLTjy";
      }
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 999999)
      ~giver:"bootstrap1"
      ~receiver:"originator"
      client
  in
  let* _ =
    Client.originate_contract_at
      ~wait:"none"
      ~amount:Tez.zero
      ~src:"originator"
      ~init:"None"
      ~burn_cap:(Tez.of_int 20)
      client
      ["mini_scenarios"; "str_id"]
      protocol
  in
  return ()

(** Test. Reproduce the scenario fixed by https://gitlab.com/tezos/tezos/-/merge_requests/3546 *)

let test_multiple_transfers =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) multiple transfer simulation"
    ~tags:["mockup"; "client"; "multiple"; "transfer"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let batch_line =
    `O
      [
        ("destination", `String Constant.bootstrap1.public_key_hash);
        ("amount", `String "0.02");
      ]
  in
  let batch n = `A (List.init n (fun _ -> batch_line)) in
  let file = Temp.file "batch.json" in
  let oc = open_out file in
  Ezjsonm.to_channel oc (batch 200) ;
  close_out oc ;
  let*! () =
    Client.multiple_transfers
      ~giver:Constant.bootstrap2.alias
      ~json_batch:file
      client
  in
  unit

let test_empty_block_baking =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Transfer (empty, asynchronous)"
    ~tags:["mockup"; "client"; "empty"; "bake_for"; "asynchronous"]
  @@ fun protocol ->
  let giver, _amount, _receiver = transfer_data in
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  Log.info "Baking pending operations..." ;
  Client.bake_for ~keys:[giver] client

let test_storage_from_file =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Load storage and input from file."
    ~tags:["mockup"; "client"; "run_script"]
  @@ fun protocol ->
  Format.printf "%s" @@ Unix.getcwd () ;
  let* client = Client.init_mockup ~protocol () in
  Lwt_io.with_temp_file (fun (temp_filename, pipe) ->
      let* () = Lwt_io.write pipe "Unit" in
      let* _storage =
        Client.run_script_at
          ~storage:temp_filename
          ~input:temp_filename
          client
          ["mini_scenarios"; "very_small"]
          protocol
      in
      unit)

(* Executes `octez-client list mockup protocols`. The call must
   succeed and return a non empty list. *)
let test_list_mockup_protocols () =
  Test.register
    ~__FILE__
    ~title:"(Mockup) List mockup protocols."
    ~tags:["mockup"; "client"; "protocols"]
  @@ fun () ->
  let client = Client.create_with_mode Client.Mockup in
  let* protocols = Client.list_protocols `Mockup client in
  if protocols = [] then Test.fail "List of mockup protocols must be non-empty" ;
  unit

(* Executes [octez-client --base-dir /tmp/mdir create mockup] when
   [/tmp/mdir] is a non empty directory which is NOT a mockup
   directory. The call must fail. *)
let test_create_mockup_dir_exists_nonempty =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Create mockup in existing base dir"
    ~tags:["mockup"; "client"; "base_dir"]
  @@ fun protocol ->
  let base_dir = Temp.dir "mockup_dir" in
  write_file ~contents:"" (base_dir // "whatever") ;
  let client = Client.create_with_mode ~base_dir Client.Mockup in
  let* () =
    Client.spawn_create_mockup client ~protocol
    |> Process.check_error
         ~msg:(rex "is not empty, please specify a fresh base directory")
  in
  unit

let test_retrieve_addresses =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Retrieve addresses"
    ~tags:["mockup"; "client"; "wallet"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* addresses = Client.list_known_addresses client in
  let expected_addresses =
    Account.Bootstrap.keys |> Array.to_list |> List.rev
    |> List.map @@ fun Account.{alias; public_key_hash; _} ->
       (alias, public_key_hash)
  in
  Check.(
    (addresses = expected_addresses)
      ~__LOC__
      (list (tuple2 string string))
      ~error_msg:"Expected addresses %R, got %L") ;
  unit

(* Executes [octez-client --base-dir /tmp/mdir create mockup] when
   [/tmp/mdir] is not fresh. The call must fail. *)
let test_create_mockup_already_initialized =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Create mockup when already initialized."
    ~tags:["mockup"; "client"; "base_dir"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* () =
    Client.spawn_create_mockup client ~protocol
    |> Process.check_error
         ~msg:(rex "is already initialized as a mockup directory")
  in
  unit

(* Tests [tezos-client create mockup]s [--protocols-constants]
   argument. The call must succeed. *)
let test_create_mockup_custom_constants =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Create mockup with mockup-custom protocol constants."
    ~tags:["mockup"; "client"; "mockup_protocol_constants"]
  @@ fun protocol ->
  let iter = Fun.flip Lwt_list.iter_s in
  (* [chain_id] is the string to pass for field [chain_id]. It's
     impossible to guess values of [chain_id], these ones have been *
     obtained by looking at the output of [compute chain id from
     seed]. *)
  iter
    [
      "NetXcqTGZX74DxG";
      "NetXaFDF7xZQCpR";
      "NetXkKbtqncJcAz";
      "NetXjjE5cZUeWPy";
      "NetXi7C1pyLhQNe";
    ]
  @@ fun chain_id ->
  (* initial_timestamp is an ISO-8601 formatted date string *)
  iter ["2020-07-21T17:11:10+02:00"; "1970-01-01T00:00:00Z"]
  @@ fun initial_timestamp ->
  let parameter_file = Temp.file "tezos-custom-constants.json" in
  let json_fields =
    [
      ("hard_gas_limit_per_operation", `String "400000");
      ("chain_id", `String chain_id);
      ("initial_timestamp", `String initial_timestamp);
    ]
  in
  let json_data : JSON.u = `O json_fields in
  JSON.encode_to_file_u parameter_file json_data ;

  let client = Client.create_with_mode Client.Mockup in
  let* () = Client.create_mockup ~protocol ~parameter_file client in
  unit

(* A [mockup_bootstrap_account] represents a bootstrap accounts as
   taken by the [--bootstrap-accounts] option of mockup mode *)
type mockup_bootstrap_account = {name : string; sk_uri : string; amount : Tez.t}

let mockup_bootstrap_account_typ =
  Check.(
    convert
      (fun {name; sk_uri; amount} -> (name, sk_uri, amount))
      (tuple3 string string Tez.typ))

let test_accounts : mockup_bootstrap_account list =
  [
    {
      name = "bootstrap0";
      sk_uri = "edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3";
      amount = Tez.of_mutez_int 2000000000000;
    };
    {
      name = "bootstrap1";
      sk_uri = "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh";
      amount = Tez.of_mutez_int 1000000000000;
    };
  ]

let mockup_bootstrap_account_to_json {name; sk_uri; amount} =
  `O
    [
      ("name", `String name);
      ("sk_uri", `String ("unencrypted:" ^ sk_uri));
      ("amount", `String (Tez.to_mutez amount |> string_of_int));
    ]

let mockup_bootstrap_accounts_to_json accounts =
  `A (List.map mockup_bootstrap_account_to_json accounts)

let mockup_bootstrap_account_of_json json : mockup_bootstrap_account =
  JSON.
    {
      name = json |-> "name" |> as_string;
      sk_uri =
        ( json |-> "sk_uri" |> as_string |> fun sk_uri ->
          match String.split_on_char ':' sk_uri with
          | ["unencrypted"; s] -> s
          | _ -> Test.fail "Could not parse [sk_uri] %s" sk_uri );
      amount = json |-> "amount" |> as_int |> Tez.of_mutez_int;
    }

let mockup_bootstrap_accounts_of_json json =
  List.map mockup_bootstrap_account_of_json (JSON.as_list json)

(* Tests [tezos-client create mockup --bootstrap-accounts]
   argument. The call must succeed. *)
let test_create_mockup_custom_bootstrap_accounts =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Create mockup with mockup-custom bootstrap accounts."
    ~tags:["mockup"; "client"; "mockup_bootstrap_accounts"]
  @@ fun protocol ->
  let bootstrap_accounts_file = Temp.file "tezos-bootstrap-accounts.json" in
  JSON.encode_to_file_u
    bootstrap_accounts_file
    (mockup_bootstrap_accounts_to_json test_accounts) ;
  let client = Client.create_with_mode Client.Mockup in
  let* () = Client.create_mockup ~protocol ~bootstrap_accounts_file client in

  let names_sent =
    test_accounts |> List.map (fun {name; _} -> name) |> List.rev
  in
  let* accounts_witnessed = Client.list_known_addresses client in
  let names_witnessed = List.map fst accounts_witnessed in
  Check.(
    (names_witnessed = names_sent)
      ~__LOC__
      (list string)
      ~error_msg:"Expected names %R, got %L") ;
  unit

let rmdir dir = Process.spawn "rm" ["-rf"; dir] |> Process.check

(* Executes [tezos-client --base-dir /tmp/mdir create mockup] when
   [/tmp/mdir] looks like a dubious base directory. Checks that a warning
   is printed. *)
let test_transfer_bad_base_dir =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Transfer bad base dir."
    ~tags:["mockup"; "client"; "initialization"]
  @@ fun protocol ->
  Log.info "First create mockup with an empty base dir" ;
  let base_dir = Temp.dir "mockup-dir" in
  Sys.rmdir base_dir ;
  let client = Client.create_with_mode ~base_dir Client.Mockup in
  let* () = Client.create_mockup ~protocol client in
  let base_dir = Client.base_dir client in
  let mockup_dir = base_dir // "mockup" in
  Log.info "A valid mockup has a directory named [mockup], in its directory" ;
  Check.directory_exists ~__LOC__ mockup_dir ;

  Log.info "Delete this directory:" ;
  let* () = rmdir mockup_dir in
  Log.info "And put a file instead:" ;
  write_file mockup_dir ~contents:"" ;

  Log.info "Now execute a command" ;
  let* () =
    Client.spawn_transfer
      ~amount:Tez.one
      ~giver:"bootstrap1"
      ~receiver:"bootstrap2"
      client
    |> Process.check_error
         ~msg:(rex "Some commands .* might not work correctly.")
  in
  unit

(* Executes [tezos-client --mode mockup config show] in a state where
   it should succeed. *)
let test_config_show_mockup =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Show config."
    ~tags:["mockup"; "client"; "config"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _ = Client.config_show ~protocol client in
  unit

(* Executes [tezos-client --mode mockup config show] when base dir is
   NOT a mockup. It should fail as this is dangerous (the default base
   directory could contain sensitive data, such as private keys) *)
let test_config_show_mockup_fail =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Show config failure."
    ~tags:["mockup"; "client"; "config"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* () = rmdir (Client.base_dir client) in
  let* _ = Client.spawn_config_show ~protocol client |> Process.check_error in
  unit

(* Executes [tezos-client config init mockup] in a state where it
   should succeed *)
let test_config_init_mockup =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Mockup config initialization."
    ~tags:["mockup"; "client"; "config"; "initialization"]
  @@ fun protocol ->
  let protocol_constants = Temp.file "protocol-constants.json" in
  let bootstrap_accounts = Temp.file "bootstrap-accounts.json" in
  let* client = Client.init_mockup ~protocol () in
  let* () =
    Client.config_init ~protocol ~bootstrap_accounts ~protocol_constants client
  in
  let (_ : JSON.t) = JSON.parse_file protocol_constants in
  let (_ : JSON.t) = JSON.parse_file bootstrap_accounts in
  unit

(* Executes [tezos-client config init mockup] when base dir is NOT a
   mockup. It should fail as this is dangerous (the default base
   directory could contain sensitive data, such as private keys) *)
let test_config_init_mockup_fail =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Mockup config initialization failure."
    ~tags:["mockup"; "client"; "config"; "initialization"]
  @@ fun protocol ->
  let protocol_constants = Temp.file "protocol-constants.json" in
  let bootstrap_accounts = Temp.file "bootstrap-accounts.json" in
  let* client = Client.init_mockup ~protocol () in
  Log.info "remove the mockup directory to invalidate the mockup state" ;
  let* () = rmdir (Client.base_dir client // "mockup") in
  let* () =
    Client.spawn_config_init
      ~protocol
      ~bootstrap_accounts
      ~protocol_constants
      client
    |> Process.check_error
  in
  Check.file_not_exists ~__LOC__ protocol_constants ;
  Check.file_not_exists ~__LOC__ bootstrap_accounts ;
  unit

(* Variant of test_transfer that uses RPCs to get the balances. *)
let test_transfer_rpc =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Mockup transfer RPC."
    ~tags:["mockup"; "client"; "transfer"; "rpc"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let get_balance (key : Account.key) =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_contract_balance ~id:key.public_key_hash ()
  in
  let giver = Account.Bootstrap.keys.(0) in
  let receiver = Account.Bootstrap.keys.(1) in
  let amount = Tez.one in
  let* giver_balance_before = get_balance giver in
  let* receiver_balance_before = get_balance receiver in
  let* () =
    Client.transfer ~amount ~giver:giver.alias ~receiver:receiver.alias client
  in
  let* giver_balance_after = get_balance giver in
  let* receiver_balance_after = get_balance receiver in
  Check.(giver_balance_after < Tez.(giver_balance_before - amount))
    Tez.typ
    ~__LOC__
    ~error_msg:"Expected giver balance < %R, got %L" ;
  Check.(receiver_balance_after = Tez.(receiver_balance_before + amount))
    Tez.typ
    ~__LOC__
    ~error_msg:"Expected receiver balance = %R, got %L" ;
  unit

let test_proto_mix =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Mockup mixed protocols."
    ~tags:["mockup"; "client"; "transfer"; "rpc"]
  @@ fun protocol ->
  let protos1, protos2 =
    match Protocol.previous_protocol protocol with
    | Some previous_protocol ->
        ( [protocol; previous_protocol],
          [Some protocol; Some previous_protocol; None] )
    | None -> ([protocol], [Some protocol; None])
  in
  Fun.flip Lwt_list.iter_s protos1 @@ fun proto1 ->
  Fun.flip Lwt_list.iter_s protos2 @@ fun proto2 ->
  (* This test covers 3 cases:

     1/ When [proto2] equals [Some proto1]: it tests that the command works.

     2/ When [proto2] is [None]: it tests that the correct
       mockup implementation is picked (i.e. the one of [proto1])
       and that the command works.

     3/ When [proto2] is [Some proto] such that [proto <> proto1]:
       it tests that creating a mockup with a protocol and
       using it with another protocol fails. *)
  let* client1 = Client.init_mockup ~protocol:proto1 () in
  let client2 =
    Client.create_with_mode ~base_dir:(Client.base_dir client1) Mockup
  in
  Fun.flip
    Lwt_list.iter_s
    [
      ["config"; "show"];
      ["config"; "init"];
      ["list"; "known"; "addresses"];
      ["get"; "balance"; "for"; "bootstrap1"];
    ]
  @@ fun cmd ->
  match (proto1, proto2) with
  | _, Some proto2 when proto1 = proto2 ->
      Client.spawn_command ~protocol_hash:(Protocol.hash proto2) client2 cmd
      |> Process.check
  | _, None -> Client.spawn_command client2 cmd |> Process.check
  | _, Some proto2 ->
      Client.spawn_command ~protocol_hash:(Protocol.hash proto2) client2 cmd
      |> Process.check_error

type mockup_config_state = {
  bootstrap_accounts : mockup_bootstrap_account list;
  protocol_constants : JSON.t;
}

let parametrize ls f = List.iter f ls

let param_name = fst

let param_value = snd

let test_create_mockup_config_show_init_roundtrip protocols =
  (* Given the [obj] of JSON schema [schema], attempt to return a
     value of the same schema that is not equal to [obj].

     If [obj] does not correspond to [schema], return an arbitrary
     value that respects [schema]. *)
  let rec distinct_sample (value : JSON.t) (schema : JSON.t) : JSON.u =
    (* Returns an integer [x] such that [min <= x <= max].
       [min] (resp. [max]) defaults to [Int.min_int] (resp. [Int.max_int]).
       Additionally, if [n_opt] is [Some n] then the returned value [x] is
       distinct from [n] ([n] must be in the bounds [min <= n <= max]).
    *)
    let distinct_sample_numeric ?minimum ?maximum n_opt =
      let n =
        match (n_opt, minimum, maximum) with
        | Some n, _, _ -> n
        | None, Some minimum, _ -> minimum
        | None, None, Some maximum -> maximum
        | _ -> 0
      in
      let max = Option.value ~default:Int.max_int maximum in
      let min = Option.value ~default:Int.min_int minimum in
      (* sanity check *)
      if not (min < max && min <= n && n <= max) then
        Test.fail
          ~__LOC__
          "[distinct_sample_numeric] given [n_opt] is not in between [min] and \
           [max] (inclusive)" ;
      if n < max then n + 1 else min
    in
    (* Returns an element from [candidates] distinct from [value_opt].
       If [value_opt] is [None], return first element in [candidates]. *)
    let distinct_sample_list ~equal candidates value_opt =
      match value_opt with
      | None -> List.hd candidates
      | Some value -> (
          match List.find_opt (Fun.negate (equal value)) candidates with
          | Some res -> res
          | None ->
              Test.fail "distinct_sample_list: could not find a distinct value")
    in
    let typ =
      match JSON.(schema |-> "type" |> as_string_opt) with
      | Some s -> s
      | None -> (
          match JSON.(schema |-> "$ref" |> as_string_opt) with
          | Some r -> r
          | None ->
              Test.fail
                "Schema %s is missing both [type] and [$ref] field"
                (JSON.encode schema))
    in
    let numerical_of_string ~typ value =
      JSON.(
        value |> as_string_opt
        |> Option.map @@ fun s ->
           mandatory
             (sf
                "distinct_sample: no support for %s outside of int's range"
                typ)
             (int_of_string_opt s))
    in
    match typ with
    | "object" ->
        assert (JSON.is_object value) ;
        `O
          (List.map
             (fun (key, key_schema) ->
               (key, distinct_sample JSON.(value |-> key) key_schema))
             JSON.(schema |-> "properties" |> as_object))
    | "integer" ->
        let n_opt = JSON.(value |> as_int_opt) in
        let minimum = JSON.(schema |-> "minimum" |> as_int_opt) in
        let maximum = JSON.(schema |-> "maximum" |> as_int_opt) in
        `Float (distinct_sample_numeric ?minimum ?maximum n_opt |> float_of_int)
    | "boolean" ->
        let b = JSON.(value |> as_bool_opt |> Option.value ~default:false) in
        `Bool (not b)
    | "#/definitions/bignum" ->
        let n_opt = numerical_of_string ~typ value in
        `String (distinct_sample_numeric n_opt |> string_of_int)
    | "#/definitions/int64" ->
        let n_opt = numerical_of_string ~typ value in
        let minimum = Int.min_int in
        let maximum = Int.max_int in
        `String
          (distinct_sample_numeric ~minimum ~maximum n_opt |> string_of_int)
    | typ when typ =~ rex "#/definitions/.*\\.mutez" ->
        let n_opt = numerical_of_string ~typ value in
        `String (distinct_sample_numeric ~minimum:0 n_opt |> string_of_int)
    | "#/definitions/Signature.Public_key_hash"
    | "#/definitions/Signature.V0.Public_key_hash" ->
        let value' =
          distinct_sample_list
            ~equal:String.equal
            (List.map
               (fun (acct : Account.key) -> acct.public_key_hash)
               (Array.to_list Account.Bootstrap.keys))
            JSON.(value |> as_string_opt)
        in
        `String value'
    | "#/definitions/random" ->
        let value' =
          distinct_sample_list
            ~equal:String.equal
            [
              "rngFtAUcm1EneHCCrxxSWAaxSukwEhSPvpTnFjVdKLEjgkapUy1pP";
              "rngGPSm87ZqWxJmZu7rewiLiyKY72ffCQQvxDuWmFBw59dWAL5VTB";
            ]
            JSON.(value |> as_string_opt)
        in
        `String value'
    | typ ->
        Test.fail
          "[distinct_sample] is not implemented for types [%s] (value: %s)"
          typ
          (JSON.encode value)
  in
  (* Create a protocol constant value adapted for the mockup client
     initialization, while attempting to change each parameter from
     the default in order to verify that config initialization
     respects all fields of the input. *)
  let protocol_constants_fixture protocol =
    (* create a temporary client used to call RPC in the given
       protocol *)
    let client = Client.create_with_mode Client.Mockup in
    let* () = Client.create_mockup ~protocol client in

    (* Fetch default values *)
    let* parametric_constants =
      RPC.Client.call client
      @@ RPC.get_chain_block_context_constants_parametric ()
    in
    (* Fetch schema, used to move from default values *)
    let* parametric_constants_schema =
      let* json =
        RPC.Client.schema client
        @@ RPC.get_chain_block_context_constants_parametric ()
      in
      return JSON.(json |-> "output")
    in
    (* Move from default values *)
    let parametric_constants_succ : JSON.t =
      JSON.annotate ~origin:"parametric_constants_succ"
      @@ distinct_sample parametric_constants parametric_constants_schema
    in
    (* Some constants are very constant. *)
    let constant_parametric_constants : JSON.t =
      JSON.annotate ~origin:"constant_parametric_constants"
      @@ `O
           [
             (* DO NOT EDIT the value consensus_threshold this is actually a constant, not a parameter *)
             ("consensus_threshold", `Float 0.0);
           ]
    in
    (* These are the mockup specific protocol parameters as per [src/proto_alpha/lib_client/mockup.ml] *)
    let mockup_constants : JSON.t =
      JSON.annotate ~origin:"mockup_constants"
      @@ `O
           [
             ("initial_timestamp", `String "2021-02-03T12:34:56Z");
             ("chain_id", `String "NetXaFDF7xZQCpR");
           ]
    in
    return
      JSON.(
        merge_objects
          (merge_objects
             parametric_constants_succ
             constant_parametric_constants)
          mockup_constants)
  in

  let get_state_using_config_show_mockup ~protocol mockup_client =
    (* Calls `config init mockup` on a mockup client and returns the
       strings of the bootstrap accounts and the protocol constants

       Note that because this a mockup specific operation, the
       [mockup_client] parameter must be in mockup mode; do not give a
       vanilla client. *)
    let* config = Client.config_show ~protocol mockup_client in
    let lines = String.split_on_char '\n' config in
    let split_lines line lines =
      let rec split_list_opt acc pred = function
        | [] -> None
        | x :: xs when pred x -> Some (List.rev acc, xs)
        | x :: xs -> split_list_opt (x :: acc) pred xs
      in
      match split_list_opt [] (( = ) line) lines with
      | Some v -> v
      | None -> Test.fail "mockup config does not contain the line '%s'" line
    in
    let ba_line = "Default value of --bootstrap-accounts:" in
    let pc_line = "Default value of --protocol-constants:" in
    let ba_lines, pc_lines =
      let _, after = split_lines ba_line lines in
      split_lines pc_line after
    in
    let origin = "Client.config_show" in
    return
      {
        bootstrap_accounts =
          JSON.parse ~origin (String.concat "\n" ba_lines)
          |> mockup_bootstrap_accounts_of_json;
        protocol_constants = JSON.parse ~origin (String.concat "\n" pc_lines);
      }
  in
  let get_state_using_config_init_mockup ~protocol mockup_client =
    (* Calls `config init mockup` on a mockup client and returns the
       strings of the bootstrap accounts and the protocol constants

       Note that because this a mockup specific operation, the
       `mock_client` parameter must be in mockup mode; do not give a
       vanilla client. *)
    let bootstrap_accounts =
      (* Prefix temp file names to avoid reusing the same file name
         for two different clients since mockup refuses to overwrite
         existing files. *)
      Temp.file (Client.name mockup_client ^ "-tezos-bootstrap-accounts.json")
    in
    let protocol_constants =
      Temp.file (Client.name mockup_client ^ "-tezos-proto-consts.json")
    in
    let* () =
      Client.config_init
        ~protocol
        ~bootstrap_accounts
        ~protocol_constants
        mockup_client
    in
    return
      {
        bootstrap_accounts =
          JSON.parse_file bootstrap_accounts
          |> mockup_bootstrap_accounts_of_json;
        protocol_constants = JSON.parse_file protocol_constants;
      }
  in
  let compute_expected_amounts bootstrap_accounts protocol_constants =
    let frozen_deposits_percentage =
      JSON.(protocol_constants |-> "frozen_deposits_percentage" |> as_int)
    in
    let pct = 100 - frozen_deposits_percentage in
    List.map
      (fun account ->
        {
          account with
          amount = Tez.(of_mutez_int (pct * to_mutez account.amount / 100));
        })
      bootstrap_accounts
  in
  (* Check that two JSON objects are equal by pair-wise comparing
     their fields. The result for two objects is the same as using
     [Check.json_t] except that field ordering does not matter (on the
     first level) and that the error messages are easier to read. *)
  let check_json_obj_equal_per_field left right =
    let left_fields = JSON.(left |> as_object |> List.map fst) in
    let right_fields = JSON.(right |> as_object |> List.map fst) in
    List.iter
      (fun field ->
        let lvalue, rvalue = JSON.(left |-> field, right |-> field) in
        Check.(
          (lvalue = rvalue)
            json
            ~__LOC__
            ~error_msg:
              ("The expected protocol constants in mockup was %L, got %R, \
                field: " ^ field)))
      (left_fields @ right_fields |> List.sort_uniq String.compare)
  in
  parametrize [("None", None); ("test_accounts", Some test_accounts)]
  @@ fun param_initial_bootstrap_accounts_opt ->
  parametrize
    [("None", None); ("protocol_constants", Some protocol_constants_fixture)]
  @@ fun param_protocol_constants ->
  parametrize
    [
      ("show", get_state_using_config_show_mockup);
      ("init", get_state_using_config_init_mockup);
    ]
  @@ fun param_read_initial_state ->
  parametrize
    [
      ("show", get_state_using_config_show_mockup);
      ("init", get_state_using_config_init_mockup);
    ]
  @@ fun param_read_final_state ->
  (let parameter_names =
     [
       param_name param_initial_bootstrap_accounts_opt;
       param_name param_protocol_constants;
       param_name param_read_initial_state;
       param_name param_read_final_state;
     ]
   in
   let parametrization_suffix = "[" ^ String.concat "," parameter_names ^ "]" in
   let tags =
     parameter_names
     |> List.filter (( <> ) "None")
     |> List.map String.lowercase_ascii
     |> List.map (fun s -> String.(sub s 0 (Int.min (length s) 32)))
     |> List.sort_uniq String.compare
   in
   (* 1/ Create a mockup, using possibly custom bootstrap_accounts (as
      specified by [param_initial_bootstrap_accounts_opt]).

      2/ Then execute either [--mode mockup config show] or [--mode
      mockup config init] to obtain the mockup's parameters, as specified
      by [param_read_initial_state].

      3/ Recreate a mockup using the output gathered in 2/ and call
      [param_read_final_state] to check that output received is similar to
      output seen in 2.

      This is a roundtrip test using a matrix :headexplodes: *)
   Protocol.register_test
     ~__FILE__
     ~title:
       ("(Mockup) Create mockup config show / init roundtrip "
      ^ parametrization_suffix)
     ~tags:(["mockup"; "client"; "base_dir"; "roundtrip"] @ tags)
   @@ fun protocol ->
   let* protocol_constants_opt, parameter_file_opt =
     match param_value param_protocol_constants with
     | Some protocol_constants_f ->
         (* create a transient client just for accessing RPCs *)
         let* param_protocol_constants = protocol_constants_f protocol in
         let protocol_constants_file =
           Temp.file "tezos-protocol-constants.json"
         in
         Log.info
           "Wrote initial protocol constants file %s"
           protocol_constants_file ;
         JSON.encode_to_file protocol_constants_file param_protocol_constants ;
         return (Some param_protocol_constants, Some protocol_constants_file)
     | None ->
         Log.info "No initial protocol constants file" ;
         return (None, None)
   in

   let bootstrap_accounts_file =
     match param_value param_initial_bootstrap_accounts_opt with
     | Some bootstrap_accounts ->
         let bootstrap_accounts_file =
           Temp.file "tezos-bootstrap-accounts.json"
         in
         Log.info
           "Wrote initial bootstrap accounts file %s"
           bootstrap_accounts_file ;
         JSON.encode_to_file_u
           bootstrap_accounts_file
           (mockup_bootstrap_accounts_to_json bootstrap_accounts) ;
         Some bootstrap_accounts_file
     | None ->
         Log.info "No initial bootstrap accounts file" ;
         None
   in

   let mockup_client = Client.create_with_mode Client.Mockup in
   let* () =
     Client.create_mockup
       ~protocol
       ?parameter_file:parameter_file_opt
       ?bootstrap_accounts_file
       mockup_client
   in

   (* 2a/ No need to check explicitly that the json obtained is valid:
      [param_read_initial_state] will raise an error in that case. *)
   Log.info "Reading initial state after first initialization" ;
   let* initial_state =
     param_value param_read_initial_state ~protocol mockup_client
   in

   (* Test that the initial mockup call honored the values it
      received. If it didn't, all calls would return the default values
      all along, and everything would seem fine; but it wouldn't
      be. This was witnessed in
      https://gitlab.com/tezos/tezos/-/issues/938 *)
   Log.info "Checking that read state corresponds to configuration input" ;
   let () =
     match param_value param_initial_bootstrap_accounts_opt with
     | Some initial_bootstrap_accounts ->
         let expected_amounts =
           compute_expected_amounts
             initial_bootstrap_accounts
             initial_state.protocol_constants
         in
         Check.(expected_amounts = initial_state.bootstrap_accounts)
           (Check.list mockup_bootstrap_account_typ)
           ~error_msg:"The expected bootstrap accounts in mockup was %L, got %R"
           ~__LOC__
     | None -> ()
   in

   (match protocol_constants_opt with
   | Some param_protocol_constants ->
       (* A hack? If the user-provided overrides contains a [null] for
          an optional field (corresponding to [`Null] in Ezjsonm), then
          that field will simply be absent in the output
          [initial_state.param_protocol_constants]. Therefore, we filter such
          values from the comparison. *)
       let expected_protocol_constants =
         JSON.filter_object param_protocol_constants @@ fun _key value ->
         not (JSON.is_null value)
       in
       check_json_obj_equal_per_field
         expected_protocol_constants
         initial_state.protocol_constants
   | None -> ()) ;

   (* 3/ Pass obtained json to a new mockup instance, to check json
      valid w.r.t. ocaml encoding *)
   Log.info "Use read state from first mockup instance to create a second one" ;
   let parameter_file = Temp.file "tezos-protocol-constants.json" in
   JSON.encode_to_file parameter_file initial_state.protocol_constants ;
   let bootstrap_accounts_file = Temp.file "tezos-bootstrap-accounts.json" in
   JSON.encode_to_file_u
     bootstrap_accounts_file
     (mockup_bootstrap_accounts_to_json initial_state.bootstrap_accounts) ;
   let mockup_client = Client.create_with_mode Client.Mockup in
   let* () =
     Client.create_mockup
       ~protocol
       ~parameter_file
       ~bootstrap_accounts_file
       mockup_client
   in

   (* 4/ Retrieve state again *)
   Log.info
     "Check that read state from second mockup equals read state from first" ;
   let* final_state =
     param_value param_read_final_state ~protocol mockup_client
   in

   let expected_amounts =
     compute_expected_amounts
       initial_state.bootstrap_accounts
       initial_state.protocol_constants
   in
   Check.(expected_amounts = final_state.bootstrap_accounts)
     (Check.list mockup_bootstrap_account_typ)
     ~error_msg:"The expected bootstrap accounts in mockup was %L, got %R"
     ~__LOC__ ;

   check_json_obj_equal_per_field
     initial_state.protocol_constants
     final_state.protocol_constants ;

   unit)
    protocols

let register ~protocols =
  test_rpc_list protocols ;
  test_same_transfer_twice protocols ;
  test_transfer_same_participants protocols ;
  test_transfer protocols ;
  test_empty_block_baking protocols ;
  test_simple_baking_event protocols ;
  test_multiple_baking protocols ;
  test_rpc_header_shell protocols ;
  test_origination_from_unrevealed_fees protocols ;
  test_multiple_transfers protocols ;
  test_storage_from_file protocols ;
  test_create_mockup_dir_exists_nonempty protocols ;
  test_retrieve_addresses protocols ;
  test_create_mockup_already_initialized protocols ;
  test_create_mockup_custom_constants protocols ;
  test_create_mockup_custom_bootstrap_accounts protocols ;
  test_transfer_bad_base_dir protocols ;
  test_config_show_mockup protocols ;
  test_config_show_mockup_fail protocols ;
  test_config_init_mockup protocols ;
  test_config_init_mockup_fail protocols ;
  test_transfer_rpc protocols ;
  test_proto_mix protocols ;
  test_create_mockup_config_show_init_roundtrip protocols

let register_global_constants ~protocols =
  test_register_global_constant_success protocols ;
  test_register_global_constant_failure protocols ;
  test_calling_contract_with_global_constant_success protocols ;
  test_calling_contract_with_global_constant_failure protocols ;
  test_originate_contract_with_global_constant_success protocols ;
  test_typechecking_and_normalization_work_with_constants protocols

let register_constant_migration ~migrate_from ~migrate_to =
  test_migration_constants ~migrate_from ~migrate_to

let register_protocol_independent () =
  test_migration_transfer () ;
  test_list_mockup_protocols ()
