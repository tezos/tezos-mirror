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
   Subject: Unexhaustive tests of the client's --mode mockup. Unexhaustive,
            because most tests of the mockup are written with the python
            framework for now. It was important, though, to provide the
            mockup's API in tezt; for other tests that use the mockup.
  *)

(* Test.
   Call `tezos-client rpc list` and check that return code is 0.
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
   Call `tezos-client rpc /chains/<chain_id>/blocks/<block_id>/header/shell` and check that return code is 0.
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

let test_balances_after_transfer giver amount receiver =
  let (giver_balance_before, giver_balance_after) = giver in
  let (receiver_balance_before, receiver_balance_after) = receiver in
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
  let (giver, amount, receiver) = transfer_data in
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
  test_balances_after_transfer
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
  let (src, _, _) = transfer_data in
  let* client = Client.init_mockup ~protocol () in
  let value = "999" in
  let burn_cap = Some (Tez.of_int 1) in
  let* _ = Client.register_global_constant ~src ~value ?burn_cap client in
  let script = "file:./tezt/tests/contracts/proto_alpha/constant_999.tz" in
  let storage = "0" in
  let input = "Unit" in
  let* result = Client.run_script ~prg:script ~storage ~input client in
  let result = String.trim result in
  Log.info "Contract with constant output storage %s" result ;
  if result = value then return ()
  else Test.fail "Expected storage '%s' but got '%s'" value result

let test_calling_contract_with_global_constant_failure =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Calling a contract with a global constant failure"
    ~tags:["mockup"; "client"; "global_constant"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let script = "file:./tezt/tests/contracts/proto_alpha/constant_999.tz" in
  let storage = "0" in
  let input = "Unit" in
  let process = Client.spawn_run_script ~prg:script ~storage ~input client in
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
  let (src, _, _) = transfer_data in
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
  let (src, _, _) = transfer_data in
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
  let (src, _, _) = transfer_data in
  let* client = Client.init_mockup ~protocol () in
  let value = "999" in
  let burn_cap = Some (Tez.of_int 1) in
  let* _ = Client.register_global_constant ~src ~value ?burn_cap client in
  let* result =
    Client.originate_contract
      ~alias:"with_global_constant"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/constant_999.tz"
      ~init:"0"
      ~burn_cap:(Tez.of_int 2)
      client
  in
  Log.info "result %s" result ;
  return ()

let test_typechecking_and_normalization_work_with_constants =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Typechecking and normalization work with constants"
    ~tags:["mockup"; "client"; "global_constant"]
  @@ fun protocol ->
  let (src, _, _) = transfer_data in
  let* client = Client.init_mockup ~protocol () in
  (* Register the type *)
  let value = "unit" in
  let burn_cap = Some (Tez.of_int 1) in
  let* _ = Client.register_global_constant ~src ~value ?burn_cap client in
  (* Register the value *)
  let value = "Unit" in
  let* _ = Client.register_global_constant ~src ~value ?burn_cap client in
  let script = "file:./tezt/tests/contracts/proto_alpha/constant_unit.tz" in
  let* _ = Client.normalize_script ~script client in
  let* _ = Client.typecheck_script ~script client in
  return ()

let test_simple_baking_event =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Transfer (asynchronous)"
    ~tags:["mockup"; "client"; "transfer"; "asynchronous"]
  @@ fun protocol ->
  let (giver, amount, receiver) = transfer_data in
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
  let (giver, amount, receiver) = transfer_data in
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  let mempool_file = Client.base_dir client // "mockup" // "mempool.json" in
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  let* mempool1 = read_file mempool_file in
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = transfer_expected_to_fail ~amount ~giver ~receiver client in
  let* mempool2 = read_file mempool_file in
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
  let (giver, amount, receiver) = transfer_data in
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  let base_dir = Client.base_dir client in
  let mempool_file = base_dir // "mockup" // "mempool.json" in
  let thrashpool_file = base_dir // "mockup" // "trashpool.json" in
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  let* mempool1 = read_file mempool_file in
  let amount = Tez.(amount + one) in
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = transfer_expected_to_fail ~amount ~giver ~receiver client in
  let* mempool2 = read_file mempool_file in
  Log.info "Checking that mempool is unchanged" ;
  if mempool1 <> mempool2 then
    Test.fail
      "Expected mempool to stay unchanged\n--\n%s\n--\n %s"
      mempool1
      mempool2 ;
  Log.info
    "Checking that last operation was discarded into a newly created trashpool" ;
  let* str = read_file thrashpool_file in
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
  let (alice, _amount, bob) = transfer_data and baker = "bootstrap3" in
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
  let (giver, amount, receiver) = ("alice", Tez.of_int 1, "bob") in
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
      test_balances_after_transfer
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

let test_migration_ticket_balance ~migrate_from ~migrate_to =
  Regression.register
    ~__FILE__
    ~title:
      (sf
         "(%s -> %s) ticket balance migration"
         (Protocol.name migrate_from)
         (Protocol.name migrate_to))
    ~tags:["mockup"; "migration"; "tickets"]
    ~output_file:("tickets" // "ticket_balance")
    (fun () ->
      let* context_json =
        perform_migration
          ~protocol:migrate_from
          ~next_protocol:migrate_to
          ~next_constants:Protocol.Constants_mainnet
          ~pre_migration:(fun client ->
            let* _ =
              Client.originate_contract
                ~alias:"with_tickets"
                ~amount:Tez.zero
                ~src:"bootstrap1"
                ~prg:
                  "file:./tezt/tests/contracts/proto_current_mainnet/tickets.tz"
                ~init:"{}"
                ~burn_cap:(Tez.of_int 2)
                client
            in
            Client.transfer
              ~amount:(Tez.of_int 0)
              ~giver:"bootstrap1"
              ~receiver:"with_tickets"
              ~burn_cap:(Tez.of_int 1)
              client)
          ~post_migration:(fun client _ ->
            let context_file =
              Client.base_dir client // "mockup" // "context.json"
            in
            let json = JSON.parse_file context_file in
            let json =
              JSON.(
                json |-> "context" |-> "context" |=> 0 |=> 1 |> as_list
                |> List.find (fun item ->
                       item |=> 0 |> as_string = "ticket_balance"))
            in
            return json)
      in
      Regression.capture (JSON.encode context_json) ;
      return ())

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
    Client.originate_contract
      ~wait:"none"
      ~alias:"contract_name"
      ~amount:Tez.zero
      ~src:"originator"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/str_id.tz"
      ~init:"None"
      ~burn_cap:(Tez.of_int 20)
      client
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
  Client.multiple_transfers ~giver:"bootstrap2" ~json_batch:file client

let test_empty_block_baking =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Transfer (empty, asynchronous)"
    ~tags:["mockup"; "client"; "empty"; "bake_for"; "asynchronous"]
  @@ fun protocol ->
  let (giver, _amount, _receiver) = transfer_data in
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  Log.info "Baking pending operations..." ;
  Client.bake_for ~keys:[giver] client

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
  test_multiple_transfers protocols

let register_global_constants ~protocols =
  test_register_global_constant_success protocols ;
  test_register_global_constant_failure protocols ;
  test_calling_contract_with_global_constant_success protocols ;
  test_calling_contract_with_global_constant_failure protocols ;
  test_originate_contract_with_global_constant_success protocols ;
  test_typechecking_and_normalization_work_with_constants protocols

let register_constant_migration ~migrate_from ~migrate_to =
  test_migration_constants ~migrate_from ~migrate_to

let register_migration_ticket_balance ~migrate_from ~migrate_to =
  test_migration_ticket_balance ~migrate_from ~migrate_to

let register_protocol_independent () = test_migration_transfer ()
