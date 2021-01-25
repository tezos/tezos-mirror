(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
let test_rpc_list ~protocol =
  Test.register
    ~__FILE__
    ~title:(sf "(%s) (Mockup) RPC list" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "mockup"; "client"; "rpc"]
  @@ fun () ->
  let* client = Client.init_mockup ~protocol () in
  let* _ = Client.rpc_list client in
  Lwt.return_unit

(* Test.
   Call `tezos-client rpc /chains/<chain_id>/blocks/<block_id>/header/shell` and check that return code is 0.
 *)
let test_rpc_header_shell ~protocol =
  Test.register
    ~__FILE__
    ~title:(sf "(%s) (Mockup) RPC header/shell" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "mockup"; "client"; "rpc"]
  @@ fun () ->
  let* client = Client.init_mockup ~protocol () in
  let* _ = Client.shell_header client in
  Lwt.return_unit

let transfer_data =
  (Constant.bootstrap1.alias, Tez.one, Constant.bootstrap2.alias)

let test_balances_after_transfer giver amount receiver =
  let (giver_balance_before, giver_balance_after) = giver in
  let (receiver_balance_before, receiver_balance_after) = receiver in
  if not (giver_balance_after < giver_balance_before -. amount) then
    Test.fail
      "Invalid balance of giver after transfer: %f (before it was %f)"
      giver_balance_after
      giver_balance_before ;
  Log.info "Balance of giver after transfer is valid: %f" giver_balance_after ;
  let receiver_expected_after = receiver_balance_before +. amount in
  if receiver_balance_after <> receiver_expected_after then
    Test.fail
      "Invalid balance of receiver after transfer: %f (expected %f)"
      receiver_balance_after
      receiver_expected_after ;
  Log.info
    "Balance of receiver after transfer is valid: %f"
    receiver_balance_after

(* Test.
   Transfer some tz and check balance changes are as expected.
 *)
let test_transfer ~protocol =
  Test.register
    ~__FILE__
    ~title:(sf "(%s) (Mockup) Transfer" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "mockup"; "client"; "transfer"]
  @@ fun () ->
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
    (Tez.to_float amount)
    (receiver_balance_before, receiver_balance_after) ;
  return ()

let test_simple_baking_event ~protocol =
  Test.register
    ~__FILE__
    ~title:
      (sf "(%s) (Mockup) Transfer (asynchronous)" (Protocol.name protocol))
    ~tags:
      ["mockup"; "client"; "transfer"; Protocol.tag protocol; "asynchronous"]
  @@ fun () ->
  let (giver, amount, receiver) = transfer_data in
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  Log.info
    "Transferring %s from %s to %s"
    (Tez.to_string amount)
    giver
    receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  Log.info "Baking pending operations..." ;
  Client.bake_for ~key:giver client

let test_same_transfer_twice ~protocol =
  Test.register
    ~__FILE__
    ~title:
      (sf
         "(%s) (Mockup) Same transfer twice (asynchronous)"
         (Protocol.name protocol))
    ~tags:
      ["mockup"; "client"; "transfer"; Protocol.tag protocol; "asynchronous"]
  @@ fun () ->
  let (giver, amount, receiver) = transfer_data in
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  let mempool_file = Client.base_dir client // "mockup" // "mempool.json" in
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  let* mempool1 = read_file mempool_file in
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  let* mempool2 = read_file mempool_file in
  Log.info "Checking that mempool is unchanged" ;
  if mempool1 <> mempool2 then
    Test.fail
      "Expected mempool to stay unchanged\n--\n%s--\n %s"
      mempool1
      mempool2 ;
  return ()

let test_transfer_same_participants ~protocol =
  Test.register
    ~__FILE__
    ~title:
      (sf
         "(%s) (Mockup) Transfer same participants (asynchronous)"
         (Protocol.name protocol))
    ~tags:
      ["mockup"; "client"; "transfer"; Protocol.tag protocol; "asynchronous"]
  @@ fun () ->
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
  (* The next process is expected to fail *)
  let process = Client.spawn_transfer ~amount ~giver ~receiver client in
  let* status = Process.wait process in
  if status = Unix.WEXITED 0 then
    Test.fail "Last transfer was successful but was expected to fail ..." ;
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
    Test.fail "Expected thrashpool to have one operation." ;
  return ()

let test_multiple_baking ~protocol =
  Test.register
    ~__FILE__
    ~title:
      (sf
         "(%s) (Mockup) Multi transfer/multi baking (asynchronous)"
         (Protocol.name protocol))
    ~tags:
      ["mockup"; "client"; "transfer"; Protocol.tag protocol; "asynchronous"]
  @@ fun () ->
  let (alice, _amount, bob) = transfer_data and baker = "bootstrap3" in
  let* client =
    Client.init_mockup ~sync_mode:Client.Asynchronous ~protocol ()
  in
  Lwt_list.iteri_s
    (fun i amount ->
      let amount = Tez.of_int amount in
      let* () = Client.transfer ~amount ~giver:alice ~receiver:bob client in
      let* () = Client.transfer ~amount ~giver:bob ~receiver:alice client in
      let* () = Client.bake_for ~key:baker client in
      let* alice_balance = Client.get_balance_for ~account:alice client in
      let* bob_balance = Client.get_balance_for ~account:bob client in
      Log.info
        "%d. Balances\n  - Alice :: %f\n  - Bob ::   %f"
        i
        alice_balance
        bob_balance ;
      if alice_balance <> bob_balance then
        Test.fail
          "Unexpected balances for Alice (%f) and Bob (%f). They should be \
           equal."
          alice_balance
          bob_balance ;
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
    Client.list_mockup_protocols transient
  in
  (* Find all registered mockup protocols which declare a next protocol *)
  let result =
    List.filter_map
      (fun (protocol : Protocol.t) ->
        match Protocol.next_protocol protocol with
        | None ->
            None
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
          | [] ->
              Test.fail "No protocol can be tested for migration!"
          | (protocol, next_protocol) :: _ ->
              perform_migration
                ~protocol
                ~next_protocol
                ~next_constants:Protocol.default_constants
                ~pre_migration
                ~post_migration )
      | Some (protocol, next_protocol) ->
          perform_migration
            ~protocol
            ~next_protocol
            ~next_constants:Protocol.default_constants
            ~pre_migration
            ~post_migration)

let test_migration_transfer ?migration_spec () =
  let (giver, amount, receiver) = transfer_data in
  test_migration
    ?migration_spec
    ~pre_migration:(fun client ->
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
      let* giver_balance_after =
        Client.get_balance_for ~account:giver client
      in
      let* receiver_balance_after =
        Client.get_balance_for ~account:receiver client
      in
      test_balances_after_transfer
        (giver_balance_before, giver_balance_after)
        (Tez.to_float amount)
        (receiver_balance_before, receiver_balance_after) ;
      return ())
    ~info:"transfer"
    ()

(* Check constants equality between that obtained by directly initializing
   a mockup context at alpha and that obtained by migrating from
   alpha~1 to alpha *)
let test_migration_constants () =
  Test.register
    ~__FILE__
    ~title:(sf "check constants consistency after migration")
    ~tags:["mockup"; "migration"]
    (fun () ->
      let alpha_pred =
        let predecessor_opt = Protocol.previous_protocol Protocol.Alpha in
        match predecessor_opt with
        | None ->
            Test.fail "Could not find predecessor protocol to Alpha"
        | Some proto ->
            proto
      in
      let constants_path =
        ["chains"; "main"; "blocks"; "head"; "context"; "constants"]
      in
      let* client_alpha =
        Client.init_mockup
          ~constants:Protocol.Constants_mainnet
          ~protocol:Protocol.Alpha
          ()
      in
      let* const_alpha = Client.(rpc GET constants_path client_alpha) in
      let* const_alpha_migrated =
        perform_migration
          ~protocol:alpha_pred
          ~next_protocol:Protocol.Alpha
          ~next_constants:Protocol.Constants_mainnet
          ~pre_migration:(fun _ -> return ())
          ~post_migration:(fun client () ->
            Client.(rpc GET constants_path client))
      in
      if const_alpha = const_alpha_migrated then return ()
      else (
        Log.error "constants (alpha):\n%s\n" (JSON.encode const_alpha) ;
        Log.error
          "constants (migrated from %s):\n%s\n"
          (Protocol.tag alpha_pred)
          (JSON.encode const_alpha_migrated) ;
        Test.fail "Protocol constants mismatch" ))

let register protocol =
  test_rpc_list ~protocol ;
  test_same_transfer_twice ~protocol ;
  test_transfer_same_participants ~protocol ;
  test_transfer ~protocol ;
  test_simple_baking_event ~protocol ;
  test_multiple_baking ~protocol ;
  test_rpc_header_shell ~protocol

let register_protocol_independent () =
  test_migration_transfer () ;
  test_migration_constants ()
