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
    (Tez.to_float amount)
    (receiver_balance_before, receiver_balance_after) ;
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
  Log.info
    "Transferring %s from %s to %s"
    (Tez.to_string amount)
    giver
    receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  Log.info "Baking pending operations..." ;
  Client.bake_for ~key:giver client

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
  let* () = Client.transfer ~amount ~giver ~receiver client in
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

let test_multiple_baking =
  Protocol.register_test
    ~__FILE__
    ~title:"(Mockup) Multi transfer/multi baking (asynchronous)"
    ~tags:["mockup"; "client"; "transfer"; "asynchronous"]
  @@ fun protocol ->
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

(** Check the dangling temp big maps cleanup performed at stitching from
    Florence to Granada leaves existing non-temp big maps unchanged.
    (and also check that dangling temp big maps are actually cleaned up)
    This test can be removed once that stitching code is removed.
*)
let test_granada_migration_temp_big_maps =
  let big_map_get_opt ~id ~key_hash client =
    Lwt.catch
      (fun () ->
        let* v = RPC.Big_maps.get ~id:(string_of_int id) ~key_hash client in
        Lwt.return_some v)
      (fun _exn -> Lwt.return_none)
  in
  let find_big_maps_with_key_1 client =
    (* There is no RPC to list all big maps, let's approximate the list of all
       big maps by iterating over -5 .. 5 and collecting only big maps that
       have key 1 *)
    let key1 =
      "expru2dKqDfZG8hu4wNGkiyunvq2hdSKuVYtcKta7BWP6Q18oNxKjS"
      (* result of [tezos-client hash data 1 of type int] *)
    in
    let key2 =
      "expruDuAZnFKqmLoisJqUGqrNzXTvw7PJM2rYk97JErM5FHCerQqgn"
      (* result of [tezos-client hash data 2 of type int] *)
    in
    let min_id = -5 in
    let max_id = 5 in
    let rec aux acc id =
      if id < min_id then return acc
      else
        let* val1_opt = big_map_get_opt ~id ~key_hash:key1 client in
        match val1_opt with
        | None ->
            aux acc (pred id)
        | Some val1 ->
            let* val2_opt = big_map_get_opt ~id ~key_hash:key2 client in
            aux ((id, val1, val2_opt) :: acc) (pred id)
    in
    aux [] max_id
  in
  let pre_migration client =
    let* _contract_address =
      Client.originate_contract
        ~alias:"temp_big_maps"
        ~amount:Tez.zero
        ~src:"bootstrap1"
        ~prg:"file:./tezt/tests/contracts/proto_alpha/temp_big_maps.tz"
        ~init:"{ Elt 1 3; Elt 2 4 }"
        ~burn_cap:(Tez.of_int 2)
        client
    in
    let* () = Client.bake_for ~key:"bootstrap1" client in
    let param =
      "Pair (Left True) 1"
      (* create a fresh big map containing { 1 -> 2 } and pass it as parameter *)
    in
    let* () =
      Client.transfer
        ~amount:Tez.zero
        ~giver:"bootstrap1"
        ~receiver:"temp_big_maps"
        ~param
        client
    in
    let* () = Client.bake_for ~key:"bootstrap1" client in
    let* big_maps = find_big_maps_with_key_1 client in
    if List.length big_maps <= 0 then
      Test.fail
        "No big maps found after initializing the contract, did we use the \
         correct RPC?" ;
    let (non_temp_big_maps, temp_big_maps) =
      List.partition (fun (id, _, _) -> id >= 0) big_maps
    in
    if List.length temp_big_maps <= 0 then
      Test.fail "Dangling temporary big map expected in Edo and Florence" ;
    Lwt.return non_temp_big_maps
  in
  let post_migration client pre_big_maps =
    let* post_big_maps = find_big_maps_with_key_1 client in
    (* Liquidity baking big maps introduced at Granada stitching won't be
       returned by [find_big_maps_with_key_1] *)
    if pre_big_maps <> post_big_maps then
      Test.fail
        "Big maps have changed, either dangling temporary ones were not \
         cleaned or non-temporary ones have changed" ;
    Lwt.return_unit
  in
  fun () ->
    Test.register
      ~__FILE__
      ~title:"(Florence -> Alpha) dangling temp big maps cleanup"
      ~tags:["mockup"; "migration"]
      (fun () ->
        perform_migration
          ~protocol:Protocol.Florence
          ~next_protocol:Protocol.Alpha
          ~next_constants:Protocol.default_constants
          ~pre_migration
          ~post_migration)

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
        Test.fail "Protocol constants mismatch" ))

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
        identity = "";
        alias = "originator";
        secret =
          "unencrypted:edskRiUZpqYpyBCUQmhpfCmzHfYahfiMqkKb9AaYKaEggXKaEKVUWPBz6RkwabTmLHXajbpiytRdMJb4v4f4T8zN9t6QCHLTjy";
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

let register ~protocols =
  test_rpc_list ~protocols ;
  test_same_transfer_twice ~protocols ;
  test_transfer_same_participants ~protocols ;
  test_transfer ~protocols ;
  test_simple_baking_event ~protocols ;
  test_multiple_baking ~protocols ;
  test_rpc_header_shell ~protocols ;
  test_origination_from_unrevealed_fees ~protocols

let register_constant_migration ~migrate_from ~migrate_to =
  test_migration_constants ~migrate_from ~migrate_to

let register_protocol_independent () =
  test_migration_transfer () ;
  test_granada_migration_temp_big_maps ()
