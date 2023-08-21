(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe  <gb.fefe@protonmail.com>                    *)
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
   Component:    Testnet dictator key
   Invocation:   dune exec tezt/tests/main.exe -- --file testnet_dictator.ml
   Subject:      This tests that, on testnets where a dictator key is setup,
                 the dictator is able to force a protocol migration.
*)

let bake node client =
  let* level = Node.get_level node in
  let* () =
    Client.bake_for
      ~keys:
        [
          Constant.bootstrap1.alias;
          Constant.bootstrap2.alias;
          Constant.bootstrap3.alias;
          Constant.bootstrap4.alias;
          Constant.bootstrap5.alias;
        ]
      ~minimal_fees:0
      ~minimal_nanotez_per_gas_unit:0
      ~minimal_nanotez_per_byte:0
      ~minimal_timestamp:true
      client
  in
  let* _i = Node.wait_for_level node (level + 1) in
  Lwt.return_unit

let bake_until_next_period node client =
  let* p = Voting.get_current_period client in
  List.fold_left
    (fun acc _ ->
      let* () = acc in
      bake node client)
    (return ())
    (Base.range 0 p.remaining)

let bake_until_migration_block node client =
  let* p = Voting.get_current_period client in
  List.fold_left
    (fun acc _ ->
      let* () = acc in
      bake node client)
    (return ())
    (Base.range 1 p.remaining)

type chain_id = Chain_id_mainnet | Chain_id_ghostnet

let all_chain_ids = [Chain_id_mainnet; Chain_id_ghostnet]

let string_of_chain_id = function
  | Chain_id_mainnet -> "mainnet"
  | Chain_id_ghostnet -> "ghostnet"

let init_with_dictator ~chain_id ~protocol =
  let patch_config, timestamp =
    match chain_id with
    | Chain_id_mainnet -> (None, None)
    | Chain_id_ghostnet ->
        ( Some (Node.Config_file.set_ghostnet_sandbox_network ()),
          Some
            (* Ghostnet was started at 2022-01-25 (as Ithacanet) and the
               default timestamp is one year ago. This ad-hoc case could
               be removed after 2023-01-25. *)
            (Client.At
               (Tezos_base.Time.System.of_notation_exn "2022-01-26T15:00:00Z"))
        )
  in
  let* node = Node.init ?patch_config [Synchronisation_threshold 0] in
  let* client = Client.init ~endpoint:(Node node) () in
  let* dictator = Client.gen_and_show_keys ~alias:"dictator" client in
  Log.info "Dictator key: %s." dictator.public_key_hash ;
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      [
        (["blocks_per_cycle"], `Int 4);
        (["cycles_per_voting_period"], `Int 1);
        (["nonce_revelation_threshold"], `Int 3);
        (["testnet_dictator"], `String dictator.public_key_hash);
      ]
  in
  let* () =
    Client.activate_protocol ~protocol ~parameter_file ?timestamp client
  in
  return (node, client, dictator)

let reveal_dictator_key node client dictator =
  let* _ =
    Operation.inject_transfer
      ~source:Constant.bootstrap1
      ~dest:dictator
      ~gas_limit:1500
      ~fee:0
      ~amount:1
      ~async:true
      client
  in
  let* () = bake node client in
  let* _ =
    Operation.inject_public_key_revelation
      client
      ~fee:0
      ~source:dictator
      ~async:true
  in
  bake node client

let register_test chain_id period =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf
         "testnet dictator (%s, %s)"
         (string_of_chain_id chain_id)
         (Voting.period_kind_to_string period))
    ~tags:
      [
        "amendment";
        string_of_chain_id chain_id;
        Voting.period_kind_to_string period;
      ]
    ~supports:(From_protocol 014)
  @@ fun protocol ->
  let* node, client, dictator = init_with_dictator ~chain_id ~protocol in
  let* () = reveal_dictator_key node client dictator in
  let may_test_forced_upgrade k =
    let* current_period = Voting.get_current_period client in
    if current_period.kind <> period then k ()
    else (
      (* Submitting a forced upgrades. *)
      Log.info
        "Submitting proposals %s for %s"
        Protocol.demo_counter_hash
        dictator.alias ;
      let* () =
        Client.submit_proposals
          ~expect_failure:(chain_id = Chain_id_mainnet)
          ~key:dictator.public_key_hash
          ~proto_hash:Protocol.demo_counter_hash
          ~force:true (* Dictator is not in voting listing. *)
          client
      in
      Log.info "Baking until the migration block..." ;
      let* () = bake_until_migration_block node client in
      Log.info
        "Checking that %s migration occurred..."
        (if chain_id = Chain_id_mainnet then "no" else "a forced") ;
      let expected_protocol =
        if chain_id = Chain_id_mainnet then Protocol.hash protocol
        else Protocol.demo_counter_hash
      in
      let* () =
        Voting.check_protocols client (Protocol.hash protocol, expected_protocol)
      in
      return ())
  in
  may_test_forced_upgrade @@ fun () ->
  Log.info "Making proposals..." ;
  let submit_proposal ~key proto_hash =
    Log.info "- submitting proposals %s for %s" proto_hash key.Account.alias ;
    Client.submit_proposals ~key:key.alias ~proto_hash client
  in
  let* () = submit_proposal ~key:Constant.bootstrap2 (Protocol.hash protocol) in
  let* () = submit_proposal ~key:Constant.bootstrap3 (Protocol.hash protocol) in
  let* () = bake_until_next_period node client in
  let* () =
    Voting.check_current_period
      client
      {
        index = 1;
        kind = Exploration;
        start_position = 4;
        position = 0;
        remaining = 3;
      }
  in
  may_test_forced_upgrade @@ fun () ->
  Log.info "Voting for Exploration..." ;
  let submit_ballot key ballot =
    Log.info "- submitting ballot for %s" key.Account.alias ;
    Client.submit_ballot
      ~key:key.alias
      ~proto_hash:(Protocol.hash protocol)
      ballot
      client
  in
  let* () = submit_ballot Constant.bootstrap1 Yay in
  let* () = submit_ballot Constant.bootstrap2 Yay in
  let* () = submit_ballot Constant.bootstrap3 Yay in
  let* () = submit_ballot Constant.bootstrap4 Yay in
  let* () = submit_ballot Constant.bootstrap5 Yay in
  let* () = bake_until_next_period node client in
  let* () =
    Voting.check_current_period
      client
      {
        index = 2;
        kind = Cooldown;
        start_position = 8;
        position = 0;
        remaining = 3;
      }
  in
  may_test_forced_upgrade @@ fun () ->
  Log.info "Skipping Cooldown period..." ;
  let* () = bake_until_next_period node client in
  let* () =
    Voting.check_current_period
      client
      {
        index = 3;
        kind = Promotion;
        start_position = 12;
        position = 0;
        remaining = 3;
      }
  in
  may_test_forced_upgrade @@ fun () ->
  Log.info "Voting for promotion..." ;
  let* () = submit_ballot Constant.bootstrap1 Yay in
  let* () = submit_ballot Constant.bootstrap2 Yay in
  let* () = submit_ballot Constant.bootstrap3 Yay in
  let* () = submit_ballot Constant.bootstrap4 Yay in
  let* () = submit_ballot Constant.bootstrap5 Yay in
  let* () = bake_until_next_period node client in
  let* () =
    Voting.check_current_period
      client
      {
        index = 4;
        kind = Adoption;
        start_position = 16;
        position = 0;
        remaining = 3;
      }
  in
  may_test_forced_upgrade @@ fun () -> return ()

let register ~protocols =
  List.iter
    (fun period ->
      List.iter
        (fun chain_id -> register_test chain_id period protocols)
        all_chain_ids)
    Voting.periods
