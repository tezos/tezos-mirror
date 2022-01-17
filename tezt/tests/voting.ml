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
   Component:    Amendment process
   Invocation:   dune exec tezt/tests/main.exe -- --file amendment.ml
   Subject:      Test the voting process from proposal to adoption.

   This test activates a protocol [from_protocol]. Then it uses the
   client's [submit ballot] command on a protocol [to_protocol] that will
   win the election, and on other protocols [loser_protocols].
   Then the [bake for] command is used to move to the exploration period.
   Another vote using [submit ballot] is made and the [bake for] command
   is used to move to the cooldown period, then to the promotion period.
   Another vote using [submit ballot] is made and the [bake for] command
   is used to move to the adoption period.

   RPCs are used regularly to check the current level and the current period
   information match what we expect.

   Optionally, [from_protocol] can be an injected test protocol.
   In this scenario, another node is started at the end of the adoption period.
   This is to check that injected protocols are propagated to other nodes
   after the migration. *)

(* Protocol to inject when testing injection. *)
let test_proto_dir = "src/bin_client/test/proto_test_injection"

(* Files that are to be copied from [test_proto_dir]. *)
let test_proto_files = ["main.ml"; "main.mli"; "TEZOS_PROTOCOL"]

type period_kind = Proposal | Exploration | Adoption | Cooldown | Promotion

let periods = [Proposal; Exploration; Adoption; Cooldown; Promotion]

let period_kind_to_string = function
  | Proposal -> "proposal"
  | Exploration -> "exploration"
  | Adoption -> "adoption"
  | Cooldown -> "cooldown"
  | Promotion -> "promotion"

let period_kind_to_int = function
  | Proposal -> 0
  | Exploration -> 1
  | Adoption -> 2
  | Cooldown -> 3
  | Promotion -> 4

let period_kind_of_string = function
  | "proposal" -> Proposal
  | "exploration" -> Exploration
  | "adoption" -> Adoption
  | "cooldown" -> Cooldown
  | "promotion" -> Promotion
  | p ->
      Test.fail
        "Unexpected period kind %s, expected one of: %s"
        p
        (String.concat ", " (List.map period_kind_to_string periods))

let period_kind_type : period_kind Check.typ =
  Check.convert period_kind_to_int Check.int

type period = {
  index : int;
  kind : period_kind;
  start_position : int;
  position : int;
  remaining : int;
}

let period_type : period Check.typ =
  Check.convert
    (fun {index; kind; start_position; position; remaining} ->
      (index, kind, start_position, position, remaining))
    Check.(tuple5 int period_kind_type int int int)

let decode_period json =
  let index = JSON.(json |-> "voting_period" |-> "index" |> as_int) in
  let kind =
    JSON.(
      json |-> "voting_period" |-> "kind" |> as_string |> period_kind_of_string)
  in
  let start_position =
    JSON.(json |-> "voting_period" |-> "start_position" |> as_int)
  in
  let position = JSON.(json |-> "position" |> as_int) in
  let remaining = JSON.(json |-> "remaining" |> as_int) in
  {index; kind; start_position; position; remaining}

let get_current_period client =
  let* json = RPC.Votes.get_current_period client in
  return (decode_period json)

let get_successor_period client =
  let* json = RPC.Votes.get_successor_period client in
  return (decode_period json)

let check_current_period client expected_period =
  let* period = get_current_period client in
  Check.((period = expected_period) period_type)
    ~error_msg:"expected current_period = %R, got %L" ;
  unit

let check_successor_period client expected_period =
  let* period = get_successor_period client in
  Check.((period = expected_period) period_type)
    ~error_msg:"expected successor_period = %R, got %L" ;
  unit

type level = {
  level : int;
  level_position : int;
  cycle : int;
  cycle_position : int;
  expected_commitment : bool;
}

let level_type : level Check.typ =
  Check.convert
    (fun {level; level_position; cycle; cycle_position; expected_commitment} ->
      (level, level_position, cycle, cycle_position, expected_commitment))
    Check.(tuple5 int int int int bool)

let decode_level json =
  let level = JSON.(json |-> "level" |> as_int) in
  let level_position = JSON.(json |-> "level_position" |> as_int) in
  let cycle = JSON.(json |-> "cycle" |> as_int) in
  let cycle_position = JSON.(json |-> "cycle_position" |> as_int) in
  let expected_commitment = JSON.(json |-> "expected_commitment" |> as_bool) in
  {level; level_position; cycle; cycle_position; expected_commitment}

let get_current_level client =
  let* json = RPC.get_current_level client in
  return (decode_level json)

let check_current_level client expected_level =
  let* level = get_current_level client in
  Check.((level = expected_level) level_type)
    ~error_msg:"expected current_period = %R, got %L" ;
  unit

let get_proposals client =
  let* proposals = RPC.Votes.get_proposals client in
  JSON.as_list proposals |> List.map JSON.as_list
  |> List.map (function
         | [hash; _] -> JSON.as_string hash
         | _ ->
             Test.fail
               "invalid proposal in JSON response: expected a list with 2 items")
  |> return

let get_current_proposal client =
  let* proposal = RPC.Votes.get_current_proposal client in
  if JSON.is_null proposal then return None
  else return (Some JSON.(proposal |> as_string))

let check_current_proposal client expected_proposal_hash =
  let* current_proposal = get_current_proposal client in
  Check.((current_proposal = Some expected_proposal_hash) (option string))
    ~error_msg:"expected current_proposal = %R, got %L" ;
  unit

let get_protocols client =
  let* block = RPC.get_block_metadata client in
  return
    ( JSON.(block |-> "protocol" |> as_string),
      JSON.(block |-> "next_protocol" |> as_string) )

let check_protocols client expected_protocols =
  let* protocols = get_protocols client in
  Check.((protocols = expected_protocols) (tuple2 string string))
    ~error_msg:"expected (protocol, next_protocol) = %R, got %L" ;
  unit

let check_listings_not_empty client =
  let* listings = RPC.Votes.get_listings client in
  match JSON.as_list listings with
  | [] ->
      Test.fail "Expected GET .../votes/listing RPC to return a non-empty list"
  | _ :: _ -> unit

type target_protocol = Known of Protocol.t | Injected_test

let target_protocol_tag = function
  | Known protocol -> Protocol.tag protocol
  | Injected_test -> "injected_test"

let register ~from_protocol ~(to_protocol : target_protocol) ~loser_protocols =
  Test.register
    ~__FILE__
    ~title:
      (sf
         "amendment: %s -> %s (losers: %s)"
         (Protocol.tag from_protocol)
         (target_protocol_tag to_protocol)
         (String.concat ", " (List.map Protocol.tag loser_protocols)))
    ~tags:
      ("amendment"
       ::
       ("from_" ^ Protocol.tag from_protocol)
       ::
       ("to_" ^ target_protocol_tag to_protocol)
       :: List.map (fun p -> "loser_" ^ Protocol.tag p) loser_protocols
      @ [
          (match to_protocol with
          | Known _ -> "known"
          | Injected_test -> "injected");
        ])
  @@ fun () ->
  (* Prepare protocol parameters such that voting periods are shorter
     to make the test run faster. *)
  let* parameter_file =
    (* Note: default [blocks_per_cycle] is already 8.
       We explicitely set it here anyway to make it clear that voting works
       with [blocks_per_voting_period < blocks_per_cycle]. *)
    (* Note: the test fails with [blocks_per_voting_period = 2]
       and [blocks_per_voting_period = 3], and enters an infinite loop
       with [blocks_per_voting_period = 1] and [blocks_per_voting_period = 0]. *)
    Protocol.write_parameter_file
      ~base:(Right (from_protocol, None))
      [
        (["blocks_per_cycle"], Some "8");
        (["blocks_per_voting_period"], Some "4");
      ]
  in
  (* Start a node and activate [from_protocol]. *)
  let* node = Node.init [Synchronisation_threshold 0] in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () =
    Client.activate_protocol ~protocol:from_protocol ~parameter_file client
  in
  let* () =
    check_current_level
      client
      {
        level = 1;
        level_position = 0;
        cycle = 0;
        cycle_position = 0;
        expected_commitment = false;
      }
  in
  let* () =
    check_current_period
      client
      {
        index = 0;
        kind = Proposal;
        start_position = 0;
        position = 0;
        remaining = 3;
      }
  in
  let* () =
    check_successor_period
      client
      {
        index = 0;
        kind = Proposal;
        start_position = 0;
        position = 1;
        remaining = 2;
      }
  in
  Log.info "Current period: proposal." ;
  (* Bake while performing more RPC checks. *)
  let* () = Client.bake_for client in
  let* () = Client.bake_for client in
  let* () =
    check_current_level
      client
      {
        level = 3;
        level_position = 2;
        cycle = 0;
        cycle_position = 2;
        expected_commitment = false;
      }
  in
  let* () =
    check_current_period
      client
      {
        index = 0;
        kind = Proposal;
        start_position = 0;
        position = 2;
        remaining = 1;
      }
  in
  let* () = Client.bake_for client in
  let* () =
    check_current_period
      client
      {
        index = 0;
        kind = Proposal;
        start_position = 0;
        position = 3;
        remaining = 0;
      }
  in
  let* proposals = get_proposals client in
  Check.((proposals = []) (list string))
    ~error_msg:"expected proposal list to be empty, got %L" ;
  let* () = check_listings_not_empty client in
  let* period = Client.show_voting_period client in
  Check.((period = "proposal") string)
    ~error_msg:"expected tezos-client show voting period to return %R, got %L" ;
  (* Inject test protocol, or use known protocol. *)
  let* to_protocol_hash =
    match to_protocol with
    | Known protocol -> return (Protocol.hash protocol)
    | Injected_test ->
        let protocol_path = Temp.dir "test_proto" in
        let* () =
          Process.run
            "cp"
            (List.map
               (fun filename -> test_proto_dir // filename)
               test_proto_files
            @ [protocol_path])
        in
        let* protocols_before = Client.Admin.list_protocols client in
        let* test_proto_hash =
          Client.Admin.inject_protocol client ~protocol_path
        in
        let* protocols_after = Client.Admin.list_protocols client in
        if List.mem test_proto_hash protocols_before then
          (* Note that [inject_protocol] normally fails if the protocol is already known.
             So this check is only here in case the behavior of [inject_protocol] changes,
             to prevent running this test with an already-known protocol. *)
          Test.fail
            "Test protocol %s was already known, I can't test injection with \
             it."
            test_proto_hash ;
        if not (List.mem test_proto_hash protocols_after) then
          Test.fail
            "Test protocol %s is not known even though it was injected \
             successfully."
            test_proto_hash ;
        Log.info "Injected: %s" test_proto_hash ;
        return test_proto_hash
  in
  (* Submit proposals: [to_protocol] and [loser_protocols]. *)
  let* () =
    Client.submit_proposals
      ~proto_hashes:(to_protocol_hash :: List.map Protocol.hash loser_protocols)
      client
  in
  (* Bake and check that the proposals are registered. *)
  let* () = Client.bake_for client in
  let* proposals = get_proposals client in
  let expected_proposals =
    to_protocol_hash :: List.map Protocol.hash loser_protocols
  in
  let sort = List.sort String.compare in
  Check.((sort proposals = sort expected_proposals) (list string))
    ~error_msg:"expected proposals = %R, got %L" ;
  (* Have another baker vote for [to_protocol] to make [to_protocol] win. *)
  let* () =
    Client.submit_proposals
      ~key:Constant.bootstrap2.alias
      ~proto_hash:to_protocol_hash
      client
  in
  (* Bake and check that the proposals are still registered. *)
  let* () = Client.bake_for client in
  let* proposals = get_proposals client in
  Check.((sort proposals = sort expected_proposals) (list string))
    ~error_msg:"expected proposals = %R, got %L" ;
  (* Bake until exploration period while checking RPC behavior. *)
  let* () = Client.bake_for client in
  let* () = Client.bake_for client in
  let* () =
    check_current_level
      client
      {
        level = 8;
        level_position = 7;
        cycle = 0;
        cycle_position = 7;
        expected_commitment = true;
      }
  in
  let* () =
    check_current_period
      client
      {
        index = 1;
        kind = Proposal;
        start_position = 4;
        position = 3;
        remaining = 0;
      }
  in
  let* () = check_listings_not_empty client in
  let* () = check_current_proposal client to_protocol_hash in
  let* () = Client.bake_for client in
  let* () =
    check_current_period
      client
      {
        index = 2;
        kind = Exploration;
        start_position = 8;
        position = 0;
        remaining = 3;
      }
  in
  let* () = check_current_proposal client to_protocol_hash in
  Log.info
    "Current period: exploration, with current proposal: %s."
    (target_protocol_tag to_protocol) ;
  (* Vote for [to_protocol]. *)
  let vote (account : Account.key) ballot =
    Client.submit_ballot
      ~key:account.alias
      ~proto_hash:to_protocol_hash
      ballot
      client
  in
  let* () = vote Constant.bootstrap1 Yay in
  let* () = vote Constant.bootstrap2 Yay in
  let* () = vote Constant.bootstrap3 Yay in
  (* Bake until cooldown period while checking RPCs. *)
  let* () = Client.bake_for client in
  let* () =
    check_current_level
      client
      {
        level = 10;
        level_position = 9;
        cycle = 1;
        cycle_position = 1;
        expected_commitment = false;
      }
  in
  let* () =
    check_current_period
      client
      {
        index = 2;
        kind = Exploration;
        start_position = 8;
        position = 1;
        remaining = 2;
      }
  in
  let* () = Client.bake_for client in
  let* () = Client.bake_for client in
  let* () =
    check_current_period
      client
      {
        index = 2;
        kind = Exploration;
        start_position = 8;
        position = 3;
        remaining = 0;
      }
  in
  let* () = Client.bake_for client in
  let* () =
    check_current_period
      client
      {
        index = 3;
        kind = Cooldown;
        start_position = 12;
        position = 0;
        remaining = 3;
      }
  in
  (* Check current proposal again now that we switched to the cooldown period. *)
  let* current_proposal = get_current_proposal client in
  Check.((current_proposal = Some to_protocol_hash) (option string))
    ~error_msg:"expected current_proposal = %R, got %L" ;
  Log.info
    "Current period: cooldown, with current proposal: %s."
    (target_protocol_tag to_protocol) ;
  (* Bake until promotion period while checking RPCs. *)
  let* () = Client.bake_for client in
  let* () =
    check_current_level
      client
      {
        level = 14;
        level_position = 13;
        cycle = 1;
        cycle_position = 5;
        expected_commitment = false;
      }
  in
  let* () =
    check_current_period
      client
      {
        index = 3;
        kind = Cooldown;
        start_position = 12;
        position = 1;
        remaining = 2;
      }
  in
  let* () = Client.bake_for client in
  let* () = Client.bake_for client in
  let* () =
    check_current_period
      client
      {
        index = 3;
        kind = Cooldown;
        start_position = 12;
        position = 3;
        remaining = 0;
      }
  in
  let* () = Client.bake_for client in
  let* () =
    check_current_period
      client
      {
        index = 4;
        kind = Promotion;
        start_position = 16;
        position = 0;
        remaining = 3;
      }
  in
  (* Check current proposal again now that we switched to the promotion period. *)
  let* current_proposal = get_current_proposal client in
  Check.((current_proposal = Some to_protocol_hash) (option string))
    ~error_msg:"expected current_proposal = %R, got %L" ;
  Log.info
    "Current period: promotion, with current proposal: %s."
    (target_protocol_tag to_protocol) ;
  (* Vote for [to_protocol]. Put in a Nay for good measure (we need more Yay as a result). *)
  let* () = vote Constant.bootstrap1 Yay in
  let* () = vote Constant.bootstrap2 Yay in
  let* () = vote Constant.bootstrap3 Yay in
  let* () = vote Constant.bootstrap4 Yay in
  let* () = vote Constant.bootstrap5 Nay in
  (* Bake until adoption period while checking RPCs. *)
  let* () = Client.bake_for client in
  let* () =
    check_current_level
      client
      {
        level = 18;
        level_position = 17;
        cycle = 2;
        cycle_position = 1;
        expected_commitment = false;
      }
  in
  let* () =
    check_current_period
      client
      {
        index = 4;
        kind = Promotion;
        start_position = 16;
        position = 1;
        remaining = 2;
      }
  in
  let* () = Client.bake_for client in
  let* () = Client.bake_for client in
  let* () =
    check_current_period
      client
      {
        index = 4;
        kind = Promotion;
        start_position = 16;
        position = 3;
        remaining = 0;
      }
  in
  let* () = Client.bake_for client in
  let* () =
    check_current_period
      client
      {
        index = 5;
        kind = Adoption;
        start_position = 20;
        position = 0;
        remaining = 3;
      }
  in
  (* Check current proposal again now that we switched to the adoption period. *)
  let* current_proposal = get_current_proposal client in
  Check.((current_proposal = Some to_protocol_hash) (option string))
    ~error_msg:"expected current_proposal = %R, got %L" ;
  Log.info
    "Current period: adoption, with current proposal: %s."
    (target_protocol_tag to_protocol) ;
  (* Bake until next_protocol changes. *)
  let* () = Client.bake_for client in
  let* () =
    check_current_level
      client
      {
        level = 22;
        level_position = 21;
        cycle = 2;
        cycle_position = 5;
        expected_commitment = false;
      }
  in
  let* () =
    check_current_period
      client
      {
        index = 5;
        kind = Adoption;
        start_position = 20;
        position = 1;
        remaining = 2;
      }
  in
  let* () = Client.bake_for client in
  let* () =
    check_protocols
      client
      (Protocol.hash from_protocol, Protocol.hash from_protocol)
  in
  Log.info "Baking last block of adoption period..." ;
  let* () = Client.bake_for client in
  match to_protocol with
  | Injected_test ->
      (* The test protocol has no amendment process and no baker. We can't go further.
         Instead, we check that another node can fetch the injected protocol. *)
      Log.info "Creating a second node..." ;
      let* node2 = Node.init [Synchronisation_threshold 1] in
      let check_event_fetching_protocol =
        (* Example:
           fetching_protocol.v0 = {
             "hash": "PtW12iP7VWYi4jz24Rwn5wrm5wtZkXuGYbEDC6U4RU4RV3ekWSe",
             "source": null
           } *)
        Node.wait_for node2 "fetching_protocol.v0" @@ fun json ->
        let hash = JSON.(json |-> "hash" |> as_string) in
        if hash = Protocol.hash from_protocol then
          (* Continue waiting for the other protocol. *)
          None
        else (
          Check.((hash = to_protocol_hash) string)
            ~error_msg:"expected node2 to fetch protocol %R, got %L" ;
          Log.info "Node 2 is fetching the injected protocol." ;
          Some ())
      in
      let check_event_new_protocol_initialisation =
        (* Example:
           new_protocol_initialisation.v0 =
             "PtW12iP7VWYi4jz24Rwn5wrm5wtZkXuGYbEDC6U4RU4RV3ekWSe" *)
        Node.wait_for node2 "new_protocol_initialisation.v0" @@ fun json ->
        let hash = JSON.as_string json in
        if hash = Protocol.hash from_protocol then
          (* Continue waiting for the other protocol. *)
          None
        else (
          Check.((hash = to_protocol_hash) string)
            ~error_msg:"expected node2 to initialize protocol %R, got %L" ;
          Log.info "Node 2 initialized the injected protocol." ;
          Some ())
      in
      let check_event_update_protocol_table =
        (* Example:
           update_protocol_table.v0 = {
             "proto_hash": "PtW12iP7VWYi4jz24Rwn5wrm5wtZkXuGYbEDC6U4RU4RV3ekWSe",
             "proto_level": 2,
             "block_hash": "BMD8dVEivzWp3zu4mFSWnJG98g61kus3t7oJqK5L2d9k71PxkQi",
             "block_level": 24
           } *)
        Node.wait_for node2 "update_protocol_table.v0" @@ fun json ->
        let proto_hash = JSON.(json |-> "proto_hash" |> as_string) in
        if proto_hash = Protocol.hash from_protocol then
          (* Continue waiting for the other protocol. *)
          None
        else
          let proto_level = JSON.(json |-> "proto_level" |> as_int) in
          Check.(
            ((proto_hash, proto_level) = (to_protocol_hash, 2))
              (tuple2 string int))
            ~error_msg:
              "expected node 2 to update its protocol table with %R, got %L" ;
          Log.info
            "Node 2 updated its protocol table with the injected protocol." ;
          Some ()
      in
      let* () = Client.Admin.connect_address ~peer:node2 client in
      Log.info
        "Node 2 initialized and connected to node1, waiting for it to sync..." ;
      let* level = Node.wait_for_level node2 (Node.get_level node) in
      Log.info "Both nodes are at level %d." level ;
      let all_checks =
        let* () = check_event_fetching_protocol
        and* () = check_event_new_protocol_initialisation
        and* () = check_event_update_protocol_table in
        unit
      in
      let timeout =
        (* We expect those three events to trigger *before* node 2 reaches the level
           of node 1. So a small timeout of 1 minute is fine.
           No timeout would mean that we could wait forever if the events are
           not emitted by the node or not received correctly,
           and a timeout of 0 could cause the Lwt scheduler to choose to wake up
           the timeout first. *)
        let* () = Lwt_unix.sleep 60. in
        Test.fail
          "timeout while waiting for fetching_protocol, \
           new_protocol_initialisation, or update_protocol_table"
      in
      let* () = Lwt.pick [all_checks; timeout] in
      unit
  | Known _ ->
      let* () =
        check_current_period
          client
          {
            index = 5;
            kind = Adoption;
            start_position = 20;
            position = 3;
            remaining = 0;
          }
      in
      let* () =
        check_protocols client (Protocol.hash from_protocol, to_protocol_hash)
      in
      Log.info
        "Current period: adoption, with next protocol: %s."
        (target_protocol_tag to_protocol) ;
      (* Bake the transition. *)
      Log.info "Baking transition block..." ;
      let everybody =
        List.map (fun x -> x.Account.public_key_hash) Constant.bootstrap_keys
      in
      let* () = Client.bake_for ~keys:everybody client in
      let* () =
        check_current_period
          client
          {
            index = 6;
            kind = Proposal;
            start_position = 24;
            position = 0;
            remaining = 3;
          }
      in
      let* () = check_protocols client (to_protocol_hash, to_protocol_hash) in
      Log.info
        "Current period: proposal, with protocol: %s."
        (target_protocol_tag to_protocol) ;
      (* Bake one last block to ensure we can still bake after the transition. *)
      let* () = Client.bake_for ~keys:everybody client in
      let* () =
        check_current_period
          client
          {
            index = 6;
            kind = Proposal;
            start_position = 24;
            position = 1;
            remaining = 2;
          }
      in
      check_protocols client (to_protocol_hash, to_protocol_hash)
