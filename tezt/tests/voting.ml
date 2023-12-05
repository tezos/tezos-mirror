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
   Invocation:   dune exec tezt/tests/main.exe -- --file voting.ml
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

(* Files that are to be copied from [test_proto_dir].
   We do not copy [TEZOS_PROTOCOL] because it declares an environment version
   which is too old when trying to adopt protocol Alpha *)
let test_proto_files = ["main.ml"; "main.mli"]

let test_proto_TEZOS_PROTOCOL =
  {|{
    "modules": ["Main"],
    "expected_env_version": 11
}
|}

type period_kind = Proposal | Exploration | Cooldown | Promotion | Adoption

let periods = [Proposal; Exploration; Cooldown; Promotion; Adoption]

let period_kind_to_string = function
  | Proposal -> "proposal"
  | Exploration -> "exploration"
  | Cooldown -> "cooldown"
  | Promotion -> "promotion"
  | Adoption -> "adoption"

let period_kind_of_string = function
  | "proposal" -> Proposal
  | "exploration" -> Exploration
  | "cooldown" -> Cooldown
  | "promotion" -> Promotion
  | "adoption" -> Adoption
  | p ->
      Test.fail
        "Unexpected period kind %s, expected one of: %s"
        p
        (String.concat ", " (List.map period_kind_to_string periods))

let period_kind_type : period_kind Check.typ =
  Check.convert period_kind_to_string Check.string

type period = {
  index : int;
  kind : period_kind;
  start_position : int;
  position : int;
  remaining : int;
}

let pp_period fmt period =
  Format.fprintf
    fmt
    "{ index: %d; kind: %s; start_position: %d; position: %d; remaining: %d }"
    period.index
    (period_kind_to_string period.kind)
    period.start_position
    period.position
    period.remaining

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

let get_current_period ?level client =
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_votes_current_period
         ?block:(Option.map string_of_int level)
         ()
  in
  return (decode_period json)

let get_successor_period ?level client =
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_votes_successor_period
         ?block:(Option.map string_of_int level)
         ()
  in
  return (decode_period json)

let check_current_period ?level client expected_period =
  let* period = get_current_period ?level client in
  Check.((period = expected_period) period_type)
    ~error_msg:"expected current_period = %R, got %L" ;
  unit

let check_successor_period ?level client expected_period =
  let* period = get_successor_period ?level client in
  Check.((period = expected_period) period_type)
    ~error_msg:"expected successor_period = %R, got %L" ;
  unit

let level_type : RPC.level Check.typ =
  Check.convert
    (fun RPC.{level; level_position; cycle; cycle_position; expected_commitment} ->
      (level, level_position, cycle, cycle_position, expected_commitment))
    Check.(tuple5 int int int int bool)

let get_current_level client =
  Client.RPC.call client @@ RPC.get_chain_block_helper_current_level ()

let check_current_level client expected_level =
  let* level = get_current_level client in
  Check.((level = expected_level) level_type)
    ~error_msg:"expected current_period = %R, got %L" ;
  unit

let get_proposals ?level client =
  let* proposals =
    Client.RPC.call client
    @@ RPC.get_chain_block_votes_proposals
         ?block:(Option.map string_of_int level)
         ()
  in
  JSON.as_list proposals |> List.map JSON.as_list
  |> List.map (function
         | [hash; _] -> JSON.as_string hash
         | _ ->
             Test.fail
               "invalid proposal in JSON response: expected a list with 2 items")
  |> return

let get_current_proposal ?level client =
  let* proposal =
    Client.RPC.call client
    @@ RPC.get_chain_block_votes_current_proposal
         ?block:(Option.map string_of_int level)
         ()
  in
  if JSON.is_null proposal then return None
  else return (Some JSON.(proposal |> as_string))

let check_current_proposal ?level client expected_proposal_hash =
  let* current_proposal = get_current_proposal ?level client in
  Check.((current_proposal = Some expected_proposal_hash) (option string))
    ~error_msg:"expected current_proposal = %R, got %L" ;
  unit

let check_protocols ?level client expected_protocols =
  let* block_metadata =
    Client.RPC.call client
    @@ RPC.get_chain_block_metadata ?block:(Option.map string_of_int level) ()
  in
  let protocols_got = (block_metadata.protocol, block_metadata.next_protocol) in
  Check.((protocols_got = expected_protocols) (tuple2 string string))
    ~error_msg:"expected (protocol, next_protocol) = %R, got %L" ;
  unit

let check_listings_not_empty client =
  let* listings =
    Client.RPC.call client @@ RPC.get_chain_block_votes_listings ()
  in
  match JSON.as_list listings with
  | [] ->
      Test.fail "Expected GET .../votes/listing RPC to return a non-empty list"
  | _ :: _ -> unit

type target_protocol = Known of Protocol.t | Injected_test | Demo

let target_protocol_tag = function
  | Known protocol -> Protocol.tag protocol
  | Injected_test -> "injected_test"
  | Demo -> "demo_counter"

let inject_test_protocol client =
  let protocol_path = Temp.dir "test_proto" in
  let* () =
    Process.run
      "cp"
      (List.map (fun filename -> test_proto_dir // filename) test_proto_files
      @ [protocol_path])
  in
  ( with_open_out (protocol_path // "TEZOS_PROTOCOL") @@ fun ch ->
    output_string ch test_proto_TEZOS_PROTOCOL ) ;
  let* protocols_before = Client.Admin.list_protocols client in
  let* test_proto_hash = Client.Admin.inject_protocol client ~protocol_path in
  let* protocols_after = Client.Admin.list_protocols client in
  if List.mem test_proto_hash protocols_before then
    (* Note that [inject_protocol] normally fails if the protocol is already known.
       So this check is only here in case the behavior of [inject_protocol] changes,
       to prevent running this test with an already-known protocol. *)
    Test.fail
      "Test protocol %s was already known, I can't test injection with it."
      test_proto_hash ;
  if not (List.mem test_proto_hash protocols_after) then
    Test.fail
      "Test protocol %s is not known even though it was injected successfully."
      test_proto_hash ;
  Log.info "Injected: %s" test_proto_hash ;
  return test_proto_hash

let test_voting ~from_protocol ~(to_protocol : target_protocol) ~loser_protocols
    =
  Test.register
    ~__FILE__
    ~title:
      (sf
         "amendment: %s -> %s (losers: %s)"
         (Protocol.tag from_protocol)
         (target_protocol_tag to_protocol)
         (if loser_protocols = [] then "none"
         else String.concat ", " (List.map Protocol.tag loser_protocols)))
    ~tags:
      ("amendment"
       :: ("from_" ^ Protocol.tag from_protocol)
       :: ("to_" ^ target_protocol_tag to_protocol)
       :: List.map (fun p -> "loser_" ^ Protocol.tag p) loser_protocols
      @ [
          (match to_protocol with
          | Known _ | Demo -> "known"
          | Injected_test -> "injected");
        ])
  @@ fun () ->
  (* Prepare protocol parameters such that voting periods are shorter
     to make the test run faster. *)
  (* Protocol Alpha uses [cycles_per_voting_period]
     instead of [blocks_per_voting_period],
     we use the values [blocks_per_cycle] = 4 and
     [cycles_for_voting_period] = 1 for Alpha, and we set
     [blocks_per_cycle] = 4 and [blocks_per_voting_period] = 4
     for other protocols. This way periods have the same number
     of blocks for all protocols, and we do not need to handle
     different cases when checking levels and periods in tests. *)
  let parameters =
    [
      (["blocks_per_cycle"], `Int 4);
      (["nonce_revelation_threshold"], `Int 2);
      (["cycles_per_voting_period"], `Int 1);
    ]
  in
  let* parameter_file =
    Protocol.write_parameter_file ~base:(Right (from_protocol, None)) parameters
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
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
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
  let* () = Client.bake_for_and_wait client in
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
    ~error_msg:"expected octez-client show voting period to return %R, got %L" ;
  (* Inject test protocol, or use known protocol. *)
  let* to_protocol_hash =
    match to_protocol with
    | Known protocol -> return (Protocol.hash protocol)
    | Injected_test -> inject_test_protocol client
    | Demo -> return Protocol.demo_counter_hash
  in
  (* Submit proposals: [to_protocol] and [loser_protocols]. *)
  let* () =
    Client.submit_proposals
      ~proto_hashes:(to_protocol_hash :: List.map Protocol.hash loser_protocols)
      client
  in
  (* Bake and check that the proposals are registered. *)
  let* () = Client.bake_for_and_wait client in
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
  let* () = Client.bake_for_and_wait client in
  let* proposals = get_proposals client in
  Check.((sort proposals = sort expected_proposals) (list string))
    ~error_msg:"expected proposals = %R, got %L" ;
  (* Bake until exploration period while checking RPC behavior. *)
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let expected_level =
    (* level_position = 7, 4 blocks per cycle =>
       cycle = 7/4 = 1,
       cycle_position = 7 % 4 = 3
    *)
    RPC.
      {
        level = 8;
        level_position = 7;
        cycle = 1;
        cycle_position = 3;
        expected_commitment = true;
      }
  in
  let* () = check_current_level client expected_level in
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
  let* () = Client.bake_for_and_wait client in
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
  let* () = Client.bake_for_and_wait client in
  let* () =
    let expected_level =
      (* level_position = 9, 4 blocks_per_cycle =>
         cycle = 9/4 = 2
         cycle_position = 9 % 4 = 1
      *)
      RPC.
        {
          level = 10;
          level_position = 9;
          cycle = 2;
          cycle_position = 1;
          expected_commitment = false;
        }
    in
    check_current_level client expected_level
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
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
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
  let* () = Client.bake_for_and_wait client in
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
  let* () = Client.bake_for_and_wait client in
  let* () =
    let expected_level =
      (* level_position = 13, 4 blocks_per_cycle =>
         cycle = 13/4 = 3
         cycle_position = 13 % 4 = 1
      *)
      RPC.
        {
          level = 14;
          level_position = 13;
          cycle = 3;
          cycle_position = 1;
          expected_commitment = false;
        }
    in
    check_current_level client expected_level
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
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
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
  let* () = Client.bake_for_and_wait client in
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
  let* () = Client.bake_for_and_wait client in
  let* () =
    let expected_level =
      (* level_position = 17, 4 blocks_per_cycle =>
         cycle = 17/4 = 4
         cycle_position = 17 % 4 = 1
      *)
      RPC.
        {
          level = 18;
          level_position = 17;
          cycle = 4;
          cycle_position = 1;
          expected_commitment = false;
        }
    in
    check_current_level client expected_level
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
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
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
  let* () = Client.bake_for_and_wait client in
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
  let* () = Client.bake_for_and_wait client in
  let* () =
    let expected_level =
      (* level_position = 21, 4 blocks_per_cycle =>
         cycle = 21/4 = 5
         cycle_position = 21 % 4 = 1
      *)
      RPC.
        {
          level = 22;
          level_position = 21;
          cycle = 5;
          cycle_position = 1;
          expected_commitment = false;
        }
    in
    check_current_level client expected_level
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
  let* () = Client.bake_for_and_wait client in
  let* () =
    check_protocols
      client
      (Protocol.hash from_protocol, Protocol.hash from_protocol)
  in
  Log.info "Baking last block of adoption period..." ;
  let* () = Client.bake_for_and_wait client in
  match to_protocol with
  | Injected_test ->
      (* The test protocol has no amendment process and no baker. We can't go further.
         Instead, we check that another node can fetch the injected protocol. *)
      Log.info "Creating a second node..." ;
      (* FIXME https://gitlab.com/tezos/tezos/-/issues/4837

         We use the "Singleprocess" option because this test relies on
         events which are emitted by the validator. The event mechanism
         of Tezt is currently not compatible with the validator events if it
         is run as an external process by the octez-node (which is the
         case by default). *)
      let* node2 = Node.init [Synchronisation_threshold 1; Singleprocess] in
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
      let* level =
        let* node_level = Node.get_level node in
        Node.wait_for_level node2 node_level
      in
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
        Array.map (fun x -> x.Account.public_key_hash) Account.Bootstrap.keys
        |> Array.to_list
      in
      let* () = Client.bake_for_and_wait ~keys:everybody client in
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
      let* () = Client.bake_for_and_wait ~keys:everybody client in
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
  | Demo ->
      let* () =
        check_protocols client (Protocol.hash from_protocol, to_protocol_hash)
      in
      Log.info
        "Current period: adoption, with next protocol: %s."
        (target_protocol_tag to_protocol) ;
      Log.info "Baking transition block..." ;
      let* () = Demo_client.bake client in
      check_protocols client (to_protocol_hash, to_protocol_hash)

let test_user_activated_protocol_override_baker_vote ~from_protocol ~to_protocol
    =
  Test.register
    ~__FILE__
    ~title:
      (sf
         "user-activated protocol override after vote with bakers: %s -> %s"
         (Protocol.tag from_protocol)
         (Protocol.tag to_protocol))
    ~tags:
      [
        "amendment";
        "protocol_override";
        "baker";
        "voting";
        "from_" ^ Protocol.tag from_protocol;
        "to_" ^ Protocol.tag to_protocol;
      ]
    ~uses:
      [
        Protocol.accuser to_protocol;
        Protocol.baker from_protocol;
        Protocol.baker to_protocol;
      ]
  @@ fun () ->
  let node_arguments = [Node.Synchronisation_threshold 0] in
  let to_protocol_hash = Protocol.hash to_protocol in

  (* Prepare protocol parameters such that voting periods are shorter
     to make the test run faster. *)
  let blocks_per_voting_period = 8 in
  let blocks_per_cycle = 2 * blocks_per_voting_period in

  (* The proposal that will be voted and overridden is submitted in
     the second proposal period. We wait to the very start of this to
     have more time to submit the proposal *)
  let target_proposal_period_start = 1 + blocks_per_voting_period in

  (* Pre-calculate the voting cycle based on the level of the submission.

     [start_of_period] maps periods to level at which they start, assuming that:

     - Proposal starts at [target_proposal_period_start]
     - Each period continues to the following period (i.e. does not move
        back to Proposal until after Adoption.
  *)
  let start_of_period = function
    | Proposal -> target_proposal_period_start
    | Exploration ->
        target_proposal_period_start + (1 * blocks_per_voting_period)
    | Cooldown -> target_proposal_period_start + (2 * blocks_per_voting_period)
    | Promotion -> target_proposal_period_start + (3 * blocks_per_voting_period)
    | Adoption -> target_proposal_period_start + (4 * blocks_per_voting_period)
  in

  (* Check whether the operations at a level contains a given protocol
     submission.

     Checks whether the operations of the block at [level]
     contains a proposal for protocol [proto_hash].
  *)
  let proposal_in_level ~proto_hash client level =
    let* ops =
      Client.RPC.call client
      @@ RPC.get_chain_block_operations ~block:(string_of_int level) ()
    in
    let proposals =
      List.concat_map
        (fun op ->
          let contents = JSON.(op |-> "contents" |> as_list) in
          let proposals =
            List.concat
            @@ List.filter_map
                 (fun op ->
                   if JSON.(op |-> "kind" |> as_string) = "proposals" then
                     Some
                       JSON.(
                         op |-> "proposals" |> as_list |> List.map as_string)
                   else None)
                 contents
          in
          proposals)
        JSON.(ops |=> 1 |> as_list)
    in
    return (List.mem proto_hash proposals)
  in

  (* Perform an [action] before the node reaches a [level_limit], or fail.

     - [~limit_description] describes the limit (e.g. "last block where action can be formed")
     - [~action_description] describes the action (e.g. "submit proposal")
  *)
  let do_before_level ?__LOC__ ~limit_description ~action_description node
      limit_level action =
    let wait_for_successor =
      let* (current_level : int) = Node.wait_for_level node limit_level in
      Test.fail
        ?__LOC__
        "Node (%s) reached block %d, beyond the level limit %d (%s) before it \
         had time to %s"
        (Node.name node)
        current_level
        limit_level
        limit_description
        action_description
    in

    let action =
      let* r = action in
      let* current_level = Node.get_level node in
      Log.info
        "Action `%s` executed before or at level %d (limit %d, %s)"
        action_description
        current_level
        limit_level
        limit_description ;
      return r
    in

    Lwt.pick [wait_for_successor; action]
  in

  (* Perform an [action] (described by [action_description])
     before the the last block of period [kind]. *)
  let do_in_period ?__LOC__ ~action_description node kind action =
    let kind_limit_level =
      start_of_period kind + blocks_per_voting_period - 1
    in
    let limit_description =
      Format.asprintf "the last block of period %s" (period_kind_to_string kind)
    in
    do_before_level
      ?__LOC__
      ~action_description
      ~limit_description
      node
      kind_limit_level
      action
  in

  (* Submit a proposal then wait for its inclusion and return the level thereof.

     Propose protocol [proto_hash], and then wait for the head to switch to the
     block in which it is included. Returns the level of that block.
  *)
  let submit_and_wait_for_proposal ?(level_out = 5) node client proto_hash =
    let* start_level = Node.get_level node in

    (* We have to perform the submissions sufficiently early in the
       proposal period:

        - it seems that proposals included in the last block of the
          proposal period are *not* accounted for -- unclear whether this is a
          bug or not
        - I'm not sure whether it's a concurrency issue in the implementation of
          [do_before_level] but it seems that it does not suffice that the
          submission is performed before the node reaches the
          predecessor of the last block. Therefore the limit is set one block
          lower to have more margin:

       Block (Proposal +0)
       Block (Proposal +1)
       Block (Proposal +blocks_per_voting_period-3):
         we try to perform the submission here at latest
       Block (Proposal +blocks_per_voting_period-2):
         limit it set here, if the node reaches this level before the
         submission was performed the test fails.
       Block (Proposal +blocks_per_voting_period-1):
         proposals included here are ignored
       Block (Exploration +0): exploration period starts if everything went well
    *)
    let* () =
      do_before_level
        ~__LOC__
        ~action_description:"submit proposal"
        ~limit_description:"second to last block of target proposal period"
        node
        (target_proposal_period_start + blocks_per_voting_period - 2)
        (Client.submit_proposals ~proto_hash client)
    in

    let rec loop level =
      if level - start_level >= level_out then
        Test.fail
          "Could not find the proposal operation after waiting %d blocks from \
           level %d"
          level_out
          start_level
      else
        let* (_ : int) = Node.wait_for_level node level in
        let* in_level = proposal_in_level ~proto_hash client level in
        if in_level then return level else loop (level + 1)
    in
    loop (start_level + 1)
  in

  (* Wait for the level at which the period of [kind] should start according to
     the [start_of_period]. *)
  let wait_for_period_kind node client kind =
    let expected_level = start_of_period kind in
    let* (_ : int) = Node.wait_for_level node expected_level in
    (* Sanity checks *)
    let* period = get_current_period ~level:expected_level client in
    let* previous_period =
      get_current_period ~level:(expected_level - 1) client
    in
    Check.(previous_period.kind <> kind)
      period_kind_type
      ~error_msg:
        ("Did not expect to find period kind %R at level "
        ^ string_of_int (expected_level - 1)) ;
    Check.(period.kind = kind)
      period_kind_type
      ~error_msg:
        ("Expected to find period kind %R but found %L in level "
        ^ string_of_int expected_level) ;
    return expected_level
  in

  (* Wait for last level at which the period of [kind] should be
     active according to the [start_of_period]. *)
  let wait_for_last_block_of_period_kind node client kind expected_level =
    let* (_ : int) = Node.wait_for_level node expected_level in
    (* Sanity checks *)
    let* period = get_current_period ~level:expected_level client in
    let* successor_period = get_successor_period ~level:expected_level client in
    Check.(successor_period.kind <> kind)
      period_kind_type
      ~error_msg:
        ("Did not expect to find period kind %L, it should be the first block \
          of the following period, in level "
        ^ string_of_int (expected_level + 1)) ;
    Check.(period.kind = kind)
      period_kind_type
      ~error_msg:
        ("Found period kind %L but expected %R (should be last block of that \
          kind), in level"
        ^ string_of_int expected_level) ;
    return expected_level
  in

  let* parameter_file =
    (* See notes above in [test_voting].

       In addition, should this test turn out to be flaky, some pointers to make
       it more robust (at the cost of speed), are:
       - at more blocks to each voting period by increasing
         [blocks_per_voting_period].
       - (in tenderbake) increasing the delay between blocks, by increasing
         the constants [minimal_block_delay] and [delay_increment_per_round]

       In conjunction, they will give the test more time to perform actions and
       assertions in between blocks.

       The parts of the test that are inherently flaky are wrapped in calls to
       [do_before_level] and [do_in_period]. These are:

         - Submitting the protocol proposal before the end of the second
           proposal period (we ignore the first proposal period).
         - Casting votes before the end of the Exploration period
         - Recasting votes before the end of the Promotion period

       If the test turns out flaky, I suggest investigating there.
    *)
    Protocol.write_parameter_file
      ~base:(Right (from_protocol, None))
      [
        (["blocks_per_cycle"], `Int blocks_per_cycle);
        (["blocks_per_voting_period"], `Int blocks_per_voting_period);
      ]
  in

  (* Start the actual test *)
  Log.info "Start a node and inject a test protocol that will be overriden" ;
  let* node = Node.init node_arguments in
  let* client = Client.init ~endpoint:(Node node) () in

  (* Curry some functions for brevity *)
  let submit_and_wait_for_proposal ?level_out =
    submit_and_wait_for_proposal ?level_out node client
  in
  let wait_for_period_kind = wait_for_period_kind node client in
  let do_in_period ?__LOC__ = do_in_period ?__LOC__ node in
  let wait_for_last_block_of_period_kind =
    wait_for_last_block_of_period_kind node client
  in

  let* test_proto_hash = inject_test_protocol client in
  let* () = Node.terminate node in

  Log.info
    "Restart the node with a protocol override configuration for %s"
    test_proto_hash ;
  Node.Config_file.(
    update
      node
      (set_sandbox_network_with_user_activated_overrides
         [(test_proto_hash, to_protocol_hash)])) ;
  let* () = Node.terminate node in
  let* () = Node.run node node_arguments in
  let* () = Node.wait_for_ready node in

  let* () =
    (* By default, [Client.activate_protocol] activates the protocols
       with a back-dated timestamp. This allows for rapid baking of
       blocks, which is desirable in synchronous tests. In this test
       we bake asynchronously with our assertions. We prefer the
       default delay between blocks, to give us larger
       margins. Therefore, we activate the protocol at the current
       timestamp. *)
    Client.activate_protocol
      ~protocol:from_protocol
      ~timestamp:Now
      ~parameter_file
      client
  in

  let* () =
    check_protocols
      client
      ( "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im",
        Protocol.hash from_protocol )
  in
  Log.info "Protocol set to activate, starting daemons" ;

  (* Register event handlers before starting the baker to avoid flakiness *)
  let wait_for_second_proposal_period_start_p =
    Node.wait_for_level node target_proposal_period_start
  in
  let wait_for_period_kind_exploration_p = wait_for_period_kind Exploration in
  let wait_for_period_kind_cooldown_p = wait_for_period_kind Cooldown in
  let wait_for_period_kind_promotion_p = wait_for_period_kind Promotion in
  let wait_for_period_kind_adoption_p = wait_for_period_kind Adoption in
  let expected_level_of_last_block_adoption =
    start_of_period Adoption + blocks_per_voting_period - 1
  in
  let wait_for_last_block_of_period_kind_adoption_p =
    wait_for_last_block_of_period_kind
      Adoption
      expected_level_of_last_block_adoption
  in
  let expected_level_of_next_proposal =
    expected_level_of_last_block_adoption + 1
  in
  let wait_for_next_proposal_p =
    Node.wait_for_level node expected_level_of_next_proposal
  in
  let good_measure = 3 in
  let wait_for_final_bakes_p =
    Node.wait_for_level node (expected_level_of_next_proposal + good_measure)
  in

  let* _ = Baker.init ~protocol:from_protocol node client in

  (* The baker for [to_protocol] is not ready until that protocol
     activates, therefore we do not resolve yet. *)
  let _to_protocol_baker_p = Baker.init ~protocol:to_protocol node client in

  (* We also start an accuser for the [to_protocol] protocol. After the
     protocol switch, we verify that it starts registering blocks. *)
  let to_protocol_accuser = Accuser.create ~protocol:to_protocol node in
  let to_protocol_accuser_received_block =
    Accuser.wait_for
      to_protocol_accuser
      "accuser_processed_block.v0"
      (fun json -> Some (json |> JSON.as_string))
  in
  let* _ = Accuser.run to_protocol_accuser in

  Log.info "Waiting for the start of the second proposal period" ;
  let* (_ : int) = wait_for_second_proposal_period_start_p in
  let* () =
    check_protocols
      ~level:2
      client
      (Protocol.hash from_protocol, Protocol.hash from_protocol)
  in

  Log.info "Submitting injected proposal: %s" test_proto_hash ;
  let* proposal_level = submit_and_wait_for_proposal test_proto_hash in
  let* proposal_period = get_current_period ~level:proposal_level client in
  Log.info
    "Found the protocol submission in level %d (remaining: %d)"
    proposal_level
    proposal_period.remaining ;
  let* proposals = get_proposals ~level:proposal_level client in
  Check.((proposals = [test_proto_hash]) (list string))
    ~error_msg:"expected proposals = %R, got %L" ;

  Log.info "Wait until exploration period" ;
  let* exploration_level = wait_for_period_kind_exploration_p in
  let* () =
    check_current_proposal ~level:exploration_level client test_proto_hash
  in

  Log.info "Vote for %s" test_proto_hash ;
  let vote (account : Account.key) ballot =
    Client.submit_ballot
      ~key:account.alias
      ~proto_hash:test_proto_hash
      ballot
      client
  in
  let votes accts ballot =
    Lwt_list.iter_s (fun acct -> vote acct ballot) accts
  in
  let vote_participants =
    [Constant.bootstrap1; Constant.bootstrap2; Constant.bootstrap3]
  in
  let* () =
    do_in_period ~__LOC__ ~action_description:"cast votes" Exploration
    @@ votes vote_participants Yay
  in

  Log.info "Wait until cooldown period" ;
  let* cooldown_level = wait_for_period_kind_cooldown_p in
  let* () =
    check_current_proposal ~level:cooldown_level client test_proto_hash
  in

  Log.info "Wait until promotion period, revote for %s" test_proto_hash ;
  let* promotion_level = wait_for_period_kind_promotion_p in
  let* () =
    check_current_proposal ~level:promotion_level client test_proto_hash
  in
  let* () =
    do_in_period ~__LOC__ ~action_description:"recast votes" Promotion
    @@ votes vote_participants Yay
  in

  Log.info "Wait until adoption period" ;
  let* adoption_level = wait_for_period_kind_adoption_p in
  let* () =
    check_current_proposal ~level:adoption_level client test_proto_hash
  in

  Log.info "Wait for the last block of the adoption period" ;
  let* (_ : int) = wait_for_last_block_of_period_kind_adoption_p in

  Log.info
    "Check that replacement protocol %s will activate instead of voted %s"
    to_protocol_hash
    test_proto_hash ;

  let* () =
    check_protocols
      ~level:expected_level_of_last_block_adoption
      client
      (Protocol.hash from_protocol, to_protocol_hash)
  in

  Log.info
    "Bake until the first block of the next proposal period at level %d"
    expected_level_of_next_proposal ;
  let* (_ : int) = wait_for_next_proposal_p in

  Log.info "Activate the replacement protocol baker" ;
  let* _ = _to_protocol_baker_p in

  Log.info
    "Check that replacement protocol %s is active"
    (Protocol.hash to_protocol) ;
  let* () =
    check_protocols
      ~level:expected_level_of_next_proposal
      client
      (Protocol.hash to_protocol, Protocol.hash to_protocol)
  in

  (* Bake a few blocks for good measure *)
  let* (_ : int) = wait_for_final_bakes_p in
  Log.info
    "Verify that the replacement accuser has registered at least one block" ;
  let* accuser_first_block_hash = to_protocol_accuser_received_block in
  let* proposal_first_block_hash =
    Client.RPC.call client
    @@ RPC.get_chain_block_hash
         ~block:(string_of_int expected_level_of_next_proposal)
         ()
  in
  Check.((accuser_first_block_hash = proposal_first_block_hash) string)
    ~error_msg:"Expected the accuser to find %R, found %L" ;

  unit

let register ~from_protocol ~(to_protocol : target_protocol) ~loser_protocols =
  test_voting ~from_protocol ~to_protocol ~loser_protocols ;
  (* The test_user_activated_protocol_override_baker_vote test only
     supports a known target protocol. *)
  match to_protocol with
  | Known to_protocol ->
      test_user_activated_protocol_override_baker_vote
        ~from_protocol
        ~to_protocol
  | Demo | Injected_test -> ()
