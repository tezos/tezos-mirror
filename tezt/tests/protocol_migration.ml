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
   Component:    Protocol
   Invocation:   dune exec tezt/tests/main.exe -- --file protocol_migration.ml
   Subject:      Checks the migration of protocol alpha
*)

let connect (client_1, node_1) (client_2, node_2) =
  let* () = Client.Admin.trust_address client_1 ~peer:node_2 in
  let* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_2 ~peer:node_1 in
  Client.Admin.connect_address client_1 ~peer:node_2

let disconnect (client_1, node_1) (client_2, node_2) =
  let* node_1_id = Node.wait_for_identity node_1
  and* node_2_id = Node.wait_for_identity node_2 in
  let* () = Client.Admin.kick_peer client_1 ~peer:node_2_id in
  Client.Admin.kick_peer client_2 ~peer:node_1_id

let get_proposer ~level client =
  let block = string_of_int level in
  let* metadata =
    RPC.Client.call client @@ RPC.get_chain_block_metadata ~block ()
  in
  Lwt.return metadata.proposer

let all_bootstrap_keys =
  List.map (fun b -> b.Account.alias) (Array.to_list Account.Bootstrap.keys)

(** Boilerplate code to create a user-migratable node. Used in the tests below. **)
let user_migratable_node_init ?node_name ?client_name ?(more_node_args = [])
    ~migration_level ~migrate_to () =
  let* node =
    Node.init
      ?name:node_name
      ~patch_config:
        (Node.Config_file.set_sandbox_network_with_user_activated_upgrades
           [(migration_level, migrate_to)])
      ([Node.Synchronisation_threshold 0; Private_mode] @ more_node_args)
  in
  let* client = Client.(init ?name:client_name ~endpoint:(Node node) ()) in
  Lwt.return (client, node)

(* Migration to Tenderbake is only supported after the first cycle,
   therefore at [migration_level >= blocks_per_cycle]. *)
let perform_protocol_migration ?node_name ?client_name ~blocks_per_cycle
    ~migration_level ~migrate_from ~migrate_to () =
  assert (migration_level >= blocks_per_cycle) ;
  Log.info "Node starting" ;
  let* client, node =
    user_migratable_node_init
      ?node_name
      ?client_name
      ~migration_level
      ~migrate_to
      ()
  in
  Log.info "Node %s initialized" (Node.name node) ;
  let* () = Client.activate_protocol ~protocol:migrate_from client in
  Log.info "Protocol %s activated" (Protocol.hash migrate_from) ;
  (* Bake until migration *)
  let* () =
    repeat (migration_level - 1) (fun () -> Client.bake_for_and_wait client)
  in
  (* Ensure that the block before migration *)
  let* pre_migration_block =
    RPC.Client.call client
    @@ RPC.get_chain_block_metadata ~block:(Int.to_string migration_level) ()
  in
  Log.info "Checking migration block consistency" ;
  Check.(
    (pre_migration_block.protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  Check.(
    (pre_migration_block.next_protocol = Protocol.hash migrate_to)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  let* () = Client.bake_for_and_wait client in
  (* Ensure that we migrated *)
  let* migration_block =
    RPC.Client.call client
    @@ RPC.get_chain_block_metadata
         ~block:(Int.to_string (migration_level + 1))
         ()
  in
  Log.info "Checking migration block consistency" ;
  Check.(
    (migration_block.protocol = Protocol.hash migrate_to)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  Check.(
    (migration_block.next_protocol = Protocol.hash migrate_to)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  (* Test that we can still bake after migration *)
  let* () = repeat 5 (fun () -> Client.bake_for_and_wait client) in
  return (client, node)

(** Test all levels for one cycle, after the first cycle. *)
let test_migration_for_whole_cycle ~migrate_from ~migrate_to =
  let parameters = JSON.parse_file (Protocol.parameter_file migrate_to) in
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  for migration_level = blocks_per_cycle to 2 * blocks_per_cycle do
    Test.register
      ~__FILE__
      ~title:(Printf.sprintf "protocol migration at level %d" migration_level)
      ~tags:["protocol"; "migration"; "sandbox"]
    @@ fun () ->
    let* _client, _node =
      perform_protocol_migration
        ~blocks_per_cycle
        ~migration_level
        ~migrate_from
        ~migrate_to
        ()
    in
    unit
  done

(** Test migration from [migrate_from] to [migrate_to].

    After migration, test snapshots:
     - node0: activate PROTO_A, migrate to PROTO_B, bake, export
              a snapshot in full and rolling modes, and terminate
     - node1: import full, bake
     - node2: import rolling, sync, bake
     - node3: reconstruct full, sync, bake
     - all 4 are synced  *)
let test_migration_with_snapshots ~migrate_from ~migrate_to =
  let migration_level = 8 in
  let parameters = JSON.parse_file (Protocol.parameter_file migrate_to) in
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  let baker = Constant.bootstrap1 in
  let node_params = Node.[Synchronisation_threshold 0; Private_mode] in
  let patch_config =
    Node.Config_file.set_sandbox_network_with_user_activated_upgrades
      [(migration_level, migrate_to)]
  in
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "protocol migration with snapshots")
    ~tags:["protocol"; "migration"; "sandbox"; "snapshot"]
  @@ fun () ->
  let* client0, node0 =
    perform_protocol_migration
      ~node_name:"node0"
      ~client_name:"client0"
      ~blocks_per_cycle
      ~migration_level
      ~migrate_from
      ~migrate_to
      ()
  in
  let rolling_available = 60 in
  Log.info
    "Bake to 'rolling_available' = %d and terminate node0"
    rolling_available ;
  let level = Node.get_level node0 in
  let synchronize head_node nodes =
    let level = Node.get_level head_node in
    Log.info
      "Synchronize node(s) %s with %s"
      (String.concat "," (List.map (fun node -> Node.name node) nodes))
      (Node.name head_node) ;
    Lwt_list.iter_p
      (fun node ->
        let* (_ : int) = Node.wait_for_level node level in
        unit)
      nodes
  in
  let* () =
    repeat (rolling_available - level + 1) @@ fun () ->
    let level_before = Node.get_level node0 in
    let* () = Client.propose_for ~key:[baker.alias] client0 in
    let* () =
      Client.preendorse_for ~key:all_bootstrap_keys ~force:true client0
    in
    let* () = Client.endorse_for ~key:all_bootstrap_keys ~force:true client0 in
    let* level_after = Node.wait_for_level node0 (level_before + 1) in
    Log.debug "Manually baked to level %d" level_after ;
    unit
  in
  let level = Node.get_level node0 in
  Check.(
    (level = rolling_available + 1)
      int
      ~error_msg:"Expected node to be at level %R, was at %L"
      ~__LOC__) ;
  Log.info "Terminate node0" ;
  let* () = Node.terminate node0 in
  Log.info "Export snapshots" ;
  let file_full = Temp.file "snapshot.full" in
  let file_rolling = Temp.file "snapshot.rolling" in
  let* () =
    Node.snapshot_export
      ~history_mode:Full_history
      ~export_level:level
      node0
      file_full
  in
  Log.info "Exported full snapshot in %s" file_full ;
  let* () =
    Node.snapshot_export
      ~history_mode:Rolling_history
      ~export_level:level
      node0
      file_rolling
  in
  Log.info "Exported rolling snapshot in %s" file_rolling ;
  Log.info "Import full snapshot in node1" ;
  let* node1 =
    Node.init
      ~name:"node1"
      ~snapshot:(file_full, false)
      ~patch_config
      node_params
  in
  let* client1 = Client.init ~name:"client1" ~endpoint:(Node node1) () in
  let* () = Client.bake_for_and_wait ~minimal_timestamp:true client1 in
  Log.info "Import rolling snapshot in node2" ;
  let* node2 =
    Node.init
      ~name:"node2"
      ~snapshot:(file_rolling, false)
      ~patch_config
      node_params
  in
  let* client2 = Client.init ~name:"client2" ~endpoint:(Node node2) () in
  let* () = connect (client1, node1) (client2, node2) in
  let* () = synchronize node1 [node2] in
  let* () = Client.bake_for_and_wait ~minimal_timestamp:true client2 in
  Log.info "Reconstruct full node3" ;
  let node3 = Node.create ~name:"node3" node_params in
  let* () = Node.config_init node3 node_params in
  Node.Config_file.update node3 patch_config ;
  let* () = Node.snapshot_import node3 file_full in
  let* () = Node.reconstruct node3 in
  let* () = Node.run node3 node_params in
  let* () = Node.wait_for_ready node3 in
  let* client3 = Client.init ~name:"client3" ~endpoint:(Node node3) () in
  let* () = connect (client1, node1) (client3, node3) in
  let* () = connect (client2, node2) (client3, node3) in
  let* () = synchronize node1 [node2; node3] in
  let* () = Client.bake_for_and_wait ~minimal_timestamp:true client3 in
  Log.info "Rerun node0" ;
  let* () = Node.run node0 node_params in
  let* () = Node.wait_for_ready node0 in
  let* () = connect (client1, node1) (client0, node0) in
  let* () = connect (client2, node2) (client0, node0) in
  let* () = connect (client3, node3) (client0, node0) in
  let* () = synchronize node1 [node0; node2; node3] in
  unit

(** [block_check ~level ~expected_block_type ~migrate_to ~migrate_from client]
    is generic check that a block of type [expected_block_type] contains
    (protocol) metatadata conforming to its type at [level]. **)
let block_check ?level ~expected_block_type ~migrate_to ~migrate_from client =
  let block =
    match level with Some level -> Some (string_of_int level) | None -> None
  in
  let* metadata =
    RPC.Client.call client @@ RPC.get_chain_block_metadata ?block ()
  in
  let protocol = metadata.protocol in
  let next_protocol = metadata.next_protocol in
  (match expected_block_type with
  | `Migration ->
      Check.(
        (next_protocol = Protocol.hash migrate_to)
          string
          ~error_msg:"expected next protocol to be %R, got %L") ;
      Check.(
        (protocol = Protocol.hash migrate_from)
          string
          ~error_msg:"expected (from) protocol to be %R, got %L")
  | `Non_migration ->
      Check.(
        (next_protocol = protocol)
          string
          ~error_msg:"expected a non migration block ")) ;
  Lwt.return_unit

(** Number of elements in [l] that satisfy the predicate [p]. *)
let list_count_p p l =
  let rec aux acc = function
    | [] -> acc
    | x :: t ->
        let acc = if p x then acc + 1 else acc in
        aux acc t
  in
  aux 0 l

let is_endorsement op =
  let kind = JSON.(op |-> "contents" |=> 0 |-> "kind" |> as_string) in
  String.equal kind "endorsement"

(** Check that the given json list of operations contains
    [expected_count] endorsements. *)
let check_endorsements ~expected_count consensus_ops =
  let actual_count = list_count_p is_endorsement (JSON.as_list consensus_ops) in
  Check.((expected_count = actual_count) int)
    ~error_msg:"Expected %L endorsements but got %R."

(** Check that the block at [level] contains [expected_count] endorsements. *)
let check_endorsements_in_block ~level ~expected_count client =
  let* consensus_operations =
    RPC.Client.call client
    @@ RPC.get_chain_block_operations_validation_pass
         ~block:(string_of_int level)
         ~validation_pass:0 (* consensus operations pass *)
         ()
  in
  Lwt.return (check_endorsements ~expected_count consensus_operations)

(** [start_protocol ~expected_bake_for_blocks ~protocol client] sets up a
   protocol with some specific easily tunable parameters (consensus threshold,
   minimal_block_delay), and starts it in the past enough so that one can bake
   [expected_bake_for_blocks] with the bake for command, without hitting a
   "Block in the future" error *and* continue from there with baker daemons
   (which cannot bake in the past) without a hitch.

   @param consensus_threshold is a function of [consensus_committee_size],
   defauls to the mainnet value (2/3 [consensus_committee_size] + 1), instead of
   0 in the sandbox.
   @param round_duration is the (minimal) round duration in seconds (set by
   parameter [minimal_block_delay]), defaults to the sandbox value (typically 1s).
   @param expected_bake_for_blocks is how many blocks you are going to bake with
   the "bake for" command
   @param client   is the client that will activate the protocol
   @param protocol is the protocol to activate **)
let start_protocol ?consensus_threshold ?round_duration
    ~expected_bake_for_blocks ~protocol client =
  let original_parameters_file = Protocol.parameter_file protocol in
  let parameters = JSON.parse_file original_parameters_file in
  let minimal_block_delay =
    match round_duration with
    | None -> JSON.(get "minimal_block_delay" parameters |> as_int)
    | Some duration -> duration
  in
  let* parameter_file =
    (* Default value of consensus_threshold is 0, means we do not need
       endorsements, let's set it up as in mainnet. *)
    let consensus_committee_size =
      JSON.(get "consensus_committee_size" parameters |> as_int)
    in
    (* By default use the computation as in
       {!val:Alpha_context.Constants.Generated.consensus_threshold} *)
    let consensus_threshold =
      match consensus_threshold with
      | None -> (consensus_committee_size * 2 / 3) + 1
      | Some f -> f ~consensus_committee_size
    in
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      [
        (["consensus_threshold"], `Int consensus_threshold);
        (["minimal_block_delay"], `String_of_int minimal_block_delay);
      ]
  in
  let bake_for_delay =
    Ptime.Span.of_int_s (expected_bake_for_blocks * minimal_block_delay)
  in
  Client.activate_protocol
    ~protocol
    ~timestamp:(Ago bake_for_delay)
    ~parameter_file
    client

(** Test that migration occuring through baker daemons

   - does not halt the chain;
   - and that the migration block is not endorsed by the newer
     protocol's baker.

   This has become an issue of sort after updating the consensus protocol to
   Tenderbake.  For one, endorsements have become mandatory, and not only a sign
   of healthiness. Then, a special case for the migration block was built-in as
   a way to deal with first ever migration, in terms of consensus protocol, was
   from Emmy* to Tenderbake.

   Revisit this test, as it may start to fail, whenever a new (family of)
   consensus protocol is put into place in Tezos. **)
let test_migration_with_bakers ?(migration_level = 4)
    ?(num_blocks_post_migration = 5) ~migrate_from ~migrate_to () =
  Test.register
    ~__FILE__
    ~title:
      (Printf.sprintf
         "chain progress/endorsement of migration block from %s to %s with \
          baker daemons"
         (Protocol.tag migrate_from)
         (Protocol.tag migrate_to))
    ~tags:
      [
        "protocol";
        "migration";
        "baker";
        "endorsing";
        "metadata";
        "from_" ^ Protocol.tag migrate_from;
        "to_" ^ Protocol.tag migrate_to;
      ]
  @@ fun () ->
  let* client, node =
    user_migratable_node_init ~migration_level ~migrate_to ()
  in
  let* () =
    start_protocol
      ~expected_bake_for_blocks:migration_level
      client
      ~protocol:migrate_from
  in

  Log.info
    "Launching 2 bakers, one for %s (pre-migration protocol), one for \
     %s(post-migration protocol)"
    (Protocol.name migrate_from)
    (Protocol.name migrate_to) ;
  let baker_for_proto protocol =
    let name = Printf.sprintf "baker-proto-%s" (Protocol.name protocol) in
    let delegates =
      List.map
        (fun account -> account.Account.alias)
        (Array.to_list Account.Bootstrap.keys)
    in
    Baker.init ~protocol ~name node client ~delegates
  in
  let* _baker_from_proto = baker_for_proto migrate_from in
  let* _baker_to_proto = baker_for_proto migrate_to in
  let* _ret = Node.wait_for_level node migration_level in

  (* Bake enough blocks after migration *)
  let last_interesting_level = migration_level + num_blocks_post_migration in
  Log.info "Waiting to reach level %d" last_interesting_level ;
  let* _ret = Node.wait_for_level node last_interesting_level in

  Log.info
    "Checking migration block has not been endorsed -- this is a special case" ;
  let* () =
    check_endorsements_in_block
      ~level:(migration_level + 1)
      ~expected_count:0
      client
  in

  (* Check block metadata *)
  let loop ~_from ~_to =
    let rec aux level =
      if level > _to then Lwt.return_unit
      else
        let expected_block_type =
          if level = migration_level then `Migration else `Non_migration
        in
        let* () =
          block_check
            ~expected_block_type
            client
            ~migrate_from
            ~migrate_to
            ~level
        in
        aux (level + 1)
    in
    aux _from
  in
  Log.info "Checking blocks have correct protocol metadata" ;
  loop ~_from:(migration_level - 1) ~_to:(last_interesting_level - 1)

(** [test_forked_migration_manual] generates the following scenario:
    - start 2 connected nodes
    - bake until migration_level - 1
    - disconnect the nodes
    - propose on each of them a (different) migration block
    - reconnect, launch bakers on every node and wait until enough blocks have
    been proposed to be convinced all is well

   @param migration_level is the level at which migration will occur

   @param num_blocks_post_migration is how many blocks the daemons will bake on top of
   the migration block

   @param migrate_from is the protocol from which we will migrate

   @param migrate_to is the protocol to which we will migrate **)
let test_forked_migration_manual ?(migration_level = 4)
    ?(num_blocks_post_migration = 3) ~migrate_from ~migrate_to () =
  Test.register
    ~__FILE__
    ~tags:
      [
        "protocol";
        "migration";
        "baker";
        "endorsing";
        "fork";
        "manual";
        "from_" ^ Protocol.tag migrate_from;
        "to_" ^ Protocol.tag migrate_to;
      ]
    ~title:
      (Printf.sprintf
         "manually forked migration blocks from %s to %s"
         (Protocol.tag migrate_from)
         (Protocol.tag migrate_to))
  @@ fun () ->
  let* ((client_1, node_1) as cn1) =
    user_migratable_node_init ~migrate_to ~migration_level ()
  in
  let* ((client_2, node_2) as cn2) =
    user_migratable_node_init ~migrate_to ~migration_level ()
  in

  let* () = connect cn1 cn2 in

  let* () =
    (* Activate a protocol where everyone needs to sign off a block proposal
       for it to be included -- set consensus_threshold to 100% of consensus_committee_size *)
    start_protocol
      ~expected_bake_for_blocks:(3 * migration_level)
      client_1
      ~protocol:migrate_from
      ~consensus_threshold:(fun ~consensus_committee_size ->
        consensus_committee_size)
  in

  let* () =
    let n = migration_level - 2 in
    Log.info "Baking %d blocks" n ;
    Lwt_list.iteri_s
      (fun n _ ->
        let* () =
          Client.propose_for
            ~key:all_bootstrap_keys
            ~protocol:migrate_from
            client_2
            ~minimal_timestamp:true
            ~force:true
        in
        let* () =
          Client.preendorse_for
            ~protocol:migrate_from
            ~key:all_bootstrap_keys
            client_2
            ~force:true
        in
        let* () =
          Client.endorse_for
            ~key:all_bootstrap_keys
            client_2
            ~protocol:migrate_from
            ~force:true
        in
        Log.info "Waiting for level %d" (n + 2) ;
        let* _ = Node.wait_for_level node_1 (n + 2) in
        return ())
      (range 1 n)
  in

  Log.info "Disconnecting nodes and proposing migration blocks" ;
  let* () = disconnect cn1 cn2 in

  let delegates_1 =
    Constant.[bootstrap1.alias; bootstrap2.alias; bootstrap3.alias]
  in
  let delegates_2 = Constant.[bootstrap4.alias; bootstrap5.alias] in

  let* () =
    Client.propose_for
      client_1
      ~key:delegates_1
      ~minimal_timestamp:true
      ~force:true
  in
  let* () =
    Client.propose_for
      client_2
      ~key:delegates_2
      ~minimal_timestamp:true
      ~force:true
  in

  Log.info "Waiting for migration level %d" migration_level ;
  let* _ = Node.wait_for_level node_1 migration_level in
  let* _ = Node.wait_for_level node_2 migration_level in

  Log.info "Checking we have migration blocks at level %d" migration_level ;
  let* () =
    block_check
      ~expected_block_type:`Migration
      client_1
      ~migrate_from
      ~migrate_to
      ~level:migration_level
  in
  let* () =
    block_check
      ~expected_block_type:`Migration
      client_2
      ~migrate_from
      ~migrate_to
      ~level:migration_level
  in

  Log.info "Checking we indeed have proposed 2 migration blocks" ;
  let get_migration_proposer = get_proposer ~level:migration_level in

  let* proposer1 = get_migration_proposer client_1 in
  let* proposer2 = get_migration_proposer client_2 in
  Check.(
    (proposer1 <> proposer2)
      string
      ~error_msg:
        "proposer %L is the same as proposer %R: only one migration block has \
         been proposed") ;

  let baker_for_proto ~delegates ~node ~client ~protocol =
    let name =
      Printf.sprintf
        "baker-proto-%s-on-%s"
        (Protocol.name protocol)
        (Node.name node)
    in
    Baker.init ~protocol ~name node client ~delegates
  in

  Log.info
    "Reconnecting nodes and launching bakers for %s"
    (Protocol.name migrate_to) ;
  let* () = connect cn1 cn2 in

  let* baker_1 =
    baker_for_proto
      ~delegates:delegates_1
      ~node:node_1
      ~client:client_1
      ~protocol:migrate_to
  in

  let* baker_2 =
    baker_for_proto
      ~delegates:delegates_2
      ~node:node_2
      ~client:client_2
      ~protocol:migrate_to
  in

  let until_level = migration_level + num_blocks_post_migration in
  Log.info "Waiting to reach level %d" until_level ;
  let* _ = Node.wait_for_level node_1 until_level in

  let* () = Baker.terminate baker_1 in
  let* () = Baker.terminate baker_2 in
  unit

(** Wait for a quorum event on a proposal at the given [level].

    We use a boolean reference, rather than waiting for the
    "new_valid_proposal" event then setting up a waiter for
    "qc_reached". This is because there is little time between both
    events, so there would be a risk that "qc_reached" happens before
    the second waiter has been registered. *)
let wait_for_qc_at_level level baker =
  let where = sf "level = %d" level in
  let level_seen = ref false in
  let proposal_waiter =
    Baker.wait_for baker "new_valid_proposal.v0" ~where (fun json ->
        let proposal_level = JSON.(json |-> "level" |> as_int) in
        if proposal_level = level then (
          level_seen := true ;
          Some ())
        else if proposal_level > level then
          Test.fail
            "Proposal at level %d seen while waiting for proposal at level %d"
            proposal_level
            level
        else None)
  in
  Background.register proposal_waiter ;
  Baker.wait_for baker "qc_reached.v0" ~where (fun (_ : JSON.t) ->
      if !level_seen then Some () else None)

let get_block_at_level level client =
  RPC.(Client.call client (get_chain_block ~block:(string_of_int level) ()))

let test_forked_migration_bakers ~migrate_from ~migrate_to =
  Test.register
    ~__FILE__
    ~tags:
      [
        "protocol";
        "migration";
        "baker";
        "endorsing";
        "fork";
        "from_" ^ Protocol.tag migrate_from;
        "to_" ^ Protocol.tag migrate_to;
      ]
    ~title:
      (Printf.sprintf
         "baker forked migration blocks from %s to %s"
         (Protocol.tag migrate_from)
         (Protocol.tag migrate_to))
  @@ fun () ->
  Log.info
    "This test checks that baker daemons behave correctly (i.e., the chain is \
     not stuck, nor does it split) in a scenario where 3 nodes get different \
     migration and post-migration blocks after being split into two \
     disconnected clusters (of 2 and 1 node respectively) then reconnected." ;
  let migration_level = 4 in
  Log.info "The migration will occur at level %d." migration_level ;
  (* How many blocks to bake after the migration block to check that
     everything is fine. Should be at least 3, so that there is at
     least one post-migration block that is guaranteed final by
     Tenderbake (two levels below the final level of the test). *)
  let num_blocks_post_migration = 4 in

  Log.info "Start and connect three nodes." ;
  let more_node_args = [Node.Connections 2] in
  let* ((client1, node1) as cn1) =
    user_migratable_node_init ~more_node_args ~migrate_to ~migration_level ()
  in
  let* ((client2, node2) as cn2) =
    user_migratable_node_init ~more_node_args ~migrate_to ~migration_level ()
  in
  let* ((client3, node3) as cn3) =
    user_migratable_node_init ~more_node_args ~migrate_to ~migration_level ()
  in
  let* () = connect cn1 cn2
  and* () = connect cn1 cn3
  and* () = connect cn2 cn3 in

  Log.info
    "Partition bootstrap delegates into 3 groups. Start bakers for pre- and \
     post-migration protocols, on a separate node for each group of delegates." ;
  (* The groups are chosen considering baker rights at levels 4 and 5,
     see comment further below. *)
  let group1 =
    (node1, client1, Constant.[bootstrap1.alias; bootstrap2.alias])
  in
  let group2 = (node2, client2, Constant.[bootstrap5.alias]) in
  let group3 =
    (node3, client3, Constant.[bootstrap3.alias; bootstrap4.alias])
  in
  let pp_delegates =
    let pp_sep fmt () = Format.fprintf fmt " and " in
    Format.pp_print_list ~pp_sep Format.pp_print_string
  in
  let baker_for_proto protocol (node, client, delegates) =
    let name = sf "baker_%s_%s" (Protocol.tag protocol) (Node.name node) in
    Log.info "Start %s for %a." name pp_delegates delegates ;
    let event_sections_levels =
      [(String.concat "." [Protocol.encoding_prefix protocol; "baker"], `Debug)]
    in
    (* We copy the code in {!Baker.init}, except that we don't wait
       for the baker to be ready. Indeed, bakers aren't ready until
       their protocol has been activated. *)
    let* () = Node.wait_for_ready node in
    let baker = Baker.create ~protocol ~name ~delegates node client in
    let* () = Baker.run ~event_sections_levels baker in
    Baker.log_block_injection ~color:Log.Color.FG.yellow baker ;
    return baker
  in
  let* baker1_from = baker_for_proto migrate_from group1
  and* _baker2_from = baker_for_proto migrate_from group2
  and* _baker3_from = baker_for_proto migrate_from group3
  and* baker1_to = baker_for_proto migrate_to group1
  and* _baker2_to = baker_for_proto migrate_to group2
  and* baker3_to = baker_for_proto migrate_to group3 in

  Log.info
    "Activate a protocol where everyone needs to sign off a block proposal for \
     it to be included, ie. set consensus_threshold to 100%% of \
     consensus_committee_size." ;
  let* () =
    start_protocol
      ~consensus_threshold:(fun ~consensus_committee_size ->
        consensus_committee_size)
      ~round_duration:2
        (* We bump the round duration to 2s (default in tests is 1s) to
           ensure that there will be enough time to kick node3 after the
           quorum at the pre-migration level but before the proposal of
           the migration-level block (see below). *)
      ~expected_bake_for_blocks:0 (* We will not use "bake for". *)
      ~protocol:migrate_from
      client1
  in

  let pre_migration_level = migration_level - 1 in
  Log.info
    "Wait for a quorum event on a proposal at pre-migration level %d. At this \
     point, we know that endorsements on this proposal have been propagated, \
     so everyone will be able to progress to the next level even if the \
     network gets split."
    pre_migration_level ;
  let* () = wait_for_qc_at_level pre_migration_level baker1_from in

  Log.info
    "Disconnect node3. There are now two independent clusters that don't \
     communicate: [node1; node2] and [node3]." ;
  let* () = disconnect cn1 cn3 and* () = disconnect cn2 cn3 in

  let post_migration_level = migration_level + 1 in
  Log.info
    "Migration blocks are not endorsed, so each cluster should be able to \
     propose blocks for the post-migration level %d. However, since none of \
     the clusters has enough voting power to reach the consensus_threshold, \
     they should not be able to propose blocks for higher levels.\n\
     We wait for each cluster to propose a block at the post-migration level. \
     (If this takes more than 10 seconds, check the comment on baking rights \
     in the code.)"
    post_migration_level ;
  (* For this step to be reasonably fast, we need both clusters to
     have delegates with baking rights for early rounds at the
     migration and post-migration levels. As of March 2023, this step
     takes less than 4 seconds with the following baking rights (with
     minimal_block_delay set to the default ie 1 second):
     - level 4 (migration level), round 0: bootstrap3 (node3)
     - level 4, round 1: bootstrap1 (node1)
     - level 5, round 0: bootstrap1 (node1)
     - level 5, round 1: bootstrap4 (node3)
     If this step takes significantly longer to complete, check
     whether the baking rights have changed and reorganize the split
     of delegates into bakers as needed. *)
  let wait_for_post_migration_proposal baker =
    Baker.wait_for baker "new_valid_proposal.v0" (fun json ->
        let level = JSON.(json |-> "level" |> as_int) in
        if level = post_migration_level then Some ()
        else if level > post_migration_level then
          Test.fail "Reached level %d with a split network." level
        else None)
  in
  let* () = wait_for_post_migration_proposal baker1_to
  and* () = wait_for_post_migration_proposal baker3_to in
  Log.info "Post-migration proposal seen in both clusters." ;

  Log.info "Check that node1 and node3 have different migration blocks." ;
  let* migr_block1 = get_block_at_level migration_level client1
  and* migr_block3 = get_block_at_level migration_level client3 in
  let get_block_hash block_json = JSON.(block_json |-> "hash" |> as_string) in
  if String.equal (get_block_hash migr_block1) (get_block_hash migr_block3) then
    Test.fail "Node1 and node3 have the same migration block." ;

  Log.info "Reconnect node3." ;
  let* () = connect cn3 cn1 and* () = connect cn3 cn2 in

  let end_level = migration_level + num_blocks_post_migration in
  Log.info "Wait for all nodes to reach level %d." end_level ;
  let* (_ : int) = Node.wait_for_level node1 end_level
  and* (_ : int) = Node.wait_for_level node2 end_level
  and* (_ : int) = Node.wait_for_level node3 end_level in

  let level_from = migration_level and level_to = end_level - 2 in
  Log.info
    "Check that node1 and node3 have selected the same final blocks from level \
     %d to %d (both included). (We don't check more recent levels because \
     Tenderbake only guarantees finality up to two levels below the current \
     level.)"
    level_from
    level_to ;
  let n_delegates = Array.length Account.Bootstrap.keys in
  let rec check_blocks ~level_from ~level_to =
    if level_from > level_to then Lwt.return_unit
    else (
      Log.info "Check block at level %d." level_from ;
      let* block1 = get_block_at_level level_from client1
      and* block3 = get_block_at_level level_from client3 in
      if not (JSON.equal block1 block3) then
        Test.fail
          "Level %d block is different on node1 and node3:\n%s\nand\n%s"
          level_from
          (JSON.encode block1)
          (JSON.encode block3) ;
      let consensus_ops = JSON.(block1 |-> "operations" |=> 0) in
      let expected_count =
        if level_from = post_migration_level then 0 else n_delegates
      in
      check_endorsements ~expected_count consensus_ops ;
      check_blocks ~level_from:(level_from + 1) ~level_to)
  in
  check_blocks ~level_from ~level_to

let register ~migrate_from ~migrate_to =
  test_migration_for_whole_cycle ~migrate_from ~migrate_to ;
  test_migration_with_bakers ~migrate_from ~migrate_to () ;
  test_forked_migration_bakers ~migrate_from ~migrate_to ;
  test_forked_migration_manual ~migrate_from ~migrate_to () ;
  test_migration_with_snapshots ~migrate_from ~migrate_to
