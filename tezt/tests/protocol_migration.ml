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

let team = Tag.layer1

let connect (client_1, node_1) (client_2, node_2) =
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  Client.Admin.connect_address client_1 ~peer:node_2

let disconnect (client_1, _node_1) (_client_2, node_2) =
  let* node_2_id = Node.wait_for_identity node_2 in
  let* () = Client.Admin.kick_peer client_1 ~peer:node_2_id in
  Background.register (Client.Admin.untrust_address client_1 ~peer:node_2) ;
  unit

let get_proposer ~level client =
  let block = string_of_int level in
  let* metadata =
    Client.RPC.call client @@ RPC.get_chain_block_metadata ~block ()
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

(** [block_check ?level ~expected_block_type ~migrate_to ~migrate_from client]
    is generic check that a block of type [expected_block_type] contains
    (protocol) metatadata conforming to its type at [level]. **)
let block_check ?level ~expected_block_type ~migrate_to ~migrate_from client =
  let block =
    match level with Some level -> Some (string_of_int level) | None -> None
  in
  let* metadata =
    Client.RPC.call client @@ RPC.get_chain_block_metadata ?block ()
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

let pp_option pp_item fmt = function
  | None -> Format.pp_print_string fmt "None"
  | Some item ->
      Format.pp_print_string fmt "Some " ;
      pp_item fmt item

let check_adaptive_issuance_launch_cycle ~loc client =
  let* launch_cycle =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_adaptive_issuance_launch_cycle ()
  and* level = Client.level client in
  let expected =
    (* From protocol R on, when the protocol is activated from
       Genesis, AI is immediately active. *)
    Some 0
  in
  Log.debug
    "Checking that adaptive_issuance_launch_cycle at level %d is %a."
    level
    (pp_option Format.pp_print_int)
    expected ;
  Check.(
    (launch_cycle = expected)
      (option int)
      ~__LOC__:loc
      ~error_msg:"Expected adaptive_issuance_launch_cycle = %R but got %L") ;
  unit

(* Check for T to U migration. Remove after U. *)
let check_delegate_stake_info client =
  let check_failure rpc =
    let*? process = Client.RPC.spawn client @@ rpc in
    Process.check ~expect_failure:true process
  in
  let check_success rpc =
    let*? process = Client.RPC.spawn client @@ rpc in
    Process.check ~expect_failure:false process
  in
  let* constants =
    Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(constants |-> "blocks_per_cycle" |> as_int) in
  let consensus_rights_delay =
    JSON.(constants |-> "consensus_rights_delay" |> as_int)
  in
  let* level = Client.level client in
  let current_cycle = level / blocks_per_cycle in
  let* () =
    Tezos_base.TzPervasives.List.iter_s
      (fun cycle ->
        let* () =
          check_success
          @@ RPC.get_chain_block_context_raw_json
               ~path:["cycle"; string_of_int cycle; "delegate_stake_info"]
               ()
        in
        unit)
      (List.init (consensus_rights_delay + 2) (fun x ->
           max 0 (current_cycle - 1 + x)))
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_raw_json
         ~path:
           [
             "cycle";
             string_of_int (current_cycle + consensus_rights_delay + 1);
             "delegate_stake_info";
           ]
         ()
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_raw_json
         ~path:
           ["cycle"; string_of_int (current_cycle - 2); "delegate_stake_info"]
         ()
  in
  unit

(* Migration to Tenderbake is only supported after the first cycle,
   therefore at [migration_level >= blocks_per_cycle]. *)
let perform_protocol_migration ?node_name ?client_name ?parameter_file
    ?more_node_args ~blocks_per_cycle ~migration_level ~migrate_from ~migrate_to
    ~baked_blocks_after_migration () =
  assert (migration_level >= blocks_per_cycle) ;
  Log.info "Node starting" ;
  let* client, node =
    user_migratable_node_init
      ?node_name
      ?client_name
      ?more_node_args
      ~migration_level
      ~migrate_to
      ()
  in
  Log.info "Node %s initialized" (Node.name node) ;
  let* () =
    Client.activate_protocol ~protocol:migrate_from client ?parameter_file
  in
  Log.info "Protocol %s activated" (Protocol.hash migrate_from) ;
  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client in
  (* Bake until migration *)
  let* () =
    repeat (migration_level - 1) (fun () -> Client.bake_for_and_wait client)
  in
  (* Ensure that the block before migration is consistent *)
  Log.info "Checking migration block consistency" ;
  let* () =
    block_check
      ~expected_block_type:`Migration
      client
      ~migrate_from
      ~migrate_to
      ~level:migration_level
  in
  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client in
  (* Remove check_delegate_stake_info after U. *)
  let* () = check_delegate_stake_info client in
  let* () = Client.bake_for_and_wait client in
  (* Ensure that we migrated *)
  Log.info "Checking migration block consistency" ;
  let* () =
    block_check
      ~expected_block_type:`Non_migration
      client
      ~migrate_from
      ~migrate_to
      ~level:(migration_level + 1)
  in
  (* Test that T to U stitching for Delegate_stake_info worked correctly.
     Remove after U. *)
  let* () = check_delegate_stake_info client in
  (* Test that we can still bake after migration *)
  let* () =
    repeat baked_blocks_after_migration (fun () ->
        Client.bake_for_and_wait client)
  in
  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client in
  (* Remove check_delegate_stake_info after U. *)
  let* () = check_delegate_stake_info client in
  return (client, node)

(** Test all levels for one cycle, after the first cycle. *)
let test_migration_for_whole_cycle ~migrate_from ~migrate_to =
  let parameters = JSON.parse_file (Protocol.parameter_file migrate_to) in
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  let consensus_rights_delay =
    JSON.(get "consensus_rights_delay" parameters |> as_int)
  in
  for migration_level = blocks_per_cycle to 2 * blocks_per_cycle do
    Test.register
      ~__FILE__
      ~title:(Printf.sprintf "protocol migration at level %d" migration_level)
      ~tags:[team; "protocol"; "migration"; "sandbox"]
    @@ fun () ->
    let* _client, _node =
      perform_protocol_migration
        ~blocks_per_cycle
        ~migration_level
        ~migrate_from
        ~migrate_to
        ~baked_blocks_after_migration:
          (2 * consensus_rights_delay * blocks_per_cycle)
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
    ~tags:[team; "protocol"; "migration"; "sandbox"; "snapshot"]
  @@ fun () ->
  let* client0, node0 =
    perform_protocol_migration
      ~more_node_args:[Node.(History_mode default_full)]
      ~node_name:"node0"
      ~client_name:"client0"
      ~blocks_per_cycle
      ~migration_level
      ~migrate_from
      ~migrate_to
      ~baked_blocks_after_migration:5
      ()
  in
  let rolling_available = 60 in
  Log.info
    "Bake to 'rolling_available' = %d and terminate node0"
    rolling_available ;
  let* level = Node.get_level node0 in
  let synchronize head_node nodes =
    let* level = Node.get_level head_node in
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
    let* level_before = Node.get_level node0 in
    let* () = Client.propose_for ~key:[baker.alias] client0 in
    let* () =
      Client.preattest_for
        ~protocol:migrate_to
        ~key:all_bootstrap_keys
        ~force:true
        client0
    in
    let* () =
      Client.attest_for
        ~protocol:migrate_to
        ~key:all_bootstrap_keys
        ~force:true
        client0
    in
    let* level_after = Node.wait_for_level node0 (level_before + 1) in
    Log.debug "Manually baked to level %d" level_after ;
    unit
  in
  let* level = Node.get_level node0 in
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
  let* () = Node.Config_file.update node3 patch_config in
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

(** Number of elements in [l] that satisfy the predicate [p]. *)
let list_count_p p l =
  let rec aux acc = function
    | [] -> acc
    | x :: t ->
        let acc = if p x then acc + 1 else acc in
        aux acc t
  in
  aux 0 l

let is_attestation ~protocol:_ op =
  let kind = JSON.(op |-> "contents" |=> 0 |-> "kind" |> as_string) in
  String.equal kind "attestation"

(** Check that the given json list of operations contains
    [expected_count] attestations. *)
let check_attestations ~protocol ~expected_count consensus_ops =
  let actual_count =
    list_count_p (is_attestation ~protocol) (JSON.as_list consensus_ops)
  in
  Check.((expected_count = actual_count) int)
    ~error_msg:"Expected %L attestations but got %R."

(** Check that the block at [level] contains [expected_count] attestations. *)
let check_attestations_in_block ~protocol ~level ~expected_count client =
  let* consensus_operations =
    Client.RPC.call client
    @@ RPC.get_chain_block_operations_validation_pass
         ~version:"1"
         ~block:(string_of_int level)
         ~validation_pass:0 (* consensus operations pass *)
         ()
  in
  Lwt.return (check_attestations ~protocol ~expected_count consensus_operations)

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
       attestations, let's set it up as in mainnet. *)
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
    let consensus_threshold_name =
      if Protocol.(number protocol >= 022) then "consensus_threshold_size"
      else "consensus_threshold"
    in
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      [
        ([consensus_threshold_name], `Int consensus_threshold);
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

(** Test that migration occurring through agnostic baker daemons does not halt the
    chain.

    This has become an issue of sort after updating the consensus protocol to
    Tenderbake.  For one, attestations have become mandatory, and not only a sign
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
         "chain progress/attestation of migration block from %s to %s with %s \
          daemon(s)"
         (Protocol.tag migrate_from)
         (Protocol.tag migrate_to)
         "agnostic_baker")
    ~tags:
      [
        team;
        "protocol";
        "migration";
        "attesting";
        "metadata";
        "from_" ^ Protocol.tag migrate_from;
        "to_" ^ Protocol.tag migrate_to;
      ]
    ~uses:[Constant.octez_agnostic_baker]
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
  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client in
  let delegates =
    List.map
      (fun account -> account.Account.alias)
      (Array.to_list Account.Bootstrap.keys)
  in
  Log.info "Launching an agnostic baker" ;
  let* _agnostic_baker =
    Agnostic_baker.init ~name:"agnostic_baker" node client ~delegates
  in
  let* _ret = Node.wait_for_level node migration_level in
  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client in

  (* Bake enough blocks after migration *)
  let last_interesting_level = migration_level + num_blocks_post_migration in
  Log.info "Waiting to reach level %d" last_interesting_level ;
  let* _ret = Node.wait_for_level node last_interesting_level in
  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client in

  Log.info
    "Checking migration block has not been attested -- this is a special case" ;
  let* () =
    check_attestations_in_block
      ~protocol:migrate_from
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
        team;
        "protocol";
        "migration";
        "attesting";
        "fork";
        "manual";
        "from_" ^ Protocol.tag migrate_from;
        "to_" ^ Protocol.tag migrate_to;
      ]
    ~uses:[Constant.octez_agnostic_baker]
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
  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client_1 in

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
          Client.preattest_for
            ~protocol:migrate_from
            ~key:all_bootstrap_keys
            client_2
            ~force:true
        in
        let* () =
          Client.attest_for
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
  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client_1 in

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

  Log.info
    "Reconnecting nodes and launching bakers for %s"
    (Protocol.name migrate_to) ;
  let* () = connect cn1 cn2 in

  let* baker_1 = Agnostic_baker.init ~delegates:delegates_1 node_1 client_1 in

  let* baker_2 = Agnostic_baker.init ~delegates:delegates_2 node_2 client_2 in

  let until_level = migration_level + num_blocks_post_migration in
  Log.info "Waiting to reach level %d" until_level ;
  let* _ = Node.wait_for_level node_1 until_level in
  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client_1 in

  let* () = Agnostic_baker.terminate baker_1 in
  let* () = Agnostic_baker.terminate baker_2 in
  unit

(** Wait for a quorum event on a proposal at the given [level].

    We use a boolean reference, rather than waiting for the
    "new_valid_proposal" event then setting up a waiter for
    "qc_reached". This is because there is little time between both
    events, so there would be a risk that "qc_reached" happens before
    the second waiter has been registered. *)
let wait_for_qc_at_level baker level =
  Log.info
    "Wait for a quorum event on a proposal at pre-migration level %d. At this \
     point, we know that attestations on this proposal have been propagated, \
     so everyone will be able to progress to the next level even if the \
     network gets split."
    level ;
  let where = sf "level = %d" level in
  let level_seen = ref false in
  let proposal_waiter =
    Agnostic_baker.wait_for baker "new_valid_proposal.v0" ~where (fun json ->
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
  Agnostic_baker.wait_for baker "qc_reached.v0" ~where (fun (_ : JSON.t) ->
      if !level_seen then Some () else None)

let get_block_at_level level client =
  Client.RPC.call
    client
    (RPC.get_chain_block ~version:"1" ~block:(string_of_int level) ())

let wait_for_post_migration_proposal baker post_migration_level =
  Agnostic_baker.wait_for baker "new_valid_proposal.v0" (fun json ->
      let level = JSON.(json |-> "level" |> as_int) in
      if level = post_migration_level then Some ()
      else if level > post_migration_level then
        Test.fail "Reached level %d with a split network." level
      else None)

let test_forked_migration_bakers ~migrate_from ~migrate_to =
  Test.register
    ~__FILE__
    ~tags:
      ([team; "protocol"; "migration"]
      @ [
          "attesting";
          "fork";
          "from_" ^ Protocol.tag migrate_from;
          "to_" ^ Protocol.tag migrate_to;
        ])
    ~uses:[Constant.octez_agnostic_baker]
    ~title:
      (Printf.sprintf
         "agnostic baker forked migration blocks from %s to %s"
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
    "Partition bootstrap delegates into 3 groups. Start agnostic bakers on a \
     separate node for each group of delegates." ;
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

  let agnostic_baker (node, client, delegates) =
    let name = sf "agnostic_baker_%s" (Node.name node) in
    Log.info "Start %s for %a." name pp_delegates delegates ;
    let* baker = Agnostic_baker.init ~name ~delegates node client in
    Agnostic_baker.log_block_injection ~color:Log.Color.FG.yellow baker ;
    return baker
  in

  let start_protocol () =
    Log.info
      "Activate a protocol where everyone needs to sign off a block proposal \
       for it to be included, ie. set consensus_threshold to 100%% of \
       consensus_committee_size." ;
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
  let post_migration_level = migration_level + 1 in

  let disconnect_clusters () =
    Log.info
      "Disconnect node3. There are now two independent clusters that don't \
       communicate: [node1; node2] and [node3]." ;
    let* () = disconnect cn1 cn3 and* () = disconnect cn2 cn3 in
    unit
  in

  let check_post_migration_proposal ~wait_for =
    Log.info
      "Migration blocks are not attested, so each cluster should be able to \
       propose blocks for the post-migration level %d. However, since none of \
       the clusters has enough voting power to reach the consensus_threshold, \
       they should not be able to propose blocks for higher levels.\n\
       We wait for each cluster to propose a block at the post-migration \
       level. (If this takes more than 10 seconds, check the comment on baking \
       rights in the code.)"
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
    let* () = wait_for () in
    Log.info "Post-migration proposal seen in both clusters." ;
    unit
  in

  let* () =
    let* agnostic_baker1 = agnostic_baker group1
    and* _agnostic_baker2 = agnostic_baker group2
    and* agnostic_baker3 = agnostic_baker group3 in
    let* () = start_protocol () in
    let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client1 in
    let* () = wait_for_qc_at_level agnostic_baker1 pre_migration_level in
    let* () = disconnect_clusters () in
    let wait_for () =
      let* () =
        wait_for_post_migration_proposal agnostic_baker1 post_migration_level
      and* () =
        wait_for_post_migration_proposal agnostic_baker3 post_migration_level
      in
      unit
    in
    check_post_migration_proposal ~wait_for
  in

  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client1 in

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
  let* () = check_adaptive_issuance_launch_cycle ~loc:__LOC__ client1 in

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
      let protocol =
        if level_from > migration_level then migrate_to else migrate_from
      in
      check_attestations ~protocol ~expected_count consensus_ops ;
      check_blocks ~level_from:(level_from + 1) ~level_to)
  in
  check_blocks ~level_from ~level_to

module Local_helpers = struct
  let get_current_level client =
    let* current_level =
      Client.RPC.call client @@ RPC.get_chain_block_helper_current_level ()
    in
    Log.info
      ~color:Log.Color.FG.blue
      "Current level = %d and cycle = %d."
      current_level.level
      current_level.cycle ;
    return current_level

  let check_current_level_and_cycle ~level ~cycle client =
    let* current_level = get_current_level client in
    Check.((current_level.cycle = cycle) ~__LOC__ int)
      ~error_msg:"Expected cycle=%R, got %L" ;
    Check.((current_level.level = level) ~__LOC__ int)
      ~error_msg:"Expected level=%R, got %L" ;
    unit

  let multiple_transfers ~baker ~giver ~(receivers : (string * Tez.t) list)
      client =
    let transfers =
      List.map
        (fun (alias, amount) ->
          `O
            [
              ("destination", `String alias);
              ("amount", `String (Tez.to_string amount));
            ])
        receivers
    in
    let json_batch = `A transfers |> JSON.encode_u in
    let*! () =
      Client.multiple_transfers ~burn_cap:Tez.one ~giver ~json_batch client
    in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in
    unit

  let create_delegates_and_stake ~baker ~giver
      ~(delegates : (string * Tez.t) list) client =
    let* accounts =
      Lwt_list.map_s
        (fun (alias, amount) ->
          let* account = Client.gen_and_show_keys ~alias client in
          Log.info
            ~color:Log.Color.FG.green
            "Create an account for %s with pkh = %s and spendable balance = %s \
             tez."
            alias
            account.public_key_hash
            (Tez.to_string amount) ;
          return account)
        delegates
    in
    let* () = multiple_transfers ~baker ~giver ~receivers:delegates client in

    let* () =
      Lwt_list.iter_s
        (fun (alias, _amount) ->
          Log.info ~color:Log.Color.FG.green "Register %s as a delegate." alias ;
          let* _ = Client.register_delegate ~delegate:alias client in
          unit)
        delegates
    in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in

    let* () =
      Lwt_list.iter_s
        (fun (alias, amount) ->
          let amount = Tez.(amount /! 2L) in
          Log.info
            ~color:Log.Color.FG.green
            "Delegate %s stakes %s tez."
            alias
            (Tez.to_string amount) ;
          Client.stake amount ~staker:alias client)
        delegates
    in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in
    return accounts

  let transfer ~baker ~amount ~giver ~receiver client =
    Log.info
      ~color:Log.Color.FG.green
      "Transfer %s tez from %s to %s."
      (Tez.to_string amount)
      giver
      receiver ;
    let* () =
      Client.transfer ~burn_cap:Tez.one ~amount ~giver ~receiver client
    in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in
    unit

  let unstake ~baker ~amount ~staker client =
    Log.info
      ~color:Log.Color.FG.green
      "Delegate %s unstakes %s tez."
      staker
      (Tez.to_string amount) ;
    let* () = Client.unstake amount ~staker client in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in
    unit

  let unstake_requests ~(staker : Account.key) client =
    let pkh = staker.public_key_hash in
    let* unstake_requests =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_unstake_requests pkh
    in
    Log.info
      ~color:Log.Color.FG.gray
      "Delegate %s: unstake_requests = %s"
      staker.alias
      (JSON.encode unstake_requests) ;
    let finalizable = JSON.(unstake_requests |-> "finalizable" |> as_list) in
    let unfinalizable =
      JSON.(unstake_requests |-> "unfinalizable" |-> "requests" |> as_list)
    in
    return (finalizable, unfinalizable)

  let unstaked_balance ~(staker : Account.key) client =
    let pkh = staker.public_key_hash in
    let* unstaked_finalizable_balance =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_unstaked_finalizable_balance pkh
    in
    let* unstaked_frozen_balance =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_unstaked_frozen_balance pkh
    in
    Log.info
      ~color:Log.Color.FG.gray
      "Delegate %s: unstaked_finalizable_balance = %s and \
       unstaked_frozen_balance = %s"
      staker.alias
      (Int.to_string unstaked_finalizable_balance)
      (Int.to_string unstaked_frozen_balance) ;
    return (unstaked_finalizable_balance, unstaked_frozen_balance)

  let check_is_finalizable_all_requests ~(staker : Account.key) client ~expected
      =
    let pkh = staker.public_key_hash in
    let* unstaked_frozen_balance =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_unstaked_frozen_balance pkh
    in
    if expected then
      Check.((unstaked_frozen_balance = 0) ~__LOC__ int)
        ~error_msg:"Expected unstaked_frozen_balance=%R, got %L"
    else
      Check.((unstaked_frozen_balance <> 0) ~__LOC__ int)
        ~error_msg:"Expected unstaked_frozen_balance<>%R, got %L" ;
    unit

  (* 1. Bake until [target_cycle] with the list of [bakers].
     2. For the last block of the [target_cycle - 1] cycle,
     the following is checked:
        - migration block consistency if it is a migration block
        - [check_last_block] is valid
     3. The first block of [target_cycle] is baked with [delegate],
        which is supposed to be its last activity in the cycle.
        The following is also checked:
        - [check_next_block] is valid *)
  let bake_until_cycle_with_and_check ~migration_level ~migrate_from ~migrate_to
      ~blocks_per_cycle ~bakers ~target_cycle ~delegate ~check_last_block
      ~check_next_block client =
    let* () = Client.bake_until_cycle ~keys:bakers ~target_cycle client in
    let level = target_cycle * blocks_per_cycle in
    let* () =
      check_current_level_and_cycle ~level ~cycle:(target_cycle - 1) client
    in
    let* () =
      if level = migration_level then (
        Log.info
          ~color:Log.Color.FG.green
          "Checking migration block consistency." ;
        block_check
          ~expected_block_type:`Migration
          client
          ~migrate_from
          ~migrate_to)
      else unit
    in
    let* () = check_last_block () in
    Log.info
      ~color:Log.Color.FG.red
      "Bake a block with %s [last activity is at cycle %s]."
      delegate
      (Int.to_string target_cycle) ;
    let* () = Client.bake_for_and_wait ~keys:[delegate] client in
    let* () =
      check_current_level_and_cycle
        ~level:(level + 1)
        ~cycle:target_cycle
        client
    in
    let* () =
      if level = migration_level then (
        Log.info
          ~color:Log.Color.FG.green
          "Checking post-migration block consistency (first block of new \
           protocol %s)."
          (Protocol.name migrate_to) ;
        block_check
          ~expected_block_type:`Non_migration
          client
          ~migrate_from
          ~migrate_to)
      else unit
    in
    let* () = check_next_block () in
    unit

  let print_parameters ~parameter_file ~migration_level =
    let parameters = JSON.parse_file parameter_file in
    let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
    let consensus_rights_delay =
      JSON.(get "consensus_rights_delay" parameters |> as_int)
    in
    Log.info
      ~color:Log.Color.FG.green
      "blocks_per_cycle = %d, consensus_rights_delay = %d, migration_level = %d"
      blocks_per_cycle
      consensus_rights_delay
      migration_level

  let activate_protocol ~parameter_file ~migrate_from ~migrate_to
      ~migration_level =
    Log.info ~color:Log.Color.FG.green "Start node and client." ;
    let* client, node =
      user_migratable_node_init ~migration_level ~migrate_to ()
    in
    Log.info
      ~color:Log.Color.FG.green
      "Activate protocol %s."
      (Protocol.name migrate_from) ;
    let* () =
      Client.activate_protocol ~parameter_file ~protocol:migrate_from client
    in
    return (client, node)

  let check_deactivated (delegate : Account.key) ~expected client =
    let pkh = delegate.public_key_hash in
    let* deactivated =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_delegate_deactivated pkh
    in
    let* grace_period =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_delegate_grace_period pkh
    in
    Log.debug
      ~color:Log.Color.FG.gray
      "Check if %s is deactivated = %s; grace_period = %s"
      delegate.alias
      (JSON.encode deactivated)
      (JSON.encode grace_period) ;
    Check.((JSON.as_bool deactivated = expected) ~__LOC__ bool)
      ~error_msg:"Expected for deactivated=%R, got %L" ;
    unit

  let log_staking_balance ~(delegate : Account.key) client =
    let pkh = delegate.public_key_hash in
    let* raw_json =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_raw_json ~path:["staking_balance"; pkh] ()
    in
    Log.info
      ~color:Log.Color.FG.gray
      "Delegate %s: staking balance = %s"
      delegate.alias
      (JSON.encode raw_json) ;
    unit

  let min_delegated_in_current_cycle_rpc ~(delegate : Account.key) client =
    let pkh = delegate.public_key_hash in
    let* raw_json =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_delegate_min_delegated_in_current_cycle pkh
    in
    let amount = JSON.(raw_json |-> "amount" |> as_int) in
    let level = JSON.(raw_json |-> "level" |-> "level" |> as_int) in
    Log.info
      ~color:Log.Color.FG.gray
      "Delegate %s: min_delegated_in_current_cycle_rpc = %s"
      delegate.alias
      (JSON.encode raw_json) ;
    return (amount, level)
end

let test_unstaked_requests_many_delegates () =
  let migrate_from = Option.get Protocol.(previous_protocol Alpha) in
  let migrate_to = Protocol.Alpha in

  Test.register
    ~__FILE__
    ~title:"protocol migration for unstaked_requests with many delegates"
    ~tags:[team; "protocol"; "migration"; "unstaked_requests"]
  @@ fun () ->
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Left (Protocol.parameter_file migrate_from))
      []
  in
  let parameters = JSON.parse_file parameter_file in
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  (* Migration at the end of cycle 4 *)
  (* delegate_0 unstakes during cycle 0 -> [unstake_requests] will be
     finalizable at the beginning of cycle 4; delegate_0 is a
     representative of the old protocol *)
  let migration_level = 5 * blocks_per_cycle in
  let () = Local_helpers.print_parameters ~parameter_file ~migration_level in

  let* client, _node =
    Local_helpers.activate_protocol
      ~parameter_file
      ~migrate_from
      ~migrate_to
      ~migration_level
  in
  let bake_until_cycle_with_and_check =
    Local_helpers.bake_until_cycle_with_and_check
      ~migration_level
      ~migrate_from
      ~migrate_to
      ~blocks_per_cycle
  in
  (* [default_baker] never gets deactivated *)
  let default_baker = Constant.bootstrap1.alias in
  (* [funder] is used to fund all new accounts *)
  let funder = Constant.bootstrap2.alias in

  (* [delegate_i] unstakes in cycle i *)
  let initial_amount = Tez.of_int 300_000 in
  let amount_to_unstake = Tez.(initial_amount /! 4L) in
  let* delegates =
    Local_helpers.create_delegates_and_stake
      ~baker:default_baker
      ~giver:funder
      ~delegates:
        (List.init 6 (fun i -> ("delegate_" ^ Int.to_string i, initial_amount)))
      client
  in
  let delegates_array = Array.of_list delegates in

  let check_finalizable_list expected_finalizable_status =
    Lwt_list.iteri_s
      (fun i expected ->
        let* () =
          Local_helpers.check_is_finalizable_all_requests
            ~staker:delegates_array.(i)
            ~expected
            client
        in
        unit)
      expected_finalizable_status
  in

  let* () =
    Local_helpers.check_current_level_and_cycle ~level:4 ~cycle:0 client
  in
  let* () =
    Local_helpers.unstake
      ~baker:default_baker
      ~staker:delegates_array.(0).alias
      ~amount:amount_to_unstake
      client
  in
  let* () =
    Lwt_list.iter_s
      (fun target_cycle ->
        let expected_list =
          (* unstaked_frozen_balance =?= 0 *)
          match target_cycle with
          | 1 -> List.init 6 (fun i -> if i = 0 then false else true)
          | 2 -> List.init 6 (fun i -> if i <= 1 then false else true)
          | 3 -> List.init 6 (fun i -> if i <= 2 then false else true)
          | 4 -> List.init 6 (fun i -> if 1 <= i && i <= 3 then false else true)
          | 5 -> List.init 6 (fun i -> if 2 <= i && i <= 4 then false else true)
          | 6 -> List.init 6 (fun i -> if 3 <= i then false else true)
          | 7 -> List.init 6 (fun i -> if 4 <= i then false else true)
          | 8 -> List.init 6 (fun i -> if i = 5 then false else true)
          | 9 -> List.init 6 (fun _i -> true)
          | _ -> failwith "unexpected input"
        in
        let* () =
          bake_until_cycle_with_and_check
            ~bakers:[default_baker]
            ~target_cycle
            ~delegate:default_baker
            ~check_last_block:(fun _ -> unit)
            ~check_next_block:(fun _ -> check_finalizable_list expected_list)
            client
        in
        let* () =
          if target_cycle <= 5 then
            Local_helpers.unstake
              ~baker:default_baker
              ~staker:delegates_array.(target_cycle).alias
              ~amount:amount_to_unstake
              client
          else unit
        in
        unit)
      (* From cycle 1 to 9 *)
      (List.init 9 (fun i -> i + 1))
  in
  unit

let test_unstaked_requests_and_min_delegated () =
  let migrate_from = Option.get Protocol.(previous_protocol Alpha) in
  let migrate_to = Protocol.Alpha in

  Test.register
    ~__FILE__
    ~title:"protocol migration for unstaked_requests and min_delegated"
    ~tags:[team; "protocol"; "migration"; "unstaked_requests"]
  @@ fun () ->
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Left (Protocol.parameter_file migrate_from))
      []
  in
  let parameters = JSON.parse_file parameter_file in
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  (* Migration at the end of cycle 1 *)
  let migration_level = 2 * blocks_per_cycle in
  let () = Local_helpers.print_parameters ~parameter_file ~migration_level in

  let* client, _node =
    Local_helpers.activate_protocol
      ~parameter_file
      ~migrate_from
      ~migrate_to
      ~migration_level
  in
  let bake_until_cycle_with_and_check =
    Local_helpers.bake_until_cycle_with_and_check
      ~migration_level
      ~migrate_from
      ~migrate_to
      ~blocks_per_cycle
  in
  (* [default_baker] never gets deactivated *)
  let default_baker = Constant.bootstrap1.alias in
  (* [funder] is used to fund all new accounts *)
  let funder = Constant.bootstrap2.alias in

  let* delegates =
    Local_helpers.create_delegates_and_stake
      ~baker:default_baker
      ~giver:funder
      ~delegates:
        (List.init 1 (fun i ->
             ("delegate_" ^ Int.to_string i, Tez.of_int 300_000)))
      client
  in
  let delegate_0 = List.hd delegates in

  let bake_until_level_and_unstake ~target_level ~amount
      ~expected_length_unfinalizable ~expected_unstaked_frozen_balance =
    let baker = default_baker in
    let keys = [baker] in
    let staker = delegate_0.alias in
    let* current_level = Local_helpers.get_current_level client in
    let* () =
      if target_level = current_level.level then unit
      else Client.bake_until_level ~keys ~target_level client
    in
    let* () =
      if target_level = migration_level then (
        Log.info
          ~color:Log.Color.FG.green
          "Checking migration block consistency." ;
        let* () =
          block_check
            ~expected_block_type:`Migration
            client
            ~migrate_from
            ~migrate_to
        in
        unit)
      else unit
    in
    let* () = Local_helpers.unstake ~baker ~staker ~amount client in
    let* finalizable, unfinalizable =
      Local_helpers.unstake_requests ~staker:delegate_0 client
    in
    Check.((List.length finalizable = 0) ~__LOC__ int)
      ~error_msg:"Expected length finalizable=%R, got %L" ;
    Check.(
      (List.length unfinalizable = expected_length_unfinalizable) ~__LOC__ int)
      ~error_msg:"Expected length unfinalizable=%R, got %L" ;

    let* unstaked_finalizable_balance, unstaked_frozen_balance =
      Local_helpers.unstaked_balance ~staker:delegate_0 client
    in
    Check.((unstaked_finalizable_balance = 0) ~__LOC__ int)
      ~error_msg:"Expected unstaked_finalizable_balance=%R, got %L" ;
    Check.(
      (unstaked_frozen_balance = expected_unstaked_frozen_balance) ~__LOC__ int)
      ~error_msg:"Expected unstaked_frozen_balance=%R, got %L" ;
    unit
  in

  (* unstake in the first block of the last cycle of migrate_from: L9 *)
  let* () =
    bake_until_level_and_unstake
      ~target_level:(migration_level - blocks_per_cycle)
      ~amount:(Tez.of_int 1_000)
      ~expected_length_unfinalizable:1
      ~expected_unstaked_frozen_balance:1_000_000_000
  in
  (* a new min_delegated for [delegate_0] *)
  let* () =
    Local_helpers.transfer
      ~baker:default_baker
      ~amount:(Tez.of_int 50_000)
      ~giver:delegate_0.alias
      ~receiver:default_baker
      client
  in
  (* unstake in the block before migration: L15 *)
  let* () =
    bake_until_level_and_unstake
      ~target_level:(migration_level - 2)
      ~amount:(Tez.of_int 2_000)
      ~expected_length_unfinalizable:1
      ~expected_unstaked_frozen_balance:3_000_000_000
  in
  let* () = Local_helpers.log_staking_balance ~delegate:delegate_0 client in
  let* amount_before, level_before =
    Local_helpers.min_delegated_in_current_cycle_rpc ~delegate:delegate_0 client
  in
  (* unstake in the migration block: L16 *)
  let* () =
    bake_until_level_and_unstake
      ~target_level:(migration_level - 1)
      ~amount:(Tez.of_int 3_000)
      ~expected_length_unfinalizable:1
      ~expected_unstaked_frozen_balance:6_000_000_000
  in
  let* () = Local_helpers.log_staking_balance ~delegate:delegate_0 client in
  let* amount_after, level_after =
    Local_helpers.min_delegated_in_current_cycle_rpc ~delegate:delegate_0 client
  in
  Check.((amount_after = amount_before) ~__LOC__ int)
    ~error_msg:"Expected amount_after=%R, got %L" ;
  Check.((level_after = level_before) ~__LOC__ int)
    ~error_msg:"Expected level_after=%R, got %L" ;
  (* unstake in the next block after migration: L17 *)
  let* () =
    bake_until_level_and_unstake
      ~target_level:migration_level
      ~amount:(Tez.of_int 4_000)
      ~expected_length_unfinalizable:2
      ~expected_unstaked_frozen_balance:10_000_000_000
  in
  (* unstake in the last block of the first cycle of migrate_to: L24 *)
  let* () =
    bake_until_level_and_unstake
      ~target_level:(migration_level + blocks_per_cycle - 1)
      ~amount:(Tez.of_int 5_000)
      ~expected_length_unfinalizable:2
      ~expected_unstaked_frozen_balance:15_000_000_000
  in
  let* () =
    bake_until_cycle_with_and_check
      ~bakers:[default_baker]
      ~target_cycle:5
      ~delegate:default_baker
      ~check_last_block:(fun _ -> unit)
      ~check_next_block:(fun _ ->
        let* _ = Local_helpers.unstake_requests ~staker:delegate_0 client in
        Local_helpers.check_is_finalizable_all_requests
          ~staker:delegate_0
          ~expected:false
          client)
      client
  in
  let* () =
    bake_until_cycle_with_and_check
      ~bakers:[default_baker]
      ~target_cycle:6
      ~delegate:default_baker
      ~check_last_block:(fun _ -> unit)
      ~check_next_block:(fun _ ->
        let* _ = Local_helpers.unstake_requests ~staker:delegate_0 client in
        Local_helpers.check_is_finalizable_all_requests
          ~staker:delegate_0
          ~expected:true
          client)
      client
  in
  unit

let test_reveal_migration () =
  let migrate_from = Protocol.(previous_protocol S023) in
  match migrate_from with
  | None -> ()
  | Some migrate_from ->
      let migrate_to = Protocol.S023 in

      Test.register
        ~__FILE__
        ~title:"protocol migration for reveal"
        ~tags:[team; "protocol"; "migration"; "reveal"]
      @@ fun () ->
      let* parameter_file =
        Protocol.write_parameter_file
          ~base:(Left (Protocol.parameter_file migrate_from))
          []
      in
      let parameters = JSON.parse_file parameter_file in
      let blocks_per_cycle =
        JSON.(get "blocks_per_cycle" parameters |> as_int)
      in
      (* Migration at the end of cycle 1 *)
      let migration_level = 2 * blocks_per_cycle in
      let () =
        Local_helpers.print_parameters ~parameter_file ~migration_level
      in
      let* client, _node =
        Local_helpers.activate_protocol
          ~parameter_file
          ~migrate_from
          ~migrate_to
          ~migration_level
      in
      let check_not_revealed id =
        let* metadata =
          Client.RPC.call client
          @@ RPC.get_chain_block_context_contract_manager_key ~id ()
        in
        assert (JSON.is_null metadata) ;
        return ()
      in
      let check_revealed id =
        let* metadata =
          Client.RPC.call client
          @@ RPC.get_chain_block_context_contract_manager_key ~id ()
        in
        assert (not (JSON.is_null metadata)) ;
        return ()
      in
      (* Three new key pairs.
     The first one will be revealed before the migration. It should still be revealed after.
     The second will be revealed after, revelation is still possible.
     The third one (tz4) will be revealed before, unrevealed by the migration, and rerevealed after. *)
      let* fresh_account = Client.gen_and_show_keys ~sig_alg:"ed25519" client in
      let* another_fresh_account =
        Client.gen_and_show_keys ~sig_alg:"ed25519" client
      in
      let* fresh_account_tz4 = Client.gen_and_show_keys ~sig_alg:"bls" client in

      Log.info "Transfer tez to new keys" ;
      let* () =
        Client.transfer
          ~burn_cap:Tez.one
          ~amount:Tez.one
          ~giver:"bootstrap1"
          ~receiver:fresh_account.alias
          client
      in
      let* () =
        Client.transfer
          ~burn_cap:Tez.one
          ~amount:Tez.one
          ~giver:"bootstrap2"
          ~receiver:another_fresh_account.alias
          client
      in
      let* () =
        Client.transfer
          ~burn_cap:Tez.one
          ~amount:Tez.one
          ~giver:"bootstrap3"
          ~receiver:fresh_account_tz4.alias
          client
      in
      let* () = Client.bake_for_and_wait client in

      (* No key has been revealed yet *)
      let* () = check_not_revealed fresh_account.public_key_hash in
      let* () = check_not_revealed fresh_account_tz4.public_key_hash in
      let* () = check_not_revealed another_fresh_account.public_key_hash in

      (* Reveal key 1 *)
      Log.info "Reveal %s" fresh_account.alias ;
      let* () = Client.reveal ~src:fresh_account.alias client |> Runnable.run in
      let* () = Client.bake_for_and_wait client in
      let* () = check_revealed fresh_account.public_key_hash in
      let* () = check_not_revealed fresh_account_tz4.public_key_hash in

      (* Reveal key 3 (tz4) *)
      Log.info "Reveal %s" fresh_account_tz4.alias ;
      let* () =
        Client.reveal ~src:fresh_account_tz4.alias client |> Runnable.run
      in
      let* () = Client.bake_for_and_wait client in
      let* () = check_revealed fresh_account.public_key_hash in
      let* () = check_revealed fresh_account_tz4.public_key_hash in

      let* tz4_balance_before =
        Client.RPC.call client
        @@ RPC.get_chain_block_context_contract_balance
             ~id:fresh_account_tz4.public_key_hash
             ()
      in

      (* Wait for migration *)
      Log.info "Bake until migration" ;
      let* () = Client.bake_until_cycle ~target_cycle:2 client in

      (* Check balance after migration *)
      let* tz4_balance_after_migration =
        Client.RPC.call client
        @@ RPC.get_chain_block_context_contract_balance
             ~id:fresh_account_tz4.public_key_hash
             ()
      in
      assert (
        Int64.equal
          (Tez.mutez_int64 tz4_balance_before)
          (Tez.mutez_int64 tz4_balance_after_migration)) ;

      (* Check revelation status. key 3 has been unrevealed. *)
      let* () = check_revealed fresh_account.public_key_hash in
      let* () = check_not_revealed another_fresh_account.public_key_hash in
      let* () = check_not_revealed fresh_account_tz4.public_key_hash in

      (* Reveal key 3 again *)
      Log.info "Reveal %s" fresh_account_tz4.alias ;
      let* () =
        Client.reveal
          ~fee:Tez.(of_mutez_int64 11_111L)
          ~src:fresh_account_tz4.alias
          client
        |> Runnable.run
      in
      let* () = Client.bake_for_and_wait client in
      let* () = check_revealed fresh_account_tz4.public_key_hash in

      (* Reveal key 2 *)
      Log.info "Reveal %s" another_fresh_account.alias ;
      let* () =
        Client.reveal ~src:another_fresh_account.alias client |> Runnable.run
      in
      let* () = Client.bake_for_and_wait client in
      let* () = check_revealed another_fresh_account.public_key_hash in

      (* Check balance after reveal *)
      let* tz4_balance_after_reveal =
        Client.RPC.call client
        @@ RPC.get_chain_block_context_contract_balance
             ~id:fresh_account_tz4.public_key_hash
             ()
      in
      assert (
        Int64.equal
          (Tez.mutez_int64 tz4_balance_before)
          (Int64.add 11_111L (Tez.mutez_int64 tz4_balance_after_reveal))) ;

      return ()

let test_tz4_manager_operation ~with_empty_mempool =
  let migrate_from = Protocol.(previous_protocol S023) in
  match migrate_from with
  | None -> ()
  | Some migrate_from ->
      let migrate_to = Protocol.S023 in

      Test.register
        ~__FILE__
        ~title:
          (sf
             "protocol migration for tz4 manager operation %s empty mempool"
             (if with_empty_mempool then "with" else "without"))
        ~tags:[team; "protocol"; "migration"; "tz4"]
      @@ fun () ->
      let* parameter_file =
        Protocol.write_parameter_file
          ~base:(Left (Protocol.parameter_file migrate_from))
          []
      in
      let parameters = JSON.parse_file parameter_file in
      let blocks_per_cycle =
        JSON.(get "blocks_per_cycle" parameters |> as_int)
      in
      (* Migration at the end of cycle 1 *)
      let migration_level = 2 * blocks_per_cycle in
      let () =
        Local_helpers.print_parameters ~parameter_file ~migration_level
      in
      let* client, _node =
        Local_helpers.activate_protocol
          ~parameter_file
          ~migrate_from
          ~migrate_to
          ~migration_level
      in
      let* tz4_account =
        Client.gen_and_show_keys ~alias:"tz4_account" ~sig_alg:"bls" client
      in
      Log.info ~color:Log.Color.FG.green "Fund a new tz4 account." ;
      let* () =
        Client.transfer
          ~burn_cap:Tez.one
          ~amount:(Tez.of_int 10_000)
          ~giver:Constant.bootstrap1.alias
          ~receiver:tz4_account.alias
          client
      in
      let* () = Client.bake_for_and_wait client in

      let bake_for_with_checks ~num_valid_ops_before ~num_valid_ops_after
          ?with_empty_mempool ~is_balance_eq (account : Account.key) client =
        Log.info ~color:Log.Color.FG.green "Before baking the block." ;
        let* mempool_before =
          Mempool.get_mempool ~validation_passes:[3] client
        in
        Check.(
          (List.length mempool_before.validated = num_valid_ops_before)
            int
            ~error_msg:
              "validated (before) mempool should contain only %R operations, \
               got %L") ;
        let* balance_before =
          Client.RPC.call client
          @@ RPC.get_chain_block_context_contract_balance
               ~id:account.public_key_hash
               ()
        in

        let with_empty_mempool =
          Option.value ~default:false with_empty_mempool
        in
        let* () =
          if with_empty_mempool then (
            Log.info
              ~color:Log.Color.FG.green
              "Bake a block with an empty mempool" ;
            let empty_mempool_file = Client.empty_mempool_file () in
            Client.bake_for_and_wait
              ~mempool:empty_mempool_file
              ~ignore_node_mempool:true
              client)
          else Client.bake_for_and_wait client
        in

        Log.info ~color:Log.Color.FG.green "After baking the block." ;
        let* mempool_after =
          Mempool.get_mempool ~validation_passes:[3] client
        in
        let* balance_after =
          Client.RPC.call client
          @@ RPC.get_chain_block_context_contract_balance
               ~id:account.public_key_hash
               ()
        in
        Check.(
          (List.length mempool_after.validated = num_valid_ops_after)
            int
            ~error_msg:
              "validated (after) mempool should contain only %R operations, \
               got %L") ;

        if is_balance_eq then
          Check.(
            (Tez.to_mutez balance_before = Tez.to_mutez balance_after)
              ~__LOC__
              int)
            ~error_msg:"Expected balance %R to be equal to %L"
        else
          Check.(
            (Tez.to_mutez balance_before > Tez.to_mutez balance_after)
              ~__LOC__
              int)
            ~error_msg:"Expected balance %R to be less than %L" ;
        unit
      in

      Log.info
        ~color:Log.Color.FG.green
        "Reveal tz4 account by making a transfer." ;
      let* () =
        Client.transfer
          ~burn_cap:Tez.one
          ~amount:(Tez.of_int 500)
          ~giver:tz4_account.alias
          ~receiver:Constant.bootstrap2.alias
          client
      in
      (* Tz4 manager operation is included in the block and applied *)
      let* () =
        bake_for_with_checks
          ~num_valid_ops_before:1
          ~num_valid_ops_after:0
          ~is_balance_eq:false
          tz4_account
          client
      in

      Log.info ~color:Log.Color.FG.green "Bake just 1 level before migration" ;
      let* () =
        Client.bake_until_level ~target_level:(migration_level - 1) client
      in

      Log.info
        ~color:Log.Color.FG.green
        "Tz4 manager operation is in a mempool just before the migration" ;
      let* () =
        Client.transfer
          ~burn_cap:Tez.one
          ~amount:(Tez.of_int 300)
          ~giver:tz4_account.alias
          ~receiver:Constant.bootstrap1.alias
          client
      in

      let* () =
        (* When baking with an empty mempool, tz4 manager operation is not
       included in the block *)
        let is_balance_eq = if with_empty_mempool then true else false in
        bake_for_with_checks
          ~num_valid_ops_before:1
          ~num_valid_ops_after:0
          ~with_empty_mempool
          ~is_balance_eq
          tz4_account
          client
      in
      Log.info ~color:Log.Color.FG.green "Checking migration block consistency" ;
      let* () =
        block_check
          ~expected_block_type:`Migration
          client
          ~migrate_from
          ~migrate_to
      in

      (* After the migration, mempool is empty *)
      let* () =
        bake_for_with_checks
          ~num_valid_ops_before:0
          ~num_valid_ops_after:0
          ~is_balance_eq:true
          tz4_account
          client
      in
      Log.info
        ~color:Log.Color.FG.green
        "Checking post-migration block consistency (first block of new \
         protocol %s)."
        (Protocol.name migrate_to) ;
      let* () =
        block_check
          ~expected_block_type:`Non_migration
          client
          ~migrate_from
          ~migrate_to
      in

      Log.info
        ~color:Log.Color.FG.green
        "New tz4 manager operation is in a mempool after the migration" ;
      let* () =
        Client.transfer
          ~burn_cap:Tez.one
          ~amount:(Tez.of_int 400)
          ~giver:tz4_account.alias
          ~receiver:Constant.bootstrap1.alias
          client
      in
      let* mempool = Mempool.get_mempool client in
      let op_hash = List.hd mempool.validated in
      let* () =
        bake_for_with_checks
          ~num_valid_ops_before:1
          ~num_valid_ops_after:0
          ~is_balance_eq:false
          tz4_account
          client
      in
      let* receipt = Client.get_receipt_for ~operation:op_hash client in
      Log.info ~color:Log.Color.FG.gray "receipt for %s:\n%s" op_hash receipt ;
      if receipt =~ rex "Revelation of manager public key" then unit
      else
        Test.fail
          "Reveal of tz4 account is not included in the block: %s"
          receipt

let register ~migrate_from ~migrate_to =
  test_migration_for_whole_cycle ~migrate_from ~migrate_to ;
  test_migration_with_bakers ~migrate_from ~migrate_to () ;
  test_forked_migration_bakers ~migrate_from ~migrate_to ;
  test_forked_migration_manual ~migrate_from ~migrate_to () ;
  test_migration_with_snapshots ~migrate_from ~migrate_to ;
  test_unstaked_requests_many_delegates () ;
  test_unstaked_requests_and_min_delegated () ;
  test_reveal_migration () ;
  test_tz4_manager_operation ~with_empty_mempool:true ;
  test_tz4_manager_operation ~with_empty_mempool:false
