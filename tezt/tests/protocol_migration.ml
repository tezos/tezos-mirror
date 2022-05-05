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

(* Migration to Tenderbake is only supported after the first cycle,
   therefore at [migration_level >= blocks_per_cycle].  *)
let test_protocol_migration ~blocks_per_cycle ~migration_level ~migrate_from
    ~migrate_to =
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "protocol migration at level %d" migration_level)
    ~tags:["protocol"; "migration"; "sandbox"]
  @@ fun () ->
  assert (migration_level >= blocks_per_cycle) ;
  let node = Node.create [] in
  let* () = Node.config_init node [] in
  Node.Config_file.(
    update
      node
      (set_sandbox_network_with_user_activated_upgrades
         [(migration_level, migrate_to)])) ;
  Log.info "Node starting" ;
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  Log.info "Node initialized" ;
  let* client = Client.(init ~endpoint:(Node node) ()) in
  let* () = Client.activate_protocol ~protocol:migrate_from client in
  Log.info "Protocol activated" ;
  (* Bake until migration *)
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  (* Ensure that we did migrate *)
  let* migration_block = RPC.get_block_metadata ~block:"2" client in
  let protocol = JSON.(migration_block |-> "protocol" |> as_string) in
  Log.info "Checking migration block consistency" ;
  Check.(
    (protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  let next_protocol = JSON.(migration_block |-> "next_protocol" |> as_string) in
  Check.(
    (next_protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  (* Test that we can still bake after migration *)
  let* () = repeat 5 (fun () -> Client.bake_for_and_wait client) in
  unit

(** Test all levels for one cycle, after the first cycle. *)
let test_migration_for_whole_cycle ~migrate_from ~migrate_to =
  let parameters = JSON.parse_file (Protocol.parameter_file migrate_to) in
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  for migration_level = blocks_per_cycle to 2 * blocks_per_cycle do
    test_protocol_migration
      ~blocks_per_cycle
      ~migration_level
      ~migrate_from
      ~migrate_to
  done

(** Boilerplate code to create a user-migratable node. Used in the tests below. **)
let user_migratable_node_init ~migration_level ~migrate_to =
  let node = Node.create [] in
  (* Calling identity_generate is necessary whenever needs to use peer-related
   * functions, such as kick_peer *)
  let* () = Node.identity_generate node in
  let* () = Node.config_init node [] in
  Node.Config_file.(
    update
      node
      (set_sandbox_network_with_user_activated_upgrades
         [(migration_level, migrate_to)])) ;
  let* () = Node.run node [Synchronisation_threshold 0; Private_mode] in
  let* () = Node.wait_for_ready node in
  let* client = Client.(init ~endpoint:(Node node) ()) in
  Lwt.return (client, node)

(** [block_check ~level ~expected_block_type ~migrate_to ~migrate_from client]
    is generic check that a block of type [expected_block_type] contains
    (protocol) metatadata conforming to its type at [level]. **)
let block_check ?level ~expected_block_type ~migrate_to ~migrate_from client =
  let block =
    match level with Some level -> Some (string_of_int level) | None -> None
  in
  let* metadata = RPC.get_block_metadata ?block client in
  let protocol = JSON.(metadata |-> "protocol" |> as_string) in
  let next_protocol = JSON.(metadata |-> "next_protocol" |> as_string) in
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

(** [get_endorsement_power ~level client] retrieves the number of endorsements,
    aka its power, for level [level - 1] at block [level]. *)
let get_endorsement_power ~level client =
  let block = string_of_int level in
  let* consensus_operations =
    let consensus_pass = 0 in
    RPC.get_operations_of_validation_pass
      ~block
      ~validation_pass:consensus_pass
      client
  in
  Lwt.return
  @@
  let open JSON in
  as_list consensus_operations
  |> List.fold_left
       (fun acc ops ->
         acc
         + (JSON.get "contents" ops |> JSON.as_list
           |> List.fold_left
                (fun acc elt ->
                  if
                    String.equal
                      JSON.(get "kind" elt |> as_string)
                      "endorsement"
                    && JSON.(get "level" elt |> as_int) = level - 1
                  then
                    let power =
                      JSON.(
                        get "metadata" elt |> get "endorsement_power" |> as_int)
                    in
                    power + acc
                  else acc)
                0))
       0

(** [check_block_has_no_endorsements ~level ~client] checks that the block at
   [level] contains no endorsements. *)
let check_block_has_no_endorsements ~level client =
  let* endorsement_power = get_endorsement_power ~level client in
  Lwt.return
  @@ Check.(
       (endorsement_power = 0)
         int
         ~error_msg:"Expected zero endorsements, got %L")

(** [check_block_has_no_endorsements ~level ~client] checks that the block at
   [level] contains no endorsements. *)
let check_block_has_endorsements ~level client =
  let* endorsement_power = get_endorsement_power ~level client in
  Lwt.return
  @@ Check.(
       (endorsement_power >= 0) int ~error_msg:"Expected non-zero endorsements")

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
  let block_interval = JSON.(get "minimal_block_delay" parameters |> as_int) in
  let minimal_block_delay =
    match round_duration with
    | None -> block_interval
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
        (["consensus_threshold"], Some (string_of_int consensus_threshold));
        ( ["minimal_block_delay"],
          Some (Printf.sprintf "\"%d\"" minimal_block_delay) );
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
  let* (client, node) =
    user_migratable_node_init ~migration_level ~migrate_to
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
    check_block_has_no_endorsements ~level:(migration_level + 1) client
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
  let* metadata = RPC.get_block_metadata ~block client in
  Lwt.return JSON.(metadata |-> "proposer" |> as_string)

let all_account_keys =
  List.map (fun b -> b.Account.alias) (Array.to_list Account.Bootstrap.keys)

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
    user_migratable_node_init ~migrate_to ~migration_level
  in
  let* ((client_2, node_2) as cn2) =
    user_migratable_node_init ~migrate_to ~migration_level
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
    repeat n (fun () ->
        let* () =
          Client.propose_for
            ~key:all_account_keys
            ~protocol:migrate_from
            client_2
            ~minimal_timestamp:true
            ~force:true
        in
        let* () =
          Client.preendorse_for
            ~protocol:migrate_from
            ~key:all_account_keys
            client_2
            ~force:true
        in
        Client.endorse_for
          ~key:all_account_keys
          client_2
          ~protocol:migrate_from
          ~force:true)
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

(** This test aims at checking that baker daemons behave correctly (i.e., the
   chain is not stuck, nor does it split) in a scenario where 3 nodes get
   different migration blocks after being disconnected (getting two subgroups of
   2 and 1 node respectively) then are reconnected together. The expected
   behavior is only one of the migration block is chosen, the nodes are
   correctly synced, and the chain goes on its merry way.

   The test may exhibit some flakiness around migration due to waiting times.

   @param migration_level is the level at which migration will occur

   @param num_blocks_post_migration is how many blocks the daemons will bake on
   top of the migration block

   @param migrate_from is the protocol from which we will migrate

   @param migrate_to is the protocol to which we will migrate **)
let test_forked_migration_bakers ?(migration_level = 4)
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
        "flaky";
        "from_" ^ Protocol.tag migrate_from;
        "to_" ^ Protocol.tag migrate_to;
      ]
    ~title:
      (Printf.sprintf
         "baker forked migration blocks from %s to %s"
         (Protocol.tag migrate_from)
         (Protocol.tag migrate_to))
  @@ fun () ->
  let* ((client_1, node_1) as cn1) =
    user_migratable_node_init ~migrate_to ~migration_level
  in
  let* ((client_2, node_2) as cn2) =
    user_migratable_node_init ~migrate_to ~migration_level
  in
  let* ((client_3, node_3) as cn3) =
    user_migratable_node_init ~migrate_to ~migration_level
  in

  let* () = connect cn1 cn2
  and* () = connect cn1 cn3
  and* () = connect cn2 cn3 in

  let round_duration = 5 in
  let* () =
    (* Activate a protocol where everyone needs to sign off a block proposal
       for it to be included -- set consensus_threshold to 100% of consensus_committee_size *)
    start_protocol
      ~expected_bake_for_blocks:migration_level
      client_1
      ~protocol:migrate_from
      ~consensus_threshold:(fun ~consensus_committee_size ->
        consensus_committee_size)
      ~round_duration
  in

  let baker_for_proto ?wait_for_ready ~delegates ~node ~client ~protocol () =
    let name =
      Printf.sprintf
        "baker-proto-%s-on-%s"
        (Protocol.name protocol)
        (Node.name node)
    in
    match wait_for_ready with
    | Some true | None -> Baker.init ~protocol ~name node client ~delegates
    | Some false ->
        let* () = Node.wait_for_ready node in
        let baker = Baker.create ~protocol ~name ~delegates node client in
        let* () = Baker.run baker in
        return baker
  in

  (* Split bootstrap delegates into 2 groups that will not be able to make
     progress independently. Indeed the required quorum is 100% and none has it
     individually. *)
  let delegates_1 = Constant.[bootstrap1.alias; bootstrap2.alias] in
  let delegates_2 = Constant.[bootstrap3.alias; bootstrap4.alias] in
  let delegates_3 = [Constant.bootstrap5.alias] in

  Log.info "Start first baker for %s" (Protocol.name migrate_to) ;

  let* _to_1 =
    baker_for_proto
      ~wait_for_ready:false
      ~delegates:delegates_1
      ~client:client_1
      ~node:node_1
      ~protocol:migrate_to
      ()
  in
  Log.info "Start second baker for %s" (Protocol.name migrate_to) ;
  let* _to_2 =
    baker_for_proto
      ~wait_for_ready:false
      ~delegates:delegates_2
      ~node:node_2
      ~client:client_2
      ~protocol:migrate_to
      ()
  in
  Log.info "Start third baker for %s" (Protocol.name migrate_to) ;
  let* _to_3 =
    baker_for_proto
      ~wait_for_ready:false
      ~delegates:delegates_3
      ~node:node_3
      ~client:client_3
      ~protocol:migrate_to
      ()
  in

  Log.info "Start first baker for %s" (Protocol.name migrate_from) ;
  let* _from_1 =
    baker_for_proto
      ~delegates:delegates_1
      ~client:client_1
      ~node:node_1
      ~protocol:migrate_from
      ()
  in

  Log.info "Start second baker for %s" (Protocol.name migrate_from) ;
  let* _from_2 =
    baker_for_proto
      ~delegates:delegates_2
      ~node:node_2
      ~client:client_2
      ~protocol:migrate_from
      ()
  in

  Log.info "Start third baker for %s" (Protocol.name migrate_from) ;
  let* _from_3 =
    baker_for_proto
      ~delegates:delegates_3
      ~node:node_3
      ~client:client_3
      ~protocol:migrate_from
      ()
  in

  Log.info "Wait for pre-migration level %d on node3." (migration_level - 1) ;
  let* _ = Node.wait_for_level node_3 (migration_level - 1) in

  let waiting_time = round_duration - 1 in
  (* [round_duration - 1] seconds to almost finish round 0 at level
     [migration_level - 1]. By this moment, there should be an
     endorsement quorum, so all bakers could advance to migration
     level. *)
  Log.info "Waiting %d seconds." waiting_time ;
  let* () = Lwt_unix.sleep (float waiting_time) in

  Log.info "Disconnecting node %s." (Node.name node_3) ;
  let* () = disconnect (client_1, node_1) (client_3, node_3) in
  let* () = disconnect (client_2, node_2) (client_3, node_3) in

  (* There should be no endorsement quorum for the migration block,
     because bootstrap5 does not see the proposal at the migration
     level, round 0 (assuming it is proposed by another delegate). *)
  let waiting_time = 1 + round_duration + 1 in
  (* 1 second from the previous wait, [round_duration] seconds to
     finish round 0 at level 4, plus 1 second, to be sure that round 0
     (at migration level) is "lost". *)
  Log.info "Waiting %d seconds." waiting_time ;
  let* () = Lwt_unix.sleep (float waiting_time) in

  Log.info "Re-connect node 3." ;
  let* () = connect cn3 cn1 and* () = connect cn3 cn2 in

  let post_migration = migration_level + num_blocks_post_migration in
  Log.info
    "Waiting on nodes %s & %s to reach level %d"
    (Node.name node_1)
    (Node.name node_2)
    post_migration ;

  let* _ = Node.wait_for_level node_1 post_migration
  and* _ = Node.wait_for_level node_2 post_migration
  and* _ = Node.wait_for_level node_3 post_migration in

  let loop ~_from ~_to () =
    let rec aux level =
      if level > _to then Lwt.return_unit
      else (
        Log.info "Checking level %d" level ;
        let block_id = string_of_int level in
        let* block_content_1 = RPC.get_block ~block:block_id client_1
        and* block_content_2 = RPC.get_block ~block:block_id client_2 in
        Check.(
          (JSON.encode block_content_1 = JSON.encode block_content_2)
            string
            ~error_msg:"Block content %L is not %R") ;
        let* () =
          (if level = migration_level then check_block_has_no_endorsements
          else check_block_has_endorsements)
            ~level
            client_1
        in
        aux (level + 1))
    in
    aux _from
  in
  (* Node may differ at current level, but should not 2 levels below, according
     to Tenderbake deterministic finality. *)
  loop ~_from:(migration_level + 1) ~_to:(post_migration - 2) ()

let register ~migrate_from ~migrate_to =
  test_migration_for_whole_cycle ~migrate_from ~migrate_to ;
  test_migration_with_bakers ~migrate_from ~migrate_to () ;
  test_forked_migration_bakers ~migrate_from ~migrate_to () ;
  test_forked_migration_manual ~migrate_from ~migrate_to ()
