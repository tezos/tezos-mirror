(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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
   Component:    Smart Contract Optimistic Rollups
   Invocation:   dune exec tezt/tests/main.exe -- --file sc_rollup.ml
*)

open Base

let hooks = Tezos_regression.hooks

(*

   Helpers
   =======

*)

let hex_encode (input : string) : string =
  match Hex.of_string input with `Hex s -> s

(* [read_kernel filename] reads binary encoded WebAssembly module (e.g. `foo.wasm`)
   and returns a hex-encoded Wasm PVM boot sector, suitable for passing to
   [originate_sc_rollup] or [with_fresh_rollup].

   See also [wasm_incomplete_kernel_boot_sector].

   Note that this uses [Tezos_scoru_wasm.Gather_floppies.Complete_kernel], so
   the kernel must fit into a single Tezos operation.
*)
let read_kernel name : string =
  let open Tezt.Base in
  let kernel_file =
    project_root // Filename.dirname __FILE__
    // "../../src/proto_alpha/lib_protocol/test/integration/wasm_kernel"
    // (name ^ ".wasm")
  in
  hex_encode (read_file kernel_file)

(* Number of levels needed to process a head as finalized. This value should
   be the same as `node_context.block_finality_time`, where `node_context` is
   the `Node_context.t` used by the rollup node. For Tenderbake, the
   block finality time is 2. *)
let block_finality_time = 2

type sc_rollup_constants = {
  origination_size : int;
  challenge_window_in_blocks : int;
  max_number_of_messages_per_commitment_period : int;
  stake_amount : Tez.t;
  commitment_period_in_blocks : int;
  max_lookahead_in_blocks : int32;
  max_active_outbox_levels : int32;
  max_outbox_messages_per_level : int;
  number_of_sections_in_dissection : int;
  timeout_period_in_blocks : int;
}

(** [boot_sector_of k] returns a valid boot sector for a PVM of
    kind [kind]. *)
let boot_sector_of = function
  | "arith" -> ""
  | "wasm_2_0_0" -> Constant.wasm_incomplete_kernel_boot_sector
  | kind -> raise (Invalid_argument kind)

let get_sc_rollup_constants client =
  let* json =
    RPC.Client.call client @@ RPC.get_chain_block_context_constants ()
  in
  let open JSON in
  let origination_size = json |-> "sc_rollup_origination_size" |> as_int in
  let challenge_window_in_blocks =
    json |-> "sc_rollup_challenge_window_in_blocks" |> as_int
  in
  let max_number_of_messages_per_commitment_period =
    json |-> "sc_rollup_max_number_of_messages_per_commitment_period" |> as_int
  in
  let stake_amount =
    json |-> "sc_rollup_stake_amount" |> as_string |> Int64.of_string
    |> Tez.of_mutez_int64
  in
  let commitment_period_in_blocks =
    json |-> "sc_rollup_commitment_period_in_blocks" |> as_int
  in
  let max_lookahead_in_blocks =
    json |-> "sc_rollup_max_lookahead_in_blocks" |> as_int32
  in
  let max_active_outbox_levels =
    json |-> "sc_rollup_max_active_outbox_levels" |> as_int32
  in
  let max_outbox_messages_per_level =
    json |-> "sc_rollup_max_outbox_messages_per_level" |> as_int
  in
  let number_of_sections_in_dissection =
    json |-> "sc_rollup_number_of_sections_in_dissection" |> as_int
  in
  let timeout_period_in_blocks =
    json |-> "sc_rollup_timeout_period_in_blocks" |> as_int
  in
  return
    {
      origination_size;
      challenge_window_in_blocks;
      max_number_of_messages_per_commitment_period;
      stake_amount;
      commitment_period_in_blocks;
      max_lookahead_in_blocks;
      max_active_outbox_levels;
      max_outbox_messages_per_level;
      number_of_sections_in_dissection;
      timeout_period_in_blocks;
    }

(* List of scoru errors messages used in tests below. *)

let commit_too_recent =
  "Attempted to cement a commitment before its refutation deadline"

let parent_not_lcc = "Parent is not the last cemented commitment"

let disputed_commit = "Attempted to cement a disputed commitment"

let commit_doesnt_exit = "Commitment scc\\w+\\sdoes not exist"

let make_parameter name = function
  | None -> []
  | Some value -> [([name], `Int value)]

let test ~__FILE__ ?(tags = []) title f =
  let tags = "sc_rollup" :: tags in
  Protocol.register_test ~__FILE__ ~title ~tags f

let regression_test ~__FILE__ ?(tags = []) title f =
  let tags = "sc_rollup" :: tags in
  Protocol.register_regression_test ~__FILE__ ~title ~tags f

let setup ?commitment_period ?challenge_window ?timeout f ~protocol =
  let parameters =
    make_parameter "sc_rollup_commitment_period_in_blocks" commitment_period
    @ make_parameter "sc_rollup_challenge_window_in_blocks" challenge_window
    @ make_parameter "sc_rollup_timeout_period_in_blocks" timeout
    @ [(["sc_rollup_enable"], `Bool true)]
  in
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base parameters in
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ~nodes_args ()
  in
  let operator = Constant.bootstrap1.alias in
  f node client operator

let get_sc_rollup_commitment_period_in_blocks client =
  let* constants = get_sc_rollup_constants client in
  return constants.commitment_period_in_blocks

let sc_rollup_node_rpc sc_node service =
  let* curl = RPC.Curl.get () in
  match curl with
  | None -> return None
  | Some curl ->
      let url =
        Printf.sprintf "%s/%s" (Sc_rollup_node.endpoint sc_node) service
      in
      let* response = curl ~url in
      return (Some response)

type test = {variant : string; tags : string list; description : string}

(** This helper injects an SC rollup origination via octez-client. Then it
    bakes to include the origination in a block. It returns the address of the
    originated rollup *)
let originate_sc_rollup ?(hooks = hooks) ?(burn_cap = Tez.(of_int 9999999))
    ?(src = "bootstrap1") ?(kind = "arith") ?(parameters_ty = "string")
    ?(boot_sector = boot_sector_of kind) client =
  let* sc_rollup =
    Client.Sc_rollup.(
      originate ~hooks ~burn_cap ~src ~kind ~parameters_ty ~boot_sector client)
  in
  let* () = Client.bake_for_and_wait client in
  return sc_rollup

(* Configuration of a rollup node
   ------------------------------

   A rollup node has a configuration file that must be initialized.
*)
let with_fresh_rollup ?kind ?boot_sector f tezos_node tezos_client operator =
  let* sc_rollup =
    originate_sc_rollup ?kind ?boot_sector ~src:operator tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      tezos_node
      tezos_client
      ~default_operator:operator
  in
  let* configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node sc_rollup
  in
  f sc_rollup sc_rollup_node configuration_filename

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2933
   Many tests can be refactored using test_scenario. *)
let test_scenario ~kind ?boot_sector ?commitment_period ?challenge_window
    ?timeout {variant; tags; description} scenario =
  let tags = tags @ [kind; variant] in
  regression_test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s - %s (%s)" kind description variant)
    (fun protocol ->
      setup ?commitment_period ?challenge_window ~protocol ?timeout
      @@ fun node client ->
      ( with_fresh_rollup ~kind ?boot_sector
      @@ fun sc_rollup sc_rollup_node _filename ->
        scenario protocol sc_rollup_node sc_rollup node client )
        node
        client)

let inbox_level (_hash, (commitment : Sc_rollup_client.commitment), _level) =
  commitment.inbox_level

let number_of_ticks (_hash, (commitment : Sc_rollup_client.commitment), _level)
    =
  commitment.number_of_ticks

let last_cemented_commitment_hash_with_level ~sc_rollup client =
  let* json =
    RPC.Client.call client
    @@ RPC
       .get_chain_block_context_sc_rollup_last_cemented_commitment_hash_with_level
         sc_rollup
  in
  let hash = JSON.(json |-> "hash" |> as_string) in
  let level = JSON.(json |-> "level" |> as_int) in
  return (hash, level)

let get_staked_on_commitment ~sc_rollup ~staker client =
  let* json =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_sc_rollup_staker_staked_on_commitment
         ~sc_rollup
         staker
  in
  let hash = JSON.(json |-> "hash" |> as_string) in
  return hash

let hash (hash, (_ : Sc_rollup_client.commitment), _level) = hash

let first_published_at_level (_hash, (_ : Sc_rollup_client.commitment), level) =
  level

let predecessor (_hash, {Sc_rollup_client.predecessor; _}, _level) = predecessor

let cement_commitment ?(src = "bootstrap1") ?fail ~sc_rollup ~hash client =
  let p =
    Client.Sc_rollup.cement_commitment ~hooks ~dst:sc_rollup ~src ~hash client
  in
  match fail with
  | None ->
      let*! () = p in
      Client.bake_for_and_wait client
  | Some failure ->
      let*? process = p in
      Process.check_error ~msg:(rex failure) process

let publish_commitment ?(src = Constant.bootstrap1.public_key_hash) ~commitment
    client sc_rollup =
  let ({compressed_state; inbox_level; predecessor; number_of_ticks}
        : Sc_rollup_client.commitment) =
    commitment
  in
  Client.Sc_rollup.publish_commitment
    ~hooks
    ~src
    ~sc_rollup
    ~compressed_state
    ~inbox_level
    ~predecessor
    ~number_of_ticks
    client

(*

   Tests
   =====

*)

(* Originate a new SCORU
   ---------------------

   - Rollup addresses are fully determined by operation hashes and origination nonce.
*)
let test_origination ~kind =
  regression_test
    ~tags:["sc_rollup"; kind]
    ~__FILE__
    (Format.asprintf "%s - origination of a SCORU executes without error" kind)
    (fun protocol ->
      setup ~protocol @@ fun _node client bootstrap1_key ->
      let* _sc_rollup = originate_sc_rollup ~kind ~src:bootstrap1_key client in
      unit)

(* Initialize configuration
   ------------------------

   Can use CLI to initialize the rollup node config file
*)
let test_rollup_node_configuration ~kind =
  regression_test
    ~__FILE__
    ~tags:["sc_rollup"]
    "configuration of a smart contract optimistic rollup node"
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup ~kind
      @@ fun _sc_rollup _sc_rollup_node filename ->
      let read_configuration =
        let open Ezjsonm in
        match from_channel (open_in filename) with
        | `O fields ->
            (* Remove "data-dir" and "rpc-port" as they are non deterministic. *)
            `O
              (List.filter
                 (fun (s, _) ->
                   match s with "data-dir" | "rpc-port" -> false | _ -> true)
                 fields)
            |> to_string
        | _ ->
            failwith "The configuration file does not have the expected format."
      in
      Log.info "Read configuration:\n %s" read_configuration ;
      return ())

(* Launching a rollup node
   -----------------------

   A running rollup node can be asked the address of the rollup it is
   interacting with.
*)
let test_rollup_node_running ~kind =
  test
    ~__FILE__
    ~tags:["sc_rollup"; "run"; kind]
    (Format.asprintf "%s - running a smart contract rollup node" kind)
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup ~kind
      @@ fun sc_rollup sc_rollup_node _filename ->
      let* () = Sc_rollup_node.run sc_rollup_node in
      let* sc_rollup_from_rpc =
        sc_rollup_node_rpc sc_rollup_node "global/sc_rollup_address"
      in
      match sc_rollup_from_rpc with
      | None ->
          (* No curl, no check. *)
          failwith "Please install curl"
      | Some sc_rollup_from_rpc ->
          let sc_rollup_from_rpc = JSON.as_string sc_rollup_from_rpc in
          if sc_rollup_from_rpc <> sc_rollup then
            failwith
              (Printf.sprintf
                 "Expecting %s, got %s when we query the sc rollup node RPC \
                  address"
                 sc_rollup
                 sc_rollup_from_rpc)
          else return ())

(* Interacting with a rollup node through a rollup client
   ------------------------------------------------------

   When a rollup node is running, a rollup client can ask this
   node its rollup address.
*)
let test_rollup_client_gets_address ~kind =
  regression_test
    ~__FILE__
    ~tags:["sc_rollup"; "run"; "client"]
    "getting a smart-contract rollup address through the client"
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup ~kind
      @@ fun sc_rollup sc_rollup_node _filename ->
      let* () = Sc_rollup_node.run sc_rollup_node in
      let sc_client = Sc_rollup_client.create sc_rollup_node in
      let* sc_rollup_from_client =
        Sc_rollup_client.sc_rollup_address sc_client
      in
      if sc_rollup_from_client <> sc_rollup then
        failwith
          (Printf.sprintf
             "Expecting %s, got %s when the client asks for the sc rollup \
              address"
             sc_rollup
             sc_rollup_from_client) ;
      return ())

(* Fetching the initial level of a sc rollup
    -----------------------------------------

   We can fetch the level when a smart contract rollup was
   originated from the context.
*)
let test_rollup_get_genesis_info ~kind =
  regression_test
    ~__FILE__
    ~tags:["sc_rollup"; "genesis_info"; kind]
    (Format.asprintf "%s - get genesis info of a sc rollup" kind)
    (fun protocol ->
      setup ~protocol @@ fun node client bootstrap ->
      let current_level = Node.get_level node in
      ( with_fresh_rollup ~kind @@ fun sc_rollup _sc_rollup_node _filename ->
        (* Bake 10 blocks to be sure that the initial level of rollup is different
           from the current level. *)
        let* _ = repeat 10 (fun () -> Client.bake_for_and_wait client) in
        let* genesis_info =
          RPC.Client.call client
          @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
        in
        (* 1 Block for activating alpha + 1 block for originating the rollup
           the rollup initial level should be 2 *)
        Check.(
          (JSON.(genesis_info |-> "level" |> as_int) = current_level + 1)
            int
            ~error_msg:"expected value %L, got %R") ;
        return () )
        node
        client
        bootstrap)

(* Fetching the last cemented commitment info for a sc rollup
    ----------------------------------------------------------

   We can fetch the hash and level of the last cemented commitment. Initially,
   this corresponds to `(Sc_rollup.Commitment_hash.zero, origination_level)`.
*)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2944
   Revisit this test once the rollup node can cement commitments. *)
let test_rollup_get_chain_block_context_sc_rollup_last_cemented_commitment_hash_with_level
    ~kind =
  regression_test
    ~__FILE__
    ~tags:["sc_rollup"; "lcc_hash_with_level"; kind]
    (Format.asprintf
       "%s - get last cemented commitment hash and inbox level of a sc rollup"
       kind)
    (fun protocol ->
      setup ~protocol @@ fun node client bootstrap ->
      ( with_fresh_rollup ~kind @@ fun sc_rollup _sc_rollup_node _filename ->
        let origination_level = Node.get_level node in

        (* Bake 10 blocks to be sure that the origination_level of rollup is different
           from the level of the head node. *)
        let* () = repeat 10 (fun () -> Client.bake_for_and_wait client) in
        let* hash, level =
          last_cemented_commitment_hash_with_level ~sc_rollup client
        in
        let* genesis_info =
          RPC.Client.call client
          @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
        in
        let genesis_hash =
          JSON.(genesis_info |-> "commitment_hash" |> as_string)
        in
        Check.(
          (hash = genesis_hash) string ~error_msg:"expected value %L, got %R") ;
        (* The level of the last cemented commitment should correspond to the
           rollup origination level. *)
        Check.(
          (level = origination_level) int ~error_msg:"expected value %L, got %R") ;
        return () )
        node
        client
        bootstrap)

(* Pushing message in the inbox
   ----------------------------

   A message can be pushed to a smart-contract rollup inbox through
   the Tezos node. Then we can observe that the messages are included in the
   inbox.
*)
let send_message ?(src = Constant.bootstrap2.alias) client msg =
  let* () = Client.Sc_rollup.send_message ~hooks ~src ~msg client in
  Client.bake_for_and_wait client

let send_messages ?src ?batch_size n client =
  let messages =
    List.map
      (fun i ->
        let batch_size = match batch_size with None -> i | Some v -> v in
        let json =
          `A (List.map (fun _ -> `String "CAFEBABE") (range 1 batch_size))
        in
        "text:" ^ Ezjsonm.to_string json)
      (range 1 n)
  in
  Lwt_list.iter_s (fun msg -> send_message ?src client msg) messages

let to_text_messages_arg msgs =
  let json = Ezjsonm.list Ezjsonm.string msgs in
  "text:" ^ Ezjsonm.to_string ~minify:true json

let send_text_messages ?src client msgs =
  send_message ?src client (to_text_messages_arg msgs)

let parse_inbox json =
  let go () =
    let inbox = JSON.as_object json in
    return
      ( List.assoc "current_level_hash" inbox |> JSON.as_string,
        List.assoc "nb_messages_in_commitment_period" inbox |> JSON.as_int )
  in
  Lwt.catch go @@ fun exn ->
  failwith
    (Printf.sprintf
       "Unable to parse inbox %s\n%s"
       (JSON.encode json)
       (Printexc.to_string exn))

let get_inbox_from_tezos_node client =
  let* inbox =
    RPC.Client.call client @@ RPC.get_chain_block_context_sc_rollups_inbox ()
  in
  parse_inbox inbox

let get_inbox_from_sc_rollup_node sc_rollup_node =
  let* inbox = sc_rollup_node_rpc sc_rollup_node "global/block/head/inbox" in
  match inbox with
  | None -> failwith "Unable to retrieve inbox from sc rollup node"
  | Some inbox -> parse_inbox inbox

let test_rollup_inbox_size ~kind =
  regression_test
    ~__FILE__
    ~tags:["sc_rollup"; "inbox"; kind]
    (Format.asprintf
       "%s - pushing messages in the inbox - check inbox size"
       kind)
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      ( with_fresh_rollup ~kind @@ fun _sc_rollup _sc_rollup_node _filename ->
        let n = 10 in
        let* () = send_messages n client in
        let* _, inbox_msg_during_commitment_period =
          get_inbox_from_tezos_node client
        in
        (* Expect [n] messages per level + SOL/EOL for each level including
           at inbox's creation. *)
        return
        @@ Check.(
             (inbox_msg_during_commitment_period
             = (n * (n + 1) / 2) + (((n + 1) * 2) + 2))
               int
               ~error_msg:"expected value %R, got %L") )
        node
        client)

let fetch_messages_from_block client =
  let* ops = RPC.Client.call client @@ RPC.get_chain_block_operations () in
  let messages =
    ops |> JSON.as_list
    |> List.concat_map JSON.as_list
    |> List.concat_map (fun op -> JSON.(op |-> "contents" |> as_list))
    |> List.filter_map (fun op ->
           if JSON.(op |-> "kind" |> as_string) = "sc_rollup_add_messages" then
             Some JSON.(op |-> "message" |> as_list)
           else None)
    |> List.hd
    |> List.map (fun message -> JSON.(message |> as_string))
  in
  return messages

(* Synchronizing the inbox in the rollup node
   ------------------------------------------

   For each new head set by the Tezos node, the rollup node retrieves
   the messages of its rollup and maintains its internal inbox in a
   persistent state stored in its data directory. This process can
   handle Tezos chain reorganization and can also catch up to ensure a
   tight synchronization between the rollup and the layer 1 chain.

   In addition, this maintenance includes the computation of a Merkle
   tree which must have the same root hash as the one stored by the
   protocol in the context.
*)
let test_rollup_inbox_of_rollup_node variant scenario ~kind =
  regression_test
    ~__FILE__
    ~tags:["sc_rollup"; "inbox"; "node"; variant; kind]
    (Printf.sprintf
       "%s - maintenance of inbox in the rollup node (%s)"
       kind
       variant)
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      ( with_fresh_rollup ~kind @@ fun sc_rollup sc_rollup_node _filename ->
        let* () = scenario protocol sc_rollup_node sc_rollup node client in
        let* inbox_from_sc_rollup_node =
          get_inbox_from_sc_rollup_node sc_rollup_node
        in
        let* inbox_from_tezos_node = get_inbox_from_tezos_node client in
        return
        @@ Check.(
             (inbox_from_sc_rollup_node = inbox_from_tezos_node)
               (tuple2 string int)
               ~error_msg:"expected value %R, got %L") )
        node
        client)

let basic_scenario _protocol sc_rollup_node _sc_rollup _node client =
  let num_messages = 2 in
  let expected_level =
    (* We start at level 2 and each message also bakes a block. With 2 messages being sent, we
       must end up at level 4. *)
    4
  in
  let* () = Sc_rollup_node.run sc_rollup_node in
  Log.info "before sending messages\n" ;
  let* () = send_messages num_messages client in
  let* level = Client.level client in
  Log.info "level: %d\n" level ;
  let* _ =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node expected_level
  in
  return ()

(* Reactivate when the following TODO is fixed:

   FIXME: https://gitlab.com/tezos/tezos/-/issues/3205

   The rollup node should be able to restart properly after an
   abnormal interruption at every point of its process.  Currently,
   the state is not persistent enough and the processing is not
   idempotent enough to achieve that property. *)
let _sc_rollup_node_stops_scenario _protocol sc_rollup_node _node client =
  let num_messages = 2 in
  let expected_level =
    (* We start at level 2 and each message also bakes a block. With 2 messages being sent twice, we
       must end up at level 6. *)
    6
  in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* () = send_messages num_messages client in
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () = send_messages num_messages client in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* _ =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node expected_level
  in
  return ()

let sc_rollup_node_disconnects_scenario _protocol sc_rollup_node _sc_rollup node
    client =
  let num_messages = 2 in
  let level = Node.get_level node in
  Log.info "we are at level %d" level ;
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* () = send_messages num_messages client in
  let* level =
    Sc_rollup_node.wait_for_level sc_rollup_node (level + num_messages)
  in
  Log.info "Terminating Tezos node" ;
  let* () = Node.terminate node in
  Log.info "Waiting before restarting Tezos node" ;
  let* () = Lwt_unix.sleep 3. in
  Log.info "Restarting Tezos node" ;
  let* () = Node.run node Node.[Connections 0; Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node in
  let* () = send_messages num_messages client in
  let* _ =
    Sc_rollup_node.wait_for_level sc_rollup_node (level + num_messages)
  in
  return ()

let sc_rollup_node_handles_chain_reorg protocol sc_rollup_node _sc_rollup node
    client =
  let num_messages = 1 in

  setup ~protocol @@ fun node' client' _ ->
  let* () = Client.Admin.trust_address client ~peer:node'
  and* () = Client.Admin.trust_address client' ~peer:node in
  let* () = Client.Admin.connect_address client ~peer:node' in

  let* () = Sc_rollup_node.run sc_rollup_node in
  let* () = send_messages num_messages client in
  (* Since we start at level 2, sending 1 message (which also bakes a block) must cause the nodes to
     observe level 3. *)
  let* _ = Node.wait_for_level node 3 in
  let* _ = Node.wait_for_level node' 3 in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node 3 in
  Log.info "Nodes are synchronized." ;

  let divergence () =
    let* identity' = Node.wait_for_identity node' in
    let* () = Client.Admin.kick_peer client ~peer:identity' in
    let* () = send_messages num_messages client in
    (* +1 block for [node] *)
    let* _ = Node.wait_for_level node 4 in

    let* () = send_messages num_messages client' in
    let* () = send_messages num_messages client' in
    (* +2 blocks for [node'] *)
    let* _ = Node.wait_for_level node' 5 in
    Log.info "Nodes are following distinct branches." ;
    return ()
  in

  let trigger_reorg () =
    let* () = Client.Admin.connect_address client ~peer:node' in
    let* _ = Node.wait_for_level node 5 in
    Log.info "Nodes are synchronized again." ;
    return ()
  in

  let* () = divergence () in
  let* () = trigger_reorg () in
  (* After bringing [node'] back, our SCORU node should see that there is a more attractive head at
     level 5. *)
  let* _ = Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node 5 in
  return ()

(* One can retrieve the list of originated SCORUs.
   -----------------------------------------------
*)
let with_fresh_rollups ~kind n f node client operator =
  let rec go n addrs k =
    if n < 1 then k addrs
    else
      with_fresh_rollup
        ~kind
        (fun addr _ _ -> go (n - 1) (String_set.add addr addrs) k)
        node
        client
        operator
  in
  go n String_set.empty f

let test_rollup_list ~kind =
  let open Lwt.Syntax in
  let go node client bootstrap1 =
    let* rollups =
      RPC.Client.call client @@ RPC.get_chain_block_context_sc_rollups ()
    in
    let rollups = JSON.as_list rollups in
    let () =
      match rollups with
      | _ :: _ ->
          failwith "Expected initial list of originated SCORUs to be empty"
      | [] -> ()
    in

    with_fresh_rollups
      ~kind
      10
      (fun scoru_addresses ->
        let* () = Client.bake_for_and_wait client in
        let+ rollups =
          RPC.Client.call client @@ RPC.get_chain_block_context_sc_rollups ()
        in
        let rollups =
          JSON.as_list rollups |> List.map JSON.as_string |> String_set.of_list
        in
        Check.(
          (rollups = scoru_addresses)
            (comparable_module (module String_set))
            ~error_msg:"%L %R"))
      node
      client
      bootstrap1
  in

  regression_test
    ~__FILE__
    ~tags:["sc_rollup"; "list"]
    "list originated rollups"
    (fun protocol -> setup ~protocol go)

(* Make sure the rollup node boots into the initial state.
   -------------------------------------------------------

   When a rollup node starts, we want to make sure that in the absence of
   messages it will boot into the initial state.
*)
let test_rollup_node_boots_into_initial_state ~kind =
  let go client sc_rollup sc_rollup_node =
    let* genesis_info =
      RPC.Client.call ~hooks client
      @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in

    let* () = Sc_rollup_node.run sc_rollup_node in
    let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in

    let* level =
      Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node init_level
    in
    Check.(level = init_level)
      Check.int
      ~error_msg:"Current level has moved past origination level (%L = %R)" ;

    let* ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
    Check.(ticks = 0)
      Check.int
      ~error_msg:"Unexpected initial tick count (%L = %R)" ;

    let* status = Sc_rollup_client.status ~hooks sc_rollup_client in
    let expected_status =
      match kind with
      | "arith" -> "Halted"
      | "wasm_2_0_0" -> "Waiting for input message"
      | _ -> raise (Invalid_argument kind)
    in
    Check.(status = expected_status)
      Check.string
      ~error_msg:"Unexpected PVM status (%L = %R)" ;

    Lwt.return_unit
  in

  regression_test
    ~__FILE__
    ~tags:["sc_rollup"; "run"; "node"; kind]
    (Format.asprintf "%s - node boots into the initial state" kind)
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      with_fresh_rollup
        ~kind
        (fun sc_rollup sc_rollup_node _filename ->
          go client sc_rollup sc_rollup_node)
        node
        client)

let test_rollup_node_advances_pvm_state protocols ~test_name ~boot_sector
    ~internal ~kind =
  let go ~internal client sc_rollup sc_rollup_node =
    let* genesis_info =
      RPC.Client.call ~hooks client
      @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in

    let* () = Sc_rollup_node.run sc_rollup_node in
    let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in

    let* level =
      Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node init_level
    in
    Check.(level = init_level)
      Check.int
      ~error_msg:"Current level has moved past origination level (%L = %R)" ;
    let* level, forwarder =
      if not internal then return (level, None)
      else
        (* Originate forwarder contract to send internal messages to rollup *)
        let* contract_id =
          Client.originate_contract
            ~alias:"rollup_deposit"
            ~amount:Tez.zero
            ~src:Constant.bootstrap1.alias
            ~prg:"file:./tezt/tests/contracts/proto_alpha/sc_rollup_forward.tz"
            ~init:"Unit"
            ~burn_cap:Tez.(of_int 1)
            client
        in
        let* () = Client.bake_for_and_wait client in
        Log.info
          "The forwarder %s contract was successfully originated"
          contract_id ;
        return (level + 1, Some contract_id)
    in
    (* Called with monotonically increasing [i] *)
    let test_message i =
      let* prev_state_hash =
        Sc_rollup_client.state_hash ~hooks sc_rollup_client
      in
      let* prev_ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
      let message = sf "%d %d + value" i ((i + 2) * 2) in
      let* () =
        match forwarder with
        | None ->
            (* External message *)
            send_message client (sf "[%S]" message)
        | Some forwarder ->
            (* Internal message through forwarder *)
            let* () =
              Client.transfer
                client
                ~amount:Tez.zero
                ~giver:Constant.bootstrap1.alias
                ~receiver:forwarder
                ~arg:(sf "Pair %S %S" sc_rollup message)
            in
            Client.bake_for_and_wait client
      in
      let* _ =
        Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node (level + i)
      in

      (* specific per kind PVM checks *)
      let* () =
        match kind with
        | "arith" ->
            let* encoded_value =
              Sc_rollup_client.state_value
                ~hooks
                sc_rollup_client
                ~key:"vars/value"
            in
            let value =
              match Data_encoding.(Binary.of_bytes int31) @@ encoded_value with
              | Error error ->
                  failwith
                    (Format.asprintf
                       "The arithmetic PVM has an unexpected state: %a"
                       Data_encoding.Binary.pp_read_error
                       error)
              | Ok x -> x
            in
            Check.(
              (value = i + ((i + 2) * 2))
                int
                ~error_msg:"Invalid value in rollup state (%L <> %R)") ;
            return ()
        | "wasm_2_0_0" ->
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/3729

                Add an appropriate check for various test kernels

                computation.wasm               - Gets into eval state
                no_parse_random.wasm           - Stuck state due to parse error
                no_parse_bad_fingerprint.wasm  - Stuck state due to parse error
            *)
            return ()
        | _otherwise -> raise (Invalid_argument kind)
      in

      let* state_hash = Sc_rollup_client.state_hash ~hooks sc_rollup_client in
      Check.(state_hash <> prev_state_hash)
        Check.string
        ~error_msg:"State hash has not changed (%L <> %R)" ;

      let* ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
      Check.(ticks >= prev_ticks)
        Check.int
        ~error_msg:"Tick counter did not advance (%L >= %R)" ;

      Lwt.return_unit
    in
    let* () = Lwt_list.iter_s test_message (range 1 10) in

    Lwt.return_unit
  in

  if not internal then
    regression_test
      ~__FILE__
      ~tags:["sc_rollup"; "run"; "node"; kind]
      test_name
      (fun protocol ->
        setup ~protocol @@ fun node client ->
        with_fresh_rollup
          ~kind
          ?boot_sector
          (fun sc_rollup_address sc_rollup_node _filename ->
            go ~internal:false client sc_rollup_address sc_rollup_node)
          node
          client)
      protocols
  else
    regression_test
      ~__FILE__
      ~tags:["sc_rollup"; "run"; "node"; "internal"; kind]
      test_name
      (fun protocol ->
        setup ~protocol @@ fun node client ->
        with_fresh_rollup
          ~kind
          ?boot_sector
          (fun sc_rollup_address sc_rollup_node _filename ->
            go ~internal:true client sc_rollup_address sc_rollup_node)
          node
          client)
      protocols

let test_rollup_node_run_with_kernel protocols ~kind ~kernel_name ~internal =
  test_rollup_node_advances_pvm_state
    protocols
    ~test_name:(Format.asprintf "%s - runs with kernel - %s" kind kernel_name)
    ~boot_sector:(Some (read_kernel kernel_name))
    ~internal
    ~kind

(* Ensure the PVM is transitioning upon incoming messages.
      -------------------------------------------------------

      When the rollup node receives messages, we like to see evidence that the PVM
      has advanced.

      Specifically [test_rollup_node_advances_pvm_state ?boot_sector protocols ~kind]

      * Originates a SCORU of [kind]
      * Originates a L1 contract to send internal messages from
      * Sends internal or external messages to the rollup

   After each a PVM kind-specific test is run, asserting the validity of the new state.
*)
let test_rollup_node_advances_pvm_state protocols ~kind ~boot_sector ~internal =
  test_rollup_node_advances_pvm_state
    protocols
    ~test_name:
      (Format.asprintf
         "%s - node advances PVM state with %smessages"
         kind
         (if internal then "internal " else ""))
    ~boot_sector
    ~internal
    ~kind

(* Ensure that commitments are stored and published properly.
   ----------------------------------------------------------

   Every 20 level, a commitment is computed and stored by the
   rollup node. The rollup node will also publish previously
   computed commitments on the layer1, in a first in first out
   fashion. To ensure that commitments are robust to chain
   reorganisations, only finalized block are processed when
   trying to publish a commitment.
*)

let bake_levels ?hook n client =
  fold n () @@ fun i () ->
  let* () = match hook with None -> return () | Some hook -> hook i in
  Client.bake_for_and_wait client

let eq_commitment_typ =
  Check.equalable
    (fun ppf (c : Sc_rollup_client.commitment) ->
      Format.fprintf
        ppf
        "@[<hov 2>{ predecessor: %s,@,\
         state: %s,@,\
         inbox level: %d,@,\
         ticks: %d }@]"
        c.predecessor
        c.compressed_state
        c.inbox_level
        c.number_of_ticks)
    ( = )

let check_commitment_eq (commitment, name) (expected_commitment, exp_name) =
  Check.((commitment = expected_commitment) (option eq_commitment_typ))
    ~error_msg:
      (sf
         "Commitment %s differs from the one %s.\n%s: %%L\n%s: %%R"
         name
         exp_name
         (String.capitalize_ascii name)
         (String.capitalize_ascii exp_name))

let tezos_client_get_commitment client sc_rollup commitment_hash =
  let* output =
    Client.rpc
      Client.GET
      [
        "chains";
        "main";
        "blocks";
        "head";
        "context";
        "sc_rollup";
        sc_rollup;
        "commitment";
        commitment_hash;
      ]
      client
  in
  Lwt.return @@ Sc_rollup_client.commitment_from_json output

let check_published_commitment_in_l1 ?(allow_non_published = false)
    ?(force_new_level = true) sc_rollup client published_commitment =
  let* () =
    if force_new_level then
      (* Triggers injection into the L1 context *)
      bake_levels 1 client
    else Lwt.return_unit
  in
  let* commitment_in_l1 =
    match published_commitment with
    | None ->
        if not allow_non_published then
          Test.fail "No commitment has been published" ;
        Lwt.return_none
    | Some (hash, _commitment, _level) ->
        tezos_client_get_commitment client sc_rollup hash
  in
  let published_commitment =
    Option.map (fun (_, c, _) -> c) published_commitment
  in
  check_commitment_eq
    (commitment_in_l1, "in L1")
    (published_commitment, "published") ;
  Lwt.return_unit

let test_commitment_scenario ?commitment_period ?challenge_window
    ?(extra_tags = []) variant =
  test_scenario
    ?commitment_period
    ?challenge_window
    {
      tags = ["commitment"; "node"] @ extra_tags;
      variant;
      description = "rollup node - correct handling of commitments";
    }

let commitment_stored _protocol sc_rollup_node sc_rollup _node client =
  (* The rollup is originated at level `init_level`, and it requires
     `sc_rollup_commitment_period_in_blocks` levels to store a commitment.
     There is also a delay of `block_finality_time` before storing a
     commitment, to avoid including wrong commitments due to chain
     reorganisations. Therefore the commitment will be stored and published
     when the [Commitment] module processes the block at level
     `init_level + sc_rollup_commitment_period_in_blocks +
     levels_to_finalise`.
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let store_commitment_level =
    init_level + levels_to_commitment + block_finality_time
  in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node init_level
  in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () =
    (* at init_level + i we publish i messages, therefore at level
       init_level + i a total of 1+..+i = (i*(i+1))/2 messages will have been
       sent.
    *)
    send_messages levels_to_commitment client
  in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (init_level + levels_to_commitment)
  in
  (* Bake [block_finality_time] additional levels to ensure that block number
     [init_level + sc_rollup_commitment_period_in_blocks] is
     processed by the rollup node as finalized. *)
  let* () = bake_levels block_finality_time client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      store_commitment_level
  in
  let* stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let stored_inbox_level = Option.map inbox_level stored_commitment in
  Check.(stored_inbox_level = Some (levels_to_commitment + init_level))
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  (* Bake one level for commitment to be included *)
  let* () = Client.bake_for_and_wait client in
  let* published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  check_commitment_eq
    (Option.map (fun (_, c, _) -> c) stored_commitment, "stored")
    (Option.map (fun (_, c, _) -> c) published_commitment, "published") ;
  check_published_commitment_in_l1 sc_rollup client published_commitment

let mode_publish mode publishes protocol sc_rollup_node sc_rollup node client =
  setup ~protocol @@ fun other_node other_client _ ->
  let* () = Client.Admin.trust_address client ~peer:other_node
  and* () = Client.Admin.trust_address other_client ~peer:node in
  let* () = Client.Admin.connect_address client ~peer:other_node in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let level = Node.get_level node in
  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let* () = send_messages levels_to_commitment client in
  let* level =
    Sc_rollup_node.wait_for_level sc_rollup_node (level + levels_to_commitment)
  in
  Log.info "Starting other rollup node." ;
  let purposes = ["publish"; "cement"; "add_messages"] in
  let operators =
    List.mapi
      (fun i purpose ->
        (purpose, Constant.[|bootstrap3; bootstrap5; bootstrap4|].(i).alias))
      purposes
  in
  let sc_rollup_other_node =
    (* Other rollup node *)
    Sc_rollup_node.create
      mode
      other_node
      other_client
      ~operators
      ~default_operator:Constant.bootstrap3.alias
  in
  let sc_rollup_other_client = Sc_rollup_client.create sc_rollup_other_node in
  let* _configuration_filename =
    Sc_rollup_node.config_init sc_rollup_other_node sc_rollup
  in
  let* () = Sc_rollup_node.run sc_rollup_other_node in
  let* _level = Sc_rollup_node.wait_for_level sc_rollup_other_node level in
  Log.info "Other rollup node synchronized." ;
  let* () = send_messages levels_to_commitment client in
  let* level =
    Sc_rollup_node.wait_for_level sc_rollup_node (level + levels_to_commitment)
  in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node level
  and* _ = Sc_rollup_node.wait_for_level sc_rollup_other_node level in
  Log.info "Both rollup nodes have reached level %d." level ;
  let* state_hash = Sc_rollup_client.state_hash sc_rollup_client
  and* state_hash_other = Sc_rollup_client.state_hash sc_rollup_other_client in
  Check.((state_hash = state_hash_other) string)
    ~error_msg:
      "State hash of other rollup node is %R but the first rollup node has %L" ;
  let* published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  let* other_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_other_client
  in
  if published_commitment = None then
    Test.fail "Operator has not published a commitment but should have." ;
  if other_published_commitment = None = publishes then
    Test.fail
      "Other has%s published a commitment but should%s."
      (if publishes then " not" else "")
      (if publishes then " have" else " never do so") ;
  unit

let commitment_not_stored_if_non_final _protocol sc_rollup_node sc_rollup _node
    client =
  (* The rollup is originated at level `init_level`, and it requires
     `sc_rollup_commitment_period_in_blocks` levels to store a commitment.
     There is also a delay of `block_finality_time` before storing a
     commitment, to avoid including wrong commitments due to chain
     reorganisations. Therefore the commitment will be stored and published
     when the [Commitment] module processes the block at level
     `init_level + sc_rollup_commitment_period_in_blocks +
     levels_to_finalise`. At the level before, the commitment will not be
     neither stored nor published.
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let levels_to_finalize = block_finality_time - 1 in
  let store_commitment_level = init_level + levels_to_commitment in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node init_level
  in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () = send_messages levels_to_commitment client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      store_commitment_level
  in
  let* () = bake_levels levels_to_finalize client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (store_commitment_level + levels_to_finalize)
  in
  let* commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let stored_inbox_level = Option.map inbox_level commitment in
  Check.(stored_inbox_level = None)
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  let* commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  let published_inbox_level = Option.map inbox_level commitment in
  Check.(published_inbox_level = None)
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been published at a level different than expected (%L = \
       %R)" ;
  Lwt.return_unit

let commitments_messages_reset _protocol sc_rollup_node sc_rollup _node client =
  (* For `sc_rollup_commitment_period_in_blocks` levels after the sc rollup
     origination, i messages are sent to the rollup, for a total of
     `sc_rollup_commitment_period_in_blocks *
     (sc_rollup_commitment_period_in_blocks + 1)/2` messages. These will be
     the number of messages in the first commitment published by the rollup
     node. Then, for other `sc_rollup_commitment_period_in_blocks` levels,
     no messages are sent to the sc-rollup address. The second commitment
     published by the sc-rollup node will contain 0 messages. Finally,
     `block_finality_time` empty levels are baked which ensures that two
     commitments are stored and published by the rollup node.
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node init_level
  in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () =
    (* At init_level + i we publish i messages, therefore at level
       init_level + 20 a total of 1+..+20 = (20*21)/2 = 210 messages
       will have been sent.
    *)
    send_messages levels_to_commitment client
  in
  (* Bake other `sc_rollup_commitment_period_in_blocks +
     block_finality_time` levels with no messages. The first
     `sc_rollup_commitment_period_in_blocks` levels contribute to the second
     commitment stored by the rollup node. The last `block_finality_time`
     levels ensure that the second commitment is stored and published by the
     rollup node.
  *)
  let* () = bake_levels (levels_to_commitment + block_finality_time) client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (init_level + (2 * levels_to_commitment) + block_finality_time)
  in
  let* stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let stored_inbox_level = Option.map inbox_level stored_commitment in
  Check.(stored_inbox_level = Some (init_level + (2 * levels_to_commitment)))
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  (let stored_number_of_ticks = Option.map number_of_ticks stored_commitment in
   Check.(stored_number_of_ticks = Some (2 * levels_to_commitment))
     (Check.option Check.int)
     ~error_msg:
       "Number of messages processed by commitment is different from the \
        number of messages expected (%L = %R)") ;
  let* published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  check_commitment_eq
    (Option.map (fun (_, c, _) -> c) stored_commitment, "stored")
    (Option.map (fun (_, c, _) -> c) published_commitment, "published") ;
  check_published_commitment_in_l1 sc_rollup client published_commitment

let commitment_stored_robust_to_failures _protocol sc_rollup_node sc_rollup node
    client =
  (* This test uses two rollup nodes for the same rollup, tracking the same L1 node.
     Both nodes process heads from the L1. However, the second node is stopped
     one level before publishing a commitment, and then is restarted.
     We should not observe any difference in the commitments stored by the
     two rollup nodes.
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let bootstrap2_key = Constant.bootstrap2.public_key_hash in
  let* client' = Client.init ?endpoint:(Some (Node node)) () in
  let sc_rollup_node' =
    Sc_rollup_node.create Operator node client' ~default_operator:bootstrap2_key
  in
  let* _configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node' sc_rollup
  in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let sc_rollup_client' = Sc_rollup_client.create sc_rollup_node' in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* () = Sc_rollup_node.run sc_rollup_node' in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node init_level
  in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () =
    (* at init_level + i we publish i messages, therefore at level
       init_level + i a total of 1+..+i = (i*(i+1))/2 messages will have been
       sent.
    *)
    send_messages levels_to_commitment client
  in
  (* The line below works as long as we have a block finality time which is strictly positive,
     which is a safe assumption. *)
  let* () = bake_levels (block_finality_time - 1) client in
  let* level_before_storing_commitment =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (init_level + levels_to_commitment + block_finality_time - 1)
  in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node'
      level_before_storing_commitment
  in
  let* () = Sc_rollup_node.terminate sc_rollup_node' in
  let* () = Sc_rollup_node.run sc_rollup_node' in
  let* () = Client.bake_for_and_wait client in
  let* () = Sc_rollup_node.terminate sc_rollup_node' in
  let* () = Client.bake_for_and_wait client in
  let* () = Sc_rollup_node.run sc_rollup_node' in
  let* level_commitment_is_stored =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (level_before_storing_commitment + 1)
  in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node'
      level_commitment_is_stored
  in
  let* stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let* stored_commitment' =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client'
  in
  check_commitment_eq
    (Option.map (fun (_, c, _) -> c) stored_commitment, "stored in first node")
    (Option.map (fun (_, c, _) -> c) stored_commitment', "stored in second node") ;
  return ()

let commitments_reorgs ~kind protocol sc_rollup_node sc_rollup node client =
  (* No messages are published after origination, for
     `sc_rollup_commitment_period_in_blocks - 1` levels. Then a divergence
     occurs:  in the first branch one message is published for
     `block_finality_time - 1` blocks. In the second branch no messages are
     published for `block_finality_time` blocks. The second branch is
     the more attractive one, and will be chosen when a reorganisation occurs.
     One more level is baked to ensure that the rollup node stores and
     publishes the commitment. The final commitment should have
     no messages and no ticks.
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let num_empty_blocks = block_finality_time in
  let num_messages = 1 in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in

  setup ~protocol @@ fun node' client' _ ->
  let* () = Client.Admin.trust_address client ~peer:node'
  and* () = Client.Admin.trust_address client' ~peer:node in
  let* () = Client.Admin.connect_address client ~peer:node' in

  let* () = Sc_rollup_node.run sc_rollup_node in
  (* We bake `sc_rollup_commitment_period_in_blocks - 1` levels, which
     should cause both nodes to observe level
     `sc_rollup_commitment_period_in_blocks + init_level - 1 . *)
  let* () = bake_levels (levels_to_commitment - 1) client in
  let* _ = Node.wait_for_level node (init_level + levels_to_commitment - 1) in
  let* _ = Node.wait_for_level node' (init_level + levels_to_commitment - 1) in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (init_level + levels_to_commitment - 1)
  in
  Log.info "Nodes are synchronized." ;

  let divergence () =
    let* identity' = Node.wait_for_identity node' in
    let* () = Client.Admin.kick_peer client ~peer:identity' in
    let* () = send_messages num_messages client in
    (* `block_finality_time - 1` blocks with message for [node] *)
    let* _ =
      Node.wait_for_level
        node
        (init_level + levels_to_commitment - 1 + num_messages)
    in

    let* () = bake_levels num_empty_blocks client' in
    (* `block_finality_time` blocks with no messages for [node'] *)
    let* _ =
      Node.wait_for_level
        node'
        (init_level + levels_to_commitment - 1 + num_empty_blocks)
    in
    Log.info "Nodes are following distinct branches." ;
    return ()
  in

  let trigger_reorg () =
    let* () = Client.Admin.connect_address client ~peer:node' in
    let* _ =
      Node.wait_for_level
        node
        (init_level + levels_to_commitment - 1 + num_empty_blocks)
    in
    Log.info "Nodes are synchronized again." ;
    return ()
  in

  let* () = divergence () in
  let* () = trigger_reorg () in
  (* After triggering a reorganisation the node should see that there is a more
     attractive head at level `init_level +
     sc_rollup_commitment_period_in_blocks + block_finality_time - 1`.
  *)
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (init_level + levels_to_commitment - 1 + num_empty_blocks)
  in
  (* exactly one level left to finalize the commitment in the node. *)
  let* () = bake_levels (block_finality_time - num_empty_blocks + 1) client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (init_level + levels_to_commitment + block_finality_time)
  in
  let* stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let stored_inbox_level = Option.map inbox_level stored_commitment in
  Check.(stored_inbox_level = Some (init_level + levels_to_commitment))
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  let () = Log.info "init_level: %d" init_level in
  (let stored_number_of_ticks = Option.map number_of_ticks stored_commitment in
   let additional_ticks =
     match kind with
     | "arith" -> 1 (* boot sector *) + 1 (* metadata *)
     | "wasm_2_0_0" -> 1 (* boot_sector *)
     | _ -> assert false
   in
   Check.(
     stored_number_of_ticks
     = Some ((2 * levels_to_commitment) + additional_ticks))
     (Check.option Check.int)
     ~error_msg:
       "Number of ticks processed by commitment is different from the number \
        of ticks expected (%L = %R)") ;
  let* published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  check_commitment_eq
    (Option.map (fun (_, c, _) -> c) stored_commitment, "stored")
    (Option.map (fun (_, c, _) -> c) published_commitment, "published") ;
  check_published_commitment_in_l1 sc_rollup client published_commitment

type balances = {liquid : int; frozen : int}

let contract_balances ~pkh client =
  let* liquid =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_contract_balance ~id:pkh ()
  in
  let* frozen =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_contract_frozen_bonds ~id:pkh ()
  in
  return {liquid = Tez.to_mutez liquid; frozen = Tez.to_mutez frozen}

(** This helper allow to attempt recovering bond for SCORU rollup operator.
    if [expect_failure] is set to some string then, we expect the command to fail
    with an error that contains that string. *)
let attempt_withdraw_stake =
  let check_eq_int a b =
    Check.((a = b) int ~error_msg:"expected value %L, got %R")
  in
  fun ?expect_failure ~sc_rollup client ->
    (* placehoders *)
    (* TODO/Fixme:
        - Shoud provide the rollup operator key (bootstrap1_key) as an
          argument to scenarios.
    *)
    let bootstrap1_key = Constant.bootstrap1.public_key_hash in
    let* constants =
      RPC.Client.call ~hooks client @@ RPC.get_chain_block_context_constants ()
    in
    let recover_bond_unfreeze =
      JSON.(constants |-> "sc_rollup_stake_amount" |> as_int)
    in
    let recover_bond_fee = 1_000_000 in
    let inject_op () =
      Client.Sc_rollup.submit_recover_bond
        ~hooks
        ~rollup:sc_rollup
        ~src:bootstrap1_key
        ~fee:(Tez.of_mutez_int recover_bond_fee)
        client
    in
    match expect_failure with
    | None ->
        let*! () = inject_op () in
        let* old_bal = contract_balances ~pkh:bootstrap1_key client in
        let* () = Client.bake_for_and_wait ~keys:["bootstrap2"] client in
        let* new_bal = contract_balances ~pkh:bootstrap1_key client in
        let expected_liq_new_bal =
          old_bal.liquid - recover_bond_fee + recover_bond_unfreeze
        in
        check_eq_int new_bal.liquid expected_liq_new_bal ;
        check_eq_int new_bal.frozen (old_bal.frozen - recover_bond_unfreeze) ;
        unit
    | Some failure_string ->
        let*? p = inject_op () in
        Process.check_error ~msg:(rex failure_string) p

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2942
   Do not pass an explicit value for `?commitment_period until
   https://gitlab.com/tezos/tezos/-/merge_requests/5212 has been merged. *)
(* Test that nodes do not publish commitments before the last cemented commitment. *)
let commitment_before_lcc_not_published _protocol sc_rollup_node sc_rollup node
    client =
  let* constants = get_sc_rollup_constants client in
  let commitment_period = constants.commitment_period_in_blocks in
  let challenge_window = constants.challenge_window_in_blocks in
  (* Rollup node 1 processes messages, produces and publishes two commitments. *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node init_level
  in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () = bake_levels commitment_period client in
  let* commitment_inbox_level =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (init_level + commitment_period)
  in
  (* Bake `block_finality_time` additional level to ensure that block number
     `init_level + sc_rollup_commitment_period_in_blocks` is processed by
     the rollup node as finalized. *)
  let* () = bake_levels block_finality_time client in
  let* commitment_finalized_level =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (commitment_inbox_level + block_finality_time)
  in
  let* rollup_node1_stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let* rollup_node1_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  let () =
    Check.(
      Option.map inbox_level rollup_node1_published_commitment
      = Some commitment_inbox_level)
      (Check.option Check.int)
      ~error_msg:
        "Commitment has been published at a level different than expected (%L \
         = %R)"
  in
  (* Cement commitment manually: the commitment can be cemented after
     `challenge_window_levels` have passed since the commitment was published
     (that is at level `commitment_finalized_level`). Note that at this point
     we are already at level `commitment_finalized_level`, hence cementation of
     the commitment can happen. *)
  let levels_to_cementation = challenge_window + 1 in
  let cemented_commitment_hash =
    Option.map hash rollup_node1_published_commitment
    |> Option.value
         ~default:"scc12XhSULdV8bAav21e99VYLTpqAjTd7NU8Mn4zFdKPSA8auMbggG"
  in
  let* () = bake_levels levels_to_cementation client in
  let* cemented_commitment_level =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (commitment_finalized_level + levels_to_cementation)
  in

  (* Withdraw stake before cementing should fail *)
  let* () =
    attempt_withdraw_stake
      ~sc_rollup
      client
      ~expect_failure:
        "Attempted to withdraw while not staked on the last cemented \
         commitment."
  in

  let* () =
    cement_commitment client ~sc_rollup ~hash:cemented_commitment_hash
  in
  let* level_after_cementation =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (cemented_commitment_level + 1)
  in

  (* Withdraw stake after cementing should succeed *)
  let* () = attempt_withdraw_stake ~sc_rollup client in

  let* () = Sc_rollup_node.terminate sc_rollup_node in
  (* Rollup node 2 starts and processes enough levels to publish a commitment.*)
  let bootstrap2_key = Constant.bootstrap2.public_key_hash in
  let* client' = Client.init ?endpoint:(Some (Node node)) () in
  let sc_rollup_node' =
    Sc_rollup_node.create Operator node client' ~default_operator:bootstrap2_key
  in
  let sc_rollup_client' = Sc_rollup_client.create sc_rollup_node' in
  let* _configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node' sc_rollup
  in
  let* () = Sc_rollup_node.run sc_rollup_node' in

  let* rollup_node2_catchup_level =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node'
      level_after_cementation
  in
  Check.(rollup_node2_catchup_level = level_after_cementation)
    Check.int
    ~error_msg:"Current level has moved past cementation inbox level (%L = %R)" ;
  (* Check that no commitment was published. *)
  let* rollup_node2_last_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client'
  in
  let rollup_node2_last_published_commitment_inbox_level =
    Option.map inbox_level rollup_node2_last_published_commitment
  in
  let () =
    Check.(rollup_node2_last_published_commitment_inbox_level = None)
      (Check.option Check.int)
      ~error_msg:
        "Commitment has been published at a level different than expected (%L \
         = %R)"
  in
  (* Check that the commitment stored by the second rollup node
     is the same commmitment stored by the first rollup node. *)
  let* rollup_node2_stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client'
  in
  let () =
    Check.(
      Option.map hash rollup_node1_stored_commitment
      = Option.map hash rollup_node2_stored_commitment)
      (Check.option Check.string)
      ~error_msg:
        "Commitment stored by first and second rollup nodes differ (%L = %R)"
  in

  (* Bake other commitment_period levels and check that rollup_node2 is
     able to publish a commitment. *)
  let* () = bake_levels commitment_period client' in
  let commitment_inbox_level = commitment_inbox_level + commitment_period in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node'
      (level_after_cementation + commitment_period)
  in
  let* rollup_node2_last_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client'
  in
  let rollup_node2_last_published_commitment_inbox_level =
    Option.map inbox_level rollup_node2_last_published_commitment
  in
  let () =
    Check.(
      rollup_node2_last_published_commitment_inbox_level
      = Some commitment_inbox_level)
      (Check.option Check.int)
      ~error_msg:
        "Commitment has been published at a level different than expected (%L \
         = %R)"
  in
  let () =
    Check.(
      Option.map predecessor rollup_node2_last_published_commitment
      = Some cemented_commitment_hash)
      (Check.option Check.string)
      ~error_msg:
        "Predecessor fo commitment published by rollup_node2 should be the \
         cemented commitment (%L = %R)"
  in
  return ()

(* Test that the level when a commitment was first published is fetched correctly
   by rollup nodes. *)
let first_published_level_is_global _protocol sc_rollup_node sc_rollup node
    client =
  (* Rollup node 1 processes messages, produces and publishes two commitments. *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* commitment_period = get_sc_rollup_commitment_period_in_blocks client in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node init_level
  in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () = bake_levels commitment_period client in
  let* commitment_inbox_level =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (init_level + commitment_period)
  in
  (* Bake `block_finality_time` additional level to ensure that block number
     `init_level + sc_rollup_commitment_period_in_blocks` is processed by
     the rollup node as finalized. *)
  let* () = bake_levels block_finality_time client in
  let* commitment_finalized_level =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (commitment_inbox_level + block_finality_time)
  in
  let* rollup_node1_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  Check.(
    Option.map inbox_level rollup_node1_published_commitment
    = Some commitment_inbox_level)
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been published at a level different than expected (%L = \
       %R)" ;
  (* Bake an additional block for the commitment to be included. *)
  let* () = Client.bake_for_and_wait client in
  let* commitment_publish_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (commitment_finalized_level + 1)
  in
  let* rollup_node1_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  Check.(
    Option.bind rollup_node1_published_commitment first_published_at_level
    = Some commitment_publish_level)
    (Check.option Check.int)
    ~error_msg:
      "Level at which commitment has first been published (%L) is wrong. \
       Expected %R." ;
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  (* Rollup node 2 starts and processes enough levels to publish a commitment.*)
  let bootstrap2_key = Constant.bootstrap2.public_key_hash in
  let* client' = Client.init ?endpoint:(Some (Node node)) () in
  let sc_rollup_node' =
    Sc_rollup_node.create Operator node client' ~default_operator:bootstrap2_key
  in
  let sc_rollup_client' = Sc_rollup_client.create sc_rollup_node' in
  let* _configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node' sc_rollup
  in
  let* () = Sc_rollup_node.run sc_rollup_node' in

  let* rollup_node2_catchup_level =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node'
      commitment_finalized_level
  in
  Check.(rollup_node2_catchup_level = commitment_finalized_level)
    Check.int
    ~error_msg:"Current level has moved past cementation inbox level (%L = %R)" ;
  (* Check that no commitment was published. *)
  let* rollup_node2_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client'
  in
  check_commitment_eq
    ( Option.map (fun (_, c, _) -> c) rollup_node1_published_commitment,
      "published by rollup node 1" )
    ( Option.map (fun (_, c, _) -> c) rollup_node2_published_commitment,
      "published by rollup node 2" ) ;
  let () =
    Check.(
      Option.bind rollup_node1_published_commitment first_published_at_level
      = Option.bind rollup_node2_published_commitment first_published_at_level)
      (Check.option Check.int)
      ~error_msg:
        "Rollup nodes do not agree on level when commitment was first \
         published (%L = %R)"
  in
  return ()

(* Check that the SC rollup is correctly originated with a boot sector.
   -------------------------------------------------------

   Originate a rollup with a custom boot sector and check if the RPC returns it.
*)
let test_rollup_arith_origination_boot_sector =
  let boot_sector = "10 10 10 + +" in

  let go client sc_rollup =
    let* client_boot_sector =
      RPC.Client.call ~hooks client
      @@ RPC.get_chain_block_context_sc_rollup_boot_sector sc_rollup
    in
    let client_boot_sector = JSON.as_string client_boot_sector in
    Check.(boot_sector = client_boot_sector)
      Check.string
      ~error_msg:"expected value %L, got %R" ;
    Lwt.return_unit
  in

  regression_test
    ~__FILE__
    ~tags:["sc_rollup"; "run"]
    (Format.asprintf "originate arith with boot sector")
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      with_fresh_rollup
        ~kind:"arith"
        ~boot_sector
        (fun sc_rollup _sc_rollup_node _filename -> go client sc_rollup)
        node
        client)

(* Check that a node makes use of the boot sector.
   -------------------------------------------------------

   Originate 2 rollups with different boot sectors to check if the are
   actually different.
*)
let test_rollup_node_uses_arith_boot_sector =
  let go_boot client sc_rollup sc_rollup_node =
    let* genesis_info =
      RPC.Client.call ~hooks client
      @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in

    let* () = Sc_rollup_node.run sc_rollup_node in

    let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
    let* level =
      Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node init_level
    in

    let* () = send_text_messages client ["10 +"] in
    let* _ =
      Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node (level + 1)
    in

    Sc_rollup_client.state_hash ~hooks sc_rollup_client
  in

  let with_booted ~boot_sector node client =
    with_fresh_rollup
      ~kind:"arith"
      ~boot_sector
      (fun sc_rollup sc_rollup_node _filename ->
        go_boot client sc_rollup sc_rollup_node)
      node
      client
  in

  regression_test
    ~__FILE__
    ~tags:["sc_rollup"; "run"; "node"]
    (Format.asprintf "ensure arith boot sector is used")
    (fun protocol ->
      setup ~protocol @@ fun node client x ->
      let* state_hash1 =
        with_booted ~boot_sector:"10 10 10 + +" node client x
      in
      let* state_hash2 = with_booted ~boot_sector:"31" node client x in
      Check.(state_hash1 <> state_hash2)
        Check.string
        ~error_msg:"State hashes should be different! (%L, %R)" ;

      Lwt.return_unit)

let test_rollup_arith_uses_reveals =
  let nadd = 32 * 1024 in
  let go_boot client sc_rollup sc_rollup_node =
    let filename =
      let filename, cout = Filename.open_temp_file "sc_rollup" ".in" in
      output_string cout "0 " ;
      for _i = 1 to nadd do
        output_string cout "1 + "
      done ;
      output_string cout "value" ;
      close_out cout ;
      filename
    in
    let* hash =
      Sc_rollup_node.import sc_rollup_node ~pvm_name:"arith" ~filename
    in
    let* genesis_info =
      RPC.Client.call ~hooks client
      @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in

    let* () = Sc_rollup_node.run sc_rollup_node in

    let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
    let* level =
      Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node init_level
    in

    let* () = send_text_messages client ["hash:" ^ hash] in
    let* () = bake_levels 2 client in
    let* _ =
      Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node (level + 1)
    in

    let* encoded_value =
      Sc_rollup_client.state_value ~hooks sc_rollup_client ~key:"vars/value"
    in
    let value =
      match Data_encoding.(Binary.of_bytes int31) @@ encoded_value with
      | Error error ->
          failwith
            (Format.asprintf
               "The arithmetic PVM has an unexpected state: %a"
               Data_encoding.Binary.pp_read_error
               error)
      | Ok x -> x
    in
    Check.(
      (value = nadd) int ~error_msg:"Invalid value in rollup state (%L <> %R)") ;
    return ()
  in

  test_scenario
    ~timeout:120
    {
      tags = ["reveals"];
      variant = "arith";
      description = "rollup node - correct handling of commitments";
    }
  @@ fun _protocol sc_rollup_node sc_rollup _node client ->
  go_boot client sc_rollup sc_rollup_node

(* Initializes a client with an existing account being
   [Constants.tz4_account]. *)
let client_with_initial_keys ~protocol ~kind =
  setup ~protocol @@ with_fresh_rollup ~kind
  @@ fun _sc_rollup sc_rollup_node _filename ->
  let sc_client = Sc_rollup_client.create sc_rollup_node in
  let account = Constant.tz4_account in
  let* () = Sc_rollup_client.import_secret_key account sc_client in
  return (sc_client, account)

(* Check that the client can show the address of a registered account.
   -------------------------------------------------------------------
*)
let test_rollup_client_show_address ~kind =
  test
    ~__FILE__
    ~tags:["run"; "client"]
    "Shows the address of a registered account"
    (fun protocol ->
      let* sc_client, account = client_with_initial_keys ~protocol ~kind in
      let* shown_account =
        Sc_rollup_client.show_address
          ~alias:account.Account.aggregate_alias
          sc_client
      in
      if
        account.aggregate_public_key_hash
        <> shown_account.aggregate_public_key_hash
      then
        failwith
          (Printf.sprintf
             "Expecting %s, got %s as public key hash from the client."
             account.aggregate_public_key_hash
             shown_account.aggregate_public_key_hash)
      else if account.aggregate_public_key <> shown_account.aggregate_public_key
      then
        failwith
          (Printf.sprintf
             "Expecting %s, got %s as public key from the client."
             account.aggregate_public_key
             shown_account.aggregate_public_key)
      else if account.aggregate_secret_key <> shown_account.aggregate_secret_key
      then
        let (Unencrypted sk) = shown_account.aggregate_secret_key in
        let (Unencrypted expected_sk) = shown_account.aggregate_secret_key in
        failwith
          (Printf.sprintf
             "Expecting %s, got %s as secret key from the client."
             expected_sk
             sk)
      else return ())

(* Check that the client can generate keys.
   ----------------------------------------
*)
let test_rollup_client_generate_keys ~kind =
  test
    ~__FILE__
    ~tags:["run"; "client"]
    "Generates new tz4 keys"
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup ~kind
      @@ fun _sc_rollup sc_rollup_node _filename ->
      let sc_client = Sc_rollup_client.create sc_rollup_node in
      let alias = "test_key" in
      let* () = Sc_rollup_client.generate_keys ~alias sc_client in
      let* _account = Sc_rollup_client.show_address ~alias sc_client in
      return ())

(* Check that the client can list keys.
   ------------------------------------
*)
let test_rollup_client_list_keys ~kind =
  test
    ~__FILE__
    ~tags:["run"; "client"]
    "Lists known aliases in the client"
    (fun protocol ->
      let* sc_client, account = client_with_initial_keys ~kind ~protocol in
      let* maybe_keys = Sc_rollup_client.list_keys sc_client in
      let expected_keys =
        [(account.aggregate_alias, account.aggregate_public_key_hash)]
      in
      if List.equal ( = ) expected_keys maybe_keys then return ()
      else
        let pp ppf l =
          Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
            (fun ppf (a, k) -> Format.fprintf ppf "%s: %s" a k)
            ppf
            l
        in
        Test.fail
          ~__LOC__
          "Expecting\n@[%a@]\ngot\n@[%a@]\nas keys from the client."
          pp
          expected_keys
          pp
          maybe_keys)

let publish_dummy_commitment ?(number_of_ticks = 1) ~inbox_level ~predecessor
    ~sc_rollup ~src client =
  let commitment : Sc_rollup_client.commitment =
    {
      compressed_state = Constant.sc_rollup_compressed_state;
      inbox_level;
      predecessor;
      number_of_ticks;
    }
  in
  let*! () = publish_commitment ~src ~commitment client sc_rollup in
  let* () = Client.bake_for_and_wait client in
  get_staked_on_commitment ~sc_rollup ~staker:src client

let test_consecutive_commitments ~kind =
  regression_test
    ~__FILE__
    ~tags:["sc_rollup"; "l1"; "commitment"; kind]
    (Format.asprintf "%s - consecutive commitments" kind)
    (fun protocol ->
      setup ~protocol @@ fun _node client bootstrap1_key ->
      let* inbox_level = Client.level client in
      let* sc_rollup = originate_sc_rollup ~kind ~src:bootstrap1_key client in
      let operator = Constant.bootstrap1.public_key_hash in
      let* {commitment_period_in_blocks; _} = get_sc_rollup_constants client in
      (* As we did no publish any commitment yet, this is supposed to fail. *)
      let*? process =
        RPC.Client.spawn client
        @@ RPC.get_chain_block_context_sc_rollup_staker_staked_on_commitment
             ~sc_rollup
             operator
      in
      let* () = Process.check_error ~msg:(rex "Unknown staker") process in
      let* predecessor, _ =
        last_cemented_commitment_hash_with_level ~sc_rollup client
      in
      let* commit_hash =
        publish_dummy_commitment
          ~inbox_level:(inbox_level + commitment_period_in_blocks + 1)
          ~predecessor
          ~sc_rollup
          ~src:operator
          client
      in
      let* _commit_hash =
        publish_dummy_commitment
          ~inbox_level:(inbox_level + (2 * commitment_period_in_blocks) + 1)
          ~predecessor:commit_hash
          ~sc_rollup
          ~src:operator
          client
      in
      unit)

(* Refutation game scenarios
   -------------------------
*)

(*

   To check the refutation game logic, we evaluate a scenario with one
   honest rollup node and one dishonest rollup node configured as with
   a given [loser_mode].

   For a given sequence of [inputs], distributed amongst several
   levels, with some possible [empty_levels]. We check that at some
   [final_level], the crime does not pay: the dishonest node has losen
   its deposit while the honest one has not.

*)
let test_refutation_scenario ?commitment_period ?challenge_window variant ~kind
    (loser_mode, inputs, final_level, empty_levels, stop_loser_at) =
  test_scenario
    ?commitment_period
    ~kind
    ~timeout:10
    ?challenge_window
    {
      tags = ["refutation"; "node"];
      variant;
      description = "refutation games winning strategies";
    }
  @@ fun _protocol sc_rollup_node sc_rollup_address node client ->
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  let bootstrap2_key = Constant.bootstrap2.public_key_hash in

  let sc_rollup_node2 =
    Sc_rollup_node.create Operator node client ~default_operator:bootstrap2_key
  in
  let* _configuration_filename =
    Sc_rollup_node.config_init ~loser_mode sc_rollup_node2 sc_rollup_address
  in
  let* () = Sc_rollup_node.run sc_rollup_node
  and* () = Sc_rollup_node.run sc_rollup_node2 in
  let sc_client1 = Sc_rollup_client.create sc_rollup_node in

  let start_level = Node.get_level node in

  let stop_loser level =
    if List.mem level stop_loser_at then
      Sc_rollup_node.terminate sc_rollup_node2
    else return ()
  in

  let rec consume_inputs i = function
    | [] -> return ()
    | inputs :: next_batches as all ->
        let level = start_level + i in
        let* () = stop_loser level in
        if List.mem level empty_levels then
          let* () = Client.bake_for_and_wait client in
          consume_inputs (i + 1) all
        else
          let* () =
            Lwt_list.iter_s
              (send_text_messages ~src:Constant.bootstrap3.alias client)
              inputs
          in
          let* () = Client.bake_for_and_wait client in
          consume_inputs (i + 1) next_batches
  in
  let* () = consume_inputs 0 inputs in
  let* after_inputs_level = Client.level client in

  let hook i =
    let level = after_inputs_level + i in
    stop_loser level
  in
  let* () = bake_levels ~hook (final_level - List.length inputs) client in

  let* honest_deposit_json =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_contract_frozen_bonds ~id:bootstrap1_key ()
  in
  let* loser_deposit_json =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_contract_frozen_bonds ~id:bootstrap2_key ()
  in
  let* {stake_amount; _} = get_sc_rollup_constants client in

  Check.(
    (honest_deposit_json = stake_amount)
      Tez.typ
      ~error_msg:"expecting deposit for honest participant = %R, got %L") ;
  Check.(
    (loser_deposit_json = Tez.zero)
      Tez.typ
      ~error_msg:"expecting loss for dishonest participant = %R, got %L") ;
  Log.info "Checking that we can still retrieve state from rollup node" ;
  (* This is a way to make sure the rollup node did not crash *)
  let* _value = Sc_rollup_client.state_hash ~hooks sc_client1 in
  return ()

let rec swap i l =
  if i <= 0 then l
  else match l with [_] | [] -> l | x :: y :: l -> y :: swap (i - 1) (x :: l)

let inputs_for n =
  List.init n @@ fun i ->
  [swap i ["3 3 +"; "1"; "1 1 x"; "3 7 8 + * y"; "2 2 out"]]

let test_refutation protocols ~kind =
  let challenge_window = 10 in
  let commitment_period = 10 in
  [
    ("inbox_proof_at_genesis", ("3 0 0", inputs_for 10, 80, [], []));
    ("pvm_proof_at_genesis", ("3 0 1", inputs_for 10, 80, [], []));
    ("inbox_proof", ("5 0 0", inputs_for 10, 80, [], []));
    ("inbox_proof_with_new_content", ("5 0 0", inputs_for 30, 80, [], []));
    (* In "inbox_proof_with_new_content" we add messages after the commitment
       period so the current inbox is not equal to the inbox snapshot-ted at the
       game creation. *)
    ("inbox_proof_one_empty_level", ("6 0 0", inputs_for 10, 80, [2], []));
    ( "inbox_proof_many_empty_levels",
      ("9 0 0", inputs_for 10, 80, [2; 3; 4], []) );
    ("pvm_proof_0", ("5 0 1", inputs_for 10, 80, [], []));
    ("pvm_proof_1", ("7 1 2", inputs_for 10, 80, [], []));
    ("pvm_proof_2", ("7 2 5", inputs_for 7, 80, [], []));
    ("pvm_proof_3", ("9 2 5", inputs_for 7, 80, [4; 5], []));
    ("timeout", ("5 0 1", inputs_for 10, 80, [], [35]));
  ]
  |> List.iter (fun (variant, inputs) ->
         test_refutation_scenario
           ~kind
           ~challenge_window
           ~commitment_period
           variant
           inputs
           protocols)

(** Helper to check that the operation whose hash is given is successfully
    included (applied) in the current head block. *)
let check_op_included =
  let get_op_status op =
    JSON.(op |-> "metadata" |-> "operation_result" |-> "status" |> as_string)
  in
  fun ~oph client ->
    let* head = RPC.Client.call client @@ RPC.get_chain_block () in
    (* Operations in a block are encoded as a list of lists of operations
       [ consensus; votes; anonymous; manager ]. Manager operations are
       at index 3 in the list. *)
    let ops = JSON.(head |-> "operations" |=> 3 |> as_list) in
    let op_contents =
      match
        List.find_opt (fun op -> oph = JSON.(op |-> "hash" |> as_string)) ops
      with
      | None -> []
      | Some op -> JSON.(op |-> "contents" |> as_list)
    in
    match op_contents with
    | [op] ->
        let status = get_op_status op in
        if String.equal status "applied" then unit
        else
          Test.fail
            ~__LOC__
            "Unexpected operation %s status: got %S instead of 'applied'."
            oph
            status
    | _ ->
        Test.fail
          "Expected to have one operation with hash %s, but got %d"
          oph
          (List.length op_contents)

(** Helper function that allows to inject the given operation in a node, bake a
    block, and check that the operation is successfully applied in the baked
    block. *)
let bake_operation_via_rpc client op =
  let* (`OpHash oph) = Operation.Manager.inject [op] client in
  let* () = Client.bake_for_and_wait client in
  check_op_included ~oph client

(** This helper function constructs the following commitment tree by baking and
    publishing commitments (but without cementing them):
    ---- c1 ---- c2 ---- c31 ---- c311
                  \
                   \---- c32 ---- c321

   Commits c1, c2, c31 and c311 are published by [operator1]. The forking
   branch c32 -- c321 is published by [operator2].
*)
let mk_forking_commitments node client ~sc_rollup ~operator1 ~operator2 =
  let* {commitment_period_in_blocks; _} = get_sc_rollup_constants client in
  (* This is the starting level on top of wich we'll construct the tree. *)
  let starting_level = Node.get_level node in
  let mk_commit ~src ~ticks ~depth ~pred =
    (* Compute the inbox level for which we'd like to commit *)
    let inbox_level = starting_level + (commitment_period_in_blocks * depth) in
    (* d is the delta between the target inbox level and the current level *)
    let d = inbox_level - Node.get_level node in
    (* Bake sufficiently many blocks to be able to commit for the desired inbox
       level. We may actually bake no blocks if d <= 0 *)
    let* () = repeat d (fun () -> Client.bake_for_and_wait client) in
    publish_dummy_commitment
      ~inbox_level
      ~predecessor:pred
      ~sc_rollup
      ~number_of_ticks:ticks
      ~src
      client
  in
  (* Retrieve the latest commitment *)
  let* c0, _ = last_cemented_commitment_hash_with_level ~sc_rollup client in
  (* Construct the tree of commitments. Fork c32 and c321 is published by
     operator2. We vary ticks to have different hashes when commiting on top of
     the same predecessor. *)
  let* c1 = mk_commit ~ticks:1 ~depth:1 ~pred:c0 ~src:operator1 in
  let* c2 = mk_commit ~ticks:2 ~depth:2 ~pred:c1 ~src:operator1 in
  let* c31 = mk_commit ~ticks:31 ~depth:3 ~pred:c2 ~src:operator1 in
  let* c32 = mk_commit ~ticks:32 ~depth:3 ~pred:c2 ~src:operator2 in
  let* c311 = mk_commit ~ticks:311 ~depth:4 ~pred:c31 ~src:operator1 in
  let* c321 = mk_commit ~ticks:321 ~depth:4 ~pred:c32 ~src:operator2 in
  return (c1, c2, c31, c32, c311, c321)

(** This helper initializes a rollup and builds a commitment tree of the form:
    ---- c1 ---- c2 ---- c31 ---- c311
                  \
                   \---- c32 ---- c321
    Then, it calls the given scenario on it.
*)
let test_forking_scenario ~title ~scenario protocols =
  regression_test
    ~__FILE__
    ~tags:["l1"; "commitment"; "cement"; "fork"; "dispute"]
    title
    (fun protocol ->
      (* Choosing challenge_windows to be quite longer than commitment_period
         to avoid being in a situation where the first commitment in the result
         of [mk_forking_commitments] is cementable without further bakes. *)
      let commitment_period = 3 in
      let challenge_window = commitment_period * 7 in
      (* Completely arbitrary as we decide when to trigger timeouts in tests.
         Making it a lot smaller than the default value to speed up tests. *)
      let timeout = 10 in
      setup ~commitment_period ~challenge_window ~timeout ~protocol
      @@ fun node client _bootstrap1_key ->
      (* Originate a Sc rollup. *)
      let* sc_rollup = originate_sc_rollup client ~parameters_ty:"unit" in
      (* Building a forking commitments tree. *)
      let operator1 = Constant.bootstrap1 in
      let operator2 = Constant.bootstrap2 in
      let level0 = Node.get_level node in
      let* commits =
        mk_forking_commitments
          node
          client
          ~sc_rollup
          ~operator1:operator1.public_key_hash
          ~operator2:operator2.public_key_hash
      in
      let level1 = Node.get_level node in
      scenario
        client
        node
        ~sc_rollup
        ~operator1
        ~operator2
        commits
        level0
        level1)
    protocols

(* A more convenient wrapper around [cement_commitment]. *)
let cement_commitments client sc_rollup ?fail =
  Lwt_list.iter_s (fun hash -> cement_commitment client ~sc_rollup ~hash ?fail)

let timeout ?expect_failure ~sc_rollup ~staker client =
  let*! () =
    Client.Sc_rollup.timeout
      ~hooks
      ~dst:sc_rollup
      ~src:"bootstrap1"
      ~staker
      client
      ?expect_failure
  in
  Client.bake_for_and_wait client

(** Given a commitment tree constructed by {test_forking_scenario}, this function:
    - tests different (failing and non-failing) cementation of commitments
      and checks the returned error for each situation (in case of failure);
    - resolves the dispute on top of c2, and checks that the defeated branch
      is removed, while the alive one can be cemented.
*)
let test_no_cementation_if_parent_not_lcc_or_if_disputed_commit protocols =
  test_forking_scenario
    ~title:
      "commitments: publish, and try to cement not on top of LCC or disputed"
    ~scenario:
      (fun client _node ~sc_rollup ~operator1 ~operator2 commits level0 level1 ->
      let c1, c2, c31, c32, c311, c321 = commits in
      let* constants = get_sc_rollup_constants client in
      let challenge_window = constants.challenge_window_in_blocks in

      let cement = cement_commitments client sc_rollup in
      let missing_blocks_to_cement = level0 + challenge_window - level1 in
      let* () =
        if missing_blocks_to_cement <= 0 then unit (* We can already cement *)
        else
          let* () =
            repeat (missing_blocks_to_cement - 1) (fun () ->
                Client.bake_for_and_wait client)
          in
          (* We cannot cement yet! *)
          let* () = cement [c1] ~fail:commit_too_recent in
          (* After these blocks, we should be able to cement all commitments
             (modulo cementation ordering & disputes resolution) *)
          repeat challenge_window (fun () -> Client.bake_for_and_wait client)
      in
      (* We cannot cement any of the commitments before cementing c1 *)
      let* () = cement [c2; c31; c32; c311; c321] ~fail:parent_not_lcc in
      (* But, we can cement c1 and then c2, in this order *)
      let* () = cement [c1; c2] in
      (* We cannot cement c31 or c32 on top of c2 because they are disputed *)
      let* () = cement [c31; c32] ~fail:disputed_commit in
      (* Of course, we cannot cement c311 or c321 because their parents are not
         cemented. *)
      let* () = cement ~fail:parent_not_lcc [c311; c321] in

      (* +++ dispute resolution +++
         Let's resolve the dispute between operator1 and operator2 on the fork
         c31 vs c32. [operator1] will make a bad initial dissection, so it
         loses the dispute, and the branch c32 --- c321 dies. *)

      (* [operator1] starts a dispute. *)
      let module M = Operation.Manager in
      let* () =
        bake_operation_via_rpc client
        @@ M.make ~source:operator2
        @@ M.sc_rollup_refute ~sc_rollup ~opponent:operator1.public_key_hash ()
      in
      (* [operator1] will not play and will be timeout-ed. *)
      let timeout_period = constants.timeout_period_in_blocks in
      let* () =
        repeat (timeout_period + 1) (fun () -> Client.bake_for_and_wait client)
      in
      (* He even timeout himself, what a shame. *)
      let* () = timeout ~sc_rollup ~staker:operator1.public_key_hash client in
      (* Attempting to cement defeated branch will fail. *)
      let* () = cement ~fail:commit_doesnt_exit [c32; c321] in
      (* Now, we can cement c31 on top of c2 and c311 on top of c31. *)
      cement [c31; c311])
    protocols

(** Given a commitment tree constructed by {test_forking_scenario}, this test
    starts a dispute and makes a first valid dissection move.
*)
let test_valid_dispute_dissection protocols =
  test_forking_scenario
    ~title:"valid dispute dissection"
    ~scenario:
      (fun client _node ~sc_rollup ~operator1 ~operator2 commits _level0 _level1 ->
      let c1, c2, c31, c32, _c311, _c321 = commits in
      let cement = cement_commitments client sc_rollup in
      let* constants = get_sc_rollup_constants client in
      let challenge_window = constants.challenge_window_in_blocks in
      let commitment_period = constants.commitment_period_in_blocks in
      let number_of_sections_in_dissection =
        constants.number_of_sections_in_dissection
      in
      let* () =
        (* Be able to cement both c1 and c2 *)
        repeat (challenge_window + commitment_period) (fun () ->
            Client.bake_for_and_wait client)
      in
      let* () = cement [c1; c2] in
      let module M = Operation.Manager in
      (* The source initialises a dispute. *)
      let source = operator2 in
      let opponent = operator1.public_key_hash in
      let* () =
        bake_operation_via_rpc client
        @@ M.make ~source
        @@ M.sc_rollup_refute ~sc_rollup ~opponent ()
      in
      (* Construct a valid dissection with valid initial hash of size
         [sc_rollup.number_of_sections_in_dissection]. The state hash below is
         the hash of the state computed after submitting the first commitment c1
         (which is also equal to states's hashes of subsequent commitments, as we
         didn't add any message in inboxes). If this hash needs to be recomputed,
         run this test with --verbose and grep for 'compressed_state' in the
         produced logs. *)
      let state_hash =
        "scs11VNjWyZw4Tgbvsom8epQbox86S2CKkE1UAZkXMM7Pj8MQMLzMf"
      in

      let rec aux i acc =
        if i = number_of_sections_in_dissection - 1 then
          List.rev ({M.state_hash = None; tick = i} :: acc)
        else aux (i + 1) ({M.state_hash = Some state_hash; tick = i} :: acc)
      in
      (* Inject a valid dissection move *)
      let refutation =
        M.{choice_tick = 0; refutation_step = Dissection (aux 0 [])}
      in

      let* () =
        bake_operation_via_rpc client
        @@ M.make ~source
        @@ M.sc_rollup_refute ~sc_rollup ~opponent ~refutation ()
      in
      (* We cannot cement neither c31, nor c32 because refutation game hasn't
         ended. *)
      cement [c31; c32] ~fail:"Attempted to cement a disputed commitment")
    protocols

(* Testing rollup node catch up mechanism
   --------------------------------------

   The rollup node must be able to catch up from the genesis
   of the rollup when paired with a node in archive mode.
*)
let test_late_rollup_node =
  test_scenario
    {
      tags = ["node"];
      variant = "late";
      description = "a late rollup should catch up";
    }
  @@ fun _protocol sc_rollup_node _sc_rollup_address _node client ->
  let* () = bake_levels 65 client in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* () = bake_levels 30 client in
  let* _status = Sc_rollup_node.wait_for_level ~timeout:2. sc_rollup_node 95 in
  return ()

(* Test interruption of rollup node before the first inbox is processed. Upon
   restart the node should not complain that an inbox is missing. *)
let test_interrupt_rollup_node =
  test_scenario
    {
      tags = ["node"];
      variant = "interrupt";
      description = "a rollup should recover on interruption before first inbox";
    }
  @@ fun _protocol sc_rollup_node _sc_rollup_address _node client ->
  let processing_promise =
    Sc_rollup_node.wait_for
      sc_rollup_node
      "sc_rollup_daemon_process_head.v0"
      (fun _ -> Some ())
  in
  let* () = bake_levels 15 client in
  let* () = Sc_rollup_node.run sc_rollup_node and* () = processing_promise in
  let* () = Sc_rollup_node.terminate ~kill:true sc_rollup_node in
  let* () = bake_levels 1 client in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:20. sc_rollup_node 18 in
  unit

(* Testing the timeout to record gas consumption in a regression trace and
   detect when the value changes.
   For functional tests on timing-out a dispute, see unit tests in
   [lib_protocol].

   For this test, we rely on [test_forking_scenario] to create a tree structure
   of commitments and we start a dispute.
   The first player is not even going to play, we'll simply bake enough blocks
   to get to the point where we can timeout. *)
let test_timeout protocols =
  test_forking_scenario
    ~title:"refutation game timeout"
    ~scenario:
      (fun client _node ~sc_rollup ~operator1 ~operator2 commits level0 level1 ->
      (* These are the commitments on the rollup. See [test_forking_scenario] to
         visualize the tree structure. *)
      let c1, c2, _c31, _c32, _c311, _c321 = commits in
      (* A helper function to cement a sequence of commitments. *)
      let cement = cement_commitments client sc_rollup in
      let* constants = get_sc_rollup_constants client in
      let challenge_window = constants.challenge_window_in_blocks in
      let timeout_period = constants.timeout_period_in_blocks in

      (* Bake enough blocks to cement the commitments up to the divergence. *)
      let* () =
        repeat
          (* There are [level0 - level1 - 1] blocks between [level1] and
             [level0], plus the challenge window for [c1] and the one for [c2].
          *)
          (level0 - level1 - 1 + (2 * challenge_window))
          (fun () -> Client.bake_for_and_wait client)
      in
      let* () = cement [c1; c2] in

      let module M = Operation.Manager in
      (* [operator2] starts a dispute, but won't be playing then. *)
      let* () =
        bake_operation_via_rpc client
        @@ M.make ~source:operator2
        @@ M.sc_rollup_refute ~sc_rollup ~opponent:operator1.public_key_hash ()
      in
      (* Get exactly to the block where we are able to timeout. *)
      let* () =
        repeat (timeout_period + 1) (fun () -> Client.bake_for_and_wait client)
      in
      timeout ~sc_rollup ~staker:operator1.public_key_hash client)
    protocols

let test_refutation_reward_and_punishment protocols =
  regression_test
    ~__FILE__
    ~tags:["l1"; "refutation"; "stake"; "reward"; "punishment"]
    "refutation: check the punishment and reward"
    (fun protocol ->
      (* Timeout is the easiest way to end a game, we set timeout period
         low to produce an outcome quickly. *)
      let timeout_period = 3 in
      let commitment_period = 2 in
      setup ~commitment_period ~timeout:timeout_period ~protocol
      @@ fun node client _ ->
      let* {commitment_period_in_blocks; stake_amount; _} =
        get_sc_rollup_constants client
      in
      let punishment = Tez.to_mutez stake_amount in
      let reward = punishment / 2 in
      (* Originate a Sc rollup. *)
      let* sc_rollup = originate_sc_rollup client ~parameters_ty:"unit" in

      (* Pick the two players and their initial balances. *)
      let operator1 = Constant.bootstrap2 in
      let operator2 = Constant.bootstrap3 in

      let* operator1_balances =
        contract_balances ~pkh:operator1.public_key_hash client
      in
      let* operator2_balances =
        contract_balances ~pkh:operator2.public_key_hash client
      in

      (* Retrieve the origination commitment *)
      let* c0, _ = last_cemented_commitment_hash_with_level ~sc_rollup client in

      (* Compute the inbox level for which we'd like to commit *)
      let starting_level = Node.get_level node in
      let inbox_level = starting_level + commitment_period_in_blocks in
      (* d is the delta between the target inbox level and the current level *)
      let d = inbox_level - Node.get_level node in
      (* Bake sufficiently many blocks to be able to commit for the desired inbox
         level. We may actually bake no blocks if d <= 0 *)
      let* () = repeat d (fun () -> Client.bake_for_and_wait client) in

      (* [operator1] stakes on a commitment. *)
      let* _ =
        publish_dummy_commitment
          ~inbox_level
          ~predecessor:c0
          ~sc_rollup
          ~number_of_ticks:1
          ~src:operator1.public_key_hash
          client
      in
      let* new_operator1_balances =
        contract_balances ~pkh:operator1.public_key_hash client
      in

      Check.(
        (new_operator1_balances.frozen
        = operator1_balances.frozen + Tez.to_mutez stake_amount)
          int
          ~error_msg:"expecting frozen balance for operator1: %R, got %L") ;

      (* [operator2] stakes on a commitment. *)
      let* _ =
        publish_dummy_commitment
          ~inbox_level
          ~predecessor:c0
          ~sc_rollup
          ~number_of_ticks:2
          ~src:operator2.public_key_hash
          client
      in
      let* new_operator2_balances =
        contract_balances ~pkh:operator2.public_key_hash client
      in
      Check.(
        (new_operator2_balances.frozen
        = operator2_balances.frozen + Tez.to_mutez stake_amount)
          int
          ~error_msg:"expecting frozen balance for operator2: %R, got %L") ;

      let module M = Operation.Manager in
      (* [operator1] starts a dispute, but will never play. *)
      let* () =
        bake_operation_via_rpc client
        @@ M.make ~source:operator1
        @@ M.sc_rollup_refute ~sc_rollup ~opponent:operator2.public_key_hash ()
      in
      (* Get exactly to the block where we are able to timeout. *)
      let* () =
        repeat (timeout_period + 1) (fun () -> Client.bake_for_and_wait client)
      in
      let* () = timeout ~sc_rollup ~staker:operator2.public_key_hash client in

      (* The game should have now ended. *)

      (* [operator2] wins half of the opponent's stake. *)
      let* final_operator2_balances =
        contract_balances ~pkh:operator2.public_key_hash client
      in
      Check.(
        (final_operator2_balances.frozen = new_operator2_balances.frozen)
          int
          ~error_msg:"operator2 should keep its frozen deposit: %R, got %L") ;
      Check.(
        (final_operator2_balances.liquid
        = new_operator2_balances.liquid + reward)
          int
          ~error_msg:"operator2 should win a reward: %R, got %L") ;

      (* [operator1] loses all its stake. *)
      let* final_operator1_balances =
        contract_balances ~pkh:operator1.public_key_hash client
      in
      Check.(
        (final_operator1_balances.frozen
        = new_operator1_balances.frozen - punishment)
          int
          ~error_msg:"operator1 should lose its frozen deposit: %R, got %L") ;

      unit)
    protocols

(* Testing the execution of outbox messages
   ----------------------------------------

   When the PVM interprets an input message that produces an output
   message, the outbox in the PVM state is populated with this output
   message. When the state is cemented (after the refutation period
   has passed without refutation), one can trigger the execution of
   the outbox message, that is a call to a given L1 contract.

   This test first populates an L1 contract that waits for an integer
   and stores this integer in its state. Then, the test executes a
   rollup operation that produces a call to this contract. Finally,
   the test triggers this call and we check that the L1 contract has
   been correctly executed by observing its local storage.

   The input depends on the PVM.
*)
let test_outbox_message_generic ?expected_error skip earliness entrypoint
    input_message expected_storage kind =
  let commitment_period = 2 and challenge_window = 5 in
  let variant =
    (if entrypoint = "" then "" else entrypoint) ^ "_" ^ string_of_int earliness
  in
  test_scenario
    ~kind
    ~commitment_period
    ~challenge_window
    {
      tags = ["outbox"];
      variant;
      description = "an outbox message should be executable";
    }
  @@ fun _protocol sc_rollup_node sc_rollup _node client ->
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_client = Sc_rollup_client.create sc_rollup_node in
  let src = Constant.bootstrap1.public_key_hash in
  let originate_target_contract () =
    let prg =
      {|
      {
        parameter (or (int %default) (int %aux));
        storage (int :s);

        code
          {
            UNPAIR;
            IF_LEFT
              { SWAP ; DROP; NIL operation }
              { SWAP ; DROP; NIL operation };
            PAIR;
          }
      } |}
    in
    let* address =
      Client.originate_contract
        ~alias:"target"
        ~amount:(Tez.of_int 100)
        ~burn_cap:(Tez.of_int 100)
        ~src
        ~prg
        ~init:"0"
        client
    in
    let* () = Client.bake_for_and_wait client in
    return address
  in
  let check_contract_execution address expected_storage =
    let* storage = Client.contract_storage address client in
    return
    @@ Check.(
         (storage = expected_storage)
           string
           ~error_msg:"Invalid contract storage: expecting '%R', got '%L'.")
  in
  let perform_rollup_execution_and_cement address =
    let* () = send_text_messages client [input_message address] in
    let blocks_to_wait =
      2 + (2 * commitment_period) + challenge_window - earliness
    in
    repeat blocks_to_wait @@ fun () -> Client.bake_for client
  in
  let trigger_outbox_message_execution address =
    let* outbox = Sc_rollup_client.outbox sc_client in
    Log.info "Outbox is %s" outbox ;
    let* answer =
      let message_index = 0 in
      let outbox_level = 4 in
      let destination = address in
      let parameters = "37" in
      let entrypoint = if entrypoint = "" then None else Some entrypoint in
      Sc_rollup_client.outbox_proof_single
        sc_client
        ?expected_error
        ~message_index
        ~outbox_level
        ~destination
        ?entrypoint
        ~parameters
    in
    match (answer, expected_error) with
    | Some _, Some _ -> assert false
    | None, None -> failwith "Unexpected error during proof generation"
    | None, Some _ -> return ()
    | Some {commitment_hash; proof}, None ->
        let*! () =
          Client.Sc_rollup.execute_outbox_message
            ~burn_cap:(Tez.of_int 10)
            ~rollup:sc_rollup
            ~src
            ~commitment_hash
            ~proof
            client
        in
        Client.bake_for client
  in
  if skip then return ()
  else
    let* target_contract_address = originate_target_contract () in
    let* () = perform_rollup_execution_and_cement target_contract_address in
    let* () = trigger_outbox_message_execution target_contract_address in
    match expected_error with
    | None ->
        let* () =
          check_contract_execution target_contract_address expected_storage
        in
        return ()
    | Some _ -> return ()

let test_outbox_message ?expected_error ~earliness entrypoint ~kind =
  let skip, input_message, expected_storage =
    let entrypoint = if entrypoint = "" then entrypoint else "%" ^ entrypoint in
    match kind with
    | "arith" ->
        ( false,
          (fun contract_address -> "37 " ^ contract_address ^ entrypoint),
          "37\n" )
    | "wasm_2_0_0" ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3790
           For the moment, the WASM PVM has no support for
           output. Hence, the storage is unchanged.*)
        (true, Fun.const "", "0\n")
    | _ ->
        (* There is no other PVM in the protocol. *)
        assert false
  in
  test_outbox_message_generic
    ?expected_error
    skip
    earliness
    entrypoint
    input_message
    expected_storage
    kind

let test_rpcs ~kind =
  test_scenario
    ~kind
    {
      tags = ["rpc"];
      variant = "api";
      description = "RPC API should work and be stable";
    }
  @@ fun _protocol sc_rollup_node sc_rollup node client ->
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_client = Sc_rollup_client.create sc_rollup_node in
  let* sc_rollup_address =
    Sc_rollup_client.rpc_get ~hooks sc_client ["global"; "sc_rollup_address"]
  in
  let sc_rollup_address = JSON.as_string sc_rollup_address in
  Check.((sc_rollup_address = sc_rollup) string)
    ~error_msg:"SC rollup address of node is %L but should be %R" ;
  let level = Node.get_level node in
  let n = 15 in
  let batch_size = 5 in
  let* () = send_messages ~batch_size n client in
  let* _ =
    Sc_rollup_node.wait_for_level ~timeout:3.0 sc_rollup_node (level + n)
  in
  let* l1_block_hash = RPC.Client.call client @@ RPC.get_chain_block_hash () in
  let* l2_block_hash =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "hash"]
  in
  let l2_block_hash = JSON.as_string l2_block_hash in
  Check.((l1_block_hash = l2_block_hash) string)
    ~error_msg:"Head on L1 is %L where as on L2 it is %R" ;
  let* l1_block_hash =
    RPC.Client.call client @@ RPC.get_chain_block_hash ~block:"5" ()
  in
  let* l2_block_hash =
    Sc_rollup_client.rpc_get ~hooks sc_client ["global"; "block"; "5"; "hash"]
  in
  let l2_block_hash = JSON.as_string l2_block_hash in
  Check.((l1_block_hash = l2_block_hash) string)
    ~error_msg:"Block 5 on L1 is %L where as on L2 it is %R" ;
  let* l2_finalied_block_level =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "finalized"; "level"]
  in
  let l2_finalied_block_level = JSON.as_int l2_finalied_block_level in
  Check.((l2_finalied_block_level = level + n - 2) int)
    ~error_msg:"Finalized block is %L but should be %R" ;
  let* l2_num_messages =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "num_messages"]
  in
  let l2_num_messages = JSON.as_int l2_num_messages in
  Check.((l2_num_messages = batch_size + 2) int)
    ~error_msg:"Number of messages of head is %L but should be %R" ;
  let* _status =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "status"]
  in
  let* _ticks =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "ticks"]
  in
  let* _state_hash =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "state_hash"]
  in
  let* _outbox =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "outbox"]
  in
  let* _head =
    Sc_rollup_client.rpc_get ~hooks sc_client ["global"; "tezos_head"]
  in
  let* _level =
    Sc_rollup_client.rpc_get ~hooks sc_client ["global"; "tezos_level"]
  in
  unit

let register ~kind ~protocols =
  test_origination ~kind protocols ;
  test_rollup_node_running ~kind protocols ;
  test_rollup_get_genesis_info ~kind protocols ;
  test_rollup_get_chain_block_context_sc_rollup_last_cemented_commitment_hash_with_level
    ~kind
    protocols ;
  test_rollup_inbox_size ~kind protocols ;
  test_rollup_inbox_of_rollup_node ~kind "basic" basic_scenario protocols ;
  test_rpcs ~kind protocols ;
  (* See above at definition of sc_rollup_node_stops_scenario:

     test_rollup_inbox_of_rollup_node
      ~kind
      "stops"
      sc_rollup_node_stops_scenario
      protocols ;
  *)
  test_rollup_inbox_of_rollup_node
    ~kind
    "disconnects"
    sc_rollup_node_disconnects_scenario
    protocols ;
  test_rollup_inbox_of_rollup_node
    ~kind
    "handles_chain_reorg"
    sc_rollup_node_handles_chain_reorg
    protocols ;
  test_rollup_node_boots_into_initial_state protocols ~kind ;
  test_rollup_node_advances_pvm_state
    protocols
    ~kind
    ~boot_sector:None
    ~internal:false ;
  test_rollup_node_advances_pvm_state
    protocols
    ~kind
    ~boot_sector:None
    ~internal:true ;
  test_commitment_scenario
    "commitment_is_stored"
    commitment_stored
    protocols
    ~kind ;
  test_commitment_scenario
    "robust_to_failures"
    commitment_stored_robust_to_failures
    protocols
    ~kind ;
  test_commitment_scenario
    ~extra_tags:["modes"; "observer"]
    "observer_does_not_publish"
    (mode_publish Observer false)
    protocols
    ~kind ;
  test_commitment_scenario
    ~extra_tags:["modes"; "maintenance"]
    "maintenance_publishes"
    (mode_publish Maintenance true)
    protocols
    ~kind ;
  test_commitment_scenario
    ~extra_tags:["modes"; "batcher"]
    "batcher_does_not_publish"
    (mode_publish Batcher false)
    protocols
    ~kind ;
  test_commitment_scenario
    ~extra_tags:["modes"; "operator"]
    "operator_publishes"
    (mode_publish Operator true)
    protocols
    ~kind ;
  test_commitment_scenario
    ~commitment_period:15
    ~challenge_window:10080
    "node_use_proto_param"
    commitment_stored
    protocols
    ~kind ;
  test_commitment_scenario
    "non_final_level"
    commitment_not_stored_if_non_final
    protocols
    ~kind ;
  test_commitment_scenario
    "messages_reset"
    commitments_messages_reset
    protocols
    ~kind ;
  test_commitment_scenario
    "handles_chain_reorgs"
    (commitments_reorgs ~kind)
    protocols
    ~kind ;
  test_commitment_scenario
    ~challenge_window:1
    "no_commitment_publish_before_lcc"
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/2976
       change tests so that we do not need to repeat custom parameters. *)
    commitment_before_lcc_not_published
    protocols
    ~kind ;
  test_commitment_scenario
    "first_published_at_level_global"
    first_published_level_is_global
    protocols
    ~kind ;
  test_consecutive_commitments protocols ~kind ;
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4020
     When looking at the logs of these tests, it appears that they do
     not come with enough inspection of the state of the rollup to
     ensure the property they are trying to exhibit.  For instance,
     just checking that the dishonest player has no stack at the end
     of the scenario does not prove they have been slashed, and it
     appeared that in some instances, they haven’t been able to
     publish a commitment to begin with. *)
  (* test_refutation protocols ~kind ; *)
  test_late_rollup_node protocols ~kind ;
  test_interrupt_rollup_node protocols ~kind ;
  test_outbox_message ~earliness:0 "" protocols ~kind ;
  test_outbox_message ~earliness:0 "aux" protocols ~kind ;
  test_outbox_message
    ~expected_error:(Base.rex ".*Invalid claim about outbox")
    ~earliness:5
    ""
    protocols
    ~kind ;
  test_outbox_message
    ~expected_error:(Base.rex ".*Invalid claim about outbox")
    ~earliness:5
    "aux"
    protocols
    ~kind

let register ~protocols =
  (* PVM-independent tests. We still need to specify a PVM kind
     because the tezt will need to originate a rollup. However,
     the tezt will not test for PVM kind specific featued. *)
  test_rollup_client_gets_address protocols ~kind:"wasm_2_0_0" ;
  test_rollup_node_configuration protocols ~kind:"wasm_2_0_0" ;
  test_rollup_list protocols ~kind:"wasm_2_0_0" ;
  test_rollup_client_show_address protocols ~kind:"wasm_2_0_0" ;
  test_rollup_client_generate_keys protocols ~kind:"wasm_2_0_0" ;
  test_rollup_client_list_keys protocols ~kind:"wasm_2_0_0" ;
  (* Specific Arith PVM tezts *)
  test_rollup_arith_origination_boot_sector protocols ;
  test_rollup_node_uses_arith_boot_sector protocols ;
  (* Specific Wasm PVM tezts *)
  test_rollup_node_run_with_kernel
    protocols
    ~kind:"wasm_2_0_0"
    ~kernel_name:"no_parse_random"
    ~internal:false ;
  test_rollup_node_run_with_kernel
    protocols
    ~kind:"wasm_2_0_0"
    ~kernel_name:"no_parse_bad_fingerprint"
    ~internal:false ;
  (* DAC tests, not supported yet by the Wasm PVM *)
  test_rollup_arith_uses_reveals protocols ~kind:"arith" ;
  (* Shared tezts - will be executed for both PVMs. *)
  register ~kind:"wasm_2_0_0" ~protocols ;
  register ~kind:"arith" ~protocols ;
  test_no_cementation_if_parent_not_lcc_or_if_disputed_commit protocols ;
  test_valid_dispute_dissection protocols ;
  test_timeout protocols ;
  test_refutation_reward_and_punishment protocols
