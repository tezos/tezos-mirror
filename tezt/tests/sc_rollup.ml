(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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
   Component:    Smart Optimistic Rollups
   Invocation:   dune exec tezt/tests/main.exe -- --file sc_rollup.ml
*)

open Base
open Sc_rollup_helpers

(*

   Helpers
   =======

*)

let default_wasm_pvm_revision = function _ -> "2.0.0-r5"

let max_nb_ticks = 50_000_000_000_000

let get_outbox_proof ?rpc_hooks ~__LOC__ sc_rollup_node ~message_index
    ~outbox_level =
  let* proof =
    Sc_rollup_node.RPC.call ?rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.outbox_proof_simple ~message_index ~outbox_level ()
  in
  match proof with
  | Some v -> return v
  | None -> failwith (Format.asprintf "Unexpected [None] at %s" __LOC__)

(* Number of levels needed to process a head as finalized. This value should
   be the same as `node_context.block_finality_time`, where `node_context` is
   the `Node_context.t` used by the rollup node. For Tenderbake, the
   block finality time is 2. *)
let block_finality_time = 2

(* List of scoru errors messages used in tests below. *)

let commit_too_recent =
  "Attempted to cement a commitment before its refutation deadline"

let disputed_commit = "Attempted to cement a disputed commitment"

let register_test ?kind ?supports ?(regression = false) ~__FILE__ ~tags ?uses
    ~title f =
  let kind_tags =
    match kind with Some "riscv" -> ["riscv"] | Some k -> [k] | _ -> []
  in
  let tags = Tag.etherlink :: "sc_rollup" :: (kind_tags @ tags) in
  if regression then
    Protocol.register_regression_test ?supports ~__FILE__ ~title ~tags ?uses f
  else Protocol.register_test ?supports ~__FILE__ ~title ~tags ?uses f

let get_sc_rollup_commitment_period_in_blocks client =
  let* constants = get_sc_rollup_constants client in
  return constants.commitment_period_in_blocks

let originate_sc_rollups ~kind n client =
  fold n String_map.empty (fun i addrs ->
      let alias = "rollup" ^ string_of_int i in
      let* addr = originate_sc_rollup ~alias ~kind client in
      return (String_map.add alias addr addrs))

let check_l1_block_contains ~kind ~what ?(extra = fun _ -> true) block =
  let ops = JSON.(block |-> "operations" |=> 3 |> as_list) in
  let ops_contents =
    List.map (fun op -> JSON.(op |-> "contents" |> as_list)) ops |> List.flatten
  in
  match
    List.find_all
      (fun content ->
        JSON.(content |-> "kind" |> as_string) = kind && extra content)
      ops_contents
  with
  | [] -> Test.fail "Block does not contain %s" what
  | contents ->
      List.iter
        (fun content ->
          let status =
            JSON.(
              content |-> "metadata" |-> "operation_result" |-> "status"
              |> as_string)
          in
          Check.((status = "applied") string)
            ~error_msg:(sf "%s status in block is %%L instead of %%R" what))
        contents ;
      contents

let wait_for_current_level node ?timeout sc_rollup_node =
  let* current_level = Node.get_level node in
  Sc_rollup_node.wait_for_level ?timeout sc_rollup_node current_level

let gen_keys_then_transfer_tez ?(giver = Constant.bootstrap1.alias)
    ?(amount = Tez.of_int 1_000) client n =
  let* keys, multiple_transfers_json_batch =
    let str_amount = Tez.to_string amount in
    fold n ([], []) (fun _i (keys, json_batch_dest) ->
        let* key = Client.gen_and_show_keys client in
        let json_batch_dest =
          `O
            [("destination", `String key.alias); ("amount", `String str_amount)]
          :: json_batch_dest
        in
        let keys = key :: keys in
        return (keys, json_batch_dest))
  in
  let*! () =
    Client.multiple_transfers
      ~giver
      ~json_batch:(JSON.encode_u (`A multiple_transfers_json_batch))
      ~burn_cap:(Tez.of_int 10)
      client
  in
  let* _ = Client.bake_for_and_wait client in
  return keys

let test_l1_scenario ?supports ?regression ?hooks ~kind ?boot_sector
    ?whitelist_enable ?whitelist ?commitment_period ?challenge_window ?timeout
    ?(src = Constant.bootstrap1.alias) ?rpc_external ?uses ?dal_rewards_weight
    {variant; tags; description} scenario =
  let tags = kind :: tags in
  register_test
    ~kind
    ?supports
    ?regression
    ~__FILE__
    ~tags
    ?uses
    ~title:(format_title_scenario kind {variant; tags; description})
  @@ fun protocol ->
  let* tezos_node, tezos_client =
    setup_l1
      ?commitment_period
      ?challenge_window
      ?dal_rewards_weight
      ?timeout
      ?whitelist_enable
      ?rpc_external
      ~riscv_pvm_enable:(kind = "riscv")
      protocol
  in
  let* sc_rollup =
    originate_sc_rollup ?hooks ~kind ?boot_sector ?whitelist ~src tezos_client
  in
  scenario protocol sc_rollup tezos_node tezos_client

let test_full_scenario ?supports ?regression ?hooks ~kind ?mode ?boot_sector
    ?commitment_period ?(parameters_ty = "string") ?challenge_window ?timeout
    ?timestamp ?rollup_node_name ?whitelist_enable ?whitelist ?operator
    ?operators ?(uses = fun _protocol -> []) ?rpc_external ?allow_degraded
    ?kernel_debug_log ?preimages_dir ?dal_attested_slots_validity_lag
    {variant; tags; description} scenario =
  let uses protocol =
    (Constant.octez_smart_rollup_node :: Option.to_list preimages_dir)
    @ uses protocol
    @ Sc_rollup_helpers.default_boot_sector_uses_of ~kind
  in
  register_test
    ~kind
    ?supports
    ?regression
    ~__FILE__
    ~tags
    ~uses
    ~title:(format_title_scenario kind {variant; tags; description})
  @@ fun protocol ->
  let riscv_pvm_enable = kind = "riscv" in
  let* tezos_node, tezos_client =
    setup_l1
      ?rpc_external
      ?commitment_period
      ?challenge_window
      ?timeout
      ?timestamp
      ?whitelist_enable
      ~riscv_pvm_enable
      ?dal_attested_slots_validity_lag
      protocol
  in
  let operator =
    if Option.is_none operator && Option.is_none operators then
      Some Constant.bootstrap1.alias
    else operator
  in
  let* rollup_node, sc_rollup =
    setup_rollup
      ~parameters_ty
      ~kind
      ?hooks
      ?mode
      ?boot_sector
      ?rollup_node_name
      ?whitelist
      ?operator
      ?operators
      ?allow_degraded
      tezos_node
      tezos_client
  in
  if
    Option.value ~default:false regression
    && Option.value ~default:false kernel_debug_log
  then
    Sc_rollup_node.on_event rollup_node (fun Sc_rollup_node.{name; value; _} ->
        if name = "kernel_debug.v0" then
          Regression.capture
            (Tezos_regression.replace_variables (JSON.as_string value))) ;
  let* () =
    match preimages_dir with
    | None -> Lwt.return_unit
    | Some src_dir ->
        let data_dir = Sc_rollup_node.data_dir rollup_node in
        let dest_dir = Filename.concat data_dir kind in
        (* Copying used here instead of softlink to avoid tampering of artefact
           when debugging from the temp test directory *)
        Process.run "cp" ["-R"; Uses.path src_dir; dest_dir]
  in
  let* () =
    if Sc_rollup_node.monitors_finalized rollup_node then
      (* For rollup nodes that only monitor finalized blocks we start by baking
         two blocks so that they can see the origination block. *)
      repeat 2 (fun () -> Client.bake_for_and_wait tezos_client)
    else unit
  in
  scenario protocol rollup_node sc_rollup tezos_node tezos_client

(*

   Tests
   =====

*)

(* Originate a new SCORU
   ---------------------

   - Rollup addresses are fully determined by operation hashes and origination nonce.
*)
let test_origination ~kind ?boot_sector =
  test_l1_scenario
    ~regression:true
    ~hooks
    ?boot_sector
    {
      variant = None;
      tags = ["origination"];
      description = "origination of a SCORU executes without error";
    }
    ~kind
    (fun _ _ _ _ -> unit)

(* Initialize configuration
   ------------------------

   Can use CLI to initialize the rollup node config file
*)
let test_rollup_node_configuration ~kind =
  test_full_scenario
    {
      variant = None;
      tags = ["configuration"];
      description = "configuration of a smart rollup node is robust";
    }
    ~kind
  @@ fun _protocol _rollup_node sc_rollup tezos_node tezos_client ->
  let config_file = Temp.file "smart-rollup-config.json" in
  let rollup_node =
    Sc_rollup_node.create
      Operator
      ~default_operator:Constant.bootstrap2.alias
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~kind
      ~config_file
  in
  let* _filename = Sc_rollup_node.config_init rollup_node sc_rollup in
  let config = Sc_rollup_node.Config_file.read rollup_node in
  let _rpc_port = JSON.(config |-> "rpc-port" |> as_int) in
  Log.info "Check that config cannot be overwritten" ;
  let p = Sc_rollup_node.spawn_config_init rollup_node sc_rollup in
  let* () =
    Process.check_error
      p
      ~exit_code:1
      ~msg:(rex "Configuration file \".*\" already exists")
  in
  (* Corrupt config file manually *)
  let config_file = Sc_rollup_node.Config_file.filename rollup_node in
  let out_chan = open_out config_file in
  (try
     output_string out_chan "corrupted" ;
     close_out out_chan
   with _ -> close_out out_chan) ;
  (* Overwrite configuration *)
  Log.info "Check that config can be overwritten with --force" ;
  let* (_ : string) =
    Sc_rollup_node.config_init ~force:true rollup_node sc_rollup
  in
  let config = Sc_rollup_node.Config_file.read rollup_node in
  let rpc_port = JSON.(config |-> "rpc-port" |> as_int) in
  Check.((rpc_port = Sc_rollup_node.rpc_port rollup_node) int)
    ~error_msg:"Read %L from overwritten config but expected %R." ;
  Log.info "Check that rollup node cannot be used for annother rollup" ;
  (* Run the rollup node to initialize store and context *)
  let* () = Sc_rollup_node.run rollup_node sc_rollup [] in
  let* () = Sc_rollup_node.terminate rollup_node in
  (* Run a rollup node in the same data_dir, but for a different rollup *)
  let* other_sc_rollup =
    originate_sc_rollup
      ~kind
      ~alias:"rollup2"
      ~src:Constant.bootstrap1.alias
      tezos_client
  in
  let expect_failure () =
    match Sc_rollup_node.process rollup_node with
    | None -> unit
    | Some p ->
        Process.check_error
          ~exit_code:1
          ~msg:(rex "This rollup node was already set up for rollup")
          p
  in
  let run_promise =
    let* () = Sc_rollup_node.run rollup_node other_sc_rollup [] in
    Test.fail "Node for other rollup in same dir run without errors"
  in
  Lwt.choose [run_promise; expect_failure ()]

(* Launching a rollup node
   -----------------------

   A running rollup node can be asked the address of the rollup it is
   interacting with.
*)
let test_rollup_node_running ~kind =
  test_full_scenario
    {
      variant = None;
      tags = ["running"];
      description = "the smart contract rollup node runs on correct address";
    }
    ~kind
  @@ fun _protocol rollup_node sc_rollup _tezos_node _tezos_client ->
  let* () = Sc_rollup_node.run rollup_node sc_rollup [] in
  let* sc_rollup_from_rpc =
    Sc_rollup_node.RPC.call ~rpc_hooks rollup_node
    @@ Sc_rollup_rpc.get_global_smart_rollup_address ()
  in
  if sc_rollup_from_rpc <> sc_rollup then
    failwith
      (Printf.sprintf
         "Expecting %s, got %s when we query the sc rollup node RPC address"
         sc_rollup
         sc_rollup_from_rpc)
  else
    let metrics_addr, metrics_port = Sc_rollup_node.metrics rollup_node in
    let url =
      "http://" ^ metrics_addr ^ ":" ^ string_of_int metrics_port ^ "/metrics"
    in
    let*! metrics = Curl.get_raw url in
    let regexp = Str.regexp "\\(# *HELP.*\n.*# *TYPE.*\n.*\\)+" in
    if not (Str.string_match regexp metrics 0) then
      Test.fail "Unable to read metrics"
    else unit

(** Genesis information and last cemented commitment at origination are correct
----------------------------------------------------------

   We can fetch the hash and level of the last cemented commitment and it's
   initially equal to the origination information.
*)
let test_rollup_get_genesis_info ~kind =
  test_l1_scenario
    {
      variant = None;
      tags = ["genesis_info"; "lcc"];
      description = "genesis info and last cemented are equal at origination";
    }
    ~kind
  @@ fun _protocol sc_rollup tezos_node tezos_client ->
  let* origination_level = Node.get_level tezos_node in
  (* Bake 10 blocks to be sure that the origination_level of rollup is different
     from the level of the head node. *)
  let* () = repeat 10 (fun () -> Client.bake_for_and_wait tezos_client) in
  let* hash, level =
    last_cemented_commitment_hash_with_level ~sc_rollup tezos_client
  in
  let* genesis_info =
    Client.RPC.call tezos_client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let genesis_hash = JSON.(genesis_info |-> "commitment_hash" |> as_string) in
  let genesis_level = JSON.(genesis_info |-> "level" |> as_int) in
  Check.((hash = genesis_hash) string ~error_msg:"expected value %L, got %R") ;
  (* The level of the last cemented commitment should correspond to the
     rollup origination level. *)
  Check.((level = origination_level) int ~error_msg:"expected value %L, got %R") ;
  Check.(
    (genesis_level = origination_level)
      int
      ~error_msg:"expected value %L, got %R") ;
  unit

(** Wait for the [sc_rollup_node_publish_execute_whitelist_update]
    event from the rollup node. *)
let wait_for_publish_execute_whitelist_update node =
  Sc_rollup_node.wait_for
    node
    "smart_rollup_node_publish_execute_whitelist_update.v0"
  @@ fun json ->
  let hash = JSON.(json |-> "hash" |> as_string) in
  let outbox_level = JSON.(json |-> "outbox_level" |> as_int) in
  let index = JSON.(json |-> "message_index" |> as_int) in
  Some (hash, outbox_level, index)

(** Wait for the [sc_rollup_node_publish_execute_whitelist_update]
    event from the rollup node. *)
let wait_for_included_successful_operation node ~operation_kind =
  Sc_rollup_node.wait_for
    node
    "smart_rollup_node_daemon_included_successful_operation.v0"
  @@ fun json ->
  if JSON.(json |-> "kind" |> as_string) = operation_kind then Some () else None

let wait_until_n_batches_are_injected rollup_node ~nb_batches =
  let nb_injected = ref 0 in
  Sc_rollup_node.wait_for rollup_node "injected_ops.v0" @@ fun json ->
  JSON.(
    json |-> "operations" |> as_list
    |> List.iter (fun json ->
           let kind = json |-> "kind" |> as_string in
           if kind = "add_messages" then nb_injected := !nb_injected + 1)) ;
  if !nb_injected = nb_batches then Some () else None

let send_message_batcher_aux ?rpc_hooks client sc_node msgs =
  let batched =
    Sc_rollup_node.wait_for sc_node "batched.v0" (Fun.const (Some ()))
  in
  let added_to_injector =
    Sc_rollup_node.wait_for sc_node "add_pending.v0" (Fun.const (Some ()))
  in
  let injected = wait_for_injecting_event ~tags:["add_messages"] sc_node in
  let* ids =
    Sc_rollup_node.RPC.call sc_node ?rpc_hooks
    @@ Sc_rollup_rpc.post_local_batcher_injection ~messages:msgs ()
  in
  (* New head will trigger injection  *)
  let* () = Client.bake_for_and_wait client in
  (* Injector should get messages right away because the batcher is configured
     to not have minima. *)
  let* _ = batched in
  let* _ = added_to_injector in
  let* _ = injected in
  return ids

let send_message_batcher ?rpc_hooks client sc_node msgs =
  let* ids = send_message_batcher_aux ?rpc_hooks client sc_node msgs in
  (* Next head will include messages  *)
  let* () = Client.bake_for_and_wait client in
  return ids

let send_messages_batcher ?rpc_hooks ?batch_size n client sc_node =
  let batches =
    List.map
      (fun i ->
        let batch_size = match batch_size with None -> i | Some v -> v in
        List.map (fun j -> Format.sprintf "%d-%d" i j) (range 1 batch_size))
      (range 1 n)
  in
  let* rids =
    Lwt_list.fold_left_s
      (fun acc msgs ->
        let* ids = send_message_batcher_aux ?rpc_hooks client sc_node msgs in
        return (List.rev_append ids acc))
      []
      batches
  in
  (* Next head will include messages of last batch *)
  let* () = Client.bake_for_and_wait client in
  return (List.rev rids)

let test_list_metrics_command_regression ~disable_performance_metrics () =
  Regression.register
    ~__FILE__
    ~tags:["metrics"; "smart_rollup_node"]
    ~uses:[Constant.octez_smart_rollup_node]
    ~title:
      Format.(
        sprintf
          "Smart rollup node: list metrics regression (%s performance metrics)"
          (if disable_performance_metrics then "without" else "with"))
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  Sc_rollup_node.list_metrics ~hooks ~disable_performance_metrics ()

(** Regression test to ensure rollup node store schema does not change without a
    migration. *)
let test_store_schema_regression =
  test_full_scenario
    ~regression:true
    {
      variant = None;
      tags = ["store"; "schema"];
      description = "Rollup node: regression on store schema";
    }
    ~kind:"wasm_2_0_0"
  @@ fun _protocol sc_rollup_node sc_rollup _node _client ->
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let sqlite, out =
    Process.spawn_with_stdin
      "sqlite3"
      [Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "store.sqlite"]
  in
  let* () = Lwt_io.write out ".schema\n" in
  let* () = Lwt_io.write out ".exit\n" in
  let* schema = Process.check_and_read_stdout sqlite in
  Regression.capture ~eol:false schema ;
  unit

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
let test_rollup_node_inbox ?(extra_tags = []) ~variant scenario ~kind =
  test_full_scenario
    {
      variant = Some variant;
      tags = ["inbox"] @ extra_tags;
      description = "maintenance of inbox in the rollup node";
    }
    ~kind
  @@ fun _protocol sc_rollup_node sc_rollup node client ->
  let* () = scenario sc_rollup_node sc_rollup node client in
  let* inbox_from_sc_rollup_node =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_inbox ()
  in
  let* inbox_from_tezos_node =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_smart_rollups_all_inbox ()
  in
  let tup_from_struct RPC.{old_levels_messages; level; current_messages_hash} =
    (old_levels_messages, level, current_messages_hash)
  in
  return
  @@ Check.(
       (tup_from_struct inbox_from_sc_rollup_node
       = tup_from_struct inbox_from_tezos_node)
         (tuple3 string int (option string))
         ~error_msg:"expected value %R, got %L")

let basic_scenario sc_rollup_node sc_rollup _node client =
  let num_messages = 2 in
  let expected_level =
    (* We start at level 2 and each message also bakes a block. With 2 messages being sent, we
       must end up at level 4. *)
    4
  in
  (* Here we use the legacy `run` command. *)
  let* _ = Sc_rollup_node.config_init sc_rollup_node sc_rollup in
  let* () = Sc_rollup_node.run ~legacy:true sc_rollup_node sc_rollup [] in
  let* () = send_messages num_messages client in
  let* _ =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node expected_level
  in
  unit

let sc_rollup_node_stops_scenario sc_rollup_node sc_rollup _node client =
  let num_messages = 2 in
  let expected_level =
    (* We start at level 2 and each message also bakes a block. With 2 messages being sent twice, we
       must end up at level 6. *)
    6
  in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* () = send_messages num_messages client in
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () = send_messages num_messages client in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* _ =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node expected_level
  in
  unit

let sc_rollup_node_disconnects_scenario sc_rollup_node sc_rollup node client =
  let num_messages = 5 in
  let* level = Node.get_level node in
  Log.info "we are at level %d" level ;
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup ~event_level:`Debug []
  in
  let* () = send_messages num_messages client in
  let* level = wait_for_current_level node sc_rollup_node in
  let* () = Lwt_unix.sleep 1. in
  Log.info "Terminating Tezos node" ;
  let* () = Node.terminate node in
  Log.info "Waiting before restarting Tezos node" ;
  let* () = Lwt_unix.sleep 3. in
  let refutation_loop_request =
    Sc_rollup_node.wait_for sc_rollup_node "request_completed.v0" @@ fun json ->
    let open JSON in
    if
      json |-> "view" |-> "request" |> as_string = "process"
      && json |-> "view" |-> "block" |-> "level" |> as_int
         = level + num_messages
    then Some ()
    else None
  in
  Log.info "Restarting Tezos node" ;
  let* () = Node.run node Node.[Connections 0; Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node in
  let* () = send_messages num_messages client in
  let* _ =
    Sc_rollup_node.wait_for_level sc_rollup_node (level + num_messages)
  in
  Lwt.choose
    [
      refutation_loop_request;
      (let* () = Lwt_unix.sleep 10. in
       Test.fail "Refutation loop did not process after reconnection");
    ]

let setup_reorg node client =
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* node2, client2 = Client.init_with_node ~nodes_args `Client () in
  let* () = Client.Admin.trust_address client ~peer:node2
  and* () = Client.Admin.trust_address client2 ~peer:node in
  let* () = Client.Admin.connect_address client ~peer:node2 in
  let divergence ~branch1 ~branch2 =
    let* identity2 = Node.wait_for_identity node2 in
    let* () = Client.Admin.kick_peer client ~peer:identity2 in
    let* () = branch1 client in
    let* () = branch2 client2 in
    let* head1 = Client.RPC.call client @@ RPC.get_chain_block_hash ()
    and* head2 = Client.RPC.call client2 @@ RPC.get_chain_block_hash () in
    Check.((head1 <> head2) string)
      ~error_msg:
        "The two nodes should have diverged but didn't. Check definitions of \
         ~branch1 and ~branch2 in your call to divergence from setup_reorg." ;
    (* One extra block in branch2 *)
    let* () = Client.bake_for_and_wait client2 in
    unit
  in
  let trigger_reorg () =
    let* level2 = Node.get_level node2 in
    let* () = Client.Admin.connect_address client ~peer:node2 in
    let* _ = Node.wait_for_level node level2 in
    Log.info "Node 1 has switched to second branch." ;
    let* head1 = Client.RPC.call client @@ RPC.get_chain_block_hash ()
    and* head2 = Client.RPC.call client2 @@ RPC.get_chain_block_hash () in
    Check.((head1 = head2) string)
      ~error_msg:"Node1 should have reorged to %R but is still on %L." ;
    unit
  in
  return (divergence, trigger_reorg)

let setup_double_reorg node client =
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* node2, client2 = Client.init_with_node ~nodes_args `Client ()
  and* node3, client3 = Client.init_with_node ~nodes_args `Client () in
  let* () = Client.Admin.trust_address client ~peer:node2
  and* () = Client.Admin.trust_address client2 ~peer:node
  and* () = Client.Admin.trust_address client ~peer:node3
  and* () = Client.Admin.trust_address client3 ~peer:node
  and* () = Client.Admin.trust_address client2 ~peer:node3
  and* () = Client.Admin.trust_address client3 ~peer:node2 in
  let* () = Client.Admin.connect_address client ~peer:node2
  and* () = Client.Admin.connect_address client ~peer:node3
  and* () = Client.Admin.connect_address client2 ~peer:node3 in
  let divergence ~branch1 ~branch2 =
    let* identity2 = Node.wait_for_identity node2
    and* identity3 = Node.wait_for_identity node3 in
    let* () = Client.Admin.kick_peer client ~peer:identity2
    and* () = Client.Admin.kick_peer client2 ~peer:identity3 in
    let* () = branch1 client in
    let* level = Node.get_level node in
    let* _ = Node.wait_for_level node3 level in
    Log.info "Nodes 1 and 3 have branch1." ;
    let* () = Client.Admin.kick_peer client ~peer:identity3 in
    let* () = branch2 client2 in
    (* One extra block in branch2 *)
    let* () = Client.bake_for_and_wait client2 in
    Log.info "Nodes 1 and 2 are following distinct branches." ;
    (* Two extra blocks in node3 *)
    let* () = Client.bake_for_and_wait client3 in
    let* () = Client.bake_for_and_wait client3 in
    Log.info "Nodes 1 and 2 and 3 are following distinct branches." ;
    unit
  in
  let trigger_reorg () =
    let* level2 = Node.get_level node2 in
    let* level3 = Node.get_level node3 in
    let* () = Client.Admin.connect_address client ~peer:node2 in
    let* _ = Node.wait_for_level node level2 in
    Log.info "Node 1 has switched to second branch." ;
    let* () = Client.Admin.connect_address client ~peer:node3 in
    let* _ = Node.wait_for_level node level3 in
    Log.info "Node 1 has switched back to its first branch (with extra blocks)." ;
    unit
  in
  return (divergence, trigger_reorg)

let sc_rollup_node_handles_chain_reorg sc_rollup_node sc_rollup node client =
  let num_messages = 1 in
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* node', client' = Client.init_with_node ~nodes_args `Client () in
  let* () = Client.Admin.trust_address client ~peer:node'
  and* () = Client.Admin.trust_address client' ~peer:node in
  let* () = Client.Admin.connect_address client ~peer:node' in

  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
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
    unit
  in

  let trigger_reorg () =
    let* () = Client.Admin.connect_address client ~peer:node' in
    let* _ = Node.wait_for_level node 5 in
    Log.info "Nodes are synchronized again." ;
    unit
  in

  let* () = divergence () in
  let* () = trigger_reorg () in
  (* After bringing [node'] back, our SCORU node should see that there is a more attractive head at
     level 5. *)
  let* _ = Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node 5 in
  unit

let bake_levels ?hook n client =
  fold n () @@ fun i () ->
  let* () = match hook with None -> unit | Some hook -> hook i in
  Client.bake_for_and_wait client

(** Bake [at_least] levels.
    Then continues baking until an event happens.
    waiting for the rollup node to catch up to the client's level.
    Returns the event value. *)
let bake_until_event ?hook ?(at_least = 0) ?(timeout = 15.) client ?event_name
    event =
  let event_value = ref None in
  let _ =
    let* return_value = event in
    event_value := Some return_value ;
    unit
  in
  let rec bake_loop i =
    let* () = match hook with None -> unit | Some hook -> hook i in
    let* () = Client.bake_for_and_wait client in
    match !event_value with
    | Some value -> return value
    | None -> bake_loop (i + 1)
  in
  let* () = bake_levels ?hook at_least client in
  let* updated_level =
    Lwt.catch
      (fun () -> Lwt.pick [Lwt_unix.timeout timeout; bake_loop 0])
      (function
        | Lwt_unix.Timeout ->
            Test.fail
              "Timeout of %f seconds reached when waiting for event %a to \
               happen."
              timeout
              (Format.pp_print_option Format.pp_print_string)
              event_name
        | e -> raise e)
  in
  return updated_level

(** Bake [at_least] levels.
    Then continues baking until the rollup node updates the lpc,
    waiting for the rollup node to catch up to the client's level.
    Returns the level at which the lpc was updated. *)
let bake_until_lpc_updated ?hook ?at_least ?timeout client sc_rollup_node =
  let event_name = "smart_rollup_node_commitment_lpc_updated.v0" in
  let event =
    Sc_rollup_node.wait_for sc_rollup_node event_name @@ fun json ->
    JSON.(json |-> "level" |> as_int_opt)
  in
  bake_until_event ?hook ?at_least ?timeout client ~event_name event

let bake_until_lcc_updated ?hook ?at_least ?timeout ~level client sc_rollup_node
    =
  let event_name = "smart_rollup_node_commitment_lcc_updated.v0" in
  let event =
    Sc_rollup_node.wait_for sc_rollup_node event_name @@ fun json ->
    let lcc_level = JSON.(json |-> "level" |> as_int) in
    Log.info "LCC updated to %d" lcc_level ;
    if lcc_level >= level then Some lcc_level else None
  in
  bake_until_event ?hook ?at_least ?timeout client ~event_name event

let bake_until_execute_outbox_message ?at_least ?timeout client rollup_node =
  bake_until_event
    ?at_least
    ?timeout
    client
    ~event_name:"included_successful_operation"
  @@ wait_for_included_successful_operation
       rollup_node
       ~operation_kind:"execute_outbox_message"

(** helpers that send a message then bake until the rollup node
    executes an output message (whitelist_update) *)
let send_messages_then_bake_until_rollup_node_execute_output_message
    ~commitment_period ~challenge_window client rollup_node msg_list =
  let* () = send_text_messages ~hooks ~input_format:`Hex client msg_list in
  let* () =
    bake_until_execute_outbox_message
      ~timeout:5.0
      ~at_least:(commitment_period + challenge_window + 1)
      client
      rollup_node
  and* res = wait_for_publish_execute_whitelist_update rollup_node in
  return res

let map_manager_op_from_block node ~block ~find_map_op_content =
  let* block_ops_json =
    Node.RPC.call node @@ RPC.get_chain_block_operations ~block ()
  in
  let manager_ops = JSON.(block_ops_json |=> 3 |> as_list) in
  let map_op_contents op_json =
    JSON.(op_json |-> "contents" |> as_list)
    |> List.filter_map find_map_op_content
  in
  List.map map_op_contents manager_ops |> List.flatten |> return

let wait_for_included_and_map_ops_content rollup_node node ~timeout
    ~find_map_op_content =
  let block =
    Sc_rollup_node.wait_for rollup_node "included.v0" @@ fun json ->
    Some JSON.(json |-> "block" |> as_string)
  in
  let* block =
    Lwt.catch
      (fun () -> Lwt.pick [Lwt_unix.timeout timeout; block])
      (function
        | Lwt_unix.Timeout ->
            Test.fail "Timeout %fs waiting for included operations." timeout
        | e -> raise e)
  in
  map_manager_op_from_block node ~block ~find_map_op_content

let check_batcher_message_status response status =
  Check.((response = status) string)
    ~error_msg:"Status of message is %L but expected %R."

(* Rollup node batcher *)
let sc_rollup_node_batcher sc_rollup_node sc_rollup node client =
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
  in
  let* _level = Sc_rollup_node.wait_sync sc_rollup_node ~timeout:10. in
  Log.info "Sending one message to the batcher" ;
  let msg1 = "3 3 + out" in
  let* ids =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.post_local_batcher_injection ~messages:[msg1] ()
  in
  let msg1_id = match ids with [i] -> i | _ -> assert false in
  let* retrieved_msg1, status_msg1 =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_batcher_queue_msg_id ~msg_id:msg1_id
  in

  check_batcher_message_status status_msg1 "pending_batch" ;
  Check.((retrieved_msg1 = msg1) string)
    ~error_msg:"Message in queue is %L but injected %R." ;
  let* queue =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_batcher_queue ()
  in
  Check.((queue = [(msg1_id, msg1)]) (list (tuple2 string string)))
    ~error_msg:"Queue is %L but should be %R." ;
  (* This block triggers injection in the injector. *)
  let injected =
    wait_for_injecting_event ~tags:["add_messages"] sc_rollup_node
  in
  let* () = Client.bake_for_and_wait client in
  let* _ = injected in
  let* _msg1, status_msg1 =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_batcher_queue_msg_id ~msg_id:msg1_id
  in
  check_batcher_message_status status_msg1 "injected" ;
  (* We bake so that msg1 is included. *)
  let* () = Client.bake_for_and_wait client in
  let* _msg1, status_msg1 =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_batcher_queue_msg_id ~msg_id:msg1_id
  in
  check_batcher_message_status status_msg1 "included" ;
  let* _ = wait_for_current_level node ~timeout:3. sc_rollup_node in
  Log.info "Sending multiple messages to the batcher" ;
  let msg2 =
    (* "012456789 012456789 012456789 ..." *)
    String.init 2048 (fun i ->
        let i = i mod 11 in
        if i = 10 then ' ' else Char.chr (i + 48))
  in
  let* ids1 =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.post_local_batcher_injection
         ~messages:(List.init 9 (Fun.const msg2))
         ()
  in
  let* ids2 =
    send_message_batcher client sc_rollup_node (List.init 9 (Fun.const msg2))
  in
  let* queue =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_batcher_queue ()
  in
  Check.((queue = []) (list (tuple2 string string)))
    ~error_msg:"Queue is %L should be %empty R." ;
  let* block = Client.RPC.call client @@ RPC.get_chain_block () in
  let contents1 =
    check_l1_block_contains
      ~kind:"smart_rollup_add_messages"
      ~what:"add messages operations"
      block
  in
  let incl_count = 0 in
  let incl_count =
    List.fold_left
      (fun count c -> count + JSON.(c |-> "message" |> as_list |> List.length))
      incl_count
      contents1
  in
  (* We bake to trigger second injection by injector. *)
  let* () = Client.bake_for_and_wait client in
  let* block = Client.RPC.call client @@ RPC.get_chain_block () in
  let contents2 =
    check_l1_block_contains
      ~kind:"smart_rollup_add_messages"
      ~what:"add messages operations"
      block
  in
  let incl_count =
    List.fold_left
      (fun count c -> count + JSON.(c |-> "message" |> as_list |> List.length))
      incl_count
      contents2
  in
  Check.((incl_count = List.length ids1 + List.length ids2) int)
    ~error_msg:"Only %L messages are included instead of %R." ;
  let* genesis_info =
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let* current_level = Node.get_level node in
  let levels =
    levels_to_commitment + init_level - current_level + block_finality_time
  in
  Log.info "Baking %d blocks for commitment of first message" levels ;
  let* _ =
    bake_until_lpc_updated ~at_least:levels ~timeout:5. client sc_rollup_node
  in
  let* _msg1, status_msg1 =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_batcher_queue_msg_id ~msg_id:msg1_id
  in
  check_batcher_message_status status_msg1 "committed" ;
  unit

let rec check_can_get_between_blocks rollup_node ~first ~last =
  if last >= first then
    let* _l2_block =
      Sc_rollup_node.RPC.call ~rpc_hooks rollup_node
      @@ Sc_rollup_rpc.get_global_block_state_hash
           ~block:(string_of_int last)
           ()
    in
    let* _l2_block =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_global_block ~block:(string_of_int last) ()
    in
    check_can_get_between_blocks rollup_node ~first ~last:(last - 1)
  else unit

let test_gc variant ?(tags = []) ~challenge_window ~commitment_period
    ~history_mode =
  let history_mode_str = Sc_rollup_node.string_of_history_mode history_mode in
  test_full_scenario
    {
      tags = ["gc"; history_mode_str; variant] @ tags;
      variant = Some variant;
      description =
        sf
          "garbage collection is triggered and finishes correctly (%s)"
          history_mode_str;
    }
    ~challenge_window
    ~commitment_period
  @@ fun _protocol sc_rollup_node sc_rollup node client ->
  (* GC will be invoked at every available opportunity, i.e. after every new lcc *)
  let gc_frequency = 1 in
  (* We want to bake enough blocks for the LCC to be updated and the GC
     triggered. *)
  let expected_level = 5 * challenge_window in
  (* counts number of times GC was started *)
  let gc_starts = ref 0 in
  (* counts number of times GC finished *)
  let gc_finalisations = ref 0 in
  (* to save the first level at which the GC was started *)
  let first_gc_level = ref (-1) in
  Sc_rollup_node.on_event sc_rollup_node (fun Sc_rollup_node.{name; value; _} ->
      match name with
      | "calling_gc.v0" ->
          (* On each [calling_gc] event, record the level for which it was
             called *)
          let gc_level = JSON.(value |-> "gc_level" |> as_int) in
          let head_level = JSON.(value |-> "head_level" |> as_int) in
          Log.info "Calling GC for %d at level %d" gc_level head_level ;
          if !first_gc_level = -1 then first_gc_level := head_level ;
          incr gc_starts
      | "gc_finished.v0" ->
          (* On each [gc_finished] event, increment a counter *)
          let gc_level = JSON.(value |-> "gc_level" |> as_int) in
          let head_level = JSON.(value |-> "head_level" |> as_int) in
          Log.info "Finished GC for %d at level %d" gc_level head_level ;
          incr gc_finalisations
      | _ -> ()) ;
  let* () =
    Sc_rollup_node.run
      sc_rollup_node
      sc_rollup
      [Gc_frequency gc_frequency; History_mode history_mode]
  in
  let* origination_level = Node.get_level node in
  (* We start at level 2, bake until the expected level *)
  let* () = bake_levels (expected_level - origination_level) client in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node expected_level
  in
  let expected_gc_calls =
    match history_mode with
    | Archive -> 0 (* No GC in archive mode *)
    | Full -> ((level - !first_gc_level) / commitment_period) + 1
  in
  (* Check that GC was launched at least the expected number of times,
   * at or after the expected level. This check is not an equality in order
   * to avoid flakiness due to GC being launched slightly later than
   * the expected level. *)
  Check.(
    (!gc_starts <= expected_gc_calls)
      int
      ~error_msg:"Expected at most %R GC calls, instead started %L times") ;
  assert (!gc_finalisations <= !gc_starts) ;
  (* We expect the first available level to be the one corresponding
   * to the lcc for the full mode or the genesis for archive mode *)
  let* {first_available_level; _} =
    Sc_rollup_node.RPC.call sc_rollup_node @@ Sc_rollup_rpc.get_local_gc_info ()
  in
  Log.info "First available level %d" first_available_level ;
  (* Check that RPC calls for blocks which were not GC'ed still return *)
  let* () =
    check_can_get_between_blocks
      sc_rollup_node
      ~first:first_available_level
      ~last:level
  in
  let* {code; _} =
    Sc_rollup_node.RPC.call_raw sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block
         ~block:(string_of_int (first_available_level - 1))
         ()
  in
  Check.(
    (code = 500) ~__LOC__ int ~error_msg:"Attempting to access data for level") ;

  Log.info "Checking that commitment publication data was not completely erased" ;
  let* lcc_hash, _lcc_level =
    Sc_rollup_helpers.last_cemented_commitment_hash_with_level ~sc_rollup client
  in
  let* lcc =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_commitments ~commitment_hash:lcc_hash ()
  in
  let* () =
    if lcc.published_at_level = None then
      Test.fail
        ~__LOC__
        "Commitment was published but publication info is not available \
         anymore."
    else unit
  in
  let* context_files =
    Process.run_and_read_stdout
      "ls"
      [Sc_rollup_node.data_dir sc_rollup_node ^ "/context/"]
  in
  let last_suffix =
    String.split_on_char '\n' context_files
    |> List.filter_map (fun s -> s =~* rex "store\\.(\\d+)\\.suffix")
    |> List.rev |> List.hd
  in
  let nb_suffix = int_of_string last_suffix in
  let split_period = max (challenge_window / 5) 1 in
  let max_nb_split =
    match history_mode with
    | Archive -> 0
    | _ -> (level - first_available_level + 1) / split_period
  in
  Check.((nb_suffix <= max_nb_split) int)
    ~error_msg:"Expected at most %R context suffix files, instead got %L" ;
  unit

(* Testing that snapshots can be exported correctly for a running node, and that
   they can be used to bootstrap a blank or existing rollup node.
   - we run two rollup nodes but stop the second one at some point
   - after a while we create a snapshot from the first rollup node
   - we import the snapshot in the second and a fresh rollup node
   - we ensure they are all synchronized
   - we also try to import invalid snapshots to make sure they are rejected. *)
let test_snapshots ?(unsafe_pvm_patches = false) ~kind ~challenge_window
    ~commitment_period ~history_mode ~compact =
  let history_mode_str = Sc_rollup_node.string_of_history_mode history_mode in

  let whitelist =
    if unsafe_pvm_patches then Some [Constant.bootstrap1.public_key_hash]
    else None
  in
  test_full_scenario
    ?whitelist
    {
      tags =
        (["snapshot"; history_mode_str]
        @ (if compact then ["compact"] else [])
        @ if unsafe_pvm_patches then ["unsafe_pvm_patches"] else []);
      variant = None;
      description =
        sf
          "snapshot can be exported and checked (%s%s%s)"
          history_mode_str
          (if compact then " compact" else "")
          (if unsafe_pvm_patches then " unsafe_pvm_patches" else "");
    }
    ~kind
    ~challenge_window
    ~commitment_period
  @@ fun _protocol sc_rollup_node sc_rollup node client ->
  (* Originate another rollup for sanity checks *)
  let* other_rollup = originate_sc_rollup ~alias:"other_rollup" ~kind client in
  (* We want to produce snapshots for rollup node which have cemented
     commitments *)
  let* level = Node.get_level node in
  let level_snapshot = level + (commitment_period + 4) in
  (* We want to build an L2 chain that goes beyond the snapshots (and has
     additional commitments). *)
  let total_blocks = level_snapshot + (2 * commitment_period) in
  let stop_rollup_node_2_levels = challenge_window + 2 in
  let maybe_add_unsafe_pvm_patches_in_config sc_rollup_node =
    if unsafe_pvm_patches then
      let* () =
        Process.check
        @@ Sc_rollup_node.spawn_config_init sc_rollup_node sc_rollup
      in
      return
      @@ Sc_rollup_node.Config_file.update
           sc_rollup_node
           (Sc_rollup_node.patch_config_unsafe_pvm_patches
              (* arbitrary value *)
              [Increase_max_nb_ticks 55_000_000_000_000])
    else unit
  in
  let maybe_add_apply_unsafe_patches_arg l =
    if unsafe_pvm_patches then Sc_rollup_node.Apply_unsafe_patches :: l else l
  in
  let* () = maybe_add_unsafe_pvm_patches_in_config sc_rollup_node in
  let* () =
    Sc_rollup_node.run
      sc_rollup_node
      sc_rollup
      (maybe_add_apply_unsafe_patches_arg [History_mode history_mode])
  in
  (* We run the other nodes in mode observer because we only care if they can
     catch up. *)
  let rollup_node_2 =
    Sc_rollup_node.create Observer node ~kind ~base_dir:(Client.base_dir client)
  in
  let rollup_node_3 =
    Sc_rollup_node.create Observer node ~kind ~base_dir:(Client.base_dir client)
  in
  let rollup_node_4 =
    Sc_rollup_node.create Observer node ~kind ~base_dir:(Client.base_dir client)
  in
  let rollup_node_5 =
    Sc_rollup_node.create Observer node ~kind ~base_dir:(Client.base_dir client)
  in
  let* () = maybe_add_unsafe_pvm_patches_in_config rollup_node_2 in
  let* () = maybe_add_unsafe_pvm_patches_in_config rollup_node_3 in
  let* () = maybe_add_unsafe_pvm_patches_in_config rollup_node_4 in
  let* () = maybe_add_unsafe_pvm_patches_in_config rollup_node_5 in
  let* () =
    Sc_rollup_node.run
      rollup_node_2
      sc_rollup
      (maybe_add_apply_unsafe_patches_arg [History_mode history_mode])
  in
  let* () =
    Sc_rollup_node.run
      rollup_node_4
      other_rollup
      (maybe_add_apply_unsafe_patches_arg [History_mode history_mode])
  in
  let* () =
    match history_mode with
    | Full -> unit
    | Archive ->
        (* Run another rollup node in full mode, to check that it can import
           archive snapshots (it needs to register its mode on disk). *)
        let* () =
          Sc_rollup_node.run
            rollup_node_5
            sc_rollup
            (maybe_add_apply_unsafe_patches_arg [History_mode Full])
        in
        let* (_ : int) = Sc_rollup_node.wait_sync rollup_node_5 ~timeout:3. in
        Sc_rollup_node.terminate rollup_node_5
  in
  let rollup_node_processing =
    let* () = bake_levels stop_rollup_node_2_levels client in
    Log.info "Stopping rollup node 2 and 4 before snapshot is made." ;
    let* () = Sc_rollup_node.terminate rollup_node_2 in
    let* () = Sc_rollup_node.terminate rollup_node_4 in
    let* () = bake_levels (total_blocks - stop_rollup_node_2_levels) client in
    let* (_ : int) = Sc_rollup_node.wait_sync sc_rollup_node ~timeout:3. in
    unit
  in
  let* (_ : int) =
    Sc_rollup_node.wait_for_level sc_rollup_node level_snapshot
  in
  let dir = Tezt.Temp.dir "snapshots" in
  let* snapshot_file =
    Sc_rollup_node.export_snapshot ~compact sc_rollup_node dir |> Runnable.run
  in
  let* exists = Lwt_unix.file_exists snapshot_file in
  if not exists then
    Test.fail ~__LOC__ "Snapshot file %s does not exist" snapshot_file ;
  let* () = rollup_node_processing in
  Log.info "Try importing snapshot for wrong rollup." ;
  let*? process_other =
    Sc_rollup_node.import_snapshot ~force:true rollup_node_4 ~snapshot_file
  in
  let* () =
    Process.check_error
      ~msg:(rex "The existing rollup node is for")
      process_other
  in
  Log.info "Importing snapshot in empty rollup node." ;
  let*! () =
    Sc_rollup_node.import_snapshot
      ~apply_unsafe_patches:unsafe_pvm_patches
      rollup_node_3
      ~snapshot_file
  in
  (* rollup_node_2 was stopped before so it has data but is late with respect to
     sc_rollup_node. *)
  Log.info "Try importing snapshot in already populated rollup node." ;
  let*? populated =
    Sc_rollup_node.import_snapshot rollup_node_2 ~snapshot_file
  in
  let* () = Process.check_error ~msg:(rex "is already populated") populated in
  Log.info "Importing snapshot in late rollup node." ;
  let*! () =
    Sc_rollup_node.import_snapshot
      ~apply_unsafe_patches:unsafe_pvm_patches
      ~force:true
      rollup_node_2
      ~snapshot_file
  in
  let* () =
    match history_mode with
    | Full -> unit
    | Archive ->
        Log.info "Import %s snapshot into full node." history_mode_str ;
        let*! () =
          Sc_rollup_node.import_snapshot
            ~force:true
            ~apply_unsafe_patches:unsafe_pvm_patches
            rollup_node_5
            ~snapshot_file
        in
        unit
  in
  Log.info "Running rollup nodes with snapshots until they catch up." ;
  let* () =
    Sc_rollup_node.run rollup_node_2 sc_rollup [History_mode history_mode]
  and* () =
    Sc_rollup_node.run rollup_node_3 sc_rollup [History_mode history_mode]
  in
  let* _ = Sc_rollup_node.wait_sync ~timeout:60. rollup_node_2
  and* _ = Sc_rollup_node.wait_sync ~timeout:60. rollup_node_3 in
  Log.info "Try importing outdated snapshot." ;
  let* () = Sc_rollup_node.terminate rollup_node_2 in
  let*? outdated =
    Sc_rollup_node.import_snapshot ~force:true rollup_node_2 ~snapshot_file
  in
  let* () =
    Process.check_error
      ~msg:(rex "The rollup node is already at level")
      outdated
  in
  Log.info "Bake until next commitment." ;
  let* () =
    let event_name = "smart_rollup_node_new_commitment.v0" in
    bake_until_event client ~event_name
    @@ Sc_rollup_node.wait_for sc_rollup_node event_name (Fun.const (Some ()))
  in
  let* _ = Sc_rollup_node.wait_sync ~timeout:30.0 sc_rollup_node in
  let*! snapshot_file =
    Sc_rollup_node.export_snapshot ~compact sc_rollup_node dir
  in
  (* The rollup node should not have published its commitment yet *)
  Log.info "Try importing snapshot without published commitment." ;
  let* () = Sc_rollup_node.terminate rollup_node_2 in
  let*? unpublished =
    Sc_rollup_node.import_snapshot ~force:true rollup_node_2 ~snapshot_file
  in
  let* () =
    Process.check_error
      ~msg:(rex "Commitment of snapshot is not published on L1.")
      unpublished
  in
  unit

(* One can retrieve the list of originated SCORUs.
   -----------------------------------------------
*)

let test_rollup_list ~kind =
  register_test
    ~kind
    ~__FILE__
    ~tags:["sc_rollup"; "list"]
    ~title:"list originated rollups"
  @@ fun protocol ->
  let* _node, client = setup_l1 protocol in
  let* rollups =
    Client.RPC.call client @@ RPC.get_chain_block_context_smart_rollups_all ()
  in
  let () =
    match rollups with
    | _ :: _ ->
        failwith "Expected initial list of originated SCORUs to be empty"
    | [] -> ()
  in
  let* scoru_addresses = originate_sc_rollups ~kind 10 client in
  let scoru_addresses =
    String_map.fold
      (fun _alias addr addrs -> String_set.add addr addrs)
      scoru_addresses
      String_set.empty
  in
  let* rollups =
    Client.RPC.call client @@ RPC.get_chain_block_context_smart_rollups_all ()
  in
  let rollups = String_set.of_list rollups in
  Check.(
    (rollups = scoru_addresses)
      (comparable_module (module String_set))
      ~error_msg:"%L %R") ;
  unit

let test_client_wallet ~kind =
  register_test
    ~kind
    ~__FILE__
    ~tags:["sc_rollup"; "wallet"; "client"]
    ~title:"test the client wallet for smart rollup"
  @@ fun protocol ->
  let* _node, client = setup_l1 protocol in
  let* expected_alias_addresses = originate_sc_rollups ~kind 10 client in
  let*! found_alias_addresses =
    Client.Sc_rollup.list_known_smart_rollups client
  in
  let found_alias_addresses =
    List.fold_left
      (fun addrs (alias, addr) -> String_map.add alias addr addrs)
      String_map.empty
      found_alias_addresses
  in
  String_map.iter
    (fun alias addr ->
      if not (String_map.mem alias found_alias_addresses) then
        Test.fail "alias %s does not exist in the wallet." alias
      else
        let found_addr = String_map.find alias found_alias_addresses in
        Check.((found_addr = addr) string ~error_msg:"%L %R"))
    expected_alias_addresses ;
  let*? process = Client.Sc_rollup.forget_all_smart_rollups client in
  let* output_err =
    Process.check_and_read_stderr ~expect_failure:true process
  in
  Check.(
    (output_err =~ rex ".*this can only be used with option --force.*")
      ~error_msg:"Expected output %L to match expression %R.") ;
  let*! () = Client.Sc_rollup.forget_all_smart_rollups ~force:true client in
  let*! found_alias_addresses =
    Client.Sc_rollup.list_known_smart_rollups client
  in
  Check.(
    (found_alias_addresses = [])
      (list (tuple2 string string))
      ~error_msg:"Expected output %L to be empty.") ;
  let expected_address = String_map.find "rollup1" expected_alias_addresses in
  let alias = "my_rollup" in
  let*! () =
    Client.Sc_rollup.remember_smart_rollup
      client
      ~alias
      ~address:expected_address
  in
  let*! found_address =
    Client.Sc_rollup.show_known_smart_rollup ~alias client
  in
  Check.(
    (String.trim found_address = expected_address)
      string
      ~error_msg:"Expected address %L to be %R.") ;
  unit

(* Make sure the rollup node boots into the initial state.
   -------------------------------------------------------

   When a rollup node starts, we want to make sure that in the absence of
   messages it will boot into the initial state.
*)
let test_rollup_node_boots_into_initial_state ?supports ~kind =
  test_full_scenario
    ?supports
    {
      variant = None;
      tags = ["bootstrap"];
      description = "rollup node boots into the initial state";
    }
    ~kind
  @@ fun _protocol sc_rollup_node sc_rollup _node client ->
  let* genesis_info =
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node init_level
  in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* ticks =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  Check.(ticks = 0)
    Check.int
    ~error_msg:"Unexpected initial tick count (%L = %R)" ;
  let* status =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_status ()
  in
  let expected_status =
    match kind with
    | "arith" -> "Halted"
    | "wasm_2_0_0" -> "Waiting for input message"
    | "riscv" -> "Evaluating"
    | _ -> raise (Invalid_argument kind)
  in
  Check.(status = expected_status)
    Check.string
    ~error_msg:"Unexpected PVM status (%L = %R)" ;
  unit

(* `inbox_file` is expected to be in the format produced by the Etherlink
 * benchmarking tool *)
let read_riscv_test_inbox inbox_file =
  let inbox =
    JSON.(
      Uses.path inbox_file |> parse_file |> as_list |> List.map as_list
      |> List.concat)
  in
  List.map (fun m -> JSON.(m |-> "external" |> as_string)) inbox

(* Originate a rollup with the given boot sector, then send 1 message per level
 * and record the output of the kernel debug log. *)
let test_advances_state_with_kernel ~title ~boot_sector ~kind ~inbox_file =
  test_full_scenario
    ~regression:true
    ~kernel_debug_log:true
    ~hooks
    {variant = None; tags = ["pvm"]; description = title}
    ~boot_sector
    ~kind
  @@ fun _protocol sc_rollup_node sc_rollup _tezos_node client ->
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* _ = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in

  let test_message message =
    let* prev_state_hash =
      Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_state_hash ()
    in
    let* () = send_message ~hooks client (sf "hex:[ \"%s\" ]" message) in
    let* _ = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
    let* state_hash =
      Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_state_hash ()
    in
    Check.(state_hash <> prev_state_hash)
      Check.string
      ~error_msg:"State hash has not changed (%L <> %R)" ;
    unit
  in
  let* () = Lwt_list.iter_s test_message (read_riscv_test_inbox inbox_file) in
  if Sc_rollup_node.monitors_finalized sc_rollup_node then
    (* Allow RISC-V rollup node to see latest messages for regression *)
    let* () = bake_levels 2 client in
    let* _ = Sc_rollup_node.wait_sync ~timeout:10. sc_rollup_node in
    unit
  else unit

let test_rollup_node_advances_pvm_state ?regression ?kernel_debug_log ~title
    ?boot_sector ~internal ~kind ?preimages_dir =
  test_full_scenario
    ?regression
    ?kernel_debug_log
    ~hooks
    {
      variant = Some (if internal then "internal" else "external");
      tags = ["pvm"];
      description = title;
    }
    ?boot_sector
    ~parameters_ty:"bytes"
    ~kind
    ?preimages_dir
  @@ fun protocol sc_rollup_node sc_rollup _tezos_node client ->
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* _ = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  let* forwarder =
    if not internal then return None
    else
      let* contract_id = originate_forward_smart_contract client protocol in
      let* _ = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
      return (Some contract_id)
  in
  (* Called with monotonically increasing [i] *)
  let test_message i =
    let* prev_state_hash =
      Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_state_hash ()
    in
    let* prev_ticks =
      Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_total_ticks ()
    in
    let message = sf "%d %d + value" i ((i + 2) * 2) in
    let* () =
      match forwarder with
      | None ->
          (* External message *)
          send_message ~hooks client (sf "[%S]" message)
      | Some forwarder ->
          (* Internal message through forwarder *)
          let message = hex_encode message in
          let* () =
            Client.transfer
              client
              ~amount:Tez.zero
              ~giver:Constant.bootstrap1.alias
              ~receiver:forwarder
              ~arg:(sf "Pair 0x%s %S" message sc_rollup)
          in
          Client.bake_for_and_wait client
    in
    let* () =
      if Sc_rollup_node.monitors_finalized sc_rollup_node then
        bake_levels 2 client
      else unit
    in
    let* _ = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in

    (* specific per kind PVM checks *)
    let* () =
      match kind with
      | "arith" ->
          let* encoded_value =
            Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
            @@ Sc_rollup_rpc.get_global_block_state ~key:"vars/value" ()
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
          unit
      | "wasm_2_0_0" ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/3729

              Add an appropriate check for various test kernels

                computation.wasm               - Gets into eval state
                no_parse_random.wasm           - Stuck state due to parse error
                no_parse_bad_fingerprint.wasm  - Stuck state due to parse error
          *)
          unit
      | "riscv" -> unit
      | _otherwise -> raise (Invalid_argument kind)
    in

    let* state_hash =
      Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_state_hash ()
    in
    Check.(state_hash <> prev_state_hash)
      Check.string
      ~error_msg:"State hash has not changed (%L <> %R)" ;
    let* ticks =
      Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_total_ticks ()
    in
    Check.(ticks >= prev_ticks)
      Check.int
      ~error_msg:"Tick counter did not advance (%L >= %R)" ;

    unit
  in
  let* () = Lwt_list.iter_s test_message (range 1 10) in

  unit

let test_rollup_node_run_with_kernel ~kind ~kernel_name ~internal =
  test_rollup_node_advances_pvm_state
    ~title:(Format.sprintf "runs with kernel - %s" kernel_name)
    ~boot_sector:(read_kernel kernel_name)
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
let test_rollup_node_advances_pvm_state ~kind ?boot_sector ?kernel_debug_log
    ~internal ?preimages_dir =
  test_rollup_node_advances_pvm_state
    ~regression:true
    ?kernel_debug_log
    ~title:"node advances PVM state with messages"
    ?boot_sector
    ~internal
    ~kind
    ?preimages_dir
(* Ensure that commitments are stored and published properly.
   ----------------------------------------------------------

   Every 20 level, a commitment is computed and stored by the
   rollup node. The rollup node will also publish previously
   computed commitments on the layer1, in a first in first out
   fashion. To ensure that commitments are robust to chain
   reorganisations, only finalized block are processed when
   trying to publish a commitment.
*)

let eq_commitment_typ =
  Check.equalable
    (fun ppf (c : RPC.smart_rollup_commitment) ->
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
  Check.((commitment = expected_commitment) eq_commitment_typ)
    ~error_msg:
      (sf
         "Commitment %s differs from the one %s.\n%s: %%L\n%s: %%R"
         name
         exp_name
         (String.capitalize_ascii name)
         (String.capitalize_ascii exp_name))

let tezos_client_get_commitment client sc_rollup commitment_hash =
  let* commitment_opt =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_commitment
         ~sc_rollup
         ~hash:commitment_hash
         ()
  in
  return commitment_opt

let check_published_commitment_in_l1 ?(force_new_level = true) sc_rollup client
    (published_commitment : Sc_rollup_rpc.commitment_info) =
  let* () =
    if force_new_level then
      (* Triggers injection into the L1 context *)
      bake_levels 1 client
    else unit
  in
  let* commitment_in_l1 =
    tezos_client_get_commitment
      client
      sc_rollup
      published_commitment.commitment_and_hash.hash
  in
  let published_commitment =
    published_commitment.commitment_and_hash.commitment
  in
  check_commitment_eq
    (commitment_in_l1, "in L1")
    (published_commitment, "published") ;
  unit

let test_commitment_scenario ?supports ?(commitment_period = 10)
    ?challenge_window ?(extra_tags = []) ~variant =
  test_full_scenario
    ?supports
    ~commitment_period
    ?challenge_window
    {
      tags = ["commitment"] @ extra_tags;
      variant = Some variant;
      description = "rollup node - correct handling of commitments";
    }

let bake_levels ?hook n client =
  fold n () @@ fun i () ->
  let* () = match hook with None -> unit | Some hook -> hook i in
  Client.bake_for_and_wait client

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
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
  in
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
  let* {
         commitment = {inbox_level = stored_inbox_level; _} as stored_commitment;
         hash = _;
       } =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_last_stored_commitment ()
  in
  Check.(stored_inbox_level = levels_to_commitment + init_level)
    Check.int
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  let* _level = bake_until_lpc_updated client sc_rollup_node in
  let* published_commitment =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  check_commitment_eq
    (stored_commitment, "stored")
    (published_commitment.commitment_and_hash.commitment, "published") ;
  check_published_commitment_in_l1 sc_rollup client published_commitment

let mode_publish mode publishes _protocol sc_rollup_node sc_rollup node client =
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* node', client' = Client.init_with_node ~nodes_args `Client () in
  let* () = Client.Admin.trust_address client ~peer:node'
  and* () = Client.Admin.trust_address client' ~peer:node in
  let* () = Client.Admin.connect_address client ~peer:node' in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let* () = send_messages levels_to_commitment client in
  let* level = Node.get_level node in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node level in
  Log.info "Starting other rollup node." ;
  let purposes = [Sc_rollup_node.Operating; Cementing; Batching] in
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
      node'
      ~base_dir:(Client.base_dir client')
      ~kind:(Sc_rollup_node.kind sc_rollup_node)
      ~operators
      ~default_operator:Constant.bootstrap3.alias
  in
  let* () = Sc_rollup_node.run sc_rollup_other_node sc_rollup [] in
  let* _level = Sc_rollup_node.wait_for_level sc_rollup_other_node level in
  Log.info "Other rollup node synchronized." ;
  let* () = send_messages levels_to_commitment client in
  let* level = Node.get_level node in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node level
  and* _ = Sc_rollup_node.wait_for_level sc_rollup_other_node level in
  Log.info "Both rollup nodes have reached level %d." level ;
  let* state_hash =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ()
  in
  let* state_hash_other =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_other_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ()
  in
  Check.((state_hash = state_hash_other) string)
    ~error_msg:
      "State hash of other rollup node is %R but the first rollup node has %L" ;
  let* {body = published_commitment; _} =
    Sc_rollup_node.RPC.call_json sc_rollup_node
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  let* {body = other_published_commitment; _} =
    Sc_rollup_node.RPC.call_json sc_rollup_other_node
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  if JSON.is_null published_commitment then
    Test.fail "Operator has not published a commitment but should have." ;
  if JSON.is_null other_published_commitment = publishes then
    Test.fail
      "Other has%s published a commitment but should%s."
      (if publishes then " not" else "")
      (if publishes then " have" else " never do so") ;
  unit

let commitment_not_published_if_non_final _protocol sc_rollup_node sc_rollup
    _node client =
  (* The rollup is originated at level `init_level`, and it requires
     `sc_rollup_commitment_period_in_blocks` levels to store a commitment.
     There is also a delay of `block_finality_time` before publishing a
     commitment, to avoid including wrong commitments due to chain
     reorganisations. Therefore the commitment will be published
     when the [Commitment] module processes the block at level
     `init_level + sc_rollup_commitment_period_in_blocks +
     levels_to_finalise`. At the level before, the commitment will be
     neither stored but not published.
  *)
  let* genesis_info =
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let levels_to_finalize = block_finality_time - 1 in
  let store_commitment_level = init_level + levels_to_commitment in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
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
  let* {commitment = {inbox_level = stored_inbox_level; _}; hash = _} =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_last_stored_commitment ()
  in
  Check.(stored_inbox_level = store_commitment_level)
    Check.int
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  let* {body = commitment_json; _} =
    Sc_rollup_node.RPC.call_json sc_rollup_node
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  Check.(JSON.is_null commitment_json = true)
    Check.bool
    ~error_msg:"No commitment published has been found by the rollup node" ;
  unit

let commitments_messages_reset kind _ sc_rollup_node sc_rollup _node client =
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
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
  in
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
  let* {
         commitment =
           {
             inbox_level = stored_inbox_level;
             number_of_ticks = stored_number_of_ticks;
             _;
           } as stored_commitment;
         hash = _;
       } =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_last_stored_commitment ()
  in
  Check.(stored_inbox_level = init_level + (2 * levels_to_commitment))
    Check.int
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  Log.info "levels_to_commitment: %d" levels_to_commitment ;
  (let expected =
     match kind with
     | "arith" -> 3 * levels_to_commitment
     | "wasm_2_0_0" ->
         4
         (* one snapshot for collecting, two snapshots for SOL,
            Info_per_level and EOL *)
         * max_nb_ticks (* number of ticks in a snapshots *)
         * levels_to_commitment (* number of inboxes *)
     | _ -> failwith "incorrect kind"
   in
   Check.(stored_number_of_ticks = expected)
     Check.int
     ~error_msg:
       "Number of ticks processed by commitment is different from the number \
        of ticks expected (%L = %R)") ;
  let* _level = bake_until_lpc_updated client sc_rollup_node in
  let* published_commitment =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  check_commitment_eq
    (stored_commitment, "stored")
    (published_commitment.commitment_and_hash.commitment, "published") ;
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
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let bootstrap2_key = Constant.bootstrap2.public_key_hash in
  let* client' = Client.init ?endpoint:(Some (Node node)) () in
  let sc_rollup_node' =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client')
      ~kind:(Sc_rollup_node.kind sc_rollup_node)
      ~default_operator:bootstrap2_key
  in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* () = Sc_rollup_node.run sc_rollup_node' sc_rollup [] in
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
  let* () = Sc_rollup_node.run sc_rollup_node' sc_rollup [] in
  let* () = Client.bake_for_and_wait client in
  let* () = Sc_rollup_node.terminate sc_rollup_node' in
  let* () = Client.bake_for_and_wait client in
  let* () = Sc_rollup_node.run sc_rollup_node' sc_rollup [] in
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
  let* {commitment = stored_commitment; hash = _} =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_last_stored_commitment ()
  in
  let* {commitment = stored_commitment'; hash = _} =
    Sc_rollup_node.RPC.call sc_rollup_node'
    @@ Sc_rollup_rpc.get_global_last_stored_commitment ()
  in
  check_commitment_eq
    (stored_commitment, "stored in first node")
    (stored_commitment', "stored in second node") ;
  unit

let commitments_reorgs ~switch_l1_node ~kind _ sc_rollup_node sc_rollup node
    client =
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
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let num_empty_blocks = block_finality_time in
  let num_messages = 1 in
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* node', client' = Client.init_with_node ~nodes_args `Client () in
  let* () = Client.Admin.trust_address client ~peer:node'
  and* () = Client.Admin.trust_address client' ~peer:node in
  let* () = Client.Admin.connect_address client ~peer:node' in

  let* () =
    Sc_rollup_node.run sc_rollup_node ~event_level:`Debug sc_rollup []
  in
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
    unit
  in

  let trigger_reorg () =
    let* () = Client.Admin.connect_address client ~peer:node' in
    let* _ =
      Node.wait_for_level
        node
        (init_level + levels_to_commitment - 1 + num_empty_blocks)
    in
    Log.info "Nodes are synchronized again." ;
    unit
  in

  let* () = divergence () in
  let* client =
    if switch_l1_node then (
      (* Switch the L1 node of a rollup node so that reverted blocks are not *)
      (* available in the new L1 node. *)
      Log.info "Changing L1 node for rollup node" ;
      let* () =
        Sc_rollup_node.change_node_and_restart
          ~event_level:`Debug
          sc_rollup_node
          sc_rollup
          node'
      in
      return client')
    else
      let* () = trigger_reorg () in
      return client
  in
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
  let* {
         commitment =
           {
             inbox_level = stored_inbox_level;
             number_of_ticks = stored_number_of_ticks;
             _;
           } as stored_commitment;
         hash = _;
       } =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_last_stored_commitment ()
  in
  Check.(stored_inbox_level = init_level + levels_to_commitment)
    Check.int
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  let () = Log.info "init_level: %d" init_level in
  (let expected_number_of_ticks =
     match kind with
     | "arith" ->
         1 (* boot sector *) + 1 (* metadata *) + (3 * levels_to_commitment)
         (* input ticks *)
     | "wasm_2_0_0" ->
         (* Number of ticks per snapshot,
            see Lib_scoru_wasm.Constants.wasm_max_tick *)
         let snapshot_ticks = max_nb_ticks in
         snapshot_ticks * 4
         (* 1 snapshot for collecting messages, 3 snapshots for SOL,
            Info_per_level and SOL *)
         * levels_to_commitment
         (* Number of inbox that are actually processed process *)
     | _ -> assert false
   in
   Check.(stored_number_of_ticks = expected_number_of_ticks)
     Check.int
     ~error_msg:
       "Number of ticks processed by commitment is different from the number \
        of ticks expected (%L = %R)") ;
  let* _ = bake_until_lpc_updated client sc_rollup_node in
  let* published_commitment =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  check_commitment_eq
    (stored_commitment, "stored")
    (published_commitment.commitment_and_hash.commitment, "published") ;
  check_published_commitment_in_l1 sc_rollup client published_commitment

(* This test simulate a reorganisation where a block is reproposed, and ensures
   that the correct commitment is published. *)
let commitments_reproposal _protocol sc_rollup_node sc_rollup node1 client1 =
  let* genesis_info =
    Client.RPC.call ~hooks client1
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client1
  in
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* node2, client2 = Client.init_with_node ~nodes_args `Client () in
  let* () = Client.Admin.trust_address client1 ~peer:node2
  and* () = Client.Admin.trust_address client2 ~peer:node1 in
  let* () = Client.Admin.connect_address client1 ~peer:node2 in
  let* () =
    Sc_rollup_node.run sc_rollup_node ~event_level:`Debug sc_rollup []
  in
  (* We bake `sc_rollup_commitment_period_in_blocks - 1` levels, which
     should cause both nodes to observe level
     `sc_rollup_commitment_period_in_blocks + init_level - 1 . *)
  let* () = bake_levels (levels_to_commitment - 1) client1 in
  let* _ = Node.wait_for_level node2 (init_level + levels_to_commitment - 1) in
  let* _ = Sc_rollup_node.wait_sync ~timeout:3. sc_rollup_node in
  Log.info "Nodes are synchronized." ;
  Log.info "Forking." ;
  let* identity2 = Node.wait_for_identity node2 in
  let* () = Client.Admin.kick_peer client1 ~peer:identity2 in
  let* () = send_text_messages client1 ["message1"]
  and* () = send_text_messages client2 ["message2"] in
  let* header1 = Client.RPC.call client1 @@ RPC.get_chain_block_header ()
  and* header2 = Client.RPC.call client2 @@ RPC.get_chain_block_header () in
  let level1 = JSON.(header1 |-> "level" |> as_int) in
  let level2 = JSON.(header2 |-> "level" |> as_int) in
  let hash1 = JSON.(header1 |-> "hash" |> as_string) in
  let hash2 = JSON.(header2 |-> "hash" |> as_string) in
  Check.((level1 = level2) int)
    ~error_msg:"Heads levels should be identical but %L <> %R" ;
  Check.((JSON.encode header1 <> JSON.encode header2) string)
    ~error_msg:"Heads should be distinct: %L and %R" ;
  Log.info "Nodes are following distinct branches." ;
  let* _ = Sc_rollup_node.wait_sync ~timeout:10. sc_rollup_node in
  let check_sc_head hash =
    let* sc_head =
      Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_hash ()
    in
    let sc_head = JSON.as_string sc_head in
    Check.((sc_head = hash) string)
      ~error_msg:"Head of rollup node %L should be one of node %R" ;
    unit
  in
  let* () = check_sc_head hash1 in
  let* state_hash1 =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ~block:hash1 ()
  in
  Log.info "Changing L1 node for rollup node (1st reorg)" ;
  let* () =
    Sc_rollup_node.change_node_and_restart
      ~event_level:`Debug
      sc_rollup_node
      sc_rollup
      node2
  in
  let* _ = Sc_rollup_node.wait_sync ~timeout:10. sc_rollup_node in
  let* () = check_sc_head hash2 in
  let* state_hash2 =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ~block:hash2 ()
  in
  Log.info
    "Changing L1 node for rollup node (2nd reorg), back to first node to \
     simulate reproposal of round 0" ;
  let* () =
    Sc_rollup_node.change_node_and_restart
      ~event_level:`Debug
      sc_rollup_node
      sc_rollup
      node1
  in
  let* _ = Sc_rollup_node.wait_sync ~timeout:10. sc_rollup_node in
  let* () = check_sc_head hash1 in
  Check.((state_hash1 <> state_hash2) string)
    ~error_msg:"States should be distinct" ;
  let state_hash_to_commit = state_hash1 in
  (* exactly one level left to finalize the commitment in the node. *)
  let* () = bake_levels (block_finality_time + 2) client1 in
  let* commitment =
    Client.RPC.call client1
    @@ RPC
       .get_chain_block_context_smart_rollups_smart_rollup_staker_staked_on_commitment
         ~sc_rollup
         Constant.bootstrap1.public_key_hash
  in
  let commitment_state_hash =
    JSON.(commitment |-> "compressed_state" |> as_string)
  in
  Check.((commitment_state_hash = state_hash_to_commit) string)
    ~error_msg:"Safety error: committed state %L instead of state %R" ;
  unit

type balances = {liquid : int; frozen : int}

let contract_balances ~pkh client =
  let* liquid =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_balance ~id:pkh ()
  in
  let* frozen =
    Client.RPC.call client
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
  fun ?expect_failure
      ~sc_rollup
      ~sc_rollup_stake_amount
      ?(check_liquid_balance = true)
      ?(src = Constant.bootstrap1.public_key_hash)
      ?(staker = Constant.bootstrap1.public_key_hash)
      ?(keys = [Constant.bootstrap2.alias])
      client
    ->
    let recover_bond_fee = 1_000_000 in
    let inject_op () =
      Client.Sc_rollup.submit_recover_bond
        ~hooks
        ~rollup:sc_rollup
        ~src
        ~fee:(Tez.of_mutez_int recover_bond_fee)
        ~staker
        client
    in
    match expect_failure with
    | None ->
        let*! () = inject_op () in
        let* old_bal = contract_balances ~pkh:staker client in
        let* () = Client.bake_for_and_wait ~keys client in
        let* new_bal = contract_balances ~pkh:staker client in
        let expected_liq_new_bal =
          old_bal.liquid - recover_bond_fee + sc_rollup_stake_amount
        in
        if check_liquid_balance then
          check_eq_int new_bal.liquid expected_liq_new_bal ;
        check_eq_int new_bal.frozen (old_bal.frozen - sc_rollup_stake_amount) ;
        unit
    | Some failure_string ->
        let*? p = inject_op () in
        Process.check_error ~msg:(rex failure_string) p

(* Test that nodes do not publish commitments before the last cemented commitment. *)
let commitment_before_lcc_not_published protocol sc_rollup_node sc_rollup node
    client =
  let* constants = get_sc_rollup_constants client in
  let commitment_period = constants.commitment_period_in_blocks in
  let challenge_window = constants.challenge_window_in_blocks in
  (* Rollup node 1 processes messages, produces and publishes two commitments. *)
  let* genesis_info =
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
  in
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
  let* _level = bake_until_lpc_updated client sc_rollup_node in
  let* {commitment = _; hash = rollup_node1_stored_hash} =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_last_stored_commitment ()
  in
  let* rollup_node1_published_commitment =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  let () =
    Check.(
      rollup_node1_published_commitment.commitment_and_hash.commitment
        .inbox_level = commitment_inbox_level)
      Check.int
      ~error_msg:
        "Commitment has been published at a level different than expected (%L \
         = %R)"
  in
  (* Cement commitment manually: the commitment can be cemented after
     `challenge_window_levels` have passed since the commitment was published
     (that is at level `commitment_finalized_level`). Note that at this point
     we are already at level `commitment_finalized_level`, hence cementation of
     the commitment can happen. *)
  let levels_to_cementation = challenge_window in
  let cemented_commitment_hash =
    rollup_node1_published_commitment.commitment_and_hash.hash
  in
  let* () = bake_levels levels_to_cementation client in
  let* _ =
    let* current_level = Node.get_level node in
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node current_level
  in

  (* Withdraw stake before cementing should fail *)
  let* () =
    attempt_withdraw_stake
      ~sc_rollup
      ~sc_rollup_stake_amount:(Tez.to_mutez constants.stake_amount)
      client
      ~keys:[]
      ~expect_failure:
        "Attempted to withdraw while not staked on the last cemented \
         commitment."
  in

  let* () =
    cement_commitment protocol client ~sc_rollup ~hash:cemented_commitment_hash
  in
  let* _ =
    let* current_level = Node.get_level node in
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node current_level
  in

  (* Withdraw stake after cementing should succeed *)
  let* () =
    attempt_withdraw_stake
      ~sc_rollup
      ~sc_rollup_stake_amount:(Tez.to_mutez constants.stake_amount)
      ~keys:[]
      ~check_liquid_balance:false
      client
  in

  let* () = Sc_rollup_node.terminate sc_rollup_node in
  (* Rollup node 2 starts and processes enough levels to publish a commitment.*)
  let bootstrap2_key = Constant.bootstrap2.public_key_hash in
  let* client' = Client.init ?endpoint:(Some (Node node)) () in
  let sc_rollup_node' =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client')
      ~kind:(Sc_rollup_node.kind sc_rollup_node)
      ~default_operator:bootstrap2_key
  in
  let* () = Sc_rollup_node.run sc_rollup_node' sc_rollup [] in

  let* _ = wait_for_current_level node ~timeout:3. sc_rollup_node' in
  (* Check that no commitment was published. *)
  let* {body = rollup_node2_last_published_commitment; _} =
    Sc_rollup_node.RPC.call_json sc_rollup_node'
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  let () =
    Check.(JSON.is_null rollup_node2_last_published_commitment = true)
      Check.bool
      ~error_msg:"Commitment has been published by node 2 at %L but shouldn't"
  in
  (* Check that the commitment stored by the second rollup node
     is the same commmitment stored by the first rollup node. *)
  let* {commitment = _; hash = rollup_node2_stored_hash} =
    Sc_rollup_node.RPC.call sc_rollup_node'
    @@ Sc_rollup_rpc.get_global_last_stored_commitment ()
  in
  let () =
    Check.(rollup_node1_stored_hash = rollup_node2_stored_hash)
      Check.string
      ~error_msg:
        "Commitments stored by first (%L) and second (%R) rollup nodes differ"
  in

  (* Bake other commitment_period levels and check that rollup_node2 is
     able to publish a commitment (bake one extra to see commitment in block). *)
  let* () = bake_levels (commitment_period + 1) client' in
  let commitment_inbox_level = commitment_inbox_level + commitment_period in
  let* _ = wait_for_current_level node ~timeout:3. sc_rollup_node' in
  let* rollup_node2_last_published_commitment =
    Sc_rollup_node.RPC.call sc_rollup_node'
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  let rollup_node2_last_published_commitment_inbox_level =
    rollup_node2_last_published_commitment.commitment_and_hash.commitment
      .inbox_level
  in
  let () =
    Check.(
      rollup_node2_last_published_commitment_inbox_level
      = commitment_inbox_level)
      Check.int
      ~error_msg:
        "Commitment has been published at a level different than expected (%L \
         = %R)"
  in
  let () =
    Check.(
      rollup_node2_last_published_commitment.commitment_and_hash.commitment
        .predecessor = cemented_commitment_hash)
      Check.string
      ~error_msg:
        "Predecessor fo commitment published by rollup_node2 should be the \
         cemented commitment (%L = %R)"
  in
  unit

(* Test that the level when a commitment was first published is fetched correctly
   by rollup nodes. *)
let first_published_level_is_global _protocol sc_rollup_node sc_rollup node
    client =
  (* Rollup node 1 processes messages, produces and publishes two commitments. *)
  let* genesis_info =
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* commitment_period = get_sc_rollup_commitment_period_in_blocks client in
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
  in
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
  let* commitment_publish_level =
    bake_until_lpc_updated client sc_rollup_node
  in
  let* rollup_node1_published_commitment =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  Check.(
    rollup_node1_published_commitment.commitment_and_hash.commitment.inbox_level
    = commitment_inbox_level)
    Check.int
    ~error_msg:
      "Commitment has been published for a level %L different than expected %R" ;
  Check.(
    rollup_node1_published_commitment.first_published_at_level
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
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client')
      ~kind:(Sc_rollup_node.kind sc_rollup_node)
      ~default_operator:bootstrap2_key
  in
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node' sc_rollup []
  in

  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node'
      commitment_publish_level
  in
  let* {body = rollup_node2_published_commitment; _} =
    Sc_rollup_node.RPC.call_json sc_rollup_node'
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  Check.(JSON.is_null rollup_node2_published_commitment = true)
    Check.bool
    ~error_msg:"Rollup node 2 cannot publish commitment without any new block." ;
  let* _level = bake_until_lpc_updated client sc_rollup_node' in
  let* rollup_node2_published_commitment =
    Sc_rollup_node.RPC.call sc_rollup_node'
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  check_commitment_eq
    ( rollup_node1_published_commitment.commitment_and_hash.commitment,
      "published by rollup node 1" )
    ( rollup_node2_published_commitment.commitment_and_hash.commitment,
      "published by rollup node 2" ) ;
  let () =
    Check.(
      rollup_node1_published_commitment.first_published_at_level
      = rollup_node2_published_commitment.first_published_at_level)
      (Check.option Check.int)
      ~error_msg:
        "Rollup nodes do not agree on level when commitment was first \
         published (%L = %R)"
  in
  unit

(* Check that the SC rollup is correctly originated with a boot sector.
   -------------------------------------------------------

   Originate a rollup with a custom boot sector and check if the rollup's
   genesis state hash is correct.
*)
let test_rollup_origination_boot_sector ~boot_sector ~kind =
  test_full_scenario
    ~boot_sector
    ~kind
    {
      variant = None;
      tags = ["boot_sector"];
      description = "boot_sector is correctly set";
    }
  @@ fun _protocol rollup_node sc_rollup _tezos_node tezos_client ->
  let* genesis_info =
    Client.RPC.call ~hooks tezos_client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let genesis_commitment_hash =
    JSON.(genesis_info |-> "commitment_hash" |> as_string)
  in
  let* init_commitment =
    Client.RPC.call ~hooks tezos_client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_commitment
         ~sc_rollup
         ~hash:genesis_commitment_hash
         ()
  in
  let init_hash = init_commitment.compressed_state in
  let* () = Sc_rollup_node.run rollup_node sc_rollup [] in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:3. rollup_node init_level in
  let* node_state_hash =
    Sc_rollup_node.RPC.call ~rpc_hooks rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ()
  in
  Check.(
    (init_hash = node_state_hash)
      string
      ~error_msg:"State hashes should be equal! (%L, %R)") ;
  unit

(** Check that a node makes use of the boot sector.
    -------------------------------------------------------

    Originate 2 rollups with different boot sectors to check if they have
    different hash.
*)
let test_boot_sector_is_evaluated ~boot_sector1 ~boot_sector2 ~kind =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4161

     The hash is calculated by the client and given as a proof to the L1. This test
     should be rewritten in a unit test maybe ?
  *)
  test_l1_scenario
    ~regression:true
    ~hooks
    ~boot_sector:boot_sector1
    ~kind
    {
      variant = None;
      tags = ["boot_sector"];
      description = "boot sector is evaluated";
    }
  @@ fun _protocol sc_rollup1 _tezos_node tezos_client ->
  let* sc_rollup2 =
    originate_sc_rollup
      ~alias:"rollup2"
      ~hooks
      ~kind
      ~boot_sector:boot_sector2
      ~src:Constant.bootstrap2.alias
      tezos_client
  in
  let genesis_state_hash ~sc_rollup tezos_client =
    let* genesis_info =
      Client.RPC.call ~hooks tezos_client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
           sc_rollup
    in
    let commitment_hash =
      JSON.(genesis_info |-> "commitment_hash" |> as_string)
    in
    let* commitment =
      Client.RPC.call ~hooks tezos_client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_commitment
           ~sc_rollup
           ~hash:commitment_hash
           ()
    in
    let state_hash = commitment.compressed_state in
    return state_hash
  in

  let* state_hash_1 = genesis_state_hash ~sc_rollup:sc_rollup1 tezos_client in
  let* state_hash_2 = genesis_state_hash ~sc_rollup:sc_rollup2 tezos_client in
  Check.(
    (state_hash_1 <> state_hash_2)
      string
      ~error_msg:"State hashes should be different! (%L, %R)") ;
  unit

let test_reveals_fails_on_wrong_hash =
  let kind = "arith" in
  test_full_scenario
    ~timeout:120
    ~kind
    {
      tags = ["reveals"; "wrong"];
      variant = None;
      description = "reveal data fails with wrong hash";
    }
  @@ fun protocol sc_rollup_node sc_rollup node client ->
  let hash = reveal_hash ~protocol ~kind "Some data" in
  let pvm_dir = Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) kind in
  let filename = Filename.concat pvm_dir hash.filename in
  let () = Sys.mkdir pvm_dir 0o700 in
  let cout = open_out filename in
  let () = output_string cout "Some data that is not related to the hash" in
  let () = close_out cout in

  let error_promise =
    Sc_rollup_node.wait_for
      sc_rollup_node
      "smart_rollup_node_daemon_error.v0"
      (fun e ->
        let id = JSON.(e |=> 0 |-> "id" |> as_string) in
        if id =~ rex "wrong_hash_of_reveal_preimage" then Some () else None)
  in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* () = send_text_messages client [hash.message] in
  let should_not_sync =
    let* _level = wait_for_current_level node ~timeout:10. sc_rollup_node in
    Test.fail "The rollup node processed the incorrect reveal without failing"
  in
  Lwt.choose [error_promise; should_not_sync]

let test_reveals_fails_on_unknown_hash =
  let kind = "arith" in
  let commitment_period = 10 in
  test_full_scenario
    ~supports:(Protocol.From_protocol 18)
    ~timeout:120
    ~commitment_period
    ~kind
    ~allow_degraded:true
    {
      tags = ["reveals"; "unknown"];
      variant = None;
      description = "reveal data fails with unknown hash";
    }
  @@ fun protocol sc_rollup_node sc_rollup node client ->
  let unknown_hash = reveal_hash ~protocol ~kind "Some data" in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let error_promise =
    Sc_rollup_node.wait_for
      sc_rollup_node
      "smart_rollup_node_daemon_error.v0"
      (fun e ->
        let id = JSON.(e |=> 0 |-> "id" |> as_string) in
        if id =~ rex "could_not_open_reveal_preimage_file" then Some ()
        else None)
  in
  (* We need to check that the rollup has entered the degraded mode,
      so we wait for 60 blocks (commitment period) + 2. *)
  let* () =
    repeat (commitment_period + 2) (fun () -> Client.bake_for_and_wait client)
  in
  (* Then, we finally send the message with the unknown hash. *)
  let* () = send_text_messages client [unknown_hash.message] in
  let should_not_sync =
    let* _level = wait_for_current_level node ~timeout:10. sc_rollup_node in
    Test.fail "The rollup node processed the unknown reveal without failing"
  in
  let* () = Lwt.choose [error_promise; should_not_sync] in
  let* () = repeat 5 (fun () -> Client.bake_for_and_wait client) in
  Log.info "Adding missing pre-image." ;
  let pvm_dir = Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) kind in
  let filename = Filename.concat pvm_dir unknown_hash.filename in
  let () = Sys.mkdir pvm_dir 0o700 in
  let cout = open_out filename in
  let () = output_string cout "Some data" in
  let () = close_out cout in
  let* () = repeat 5 (fun () -> Client.bake_for_and_wait client) in
  let* _level = wait_for_current_level node ~timeout:10. sc_rollup_node in
  unit

let test_reveals_4k =
  let kind = "arith" in
  test_full_scenario
    ~timeout:120
    ~kind
    {
      tags = ["reveals"; "4k"];
      variant = None;
      description = "reveal 4kB of data";
    }
  @@ fun protocol sc_rollup_node sc_rollup node client ->
  let data = String.make 4096 'z' in
  let hash = reveal_hash ~protocol ~kind data in
  let pvm_dir = Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) kind in
  let filename = Filename.concat pvm_dir hash.filename in
  let () = Sys.mkdir pvm_dir 0o700 in
  let () = with_open_out filename @@ fun cout -> output_string cout data in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let failure =
    let* () =
      Sc_rollup_node.process sc_rollup_node |> Option.get |> Process.check
    in
    Test.fail "Node terminated before reveal"
  in
  let* () = send_text_messages client [hash.message] in
  let sync =
    let* _level = wait_for_current_level node ~timeout:10. sc_rollup_node in
    unit
  in
  Lwt.choose [sync; failure]

let test_reveals_fetch_remote =
  let kind = "arith" in
  test_full_scenario
    ~timeout:120
    ~kind
    {
      tags = ["reveals"; "fetch"];
      variant = None;
      description = "reveal from remote service";
    }
  @@ fun protocol sc_rollup_node sc_rollup node client ->
  let data = String.make 1024 '\202' in
  let hash = reveal_hash ~protocol ~kind data in
  let pre_images_dir = Temp.dir "preimages_remote_dir" in
  let filename = Filename.concat pre_images_dir hash.filename in
  let () = with_open_out filename @@ fun cout -> output_string cout data in
  let provider_port = Port.fresh () in
  Log.info "Starting webserver for static pre-images content." ;
  let fetched = ref false in
  serve_files
    ~name:"pre_image_provider"
    ~port:provider_port
    ~root:pre_images_dir
    ~on_request:(fun _ -> fetched := true)
  @@ fun () ->
  let pre_images_endpoint =
    sf "http://%s:%d" Constant.default_host provider_port
  in
  Log.info "Run rollup node." ;
  let* () =
    Sc_rollup_node.run
      sc_rollup_node
      sc_rollup
      [Pre_images_endpoint pre_images_endpoint]
  in
  let failure =
    let* () =
      Sc_rollup_node.process sc_rollup_node |> Option.get |> Process.check
    in
    Test.fail "Node terminated before reveal"
  in
  Log.info "Send reveal message %s." hash.message ;
  let* () = send_text_messages client [hash.message] in
  let sync =
    let* _level = wait_for_current_level node ~timeout:10. sc_rollup_node in
    if not !fetched then
      Test.fail ~__LOC__ "Pre-image was not fetched from remote server" ;
    unit
  in
  Lwt.choose [sync; failure]

let test_reveals_above_4k =
  let kind = "arith" in
  test_full_scenario
    ~timeout:120
    ~kind
    {
      tags = ["reveals"; "4k"];
      variant = None;
      description = "reveal more than 4kB of data";
    }
  @@ fun protocol sc_rollup_node sc_rollup node client ->
  let data = String.make 4097 'z' in
  let hash = reveal_hash ~protocol ~kind data in
  let pvm_dir = Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) kind in
  let filename = Filename.concat pvm_dir hash.filename in
  let () = Sys.mkdir pvm_dir 0o700 in
  let cout = open_out filename in
  let () = output_string cout data in
  let () = close_out cout in
  let error_promise =
    Sc_rollup_node.wait_for
      sc_rollup_node
      "smart_rollup_node_daemon_error.v0"
      (fun e ->
        let id = JSON.(e |=> 0 |-> "id" |> as_string) in
        if id =~ rex "could_not_encode_raw_data" then Some () else None)
  in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* () = send_text_messages client [hash.message] in
  let should_not_sync =
    let* _level = wait_for_current_level node ~timeout:10. sc_rollup_node in
    Test.fail "The rollup node processed the incorrect reveal without failing"
  in
  Lwt.choose [error_promise; should_not_sync]

let test_consecutive_commitments _protocol _rollup_node sc_rollup _tezos_node
    tezos_client =
  let* inbox_level = Client.level tezos_client in
  let operator = Constant.bootstrap1.public_key_hash in
  let* {commitment_period_in_blocks; _} =
    get_sc_rollup_constants tezos_client
  in
  (* As we did no publish any commitment yet, this is supposed to fail. *)
  let*? process =
    Client.RPC.spawn tezos_client
    @@ RPC
       .get_chain_block_context_smart_rollups_smart_rollup_staker_staked_on_commitment
         ~sc_rollup
         operator
  in
  let* () =
    Process.check_error
      ~msg:(rex "This implicit account is not a staker of this smart rollup")
      process
  in
  let* _commit, commit_hash =
    bake_period_then_publish_commitment ~sc_rollup ~src:operator tezos_client
  in
  let* () =
    repeat (commitment_period_in_blocks + 2) (fun () ->
        Client.bake_for_and_wait tezos_client)
  in
  let* _commit, _commit_hash =
    forge_and_publish_commitment
      ~inbox_level:(inbox_level + (2 * commitment_period_in_blocks))
      ~predecessor:commit_hash
      ~sc_rollup
      ~src:operator
      tezos_client
  in
  unit

let test_can_stake ~kind =
  test_l1_scenario
    ~kind
    ~uses:(fun _ -> [Constant.octez_smart_rollup_node])
    {
      tags = ["commitment"; "stake"];
      variant = None;
      description = "Rollup node checks operator can stake";
    }
  @@ fun _protocol sc_rollup tezos_node tezos_client ->
  let* operator = Client.gen_and_show_keys ~alias:"operator" tezos_client in
  let rollup_node =
    Sc_rollup_node.create
      ~default_operator:operator.alias
      Operator
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~kind
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 5000)
      ~giver:Constant.bootstrap1.alias
      ~receiver:operator.alias
      tezos_client
  in
  let* () = Client.bake_for_and_wait tezos_client in
  let* () = Sc_rollup_node.run ~wait_ready:false rollup_node sc_rollup []
  and* () =
    Lwt.choose
      [
        (let* () = Lwt_unix.sleep 10. in
         Test.fail "Rollup node did not exit");
        Sc_rollup_node.check_error
          ~exit_code:1
          ~msg:(rex "needs more than 10000")
          rollup_node;
      ]
  in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 6000)
      ~burn_cap:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~receiver:operator.alias
      tezos_client
  in
  let* () = Client.bake_for_and_wait tezos_client in
  let* () = Sc_rollup_node.run rollup_node sc_rollup [] in
  let* _ = Sc_rollup_node.wait_sync rollup_node ~timeout:20. in
  unit

let test_refutation_scenario ?regression ?commitment_period ?challenge_window
    ~variant ~mode ~kind ?(ci_disabled = false) ?uses ?(timeout = 60) ?timestamp
    ?boot_sector ?(extra_tags = []) ?with_dal ?dal_attested_slots_validity_lag
    ({allow_degraded; _} as scenario) =
  let regression =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5313
       Disabled dissection regressions for parallel games, as it introduces
       flakyness. *)
    if Option.is_some regression then regression
    else Some (List.compare_length_with scenario.loser_modes 1 <= 0)
  in
  let tags =
    ["refutation"] @ if mode = Sc_rollup_node.Accuser then ["accuser"] else []
  in
  let tags = if ci_disabled then Tag.ci_disabled :: tags else tags in
  let variant = variant ^ if mode = Accuser then "+accuser" else "" in
  test_full_scenario
    ?regression
    ?hooks:None (* We only want to capture dissections manually *)
    ?commitment_period
    ~kind
    ?uses
    ~mode
    ~timeout
    ?timestamp
    ?boot_sector
    ?challenge_window
    ~rollup_node_name:"honest"
    ~allow_degraded
    ?dal_attested_slots_validity_lag
    {
      tags = tags @ extra_tags;
      variant = Some variant;
      description = "refutation games winning strategies";
    }
    (test_refutation_scenario_aux ?with_dal ~mode ~kind scenario)

let test_refutation protocols ~kind =
  let challenge_window = 10 in
  let commitment_period = 10 in
  let tests =
    match kind with
    | "arith" ->
        [
          (* As a reminder, the format of loser modes is a space-separated
             list of space-separated triples of integers. The first integer
             is the inbox level of the failure, the second integer is the
             message index of the failure, and the third integer is the
             index of the failing tick during the processing of this
             message. *)
          ( "inbox_proof_at_genesis",
            refutation_scenario_parameters
              ~loser_modes:["3 0 0"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_honest );
          ( "pvm_proof_at_genesis",
            refutation_scenario_parameters
              ~loser_modes:["3 0 1"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_honest );
          ( "inbox_proof",
            refutation_scenario_parameters
              ~loser_modes:["5 0 0"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_honest );
          ( "inbox_proof_with_new_content",
            refutation_scenario_parameters
              ~loser_modes:["5 0 0"]
              (inputs_for 30)
              ~final_level:80
              ~priority:`Priority_honest );
          (* In "inbox_proof_with_new_content" we add messages after the commitment
             period so the current inbox is not equal to the inbox snapshot-ted at the
             game creation. *)
          ( "inbox_proof_one_empty_level",
            refutation_scenario_parameters
              ~loser_modes:["6 0 0"]
              (inputs_for 10)
              ~final_level:80
              ~empty_levels:[2]
              ~priority:`Priority_honest );
          ( "inbox_proof_many_empty_levels",
            refutation_scenario_parameters
              ~loser_modes:["9 0 0"]
              (inputs_for 10)
              ~final_level:80
              ~empty_levels:[2; 3; 4]
              ~priority:`Priority_honest );
          ( "pvm_proof_0",
            refutation_scenario_parameters
              ~loser_modes:["5 2 1"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_honest );
          ( "pvm_proof_1",
            refutation_scenario_parameters
              ~loser_modes:["7 2 0"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_honest );
          ( "pvm_proof_2",
            refutation_scenario_parameters
              ~loser_modes:["7 2 5"]
              (inputs_for 7)
              ~final_level:80
              ~priority:`Priority_honest );
          ( "pvm_proof_3",
            refutation_scenario_parameters
              ~loser_modes:["9 2 5"]
              (inputs_for 7)
              ~final_level:80
              ~empty_levels:[4; 5]
              ~priority:`Priority_honest );
          ( "pvm_proof_second_period",
            refutation_scenario_parameters
              ~loser_modes:["15 2 5"]
              (inputs_for 18)
              ~final_level:80
              ~priority:`Priority_honest );
          ( "timeout",
            refutation_scenario_parameters
              ~loser_modes:["5 2 1"]
              (inputs_for 10)
              ~final_level:80
              ~stop_loser_at:[35]
              ~priority:`Priority_honest );
          ( "reset_honest_during_game",
            refutation_scenario_parameters
              ~loser_modes:["5 2 1"]
              (inputs_for 10)
              ~final_level:80
              ~reset_honest_on:
                [("smart_rollup_node_conflict_detected.v0", 3, None)]
              ~priority:`Priority_honest );
          ( "degraded_new",
            refutation_scenario_parameters
              ~loser_modes:["7 2 5"]
              (inputs_for 7)
              ~final_level:80
              ~bad_reveal_at:
                [
                  14
                  (* Commitment for inbox 7 will be made at level 12 and published
                     at level 14 *);
                ]
              ~allow_degraded:true
              ~priority:`Priority_honest );
          ( "degraded_ongoing",
            refutation_scenario_parameters
              ~loser_modes:["7 2 5"]
              (inputs_for 7)
              ~final_level:80
              ~bad_reveal_at:[21]
              ~allow_degraded:true
              ~priority:`Priority_honest );
          ( "parallel_games_0",
            refutation_scenario_parameters
              ~loser_modes:["3 0 0"; "3 0 1"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_honest );
          ( "parallel_games_1",
            refutation_scenario_parameters
              ~loser_modes:["3 0 0"; "3 0 1"; "3 0 0"]
              (inputs_for 10)
              ~final_level:200
              ~priority:`Priority_honest );
        ]
    | "wasm_2_0_0" ->
        [
          (* First message of an inbox (level 3) *)
          ( "inbox_proof_0",
            refutation_scenario_parameters
              ~loser_modes:["3 0 0"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_loser );
          (* Fourth message of an inbox (level 3) *)
          ( "inbox_proof_1",
            refutation_scenario_parameters
              ~loser_modes:["3 4 0"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_loser );
          (* Echo kernel takes around 2,100 ticks to execute *)
          (* Second tick of decoding *)
          ( "pvm_proof_0",
            refutation_scenario_parameters
              ~loser_modes:["5 7 11_000_000_001"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_loser );
          ( "pvm_proof_1",
            refutation_scenario_parameters
              ~loser_modes:["7 7 11_000_001_000"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_loser );
          (* End of evaluation *)
          ( "pvm_proof_2",
            refutation_scenario_parameters
              ~loser_modes:["7 7 22_000_002_000"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_loser );
          (* During padding *)
          ( "pvm_proof_3",
            refutation_scenario_parameters
              ~loser_modes:["7 7 22_010_000_000"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_loser );
          (* Conflict in second commitment period *)
          ( "pvm_proof_second_period",
            refutation_scenario_parameters
              ~loser_modes:["15 7 11_000_001_000"]
              (inputs_for 16)
              ~final_level:80
              ~priority:`Priority_loser );
          ( "parallel_games_0",
            refutation_scenario_parameters
              ~loser_modes:["4 0 0"; "5 7 11_000_000_001"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_loser );
          ( "parallel_games_1",
            refutation_scenario_parameters
              ~loser_modes:["4 0 0"; "7 7 22_000_002_000"; "7 7 22_000_002_000"]
              (inputs_for 10)
              ~final_level:80
              ~priority:`Priority_loser );
        ]
    | _ -> assert false
  in
  List.iter
    (fun (variant, inputs) ->
      test_refutation_scenario
        ~kind
        ~mode:Operator
        ~challenge_window
        ~commitment_period
        ~variant
        inputs
        protocols)
    tests

let test_invalid_dal_parameters protocols =
  test_refutation_scenario
    ~uses:(fun _protocol ->
      [
        Constant.WASM.echo_dal_reveal_parameters; Constant.smart_rollup_installer;
      ])
    ~kind:"wasm_2_0_0"
    ~mode:Operator
    ~challenge_window:10
    ~timeout:160
    ~commitment_period:10
    ~variant:"invalid_dal_parameters"
    ~boot_sector:
      (read_kernel
         ~base:""
         ~suffix:""
         (Uses.path Constant.WASM.echo_dal_reveal_parameters))
    (refutation_scenario_parameters
       ~loser_modes:["reveal_dal_parameters 6 6 6 6"]
       (inputs_for 10)
       ~final_level:160
       ~priority:`Priority_honest)
    protocols

(* kernel_imported_publish_level is the imported published level hardcoded in
   the [Constant.WASM.echo_dal_reveal_pages] kernel. *)
let with_dal_ready_for_echo_dal_reveal_pages ~operator_profiles
    ~kernel_imported_publish_level ~may_publish_and_attest_slot
    ~expected_attestation_lag =
 fun protocol tezos_node tezos_client ->
  (* We create and start a DAL node with the given producer profile. *)
  let dal_node = Dal_node.create ~node:tezos_node () in
  let* () = Dal_node.init_config ~operator_profiles dal_node in
  let* () = Dal_node.run dal_node ~wait_ready:true in
  let* () =
    match may_publish_and_attest_slot with
    | None ->
        (* No slot to publish and atest *)
        unit
    | Some (published_level, slot_index) ->
        (* We want to publish a slot at the given level and index, and then attest it. *)
        let* curr_level = Node.get_level tezos_node in
        if curr_level > published_level then
          Test.fail
            "publish level (%d) smaller than current level (%d)@."
            published_level
            curr_level ;
        (* Advance the L1 chain until [published_level - 1]. *)
        let* () =
          Client.bake_for ~count:(published_level - curr_level - 1) tezos_client
        in
        let* _lvl = Node.wait_for_level tezos_node (published_level - 1) in
        let* dal_parameters =
          Dal_common.Parameters.from_client protocol tezos_client
        in
        let attestation_lag = dal_parameters.attestation_lag in
        let slot_size = dal_parameters.cryptobox.slot_size in
        if expected_attestation_lag <> attestation_lag then
          Test.fail
            "Expected attestation_lag %d, got %d@."
            expected_attestation_lag
            attestation_lag ;
        Dal.publish_store_and_attest_slot
          ~protocol
          tezos_client
          tezos_node
          dal_node
          Constant.bootstrap1
          ~index:slot_index
          ~content:
            (Dal_common.Helpers.make_slot
               ~slot_size
               (String.make slot_size 'T'))
          dal_parameters
  in
  (* Whether we published and attested a slot or not, we advance the L1 chain
     until [kernel_imported_publish_level + expected_attestation_lag] is
     finalized to ensure that the DAL node can answer the rollup's requests
     about the slot 1 published at level [kernel_imported_publish_level]. *)
  let target_final_l1_level =
    kernel_imported_publish_level + expected_attestation_lag
  in
  let* curr_level = Node.get_level tezos_node in
  let block_finality = 2 in
  let num_blocks_to_bake =
    target_final_l1_level + block_finality - curr_level
  in
  let* () =
    if num_blocks_to_bake <= 0 then unit
    else
      let wait_for_target_final_l1_level =
        Dal_node.wait_for dal_node "dal_new_L1_final_block.v0" (fun e ->
            if JSON.(e |-> "level" |> as_int) = target_final_l1_level then
              Some ()
            else None)
      in
      let* () =
        (* [Client.bake_for ~count] doesn't work here / is flaky. *)
        repeat num_blocks_to_bake (fun () -> Client.bake_for tezos_client)
      in
      wait_for_target_final_l1_level
  in
  (* Ensure that in case we wanted an attested slot, it's indeed attested
     (otherwise, we'd not test the desired configuration). *)
  let* () =
    match may_publish_and_attest_slot with
    | None -> unit
    | Some (published_level, slot_index) ->
        let open Dal_common.RPC in
        let open Dal_common.RPC.Local in
        let* slot_status =
          call dal_node
          @@ get_level_slot_status ~slot_level:published_level ~slot_index
        in
        Check.(slot_status = Attested expected_attestation_lag)
          ~__LOC__
          Dal_common.Check.slot_id_status_typ
          ~error_msg:
            "The value of the fetched status should match the expected one \
             (current = %L, expected = %R)" ;
        Log.info
          "Slot published at level %d and index %d is attested as expected@."
          published_level
          slot_index ;
        unit
  in
  Lwt.return_some dal_node

type case = {
  inbox_level : int;
  player_priority : [`Priority_loser | `Priority_honest];
  attestation_status : [`Attested | `Unattested];
      (* TODO: We might want to distinguish "Published | Unpublished" for the
         Unattested case in the future. *)
}

let player_priority_to_string = function
  | `Priority_loser -> "Priority_loser"
  | `Priority_honest -> "Priority_honest"

let attestation_status_to_string = function
  | `Attested -> "Attested"
  | `Unattested -> "Unattested"

let test_refutation_with_dal_page_import protocols =
  (* This is the published level for which the toy kernel
     [Constant.WASM.echo_dal_reveal_pages] asks to import page 2 of slot 1. *)
  let published_level = 15 in
  (* This is the test's attestation lag (checked in the scenario). *)
  let dal_lag = 5 in
  (* This is the slot index at which we'll possibly publish a slot. *)
  let slot_index = 1 in
  (* Once a slot is attested, its import is valid for this number of
     blocks. After that, any import is considered invalid. *)
  let dal_ttl = 50 in

  (* First dimension of the tests: we vary inbox_level at which the divergence
     between the honest and loser rollups will happen. This stress-tests various
     corner cases regarding the level at which the import request is sent
     w.r.t. published level.  *)
  let dimension_inbox_level =
    [
      (* Test import around published_level. Nothing should be imported *)
      published_level - 1;
      published_level + 0;
      published_level + 1;
      (* Test import around attested_level. Nothing should be imported for the
         first level below. For the two others, the page should be imported if
         the slot is attested. *)
      published_level + dal_lag - 1;
      published_level + dal_lag + 0;
      published_level + dal_lag + 1;
      (* Test import attested_level + TLL. The slot's page should not be
         imported for the third case below, even if it is attested, because its
         TTL expired.  *)
      published_level + dal_ttl + dal_lag - 1;
      published_level + dal_ttl + dal_lag + 0;
      published_level + dal_ttl + dal_lag + 1;
    ]
  in

  (* Second dimension of the tests: Which player will start the game. This will
     have an impact on which one will provide the final proof. *)
  let dimension_player_priority = [`Priority_loser; `Priority_honest] in

  (* Third dimension of the tests: We will test both with attested an unattested
     slots. *)
  let dimension_attested = [`Attested; `Unattested] in

  (* The list of tests, as a cartesian product of 3 dimensions above. *)
  let all_cases =
    List.fold_left
      (fun accu inbox_level ->
        List.fold_left
          (fun accu player_priority ->
            List.fold_left
              (fun accu attestation_status ->
                {inbox_level; player_priority; attestation_status} :: accu)
              accu
              dimension_attested)
          accu
          dimension_player_priority)
      []
      dimension_inbox_level
  in

  List.iter
    (fun {inbox_level; attestation_status; player_priority} ->
      (* One test name for each dimensions combination. *)
      let variant =
        Format.sprintf
          "dal_page_published_at_level_%d_flipped_at_inbox_level_%d_dal_lag_is_%d_%s_%s"
          published_level
          inbox_level
          dal_lag
          (player_priority_to_string player_priority)
          (attestation_status_to_string attestation_status)
      in
      (* The behaviour of the loser mode is parameterized by the inbox level at
         which the payload of imported page is flipped. *)
      let loser_modes =
        (* See src/lib_smart_rollup_node/loser_mode.mli for the semantics of this
            loser mode. *)
        [
          Format.sprintf
            "reveal_dal_page published_level:15 slot_index:1 page_index:2 \
             strategy:flip inbox_level:%d"
            inbox_level;
        ]
      in
      let may_publish_and_attest_slot =
        if attestation_status = `Unattested then None
        else Some (published_level, slot_index)
      in
      let with_dal =
        with_dal_ready_for_echo_dal_reveal_pages
          ~operator_profiles:[slot_index]
          ~kernel_imported_publish_level:published_level
          ~expected_attestation_lag:dal_lag
          ~may_publish_and_attest_slot
      in
      let priority =
        (player_priority :> [`No_priority | `Priority_honest | `Priority_loser])
      in
      test_refutation_scenario
        ~uses:(fun _protocol ->
          [Constant.WASM.echo_dal_reveal_pages; Constant.octez_dal_node])
        ~kind:"wasm_2_0_0"
        ~mode:Operator
        ~challenge_window:150
        ~timeout:120
        ~commitment_period:10
        ~dal_attested_slots_validity_lag:dal_ttl
        ~variant
        ~boot_sector:
          (read_kernel
             ~base:""
             ~suffix:""
             (Uses.path Constant.WASM.echo_dal_reveal_pages))
        (refutation_scenario_parameters
           ~loser_modes
           (inputs_for 10)
           ~final_level:100
           ~priority)
        protocols
        ~regression:false
        ~with_dal)
    all_cases

let test_refutation_with_dal_page_import_id_far_in_the_future protocols =
  (* This is the published level for which the toy kernel
     [Constant.WASM.echo_dal_reveal_pages_high_target_pub_level] asks to import
     page 2 of slot 1. *)
  let published_level = 3000 in
  (* This is the test's attestation lag (checked in the scenario). *)
  let dal_lag = 5 in
  (* This is the slot index at which we'll possibly publish a slot. *)
  let slot_index = 1 in
  (* Once a slot is attested, its import is valid for this number of
     blocks. After that, any import is considered invalid. *)
  let dal_ttl = 50 in

  (* This inbox level is too small w.r.t. to target published level to import.
     The rollup node should be able to progress even without knowing the status
     of that slot whose id is far in the future.   *)
  let inbox_level = 5 in

  (* Which player will start the game. This will have an impact on which one
     will provide the final proof. *)
  let dimension_player_priority = [`Priority_loser; `Priority_honest] in

  (* The list of tests, as a cartesian product of 3 dimensions above. *)
  let all_cases =
    List.fold_left
      (fun accu player_priority ->
        {inbox_level; player_priority; attestation_status = `Unattested} :: accu)
      []
      dimension_player_priority
  in

  List.iter
    (fun {inbox_level; attestation_status; player_priority} ->
      (* One test name for each dimensions combination. *)
      let variant =
        Format.sprintf
          "dal_page_with_id_far_in_the_future_flipped_at_inbox_level_%d_%s_%s"
          inbox_level
          (player_priority_to_string player_priority)
          (attestation_status_to_string attestation_status)
      in
      (* The behaviour of the loser mode is parameterized by the inbox level at
         which the payload of imported page is flipped. *)
      let loser_modes =
        (* See src/lib_smart_rollup_node/loser_mode.mli for the semantics of this
            loser mode. *)
        [
          Format.sprintf
            "reveal_dal_page published_level:%d slot_index:1 page_index:2 \
             strategy:flip inbox_level:%d"
            published_level
            inbox_level;
        ]
      in
      let may_publish_and_attest_slot =
        if attestation_status = `Unattested then None
        else Some (published_level, slot_index)
      in
      let with_dal =
        with_dal_ready_for_echo_dal_reveal_pages
          ~operator_profiles:[slot_index]
          ~kernel_imported_publish_level:inbox_level
            (* We'll not progress until published_level on purpose. *)
          ~expected_attestation_lag:dal_lag
          ~may_publish_and_attest_slot
      in
      let priority =
        (player_priority :> [`No_priority | `Priority_honest | `Priority_loser])
      in
      test_refutation_scenario
        ~uses:(fun _protocol ->
          [Constant.WASM.echo_dal_reveal_pages; Constant.octez_dal_node])
        ~kind:"wasm_2_0_0"
        ~mode:Operator
        ~challenge_window:150
        ~timeout:120
        ~commitment_period:10
        ~dal_attested_slots_validity_lag:dal_ttl
        ~variant
        ~boot_sector:
          (read_kernel
             ~base:""
             ~suffix:""
             (Uses.path
                Constant.WASM.echo_dal_reveal_pages_high_target_pub_level))
        (refutation_scenario_parameters
           ~loser_modes
           (inputs_for 10)
           ~final_level:100
           ~priority)
        protocols
        ~regression:false
        ~with_dal)
    all_cases

(** Run one of the refutation tests with an accuser instead of a full operator. *)
let test_accuser protocols =
  test_refutation_scenario
    ~kind:"wasm_2_0_0"
    ~mode:Accuser
    ~challenge_window:10
    ~commitment_period:10
    ~variant:"pvm_proof_2"
    (refutation_scenario_parameters
       ~loser_modes:["7 7 22_000_002_000"]
       (inputs_for 10)
       ~final_level:80
       ~priority:`Priority_honest)
    protocols

(** Run one of the refutation tests in bailout mode instead of using a
    full operator. *)
let test_bailout_refutation protocols =
  test_refutation_scenario
    ~kind:"arith"
    ~mode:Operator
    ~challenge_window:10
    ~commitment_period:10
    ~variant:"bailout_mode_defends_its_commitment"
    (refutation_scenario_parameters
       ~loser_modes:["5 2 1"]
       (inputs_for 10)
       ~final_level:80
       ~reset_honest_on:
         [("smart_rollup_node_conflict_detected.v0", 3, Some Bailout)]
       ~priority:`Priority_honest)
    protocols

(** Run the node in bailout mode, the node will exit with an error,
    when it's run without an operator key *)
let bailout_mode_fail_to_start_without_operator ~kind =
  test_l1_scenario
    ~kind
    {
      variant = None;
      tags = ["rollup_node"; "mode"; "bailout"];
      description = "rollup node in bailout fails without operator";
    }
    ~uses:(fun _protocol -> [Constant.octez_smart_rollup_node])
  @@ fun _protocol sc_rollup tezos_node tezos_client ->
  let sc_rollup_node =
    Sc_rollup_node.create
      Bailout
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~kind
      ~operators:
        [
          (Sc_rollup_node.Cementing, Constant.bootstrap1.alias);
          (Sc_rollup_node.Recovering, Constant.bootstrap1.alias);
        ]
  in
  let* () = Sc_rollup_node.run ~wait_ready:false sc_rollup_node sc_rollup []
  and* () =
    Sc_rollup_node.check_error
      ~exit_code:1
      ~msg:(rex "Missing operators for purposes operating.")
      sc_rollup_node
  in
  unit

(** Start a bailout rollup node, fails directly as the operator has not stake. *)
let bailout_mode_fail_operator_no_stake ~kind =
  let _operator = Constant.bootstrap1.public_key_hash in
  test_full_scenario
    ~kind
    ~mode:Bailout
    {
      variant = None;
      tags = ["mode"; "bailout"];
      description = "rollup node in bailout fails operator has no stake";
    }
  @@ fun _protocol sc_rollup_node sc_rollup _tezos_node _tezos_client ->
  let* () = Sc_rollup_node.run ~wait_ready:false sc_rollup_node sc_rollup []
  and* () =
    Sc_rollup_node.check_error
      sc_rollup_node
      ~exit_code:1
      ~msg:(rex "This implicit account is not a staker of this smart rollup.")
  in
  unit

(** Bailout mode operator has no stake, scenario:
    - start an operator rollup and wait until it publish a commitment
    - stop the rollup node
    - bakes until refutation period is over
    - using octez client cement the commitment
    - restart the rollup node in bailout mode
  check that it fails directly when the operator has no stake.
    *)
let bailout_mode_recover_bond_starting_no_commitment_staked ~kind =
  let operator = Constant.bootstrap1.public_key_hash in
  let commitment_period = 5 in
  let challenge_window = 5 in
  test_full_scenario
    ~operator
    ~kind
    {
      variant = None;
      tags = ["mode"; "bailout"];
      description =
        "rollup node in bailout recovers bond when starting if no commitment \
         staked";
    }
    ~commitment_period
    ~challenge_window
  @@ fun protocol sc_rollup_node sc_rollup tezos_node tezos_client ->
  let () = Log.info "Start the rollup in Operator mode" in
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
  in
  (* bake until the first commitment is published *)
  let* _level =
    bake_until_lpc_updated
      ~at_least:commitment_period
      tezos_client
      sc_rollup_node
  in
  let* published_commitment =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  Log.info "Terminate the node" ;
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  Log.info "Bake until refutation period is over" ;
  let* () = bake_levels challenge_window tezos_client in
  (* manually cement the commitment *)
  let to_cement_commitment_hash =
    published_commitment.commitment_and_hash.hash
  in
  let* () =
    cement_commitment
      protocol
      tezos_client
      ~sc_rollup
      ~hash:to_cement_commitment_hash
  in
  let* () = Client.bake_for_and_wait tezos_client in
  Log.info "Check that there is still tezt in frozen balance" ;
  let* frozen_balance =
    Client.RPC.call tezos_client
    @@ RPC.get_chain_block_context_contract_frozen_bonds ~id:operator ()
  in
  let () =
    Check.(
      (Tez.to_mutez frozen_balance <> 0)
        int
        ~error_msg:
          "The operator should have a stake nor holds a frozen balance.")
  in
  let* staked_on =
    Client.RPC.call tezos_client
    @@ RPC
       .get_chain_block_context_smart_rollups_smart_rollup_staker_staked_on_commitment
         ~sc_rollup
         operator
  in
  let staked_on =
    (* We do not really care about the value *)
    Option.map (Fun.const ()) (JSON.as_opt staked_on)
  in
  let () =
    Check.(
      (staked_on = None)
        (option unit)
        ~error_msg:
          "The operator should have a stake but no commitment attached to it.")
  in
  Log.info
    "Start a rollup node in bailout mode, operator still has stake but no \
     attached commitment" ;
  let sc_rollup_node' =
    Sc_rollup_node.create
      Bailout
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~kind
      ~default_operator:operator
      ~operators:[(Recovering, Constant.bootstrap2.public_key_hash)]
      ~data_dir:(Sc_rollup_node.data_dir sc_rollup_node)
  in
  let wait_for_bailout_exit =
    let event_name = "smart_rollup_node_daemon_exit_bailout_mode.v0" in
    bake_until_event tezos_client ~event_name
    @@ Sc_rollup_node.wait_for sc_rollup_node' event_name (Fun.const (Some ()))
  in
  let* () = Sc_rollup_node.run ~event_level:`Debug sc_rollup_node' sc_rollup []
  and* () = wait_for_bailout_exit in
  Log.info "Check that the bond have been recovered by the rollup node" ;
  let* frozen_balance =
    Client.RPC.call tezos_client
    @@ RPC.get_chain_block_context_contract_frozen_bonds ~id:operator ()
  in
  let () =
    Check.(
      (Tez.to_mutez frozen_balance = 0)
        int
        ~error_msg:"The operator should have recovered its stake.")
  in
  unit

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
  let* starting_level = Node.get_level node in
  let mk_commit ~src ~ticks ~depth ~pred =
    (* Compute the inbox level for which we'd like to commit *)
    let inbox_level = starting_level + (commitment_period_in_blocks * depth) in
    (* d is the delta between the target inbox level and the current level *)
    let* current_level = Node.get_level node in
    let d = inbox_level - current_level + 1 in
    (* Bake sufficiently many blocks to be able to commit for the desired inbox
       level. We may actually bake no blocks if d <= 0 *)
    let* () = repeat d (fun () -> Client.bake_for_and_wait client) in
    let* _, commitment_hash =
      forge_and_publish_commitment
        ~inbox_level
        ~predecessor:pred
        ~sc_rollup
        ~number_of_ticks:ticks
        ~src
        client
    in
    return commitment_hash
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
let test_forking_scenario ~kind ~variant scenario =
  let commitment_period = 3 in
  let challenge_window = commitment_period * 7 in
  let timeout = 10 in
  test_l1_scenario
    ~challenge_window
    ~commitment_period
    ~timeout
    ~kind
    {
      tags = ["refutation"; "game"; "commitment"];
      variant = Some variant;
      description = "rollup with a commitment dispute";
    }
  @@ fun protocol sc_rollup tezos_node tezos_client ->
  (* Choosing challenge_windows to be quite longer than commitment_period
     to avoid being in a situation where the first commitment in the result
     of [mk_forking_commitments] is cementable without further bakes. *)

  (* Completely arbitrary as we decide when to trigger timeouts in tests.
     Making it a lot smaller than the default value to speed up tests. *)
  (* Building a forking commitments tree. *)
  let operator1 = Constant.bootstrap1 in
  let operator2 = Constant.bootstrap2 in
  let* level0 = Node.get_level tezos_node in
  let* commits =
    mk_forking_commitments
      tezos_node
      tezos_client
      ~sc_rollup
      ~operator1:operator1.public_key_hash
      ~operator2:operator2.public_key_hash
  in
  let* level1 = Node.get_level tezos_node in
  scenario
    tezos_client
    tezos_node
    protocol
    ~sc_rollup
    ~operator1
    ~operator2
    commits
    level0
    level1

(* A more convenient wrapper around [cement_commitment]. *)
let cement_commitments protocol client sc_rollup ?fail =
  Lwt_list.iter_s (fun hash ->
      cement_commitment protocol client ~sc_rollup ~hash ?fail)

(** Given a commitment tree constructed by {test_forking_scenario}, this function:
    - tests different (failing and non-failing) cementation of commitments
      and checks the returned error for each situation (in case of failure);
    - resolves the dispute on top of c2, and checks that the defeated branch
      is removed, while the alive one can be cemented.
*)
let test_no_cementation_if_parent_not_lcc_or_if_disputed_commit =
  test_forking_scenario ~variant:"publish, and cement on wrong commitment"
  @@
  fun client
      _node
      protocol
      ~sc_rollup
      ~operator1
      ~operator2
      commits
      level0
      level1
    ->
  let c1, c2, c31, c32, c311, _c321 = commits in
  let* constants = get_sc_rollup_constants client in
  let challenge_window = constants.challenge_window_in_blocks in
  let cement = cement_commitments protocol client sc_rollup in
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
  (* c1 and c2 will be cemented. *)
  let* () = cement [c1; c2] in
  (* We cannot cement c31 or c32 on top of c2 because they are disputed *)
  let* () = cement [c31; c32] ~fail:disputed_commit in

  (* +++ dispute resolution +++
     Let's resolve the dispute between operator1 and operator2 on the fork
     c31 vs c32. [operator1] will make a bad initial dissection, so it
     loses the dispute, and the branch c32 --- c321 dies. *)

  (* [operator1] starts a dispute. *)
  let* () =
    start_refute
      client
      ~source:operator2
      ~opponent:operator1.public_key_hash
      ~sc_rollup
      ~player_commitment_hash:c32
      ~opponent_commitment_hash:c31
  in
  (* [operator1] will not play and will be timeout-ed. *)
  let timeout_period = constants.timeout_period_in_blocks in
  let* () =
    repeat (timeout_period + 1) (fun () -> Client.bake_for_and_wait client)
  in
  (* He even timeout himself, what a shame. *)
  let* () =
    timeout
      ~sc_rollup
      ~staker1:operator1.public_key_hash
      ~staker2:operator2.public_key_hash
      client
  in
  (* Now, we can cement c31 on top of c2 and c311 on top of c31. *)
  cement [c31; c311]

(** Given a commitment tree constructed by {test_forking_scenario}, this test
    starts a dispute and makes a first valid dissection move.
*)
let test_valid_dispute_dissection =
  test_forking_scenario ~variant:"valid dispute dissection"
  @@
  fun client
      _node
      protocol
      ~sc_rollup
      ~operator1
      ~operator2
      commits
      _level0
      _level1
    ->
  let c1, c2, c31, c32, _c311, _c321 = commits in
  let cement = cement_commitments protocol client sc_rollup in
  let* constants = get_sc_rollup_constants client in
  let challenge_window = constants.challenge_window_in_blocks in
  let commitment_period = constants.commitment_period_in_blocks in
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
    start_refute
      client
      ~source
      ~opponent
      ~sc_rollup
      ~player_commitment_hash:c32
      ~opponent_commitment_hash:c31
  in
  (* If this hash needs to be recomputed, run this test with --verbose
     and grep for 'compressed_state' in the produced logs. *)
  let state_hash = "srs11Z9V76SGd97kGmDQXV8tEF67C48GMy77RuaHdF1kWLk6UTmMfj" in

  (* Inject a valid dissection move *)
  let* () =
    move_refute_with_unique_state_hash
      client
      ~source
      ~opponent
      ~sc_rollup
      ~state_hash
  in

  (* We cannot cement neither c31, nor c32 because refutation game hasn't
     ended. *)
  cement [c31; c32] ~fail:"Attempted to cement a disputed commitment"

(* Testing the timeout to record gas consumption in a regression trace and
   detect when the value changes.
   For functional tests on timing-out a dispute, see unit tests in
   [lib_protocol].

   For this test, we rely on [test_forking_scenario] to create a tree structure
   of commitments and we start a dispute.
   The first player is not even going to play, we'll simply bake enough blocks
   to get to the point where we can timeout. *)
let test_timeout =
  test_forking_scenario ~variant:"timeout"
  @@
  fun client
      _node
      protocol
      ~sc_rollup
      ~operator1
      ~operator2
      commits
      level0
      level1
    ->
  (* These are the commitments on the rollup. See [test_forking_scenario] to
       visualize the tree structure. *)
  let c1, c2, c31, c32, _c311, _c321 = commits in
  (* A helper function to cement a sequence of commitments. *)
  let cement = cement_commitments protocol client sc_rollup in
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
    start_refute
      client
      ~source:operator2
      ~opponent:operator1.public_key_hash
      ~sc_rollup
      ~player_commitment_hash:c32
      ~opponent_commitment_hash:c31
  in
  (* Get exactly to the block where we are able to timeout. *)
  let* () =
    repeat (timeout_period + 1) (fun () -> Client.bake_for_and_wait client)
  in
  timeout
    ~sc_rollup
    ~staker1:operator1.public_key_hash
    ~staker2:operator2.public_key_hash
    client

(* Testing rollup node catch up mechanism I
   --------------------------------------

   The rollup node must be able to catch up from the genesis
   of the rollup when paired with a node in archive mode.
*)
let test_late_rollup_node ~kind =
  test_full_scenario
    ~commitment_period:3
    ~kind
    {
      tags = ["late"];
      variant = None;
      description = "a late rollup should catch up";
    }
  @@ fun _protocol sc_rollup_node sc_rollup_address node client ->
  let* () = bake_levels 65 client in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let* () = bake_levels 30 client in
  let* _status = Sc_rollup_node.wait_for_level ~timeout:2. sc_rollup_node 95 in
  Log.info "First rollup node synchronized." ;
  let sc_rollup_node2 =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~kind
      ~default_operator:
        Constant.bootstrap1.alias (* Same as other rollup_node *)
  in
  Log.info "Start rollup node from scratch with same operator" ;
  let* () = Sc_rollup_node.run sc_rollup_node2 sc_rollup_address [] in
  let sync_rpc_process =
    Curl.get (Sc_rollup_node.endpoint sc_rollup_node2 ^ "/local/synchronized")
  in
  let* _level = wait_for_current_level node ~timeout:10. sc_rollup_node2 in
  let*! _ = sync_rpc_process in
  Log.info "Other rollup node synchronized." ;
  let*! sync_json =
    Curl.get (Sc_rollup_node.endpoint sc_rollup_node2 ^ "/local/synchronized")
  in
  Check.((JSON.encode sync_json = {|"synchronized"|}) string)
    ~error_msg:"Sync RPC did not respond with synchronized" ;
  let* () = Client.bake_for_and_wait client in
  let* _level = wait_for_current_level node ~timeout:2. sc_rollup_node2 in
  Log.info "Other rollup node progresses." ;
  unit

(* Testing rollup node catch up mechanism II
   --------------------------------------

   An alternative rollup node must be able to catch up from the genesis
   of the rollup when paired with a node in archive mode when there is
   already an other rollup node with a different operator already operating
   on the given rollup. This same alternative rollup node must be able to
   catch up a second time when it is stopped midway.
*)
let test_late_rollup_node_2 ~kind =
  test_full_scenario
    ~commitment_period:3
    ~challenge_window:10
    ~kind
    {
      tags = ["late"; "gc"];
      variant = None;
      description = "a late alternative rollup should catch up";
    }
  @@ fun _protocol sc_rollup_node sc_rollup_address node client ->
  let* () = bake_levels 65 client in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let* () = bake_levels 30 client in
  let* _status = Sc_rollup_node.wait_for_level ~timeout:2. sc_rollup_node 95 in
  Log.info "First rollup node synchronized." ;
  let sc_rollup_node2 =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~kind
      ~default_operator:Constant.bootstrap2.alias
  in
  Log.info
    "Starting alternative rollup node from scratch with a different operator." ;
  (* Do gc every block, to test we don't remove live data *)
  let* () = Sc_rollup_node.run sc_rollup_node2 sc_rollup_address [] in
  let* _level = wait_for_current_level node ~timeout:20. sc_rollup_node2 in
  Log.info "Alternative rollup node is synchronized." ;
  let* () = Client.bake_for_and_wait client in
  let* _level = wait_for_current_level node ~timeout:2. sc_rollup_node2 in
  Log.info "Both rollup nodes are progressing and are synchronized." ;
  let* () = Sc_rollup_node.terminate sc_rollup_node2 in
  Log.info "Alternative rollup node terminated." ;
  let* () = bake_levels 30 client in
  let* () = Sc_rollup_node.run sc_rollup_node2 sc_rollup_address [] in
  Log.info "Alternative rollup node is re-running." ;
  let* () = bake_levels 30 client in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:2. sc_rollup_node2 155 in
  Log.info
    "Alternative rollup node is synchronized once again after being terminated \
     once." ;
  unit

(* Test interruption of rollup node before the first inbox is processed. Upon
   restart the node should not complain that an inbox is missing. *)
let test_interrupt_rollup_node =
  test_full_scenario
    {
      tags = ["interrupt"];
      variant = None;
      description = "a rollup should recover on interruption before first inbox";
    }
  @@ fun _protocol sc_rollup_node sc_rollup _node client ->
  let processing_promise =
    Sc_rollup_node.wait_for
      sc_rollup_node
      "smart_rollup_node_daemon_process_head.v0"
      (fun _ -> Some ())
  in
  let* () = bake_levels 15 client in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup []
  and* () = processing_promise in
  let* () = Sc_rollup_node.kill sc_rollup_node in
  let* () = bake_levels 1 client in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:20. sc_rollup_node 18 in
  unit

let remote_signer_uri signer ~yes =
  if yes then Some (Signer.uri signer) else None

let test_remote_signer ~hardcoded_remote_signer ~kind =
  let default_operator = Constant.bootstrap2 in
  let operators =
    [
      (Sc_rollup_node.Operating, Constant.bootstrap3);
      (Cementing, Constant.bootstrap4);
      (Batching, Constant.bootstrap5);
    ]
  in
  test_full_scenario
    {
      tags = ["remote"; "signer"];
      variant = None;
      description =
        sf
          "rollup node can sign operations with %s remote signer"
          (if hardcoded_remote_signer then "hardcoded" else "relocatable");
    }
    ~uses:(fun _ -> [Constant.octez_signer])
    ~kind
    ~commitment_period:3
    ~challenge_window:5
    ~mode:Operator
    ~operator:default_operator.public_key_hash
    ~operators:
      (List.map (fun (p, k) -> (p, k.Account.public_key_hash)) operators)
  @@ fun _protocol sc_rollup_node sc_rollup node _client ->
  let keys = default_operator :: List.map snd operators in
  Log.info "Starting remote signer" ;
  let* signer = Signer.init ~keys () in
  let client_remote_signer =
    remote_signer_uri signer ~yes:(not hardcoded_remote_signer)
  in
  let base_dir = Sc_rollup_node.base_dir sc_rollup_node in
  let* client =
    Client.init
      ~base_dir
      ~endpoint:(Node node)
      ?remote_signer:client_remote_signer
      ()
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~default_operator:default_operator.public_key_hash
      ~operators:
        (List.map (fun (p, k) -> (p, k.Account.public_key_hash)) operators)
      ?remote_signer:client_remote_signer
      ~base_dir
      ~kind
      Operator
      node
  in
  Log.info "Registering keys as remote in client" ;
  let import (k : Account.key) =
    Client.import_signer_key
      client
      ~force:true
      ~public_key_hash:k.public_key_hash
      ~alias:k.alias
      ?signer:(remote_signer_uri signer ~yes:hardcoded_remote_signer)
  in
  let* () = Lwt_list.iter_s import keys in
  let sks =
    JSON.parse_file (Filename.concat (Client.base_dir client) "secret_keys")
    |> JSON.as_list
  in
  Log.info "Check configuration uses remote signer" ;
  let check_config_key (k : Account.key) =
    let open JSON in
    let entry = List.find (fun e -> e |-> "name" |> as_string = k.alias) sks in
    let sk = entry |-> "value" |> as_string in
    if
      not
      @@ (String.starts_with ~prefix:"http://" sk
         || String.starts_with ~prefix:"remote:" sk)
    then Test.fail ~__LOC__ "Key %s not using remote signer" k.alias
  in
  List.iter check_config_key keys ;
  Log.info "Create wallet just for baking" ;
  let baking_client =
    Client.create ~name:"baking_client" ~endpoint:(Node node) ()
  in
  let* () =
    Client.import_secret_key
      baking_client
      Constant.bootstrap1.secret_key
      ~alias:Constant.bootstrap1.alias
  in
  Log.info "Stopping signer" ;
  let* () = Signer.terminate signer in
  let* () = Sc_rollup_node.run ~wait_ready:false sc_rollup_node sc_rollup []
  and* () =
    Lwt.choose
      [
        (let* () = Lwt_unix.sleep 30. in
         Test.fail
           "Rollup node did not detect the remote signer was unreachable");
        Sc_rollup_node.check_error
          ~exit_code:1
          ~msg:(rex "Cannot get public key for signer")
          sc_rollup_node;
      ]
  in
  Log.info "Restart signer" ;
  let* () = Signer.restart signer in
  Log.info "Starting rollup node" ;
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
  in
  let signer_has_signed =
    Lwt.pick
      [
        Signer.wait_for signer "signing_data.v0" (fun _ ->
            Log.info ~color:Log.Color.BG.green "Remote signer signed!" ;
            Some ());
        (let* () = Lwt_unix.sleep 120. in
         Test.fail "Remote signer did not sign");
      ]
  in
  Log.info "Progressing until rollup node publishes and cements" ;
  let* _ =
    bake_until_lcc_updated
      ~at_least:5
      ~timeout:120.
      ~level:3
      baking_client
      sc_rollup_node
  in
  let* () = signer_has_signed in
  unit

let test_refutation_reward_and_punishment ~kind =
  let timeout_period = 3 in
  let commitment_period = 2 in
  test_l1_scenario
    ~kind
    ~timeout:timeout_period
    ~commitment_period
    ~dal_rewards_weight:0
      (* Set the DAL rewards to zero, to not interfere with this test. See details
         here: https://gitlab.com/tezos/tezos/-/issues/7696#note_2307123115. *)
    ~regression:true
    ~hooks
    {
      tags = ["refutation"; "reward"; "punishment"];
      variant = None;
      description = "participant of a refutation game are slashed/rewarded";
    }
  @@ fun _protocol sc_rollup node client ->
  (* Timeout is the easiest way to end a game, we set timeout period
         low to produce an outcome quickly. *)
  let* {commitment_period_in_blocks; stake_amount; _} =
    get_sc_rollup_constants client
  in
  let punishment = Tez.to_mutez stake_amount in
  let reward = punishment / 2 in

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
  let* starting_level = Node.get_level node in
  let inbox_level = starting_level + commitment_period_in_blocks in
  (* d is the delta between the target inbox level and the current level *)
  let* current_level = Node.get_level node in
  let d = inbox_level - current_level + 1 in
  (* Bake sufficiently many blocks to be able to commit for the desired inbox
     level. We may actually bake no blocks if d <= 0 *)
  let* () = repeat d (fun () -> Client.bake_for_and_wait client) in

  (* [operator1] stakes on a commitment. *)
  let* _, operator1_commitment =
    forge_and_publish_commitment
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
  let* _, operator2_commitment =
    forge_and_publish_commitment
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
    let* () =
      (* Bake one commitment period before starting the refutation game. *)
      let* constants = get_sc_rollup_constants client in
      let commitment_period_in_blocks = constants.commitment_period_in_blocks in
      Client.bake_for_and_wait ~count:(commitment_period_in_blocks - 1) client
    in
    start_refute
      client
      ~source:operator1
      ~opponent:operator2.public_key_hash
      ~sc_rollup
      ~player_commitment_hash:operator1_commitment
      ~opponent_commitment_hash:operator2_commitment
  in
  (* Get exactly to the block where we are able to timeout. *)
  let* () =
    repeat (timeout_period + 1) (fun () -> Client.bake_for_and_wait client)
  in
  let* () =
    timeout
      ~sc_rollup
      ~staker1:operator2.public_key_hash
      ~staker2:operator1.public_key_hash
      ~src:Constant.bootstrap1.alias
      client
  in

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
    (final_operator2_balances.liquid = new_operator2_balances.liquid + reward)
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

  unit

let originate_outbox_target_contract ~storage_ty ~init_storage ~sc_rollup
    ~executor ~originator client =
  Log.info "Originate target contract" ;
  let prg =
    Printf.sprintf
      {|
          {
            parameter (or (%s %%default) (%s %%aux));
            storage (%s :s);
            code
              {
                # Check that SENDER is the rollup address
                SENDER;
                PUSH address %S;
                ASSERT_CMPEQ;
                # Check that SOURCE is the implicit account used for executing
                # the outbox message.
                SOURCE;
                PUSH address %S;
                ASSERT_CMPEQ;
                UNPAIR;
                IF_LEFT
                  { SWAP ; DROP; NIL operation }
                  { SWAP ; DROP; NIL operation };
                PAIR;
              }
          }
        |}
      storage_ty
      storage_ty
      storage_ty
      sc_rollup
      executor
  in
  let* address =
    Client.originate_contract
      ~alias:"target"
      ~amount:(Tez.of_int 100)
      ~burn_cap:(Tez.of_int 100)
      ~src:originator
      ~prg
      ~init:init_storage
      client
  in
  let* () = Client.bake_for_and_wait client in
  return address

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
let test_outbox_message_generic ?supports ?regression ?expected_error
    ?expected_l1_error ~earliness ?entrypoint ~init_storage ~storage_ty
    ?outbox_parameters_ty ?boot_sector ?(reorg = false) ~input_message
    ~expected_storage ~kind ~message_kind ~auto_execute_outbox () =
  let commitment_period = 2 and challenge_window = 5 in
  let _outbox_level = 5 in
  let message_index = 0 in
  let message_kind_s =
    match message_kind with `Internal -> "intern" | `External -> "extern"
  in
  let entrypoint_s = Option.value ~default:"default" entrypoint in
  let outbox_parameters_ty_s =
    Option.value ~default:"no_parameters_ty" outbox_parameters_ty
  in
  let auto_s = if auto_execute_outbox then ", auto_execute_outbox" else "" in
  let reorg_s = if reorg then ", reorg" else "" in
  test_full_scenario
    ?supports
    ?regression
    ~hooks
    ?boot_sector
    ~parameters_ty:"bytes"
    ~kind
    ~commitment_period
    ~challenge_window
    {
      tags =
        (["outbox"; message_kind_s; entrypoint_s; outbox_parameters_ty_s]
        @ if reorg then ["reorg"] else []);
      variant =
        Some
          (Format.sprintf
             "%s, entrypoint: %%%s, eager: %d, %s, %s%s%s"
             init_storage
             entrypoint_s
             earliness
             message_kind_s
             outbox_parameters_ty_s
             auto_s
             reorg_s);
      description = "output exec";
    }
    ~uses:(fun _protocol -> [Constant.octez_codec])
  @@ fun protocol rollup_node sc_rollup node client ->
  let* reorg =
    if reorg then
      let* actions = setup_double_reorg node client in
      return (Some actions)
    else return None
  in
  let src = Constant.bootstrap1.public_key_hash in
  let src2 = Constant.bootstrap2.public_key_hash in
  let* () =
    if auto_execute_outbox then (
      Sc_rollup_node.change_operators rollup_node [(Executing_outbox, src2)] ;
      let* () =
        Process.check @@ Sc_rollup_node.spawn_config_init rollup_node sc_rollup
      in
      Sc_rollup_node.Config_file.update
        rollup_node
        Sc_rollup_node.patch_config_execute_outbox ;
      Log.info "config updated" ;
      unit)
    else unit
  in
  let* () = Sc_rollup_node.run ~event_level:`Debug rollup_node sc_rollup [] in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. rollup_node in
  let consumed_outputs outbox_level =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_consumed_outputs
         ~sc_rollup
         ~outbox_level
         ()
  in
  let check_contract_execution address expected_storage =
    Log.info "Check contract execution" ;
    let* storage = Client.contract_storage address client in
    return
    @@ Check.(
         (String.trim storage = expected_storage)
           string
           ~error_msg:"Invalid contract storage: expecting '%R', got '%L'.")
  in
  let check_outbox_execution outbox_level indexes status =
    let* _ = Sc_rollup_node.wait_sync rollup_node ~timeout:10. in
    let* executable_outboxes =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_local_outbox_pending_executable ()
    in
    let* unexecutable_outboxes =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_local_outbox_pending_unexecutable ()
    in
    let get_indexes l =
      match List.assoc_opt outbox_level l with
      | None -> []
      | Some l -> List.map (fun x -> x.Sc_rollup_rpc.message_index) l
    in
    let executable = get_indexes executable_outboxes in
    let unexecutable = get_indexes unexecutable_outboxes in
    match (status, executable, unexecutable) with
    | `Executable, e, [] when e = indexes -> unit
    | `Unexecutable, [], e when e = indexes -> unit
    | _ ->
        Test.fail
          "Outbox level should be [%a] %s but there is [%a] executable, [%a] \
           unexecutable."
          (Format.pp_print_list Format.pp_print_int)
          indexes
          (match status with
          | `Executable -> "executable"
          | `Unexecutable -> "unexecutable")
          (Format.pp_print_list Format.pp_print_int)
          executable
          (Format.pp_print_list Format.pp_print_int)
          unexecutable
  in
  let perform_rollup_execution_and_cement source_address target_address =
    Log.info "Perform rollup execution and cement" ;
    let trigger_output_message ?(extra_empty_messages = 0) client =
      let* payload = input_message protocol target_address in
      let* () =
        match payload with
        | `External payload ->
            let extra = List.init extra_empty_messages (fun _ -> "") in
            send_text_messages
              ~hooks
              ~input_format:`Hex
              client
              (extra @ [payload])
        | `Internal payload ->
            let payload = "0x" ^ payload in
            let* () =
              Client.transfer
                ~amount:Tez.(of_int 100)
                ~burn_cap:Tez.(of_int 100)
                ~storage_limit:100000
                ~giver:Constant.bootstrap1.alias
                ~receiver:source_address
                ~arg:(sf "Pair %s %S" payload sc_rollup)
                client
            in
            Client.bake_for_and_wait client
      in
      let* _ = Sc_rollup_node.wait_sync rollup_node ~timeout:10. in
      unit
    in
    let* () =
      match reorg with
      | None -> trigger_output_message client
      | Some (divergence, _) ->
          divergence
            ~branch1:trigger_output_message
            ~branch2:(trigger_output_message ~extra_empty_messages:2)
    in
    let* outbox_level = Sc_rollup_node.wait_sync rollup_node ~timeout:10. in
    let* () =
      match reorg with
      | None -> unit
      | Some (_, trigger_reorg) ->
          let* () = trigger_reorg () in
          let* _ = Sc_rollup_node.wait_sync rollup_node ~timeout:10. in
          unit
    in
    let* {outbox; _} =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_global_block
           ~block:(string_of_int outbox_level)
           ~outbox:true
           ()
    in
    let nb_outbox_transactions_in_block =
      outbox
      |> List.map @@ fun (Sc_rollup_rpc.Transactions trs) -> List.length trs
    in
    Check.((nb_outbox_transactions_in_block = [1]) (list int))
      ~error_msg:"Block has %L outbox transactions but expected %R" ;
    let* () = check_outbox_execution outbox_level [0] `Unexecutable in
    let* _ =
      bake_until_lcc_updated
        ~timeout:100.
        client
        rollup_node
        ~level:outbox_level
        ~hook:(fun _ ->
          let* _level = Sc_rollup_node.wait_sync ~timeout:10. rollup_node in
          unit)
    in
    return outbox_level
  in
  let trigger_outbox_message_execution ?expected_l1_error ~outbox_level address
      =
    Log.info "Trigger outbox message execution" ;
    let parameters = "37" in
    let check_expected_outbox () =
      let* outbox =
        Sc_rollup_node.RPC.call rollup_node
        @@ Sc_rollup_rpc.get_global_block_outbox ~outbox_level ()
      in
      Log.info "Outbox is %s" (JSON.encode outbox) ;
      match expected_error with
      | None ->
          let expected =
            JSON.parse ~origin:"trigger_outbox_message_execution"
            @@ Printf.sprintf
                 {|
              [ { "outbox_level": %d, "message_index": "%d",
                  "message":
                  { "transactions":
                      [ { "parameters": { "int": "%s" }%s,
                          "destination": "%s"%s } ]%s
                     } } ] |}
                 outbox_level
                 message_index
                 parameters
                 (match outbox_parameters_ty with
                 | None -> ""
                 | Some outbox_parameters_ty ->
                     Format.asprintf
                       {| , "parameters_ty" : { "prim": "%s"} |}
                       outbox_parameters_ty)
                 address
                 (match entrypoint with
                 | None -> ""
                 | Some entrypoint ->
                     Format.asprintf {| , "entrypoint" : "%s" |} entrypoint)
                 (Printf.sprintf
                    {|, "kind": "%s"|}
                    (Option.fold
                       ~none:"untyped"
                       ~some:(fun _ -> "typed")
                       outbox_parameters_ty))
          in
          Log.info "Expected is %s" (JSON.encode expected) ;
          assert (JSON.encode expected = JSON.encode outbox) ;
          Sc_rollup_node.RPC.call rollup_node
          @@ Sc_rollup_rpc.outbox_proof_simple ~message_index ~outbox_level ()
      | Some _ ->
          assert (JSON.encode outbox = "[]") ;
          return None
    in
    if Option.is_some expected_error then unit
    else if auto_execute_outbox then
      let* consumed = consumed_outputs outbox_level in
      if consumed = [0] then (* Already executed automatically *)
        unit
      else
        let* () = check_outbox_execution outbox_level [0] `Executable in
        bake_until_execute_outbox_message
          ~at_least:1
          ~timeout:30.
          client
          rollup_node
    else
      let* answer = check_expected_outbox () in
      match answer with
      | None -> failwith "Unexpected error during proof generation"
      | Some {commitment_hash; proof} -> (
          match expected_l1_error with
          | None ->
              let* () = check_outbox_execution outbox_level [0] `Executable in
              let*! () =
                Client.Sc_rollup.execute_outbox_message
                  ~hooks
                  ~burn_cap:(Tez.of_int 1)
                  ~rollup:sc_rollup
                  ~src:src2
                  ~commitment_hash
                  ~proof
                  client
              in
              let* () = Client.bake_for_and_wait client in
              let* _ = Sc_rollup_node.wait_sync rollup_node ~timeout:10. in
              let* _ = Client.RPC.call client @@ RPC.get_chain_block () in
              unit
              (* Client.bake_for_and_wait client *)
          | Some msg ->
              let*? process =
                Client.Sc_rollup.execute_outbox_message
                  ~hooks
                  ~burn_cap:(Tez.of_int 10)
                  ~rollup:sc_rollup
                  ~src:src2
                  ~commitment_hash
                  ~proof
                  client
              in
              Process.check_error ~msg process)
  in
  let* target_contract_address =
    originate_outbox_target_contract
      ~storage_ty
      ~init_storage
      ~sc_rollup
      ~executor:src2
      ~originator:src
      client
  in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. rollup_node in
  let* source_contract_address =
    originate_forward_smart_contract client protocol
  in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. rollup_node in
  let* outbox_level =
    perform_rollup_execution_and_cement
      source_contract_address
      target_contract_address
  in
  let* () =
    if auto_execute_outbox then (* may be already executed automatically *)
      unit
    else
      let* prior_consumed_outputs = consumed_outputs outbox_level in
      Check.((prior_consumed_outputs = []) (list int))
        ~error_msg:"Expected empty list found %L for consumed outputs" ;
      unit
  in
  let* () =
    trigger_outbox_message_execution
      ?expected_l1_error
      ~outbox_level
      target_contract_address
  in
  match expected_error with
  | None ->
      let* () =
        if Option.is_none expected_l1_error then (
          let* after_consumed_outputs = consumed_outputs outbox_level in
          Check.((after_consumed_outputs = [message_index]) (list int))
            ~error_msg:"Expected %R found %L for consumed outputs" ;
          unit)
        else unit
      in
      let* () =
        check_contract_execution target_contract_address expected_storage
      in
      unit
  | Some _ -> unit

let test_outbox_message ?supports ?regression ?expected_error ?expected_l1_error
    ~earliness ?entrypoint ?(init_storage = "0") ?(storage_ty = "int")
    ?(outbox_parameters = "37") ?outbox_parameters_ty ?reorg ~kind ~message_kind
    ~auto_execute_outbox =
  let wrap payload =
    match message_kind with
    | `Internal -> `Internal payload
    | `External -> `External payload
  in
  let boot_sector, input_message, expected_storage =
    match kind with
    | "arith" ->
        let input_message _protocol contract_address =
          let payload =
            Printf.sprintf
              "%s %s%s"
              outbox_parameters
              contract_address
              (match entrypoint with Some e -> "%" ^ e | None -> "")
          in
          let payload = hex_encode payload in
          return @@ wrap payload
        in
        (None, input_message, outbox_parameters)
    | "wasm_2_0_0" ->
        let bootsector = read_kernel "echo" in
        let input_message protocol contract_address =
          let parameters_json = `O [("int", `String outbox_parameters)] in
          let transaction =
            Sc_rollup_helpers.
              {
                destination = contract_address;
                entrypoint;
                parameters = parameters_json;
                parameters_ty =
                  (match outbox_parameters_ty with
                  | Some json_value -> Some (`O [("prim", `String json_value)])
                  | None -> None);
              }
          in
          let* answer =
            Codec.encode
              ~name:
                (Protocol.encoding_prefix protocol
                ^ ".smart_rollup.outbox.message")
              (Sc_rollup_helpers.json_of_output_tx_batch [transaction])
          in
          return (wrap (String.trim answer))
        in
        ( Some bootsector,
          input_message,
          if Option.is_none expected_l1_error then outbox_parameters
          else init_storage )
    | _ ->
        (* There is no other PVM in the protocol. *)
        assert false
  in
  test_outbox_message_generic
    ?supports
    ?regression
    ?expected_error
    ?expected_l1_error
    ~earliness
    ?entrypoint
    ?outbox_parameters_ty
    ?boot_sector
    ?reorg
    ~init_storage
    ~storage_ty
    ~input_message
    ~expected_storage
    ~message_kind
    ~kind
    ~auto_execute_outbox
    ()

let test_outbox_message_reorg protocols ~kind =
  test_outbox_message
    ~earliness:0
    ~message_kind:`External
    ~kind
    ~auto_execute_outbox:true
    ~reorg:true
    protocols

let test_outbox_message_reorg_disappear ~kind =
  let commitment_period = 2 and challenge_window = 2 in
  test_full_scenario
    ~parameters_ty:"bytes"
    ~kind
    ~commitment_period
    ~challenge_window
    {
      tags = ["outbox"; "reorg"];
      variant = None;
      description =
        "Outbox execution when reorg without message should not crash";
    }
    ~uses:(fun _protocol -> [Constant.octez_codec])
  @@ fun protocol rollup_node sc_rollup node client ->
  let src = Constant.bootstrap1.public_key_hash in
  let executor = Constant.bootstrap2.public_key_hash in
  Sc_rollup_node.change_operators rollup_node [(Executing_outbox, executor)] ;
  let* () =
    Process.check @@ Sc_rollup_node.spawn_config_init rollup_node sc_rollup
  in
  Sc_rollup_node.Config_file.update
    rollup_node
    Sc_rollup_node.patch_config_execute_outbox ;
  Log.info "config updated" ;
  let* () =
    Sc_rollup_node.run ~event_level:`Debug rollup_node sc_rollup [No_degraded]
  in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. rollup_node in
  let* target_contract_address =
    originate_outbox_target_contract
      ~storage_ty:"int"
      ~init_storage:"0"
      ~sc_rollup
      ~executor
      ~originator:src
      client
  in
  Log.info "Perform rollup execution and cement" ;
  let trigger_output_message client =
    let parameters_json = `O [("int", `String "37")] in
    let transaction =
      Sc_rollup_helpers.
        {
          destination = target_contract_address;
          entrypoint = None;
          parameters = parameters_json;
          parameters_ty = None;
        }
    in
    let* answer =
      Codec.encode
        ~name:
          (Protocol.encoding_prefix protocol ^ ".smart_rollup.outbox.message")
        (Sc_rollup_helpers.json_of_output_tx_batch [transaction])
    in
    let payload = String.trim answer in
    send_text_messages ~input_format:`Hex client [payload]
  in
  let* divergence, trigger_reorg = setup_reorg node client in
  let* () =
    (* First branch triggers an output message, while second branch has no
       message on the rollup. *)
    divergence ~branch1:trigger_output_message ~branch2:Client.bake_for_and_wait
  in
  let* outbox_level = Sc_rollup_node.wait_sync rollup_node ~timeout:10. in
  let* () = trigger_reorg () in
  let* _ =
    bake_until_lcc_updated ~timeout:100. client rollup_node ~level:outbox_level
  in
  let* () = Client.bake_for_and_wait client in
  let* _level = Sc_rollup_node.wait_sync ~timeout:10. rollup_node in
  unit

let test_outbox_message protocols ~kind =
  let test
      (expected_error, earliness, entrypoint, message_kind, auto_execute_outbox)
      =
    test_outbox_message
      ?expected_error
      ~earliness
      ?entrypoint
      ~message_kind
      protocols
      ~kind
      ~auto_execute_outbox ;
    (* arith does not support, yet, the typed outbox messages *)
    if kind <> "arith" then
      test_outbox_message
        ~supports:(Protocol.From_protocol 17)
        ?expected_error
        ~earliness
        ?entrypoint
        ~message_kind
        ~outbox_parameters_ty:"int"
        protocols
        ~kind
        ~auto_execute_outbox
  in
  List.iter
    test
    [
      (None, 0, None, `Internal, false);
      (None, 0, Some "aux", `Internal, false);
      (Some (Base.rex ".*Invalid claim about outbox"), 5, None, `Internal, false);
      ( Some (Base.rex ".*Invalid claim about outbox"),
        5,
        Some "aux",
        `Internal,
        false );
      (None, 0, None, `External, false);
      (None, 0, Some "aux", `External, false);
      (Some (Base.rex ".*Invalid claim about outbox"), 5, None, `External, false);
      ( Some (Base.rex ".*Invalid claim about outbox"),
        5,
        Some "aux",
        `External,
        false );
      (None, 0, None, `Internal, true);
      (None, 0, Some "aux", `Internal, true);
      (None, 0, None, `External, true);
      (None, 0, Some "aux", `External, true);
    ] ;
  if kind <> "arith" then (
    (* wrong type for the parameters *)
    test_outbox_message
      ~expected_l1_error:
        (Base.rex "A data expression was invalid for its expected type.")
      ~supports:(Protocol.From_protocol 17)
      ~earliness:0
      ~message_kind:`Internal
      ~outbox_parameters_ty:"string"
      protocols
      ~kind
      ~auto_execute_outbox:false ;
    test_outbox_message
      ~expected_l1_error:
        (Base.rex ".*or a parameter was supplied of the wrong type")
      ~supports:(Protocol.From_protocol 17)
      ~earliness:0
      ~message_kind:`Internal
      ~init_storage:{|"word"|}
      ~storage_ty:"string"
      ~outbox_parameters_ty:"int"
      protocols
      ~kind
      ~auto_execute_outbox:false)

let test_rpcs ~kind
    ?(boot_sector = Sc_rollup_helpers.default_boot_sector_of ~kind)
    ?kernel_debug_log ?preimages_dir =
  test_full_scenario
    ~uses:(fun _protocol -> default_boot_sector_uses_of ~kind)
    ~regression:true
    ?kernel_debug_log
    ~hooks
    ~kind
    ~boot_sector
    ~whitelist_enable:true
    ?preimages_dir
    {
      tags = ["rpc"; "api"];
      variant = None;
      description = "RPC API should work and be stable";
    }
  @@ fun protocol sc_rollup_node sc_rollup node client ->
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
  in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  (* Smart rollup address endpoint test *)
  let* sc_rollup_address =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_smart_rollup_address ()
  in
  Check.((sc_rollup_address = sc_rollup) string)
    ~error_msg:"SC rollup address of node is %L but should be %R" ;
  let n = 15 in
  let batch_size = 5 in
  let* ids =
    send_messages_batcher
      ~rpc_hooks:Tezos_regression.rpc_hooks
      ~batch_size
      n
      client
      sc_rollup_node
  in
  Check.((List.length ids = n * batch_size) int)
    ~error_msg:"Injected %L messages but should have injected %R" ;
  (* Head block hash endpoint test *)
  let* level = Node.get_level node in
  let* _ = Sc_rollup_node.wait_sync ~timeout:10. sc_rollup_node in
  let* l1_block_hash = Client.RPC.call client @@ RPC.get_chain_block_hash () in
  let* l1_finalized_block_hash =
    Client.RPC.call client @@ RPC.get_chain_block_hash ~block:"head~2" ()
  in
  let* l2_block_hash =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_hash ()
  in
  let l2_block_hash = JSON.as_string l2_block_hash in
  if Sc_rollup_node.monitors_finalized sc_rollup_node then
    Check.((l1_finalized_block_hash = l2_block_hash) string)
      ~error_msg:"Finalized head on L1 is %L where as head on L2 is %R"
  else
    Check.((l1_block_hash = l2_block_hash) string)
      ~error_msg:"Head on L1 is %L where as on L2 it is %R" ;
  let* l1_block_hash_5 =
    Client.RPC.call client @@ RPC.get_chain_block_hash ~block:"5" ()
  in
  let* l2_block_hash_5 =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_hash ~block:"5" ()
  in
  let l2_block_hash_5 = JSON.as_string l2_block_hash_5 in
  Check.((l1_block_hash_5 = l2_block_hash_5) string)
    ~error_msg:"Block 5 on L1 is %L where as on L2 it is %R" ;
  let* l2_finalied_block_level =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_level ~block:"finalized" ()
  in
  let l2_finalied_block_level = JSON.as_int l2_finalied_block_level in
  Check.((l2_finalied_block_level = level - 2) int)
    ~error_msg:"Finalized block is %L but should be %R" ;
  let* l2_num_messages =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_num_messages ()
  in
  (* There are always 3 extra messages inserted by the protocol in the inbox. *)
  let nb_protocol_messages = 3 in
  let l2_num_messages = JSON.as_int l2_num_messages in
  Check.((l2_num_messages = batch_size + nb_protocol_messages) int)
    ~error_msg:"Number of messages of head is %L but should be %R" ;

  (* Durable value storage RPC tests *)
  let* () =
    match kind with
    | "arith" ->
        (* Make sure we neither have WASM nor Arith PVM endpoint in arith PVM *)
        let* response =
          Sc_rollup_node.RPC.call_raw sc_rollup_node ~rpc_hooks
          @@ Sc_rollup_rpc.get_global_block_durable_state_value
               ~pvm_kind:"wasm_2_0_0"
               ~operation:Sc_rollup_rpc.Value
               ~key:"/readonly/wasm_version"
               ()
        in
        let* () = return @@ RPC_core.check_string_response ~code:404 response in
        let* response =
          Sc_rollup_node.RPC.call_raw sc_rollup_node ~rpc_hooks
          @@ Sc_rollup_rpc.get_global_block_durable_state_value
               ~pvm_kind:"arith"
               ~operation:Sc_rollup_rpc.Value
               ~key:"/readonly/wasm_version"
               ()
        in
        return @@ RPC_core.check_string_response ~code:404 response
    | "wasm_2_0_0" ->
        let* wasm_boot_sector =
          Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
          @@ Sc_rollup_rpc.get_global_block_durable_state_value
               ~pvm_kind:kind
               ~operation:Sc_rollup_rpc.Value
               ~key:"/kernel/boot.wasm"
               ()
        in
        Check.(
          (wasm_boot_sector = Some Constant.wasm_echo_kernel_boot_sector)
            (option string))
          ~error_msg:"Encoded WASM kernel is %L but should be %R" ;
        let* nonexisting_wasm_boot_sector =
          Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
          @@ Sc_rollup_rpc.get_global_block_durable_state_value
               ~pvm_kind:kind
               ~operation:Sc_rollup_rpc.Value
               ~key:"/kernel/boot.wasm2"
               ()
        in
        Check.((nonexisting_wasm_boot_sector = None) (option string))
          ~error_msg:"Encoded WASM kernel is %L but should be %R" ;

        let* wasm_version_hex_opt =
          Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
          @@ Sc_rollup_rpc.get_global_block_durable_state_value
               ~pvm_kind:kind
               ~operation:Sc_rollup_rpc.Value
               ~key:"/readonly/wasm_version"
               ()
        in
        let wasm_version =
          Option.map
            (fun wasm_version_hex -> Hex.to_string (`Hex wasm_version_hex))
            wasm_version_hex_opt
        in
        Check.(
          (wasm_version = Some (default_wasm_pvm_revision protocol))
            (option string))
          ~error_msg:"Decoded WASM version is %L but should be %R" ;

        let* wasm_version_len =
          Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
          @@ Sc_rollup_rpc.get_global_block_durable_state_value
               ~pvm_kind:kind
               ~operation:Sc_rollup_rpc.Length
               ~key:"/readonly/wasm_version"
               ()
        in
        Check.(
          (wasm_version_len
          = Some
              (default_wasm_pvm_revision protocol
              |> String.length |> Int64.of_int))
            (option int64))
          ~error_msg:"WASM version value length is %L but should be %R" ;

        let* kernel_subkeys =
          Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
          @@ Sc_rollup_rpc.get_global_block_durable_state_value
               ~pvm_kind:kind
               ~operation:Sc_rollup_rpc.Subkeys
               ~key:"/readonly/kernel"
               ()
        in
        Check.((kernel_subkeys = ["boot.wasm"; "env"]) (list string))
          ~error_msg:"The key's subkeys are %L but should be %R" ;

        Log.info "Add elements in the durable storage" ;
        (* Stop the rollup node. *)
        let* () = Sc_rollup_node.terminate sc_rollup_node in

        let elements =
          List.init 30 (fun i ->
              (Format.sprintf "number%02d" i, Format.sprintf "%02x" i))
        in
        let* () =
          Lwt_list.iter_s
            (fun (key, value) ->
              Sc_rollup_node.patch_durable_storage
                sc_rollup_node
                ~key:("/numbers/" ^ key)
                ~value)
            elements
        in
        let* () =
          Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
        in

        let* number_values =
          Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
          @@ Sc_rollup_rpc.get_global_block_durable_state_value
               ~pvm_kind:kind
               ~operation:Sc_rollup_rpc.Values
               ~key:"/numbers"
               ()
        in
        let number_values =
          List.fast_sort
            (fun (k1, _) (k2, _) -> String.compare k1 k2)
            number_values
        in
        Check.((number_values = elements) (list (tuple2 string string)))
          ~error_msg:"/numbers has %L but should contain values for %R" ;

        unit
    | "riscv" -> unit
    | _ -> failwith "incorrect kind"
  in
  let* _status =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_status ()
  in
  let* _ticks =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_ticks ()
  in
  let* _state_hash =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ()
  in
  let* _outbox =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_outbox
         ~block:"head"
         ~outbox_level:l2_finalied_block_level
         ()
  in
  let* _head =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_tezos_head ()
  in
  let* _level =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_tezos_level ()
  in
  let* {block_hash = l2_block_hash'; _} =
    Sc_rollup_node.RPC.call sc_rollup_node @@ Sc_rollup_rpc.get_global_block ()
  in
  Check.((l2_block_hash' = l2_block_hash) string)
    ~error_msg:"L2 head is from full block is %L but should be %R" ;
  if Protocol.number protocol >= 018 then (
    let whitelist = [Constant.bootstrap1.public_key_hash] in
    let* _, sc_rollup =
      setup_rollup ~alias:"rollup2" ~kind ~whitelist node client
    in
    let* retrieved_whitelist =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_whitelist
           sc_rollup
    in
    Check.(
      is_true
        (match retrieved_whitelist with
        | Some l -> List.equal String.equal l whitelist
        | _ -> false))
      ~error_msg:"no whitelist found." ;
    unit)
  else unit

let test_messages_processed_by_commitment ~kind =
  test_full_scenario
    {
      variant = None;
      tags = ["commitment"; "evaluation"];
      description = "checks messages processed during a commitment period";
    }
    ~kind
  @@ fun _protocol sc_rollup_node sc_rollup _node client ->
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* {commitment_period_in_blocks; _} = get_sc_rollup_constants client in
  let* genesis_info =
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let store_commitment_level =
    init_level + commitment_period_in_blocks + block_finality_time
  in
  (* Bake enough blocks so [sc_rollup_node] posts a commitment. *)
  let* () =
    repeat (commitment_period_in_blocks + block_finality_time) (fun () ->
        Client.bake_for_and_wait client)
  in
  (* Wait until the [sc_rollup_node] store the commitment. *)
  let* (_ : int) =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      store_commitment_level
  in
  let* {commitment = {inbox_level; _}; hash = _} =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_last_stored_commitment ()
  in
  let* current_level =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_current_level
         ~block:(string_of_int inbox_level)
         ()
  in
  Check.((current_level = inbox_level) int)
    ~error_msg:
      "The rollup node should process all the levels of a commitment period, \
       expected %L, got %R" ;
  unit

let test_recover_bond_of_stakers =
  test_l1_scenario
    ~regression:true
    ~hooks
    ~boot_sector:""
    ~kind:"arith"
    ~challenge_window:10
    ~commitment_period:10
    {
      variant = None;
      tags = ["commitment"; "staker"; "recover"];
      description = "recover bond of stakers";
    }
  @@ fun protocol sc_rollup _tezos_node tezos_client ->
  let* {
         commitment_period_in_blocks;
         challenge_window_in_blocks;
         stake_amount;
         _;
       } =
    get_sc_rollup_constants tezos_client
  in
  let* predecessor, level =
    last_cemented_commitment_hash_with_level ~sc_rollup tezos_client
  in
  let staker1 = Constant.bootstrap1 in
  let staker2 = Constant.bootstrap2 in
  (* Bake enough to publish. *)
  let* () =
    repeat (commitment_period_in_blocks + 1) (fun () ->
        Client.bake_for_and_wait tezos_client)
  in
  (* Both accounts stakes on a commitment. *)
  let* _, commitment1 =
    forge_and_publish_commitment
      ~inbox_level:(level + commitment_period_in_blocks)
      ~predecessor
      ~sc_rollup
      ~src:staker1.public_key_hash
      tezos_client
  in
  let* _, commitment2 =
    forge_and_publish_commitment
      ~inbox_level:(level + commitment_period_in_blocks)
      ~predecessor
      ~sc_rollup
      ~src:staker2.public_key_hash
      tezos_client
  in
  assert (commitment1 = commitment2) ;
  (* Bake enough to cement. *)
  let* () =
    repeat challenge_window_in_blocks (fun () ->
        Client.bake_for_and_wait tezos_client)
  in
  (* Cement. *)
  let* () =
    cement_commitment protocol tezos_client ~sc_rollup ~hash:commitment1
  in

  (* Staker1 withdraw its stake. *)
  let* () =
    attempt_withdraw_stake
      ~check_liquid_balance:false
      ~sc_rollup
      ~sc_rollup_stake_amount:(Tez.to_mutez stake_amount)
      ~src:staker1.public_key_hash
      ~staker:staker1.public_key_hash
      tezos_client
  in
  (* Staker1 withdraw the stake of staker2. *)
  let* () =
    attempt_withdraw_stake
      ~check_liquid_balance:false
      ~sc_rollup
      ~sc_rollup_stake_amount:(Tez.to_mutez stake_amount)
      ~src:staker1.public_key_hash
      ~staker:staker2.public_key_hash
      tezos_client
  in
  unit

let test_injector_auto_discard =
  let kind = "arith" in
  test_full_scenario
    {
      variant = None;
      tags = ["injector"];
      description = "Injector discards repeatedly failing operations";
    }
    ~kind
  @@ fun _protocol _sc_rollup_node sc_rollup tezos_node client ->
  let* operator = Client.gen_and_show_keys client in
  (* Change operator and only batch messages *)
  let sc_rollup_node =
    Sc_rollup_node.create
      Batcher
      tezos_node
      ~base_dir:(Client.base_dir client)
      ~kind
      ~operators:[(Sc_rollup_node.Batching, operator.alias)]
  in
  let nb_attempts = 5 in
  let* () =
    Sc_rollup_node.run
      ~event_level:`Debug
      sc_rollup_node
      sc_rollup
      [Injector_attempts nb_attempts]
  in
  let monitor_injector_queue =
    Sc_rollup_node.wait_for
      sc_rollup_node
      "number_of_operations_in_queue.v0"
      (fun event ->
        let nb = JSON.(event |-> "number_of_operations" |> as_int) in
        Log.info "Injector: %d operations in queue" nb ;
        (* Because we send one batch of messages by block to the injector, and
           each operation is allowed to fail [nb_attempts] times, we should have
           at most [nb_attempts] in the queue. *)
        Check.((nb <= nb_attempts) int)
          ~error_msg:
            "There are %L add messages operations in the injector queue but \
             there should be at most %R" ;
        None)
  in
  let n = 65 in
  let batch_size = 3 in
  let* _ids =
    send_messages_batcher
      ~rpc_hooks:Tezos_regression.rpc_hooks
      ~batch_size
      n
      client
      sc_rollup_node
  in
  Lwt.cancel monitor_injector_queue ;
  unit

let test_arg_boot_sector_file ~kind =
  let hex_if_wasm s =
    match kind with "wasm_2_0_0" -> Hex.(of_string s |> show) | _ -> s
  in
  let boot_sector =
    hex_if_wasm "Nantes aurait été un meilleur nom de protocol"
  in
  test_full_scenario
    ~supports:(Protocol.From_protocol 018)
    ~kind
    ~boot_sector
    {
      variant = None;
      tags = ["node"; "boot_sector"; "boot_sector_file"];
      description = "Rollup node uses argument boot sector file";
    }
  @@ fun _protocol rollup_node rollup _node client ->
  let invalid_boot_sector =
    hex_if_wasm "Nairobi est un bien meilleur nom de protocol que Nantes"
  in
  let invalid_boot_sector_file =
    Filename.temp_file "invalid-boot-sector" ".hex"
  in
  let () = write_file invalid_boot_sector_file ~contents:invalid_boot_sector in
  let valid_boot_sector_file = Filename.temp_file "valid-boot-sector" ".hex" in
  let () = write_file valid_boot_sector_file ~contents:boot_sector in
  (* Starts the rollup node with an invalid boot sector. Asserts that the
     node fails with an invalid genesis state. *)
  let* () =
    Sc_rollup_node.run
      ~wait_ready:false
      rollup_node
      rollup
      [Boot_sector_file invalid_boot_sector_file]
  and* () =
    Sc_rollup_node.check_error
      ~exit_code:1
      ~msg:
        (rex
           "Computed genesis commitment hash .* is not equal to the rollup \
            genesis commitment hash .* which commits state hash .*")
      rollup_node
  in
  (* Starts the rollup node with a valid boot sector. Asserts that the node
     works as expected by processing blocks. *)
  let* () =
    Sc_rollup_node.run
    (* the restart is needed because the node and/or tezt daemon
       might not fully stoped yet. Another solution would be to have
       a sleep but it's more uncertain. *)
      ~restart:true
      rollup_node
      rollup
      [Boot_sector_file valid_boot_sector_file]
  in
  let* () = Client.bake_for_and_wait client in
  let* _ = Sc_rollup_node.wait_sync ~timeout:10. rollup_node in
  unit

let test_unsafe_genesis_patch ~private_ ~kind =
  let commitment_period = 3 in
  let max_nb_tick = 50_000_000_000_000L in
  let unsupported_pvm = match kind with "wasm_2_0_0" -> false | _ -> true in
  let should_fail = unsupported_pvm || not private_ in
  let operator = Constant.bootstrap1.public_key_hash in
  let whitelist = if private_ then Some [operator] else None in
  test_full_scenario
    ~kind
    ~commitment_period
    ~supports:(Protocol.From_protocol 018)
    ?whitelist
    {
      variant = None;
      tags = ["node"; "unsafe_patch"];
      description =
        sf
          "Rollup (%s) can%s apply unsafe genesis PVM patches"
          (if private_ then "private" else "public")
          (if should_fail then "not" else "");
    }
  @@ fun _protocol rollup_node rollup _node client ->
  Log.info "Set patch in configuration" ;
  let* _ = Sc_rollup_node.config_init rollup_node rollup in
  Sc_rollup_node.Config_file.update rollup_node (fun config ->
      let open JSON in
      put
        ( "unsafe-pvm-patches",
          parse
            ~origin:"increase-tick"
            (sf {| [ { "increase_max_nb_tick" : "%Ld"} ] |} max_nb_tick) )
        config) ;
  () ;
  let* () =
    Sc_rollup_node.run
      ~wait_ready:false
      rollup_node
      rollup
      [Apply_unsafe_patches]
  in
  if should_fail then
    let msg =
      if unsupported_pvm then rex "Patch .* is not supported"
      else if not private_ then
        rex "Unsafe PVM patches can only be applied in private rollups"
      else assert false
    in
    Sc_rollup_node.check_error ~exit_code:1 ~msg rollup_node
  else
    let* () = bake_levels (commitment_period + 4) client in
    let* _ = Sc_rollup_node.wait_sync ~timeout:10. rollup_node in
    let* published_commitment =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_local_last_published_commitment ()
    in
    let* () =
      check_published_commitment_in_l1 rollup client published_commitment
    in
    let* pvm_max_bytes =
      Sc_rollup_node.RPC.call rollup_node ~rpc_hooks
      @@ Sc_rollup_rpc.get_global_block_state ~key:"pvm/max_nb_ticks" ()
    in
    let pvm_max =
      pvm_max_bytes
      |> Data_encoding.Binary.of_bytes_exn Data_encoding.n
      |> Z.to_int64
    in
    Check.((pvm_max = max_nb_tick) int64)
      ~error_msg:"PVM max tick should have been increased to %R but is %L" ;
    unit

let test_bootstrap_smart_rollup_originated =
  register_test
    ~supports:(From_protocol 018)
    ~__FILE__
    ~tags:["bootstrap"; "parameter"]
    ~title:"Bootstrap smart rollups are listed"
  @@ fun protocol ->
  let bootstrap_arith : Protocol.bootstrap_smart_rollup =
    {
      address = "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf";
      pvm_kind = "arith";
      boot_sector = "";
      parameters_ty = `O [("prim", `String "unit")];
      whitelist = None;
    }
  in
  let bootstrap_wasm : Protocol.bootstrap_smart_rollup =
    {
      address = "sr163Lv22CdE8QagCwf48PWDTquk6isQwv57";
      pvm_kind = "wasm_2_0_0";
      boot_sector = "";
      parameters_ty = `O [("prim", `String "unit")];
      whitelist = None;
    }
  in
  let bootstrap_smart_rollups = [bootstrap_arith; bootstrap_wasm] in
  let* _node, client = setup_l1 ~bootstrap_smart_rollups protocol in
  let* rollups =
    Client.RPC.call client @@ RPC.get_chain_block_context_smart_rollups_all ()
  in
  let bootstrap_smart_rollups_addresses =
    List.map (fun Protocol.{address; _} -> address) bootstrap_smart_rollups
  in
  Check.(
    (rollups = bootstrap_smart_rollups_addresses)
      (list string)
      ~error_msg:"Expected %R bootstrapped smart rollups, got %L") ;
  unit

let test_bootstrap_private_smart_rollup_originated =
  register_test
    ~supports:(From_protocol 018)
    ~__FILE__
    ~tags:["bootstrap"; "parameter"; "private"]
    ~title:"Bootstrap private smart rollups are private"
  @@ fun protocol ->
  let whitelist = Some [Constant.bootstrap1.public_key_hash] in
  let bootstrap_arith : Protocol.bootstrap_smart_rollup =
    {
      address = "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf";
      pvm_kind = "arith";
      boot_sector = "";
      parameters_ty = `O [("prim", `String "unit")];
      whitelist;
    }
  in
  let bootstrap_smart_rollups = [bootstrap_arith] in
  let* _node, client =
    setup_l1 ~bootstrap_smart_rollups ~whitelist_enable:true protocol
  in
  let* found_whitelist =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_whitelist
         bootstrap_arith.address
  in
  Check.(
    (whitelist = found_whitelist)
      (option (list string))
      ~error_msg:"Expected %R whitelist for bootstrapped smart rollups , got %L") ;
  unit

let test_rollup_node_missing_preimage_exit_at_initialisation ~kind =
  register_test
    ~supports:(From_protocol 016)
    ~__FILE__
    ~tags:["node"; "preimage"; "boot_sector"]
    ~uses:(fun _protocol ->
      Constant.
        [octez_smart_rollup_node; smart_rollup_installer; Constant.WASM.echo])
    ~title:
      "Rollup node exit if at initialisation, there is one or multiple \
       preimage(s) missing."
  @@ fun protocol ->
  let* node, client = setup_l1 protocol in
  let rollup_node =
    Sc_rollup_node.create
      ~base_dir:(Client.base_dir client)
      ~kind
      ~default_operator:Constant.bootstrap1.alias
      Operator
      node
  in
  let* {boot_sector; _} =
    (* The preimages will be saved in the rollup node's data directory
       ROLLUP_NODE_DATA_DIR, whereas the rollup node will try to look
       for the preimages in ROLLUP_NODE_DATA_DIR/wasm_2_0_0. *)
    let preimages_dir = Sc_rollup_node.data_dir rollup_node in
    Sc_rollup_helpers.prepare_installer_kernel ~preimages_dir Constant.WASM.echo
  in
  let* rollup_address =
    originate_sc_rollup
      ~kind:"wasm_2_0_0"
      ~boot_sector
      ~src:Constant.bootstrap1.alias
      client
  in
  let* _ = Sc_rollup_node.config_init rollup_node rollup_address in
  let* () = Sc_rollup_node.run rollup_node rollup_address [] in
  let* () = Client.bake_for_and_wait client
  and* () =
    Sc_rollup_node.check_error
      ~msg:(rex "Could not open file containing preimage of reveal hash")
      rollup_node
  in
  Lwt.return_unit

let test_private_rollup_whitelist ?check_error ~regression ~description
    ~commit_publisher ~whitelist =
  test_l1_scenario
    ~regression
    ~supports:(From_protocol 018)
    ~whitelist_enable:true
    ~whitelist
    ~src:Constant.bootstrap1.public_key_hash
    {variant = None; tags = ["whitelist"]; description}
    ~kind:"arith"
  @@ fun _protocol sc_rollup _node client ->
  let* () = Client.bake_for_and_wait client in
  let* predecessor, inbox_level =
    last_cemented_commitment_hash_with_level ~sc_rollup client
  in
  let _, client_runnable =
    forge_and_publish_commitment_return_runnable
      ~predecessor
      ~inbox_level
      ~sc_rollup
      ~src:commit_publisher
      client
  in
  let client_process = client_runnable.value in
  match check_error with
  | Some check -> check client_process
  | None -> return ()

let test_private_rollup_whitelisted_staker =
  test_private_rollup_whitelist
    ~regression:true
    ~whitelist:[Constant.bootstrap1.public_key_hash]
    ~commit_publisher:Constant.bootstrap1.alias
    ~description:"Whitelisted staker can publish a commitment"

let test_private_rollup_non_whitelisted_staker =
  test_private_rollup_whitelist
    ~regression:false
    ~whitelist:[Constant.bootstrap2.public_key_hash]
    ~commit_publisher:Constant.bootstrap1.alias
    ~description:"Non-whitelisted staker cannot publish a commitment"
    ~check_error:
      (Process.check_error
         ~msg:
           (rex
              "The rollup is private and the submitter of the commitment is \
               not present in the whitelist"))

let test_private_rollup_node_publish_in_whitelist =
  let commitment_period = 3 in
  test_full_scenario
    ~supports:(From_protocol 018)
    ~whitelist_enable:true
    ~whitelist:[Constant.bootstrap1.public_key_hash]
    ~operator:Constant.bootstrap1.alias
    ~commitment_period
    {
      variant = None;
      tags = ["whitelist"];
      description =
        "Rollup node publishes commitment if the operator is in the whitelist";
    }
    ~kind:"arith"
  @@ fun _protocol rollup_node sc_rollup _tezos_node tezos_client ->
  let* () = Sc_rollup_node.run ~event_level:`Debug rollup_node sc_rollup [] in
  let levels = commitment_period in
  Log.info "Baking at least %d blocks for commitment of first message" levels ;
  let* _new_level =
    bake_until_lpc_updated ~at_least:levels ~timeout:5. tezos_client rollup_node
  in
  bake_levels levels tezos_client

let test_private_rollup_node_publish_not_in_whitelist =
  let operator = Constant.bootstrap1.alias in
  test_full_scenario
    ~supports:(From_protocol 018)
    ~whitelist_enable:true
    ~whitelist:[Constant.bootstrap2.public_key_hash]
    ~operator
    ~mode:Operator
    {
      variant = None;
      tags = ["whitelist"; "bla"];
      description =
        "Rollup node fails to start if the operator is not in the whitelist";
    }
    ~kind:"arith"
  @@ fun _protocol rollup_node sc_rollup _tezos_node _client ->
  let* () = Sc_rollup_node.run ~wait_ready:false rollup_node sc_rollup []
  and* () =
    Sc_rollup_node.check_error
      rollup_node
      ~exit_code:1
      ~msg:(rex ".*The operator is not in the whitelist.*")
  in
  unit

let test_rollup_whitelist_update ~kind =
  let commitment_period = 2 and challenge_window = 5 in
  let whitelist = [Constant.bootstrap1.public_key_hash] in
  test_full_scenario
    {
      variant = None;
      tags = ["private"; "whitelist"; "update"];
      description = "kernel update whitelist";
    }
    ~uses:(fun _protocol -> [Constant.octez_codec])
    ~kind
    ~whitelist_enable:true
    ~whitelist
    ~supports:(From_protocol 018)
    ~commitment_period
    ~challenge_window
    ~operator:Constant.bootstrap1.public_key_hash
  @@ fun protocol rollup_node rollup_addr node client ->
  let encode_whitelist_msg whitelist =
    Codec.encode
      ~name:(Protocol.encoding_prefix protocol ^ ".smart_rollup.outbox.message")
      (`O
         [
           ("whitelist", `A (List.map (fun pkh -> `String pkh) whitelist));
           ("kind", `String "whitelist_update");
         ])
  in
  let send_whitelist_then_bake_until_exec encoded_whitelist_msgs =
    let* _res =
      send_messages_then_bake_until_rollup_node_execute_output_message
        ~commitment_period
        ~challenge_window
        client
        rollup_node
        encoded_whitelist_msgs
    in
    unit
  in
  let last_published_commitment_hash rollup_node =
    let* Sc_rollup_rpc.{commitment_and_hash = {commitment; _}; _} =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_local_last_published_commitment ()
    in
    return commitment
  in
  let* () = Sc_rollup_node.run ~event_level:`Debug rollup_node rollup_addr [] in
  (* bake until the first commitment is published. *)
  let* _level =
    bake_until_lpc_updated ~at_least:commitment_period client rollup_node
  in
  let* () =
    let* commitment = last_published_commitment_hash rollup_node in
    (* Bootstrap2 attempts to publish a commitments while not present in the whitelist. *)
    let*? process =
      publish_commitment
        ~src:Constant.bootstrap2.alias
        ~commitment
        client
        rollup_addr
    in
    let* output_err =
      Process.check_and_read_stderr ~expect_failure:true process
    in
    (* The attempt at publishing a commitment fails. *)
    Check.(
      (output_err
      =~ rex
           ".*The rollup is private and the submitter of the commitment is not \
            present in the whitelist.*")
        ~error_msg:"Expected output \"%L\" to match expression \"%R\".") ;
    unit
  in
  let* () =
    let* encoded_whitelist_update =
      encode_whitelist_msg
        [
          Constant.bootstrap1.public_key_hash;
          Constant.bootstrap2.public_key_hash;
        ]
    in
    send_whitelist_then_bake_until_exec [encoded_whitelist_update]
  in
  let* () =
    (* Bootstrap2 now can publish a commitments as it's present in the whitelist. *)
    let* commitment = last_published_commitment_hash rollup_node in
    let*! () =
      publish_commitment
        ~src:Constant.bootstrap2.alias
        ~commitment
        client
        rollup_addr
    in
    let* () = Client.bake_for_and_wait client in
    unit
  in
  Log.info
    "submits two whitelist update in one inbox level. Only the second update \
     is executed by the rollup node." ;
  let* () =
    let* encoded_whitelist_update1 =
      encode_whitelist_msg [Constant.bootstrap3.public_key_hash]
    in
    let* encoded_whitelist_update2 =
      Codec.encode
        ~name:
          (Protocol.encoding_prefix protocol ^ ".smart_rollup.outbox.message")
        (`O [("kind", `String "whitelist_update")])
    in
    send_whitelist_then_bake_until_exec
      [encoded_whitelist_update1; encoded_whitelist_update2]
  in
  let* commitment = last_published_commitment_hash rollup_node in
  (* now an adress that was not previously in the whitelist can
     publish a commitment *)
  let*! () =
    publish_commitment
      ~src:Constant.bootstrap4.alias
      ~commitment
      client
      rollup_addr
  in
  let* () = Client.bake_for_and_wait client in
  Log.info
    "Start a new rollup node with an operator that was not in the whitelist to \
     ensure it can catch up" ;
  let rollup_node2 =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~kind
      ~default_operator:Constant.bootstrap3.alias
  in
  let* () = Sc_rollup_node.run rollup_node2 rollup_addr [] in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. rollup_node2 in
  unit

let test_rollup_whitelist_outdated_update ~kind =
  let commitment_period = 2 and challenge_window = 5 in
  let whitelist =
    [Constant.bootstrap1.public_key_hash; Constant.bootstrap2.public_key_hash]
  in
  test_full_scenario
    {
      variant = None;
      tags = ["whitelist"];
      description = "outdated whitelist update";
    }
    ~uses:(fun _protocol -> [Constant.octez_codec])
    ~kind
    ~whitelist_enable:true
    ~whitelist
    ~supports:(From_protocol 018)
    ~commitment_period
    ~challenge_window
  @@ fun protocol rollup_node rollup_addr _node client ->
  let* () = Sc_rollup_node.run ~event_level:`Debug rollup_node rollup_addr [] in
  let* payload =
    Codec.encode
      ~name:(Protocol.encoding_prefix protocol ^ ".smart_rollup.outbox.message")
      (`O
         [
           ("whitelist", `A [`String Constant.bootstrap1.public_key_hash]);
           ("kind", `String "whitelist_update");
         ])
  in
  let* payload2 =
    Codec.encode
      ~name:(Protocol.encoding_prefix protocol ^ ".smart_rollup.outbox.message")
      (`O
         [
           ( "whitelist",
             `A
               [
                 `String Constant.bootstrap1.public_key_hash;
                 `String Constant.bootstrap2.public_key_hash;
               ] );
           ("kind", `String "whitelist_update");
         ])
  in
  (* Execute whitelist update with outdated message index. *)
  let* _hash, outbox_level, message_index =
    send_messages_then_bake_until_rollup_node_execute_output_message
      ~commitment_period
      ~challenge_window
      client
      rollup_node
      [payload; payload2]
  in
  Check.((message_index = 1) int)
    ~error_msg:"Executed output message of index %L expected %R." ;
  let* {commitment_hash; proof} =
    get_outbox_proof rollup_node ~__LOC__ ~message_index:0 ~outbox_level
  in
  let {value = process; _} =
    Client.Sc_rollup.execute_outbox_message
      ~hooks
      ~burn_cap:(Tez.of_int 10)
      ~fee:(Tez.of_mutez_int 1498)
      ~rollup:rollup_addr
      ~src:Constant.bootstrap3.alias
      ~commitment_hash
      ~proof
      client
  in
  let* () =
    Process.check_error
      ~msg:(rex ".*Outdated whitelist update: got message index.*")
      process
  in

  (* Execute whitelist update with outdated outbox level. *)
  let* _hash, _outbox_level, _message_index =
    send_messages_then_bake_until_rollup_node_execute_output_message
      ~commitment_period
      ~challenge_window
      client
      rollup_node
      [payload; payload2]
  in
  let* {commitment_hash; proof} =
    get_outbox_proof rollup_node ~__LOC__ ~message_index ~outbox_level
  in
  let {value = process; _} =
    Client.Sc_rollup.execute_outbox_message
      ~hooks
      ~burn_cap:(Tez.of_int 10)
      ~fee:(Tez.of_mutez_int 1498)
      ~rollup:rollup_addr
      ~src:Constant.bootstrap3.alias
      ~commitment_hash
      ~proof
      client
  in
  Process.check_error
    ~msg:(rex ".*Outdated whitelist update: got outbox level.*")
    process

(** This test uses the rollup node, first it is running in an Operator
    mode, it bakes some blocks, then terminate. Then we restart the
    node in a Bailout mode, initiate the recover_bond process, and
    make sure that no new commitments are published. *)
let bailout_mode_not_publish ~kind =
  let operator = Constant.bootstrap5.public_key_hash in
  let commitment_period = 5 in
  let challenge_window = 5 in
  test_full_scenario
    {
      tags = ["node"; "mode"; "bailout"];
      variant = None;
      description = "rollup node - bailout mode does not publish";
    }
    ~kind
    ~operator
    ~mode:Operator
    ~challenge_window
    ~commitment_period
  @@ fun _protocol sc_rollup_node sc_rollup _tezos_node tezos_client ->
  (* Run the rollup node in Operator mode, bake some blocks until
     a commitment is published *)
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
  in
  let* _level =
    bake_until_lpc_updated
      ~at_least:commitment_period
      tezos_client
      sc_rollup_node
  in
  let* published_commitment_before =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_local_last_published_commitment ()
  in
  let* staked_on_commitment =
    get_staked_on_commitment ~sc_rollup ~staker:operator tezos_client
  in
  Log.info "Check that the LPC is equal to the staked commitment onchain." ;
  let () =
    Check.(
      published_commitment_before.commitment_and_hash.hash
      = staked_on_commitment)
      Check.string
      ~error_msg:"Last published commitment is not latest commitment staked on."
  in
  (* Terminate the rollup of Operator mode and restart it with the Bailout mode *)
  let* () =
    Sc_rollup_node.run
      ~restart:true
      ~event_level:`Debug
      sc_rollup_node
      sc_rollup
      []
      ~mode:Bailout
  in
  (* The challenge window is neded to compute the correct number of block before
     cementation, we also add 2 times of commitment period to make sure
     no commit are published. *)
  let* () =
    repeat
      ((2 * commitment_period) + challenge_window)
      (fun () -> Client.bake_for_and_wait tezos_client)
  and* () =
    Sc_rollup_node.wait_for
      sc_rollup_node
      "smart_rollup_node_recover_bond.v0"
      (Fun.const (Some ()))
  and* () =
    Sc_rollup_node.wait_for
      sc_rollup_node
      "smart_rollup_node_daemon_exit_bailout_mode.v0"
      (Fun.const (Some ()))
  and* exit_error = Sc_rollup_node.wait sc_rollup_node in
  let* lcc_hash, _level =
    Sc_rollup_helpers.last_cemented_commitment_hash_with_level
      ~sc_rollup
      tezos_client
  in
  Log.info "Check the LCC is the same." ;
  let () =
    Check.(lcc_hash = published_commitment_before.commitment_and_hash.hash)
      Check.string
      ~error_msg:
        "Published commitment is not the same as the cemented commitment hash."
  in
  Log.info
    "The node has submitted the recover_bond operation, and the operator is no \
     longer staked." ;
  let* frozen_balance =
    Client.RPC.call tezos_client
    @@ RPC.get_chain_block_context_contract_frozen_bonds ~id:operator ()
  in
  let () =
    Check.(
      (Tez.to_mutez frozen_balance = 0)
        int
        ~error_msg:
          "The operator should not have a stake nor holds a frozen balance.")
  in
  match exit_error with
  | WEXITED 0 -> unit
  | _ -> failwith "rollup node did not stop gracefully"

let custom_mode_empty_operation_kinds ~kind =
  test_l1_scenario
    ~kind
    {
      variant = None;
      tags = ["mode"; "custom"];
      description = "custom mode has empty operation kinds";
    }
    ~uses:(fun _protocol -> [Constant.octez_smart_rollup_node])
  @@ fun _protocol sc_rollup tezos_node tezos_client ->
  let sc_rollup_node =
    Sc_rollup_node.create
      (Custom [])
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~kind
      ~default_operator:Constant.bootstrap1.alias
  in
  let* () = Sc_rollup_node.run ~wait_ready:false sc_rollup_node sc_rollup []
  and* () =
    Sc_rollup_node.check_error
      sc_rollup_node
      ~exit_code:1
      ~msg:(rex "Operation kinds for custom mode are empty.")
  in
  unit

(* adds multiple batcher keys for a rollup node runs in batcher mode
   and make sure all keys are used to sign batches and injected in a
   block. *)
let test_multiple_batcher_key ~kind =
  test_l1_scenario
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6650

     also might be related to https://gitlab.com/tezos/tezos/-/issues/3014

     Test is flaky without rpc_external:false and it seems to be
     related to this issue. When investigating it seems that the sink
     used by the octez node has a race condition between process, it
     seems it due to the rpc server being run in a separate process. *)
    ~rpc_external:false
    ~kind
    {
      variant = None;
      tags = [Tag.flaky; "node"; "mode"; "batcher"];
      description = "multiple keys set for batcher";
    }
    ~uses:(fun _protocol -> [Constant.octez_smart_rollup_node])
  @@ fun _protocol sc_rollup tezos_node client ->
  (* nb_of_batcher * msg_per_batch * msg_size = expected_block_size
     16 * 32 * 1000 = 512_000 = maximum size of Tezos L1 block *)
  let nb_of_batcher = 16 in
  let msg_per_batch = 32 in
  let msg_size = 1000 in
  let* keys = gen_keys_then_transfer_tez client nb_of_batcher in
  let operators =
    List.map
      (fun k -> (Sc_rollup_node.Batching, k.Account.public_key_hash))
      keys
  in
  let* sc_rollup_node, _sc_rollup =
    setup_rollup
      ~parameters_ty:"string"
      ~kind
      ~mode:Batcher
      ~operators
      ~sc_rollup
      tezos_node
      client
  in
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup []
  in
  let batch () =
    let msg_cpt = ref 0 in
    (* create batch with different payloads so the logs show
       differents messages. *)
    List.init msg_per_batch (fun _ ->
        msg_cpt := !msg_cpt + 1 ;
        String.make msg_size @@ Char.chr (96 + !msg_cpt))
  in
  let inject_n_msgs_batches nb_of_batches =
    let* _hashes =
      Lwt.all @@ List.init nb_of_batches
      @@ fun _ ->
      let batch = batch () in
      let _hashes =
        Sc_rollup_node.RPC.call sc_rollup_node
        @@ Sc_rollup_rpc.post_local_batcher_injection ~messages:batch ()
      in
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.post_local_batcher_injection
           ~drop_duplicate:true
           ~messages:batch
           ()
    in
    unit
  in
  let wait_for_included_check_batches_and_returns_pkhs () =
    let find_map_op_content op_content_json =
      let kind = JSON.(op_content_json |-> "kind" |> as_string) in
      if kind = "smart_rollup_add_messages" then (
        let nb_msgs =
          JSON.(op_content_json |-> "message" |> as_list |> List.length)
        in
        Check.(nb_msgs = msg_per_batch)
          Check.int
          ~error_msg:"%L found where %R was expected" ;
        let src = JSON.(op_content_json |-> "source" |> as_string) in
        Some src)
      else None
    in
    wait_for_included_and_map_ops_content
      sc_rollup_node
      tezos_node
      ~timeout:30.
      ~find_map_op_content
  in
  let check_against_operators_pkhs =
    let operators_pkhs = List.map snd operators |> List.sort String.compare in
    fun pkhs ->
      Check.((List.sort String.compare pkhs = operators_pkhs) (list string))
        ~error_msg:"%L found where %R was expected)"
  in
  Log.info "Injecting 2 * number of batchers into a full batch" ;
  let* () = inject_n_msgs_batches (2 * nb_of_batcher) in
  Log.info "Waiting until all batchers key have injected a batch" ;
  let* () =
    let* () = Client.bake_for_and_wait client
    and* () =
      wait_until_n_batches_are_injected sc_rollup_node ~nb_batches:nb_of_batcher
    in
    unit
  in
  Log.info "Baking to include all previous batches and the new injection salvo" ;
  let* pkhs_first_salvo = wait_for_included_check_batches_and_returns_pkhs ()
  and* () = Client.bake_for_and_wait client
  and* () =
    wait_until_n_batches_are_injected sc_rollup_node ~nb_batches:nb_of_batcher
  in
  let () = check_against_operators_pkhs pkhs_first_salvo in
  let* pkhs_snd_salvo = wait_for_included_check_batches_and_returns_pkhs ()
  and* () = Client.bake_for_and_wait client in
  let () = check_against_operators_pkhs pkhs_snd_salvo in
  Log.info "Injecting a non full batches to check for keys rotation" ;
  let* _lvl = Client.bake_for_and_wait client in
  let* _ = Sc_rollup_node.wait_sync sc_rollup_node ~timeout:10. in
  let nb_round_1 = nb_of_batcher / 2 in
  let nb_round_2 = nb_of_batcher - nb_round_1 in
  let* () = inject_n_msgs_batches nb_round_1 in
  let* () = Client.bake_for_and_wait client
  and* () =
    wait_until_n_batches_are_injected sc_rollup_node ~nb_batches:nb_round_1
  in
  let* pkhs1 = wait_for_included_check_batches_and_returns_pkhs ()
  and* () = Client.bake_for_and_wait client in
  let* () = inject_n_msgs_batches nb_round_2 in
  let* () = Client.bake_for_and_wait client
  and* () =
    wait_until_n_batches_are_injected sc_rollup_node ~nb_batches:nb_round_2
  in
  let* pkhs2 = wait_for_included_check_batches_and_returns_pkhs ()
  and* () = Client.bake_for_and_wait client in
  let pkhs_used_twice = List.filter (fun s -> List.mem s pkhs2) pkhs1 in
  Log.debug "%d Keys used 1:" (List.length pkhs1) ;
  List.iter (Log.debug "- %s") pkhs1 ;
  Log.debug "%d Keys used 2:" (List.length pkhs2) ;
  List.iter (Log.debug "- %s") pkhs2 ;
  Check.((List.length pkhs_used_twice = 0) int)
    ~error_msg:"Reused %L keys but should have rotated not reused any." ;
  unit

let test_batcher_order_msgs ~kind =
  test_full_scenario
    {
      tags = ["batcher"; "messages"; "order"];
      variant = None;
      description = "rollup node - Batcher order message correctly";
    }
    ~mode:Batcher
    ~kind
    ~operators:[(Sc_rollup_node.Batching, Constant.bootstrap1.alias)]
  @@ fun _protocol rollup_node rollup_addr node client ->
  let min_batch_elements = 10 in
  let max_batch_elements = 20 in
  let* _config = Sc_rollup_node.config_init rollup_node rollup_addr in
  let () =
    Sc_rollup_node.Config_file.update rollup_node
    @@ JSON.update "batcher"
    @@ fun json ->
    let json =
      JSON.put
        ( "min_batch_elements",
          JSON.annotate
            ~origin:"min batch elements size"
            (`Float (Int.to_float min_batch_elements)) )
        json
    in
    let json =
      JSON.put
        ( "max_batch_elements",
          JSON.annotate
            ~origin:"max batch elements size"
            (`Float (Int.to_float max_batch_elements)) )
        json
    in
    json
  in

  let inject_int_of_string ?order ?drop_duplicate messages =
    Sc_rollup_node.RPC.call rollup_node
    @@ Sc_rollup_rpc.post_local_batcher_injection
         ?order
         ?drop_duplicate
         ~messages:(List.map string_of_int messages)
         ()
  in
  let wait_for_included_and_returns_msgs () =
    let find_map_op_content op_content_json =
      let kind = JSON.(op_content_json |-> "kind" |> as_string) in
      if kind = "smart_rollup_add_messages" then
        let msgs =
          JSON.(op_content_json |-> "message" |> as_list |> List.map as_string)
        in
        Some msgs
      else None
    in
    wait_for_included_and_map_ops_content
      rollup_node
      node
      ~timeout:30.
      ~find_map_op_content
  in

  let check_queue ~__LOC__ ~expected_queued_hashes ~expected_messages =
    let* queued_hashes, queued_msgs =
      let* queued_msgs =
        Sc_rollup_node.RPC.call rollup_node
        @@ Sc_rollup_rpc.get_local_batcher_queue ()
      in
      return @@ List.split queued_msgs
    in
    Check.(
      (List.sort String.compare queued_hashes
      = List.sort_uniq String.compare expected_queued_hashes)
        (list string)
        ~__LOC__)
      ~error_msg:"Queued msgs hashes %L was found but %R was expected" ;

    Check.(
      (List.map int_of_string queued_msgs = expected_messages)
        (list int)
        ~__LOC__)
      ~error_msg:"Queued msgs  %L was found but %R was expected" ;
    unit
  in

  let check_injector_queues ~__LOC__ ~expected_add_message_op =
    let* injector_queues =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_admin_injector_queues ()
    in
    match injector_queues with
    | [(injector_tag, injector_op_queue)] ->
        Check.(list_mem string "add_messages" injector_tag ~__LOC__)
          ~error_msg:"Injector queue %L operation tag does not contains tags %R" ;
        let map_assert_only_add_messages op =
          let kind = JSON.(op |-> "kind" |> as_string) in
          Check.(("add_messages" = kind) string ~__LOC__)
            ~error_msg:"Injector queue %L operation tag is not %R" ;
          JSON.(
            op |-> "message" |> as_list |> List.map JSON.as_string
            |> List.map (fun s -> int_of_string @@ Hex.to_string (`Hex s)))
        in
        let add_messages_op =
          List.map map_assert_only_add_messages injector_op_queue
        in
        Check.(
          (add_messages_op = expected_add_message_op) (list (list int)) ~__LOC__)
          ~error_msg:
            "Injector queues add_messages %L was found but %R was expected" ;
        unit
    | _ -> failwith "invalid number of injector queues, only one expected"
  in
  let bake_then_check_included_msgs ~__LOC__ ~expected_messages =
    let* level = Node.get_level node in
    let wait_for_injected =
      wait_until_n_batches_are_injected
        rollup_node
        ~nb_batches:(List.length expected_messages)
    in
    let* () = Client.bake_for_and_wait client
    and* () = wait_for_injected
    and* _lvl = Sc_rollup_node.wait_for_level rollup_node (level + 1) in
    let wait_for_included = wait_for_included_and_returns_msgs () in
    let* () = Client.bake_for_and_wait client
    and* msgs = wait_for_included
    and* _lvl = Sc_rollup_node.wait_for_level rollup_node (level + 2) in

    Check.(
      (List.map
         (List.map (fun s -> int_of_string @@ Hex.to_string (`Hex s)))
         msgs
      = expected_messages)
        (list (list int))
        ~__LOC__)
      ~error_msg:"Included msgs %L was found but %R was expected" ;
    unit
  in

  let* level = Node.get_level node in
  let* () =
    Sc_rollup_node.run
      ~event_level:`Debug
      rollup_node
      rollup_addr
      [Injector_retention_period 0]
    (*so injector included queued messages is not entraiving us *)
  in
  let* _ = Sc_rollup_node.wait_for_level rollup_node level in

  (* To make sure we are all bootstrapped, might be unused *)
  Log.info "injecting 3 messages with order specified (e.g. [10; 20; 30])" ;
  let* hashes_1 =
    fold 3 [] (fun i acc ->
        let order = 10 * (i + 1) in
        let* hashes = inject_int_of_string ~order [order] in
        return @@ hashes @ acc)
  in
  Log.info
    "injecting 4 messages with order specified in reverses (e.g. [ 30; 20; 10])" ;
  let* hashes_2 =
    fold 3 [] (fun i acc ->
        let order = 30 - (10 * i) in
        let* hashes = inject_int_of_string ~order [order] in
        return @@ hashes @ acc)
  in
  Log.info "injecting 1 message with no order specified" ;
  let* hashes_3 = inject_int_of_string [31] in
  Log.info "injecting 2 time the same message with order 11 and ~drop_duplicate" ;
  let* hashes_4 = inject_int_of_string ~order:11 ~drop_duplicate:true [1; 1] in
  Log.info "Reinjecting the same message with order 0" ;
  let* hashes_5 = inject_int_of_string ~order:0 ~drop_duplicate:true [1] in
  let expected_queued_hashes =
    hashes_1 @ hashes_2 @ hashes_3 @ hashes_4 @ hashes_5
  in

  let expected_messages = [1; 10; 10; 20; 20; 30; 30; 31] in
  let* () = check_queue ~__LOC__ ~expected_queued_hashes ~expected_messages in
  let* () =
    bake_then_check_included_msgs
      ~__LOC__
      ~expected_messages:[expected_messages]
  in

  Log.info
    "Injecting lots of messages to make sure order is preserved over batches, \
     e.g. first batches contains elements smaller than second batch." ;
  let half_min_batch = min_batch_elements / 2 in
  let* hashes_1 =
    (* [0; 10; ... 40] *)
    inject_int_of_string @@ List.init half_min_batch (fun i -> i * 10)
  in
  let* hashes_2 =
    (* [50; 60; ... 80] *)
    inject_int_of_string
    @@ List.init (half_min_batch - 1) (fun i -> (half_min_batch + i) * 10)
  in
  let expected_queued_hashes = hashes_1 @ hashes_2 in
  let expected_messages =
    List.init (min_batch_elements - 1) (fun i -> i * 10)
  in
  let* () = check_queue ~__LOC__ ~expected_queued_hashes ~expected_messages in

  (* this willl trigger batch production *)
  let* _hashes_4 =
    (* [90; 100; ... 290] *)
    inject_int_of_string
      (((min_batch_elements - 1) * 10)
      :: List.init max_batch_elements (fun i -> (min_batch_elements + i) * 10))
  in
  let expected_messages =
    [
      (* [0; 10; ... 190] *)
      List.init max_batch_elements (fun i -> i * 10);
      (* [200; 210; ... 290] *)
      List.init min_batch_elements (fun i -> (max_batch_elements + i) * 10);
    ]
  in
  let* () = bake_then_check_included_msgs ~__LOC__ ~expected_messages in

  Log.info
    "Injecting multiple time the mimimal batches element in reverse order of \
     priority to make sure the injector order them correctly" ;
  (* [0; 1; ... 9] *)
  let messages_no_order = List.init min_batch_elements Fun.id in
  let* _hashes = inject_int_of_string messages_no_order in
  (* [10; 11; ... 19] *)
  let messages_order_2 =
    List.init min_batch_elements (fun i -> min_batch_elements + i)
  in
  let* _hashes = inject_int_of_string ~order:2 messages_order_2 in
  (* [20; 21; ... 29] *)
  let messages_order_1 =
    List.init min_batch_elements (fun i -> (2 * min_batch_elements) + i)
  in
  let* _hashes = inject_int_of_string ~order:1 messages_order_1 in
  let expected_messages =
    [messages_order_1; messages_order_2; messages_no_order]
  in
  let* () = bake_then_check_included_msgs ~__LOC__ ~expected_messages in
  Log.info "Testing the batcher clear RPCs." ;

  Log.info "Clearing messages with order <= 2 in the batcher queue." ;
  let* hash_1 = inject_int_of_string ~order:1 ~drop_duplicate:true [1] in
  let* hash_2 = inject_int_of_string ~order:2 ~drop_duplicate:true [2] in
  let* hash_3 = inject_int_of_string ~order:3 ~drop_duplicate:true [3] in
  let expected_queued_hashes = hash_1 @ hash_2 @ hash_3 in
  let expected_messages = [1; 2; 3] in
  let* () = check_queue ~__LOC__ ~expected_queued_hashes ~expected_messages in
  let* () =
    Sc_rollup_node.RPC.call rollup_node
    @@ Sc_rollup_rpc.delete_admin_batcher_queue
         ~drop_no_order:false
         ~order_below:2
         ()
  in
  let expected_queued_hashes = hash_3 in
  let expected_messages = [3] in
  let* () = check_queue ~__LOC__ ~expected_queued_hashes ~expected_messages in

  Log.info
    "Clearing messages with order <= 2 in the batcher queue or no order \
     specified." ;
  let* hash_1 = inject_int_of_string ~order:1 ~drop_duplicate:true [1] in
  let* hash_2 = inject_int_of_string ~order:2 ~drop_duplicate:true [2] in
  (* [3] is still in the batcher *)
  let* hash_4 = inject_int_of_string (* no order *) ~drop_duplicate:true [4] in
  let expected_queued_hashes = hash_1 @ hash_2 @ hash_3 @ hash_4 in
  let expected_messages = [1; 2; 3; 4] in
  let* () = check_queue ~__LOC__ ~expected_queued_hashes ~expected_messages in
  let* () =
    Sc_rollup_node.RPC.call rollup_node
    @@ Sc_rollup_rpc.delete_admin_batcher_queue
         ~drop_no_order:true
         ~order_below:2
         ()
  in
  let expected_queued_hashes = hash_3 in
  let expected_messages = [3] in
  let* () = check_queue ~__LOC__ ~expected_queued_hashes ~expected_messages in

  Log.info "Clearing all the messages in the batcher queue." ;
  let* hashes =
    inject_int_of_string ~order:1 ~drop_duplicate:true
    @@ List.init half_min_batch Fun.id
  in
  let expected_queued_hashes = hashes in
  let expected_messages = List.init half_min_batch Fun.id in
  let* () = check_queue ~__LOC__ ~expected_queued_hashes ~expected_messages in

  let* () =
    Sc_rollup_node.RPC.call rollup_node
    @@ Sc_rollup_rpc.delete_admin_batcher_queue ()
  in
  let* () =
    check_queue ~__LOC__ ~expected_queued_hashes:[] ~expected_messages:[]
  in

  Log.info "Testing the injector clear RPCs." ;

  Log.info "Clearing all operations in the injector queue." ;
  (*enough messages to create an operations *)
  let messages = List.init min_batch_elements Fun.id in
  let* _hashes_1 = inject_int_of_string messages in

  (*nothing left in the batcher*)
  let* () =
    check_queue ~__LOC__ ~expected_queued_hashes:[] ~expected_messages:[]
  in
  let expected_add_message_op = [messages] in
  let* () = check_injector_queues ~__LOC__ ~expected_add_message_op in
  let* () =
    Sc_rollup_node.RPC.call rollup_node
    @@ Sc_rollup_rpc.delete_admin_injector_queues ()
  in
  let* () = check_injector_queues ~__LOC__ ~expected_add_message_op:[] in

  Log.info "Clearing operations with order <= 1 ." ;
  let messages_1 = messages in
  let messages_2 =
    List.init min_batch_elements (fun i -> min_batch_elements + i)
  in
  let messages_3 =
    List.init min_batch_elements (fun i -> (2 * min_batch_elements) + i)
  in

  let* _hashes = inject_int_of_string ~order:0 messages_1 in
  let* _hashes = inject_int_of_string ~order:1 messages_2 in
  let* _hashes = inject_int_of_string ~order:2 messages_3 in

  let expected_add_message_op = [messages_1; messages_2; messages_3] in
  let* () = check_injector_queues ~__LOC__ ~expected_add_message_op in
  let* () =
    Sc_rollup_node.RPC.call rollup_node
    @@ Sc_rollup_rpc.delete_admin_injector_queues ~order_below:1 ()
  in
  let expected_add_message_op =
    (* strictly below order messages are cleared *) [messages_2; messages_3]
  in
  let* () = check_injector_queues ~__LOC__ ~expected_add_message_op in

  Log.info "Clearing operations with order <= 1 and no order specified ." ;
  let messages_no_order = messages in
  let* _hashes_1 = inject_int_of_string @@ messages_no_order in

  let expected_add_message_op = [messages_2; messages_3; messages_no_order] in
  let* () = check_injector_queues ~__LOC__ ~expected_add_message_op in
  let* () =
    Sc_rollup_node.RPC.call rollup_node
    @@ Sc_rollup_rpc.delete_admin_injector_queues
         ~order_below:2
         ~drop_no_order:true
         ()
  in
  let expected_add_message_op = [messages_3] in

  let* () = check_injector_queues ~__LOC__ ~expected_add_message_op in
  unit

let test_injector_order_operations_by_kind ~kind =
  let commitment_period = 5 in
  let challenge_window = 5 in
  test_full_scenario
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6650

     cf multiple_batcher_test comment. *)
    ~rpc_external:false
    ~kind
    ~commitment_period
    ~challenge_window
    ~mode:Operator
    {
      variant = None;
      tags = ["injector"; "operations"; "order"];
      description = "Injector order operations by kind";
    }
  @@ fun _protocol rollup_node rollup_addr _node client ->
  let* () = Sc_rollup_node.run ~event_level:`Debug rollup_node rollup_addr [] in
  let* _ = Client.bake_for_and_wait client in
  (* to be 1 block after the origination, otherwise it fails for
     something we don't care *)
  let check_sr_ops_are_ordered () =
    let* block = Client.RPC.call client @@ RPC.get_chain_block () in
    let ops = JSON.(block |-> "operations" |=> 3 |> as_list) in
    let ops_kind =
      let open JSON in
      let filter_sr_op json =
        let kind = json |-> "kind" |> as_string in
        if String.starts_with ~prefix:"smart_rollup" kind then Some kind
        else None
      in
      List.map
        (fun op -> op |-> "contents" |> as_list |> List.filter_map filter_sr_op)
        ops
      |> List.flatten
    in
    let all =
      [
        ["smart_rollup_timeout"];
        ["smart_rollup_refute"];
        ["smart_rollup_publish"; "smart_rollup_cement"];
        ["smart_rollup_recover"];
        ["smart_rollup_add_messages"];
        ["smart_rollup_execute_outbox_message"];
      ]
    in
    let is_sorted =
      let rec aux all l =
        match (all, l) with
        | _, hd :: rest when not (String.starts_with ~prefix:"smart_rollup" hd)
          ->
            (*skip op not smart rollup*)
            aux all rest
        | current :: all_rest, hd :: rest ->
            if List.mem hd current then aux all rest else aux all_rest l
        | _, [] -> (* all element of l appeared in all in the same order *) true
        | [], _ ->
            (* There is still at least an element of L but the
               sorted list is empty. L was not correctly
               ordered. *)
            false
      in
      aux all
    in
    Check.is_true
      ~__LOC__
      ~error_msg:
        (Format.asprintf
           "injected operations are not sorted, [%a]"
           Format.(
             pp_print_list
               ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
               pp_print_string)
           ops_kind)
    @@ is_sorted ops_kind ;
    unit
  in

  let hook i =
    let* () =
      (* check done at each block so we don't miss any *)
      check_sr_ops_are_ordered ()
    in
    let* _ =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.post_local_batcher_injection
           ~messages:[string_of_int i]
           ()
    in
    unit
  in
  (* We only check add_messages, publish and cement operations
     order. We could do more but test becomes more involved and I
     think it's not necessary. *)
  let* level =
    bake_until_lpc_updated
      ~at_least:(commitment_period + 2)
      ~hook
      client
      rollup_node
  in
  let* _ =
    bake_until_lcc_updated
      ~at_least:(challenge_window + 2)
      ~hook
      client
      rollup_node
      ~level
  in
  unit

(** Injector only uses key that have no operation in the mempool
   currently.
   1. Batcher setup:
   - 5 signers in the batcher.
   - 10 batches to inject_int_of_string.
   2. First Block:
   - Mempool:
     - Contains enough batches to fill the block from users with high fees.
     - Includes 5 batches injected by the batcher.
   - Block contents:
     - Only contains batches with high fees.
   3. Second Block:
   - Mempool:
     - Has 5 batches injected by the batcher.
   - Block contents:
     - Contains batches.
   - Processing:
     - Node re-injects 5 batches during block processing.
   4.Third Block:
   - Mempool:
     - Still has 5 batches injected by the batcher.
   - Block contents:
     - Contains batches.
*)
let test_injector_uses_available_keys ~kind =
  let operators =
    List.map
      (fun k -> (Sc_rollup_node.Batching, k.Account.public_key_hash))
      Constant.[bootstrap1; bootstrap2; bootstrap3; bootstrap4; bootstrap5]
  in
  let operators_pkh = List.map snd operators in
  let nb_operators = List.length operators in
  test_full_scenario
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6650

     cf multiple_batcher_test comment. *)
    ~rpc_external:false
    ~kind
    ~operators
    ~mode:Batcher
    {
      variant = None;
      tags = ["injector"; "keys"];
      description = "injector uses only available signers";
    }
  @@ fun _protocol rollup_node rollup_addr node client ->
  Log.info "Batcher setup" ;
  let* () = Sc_rollup_node.run ~event_level:`Debug rollup_node rollup_addr [] in
  let batch ~msg_per_batch ~msg_size =
    let msg_cpt = ref 0 in
    (* create batch with different payloads so the logs show
       differents messages. *)
    List.init msg_per_batch (fun _ ->
        msg_cpt := !msg_cpt + 1 ;
        String.make msg_size @@ Char.chr (96 + !msg_cpt))
  in
  let batch_str ~msg_per_batch ~msg_size =
    let batch = batch ~msg_per_batch ~msg_size in
    let json = `A (List.map (fun s -> `String s) batch) in
    "text:" ^ Ezjsonm.to_string json
  in
  let reveal_key keys =
    Lwt_list.iter_p
      (fun key -> Runnable.run @@ Client.reveal ~src:key.Account.alias client)
      keys
  in
  let inject_with_keys keys ~msg_per_batch ~msg_size =
    Lwt_list.iter_p
      (fun key ->
        Client.Sc_rollup.send_message
          ~src:key.Account.alias
          ~fee:(Tez.of_int 10)
          ~fee_cap:(Tez.of_int 10)
          ~msg:(batch_str ~msg_per_batch ~msg_size)
          client)
      keys
  in
  let inject_n_msgs_batches_in_rollup_node ~nb_of_batches ~msg_per_batch
      ~msg_size =
    let* _ids =
      Lwt.all @@ List.init nb_of_batches
      @@ fun _ ->
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.post_local_batcher_injection
           ~messages:(batch ~msg_per_batch ~msg_size)
           ()
    in
    unit
  in
  let find_map_op_content op_content_json =
    let kind = JSON.(op_content_json |-> "kind" |> as_string) in
    if kind = "smart_rollup_add_messages" then
      let src = JSON.(op_content_json |-> "source" |> as_string) in
      Some src
    else None
  in
  let wait_for_get_messages_and_get_batches_pkhs () =
    let* level, block =
      Sc_rollup_node.wait_for
        ~timeout:10.
        rollup_node
        "smart_rollup_node_layer_1_get_messages.v0"
      @@ fun json ->
      let hash = JSON.(json |-> "hash" |> as_string) in
      let level = JSON.(json |-> "level" |> as_int32) in
      Some (level, hash)
    in
    let* l = map_manager_op_from_block node ~block ~find_map_op_content in
    return (level, l)
  in
  let wait_twice_for_included_and_get_batches_pkhs () =
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let get_op_content p =
      let* level, block = p in
      let* l = map_manager_op_from_block node ~block ~find_map_op_content in
      return (level, l)
    in
    let seen_blocks = ref None in
    let listener =
      Sc_rollup_node.wait_for rollup_node "included.v0" ~timeout:20.
      @@ fun json ->
      let level = JSON.(json |-> "level" |> as_int32) in
      let block = JSON.(json |-> "block" |> as_string) in
      let inj_ops = JSON.(json |-> "operations" |> as_list) in
      (* Only count unique blocks where the injector included operations.
         Note: The injector emits one "included.v0" event per operation ID,
         so when multiple operations are included in the same block, we get
         multiple events for that block. We deduplicate by tracking seen blocks. *)
      if inj_ops <> [] && not (Some block = !seen_blocks) then
        if Option.is_none !seen_blocks then (
          seen_blocks := Some block ;
          Lwt.wakeup_later r1 (level, block) ;
          None (* Keep listening for 2nd unique block *))
        else (
          Lwt.wakeup_later r2 (level, block) ;
          Some () (* Stop listening after 2nd unique block *))
      else None
      (* Skip events with no operations or duplicate blocks *)
    in
    (* The listener is exposed outside the scope of the function, otherwise the
       timeout on [wait_for] is never catched and the test might run
       indefinitely. *)
    (listener, get_op_content p1, get_op_content p2)
  in
  Log.info "Checking that the batcher keys received are correct." ;
  let check_keys ~received ~expected =
    let sorted_expected = List.sort String.compare expected in
    let sorted_received = List.sort String.compare received in
    Check.(
      (sorted_received = sorted_expected)
        (list string)
        ~error_msg:"%L found where %R was expected)")
  in
  (* nb_of_keys * msg_per_batch * msg_size = expected_block_size
     16 * 8 * 4000 = 512_000 = maximum size of Tezos L1 block *)
  let nb_of_keys = 16 and msg_per_batch = 8 and msg_size = 4000 in
  let* keys = gen_keys_then_transfer_tez client nb_of_keys in
  let keys_pkh = List.map (fun k -> k.Account.public_key_hash) keys in
  let* () = reveal_key keys in
  (* Arm listeners for the inclusion of the batches. *)
  let ( listener_promise,
        first_rollup_batch_inclusion_promise,
        second_rollup_batch_inclusion_promise ) =
    wait_twice_for_included_and_get_batches_pkhs ()
  in
  (* test start here *)
  let* () = Client.bake_for_and_wait client in
  let* () =
    inject_n_msgs_batches_in_rollup_node
    (* we inject_int_of_string 2 times the number of operators so the rollup node
       must inject_int_of_string in two salvos all the batches. *)
      ~nb_of_batches:(2 * nb_operators)
      ~msg_per_batch
      ~msg_size
  in
  let* () = Client.bake_for_and_wait client
  and* () =
    wait_until_n_batches_are_injected rollup_node ~nb_batches:nb_operators
  in
  let second_injection_promise =
    wait_until_n_batches_are_injected rollup_node ~nb_batches:nb_operators
  in
  Log.info "Inject enough batches to fill the block." ;
  let* () = inject_with_keys keys ~msg_per_batch ~msg_size in
  let user_batch_promise = wait_for_get_messages_and_get_batches_pkhs () in
  Log.info "We now bake 3 blocks." ;
  let* () =
    repeat 3 (fun () ->
        let* () = Client.bake_for_and_wait client in
        Lwt_unix.sleep 0.5)
  in
  (* The run features the expected operations*)
  let* () = second_injection_promise and* () = listener_promise in

  (* We now check that all batches are found. *)
  Log.info
    "One block contains the batches injected by the rollup node simultaneously \
     with direct injection." ;
  let* level_first_rollup_batch, used_pkhs =
    first_rollup_batch_inclusion_promise
  in
  check_keys ~received:used_pkhs ~expected:operators_pkh ;
  Log.info "Another one contains the batches injected manually to the L1." ;
  let* level_manual_injection, used_pkhs = user_batch_promise in
  check_keys ~received:used_pkhs ~expected:keys_pkh ;
  Log.info
    "Last block contains batches found are those injected by the rollup node \
     using keys that have been utilized in block up to this point." ;
  let* level_second_rollup_batch, used_pkhs =
    second_rollup_batch_inclusion_promise
  in
  check_keys ~received:used_pkhs ~expected:operators_pkh ;

  (* And check that all levels are different. *)
  Log.info "Checking that all levels are different" ;
  Check.((level_manual_injection <> level_first_rollup_batch) int32)
    ~error_msg:
      "Since batches are supposed to fill a block, the manually injected batch \
       and the first batch from the rollup are expected to be included at the \
       different levels" ;
  Check.((level_manual_injection <> level_second_rollup_batch) int32)
    ~error_msg:
      "Since batches are supposed to fill a block, the manually injected batch \
       and the second batch from the rollup are expected to be included at the \
       different levels" ;
  Check.((level_second_rollup_batch <> level_first_rollup_batch) int32)
    ~error_msg:
      "Since batches are supposed to fill a block, the batches from the rollup \
       are expected to be included at the different levels" ;
  unit

let test_batcher_dont_reinject_already_injected_messages ~kind =
  test_full_scenario
    ~kind
    ~mode:Batcher
    {
      variant = None;
      tags = ["batcher"; "drop_duplicate"];
      description = "batcher don't reinject messages that were already injected";
    }
  @@ fun _protocol rollup_node rollup_addr _node client ->
  Log.info "Batcher setup" ;
  let inject_int_of_string ~drop_duplicate ~messages =
    Sc_rollup_node.RPC.call rollup_node
    @@ Sc_rollup_rpc.post_local_batcher_injection ~drop_duplicate ~messages ()
  in
  let check_batcher_queue_size ?__LOC__ ~expected_size () =
    let* batcher_message =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_local_batcher_queue ()
    in
    Check.(
      (List.length batcher_message = expected_size)
        ?__LOC__
        int
        ~error_msg:
          "There must be exactly %R messages in the batcher queue, found %L.") ;
    unit
  in

  let check_message_injection_status ?__LOC__ ~expected_status ~message_id () =
    let* _, status =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_local_batcher_queue_msg_id ~msg_id:message_id
    in
    Check.(
      (status = expected_status)
        string
        ~error_msg:"expected injection status %R, found %L"
        ?__LOC__) ;
    unit
  in

  let messages = ["msg"; "msg"] in

  let bake_inject_check ?__LOC__ ?(inject_msgs = true) ~check_message_id
      ~expected_status ~expected_size () =
    let wait_for_total_injected =
      Sc_rollup_node.wait_for
        rollup_node
        "total_injected_ops.v0"
        (Fun.const (Some ()))
    in
    let* () = Client.bake_for_and_wait client
    and* () = wait_for_total_injected in

    let* () =
      if inject_msgs then (
        let* message_ids =
          inject_int_of_string ~drop_duplicate:true ~messages
        in
        let message_id = List.nth message_ids 0 in
        Check.(
          (message_id = check_message_id)
            ?__LOC__
            string
            ~error_msg:
              "messages id when unique is false must be equal (given %L, \
               expected %R).") ;
        unit)
      else unit
    in
    let* () =
      check_message_injection_status
        ?__LOC__
        ~expected_status
        ~message_id:check_message_id
        ()
    in
    let* () = check_batcher_queue_size ?__LOC__ ~expected_size () in
    unit
  in

  let* () =
    Sc_rollup_node.run
      ~event_level:`Debug
      rollup_node
      rollup_addr
      [Injector_retention_period 1]
  and* _lvl = Sc_rollup_node.wait_sync rollup_node ~timeout:10. in

  let* messages_id_first =
    inject_int_of_string ~drop_duplicate:false ~messages
  in
  let* () = check_batcher_queue_size ~__LOC__ ~expected_size:2 () in
  let* messages_id_second =
    inject_int_of_string ~drop_duplicate:false ~messages
  in
  let* () = check_batcher_queue_size ~__LOC__ ~expected_size:4 () in
  Check.(
    (messages_id_first <> messages_id_second)
      ~__LOC__
      (list string)
      ~error_msg:"first messages batch id %L must be different than second %R") ;

  let* first_message_ids =
    inject_int_of_string ~drop_duplicate:true ~messages
  in
  let* () = check_batcher_queue_size ~__LOC__ ~expected_size:5 () in

  let check_message_id = List.nth first_message_ids 0 in

  Check.(
    (check_message_id = List.nth first_message_ids 1)
      ~__LOC__
      string
      ~error_msg:
        "messages id with check on must be equal (given %L, expected %R).") ;

  Log.info "Resubmiting the same message dont add it to the queue." ;
  let* second_message_ids =
    inject_int_of_string ~drop_duplicate:true ~messages
  in
  let* () = check_batcher_queue_size ~__LOC__ ~expected_size:5 () in
  Check.(
    (first_message_ids = second_message_ids)
      ~__LOC__
      (list string)
      ~error_msg:
        "Same messages injected with injection check must have same ids \
         returned (first %L, second %R).") ;

  let* () =
    check_message_injection_status
      ~__LOC__
      ~expected_status:"pending_batch"
      ~message_id:check_message_id
      ()
  in

  let* () =
    bake_inject_check
      ~__LOC__
      ~check_message_id
      ~expected_status:"injected"
      ~expected_size:0
      ()
  in
  let* () =
    repeat
      3
      (* 2 blocks for the op to be finalized
         + 1 block for the retention period.*)
      (bake_inject_check
         ~__LOC__
         ~check_message_id
         ~expected_status:"included"
         ~expected_size:0)
  in
  let* () =
    bake_inject_check
      ~__LOC__
      ~check_message_id
      ~inject_msgs:false
        (* after the retention period is over we have no more info
           about the op *)
      ~expected_status:"unknown"
      ~expected_size:0
      ()
  in
  let* () =
    bake_inject_check
      ~__LOC__
      ~check_message_id
      ~expected_status:"pending_batch"
      ~expected_size:1
      ()
  in
  unit

let start_rollup_node_with_encrypted_key ~kind =
  test_l1_scenario
    ~hooks
    ~kind
    {
      variant = None;
      tags = ["rollup_node"];
      description = "start a rollup node with an encrypted key";
    }
    ~uses:(fun _protocol -> [Constant.octez_smart_rollup_node])
  @@ fun _protocol sc_rollup node client ->
  let encrypted_account =
    {
      alias = "encrypted_account";
      public_key_hash = "";
      public_key = "";
      Account.secret_key =
        Encrypted
          "edesk1n2uGpPtVaeyhWkZzTEcaPRzkQHrqkw5pk8VkZvp3rM5KSc3mYNH5cJEuNcfB91B3G3JakKzfLQSmrgF4ht";
    }
  in
  let password = "password" in
  let* () =
    let Account.{alias; secret_key; _} = encrypted_account in
    Client.import_encrypted_secret_key client ~alias secret_key ~password
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.(of_int 1)
      ~amount:(Tez.of_int 20_000)
      ~giver:Constant.bootstrap1.alias
      ~receiver:encrypted_account.alias
      client
  in
  let* _ = Client.bake_for_and_wait client in
  let rollup_node =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~kind
      ~default_operator:encrypted_account.alias
  in
  let* () = Sc_rollup_node.run ~wait_ready:false rollup_node sc_rollup [] in
  let* () =
    repeat 3 (fun () ->
        Sc_rollup_node.write_in_stdin rollup_node "invalid_password")
  and* () =
    Sc_rollup_node.check_error
      rollup_node
      ~exit_code:1
      ~msg:(rex "3 incorrect password attempts")
  in
  let password_file = Filename.temp_file "password_file" "" in
  let () = write_file password_file ~contents:password in
  let* () =
    Sc_rollup_node.run ~restart:true ~password_file rollup_node sc_rollup []
  in
  unit

let test_patch_durable_storage_on_commitment =
  let kind = "wasm_2_0_0" in
  test_full_scenario
    ~commitment_period:2
    ~timeout:120
    ~kind
    {
      tags = ["patch"; "durable_storage"];
      variant = None;
      description = "Patch durable storage fails on commitment";
    }
  @@ fun _protocol sc_rollup_node sc_rollup _node client ->
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* _ = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  (* Stop the rollup node. *)
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  (* This will fail as there is a commitment for that level. *)
  let* () =
    Lwt.catch
      (fun () ->
        Sc_rollup_node.patch_durable_storage
          sc_rollup_node
          ~key:"/foo"
          ~value:"00")
      (fun _exn -> unit)
  in
  (* Restart the node and bake a block. *)
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* () = Client.bake_for_and_wait client in
  let* _ = Sc_rollup_node.wait_sync sc_rollup_node ~timeout:30. in
  (* It will now work. *)
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () =
    Sc_rollup_node.patch_durable_storage sc_rollup_node ~key:"/foo" ~value:"00"
  in
  unit

let register_riscv ~protocols =
  let kind = "riscv" in
  let boot_sector =
    read_riscv_kernel
      (Uses.make ~tag:"riscv" ~path:"src/riscv/assets/riscv-dummy.elf" ())
      (Uses.make
         ~tag:"riscv"
         ~path:"src/riscv/assets/riscv-dummy.elf.checksum"
         ())
  in
  let preimages_dir =
    Uses.make ~tag:kind ~path:"src/riscv/assets/preimages" ()
  in
  test_origination ~kind ~boot_sector protocols ;
  test_rpcs ~kind ~boot_sector protocols ~kernel_debug_log:true ~preimages_dir ;
  test_rollup_node_boots_into_initial_state protocols ~kind ;
  test_rollup_node_advances_pvm_state
    protocols
    ~kind
    ~boot_sector
    ~internal:false
    ~kernel_debug_log:true
    ~preimages_dir

let register_riscv_kernel ~protocols ~kernel =
  let kind = "riscv" in
  let variant, inbox_file =
    match kernel with
    | "jstz" ->
        (* The Jstz inbox is generated using the inbox-bench tool from https://github.com/tezos/riscv-pvm
         * `inbox-bench generate --inbox-file jstz-inbox.json --transfers 1 --address sr1N6iTfzhj2iGYfxACdy5kgHX5qzuW6xubY` *)
        ("Jstz", "jstz-inbox.json")
    | "etherlink" ->
        (* The Etherlink inbox is generated using a version of the Etherlink benchmark tool
         *  modified to address messages to sr1SboL6bEDznwdMkk2pV4i5t7L1iVHnEzDX:
         *  `node etherlink/kernel_latest/benchmarks/scripts/benchmarks/bench_linear_erc20.js --count 1` *)
        ("Etherlink", "etherlink-inbox.json")
    | _ -> failwith ("Unknown kernel: " ^ kernel)
  in
  let inbox_path = "tezt/tests/riscv-tests/" ^ inbox_file in
  let boot_sector =
    read_riscv_kernel
      (Uses.make ~tag:"riscv" ~path:("src/riscv/assets/" ^ kernel) ())
      (Uses.make
         ~tag:"riscv"
         ~path:("src/riscv/assets/" ^ kernel ^ ".checksum")
         ())
  in
  let inbox_file_uses = Uses.make ~tag:"riscv" ~path:inbox_path () in
  test_advances_state_with_kernel
    protocols
    ~kind
    ~title:("node advances PVM state with messages (" ^ variant ^ ")")
    ~boot_sector
    ~inbox_file:inbox_file_uses ;
  (* The refutation game scenario is too long to enable in merge pipelines and currently
   * consumes too much memory to run as a regular slow test.
   * It can be manually run as part of the `tezt-riscv-slow-sequential` job. *)
  test_refutation_scenario
    ~kind
    ~ci_disabled:true
    ~extra_tags:["riscv_slow_sequential"]
    ~mode:Operator
    ~challenge_window:400
    ~timeout:400
    ~timestamp:(Ago (Client.Time.Span.of_seconds_exn 200.))
      (* Setting the timestamp results in blocks being produced more slowly *)
    ~commitment_period:10
    ~variant
    ~uses:(fun _protocol -> [inbox_file_uses])
    ~boot_sector
    (refutation_scenario_parameters
       ~loser_modes:["5 0 1000"]
       (List.map (fun x -> [x]) (read_riscv_test_inbox inbox_file_uses))
       ~input_format:`Hex
       ~final_level:500
       ~priority:`No_priority)
    protocols

let register ~kind ~protocols =
  test_origination ~kind protocols ;
  test_rollup_get_genesis_info ~kind protocols ;
  test_rpcs ~kind protocols ;
  test_commitment_scenario
    ~variant:"commitment_is_stored"
    commitment_stored
    protocols
    ~kind ;
  test_commitment_scenario
    ~variant:"robust_to_failures"
    commitment_stored_robust_to_failures
    protocols
    ~kind ;
  test_commitment_scenario
    ~extra_tags:["modes"; "observer"]
    ~variant:"observer_does_not_publish"
    (mode_publish Observer false)
    protocols
    ~kind ;
  test_commitment_scenario
    ~extra_tags:["modes"; "maintenance"]
    ~variant:"maintenance_publishes"
    (mode_publish Maintenance true)
    protocols
    ~kind ;
  test_commitment_scenario
    ~extra_tags:["modes"; "batcher"]
    ~variant:"batcher_does_not_publish"
    (mode_publish Batcher false)
    protocols
    ~kind ;
  test_commitment_scenario
    ~extra_tags:["modes"; "operator"]
    ~variant:"operator_publishes"
    (mode_publish Operator true)
    protocols
    ~kind ;
  test_commitment_scenario
    ~commitment_period:15
    ~challenge_window:10080
    ~variant:"node_use_proto_param"
    commitment_stored
    protocols
    ~kind ;
  test_commitment_scenario
    ~variant:"non_final_level"
    ~extra_tags:[Tag.flaky]
    commitment_not_published_if_non_final
    protocols
    ~kind ;
  test_commitment_scenario
    ~variant:"messages_reset"
    (commitments_messages_reset kind)
    protocols
    ~kind ;
  test_commitment_scenario
    ~variant:"handles_chain_reorgs"
    (commitments_reorgs ~kind ~switch_l1_node:false)
    protocols
    ~kind ;
  test_commitment_scenario
    ~variant:"handles_chain_reorgs_missing_blocks"
    (commitments_reorgs ~kind ~switch_l1_node:true)
    protocols
    ~kind ;
  test_commitment_scenario
    ~commitment_period:3
    ~variant:"correct_commitment_in_reproposal_reorg"
    ~extra_tags:["reproposal"]
    commitments_reproposal
    protocols
    ~kind ;
  test_commitment_scenario
    ~challenge_window:1
    ~variant:"no_commitment_publish_before_lcc"
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/2976
       change tests so that we do not need to repeat custom parameters. *)
    commitment_before_lcc_not_published
    protocols
    ~kind ;
  test_commitment_scenario
    ~variant:"first_published_at_level_global"
    first_published_level_is_global
    protocols
    ~kind ;
  test_commitment_scenario
  (* Reduce commitment period here in order avoid waiting for default 30 (and even 60) blocks to be baked*)
    ~commitment_period:3
    ~variant:"consecutive commitments"
    test_consecutive_commitments
    protocols
    ~kind ;
  test_outbox_message protocols ~kind ;
  test_unsafe_genesis_patch protocols ~private_:true ~kind ;
  test_unsafe_genesis_patch protocols ~private_:false ~kind

let register ~protocols =
  (* PVM-independent tests. We still need to specify a PVM kind
     because the tezt will need to originate a rollup. However,
     the tezt will not test for PVM kind specific features. *)
  test_rollup_list protocols ~kind:"wasm_2_0_0" ;
  test_valid_dispute_dissection ~kind:"arith" protocols ;
  test_refutation_reward_and_punishment protocols ~kind:"arith" ;
  test_timeout ~kind:"arith" protocols ;
  test_no_cementation_if_parent_not_lcc_or_if_disputed_commit
    ~kind:"arith"
    protocols ;
  test_refutation protocols ~kind:"arith" ;
  test_refutation protocols ~kind:"wasm_2_0_0" ;
  test_recover_bond_of_stakers protocols ;
  test_patch_durable_storage_on_commitment protocols ;
  (* Specific Arith PVM tezts *)
  test_rollup_origination_boot_sector
    ~boot_sector:"10 10 10 + +"
    ~kind:"arith"
    protocols ;
  test_boot_sector_is_evaluated
    ~boot_sector1:"10 10 10 + +"
    ~boot_sector2:"31"
    ~kind:"arith"
    protocols ;
  test_reveals_4k protocols ;
  test_reveals_above_4k protocols ;
  test_reveals_fetch_remote protocols ;
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

  (* Specific RISC-V PVM tezts *)
  register_riscv ~protocols:[Protocol.Alpha] ;
  register_riscv_kernel ~protocols:[Protocol.Alpha] ~kernel:"jstz" ;
  register_riscv_kernel ~protocols:[Protocol.Alpha] ~kernel:"etherlink" ;
  (* Shared tezts - will be executed for each PVMs. *)
  register ~kind:"wasm_2_0_0" ~protocols ;
  register ~kind:"arith" ~protocols ;
  (* Both Arith and Wasm PVM tezts *)
  test_bootstrap_smart_rollup_originated protocols ;
  test_bootstrap_private_smart_rollup_originated protocols ;
  (* Private rollup node *)
  test_private_rollup_whitelisted_staker protocols ;
  test_private_rollup_non_whitelisted_staker protocols ;
  test_rollup_whitelist_update ~kind:"wasm_2_0_0" protocols ;
  test_rollup_whitelist_outdated_update ~kind:"wasm_2_0_0" protocols

let register_protocol_independent () =
  let protocols = Protocol.[Alpha] in
  test_list_metrics_command_regression ~disable_performance_metrics:true () ;
  test_list_metrics_command_regression ~disable_performance_metrics:false () ;
  test_store_schema_regression protocols ;
  let with_kind kind =
    test_rollup_node_running ~kind protocols ;
    test_rollup_node_boots_into_initial_state protocols ~kind ;
    test_rollup_node_advances_pvm_state protocols ~kind ~internal:false ;
    test_rollup_node_advances_pvm_state protocols ~kind ~internal:true
  in
  with_kind "wasm_2_0_0" ;
  with_kind "arith" ;
  let kind = "wasm_2_0_0" in
  start_rollup_node_with_encrypted_key protocols ~kind ;
  test_rollup_node_missing_preimage_exit_at_initialisation protocols ~kind ;
  test_rollup_node_configuration protocols ~kind ;
  test_client_wallet protocols ~kind ;
  test_reveals_fails_on_wrong_hash protocols ;
  test_reveals_fails_on_unknown_hash protocols ;
  test_injector_auto_discard protocols ;
  test_accuser protocols ;
  test_invalid_dal_parameters protocols ;
  test_refutation_with_dal_page_import protocols ;
  test_refutation_with_dal_page_import_id_far_in_the_future protocols ;
  test_bailout_refutation protocols ;
  test_multiple_batcher_key ~kind protocols ;
  test_batcher_order_msgs ~kind protocols ;
  test_injector_uses_available_keys protocols ~kind ;
  test_injector_order_operations_by_kind protocols ~kind ;
  test_batcher_dont_reinject_already_injected_messages protocols ~kind ;
  test_private_rollup_node_publish_in_whitelist protocols ;
  test_private_rollup_node_publish_not_in_whitelist protocols ;
  test_can_stake protocols ~kind ;
  test_rollup_node_inbox
    ~kind
    ~variant:"stops"
    sc_rollup_node_stops_scenario
    protocols ;
  test_rollup_node_inbox
    ~kind
    ~variant:"disconnects"
    sc_rollup_node_disconnects_scenario
    protocols ;
  test_rollup_node_inbox
    ~kind
    ~variant:"handles_chain_reorg"
    sc_rollup_node_handles_chain_reorg
    protocols ;
  test_rollup_node_inbox
    ~kind
    ~variant:"batcher"
    ~extra_tags:["batcher"; Tag.flaky]
    sc_rollup_node_batcher
    protocols ;
  test_rollup_node_inbox ~kind ~variant:"basic" basic_scenario protocols ;
  test_outbox_message_reorg ~kind protocols ;
  test_outbox_message_reorg_disappear ~kind protocols ;
  test_gc
    "many_gc"
    ~kind
    ~challenge_window:5
    ~commitment_period:2
    ~history_mode:Full
    protocols ;
  test_gc
    "sparse_gc"
    ~kind
    ~challenge_window:10
    ~commitment_period:5
    ~history_mode:Full
    protocols ;
  test_gc
    "no_gc"
    ~kind
    ~challenge_window:10
    ~commitment_period:5
    ~history_mode:Archive
    protocols ;
  test_snapshots
    ~kind
    ~challenge_window:10
    ~commitment_period:10
    ~history_mode:Full
    ~compact:false
    protocols ;
  test_snapshots
    ~kind
    ~challenge_window:10
    ~commitment_period:10
    ~history_mode:Full
    ~compact:true
    protocols ;
  test_snapshots
    ~kind
    ~challenge_window:10
    ~commitment_period:10
    ~history_mode:Archive
    ~compact:false
    protocols ;
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7835
        Re-enable when unsafe patches are properly part of
        snapshot.

     test_snapshots
       ~unsafe_pvm_patches:true
       ~kind
       ~challenge_window:10
       ~commitment_period:10
       ~history_mode:Archive
       ~compact:false
       protocols ;
  *)
  custom_mode_empty_operation_kinds ~kind protocols ;
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4373
     Uncomment this test as soon as the issue done.
     test_reinject_failed_commitment protocols ~kind ; *)
  test_late_rollup_node protocols ~kind ;
  test_late_rollup_node_2 protocols ~kind ;
  test_interrupt_rollup_node protocols ~kind ;
  test_arg_boot_sector_file ~kind protocols ;
  test_messages_processed_by_commitment ~kind protocols ;
  bailout_mode_not_publish ~kind protocols ;
  bailout_mode_fail_to_start_without_operator ~kind protocols ;
  bailout_mode_fail_operator_no_stake ~kind protocols ;
  bailout_mode_recover_bond_starting_no_commitment_staked ~kind protocols ;
  test_remote_signer ~hardcoded_remote_signer:true ~kind protocols ;
  test_remote_signer ~hardcoded_remote_signer:false ~kind protocols
