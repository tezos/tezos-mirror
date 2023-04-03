(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

let default_wasm_pvm_revision = function
  | Protocol.Alpha -> "2.0.0-r1"
  | Protocol.Lima | Protocol.Mumbai -> "2.0.0"

let assert_some_client_command cmd ~__LOC__ ?hooks client =
  let*! v_opt = cmd ?hooks client in
  match v_opt with
  | Some v -> return v
  | None -> failwith (Format.asprintf "Unexpected [None] at %s" __LOC__)

let get_last_stored_commitment =
  assert_some_client_command Sc_rollup_client.last_stored_commitment

let get_last_published_commitment =
  assert_some_client_command Sc_rollup_client.last_published_commitment

(* Number of levels needed to process a head as finalized. This value should
   be the same as `node_context.block_finality_time`, where `node_context` is
   the `Node_context.t` used by the rollup node. For Tenderbake, the
   block finality time is 2. *)
let block_finality_time = 2

let reveal_hash_hex data =
  let hash =
    Tezos_crypto.Blake2B.(hash_string [data] |> to_string) |> hex_encode
  in
  "00" ^ hash

let reveal_hash_b58_mumbai_arith data =
  Tezos_protocol_016_PtMumbai.Protocol.Sc_rollup_reveal_hash.(
    hash_string ~scheme:Blake2B [data] |> to_b58check)

type reveal_hash = {message : string; filename : string}

let reveal_hash ~protocol ~kind data =
  let hex_hash = reveal_hash_hex data in
  match (protocol, kind) with
  | Protocol.Mumbai, "arith" ->
      (* Only for arith PVM, the message should have the form "hash:scrrh1..."  *)
      {
        message = "hash:" ^ reveal_hash_b58_mumbai_arith data;
        filename = hex_hash;
      }
  | _, "arith" -> {message = "hash:" ^ hex_hash; filename = hex_hash}
  | _, _ ->
      (* Not used for wasm yet. *)
      assert false

type sc_rollup_constants = {
  origination_size : int;
  challenge_window_in_blocks : int;
  stake_amount : Tez.t;
  commitment_period_in_blocks : int;
  max_lookahead_in_blocks : int32;
  max_active_outbox_levels : int32;
  max_outbox_messages_per_level : int;
  number_of_sections_in_dissection : int;
  timeout_period_in_blocks : int;
}

let get_sc_rollup_constants client =
  let* json =
    RPC.Client.call client @@ RPC.get_chain_block_context_constants ()
  in
  let open JSON in
  let origination_size = json |-> "smart_rollup_origination_size" |> as_int in
  let challenge_window_in_blocks =
    json |-> "smart_rollup_challenge_window_in_blocks" |> as_int
  in
  let stake_amount =
    json |-> "smart_rollup_stake_amount" |> as_string |> Int64.of_string
    |> Tez.of_mutez_int64
  in
  let commitment_period_in_blocks =
    json |-> "smart_rollup_commitment_period_in_blocks" |> as_int
  in
  let max_lookahead_in_blocks =
    json |-> "smart_rollup_max_lookahead_in_blocks" |> as_int32
  in
  let max_active_outbox_levels =
    json |-> "smart_rollup_max_active_outbox_levels" |> as_int32
  in
  let max_outbox_messages_per_level =
    json |-> "smart_rollup_max_outbox_messages_per_level" |> as_int
  in
  let number_of_sections_in_dissection =
    json |-> "smart_rollup_number_of_sections_in_dissection" |> as_int
  in
  let timeout_period_in_blocks =
    json |-> "smart_rollup_timeout_period_in_blocks" |> as_int
  in
  return
    {
      origination_size;
      challenge_window_in_blocks;
      stake_amount;
      commitment_period_in_blocks;
      max_lookahead_in_blocks;
      max_active_outbox_levels;
      max_outbox_messages_per_level;
      number_of_sections_in_dissection;
      timeout_period_in_blocks;
    }

let originate_forward_smart_contract ?(src = Constant.bootstrap1.alias) client
    protocol =
  (* Originate forwarder contract to send internal messages to rollup *)
  let* alias, contract_id =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 1)
      client
      ["mini_scenarios"; "sc_rollup_forward"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  Log.info
    "The forwarder %s (%s) contract was successfully originated"
    alias
    contract_id ;
  return contract_id

(* List of scoru errors messages used in tests below. *)

let commit_too_recent =
  "Attempted to cement a commitment before its refutation deadline"

let disputed_commit = "Attempted to cement a disputed commitment"

let register_test ?supports ?(regression = false) ~__FILE__ ~tags ~title f =
  let tags = "sc_rollup" :: tags in
  if regression then
    Protocol.register_regression_test ?supports ~__FILE__ ~title ~tags f
  else Protocol.register_test ?supports ~__FILE__ ~title ~tags f

let get_sc_rollup_commitment_period_in_blocks client =
  let* constants = get_sc_rollup_constants client in
  return constants.commitment_period_in_blocks

let sc_rollup_node_rpc sc_node service =
  let url = Printf.sprintf "%s/%s" (Sc_rollup_node.endpoint sc_node) service in
  let*! response = RPC.Curl.get url in
  return response

type test = {variant : string option; tags : string list; description : string}

let originate_sc_rollups ~kind n client =
  fold n String_set.empty (fun _i addrs ->
      let* addr = originate_sc_rollup ~kind client in
      return (String_set.add addr addrs))

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

(** Wait for the rollup node to detect a conflict *)
let wait_for_conflict_detected sc_node =
  Sc_rollup_node.wait_for sc_node "sc_rollup_node_conflict_detected.v0"
  @@ fun json ->
  let our_hash = JSON.(json |-> "our_commitment_hash" |> as_string) in
  Some (our_hash, json)

(** Wait for the rollup node to detect a timeout *)
let wait_for_timeout_detected sc_node =
  Sc_rollup_node.wait_for sc_node "sc_rollup_node_timeout_detected.v0"
  @@ fun json ->
  let other = JSON.(json |> as_string) in
  Some other

(** Wait for the rollup node to compute a dissection *)
let wait_for_computed_dissection sc_node =
  Sc_rollup_node.wait_for sc_node "sc_rollup_node_computed_dissection.v0"
  @@ fun json ->
  let opponent = JSON.(json |-> "opponent" |> as_string) in
  Some (opponent, json)

(* Configuration of a rollup node
   ------------------------------

   A rollup node has a configuration file that must be initialized.
*)
let setup_rollup ~protocol ~kind ?hooks ?(mode = Sc_rollup_node.Operator)
    ?boot_sector ?(parameters_ty = "string")
    ?(operator = Constant.bootstrap1.alias) ?data_dir tezos_node tezos_client =
  let* sc_rollup =
    originate_sc_rollup
      ?hooks
      ~kind
      ?boot_sector
      ~parameters_ty
      ~src:operator
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~protocol
      mode
      tezos_node
      ?data_dir
      ~base_dir:(Client.base_dir tezos_client)
      ~default_operator:operator
  in
  let rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
  return (sc_rollup_node, rollup_client, sc_rollup)

let format_title_scenario kind {variant; tags = _; description} =
  Printf.sprintf
    "%s - %s%s"
    kind
    description
    (match variant with Some variant -> " (" ^ variant ^ ")" | None -> "")

let test_l1_scenario ?regression ?hooks ~kind ?boot_sector ?commitment_period
    ?challenge_window ?timeout ?(src = Constant.bootstrap1.alias)
    {variant; tags; description} scenario =
  let tags = kind :: tags in
  register_test
    ?regression
    ~__FILE__
    ~tags
    ~title:(format_title_scenario kind {variant; tags; description})
  @@ fun protocol ->
  let* tezos_node, tezos_client =
    setup_l1 ?commitment_period ?challenge_window ?timeout protocol
  in
  let* sc_rollup =
    originate_sc_rollup ?hooks ~kind ?boot_sector ~src tezos_client
  in
  scenario sc_rollup tezos_node tezos_client

let test_l1_migration_scenario ?parameters_ty ?(src = Constant.bootstrap1.alias)
    ?variant ?(tags = []) ~kind ~migrate_from ~migrate_to ~scenario_prior
    ~scenario_after ~description () =
  let tags = kind :: "migration" :: tags in
  Test.register
    ~__FILE__
    ~tags
    ~title:(format_title_scenario kind {variant; tags; description})
  @@ fun () ->
  let* tezos_node, tezos_client =
    setup_l1 ~commitment_period:10 ~challenge_window:10 ~timeout:10 migrate_from
  in
  let* sc_rollup = originate_sc_rollup ?parameters_ty ~kind ~src tezos_client in
  let* prior_res = scenario_prior tezos_client ~sc_rollup in
  let current_level = Node.get_level tezos_node in
  let migration_level = current_level + 1 in
  let* () = Node.terminate tezos_node in
  let patch_config =
    Node.Config_file.set_sandbox_network_with_user_activated_upgrades
      [(migration_level, migrate_to)]
  in
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* () = Node.run ~patch_config tezos_node nodes_args in
  let* () = Node.wait_for_ready tezos_node in
  let* () =
    repeat migration_level (fun () -> Client.bake_for_and_wait tezos_client)
  in
  let* () = scenario_after tezos_client ~sc_rollup prior_res in
  unit

let test_full_scenario ?supports ?regression ?hooks ~kind ?mode ?boot_sector
    ?commitment_period ?(parameters_ty = "string") ?challenge_window ?timeout
    {variant; tags; description} scenario =
  let tags = kind :: "rollup_node" :: tags in
  register_test
    ?supports
    ?regression
    ~__FILE__
    ~tags
    ~title:(format_title_scenario kind {variant; tags; description})
  @@ fun protocol ->
  let* tezos_node, tezos_client =
    setup_l1 ?commitment_period ?challenge_window ?timeout protocol
  in
  let* rollup_node, rollup_client, sc_rollup =
    setup_rollup
      ~protocol
      ~parameters_ty
      ~kind
      ?hooks
      ?mode
      ?boot_sector
      tezos_node
      tezos_client
  in
  scenario protocol rollup_node rollup_client sc_rollup tezos_node tezos_client

let commitment_info_inbox_level
    (commitment_info : Sc_rollup_client.commitment_info) =
  commitment_info.commitment_and_hash.commitment.inbox_level

let commitment_info_predecessor
    (commitment_info : Sc_rollup_client.commitment_info) =
  commitment_info.commitment_and_hash.commitment.predecessor

let commitment_info_commitment
    (commitment_info : Sc_rollup_client.commitment_info) =
  commitment_info.commitment_and_hash.commitment

let commitment_info_hash (commitment_info : Sc_rollup_client.commitment_info) =
  commitment_info.commitment_and_hash.hash

let commitment_info_first_published_at_level
    (commitment_info : Sc_rollup_client.commitment_info) =
  commitment_info.first_published_at_level

let last_cemented_commitment_hash_with_level ~sc_rollup client =
  let* json =
    RPC.Client.call client
    @@ RPC
       .get_chain_block_context_smart_rollups_smart_rollup_last_cemented_commitment_hash_with_level
         sc_rollup
  in
  let hash = JSON.(json |-> "hash" |> as_string) in
  let level = JSON.(json |-> "level" |> as_int) in
  return (hash, level)

let get_staked_on_commitment ~sc_rollup ~staker client =
  let* json =
    RPC.Client.call client
    @@ RPC
       .get_chain_block_context_smart_rollups_smart_rollup_staker_staked_on_commitment
         ~sc_rollup
         staker
  in
  match JSON.(json |-> "hash" |> as_string_opt) with
  | Some hash -> return hash
  | None -> failwith (Format.sprintf "hash is missing %s" __LOC__)

let cement_commitment ?(src = Constant.bootstrap1.alias) ?fail ~sc_rollup ~hash
    client =
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
  test_l1_scenario
    ~regression:true
    ~hooks
    {
      variant = None;
      tags = ["origination"];
      description = "origination of a SCORU executes without error";
    }
    ~kind
    (fun _ _ _ -> unit)

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
  @@ fun protocol rollup_node _rollup_client sc_rollup tezos_node tezos_client
    ->
  let* _filename = Sc_rollup_node.config_init rollup_node sc_rollup in
  let config = Sc_rollup_node.Config_file.read rollup_node in
  let _rpc_port = JSON.(config |-> "rpc-port" |> as_int) in
  let data_dir = Sc_rollup_node.data_dir rollup_node in
  (* Run the rollup node to initialize store and context *)
  let* () = Sc_rollup_node.run rollup_node sc_rollup [] in
  let* () = Sc_rollup_node.terminate rollup_node in
  (* Run a rollup node in the same data_dir, but for a different rollup *)
  let* other_rollup_node, _rollup_client, other_sc_rollup =
    setup_rollup ~protocol ~kind tezos_node tezos_client ~data_dir
  in
  let expect_failure () =
    match Sc_rollup_node.process other_rollup_node with
    | None -> unit
    | Some p ->
        Process.check_error
          ~exit_code:1
          ~msg:(rex "This rollup node was already set up for rollup")
          p
  in
  let run_promise =
    let* () = Sc_rollup_node.run other_rollup_node other_sc_rollup [] in
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
  @@ fun _protocol rollup_node rollup_client sc_rollup _tezos_node _tezos_client
    ->
  let metrics_port = string_of_int (Port.fresh ()) in
  let metrics_addr = "localhost:" ^ metrics_port in
  let* () =
    Sc_rollup_node.run rollup_node sc_rollup ["--metrics-addr"; metrics_addr]
  in
  let* sc_rollup_from_rpc =
    sc_rollup_node_rpc rollup_node "global/smart_rollup_address"
  in
  let sc_rollup_from_rpc = JSON.as_string sc_rollup_from_rpc in
  let*! sc_rollup_from_client =
    Sc_rollup_client.sc_rollup_address ~hooks rollup_client
  in
  if sc_rollup_from_rpc <> sc_rollup then
    failwith
      (Printf.sprintf
         "Expecting %s, got %s when we query the sc rollup node RPC address"
         sc_rollup
         sc_rollup_from_rpc)
  else if sc_rollup_from_client <> sc_rollup then
    failwith
      (Printf.sprintf
         "Expecting %s, got %s when the client asks for the sc rollup address"
         sc_rollup
         sc_rollup_from_client)
  else
    let url = "http://" ^ metrics_addr ^ "/metrics" in
    let*! metrics = RPC.Curl.get_raw url in
    let regexp = Str.regexp "\\(#HELP.*\n.*#TYPE.*\n.*\\)+" in
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
  @@ fun sc_rollup tezos_node tezos_client ->
  let origination_level = Node.get_level tezos_node in
  (* Bake 10 blocks to be sure that the origination_level of rollup is different
     from the level of the head node. *)
  let* () = repeat 10 (fun () -> Client.bake_for_and_wait tezos_client) in
  let* hash, level =
    last_cemented_commitment_hash_with_level ~sc_rollup tezos_client
  in
  let* genesis_info =
    RPC.Client.call tezos_client
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

(* Wait for the [injecting_pending] event from the injector. *)
let wait_for_injecting_event ?(tags = []) ?count node =
  Sc_rollup_node.wait_for node "injecting_pending.v0" @@ fun json ->
  let event_tags = JSON.(json |-> "tags" |> as_list |> List.map as_string) in
  let event_count = JSON.(json |-> "count" |> as_int) in
  match count with
  | Some c when c <> event_count -> None
  | _ ->
      if List.for_all (fun t -> List.mem t event_tags) tags then
        Some event_count
      else None

(* Wait for the [sc_rollup_node_publish_commitment] event from the rollup node. *)
let wait_for_publish_commitment node =
  Sc_rollup_node.wait_for node "sc_rollup_node_publish_commitment.v0"
  @@ fun json ->
  let hash = JSON.(json |-> "hash" |> as_string) in
  let level = JSON.(json |-> "level" |> as_int) in
  Some (hash, level)

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

(* Pushing message in the inbox
   ----------------------------

   A message can be pushed to a smart-contract rollup inbox through
   the Tezos node. Then we can observe that the messages are included in the
   inbox.
*)
let send_message_client ?hooks ?(src = Constant.bootstrap2.alias) client msg =
  let* () = Client.Sc_rollup.send_message ?hooks ~src ~msg client in
  Client.bake_for_and_wait client

let send_messages_client ?hooks ?src ?batch_size n client =
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
  Lwt_list.iter_s
    (fun msg -> send_message_client ?hooks ?src client msg)
    messages

let send_message_batcher_aux ?hooks client sc_node sc_client msgs =
  let batched =
    Sc_rollup_node.wait_for sc_node "batched.v0" (Fun.const (Some ()))
  in
  let added_to_injector =
    Sc_rollup_node.wait_for sc_node "add_pending.v0" (Fun.const (Some ()))
  in
  let injected = wait_for_injecting_event ~tags:["add_messages"] sc_node in
  let*! hashes = Sc_rollup_client.inject ?hooks sc_client msgs in
  (* New head will trigger injection  *)
  let* () = Client.bake_for_and_wait client in
  (* Injector should get messages right away because the batcher is configured
     to not have minima. *)
  let* _ = batched in
  let* _ = added_to_injector in
  let* _ = injected in
  return hashes

let send_message_batcher ?hooks client sc_node sc_client msgs =
  let* hashes = send_message_batcher_aux ?hooks client sc_node sc_client msgs in
  (* Next head will include messages  *)
  let* () = Client.bake_for_and_wait client in
  return hashes

let send_messages_batcher ?hooks ?batch_size n client sc_node sc_client =
  let batches =
    List.map
      (fun i ->
        let batch_size = match batch_size with None -> i | Some v -> v in
        List.map (fun j -> Format.sprintf "%d-%d" i j) (range 1 batch_size))
      (range 1 n)
  in
  let* rhashes =
    Lwt_list.fold_left_s
      (fun acc msgs ->
        let* hashes =
          send_message_batcher_aux ?hooks client sc_node sc_client msgs
        in
        return (List.rev_append hashes acc))
      []
      batches
  in
  (* Next head will include messages of last batch *)
  let* () = Client.bake_for_and_wait client in
  return (List.rev rhashes)

let send_message = send_message_client

let send_messages = send_messages_client

let to_text_messages_arg msgs =
  let json = Ezjsonm.list Ezjsonm.string msgs in
  "text:" ^ Ezjsonm.to_string ~minify:true json

let to_hex_messages_arg msgs =
  let json = Ezjsonm.list Ezjsonm.string msgs in
  "hex:" ^ Ezjsonm.to_string ~minify:true json

let send_text_messages ?(format = `Raw) ?hooks ?src client msgs =
  match format with
  | `Raw -> send_message ?hooks ?src client (to_text_messages_arg msgs)
  | `Hex -> send_message ?hooks ?src client (to_hex_messages_arg msgs)

let parse_inbox json =
  let go () =
    return JSON.(json |-> "old_levels_messages", json |-> "level" |> as_int)
  in
  Lwt.catch go @@ fun exn ->
  failwith
    (Printf.sprintf
       "Unable to parse inbox %s\n%s"
       (JSON.encode json)
       (Printexc.to_string exn))

let get_inbox_from_tezos_node client =
  let* inbox =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_smart_rollups_all_inbox ()
  in
  parse_inbox inbox

let get_inbox_from_sc_rollup_node sc_rollup_node =
  let* inbox = sc_rollup_node_rpc sc_rollup_node "global/block/head/inbox" in
  parse_inbox inbox

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
let test_rollup_inbox_of_rollup_node ?(extra_tags = []) ~variant scenario ~kind
    =
  test_full_scenario
    {
      variant = Some variant;
      tags = ["inbox"] @ extra_tags;
      description = "maintenance of inbox in the rollup node";
    }
    ~kind
  @@ fun _protocol sc_rollup_node sc_rollup_client sc_rollup node client ->
  let* () = scenario sc_rollup_node sc_rollup_client sc_rollup node client in
  let* inbox_from_sc_rollup_node =
    get_inbox_from_sc_rollup_node sc_rollup_node
  in
  let* inbox_from_tezos_node = get_inbox_from_tezos_node client in
  return
  @@ Check.(
       (inbox_from_sc_rollup_node = inbox_from_tezos_node)
         (tuple2 json int)
         ~error_msg:"expected value %R, got %L")

let basic_scenario sc_rollup_node _rollup_client sc_rollup _node client =
  let num_messages = 2 in
  let expected_level =
    (* We start at level 2 and each message also bakes a block. With 2 messages being sent, we
       must end up at level 4. *)
    4
  in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* () = send_messages num_messages client in
  let* _ =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node expected_level
  in
  unit

let sc_rollup_node_stops_scenario sc_rollup_node _rollup_client sc_rollup _node
    client =
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

let sc_rollup_node_disconnects_scenario sc_rollup_node _rollup_client sc_rollup
    node client =
  let num_messages = 2 in
  let level = Node.get_level node in
  Log.info "we are at level %d" level ;
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* () = send_messages num_messages client in
  let* level =
    Sc_rollup_node.wait_for_level sc_rollup_node (Node.get_level node)
  in
  let* () = Lwt_unix.sleep 1. in
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
  unit

let sc_rollup_node_handles_chain_reorg sc_rollup_node _rollup_client sc_rollup
    node client =
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

let bake_until ?hook cond n client =
  assert (0 <= n) ;
  let rec go i =
    if 0 < i then
      let* cond = cond client in
      if cond then
        let* () = match hook with None -> unit | Some hook -> hook (n - i) in
        let* () = Client.bake_for_and_wait client in
        go (i - 1)
      else return ()
    else return ()
  in
  go n

let bake_levels ?hook n client =
  fold n () @@ fun i () ->
  let* () = match hook with None -> unit | Some hook -> hook i in
  Client.bake_for_and_wait client

(* Rollup node batcher *)
let sc_rollup_node_batcher sc_rollup_node sc_rollup_client sc_rollup node client
    =
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  Log.info "Sending one message to the batcher" ;
  let msg1 = "3 3 + out" in
  let*! hashes = Sc_rollup_client.inject sc_rollup_client [msg1] in
  let msg1_hash = match hashes with [h] -> h | _ -> assert false in
  let*! retrieved_msg1, status_msg1 =
    Sc_rollup_client.get_batcher_msg sc_rollup_client msg1_hash
  in
  let check_status response status =
    Check.((JSON.(response |-> "status" |> as_string) = status) string)
      ~error_msg:"Status of message  is %L but expected %R."
  in
  check_status status_msg1 "pending_batch" ;
  Check.((retrieved_msg1 = msg1) string)
    ~error_msg:"Message in queue is %L but injected %R." ;
  let*! queue = Sc_rollup_client.batcher_queue sc_rollup_client in
  Check.((queue = [(msg1_hash, msg1)]) (list (tuple2 string string)))
    ~error_msg:"Queue is %L but should be %R." ;
  (* This block triggers injection in the injector. *)
  let injected =
    wait_for_injecting_event ~tags:["add_messages"] sc_rollup_node
  in
  let* () = Client.bake_for_and_wait client in
  let* _ = injected in
  let*! _msg1, status_msg1 =
    Sc_rollup_client.get_batcher_msg sc_rollup_client msg1_hash
  in
  check_status status_msg1 "injected" ;
  (* We bake so that msg1 is included. *)
  let* () = Client.bake_for_and_wait client in
  let*! _msg1, status_msg1 =
    Sc_rollup_client.get_batcher_msg sc_rollup_client msg1_hash
  in
  check_status status_msg1 "included" ;
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (Node.get_level node)
  in
  Log.info "Sending multiple messages to the batcher" ;
  let msg2 =
    (* "012456789 012456789 012456789 ..." *)
    String.init 2048 (fun i ->
        let i = i mod 11 in
        if i = 10 then ' ' else Char.chr (i + 48))
  in
  let*! hashes1 =
    Sc_rollup_client.inject sc_rollup_client (List.init 9 (Fun.const msg2))
  in
  let* hashes2 =
    send_message_batcher
      client
      sc_rollup_node
      sc_rollup_client
      (List.init 9 (Fun.const msg2))
  in
  let*! queue = Sc_rollup_client.batcher_queue sc_rollup_client in
  Check.((queue = []) (list (tuple2 string string)))
    ~error_msg:"Queue is %L should be %empty R." ;
  let* block = RPC.Client.call client @@ RPC.get_chain_block () in
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
  let* block = RPC.Client.call client @@ RPC.get_chain_block () in
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
  Check.((incl_count = List.length hashes1 + List.length hashes2) int)
    ~error_msg:"Only %L messages are included instead of %R." ;
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let levels =
    levels_to_commitment + init_level - Node.get_level node
    + block_finality_time + 1
  in
  Log.info "Baking %d blocks for commitment of first message" levels ;
  let* () = bake_levels levels client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (Node.get_level node)
  in
  let*! _msg1, status_msg1 =
    Sc_rollup_client.get_batcher_msg sc_rollup_client msg1_hash
  in
  check_status status_msg1 "committed" ;
  unit

(* One can retrieve the list of originated SCORUs.
   -----------------------------------------------
*)

let test_rollup_list ~kind =
  register_test
    ~__FILE__
    ~tags:["sc_rollup"; "list"]
    ~title:"list originated rollups"
  @@ fun protocol ->
  let* _node, client = setup_l1 protocol in
  let* rollups =
    RPC.Client.call client @@ RPC.get_chain_block_context_smart_rollups_all ()
  in
  let rollups = JSON.as_list rollups in
  let () =
    match rollups with
    | _ :: _ ->
        failwith "Expected initial list of originated SCORUs to be empty"
    | [] -> ()
  in
  let* scoru_addresses = originate_sc_rollups ~kind 10 client in
  let* () = Client.bake_for_and_wait client in
  let* rollups =
    RPC.Client.call client @@ RPC.get_chain_block_context_smart_rollups_all ()
  in
  let rollups =
    JSON.as_list rollups |> List.map JSON.as_string |> String_set.of_list
  in
  Check.(
    (rollups = scoru_addresses)
      (comparable_module (module String_set))
      ~error_msg:"%L %R") ;
  unit

(* Make sure the rollup node boots into the initial state.
   -------------------------------------------------------

   When a rollup node starts, we want to make sure that in the absence of
   messages it will boot into the initial state.
*)
let test_rollup_node_boots_into_initial_state ~kind =
  test_full_scenario
    {
      variant = None;
      tags = ["bootstrap"];
      description = "rollup node boots into the initial state";
    }
    ~kind
  @@ fun _protocol sc_rollup_node sc_rollup_client sc_rollup _node client ->
  let* genesis_info =
    RPC.Client.call ~hooks client
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

  let*! ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
  Check.(ticks = 0)
    Check.int
    ~error_msg:"Unexpected initial tick count (%L = %R)" ;

  let*! status = Sc_rollup_client.status ~hooks sc_rollup_client in
  let expected_status =
    match kind with
    | "arith" -> "Halted"
    | "wasm_2_0_0" -> "Waiting for input message"
    | _ -> raise (Invalid_argument kind)
  in
  Check.(status = expected_status)
    Check.string
    ~error_msg:"Unexpected PVM status (%L = %R)" ;
  unit

let test_rollup_node_advances_pvm_state ?regression ~title ?boot_sector
    ~internal ~kind =
  test_full_scenario
    ?regression
    ~hooks
    {
      variant = Some (if internal then "internal" else "external");
      tags = ["pvm"];
      description = title;
    }
    ?boot_sector
    ~parameters_ty:"bytes"
    ~kind
  @@ fun protocol sc_rollup_node sc_rollup_client sc_rollup _tezos_node client
    ->
  let* genesis_info =
    RPC.Client.call ~hooks client
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
  let* level, forwarder =
    if not internal then return (level, None)
    else
      let* contract_id = originate_forward_smart_contract client protocol in
      return (level + 1, Some contract_id)
  in
  (* Called with monotonically increasing [i] *)
  let test_message i =
    let*! prev_state_hash =
      Sc_rollup_client.state_hash ~hooks sc_rollup_client
    in
    let*! prev_ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
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
    let* (_ : int) =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node (level + i)
    in

    (* specific per kind PVM checks *)
    let* () =
      match kind with
      | "arith" ->
          let*! encoded_value =
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
          unit
      | "wasm_2_0_0" ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/3729

              Add an appropriate check for various test kernels

                computation.wasm               - Gets into eval state
                no_parse_random.wasm           - Stuck state due to parse error
                no_parse_bad_fingerprint.wasm  - Stuck state due to parse error
          *)
          unit
      | _otherwise -> raise (Invalid_argument kind)
    in

    let*! state_hash = Sc_rollup_client.state_hash ~hooks sc_rollup_client in
    Check.(state_hash <> prev_state_hash)
      Check.string
      ~error_msg:"State hash has not changed (%L <> %R)" ;

    let*! ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
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
let test_rollup_node_advances_pvm_state ~kind ?boot_sector ~internal =
  test_rollup_node_advances_pvm_state
    ~regression:true
    ~title:"node advances PVM state with messages"
    ?boot_sector
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
  let* json =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_commitment
         ~sc_rollup
         ~hash:commitment_hash
         ()
  in
  return (Sc_rollup_client.commitment_from_json json)

let check_published_commitment_in_l1 ?(allow_non_published = false)
    ?(force_new_level = true) sc_rollup client published_commitment =
  let* () =
    if force_new_level then
      (* Triggers injection into the L1 context *)
      bake_levels 1 client
    else unit
  in
  let* commitment_in_l1 =
    match published_commitment with
    | None ->
        if not allow_non_published then
          Test.fail "No commitment has been published" ;
        Lwt.return_none
    | Some Sc_rollup_client.{commitment_and_hash = {hash; _}; _} ->
        tezos_client_get_commitment client sc_rollup hash
  in
  let published_commitment =
    Option.map commitment_info_commitment published_commitment
  in
  check_commitment_eq
    (commitment_in_l1, "in L1")
    (published_commitment, "published") ;
  unit

let test_commitment_scenario ?commitment_period ?challenge_window
    ?(extra_tags = []) ~variant =
  test_full_scenario
    ?commitment_period
    ?challenge_window
    {
      tags = ["commitment"] @ extra_tags;
      variant = Some variant;
      description = "rollup node - correct handling of commitments";
    }

let commitment_stored _protocol sc_rollup_node sc_rollup_client sc_rollup _node
    client =
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
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let store_commitment_level =
    init_level + levels_to_commitment + block_finality_time
  in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
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
  let* {
         commitment = {inbox_level = stored_inbox_level; _} as stored_commitment;
         hash = _;
       } =
    get_last_stored_commitment ~__LOC__ ~hooks sc_rollup_client
  in
  Check.(stored_inbox_level = levels_to_commitment + init_level)
    Check.int
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  (* Bake one level for commitment to be included *)
  let* () = Client.bake_for_and_wait client in
  let*! published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  check_commitment_eq
    (Option.some stored_commitment, "stored")
    (Option.map commitment_info_commitment published_commitment, "published") ;
  check_published_commitment_in_l1 sc_rollup client published_commitment

let mode_publish mode publishes protocol sc_rollup_node sc_rollup_client
    sc_rollup node client =
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
  let level = Node.get_level node in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node level in
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
      ~protocol
      mode
      node'
      ~base_dir:(Client.base_dir client')
      ~operators
      ~default_operator:Constant.bootstrap3.alias
  in
  let sc_rollup_other_client =
    Sc_rollup_client.create ~protocol sc_rollup_other_node
  in
  let* () = Sc_rollup_node.run sc_rollup_other_node sc_rollup [] in
  let* _level = Sc_rollup_node.wait_for_level sc_rollup_other_node level in
  Log.info "Other rollup node synchronized." ;
  let* () = send_messages levels_to_commitment client in
  let level = Node.get_level node in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node level
  and* _ = Sc_rollup_node.wait_for_level sc_rollup_other_node level in
  Log.info "Both rollup nodes have reached level %d." level ;
  let state_hash = Sc_rollup_client.state_hash ~hooks sc_rollup_client
  and state_hash_other =
    Sc_rollup_client.state_hash ~hooks sc_rollup_other_client
  in
  let*! state_hash in
  let*! state_hash_other in
  Check.((state_hash = state_hash_other) string)
    ~error_msg:
      "State hash of other rollup node is %R but the first rollup node has %L" ;
  let*! published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  let*! other_published_commitment =
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

let commitment_not_published_if_non_final _protocol sc_rollup_node
    sc_rollup_client sc_rollup _node client =
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
    RPC.Client.call ~hooks client
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
    get_last_stored_commitment ~__LOC__ ~hooks sc_rollup_client
  in
  Check.(stored_inbox_level = store_commitment_level)
    Check.int
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  let*! commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  let commitment_info_inbox_level =
    Option.map commitment_info_inbox_level commitment
  in
  Check.(commitment_info_inbox_level = None)
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been published at a level different than expected (%L = \
       %R)" ;
  unit

let commitments_messages_reset kind _protocol sc_rollup_node sc_rollup_client
    sc_rollup node client =
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
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
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
  let* {
         commitment =
           {
             inbox_level = stored_inbox_level;
             number_of_ticks = stored_number_of_ticks;
             _;
           } as stored_commitment;
         hash = _;
       } =
    get_last_stored_commitment ~__LOC__ ~hooks sc_rollup_client
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
         * 11_000_000_000 (* number of ticks in a snapshots *)
         * levels_to_commitment (* number of inboxes *)
     | _ -> failwith "incorrect kind"
   in
   Check.(stored_number_of_ticks = expected)
     Check.int
     ~error_msg:
       "Number of ticks processed by commitment is different from the number \
        of ticks expected (%L = %R)") ;
  let* () = Client.bake_for_and_wait client in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node (Node.get_level node) in
  let*! published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  check_commitment_eq
    (Option.some stored_commitment, "stored")
    (Option.map commitment_info_commitment published_commitment, "published") ;
  check_published_commitment_in_l1 sc_rollup client published_commitment

let commitment_stored_robust_to_failures protocol sc_rollup_node
    sc_rollup_client sc_rollup node client =
  (* This test uses two rollup nodes for the same rollup, tracking the same L1 node.
     Both nodes process heads from the L1. However, the second node is stopped
     one level before publishing a commitment, and then is restarted.
     We should not observe any difference in the commitments stored by the
     two rollup nodes.
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client
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
      ~protocol
      Operator
      node
      ~base_dir:(Client.base_dir client')
      ~default_operator:bootstrap2_key
  in
  let sc_rollup_client' = Sc_rollup_client.create ~protocol sc_rollup_node' in
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
    get_last_stored_commitment ~__LOC__ ~hooks sc_rollup_client
  in
  let* {commitment = stored_commitment'; hash = _} =
    get_last_stored_commitment ~__LOC__ ~hooks sc_rollup_client'
  in
  check_commitment_eq
    (Some stored_commitment, "stored in first node")
    (Some stored_commitment', "stored in second node") ;
  unit

let commitments_reorgs ~switch_l1_node ~kind _protocol sc_rollup_node
    sc_rollup_client sc_rollup node client =
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

  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
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
  let* client, node =
    if switch_l1_node then (
      (* Switch the L1 node of a rollup node so that reverted blocks are not *)
      (* available in the new L1 node. *)
      Log.info "Changing L1 node for rollup node" ;
      let* () =
        Sc_rollup_node.change_node_and_restart sc_rollup_node sc_rollup node'
      in
      return (client', node'))
    else
      let* () = trigger_reorg () in
      return (client, node)
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
    get_last_stored_commitment ~__LOC__ ~hooks sc_rollup_client
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
         let snapshot_ticks = 11_000_000_000 in
         snapshot_ticks * 4
         (* 1 snapshot for collecting messages, 3 snapshots for SOL,
            Info_per_level and SOL *) * levels_to_commitment
         (* Number of inbox that are actually processed process *)
     | _ -> assert false
   in
   Check.(stored_number_of_ticks = expected_number_of_ticks)
     Check.int
     ~error_msg:
       "Number of ticks processed by commitment is different from the number \
        of ticks expected (%L = %R)") ;
  let* () = Client.bake_for_and_wait client in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node (Node.get_level node) in
  let*! published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  check_commitment_eq
    (Option.some stored_commitment, "stored")
    (Option.map commitment_info_commitment published_commitment, "published") ;
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
  fun ?expect_failure
      ~sc_rollup
      ~sc_rollup_stake_amount
      ?(check_liquid_balance = true)
      ?(src = Constant.bootstrap1.public_key_hash)
      ?(staker = Constant.bootstrap1.public_key_hash)
      client ->
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
        let* () =
          Client.bake_for_and_wait ~keys:[Constant.bootstrap2.alias] client
        in
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
let commitment_before_lcc_not_published protocol sc_rollup_node sc_rollup_client
    sc_rollup node client =
  let* constants = get_sc_rollup_constants client in
  let commitment_period = constants.commitment_period_in_blocks in
  let challenge_window = constants.challenge_window_in_blocks in
  (* Rollup node 1 processes messages, produces and publishes two commitments. *)
  let* genesis_info =
    RPC.Client.call ~hooks client
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
  let* () = bake_levels commitment_period client in
  let* commitment_inbox_level =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (init_level + commitment_period)
  in
  (* Bake `block_finality_time` additional level to ensure that block number
     `init_level + sc_rollup_commitment_period_in_blocks` is processed by
     the rollup node as finalized and one additional for commitment inclusion. *)
  let* () = bake_levels (block_finality_time + 1) client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (Node.get_level node)
  in
  let* {commitment = _; hash = rollup_node1_stored_hash} =
    get_last_stored_commitment ~__LOC__ ~hooks sc_rollup_client
  in
  let* rollup_node1_published_commitment =
    get_last_published_commitment ~__LOC__ ~hooks sc_rollup_client
  in
  let () =
    Check.(
      commitment_info_inbox_level rollup_node1_published_commitment
      = commitment_inbox_level)
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
    commitment_info_hash rollup_node1_published_commitment
  in
  let* () = bake_levels levels_to_cementation client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (Node.get_level node)
  in

  (* Withdraw stake before cementing should fail *)
  let* () =
    attempt_withdraw_stake
      ~sc_rollup
      ~sc_rollup_stake_amount:(Tez.to_mutez constants.stake_amount)
      client
      ~expect_failure:
        "Attempted to withdraw while not staked on the last cemented \
         commitment."
  in

  let* () =
    cement_commitment client ~sc_rollup ~hash:cemented_commitment_hash
  in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (Node.get_level node)
  in

  (* Withdraw stake after cementing should succeed *)
  let* () =
    attempt_withdraw_stake
      ~sc_rollup
      ~sc_rollup_stake_amount:(Tez.to_mutez constants.stake_amount)
      client
  in

  let* () = Sc_rollup_node.terminate sc_rollup_node in
  (* Rollup node 2 starts and processes enough levels to publish a commitment.*)
  let bootstrap2_key = Constant.bootstrap2.public_key_hash in
  let* client' = Client.init ?endpoint:(Some (Node node)) () in
  let sc_rollup_node' =
    Sc_rollup_node.create
      ~protocol
      Operator
      node
      ~base_dir:(Client.base_dir client')
      ~default_operator:bootstrap2_key
  in
  let sc_rollup_client' = Sc_rollup_client.create ~protocol sc_rollup_node' in
  let* () = Sc_rollup_node.run sc_rollup_node' sc_rollup [] in

  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node'
      (Node.get_level node)
  in
  (* Check that no commitment was published. *)
  let*! rollup_node2_last_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client'
  in
  let rollup_node2_last_published_commitment_inbox_level =
    Option.map
      commitment_info_inbox_level
      rollup_node2_last_published_commitment
  in
  let () =
    Check.(rollup_node2_last_published_commitment_inbox_level = None)
      (Check.option Check.int)
      ~error_msg:"Commitment has been published by node 2 at %L but shouldn't"
  in
  (* Check that the commitment stored by the second rollup node
     is the same commmitment stored by the first rollup node. *)
  let* {commitment = _; hash = rollup_node2_stored_hash} =
    get_last_stored_commitment ~__LOC__ ~hooks sc_rollup_client'
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
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node'
      (Node.get_level node)
  in
  let* rollup_node2_last_published_commitment =
    get_last_published_commitment ~__LOC__ ~hooks sc_rollup_client'
  in
  let rollup_node2_last_published_commitment_inbox_level =
    commitment_info_inbox_level rollup_node2_last_published_commitment
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
      commitment_info_predecessor rollup_node2_last_published_commitment
      = cemented_commitment_hash)
      Check.string
      ~error_msg:
        "Predecessor fo commitment published by rollup_node2 should be the \
         cemented commitment (%L = %R)"
  in
  unit

(* Test that the level when a commitment was first published is fetched correctly
   by rollup nodes. *)
let first_published_level_is_global protocol sc_rollup_node sc_rollup_client
    sc_rollup node client =
  (* Rollup node 1 processes messages, produces and publishes two commitments. *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* commitment_period = get_sc_rollup_commitment_period_in_blocks client in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
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
  let* () = bake_levels (block_finality_time + 1) client in
  let* _commitment_finalized_level =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node
      (Node.get_level node)
  in
  let* rollup_node1_published_commitment =
    get_last_published_commitment ~__LOC__ ~hooks sc_rollup_client
  in
  Check.(
    commitment_info_inbox_level rollup_node1_published_commitment
    = commitment_inbox_level)
    Check.int
    ~error_msg:
      "Commitment has been published for a level %L different than expected %R" ;
  let commitment_publish_level = Node.get_level node in
  Check.(
    commitment_info_first_published_at_level rollup_node1_published_commitment
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
      ~protocol
      Operator
      node
      ~base_dir:(Client.base_dir client')
      ~default_operator:bootstrap2_key
  in
  let sc_rollup_client' = Sc_rollup_client.create ~protocol sc_rollup_node' in
  let* () = Sc_rollup_node.run sc_rollup_node' sc_rollup [] in

  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node'
      commitment_publish_level
  in
  let*! rollup_node2_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client'
  in
  Check.(
    Option.bind
      rollup_node2_published_commitment
      commitment_info_first_published_at_level
    = None)
    (Check.option Check.int)
    ~error_msg:"Rollup node 2 cannot publish commitment without any new block." ;
  let* () = Client.bake_for_and_wait client in
  let commitment_publish_level2 = Node.get_level node in
  let* _ =
    Sc_rollup_node.wait_for_level sc_rollup_node' commitment_publish_level2
  in
  let* rollup_node2_published_commitment =
    get_last_published_commitment ~__LOC__ ~hooks sc_rollup_client'
  in
  check_commitment_eq
    ( Option.some (commitment_info_commitment rollup_node1_published_commitment),
      "published by rollup node 1" )
    ( Option.some (commitment_info_commitment rollup_node2_published_commitment),
      "published by rollup node 2" ) ;
  let () =
    Check.(
      commitment_info_first_published_at_level rollup_node1_published_commitment
      = commitment_info_first_published_at_level
          rollup_node2_published_commitment)
      (Check.option Check.int)
      ~error_msg:
        "Rollup nodes do not agree on level when commitment was first \
         published (%L = %R)"
  in
  unit

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4373 *)
let _test_reinject_failed_commitment ~protocol ~kind =
  let commitment_period = 3 in
  test_full_scenario
    ~kind
    ~commitment_period
    {
      tags = ["injector"; "reinject"; "failed"];
      variant = None;
      description = "Republish commitments that are included as failed";
    }
  @@ fun _protocol sc_rollup_node1 _sc_rollup_client1 sc_rollup node client ->
  let sc_rollup_node2 =
    Sc_rollup_node.create
      ~protocol
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:Constant.bootstrap5.public_key_hash
  in
  Log.info "Run two honest rollup nodes." ;
  let* () = Sc_rollup_node.run sc_rollup_node1 sc_rollup []
  and* () = Sc_rollup_node.run sc_rollup_node2 sc_rollup [] in
  Log.info "Add messages and advance L1 to trigger commitment." ;
  (* We bake one extra block to allow for the injection of the commitment and
     another block to allow for reinjection of the commitment that has the out
     of gas error. *)
  let retried = ref false in
  let _ =
    let* () =
      Sc_rollup_node.wait_for sc_rollup_node1 "retry_operation.v0" (fun _ ->
          Some ())
    in
    retried := true ;
    unit
  in
  let _ =
    let* () =
      Sc_rollup_node.wait_for sc_rollup_node2 "retry_operation.v0" (fun _ ->
          Some ())
    in
    retried := true ;
    unit
  in
  let* () = bake_levels (commitment_period + block_finality_time + 2) client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node1
      (Node.get_level node)
  and* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:3.
      sc_rollup_node2
      (Node.get_level node)
  in
  Log.info "Rollup nodes both at %d." (Node.get_level node) ;
  (* NOTE: if the gas consumed by publishing commitments is fixed, remove the
     following check. One of the commitments is supposed to be included as
     failed in a block because when two identical commitments are in the same
     block, the second one consumes more gas than the first, and consumes more
     gas than what was simulated. *)
  if not !retried then
    Test.fail "Rollup nodes did not retry to republish commitment" ;
  let* {stake_amount; _} = get_sc_rollup_constants client in
  let check_committed id =
    let* deposit_json =
      RPC.Client.call client
      @@ RPC.get_chain_block_context_contract_frozen_bonds ~id ()
    in
    Check.(
      (deposit_json = stake_amount)
        Tez.typ
        ~error_msg:
          "A node has not committed, expecting deposit for participant = %R, \
           got %L") ;
    unit
  in
  let* () = check_committed Constant.bootstrap1.public_key_hash
  and* () = check_committed Constant.bootstrap5.public_key_hash in
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
  @@ fun _protocol rollup_node rollup_client sc_rollup _tezos_node tezos_client
    ->
  let* genesis_info =
    RPC.Client.call ~hooks tezos_client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let genesis_commitment_hash =
    JSON.(genesis_info |-> "commitment_hash" |> as_string)
  in
  let* init_commitment =
    RPC.Client.call ~hooks tezos_client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_commitment
         ~sc_rollup
         ~hash:genesis_commitment_hash
         ()
  in
  let init_hash = JSON.(init_commitment |-> "compressed_state" |> as_string) in
  let* () = Sc_rollup_node.run rollup_node sc_rollup [] in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:3. rollup_node init_level in
  let*! node_state_hash = Sc_rollup_client.state_hash ~hooks rollup_client in
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
  @@ fun sc_rollup1 _tezos_node tezos_client ->
  let* sc_rollup2 =
    originate_sc_rollup
      ~hooks
      ~kind
      ~boot_sector:boot_sector2
      ~src:Constant.bootstrap2.alias
      tezos_client
  in
  let genesis_state_hash ~sc_rollup tezos_client =
    let* genesis_info =
      RPC.Client.call ~hooks tezos_client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
           sc_rollup
    in
    let commitment_hash =
      JSON.(genesis_info |-> "commitment_hash" |> as_string)
    in
    let* commitment =
      RPC.Client.call ~hooks tezos_client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_commitment
           ~sc_rollup
           ~hash:commitment_hash
           ()
    in
    let state_hash = JSON.(commitment |-> "compressed_state" |> as_string) in
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
  @@ fun protocol sc_rollup_node _sc_rollup_client sc_rollup node client ->
  let hash = reveal_hash ~protocol ~kind "Some data" in
  let pvm_dir = Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) kind in
  let filename = Filename.concat pvm_dir hash.filename in
  let () = Sys.mkdir pvm_dir 0o700 in
  let cout = open_out filename in
  let () = output_string cout "Some data that is not related to the hash" in
  let () = close_out cout in

  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  (* Prepare the handler to wait for the rollup node to fail before
     sending the L1 message that will trigger the failure. This
     ensures that the failure handler can access the status code
     of the rollup node even after it has terminated. *)
  let expect_failure =
    let node_process = Option.get @@ Sc_rollup_node.process sc_rollup_node in
    Process.check_error
      ~exit_code:1
      ~msg:
        (rex
           "The hash of reveal preimage is \\w+ while a value of \\w+ is \
            expected")
      node_process
  in
  let* () = send_text_messages client [hash.message] in
  let should_not_sync =
    let* _level =
      Sc_rollup_node.wait_for_level
        ~timeout:10.
        sc_rollup_node
        (Node.get_level node)
    in
    Test.fail "The rollup node processed the incorrect reveal without failing"
  in
  Lwt.choose [expect_failure; should_not_sync]

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
  @@ fun protocol sc_rollup_node _sc_rollup_client sc_rollup node client ->
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
    let* _level =
      Sc_rollup_node.wait_for_level
        ~timeout:10.
        sc_rollup_node
        (Node.get_level node)
    in
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
  @@ fun protocol sc_rollup_node _sc_rollup_client sc_rollup node client ->
  let data = String.make 4097 'z' in
  let hash = reveal_hash ~protocol ~kind data in
  let pvm_dir = Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) kind in
  let filename = Filename.concat pvm_dir hash.filename in
  let () = Sys.mkdir pvm_dir 0o700 in
  let cout = open_out filename in
  let () = output_string cout data in
  let () = close_out cout in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let expect_failure =
    let node_process = Option.get @@ Sc_rollup_node.process sc_rollup_node in
    Process.check_error
      ~exit_code:1
      ~msg:
        (rex
           "Could not encode raw data to reveal with the expected protocol \
            encoding")
      node_process
  in
  let* () = send_text_messages client [hash.message] in
  let should_not_sync =
    let* _level =
      Sc_rollup_node.wait_for_level
        ~timeout:10.
        sc_rollup_node
        (Node.get_level node)
    in
    Test.fail "The rollup node processed the incorrect reveal without failing"
  in
  Lwt.choose [expect_failure; should_not_sync]

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4147

    remove the need to have a scoru to run wallet command. In tezt the
    {!Sc_rollup_client.create} takes a node where the command tested here does
    not need to have a originated scoru nor a rollup node.
*)

(** Initializes a client with an account.*)
let test_scenario_client_with_account ~account ~variant ~kind f =
  test_full_scenario
    ~kind
    {
      tags = ["rollup_client"; "wallet"];
      variant = Some variant;
      description = "rollup client wallet is valid";
    }
  @@ fun _protocol
             _rollup_node
             rollup_client
             _sc_rollup
             _tezos_node
             _tezos_client ->
  let* () = Sc_rollup_client.import_secret_key account rollup_client in
  f rollup_client

(* Check that the client can show the address of a registered account.
   -------------------------------------------------------------------
*)
let test_rollup_client_show_address ~kind =
  let account = Constant.aggregate_tz4_account in
  test_scenario_client_with_account ~account ~kind ~variant:"show address"
  @@ fun rollup_client ->
  let* shown_account =
    Sc_rollup_client.show_address ~alias:account.aggregate_alias rollup_client
  in
  Check.(
    (account.aggregate_public_key_hash = shown_account.aggregate_public_key_hash)
      string
      ~error_msg:"Expecting %L, got %R as public key hash from the client.") ;
  Check.(
    (account.aggregate_public_key = shown_account.aggregate_public_key)
      string
      ~error_msg:"Expecting %L, got %R as public key from the client.") ;
  Check.(
    (shown_account.aggregate_secret_key = account.aggregate_secret_key)
      Account.secret_key_typ
      ~error_msg:"Expecting %L, got %R as secret key from the client.") ;
  unit

(* Check that the client can generate keys.
   ----------------------------------------
*)
let test_rollup_client_generate_keys ~kind =
  let account = Constant.aggregate_tz4_account in
  test_scenario_client_with_account ~account ~kind ~variant:"gen address"
  @@ fun rollup_client ->
  let alias = "test_key" in
  let* () = Sc_rollup_client.generate_keys ~alias rollup_client in
  let* _account = Sc_rollup_client.show_address ~alias rollup_client in
  unit

(* Check that the client can list keys.
   ------------------------------------
*)
let test_rollup_client_list_keys ~kind =
  let account = Constant.aggregate_tz4_account in
  test_scenario_client_with_account ~account ~kind ~variant:"list alias"
  @@ fun rollup_client ->
  let* maybe_keys = Sc_rollup_client.list_keys rollup_client in
  let expected_keys =
    [(account.aggregate_alias, account.aggregate_public_key_hash)]
  in
  Check.(
    (expected_keys = maybe_keys)
      (list (tuple2 string string))
      ~error_msg:"Expecting\n%L\ngot\n%R\nas keys from the client.") ;
  unit

let test_consecutive_commitments _protocol _rollup_node _rollup_client sc_rollup
    _tezos_node tezos_client =
  let* inbox_level = Client.level tezos_client in
  let operator = Constant.bootstrap1.public_key_hash in
  let* {commitment_period_in_blocks; _} =
    get_sc_rollup_constants tezos_client
  in
  (* As we did no publish any commitment yet, this is supposed to fail. *)
  let*? process =
    RPC.Client.spawn tezos_client
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
  let* predecessor, _ =
    last_cemented_commitment_hash_with_level ~sc_rollup tezos_client
  in
  (* Bake commitment_period_in_blocks blocks in order prevent commitment being
     posted for future inbox_level *)
  let* () =
    repeat (commitment_period_in_blocks + 1) (fun () ->
        Client.bake_for_and_wait tezos_client)
  in
  let* commit_hash =
    publish_dummy_commitment
      ~inbox_level:(inbox_level + commitment_period_in_blocks)
      ~predecessor
      ~sc_rollup
      ~src:operator
      tezos_client
  in
  let* () =
    repeat (commitment_period_in_blocks + 2) (fun () ->
        Client.bake_for_and_wait tezos_client)
  in
  let* _commit_hash =
    publish_dummy_commitment
      ~inbox_level:(inbox_level + (2 * commitment_period_in_blocks))
      ~predecessor:commit_hash
      ~sc_rollup
      ~src:operator
      tezos_client
  in
  unit

(* Refutation game scenarios
   -------------------------
*)

let remove_state_from_dissection dissection =
  JSON.update
    "dissection"
    (fun d ->
      let d =
        JSON.as_list d
        |> List.map (fun s ->
               JSON.filter_object s (fun key _ -> not (key = "state"))
               |> JSON.unannotate)
      in
      JSON.annotate ~origin:"trimmed_dissection" (`A d))
    dissection

(*

   To check the refutation game logic, we evaluate a scenario with one
   honest rollup node and one dishonest rollup node configured as with
   a given [loser_mode].

   For a given sequence of [inputs], distributed amongst several
   levels, with some possible [empty_levels]. We check that at some
   [final_level], the crime does not pay: the dishonest node has losen
   its deposit while the honest one has not.

*)
let test_refutation_scenario ?commitment_period ?challenge_window ~variant ~mode
    ~kind
    ( loser_modes,
      inputs,
      final_level,
      empty_levels,
      stop_loser_at,
      reset_honest_on ) =
  let regression =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5313
       Disabled dissection regressions for parallel games, as it introduces
       flakyness. *)
    List.compare_length_with loser_modes 1 <= 0
  in
  test_full_scenario
    ~regression
    ?hooks:None (* We only want to capture dissections manually *)
    ?commitment_period
    ~kind
    ~mode
    ~timeout:60
    ?challenge_window
    {
      tags = (["refutation"] @ if mode = Accuser then ["accuser"] else []);
      variant = Some (variant ^ if mode = Accuser then "+accuser" else "");
      description = "refutation games winning strategies";
    }
  @@ fun protocol sc_rollup_node sc_client1 sc_rollup_address node client ->
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  let loser_keys =
    List.mapi
      (fun i _ -> Account.Bootstrap.keys.(i + 1).public_key_hash)
      loser_modes
  in

  let game_started = ref false in
  let conflict_detected = ref false in
  let detected_conflicts = ref [] in
  let published_commitments = ref [] in
  let detected_timeouts = Hashtbl.create 5 in
  let dissections = Hashtbl.create 17 in

  let run_honest_node sc_rollup_node =
    let gather_conflicts_promise =
      let rec gather_conflicts () =
        let* conflict = wait_for_conflict_detected sc_rollup_node in
        conflict_detected := true ;
        detected_conflicts := conflict :: !detected_conflicts ;
        gather_conflicts ()
      in
      gather_conflicts ()
    in
    let gather_commitments_promise =
      let rec gather_commitments () =
        let* c = wait_for_publish_commitment sc_rollup_node in
        published_commitments := c :: !published_commitments ;
        gather_commitments ()
      in
      gather_commitments ()
    in
    let gather_timeouts_promise =
      let rec gather_timeouts () =
        let* other = wait_for_timeout_detected sc_rollup_node in
        Hashtbl.replace
          detected_timeouts
          other
          (Option.value ~default:0 (Hashtbl.find_opt detected_timeouts other)
          + 1) ;
        gather_timeouts ()
      in
      gather_timeouts ()
    in
    let gather_dissections_promise =
      let rec gather_dissections () =
        let* opponent, dissection =
          wait_for_computed_dissection sc_rollup_node
        in
        let dissection =
          match kind with
          | "arith" -> dissection
          | _ (* wasm *) ->
              (* Remove state hashes from WASM dissections as they depend on
                 timestamps *)
              remove_state_from_dissection dissection
        in
        (* Use buckets of table to store multiple dissections for same
           opponent. *)
        Hashtbl.add dissections opponent dissection ;
        gather_dissections ()
      in
      gather_dissections ()
    in
    let* () =
      Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup_address []
    in
    return
      [
        gather_conflicts_promise;
        gather_commitments_promise;
        gather_timeouts_promise;
        gather_dissections_promise;
      ]
  in

  let loser_sc_rollup_nodes =
    List.map2
      (fun default_operator _ ->
        Sc_rollup_node.create
          ~protocol
          Operator
          node
          ~base_dir:(Client.base_dir client)
          ~default_operator)
      loser_keys
      loser_modes
  in
  let* gather_promises = run_honest_node sc_rollup_node
  and* () =
    Lwt_list.iter_p (fun (loser_mode, loser_sc_rollup_node) ->
        Sc_rollup_node.run loser_sc_rollup_node ~loser_mode sc_rollup_address [])
    @@ List.combine loser_modes loser_sc_rollup_nodes
  in

  let restart_promise =
    (* Reset node when detecting certain events *)
    Lwt_list.iter_p
      (fun (event, delay) ->
        let* () =
          Sc_rollup_node.wait_for sc_rollup_node event @@ fun _json -> Some ()
        in
        let current_level = Node.get_level node in
        let* _ =
          Sc_rollup_node.wait_for_level
            ~timeout:3.0
            sc_rollup_node
            (current_level + delay)
        in
        let* () = Sc_rollup_node.terminate sc_rollup_node in
        let* _ = run_honest_node sc_rollup_node in
        unit)
      reset_honest_on
  in

  let stop_losers level =
    if List.mem level stop_loser_at then
      Lwt_list.iter_p
        (fun loser_sc_rollup_node ->
          Sc_rollup_node.terminate loser_sc_rollup_node)
        loser_sc_rollup_nodes
    else unit
  in
  let rec consume_inputs = function
    | [] -> unit
    | inputs :: next_batches as all ->
        let* level = Client.level client in
        let* () = stop_losers level in
        if List.mem level empty_levels then
          let* () = Client.bake_for_and_wait client in
          consume_inputs all
        else
          let* () =
            send_text_messages ~src:Constant.bootstrap3.alias client inputs
          in
          consume_inputs next_batches
  in
  let* () = consume_inputs inputs in
  let* after_inputs_level = Client.level client in

  let hook i =
    let level = after_inputs_level + i in
    stop_losers level
  in
  let keep_going client =
    let* game =
      RPC.Client.call client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_staker_games
           ~staker:bootstrap1_key
           sc_rollup_address
           ()
    in
    if !game_started then return @@ not (JSON.is_null game)
    else (
      game_started := not @@ JSON.is_null game ;
      return true)
  in

  let* () =
    bake_until ~hook keep_going (final_level - List.length inputs) client
  in

  if not !conflict_detected then
    Test.fail "Honest node did not detect the conflict" ;

  let multiple_timeouts_for_opponent =
    Hashtbl.fold
      (fun _other timeouts no -> no || timeouts > 1)
      detected_timeouts
      false
  in

  if multiple_timeouts_for_opponent then
    Test.fail "Attempted to timeout an opponent more than once" ;

  if mode = Accuser then (
    assert (!detected_conflicts <> []) ;
    List.iter
      (fun (commitment_hash, level) ->
        if not (List.mem_assoc commitment_hash !detected_conflicts) then
          Test.fail
            "Accuser published the commitment %s at level %d which never \
             appeared in a conflict"
            commitment_hash
            level)
      !published_commitments) ;

  let* {stake_amount; _} = get_sc_rollup_constants client in
  let* honest_deposit_json =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_contract_frozen_bonds ~id:bootstrap1_key ()
  in
  let* loser_deposits_json =
    Lwt_list.map_p
      (fun id ->
        RPC.Client.call client
        @@ RPC.get_chain_block_context_contract_frozen_bonds ~id ())
      loser_keys
  in

  Check.(
    (honest_deposit_json = stake_amount)
      Tez.typ
      ~error_msg:"expecting deposit for honest participant = %R, got %L") ;
  List.iter
    (fun loser_deposit_json ->
      Check.(
        (loser_deposit_json = Tez.zero)
          Tez.typ
          ~error_msg:"expecting loss for dishonest participant = %R, got %L"))
    loser_deposits_json ;
  Log.info "Checking that we can still retrieve state from rollup node" ;
  (* This is a way to make sure the rollup node did not crash *)
  let*! _value = Sc_rollup_client.state_hash sc_client1 in
  List.iter Lwt.cancel (restart_promise :: gather_promises) ;
  (* Capture dissections *)
  Hashtbl.to_seq_values dissections
  |> List.of_seq |> List.rev
  |> List.iter (fun dissection ->
         Regression.capture "\n" ;
         Regression.capture @@ JSON.encode dissection) ;
  unit

let rec swap i l =
  if i <= 0 then l
  else match l with [_] | [] -> l | x :: y :: l -> y :: swap (i - 1) (x :: l)

let inputs_for n =
  List.concat @@ List.init n
  @@ fun i -> [swap i ["3 3 +"; "1"; "1 1 x"; "3 7 8 + * y"; "2 2 out"]]

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
          ("inbox_proof_at_genesis", (["3 0 0"], inputs_for 10, 80, [], [], []));
          ("pvm_proof_at_genesis", (["3 0 1"], inputs_for 10, 80, [], [], []));
          ("inbox_proof", (["5 0 0"], inputs_for 10, 80, [], [], []));
          ( "inbox_proof_with_new_content",
            (["5 0 0"], inputs_for 30, 80, [], [], []) );
          (* In "inbox_proof_with_new_content" we add messages after the commitment
             period so the current inbox is not equal to the inbox snapshot-ted at the
             game creation. *)
          ( "inbox_proof_one_empty_level",
            (["6 0 0"], inputs_for 10, 80, [2], [], []) );
          ( "inbox_proof_many_empty_levels",
            (["9 0 0"], inputs_for 10, 80, [2; 3; 4], [], []) );
          ("pvm_proof_0", (["5 2 1"], inputs_for 10, 80, [], [], []));
          ("pvm_proof_1", (["7 2 0"], inputs_for 10, 80, [], [], []));
          ("pvm_proof_2", (["7 2 5"], inputs_for 7, 80, [], [], []));
          ("pvm_proof_3", (["9 2 5"], inputs_for 7, 80, [4; 5], [], []));
          ("timeout", (["5 2 1"], inputs_for 10, 80, [], [35], []));
          ( "reset_honest_during_game",
            ( ["5 2 1"],
              inputs_for 10,
              80,
              [],
              [],
              [("sc_rollup_node_conflict_detected.v0", 2)] ) );
          ( "parallel_games_0",
            (["3 0 0"; "3 0 1"], inputs_for 10, 80, [], [], []) );
          ( "parallel_games_1",
            (["3 0 0"; "3 0 1"; "3 0 0"], inputs_for 10, 200, [], [], []) );
        ]
    | "wasm_2_0_0" ->
        [
          (* First message of an inbox (level 3) *)
          ("inbox_proof_0", (["3 0 0"], inputs_for 10, 80, [], [], []));
          (* Fourth message of an inbox (level 3) *)
          ("inbox_proof_1", (["3 4 0"], inputs_for 10, 80, [], [], []));
          (* Echo kernel takes around 2,100 ticks to execute *)
          (* Second tick of decoding *)
          ( "pvm_proof_0",
            (["5 7 11_000_000_001"], inputs_for 10, 80, [], [], []) );
          ( "pvm_proof_1",
            (["7 7 11_000_001_000"], inputs_for 10, 80, [], [], []) );
          (* End of evaluation *)
          ( "pvm_proof_2",
            (["7 7 22_000_002_000"], inputs_for 10, 80, [], [], []) );
          (* During padding *)
          ( "pvm_proof_3",
            (["7 7 22_010_000_000"], inputs_for 10, 80, [], [], []) );
          ( "parallel_games_0",
            (["4 0 0"; "5 7 11_000_000_001"], inputs_for 10, 80, [], [], []) );
          ( "parallel_games_1",
            ( ["4 0 0"; "7 7 22_000_002_000"; "7 7 22_000_002_000"],
              inputs_for 10,
              80,
              [],
              [],
              [] ) );
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

(** Run one of the refutation tests with an accuser instead of a full operator. *)
let test_accuser protocols =
  test_refutation_scenario
    ~kind:"wasm_2_0_0"
    ~mode:Accuser
    ~challenge_window:10
    ~commitment_period:10
    ~variant:"pvm_proof_2"
    (["7 7 22_000_002_000"], inputs_for 10, 80, [], [], [])
    protocols

(** Helper to check that the operation whose hash is given is successfully
    included (applied) in the current head block. *)
let check_op_included ~__LOC__ =
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
let bake_operation_via_rpc ~__LOC__ client op =
  let* (`OpHash oph) = Operation.Manager.inject [op] client in
  let* () = Client.bake_for_and_wait client in
  check_op_included ~__LOC__ ~oph client

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
    let d = inbox_level - Node.get_level node + 1 in
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
  @@ fun sc_rollup tezos_node tezos_client ->
  (* Choosing challenge_windows to be quite longer than commitment_period
     to avoid being in a situation where the first commitment in the result
     of [mk_forking_commitments] is cementable without further bakes. *)

  (* Completely arbitrary as we decide when to trigger timeouts in tests.
     Making it a lot smaller than the default value to speed up tests. *)
  (* Building a forking commitments tree. *)
  let operator1 = Constant.bootstrap1 in
  let operator2 = Constant.bootstrap2 in
  let level0 = Node.get_level tezos_node in
  let* commits =
    mk_forking_commitments
      tezos_node
      tezos_client
      ~sc_rollup
      ~operator1:operator1.public_key_hash
      ~operator2:operator2.public_key_hash
  in
  let level1 = Node.get_level tezos_node in
  scenario
    tezos_client
    tezos_node
    ~sc_rollup
    ~operator1
    ~operator2
    commits
    level0
    level1

(* A more convenient wrapper around [cement_commitment]. *)
let cement_commitments client sc_rollup ?fail =
  Lwt_list.iter_s (fun hash -> cement_commitment client ~sc_rollup ~hash ?fail)

let timeout ?expect_failure ~sc_rollup ~staker1 ~staker2 client =
  let*! () =
    Client.Sc_rollup.timeout
      ~hooks
      ~dst:sc_rollup
      ~src:Constant.bootstrap1.alias
      ~staker1
      ~staker2
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
let test_no_cementation_if_parent_not_lcc_or_if_disputed_commit =
  test_forking_scenario ~variant:"publish, and cement on wrong commitment"
  @@ fun client _node ~sc_rollup ~operator1 ~operator2 commits level0 level1 ->
  let c1, c2, c31, c32, c311, _c321 = commits in
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
  (* c1 and c2 will be cemented. *)
  let* () = cement [c1; c2] in
  (* We cannot cement c31 or c32 on top of c2 because they are disputed *)
  let* () = cement [c31; c32] ~fail:disputed_commit in

  (* +++ dispute resolution +++
     Let's resolve the dispute between operator1 and operator2 on the fork
     c31 vs c32. [operator1] will make a bad initial dissection, so it
     loses the dispute, and the branch c32 --- c321 dies. *)

  (* [operator1] starts a dispute. *)
  let module M = Operation.Manager in
  let* () =
    let refutation =
      M.Start {player_commitment_hash = c32; opponent_commitment_hash = c31}
    in
    bake_operation_via_rpc ~__LOC__ client
    @@ M.make ~source:operator2
    @@ M.sc_rollup_refute
         ~sc_rollup
         ~opponent:operator1.public_key_hash
         ~refutation
         ()
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
  @@ fun client _node ~sc_rollup ~operator1 ~operator2 commits _level0 _level1
    ->
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
    let refutation =
      M.Start {player_commitment_hash = c32; opponent_commitment_hash = c31}
    in
    bake_operation_via_rpc ~__LOC__ client
    @@ M.make ~source
    @@ M.sc_rollup_refute ~sc_rollup ~opponent ~refutation ()
  in
  (* Construct a valid dissection with valid initial hash of size
     [sc_rollup.number_of_sections_in_dissection]. The state hash below is
     the hash of the state computed after submitting the first commitment c1
     (which is also equal to states's hashes of subsequent commitments, as we
     didn't add any message in inboxes). If this hash needs to be recomputed,
     run this test with --verbose and grep for 'compressed_state' in the
     produced logs. *)
  let state_hash = "srs11Z9V76SGd97kGmDQXV8tEF67C48GMy77RuaHdF1kWLk6UTmMfj" in

  let rec aux i acc =
    if i = number_of_sections_in_dissection - 1 then
      List.rev ({M.state_hash = None; tick = i} :: acc)
    else aux (i + 1) ({M.state_hash = Some state_hash; tick = i} :: acc)
  in
  (* Inject a valid dissection move *)
  let refutation =
    M.Move {choice_tick = 0; refutation_step = Dissection (aux 0 [])}
  in

  let* () =
    bake_operation_via_rpc ~__LOC__ client
    @@ M.make ~source
    @@ M.sc_rollup_refute ~sc_rollup ~opponent ~refutation ()
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
  @@ fun client _node ~sc_rollup ~operator1 ~operator2 commits level0 level1 ->
  (* These are the commitments on the rollup. See [test_forking_scenario] to
       visualize the tree structure. *)
  let c1, c2, c31, c32, _c311, _c321 = commits in
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
    let refutation =
      M.Start {player_commitment_hash = c32; opponent_commitment_hash = c31}
    in
    bake_operation_via_rpc ~__LOC__ client
    @@ M.make ~source:operator2
    @@ M.sc_rollup_refute
         ~sc_rollup
         ~opponent:operator1.public_key_hash
         ~refutation
         ()
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
let test_late_rollup_node =
  test_full_scenario
    ~commitment_period:3
    {
      tags = ["late"];
      variant = None;
      description = "a late rollup should catch up";
    }
  @@ fun protocol sc_rollup_node _rollup_client sc_rollup_address node client ->
  let* () = bake_levels 65 client in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let* () = bake_levels 30 client in
  let* _status = Sc_rollup_node.wait_for_level ~timeout:2. sc_rollup_node 95 in
  Log.info "First rollup node synchronized." ;
  let sc_rollup_node2 =
    Sc_rollup_node.create
      ~protocol
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:
        Constant.bootstrap1.alias (* Same as other rollup_node *)
  in
  Log.info "Start rollup node from scratch with same operator" ;
  let* () = Sc_rollup_node.run sc_rollup_node2 sc_rollup_address [] in
  let* _level =
    Sc_rollup_node.wait_for_level
      ~timeout:2.
      sc_rollup_node2
      (Node.get_level node)
  in
  Log.info "Other rollup node synchronized." ;
  let* () = Client.bake_for_and_wait client in
  let* _level =
    Sc_rollup_node.wait_for_level
      ~timeout:2.
      sc_rollup_node2
      (Node.get_level node)
  in
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
let test_late_rollup_node_2 =
  test_full_scenario
    ~commitment_period:3
    {
      tags = ["late"];
      variant = None;
      description = "a late alternative rollup should catch up";
    }
  @@ fun protocol sc_rollup_node _rollup_client sc_rollup_address node client ->
  let* () = bake_levels 65 client in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let* () = bake_levels 30 client in
  let* _status = Sc_rollup_node.wait_for_level ~timeout:2. sc_rollup_node 95 in
  Log.info "First rollup node synchronized." ;
  let sc_rollup_node2 =
    Sc_rollup_node.create
      ~protocol
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:Constant.bootstrap2.alias
  in
  Log.info
    "Starting alternative rollup node from scratch with a different operator." ;
  let* () = Sc_rollup_node.run sc_rollup_node2 sc_rollup_address [] in
  let* _level =
    Sc_rollup_node.wait_for_level
      ~timeout:2.
      sc_rollup_node2
      (Node.get_level node)
  in
  Log.info "Alternative rollup node is synchronized." ;
  let* () = Client.bake_for_and_wait client in
  let* _level =
    Sc_rollup_node.wait_for_level
      ~timeout:2.
      sc_rollup_node2
      (Node.get_level node)
  in
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
  @@ fun _protocol sc_rollup_node _rollup_client sc_rollup _node client ->
  let processing_promise =
    Sc_rollup_node.wait_for
      sc_rollup_node
      "sc_rollup_daemon_process_head.v0"
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

let test_refutation_reward_and_punishment ~kind =
  let timeout_period = 3 in
  let commitment_period = 2 in
  test_l1_scenario
    ~kind
    ~timeout:timeout_period
    ~commitment_period
    ~regression:true
    ~hooks
    {
      tags = ["refutation"; "reward"; "punishment"];
      variant = None;
      description = "participant of a refutation game are slashed/rewarded";
    }
  @@ fun sc_rollup node client ->
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
  let starting_level = Node.get_level node in
  let inbox_level = starting_level + commitment_period_in_blocks in
  (* d is the delta between the target inbox level and the current level *)
  let d = inbox_level - Node.get_level node + 1 in
  (* Bake sufficiently many blocks to be able to commit for the desired inbox
     level. We may actually bake no blocks if d <= 0 *)
  let* () = repeat d (fun () -> Client.bake_for_and_wait client) in

  (* [operator1] stakes on a commitment. *)
  let* operator1_commitment =
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
  let* operator2_commitment =
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
    let refutation =
      M.Start
        {
          player_commitment_hash = operator1_commitment;
          opponent_commitment_hash = operator2_commitment;
        }
    in
    bake_operation_via_rpc ~__LOC__ client
    @@ M.make ~source:operator1
    @@ M.sc_rollup_refute
         ~sc_rollup
         ~opponent:operator2.public_key_hash
         ~refutation
         ()
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
    ?outbox_parameters_ty ?boot_sector ~input_message ~expected_storage ~kind
    ~message_kind =
  let commitment_period = 2 and challenge_window = 5 in
  let message_kind_s =
    match message_kind with `Internal -> "intern" | `External -> "extern"
  in
  let entrypoint_s = Option.value ~default:"default" entrypoint in
  let outbox_parameters_ty_s =
    Option.value ~default:"no_parameters_ty" outbox_parameters_ty
  in
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
      tags = ["outbox"; message_kind_s; entrypoint_s; outbox_parameters_ty_s];
      variant =
        Some
          (Format.sprintf
             "%s, entrypoint: %%%s, eager: %d, %s, %s"
             init_storage
             entrypoint_s
             earliness
             message_kind_s
             outbox_parameters_ty_s);
      description = "output exec";
    }
  @@ fun protocol rollup_node sc_client sc_rollup _node client ->
  let* () = Sc_rollup_node.run rollup_node sc_rollup [] in
  let src = Constant.bootstrap1.public_key_hash in
  let src2 = Constant.bootstrap2.public_key_hash in
  let originate_target_contract () =
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
        src2
    in
    let* address =
      Client.originate_contract
        ~alias:"target"
        ~amount:(Tez.of_int 100)
        ~burn_cap:(Tez.of_int 100)
        ~src
        ~prg
        ~init:init_storage
        client
    in
    let* () = Client.bake_for_and_wait client in
    return address
  in
  let check_contract_execution address expected_storage =
    let* storage = Client.contract_storage address client in
    return
    @@ Check.(
         (String.trim storage = expected_storage)
           string
           ~error_msg:"Invalid contract storage: expecting '%R', got '%L'.")
  in
  let perform_rollup_execution_and_cement source_address target_address =
    let* payload = input_message sc_client target_address in
    let* () =
      match payload with
      | `External payload ->
          send_text_messages ~hooks ~format:`Hex client [payload]
      | `Internal payload ->
          let payload = "0x" ^ payload in
          Client.transfer
            ~amount:Tez.(of_int 100)
            ~burn_cap:Tez.(of_int 100)
            ~storage_limit:100000
            ~giver:Constant.bootstrap1.alias
            ~receiver:source_address
            ~arg:(sf "Pair %s %S" payload sc_rollup)
            client
    in
    let blocks_to_wait =
      2 + (2 * commitment_period) + challenge_window - earliness
    in
    repeat blocks_to_wait @@ fun () -> Client.bake_for client
  in
  let trigger_outbox_message_execution ?expected_l1_error address =
    let outbox_level = 5 in
    let destination = address in
    let parameters = "37" in
    let message_index = 0 in
    let check_expected_outbox () =
      let* outbox =
        Runnable.run @@ Sc_rollup_client.outbox ~outbox_level sc_client
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
                 (match protocol with
                 | Protocol.Mumbai -> ""
                 | _ ->
                     Printf.sprintf
                       {|, "kind": "%s"|}
                       (Option.fold
                          ~none:"untyped"
                          ~some:(fun _ -> "typed")
                          outbox_parameters_ty))
          in
          Log.info "Expected is %s" (JSON.encode expected) ;
          assert (JSON.encode expected = JSON.encode outbox) ;
          Sc_rollup_client.outbox_proof_single
            sc_client
            ?expected_error
            ~message_index
            ~outbox_level
            ~destination
            ?entrypoint
            ~parameters
            ?parameters_ty:outbox_parameters_ty
      | Some _ ->
          assert (JSON.encode outbox = "[]") ;
          return None
    in
    let* answer = check_expected_outbox () in
    match (answer, expected_error) with
    | Some _, Some _ -> assert false
    | None, None -> failwith "Unexpected error during proof generation"
    | None, Some _ -> unit
    | Some {commitment_hash; proof}, None -> (
        match expected_l1_error with
        | None ->
            let*! () =
              Client.Sc_rollup.execute_outbox_message
                ~hooks
                ~burn_cap:(Tez.of_int 10)
                ~rollup:sc_rollup
                ~src:src2
                ~commitment_hash
                ~proof
                client
            in
            Client.bake_for_and_wait client
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
  let* target_contract_address = originate_target_contract () in
  let* source_contract_address =
    originate_forward_smart_contract client protocol
  in
  let* () =
    perform_rollup_execution_and_cement
      source_contract_address
      target_contract_address
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    trigger_outbox_message_execution ?expected_l1_error target_contract_address
  in
  match expected_error with
  | None ->
      let* () =
        check_contract_execution target_contract_address expected_storage
      in
      unit
  | Some _ -> unit

let test_outbox_message ?supports ?regression ?expected_error ?expected_l1_error
    ~earliness ?entrypoint ?(init_storage = "0") ?(storage_ty = "int")
    ?(outbox_parameters = "37") ?outbox_parameters_ty ~kind ~message_kind =
  let wrap payload =
    match message_kind with
    | `Internal -> `Internal payload
    | `External -> `External payload
  in
  let boot_sector, input_message, expected_storage =
    match kind with
    | "arith" ->
        let input_message _client contract_address =
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
        let input_message client contract_address =
          let transaction =
            Sc_rollup_client.
              {
                destination = contract_address;
                entrypoint;
                parameters = outbox_parameters;
                parameters_ty = outbox_parameters_ty;
              }
          in
          let* answer = Sc_rollup_client.encode_batch client [transaction] in
          match answer with
          | None -> failwith "Encoding of batch should not fail."
          | Some answer -> return (wrap answer)
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
    ~init_storage
    ~storage_ty
    ~input_message
    ~expected_storage
    ~message_kind
    ~kind

let test_outbox_message protocols ~kind =
  let test (expected_error, earliness, entrypoint, message_kind) =
    test_outbox_message
      ?expected_error
      ~earliness
      ?entrypoint
      ~message_kind
      protocols
      ~kind ;
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
  in
  List.iter
    test
    [
      (None, 0, None, `Internal);
      (None, 0, Some "aux", `Internal);
      (Some (Base.rex ".*Invalid claim about outbox"), 5, None, `Internal);
      (Some (Base.rex ".*Invalid claim about outbox"), 5, Some "aux", `Internal);
      (None, 0, None, `External);
      (None, 0, Some "aux", `External);
      (Some (Base.rex ".*Invalid claim about outbox"), 5, None, `External);
      (Some (Base.rex ".*Invalid claim about outbox"), 5, Some "aux", `External);
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
      ~kind ;
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
      ~kind)

let test_rpcs ~kind
    ?(boot_sector = Sc_rollup_helpers.default_boot_sector_of ~kind) =
  test_full_scenario
    ~regression:true
    ~hooks
    ~kind
    ~boot_sector
    {
      tags = ["rpc"; "api"];
      variant = None;
      description = "RPC API should work and be stable";
    }
  @@ fun protocol sc_rollup_node sc_client sc_rollup node client ->
  let* _origination_proof =
    RPC.Client.call ~hooks client
    @@ RPC.post_chain_block_context_smart_rollups_all_origination_proof
         ~kind
         ~boot_sector
         ()
  in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  (* Smart rollup address endpoint test *)
  let*! sc_rollup_address =
    Sc_rollup_client.rpc_get ~hooks sc_client ["global"; "smart_rollup_address"]
  in
  let sc_rollup_address = JSON.as_string sc_rollup_address in
  Check.((sc_rollup_address = sc_rollup) string)
    ~error_msg:"SC rollup address of node is %L but should be %R" ;
  let n = 15 in
  let batch_size = 5 in
  let* hashes =
    send_messages_batcher ~hooks ~batch_size n client sc_rollup_node sc_client
  in
  Check.((List.length hashes = n * batch_size) int)
    ~error_msg:"Injected %L messages but should have injected %R" ;
  (* Head block hash endpoint test *)
  let level = Node.get_level node in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:3.0 sc_rollup_node level in
  let* l1_block_hash = RPC.Client.call client @@ RPC.get_chain_block_hash () in
  let*! l2_block_hash =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "hash"]
  in
  let l2_block_hash = JSON.as_string l2_block_hash in
  Check.((l1_block_hash = l2_block_hash) string)
    ~error_msg:"Head on L1 is %L where as on L2 it is %R" ;
  let* l1_block_hash_5 =
    RPC.Client.call client @@ RPC.get_chain_block_hash ~block:"5" ()
  in
  let*! l2_block_hash_5 =
    Sc_rollup_client.rpc_get ~hooks sc_client ["global"; "block"; "5"; "hash"]
  in
  let l2_block_hash_5 = JSON.as_string l2_block_hash_5 in
  Check.((l1_block_hash_5 = l2_block_hash_5) string)
    ~error_msg:"Block 5 on L1 is %L where as on L2 it is %R" ;
  let*! l2_finalied_block_level =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "finalized"; "level"]
  in
  let l2_finalied_block_level = JSON.as_int l2_finalied_block_level in
  Check.((l2_finalied_block_level = level - 2) int)
    ~error_msg:"Finalized block is %L but should be %R" ;
  let*! l2_num_messages =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "num_messages"]
  in
  let l2_num_messages = JSON.as_int l2_num_messages in
  Check.((l2_num_messages = batch_size) int)
    ~error_msg:"Number of messages of head is %L but should be %R" ;

  (* Durable value storage RPC tests *)
  let* () =
    match kind with
    | "arith" ->
        (* Make sure we neither have WASM nor Arith PVM endpoint in arith PVM *)
        let*? process =
          Sc_rollup_client.inspect_durable_state_value
            ~hooks
            sc_client
            ~pvm_kind:"wasm_2_0_0"
            ~operation:Sc_rollup_client.Value
            ~key:"/readonly/wasm_version"
        in
        let* () =
          Process.check_error
            ~msg:(Base.rex "No service found at this URL")
            process
        in

        let*? process =
          Sc_rollup_client.inspect_durable_state_value
            ~hooks
            sc_client
            ~pvm_kind:"arith"
            ~operation:Sc_rollup_client.Value
            ~key:"/readonly/wasm_version"
        in
        Process.check_error
          ~msg:(Base.rex "No service found at this URL")
          process
    | "wasm_2_0_0" ->
        let*! wasm_boot_sector =
          Sc_rollup_client.inspect_durable_state_value
            ~hooks
            sc_client
            ~pvm_kind:kind
            ~operation:Sc_rollup_client.Value
            ~key:"/kernel/boot.wasm"
        in
        Check.(
          (wasm_boot_sector = Some Constant.wasm_echo_kernel_boot_sector)
            (option string))
          ~error_msg:"Encoded WASM kernel is %L but should be %R" ;

        let*! nonexisting_wasm_boot_sector =
          Sc_rollup_client.inspect_durable_state_value
            ~hooks
            sc_client
            ~pvm_kind:kind
            ~operation:Sc_rollup_client.Value
            ~key:"/kernel/boot.wasm2"
        in
        Check.((nonexisting_wasm_boot_sector = None) (option string))
          ~error_msg:"Encoded WASM kernel is %L but should be %R" ;

        let*! wasm_version_hex_opt =
          Sc_rollup_client.inspect_durable_state_value
            ~hooks
            sc_client
            ~pvm_kind:kind
            ~operation:Sc_rollup_client.Value
            ~key:"/readonly/wasm_version"
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

        let*! wasm_version_len =
          Sc_rollup_client.inspect_durable_state_value
            ~hooks
            sc_client
            ~pvm_kind:kind
            ~operation:Sc_rollup_client.Length
            ~key:"/readonly/wasm_version"
        in
        Check.(
          (wasm_version_len
          = Some
              (default_wasm_pvm_revision protocol
              |> String.length |> Int64.of_int))
            (option int64))
          ~error_msg:"WASM version value length is %L but should be %R" ;

        let*! kernel_subkeys =
          Sc_rollup_client.inspect_durable_state_value
            ~hooks
            sc_client
            ~pvm_kind:kind
            ~operation:Sc_rollup_client.Subkeys
            ~key:"/readonly/kernel"
        in
        Check.((kernel_subkeys = ["boot.wasm"; "env"]) (list string))
          ~error_msg:"The key's subkeys are %L but should be %R" ;
        return ()
    | _ -> failwith "incorrect kind"
  in
  let*! _status =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "status"]
  in
  let*! _ticks =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "ticks"]
  in
  let*! _state_hash =
    Sc_rollup_client.rpc_get
      ~hooks
      sc_client
      ["global"; "block"; "head"; "state_hash"]
  in
  let* _outbox =
    Runnable.run
    @@ Sc_rollup_client.outbox
         ~hooks
         ~outbox_level:l2_finalied_block_level
         sc_client
  in
  let*! _head =
    Sc_rollup_client.rpc_get ~hooks sc_client ["global"; "tezos_head"]
  in
  let*! _level =
    Sc_rollup_client.rpc_get ~hooks sc_client ["global"; "tezos_level"]
  in
  let*! l2_block =
    Sc_rollup_client.rpc_get ~hooks sc_client ["global"; "block"; "head"]
  in
  let l2_block_hash' = JSON.(l2_block |-> "block_hash" |> as_string) in
  Check.((l2_block_hash' = l2_block_hash) string)
    ~error_msg:"L2 head is from full block is %L but should be %R" ;
  unit

let test_messages_processed_by_commitment ~kind =
  test_full_scenario
    {
      variant = None;
      tags = ["commitment"; "evaluation"];
      description = "checks messages processed during a commitment period";
    }
    ~kind
  @@ fun _protocol sc_rollup_node sc_rollup_client sc_rollup _node client ->
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  let* {commitment_period_in_blocks; _} = get_sc_rollup_constants client in
  let* genesis_info =
    RPC.Client.call ~hooks client
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
    get_last_stored_commitment ~__LOC__ ~hooks sc_rollup_client
  in

  let*! current_level =
    Sc_rollup_client.state_current_level
      ~block:(string_of_int inbox_level)
      sc_rollup_client
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
  @@ fun sc_rollup _tezos_node tezos_client ->
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
  let* commitment1 =
    publish_dummy_commitment
      ~inbox_level:(level + commitment_period_in_blocks)
      ~predecessor
      ~sc_rollup
      ~src:staker1.public_key_hash
      tezos_client
  in
  let* commitment2 =
    publish_dummy_commitment
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
  let* () = cement_commitment tezos_client ~sc_rollup ~hash:commitment1 in

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

(** Test that it is still possible to send message after a migration
(internal and external). *)
let test_migration_inbox ~kind ~migrate_from ~migrate_to =
  let tags = ["internal"; "external"; "message"; "inbox"]
  and description = "testing to send inbox operation post migration."
  and scenario_prior tezos_client ~sc_rollup =
    let* minter_address =
      originate_forward_smart_contract tezos_client migrate_from
    in
    let* () = send_messages 2 tezos_client in
    let* () =
      Client.transfer
        ~amount:Tez.(of_int 100)
        ~burn_cap:Tez.(of_int 100)
        ~storage_limit:100000
        ~giver:Constant.bootstrap1.alias
        ~receiver:minter_address
        ~arg:
          (Format.sprintf "Pair 0x%s %S " (hex_encode "pred_message") sc_rollup)
        tezos_client
    in
    return minter_address
  and scenario_after tezos_client ~sc_rollup minter_address =
    let* () = send_messages 2 tezos_client in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5286
       Check messages are correctly added in the inbox. *)
    Client.transfer
      ~amount:Tez.(of_int 100)
      ~burn_cap:Tez.(of_int 100)
      ~storage_limit:100000
      ~giver:Constant.bootstrap1.alias
      ~receiver:minter_address
      ~arg:
        (Format.sprintf "Pair 0x%s %S " (hex_encode "next_message") sc_rollup)
      tezos_client
  in
  test_l1_migration_scenario
    ~parameters_ty:"bytes"
    ~kind
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~tags
    ~description
    ()

(** Test to continue to send messages after a migration. *)
let test_migration_ticket_inbox ~kind ~migrate_from ~migrate_to =
  let tags = ["internal"; "inbox"; "ticket"]
  and description =
    "testing to send internal message with ticket post migration."
  and scenario_prior tezos_client ~sc_rollup =
    (* Originate forwarder contract to send internal messages to rollup *)
    let* alias, minter_address =
      Client.originate_contract_at
        ~amount:Tez.zero
        ~src:Constant.bootstrap1.alias
        ~init:"Unit"
        ~burn_cap:Tez.(of_int 1)
        tezos_client
        ["mini_scenarios"; "sc_rollup_mint_and_forward"]
        migrate_from
    in
    let* () = Client.bake_for_and_wait tezos_client in
    Log.info
      "The minter-forwarder %s (%s) contract was successfully originated"
      alias
      minter_address ;
    let* () =
      Client.transfer
        ~amount:Tez.(of_int 100)
        ~burn_cap:Tez.(of_int 100)
        ~storage_limit:100000
        ~giver:Constant.bootstrap1.alias
        ~receiver:minter_address
        ~arg:
          (Format.sprintf
             "Pair (Pair 0x%s 10) %S "
             (hex_encode "pred_message")
             sc_rollup)
        tezos_client
    in
    return minter_address
  and scenario_after tezos_client ~sc_rollup minter_address =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5286
       Check messages are correctly added in the inbox. *)
    Client.transfer
      ~amount:Tez.(of_int 100)
      ~burn_cap:Tez.(of_int 100)
      ~storage_limit:100000
      ~giver:Constant.bootstrap1.alias
      ~receiver:minter_address
      ~arg:
        (Format.sprintf
           "Pair (Pair 0x%s 10) %S "
           (hex_encode "pred_message")
           sc_rollup)
      tezos_client
  in
  test_l1_migration_scenario
    ~parameters_ty:"ticket bytes"
    ~kind
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~tags
    ~description
    ()

let register ~kind ~protocols =
  test_origination ~kind protocols ;
  test_rollup_node_running ~kind protocols ;
  test_rollup_get_genesis_info ~kind protocols ;
  test_rollup_inbox_of_rollup_node
    ~kind
    ~variant:"basic"
    basic_scenario
    protocols ;
  test_rpcs ~kind protocols ;
  test_rollup_inbox_of_rollup_node
    ~kind
    ~variant:"stops"
    sc_rollup_node_stops_scenario
    protocols ;
  test_rollup_inbox_of_rollup_node
    ~kind
    ~variant:"disconnects"
    sc_rollup_node_disconnects_scenario
    protocols ;
  test_rollup_inbox_of_rollup_node
    ~kind
    ~variant:"handles_chain_reorg"
    sc_rollup_node_handles_chain_reorg
    protocols ;
  test_rollup_inbox_of_rollup_node
    ~kind
    ~variant:"batcher"
    ~extra_tags:["batcher"]
    sc_rollup_node_batcher
    protocols ;
  test_rollup_node_boots_into_initial_state protocols ~kind ;
  test_rollup_node_advances_pvm_state protocols ~kind ~internal:false ;
  test_rollup_node_advances_pvm_state protocols ~kind ~internal:true ;
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
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4373
     Uncomment this test as soon as the issue done.
     test_reinject_failed_commitment protocols ~kind ; *)
  test_late_rollup_node protocols ~kind ;
  test_late_rollup_node_2 protocols ~kind ;
  test_interrupt_rollup_node protocols ~kind ;
  test_outbox_message protocols ~kind ;
  test_messages_processed_by_commitment ~kind protocols

let register ~protocols =
  (* PVM-independent tests. We still need to specify a PVM kind
     because the tezt will need to originate a rollup. However,
     the tezt will not test for PVM kind specific featued. *)
  test_rollup_node_configuration protocols ~kind:"wasm_2_0_0" ;
  test_rollup_list protocols ~kind:"wasm_2_0_0" ;
  test_rollup_client_show_address protocols ~kind:"wasm_2_0_0" ;
  test_rollup_client_generate_keys protocols ~kind:"wasm_2_0_0" ;
  test_rollup_client_list_keys protocols ~kind:"wasm_2_0_0" ;
  test_valid_dispute_dissection ~kind:"arith" protocols ;
  test_refutation_reward_and_punishment protocols ~kind:"arith" ;
  test_timeout ~kind:"arith" protocols ;
  test_no_cementation_if_parent_not_lcc_or_if_disputed_commit
    ~kind:"arith"
    protocols ;
  test_refutation protocols ~kind:"arith" ;
  test_refutation protocols ~kind:"wasm_2_0_0" ;
  test_recover_bond_of_stakers protocols ;
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
  test_reveals_fails_on_wrong_hash protocols ;
  test_reveals_4k protocols ;
  test_reveals_above_4k protocols ;
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
  test_accuser protocols ;
  (* Shared tezts - will be executed for both PVMs. *)
  register ~kind:"wasm_2_0_0" ~protocols ;
  register ~kind:"arith" ~protocols

let register_migration ~kind ~migrate_from ~migrate_to =
  test_migration_inbox ~kind ~migrate_from ~migrate_to ;
  test_migration_ticket_inbox ~kind ~migrate_from ~migrate_to

let register_migration ~migrate_from ~migrate_to =
  register_migration ~kind:"arith" ~migrate_from ~migrate_to ;
  register_migration ~kind:"wasm_2_0_0" ~migrate_from ~migrate_to
