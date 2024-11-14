(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component: RPC
   Invocation: REF_DIR=<reference_dir> BOOT_SNAP=<bootstrap_dir> dune exec
     tezt/manual_tests/main.exe -- --file non_blocking_rpc.ml --info
   Subject: Benchmark the non_blocking RPC feature. This test shows that
            the non blocking RPC feature unblocks the progres of a boostrapping
            node while being bombarded by heavy RPCs.

   REF_DIR
      Points to a directory for a node which is at a particular
      level L which is higher than the level from which the other two nodes will
      start from. The test was run using the directory from the Google Drive from
      https://drive.google.com/drive/folders/1NajNOwLbCGuHW1MklM_Xh7Jyo0asDIR7,
      using "mainnet_5_610_000-10k_contexts.tar.gz".

   BOOT_SNAP
      Points to a rolling snapshot which is at a level L', which
      is smaller than L by enough blocks such that after the test finishes, no block
      from the two will be able to reach L. The test was run with a rolling snapshot
      pointing to the level 5_600_000 on "mainnet".

   Issue: https://gitlab.com/tezos/tezos/-/issues/7299
   This is a temporary solution, the issue states that we plan to move this to
   `tezt/long_tests`.
*)
(* Scenario
   --------
   There are 3 nodes in this scenario:
   1. "REF_NODE": a reference node which will be at some level L, started from a REFERENCE_DIR
   directory, with enough context and history for nodes to be bootstrapped against it;
   2. "LOCAL_NODE": a node which will be bootstrapped starting from an older level L', which
   will have a local RPC port
   3. "EXTERNAL_NODE": a node which will be bootstrapped starting from the same level L', which
   will have an external RPC port

   There is also a process [RPC_spammer] which is responsible to call the two bootstrapping
   nodes with heavy RPCs to try to delay their progress as much as possible.

   Level L:                          REF_NODE
                                       /\
   (Bootstrapping)                    /  \
                                     /    \
   Level L_2:                       /  EXTERNAL_NODE
                                   /         /
                                  /         /
   Level L_1:                 LOCAL_NODE   /
                                     \    /
   (RPC spam)                         \  /
                                       \/
                                   RPC_spammer

   After a fixed amount of time, we stop the two nodes and check how far they got to. The scenario
   is meant to showcase that the second node [EXTERNAL_NODE] was able to advance further, due to the fact
   that the node was not stuck during its bootstrapping process, unlike [LOCAL_NODE] which had to serve
   the requests from [RPC_spammer] most of the time. We want to show that L_1 < L_2, consequently.
*)

let write_file file json =
  Lwt_io.with_file ~mode:Output file (fun chan ->
      let str = Data_encoding.Json.to_string ~minify:false json in
      Lwt_io.write chan str)

(* Hardcoded identities to speed up test. *)
let get_hardcoded_identity = function
  | `REF ->
      "{\"peer_id\":\"idsD15KLaWb3yQ6jHUeTFtcc4JRrgt\",\"public_key\":\"734abdbdb78159abca48b93784b945452b68e904c0e5af0e113b7255f5284509\",\"secret_key\":\"d96f9a92f0493dde2e663327b74b0deac28caf1fa6d05e76c848e42695f36a04\",\"proof_of_work_stamp\":\"b71136f9725249ced9a21aa472b337800e7283ed49c02e18\"}"
  | `EXT ->
      "{\"peer_id\":\"idtA1yqrudZmhxk1EMBfQ4KCureSud\",\"public_key\":\"6a9e8dfb9b6c7ae42f10f66bff516527e90932b58b0173ab73df60adfdb81739\",\"secret_key\":\"d576ae516f2896b718f6fdf8170ed1033f26d61a17cd3ae7197c781218263993\",\"proof_of_work_stamp\":\"9389cc4c199c0e500c0b673844ec5fc00f6b06946e7e7df0\"}"
  | `LOC ->
      "{\"peer_id\":\"idt9A7cMHPzxzD6uJ76hjJHYtcD88B\",\"public_key\":\"34b9fd2af7e925c6736fe9e97fa92e498c1bf6f775f5c2edf7c2a60970197e15\",\"secret_key\":\"ab6c3723808032b053445d53f91652382b72f5e6aab838fc84d7f4b47e21be74\",\"proof_of_work_stamp\":\"50363c2170dfae00cbbc43edb74e723792c63777f98c5aa7\"}"
  | _ -> Test.fail "Cannot find requested identity"

let write_identity ~path id =
  let identity = get_hardcoded_identity id in
  match Data_encoding.Json.from_string identity with
  | Error e -> Test.fail "Cannot convert to JSON: %s" e
  | Ok json -> write_file (Filename.concat path "identity.json") json

let reference_dir () =
  match Sys.getenv_opt "REF_DIR" with
  | Some v -> v
  | None -> Test.fail "cannot find reference directory"

let bootstrapping_snapshot () =
  match Sys.getenv_opt "BOOT_SNAP" with
  | Some v -> v
  | None -> Test.fail "cannot find bootstrapping snapshot"

(* The nodes in this experiment will:
   - run on `mainnet`
   - have 0 connections when they run in an isolated environment where
   we can query them for statistical data, or have a non-zero number of
   connections when we want them to progress
   - have a synchronisation threshold of 1
   - generate a fresh identity (level 26 for mainnet)
   - run in private mode such that the network is not public
   - not have any bootstrap peer, as they do not need to. *)
let make_args connections : Node.argument list =
  [
    Network "mainnet";
    Connections connections;
    Synchronisation_threshold 1;
    Expected_pow 26;
    Private_mode;
    No_bootstrap_peers;
  ]

(** [spawn_reference_node ~reference_dir] creates the node which will be at a
    highest level in the network, from which the bootstrapping nodes will obtain
    their blocks.

    Raises exception if [~reference_dir] does not exist. *)
let spawn_reference_node ~reference_dir =
  if Sys.is_directory reference_dir then
    let node =
      Node.create ~name:"REF_NODE" ~data_dir:reference_dir (make_args 2)
    in
    let* () = write_identity ~path:(Node.data_dir node) `REF in
    Lwt.return node
  else Test.fail "Provided directory for reference node does not exist"

(** [spawn_bootstrapping_node ~node_name ~snapshot_uri ~reference_node]
    creates a node with name [~node_name] for debugging purposes, from
    the snapshot provided at [~snapshot_uri] and it adds a [~reference_node]
    as its single peer, from which it obtains the next blocks; [~rpc_external]
    is used to distinguish between having an external RPC process or not. *)
let spawn_bootstrapping_node ~node_name ~snapshot_uri ~reference_node
    ~rpc_external =
  let node = Node.create ~name:node_name ~rpc_external (make_args 2) in
  let* () = Node.config_init node [] in
  let* () =
    write_identity
      ~path:(Node.data_dir node)
      (if rpc_external then `EXT else `LOC)
  in
  let* () = Node.snapshot_import ~no_check:true node snapshot_uri in
  let () = Cluster.symmetric_add_peer reference_node node in
  return node

(** [rpc_spammer bootstrap_node] first waits for the [bootstrap_node] to be
    ready to accept RPC calls, and then, while it is in the process of
    bootstrapping, makes a list of heavy RPC calls. *)
let rpc_spammer bootstrap_node =
  Log.info "Waiting for node to be ready for RPC calls..." ;
  let* () = Node.wait_for_ready bootstrap_node in
  let rec rpc_loop () =
    let rpc_calls_list =
      [
        (* GET /chains/<chain>/blocks/<block>/context/contracts *)
        RPC.get_chain_block_context_contracts ();
        (* GET /chains/<chain>/blocks/<block>/context/big_maps/<id> -> ~182 MB *)
        RPC.get_chain_block_context_big_maps ~id:"399689" ();
        (* GET /chains/<chain>/blocks/<block>/helpers/baking_rights *)
        RPC.get_chain_block_helper_baking_rights ();
        (* GET /chains/<chain>/blocks/<block>/context/big_maps/<id> -> ~1.98 MB *)
        RPC.get_chain_block_context_big_maps ~id:"3943" ();
        (* GET /chains/<chain>/blocks/<block>/helpers/attestation_rights *)
        RPC.get_chain_block_helper_attestation_rights ();
        (* GET /chains/<chain>/blocks/<block>/context/big_maps/<id> -> ~21.2 MB *)
        RPC.get_chain_block_context_big_maps ~id:"9923" ();
      ]
    in
    let endpoint = Node.as_rpc_endpoint bootstrap_node in
    Log.info "Make RPC calls on endpoint: %s" (Endpoint.as_string endpoint) ;
    let* _responses =
      Lwt_list.map_s
        (RPC_core.call_raw ~log_response_body:false endpoint)
        rpc_calls_list
    in
    Log.info "RPC calls on port %s succeeded." (Endpoint.as_string endpoint) ;
    rpc_loop ()
  in
  rpc_loop ()

(** [get_isolated_progress ~bootstrap_node] runs the [~bootstrap_node]
    node in an isolated environment, to gather its progress, which in this case
    is the current level and returns it.*)
let get_isolated_progress ~bootstrap_node =
  let* () = Node.run bootstrap_node (make_args 0) in
  let* () = Node.wait_for_ready bootstrap_node in
  let* node_level =
    RPC_core.call
      ~log_response_body:false
      (Node.as_rpc_endpoint bootstrap_node)
      (RPC.get_chain_block_helper_current_level ())
  in
  let* () = Node.terminate bootstrap_node in
  Lwt.return node_level.level

(** [benchmark_bootstrap_node ~reference_node ~node_name ~rpc_external] runs
    the benchmarking scenario for each node in the network; the scenario is
    to create a node with name [~node_name], bootstrap it against [~reference_node];
    the scenario focuses on whether the node has an external RPC process or not,
    as indicated by [~rpc_external]. *)
let benchmark_bootstrap_node ~reference_node ~node_name ~rpc_external =
  let* bootstrap_node =
    spawn_bootstrapping_node
      ~node_name
      ~snapshot_uri:(bootstrapping_snapshot ())
      ~reference_node
      ~rpc_external
  in
  Log.info "Start bootstrapping for %s" node_name ;
  let* () =
    Lwt_list.iter_p
      (fun node ->
        let* () = Node.run node (make_args 2) in
        Node.wait_for_ready node)
      [reference_node; bootstrap_node]
  in
  (* Run the benchmarking for each node *)
  let timeout = 300. in
  let* () = Lwt.pick [rpc_spammer bootstrap_node; Lwt_unix.sleep timeout] in
  let* () = Lwt_list.iter_p Node.terminate [reference_node; bootstrap_node] in
  get_isolated_progress ~bootstrap_node

let get_progress_after_bootstrapping () =
  Test.register
    ~__FILE__
    ~title:"Check progress made by node during bootstrapping"
    ~tags:["get"; "node"; "progress"]
  @@ fun () ->
  let* reference_node =
    spawn_reference_node ~reference_dir:(reference_dir ())
  in
  let* external_level =
    benchmark_bootstrap_node
      ~reference_node
      ~node_name:"EXTERNAL_NODE"
      ~rpc_external:true
  in
  let* local_level =
    benchmark_bootstrap_node
      ~reference_node
      ~node_name:"LOCAL_NODE"
      ~rpc_external:false
  in
  Log.info "Local RPC at level %d" local_level ;
  Log.info "External RPC at level %d" external_level ;
  Log.info
    "Bootstrap efficiency: %d - %d = %d levels"
    external_level
    local_level
    (external_level - local_level) ;
  Lwt.return_unit

let register () = get_progress_after_bootstrapping ()
