(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component: Storage snapshot
   Invocation: dune exec tezt/tests/main.exe -- -f storage_snapshots.ml
   Subject: Tests both the snapshot mechanism and the store's behaviour
*)

let node_arguments = Node.[Synchronisation_threshold 0]

let pp_snapshot_export_format fmt v =
  Format.fprintf fmt "%s" (match v with Node.Tar -> "tar" | Raw -> "raw")

let pp_snapshot_history_mode fmt v =
  Format.fprintf
    fmt
    "%s"
    (match v with
    | Node.Rolling_history -> "rolling"
    | Node.Full_history -> "full")

let get_constants client =
  let* constants =
    Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
  in
  let preserved_cycles = JSON.(constants |-> "preserved_cycles" |> as_int) in
  let blocks_per_cycle = JSON.(constants |-> "blocks_per_cycle" |> as_int) in
  let max_op_ttl =
    JSON.(constants |-> "max_operations_time_to_live" |> as_int)
  in
  return (preserved_cycles, blocks_per_cycle, max_op_ttl)

let export_snapshot node ~export_level ~snapshot_dir ~history_mode
    ~export_format =
  Log.info
    "Exporting %a snapshot for %s at level %d"
    pp_snapshot_history_mode
    history_mode
    (Node.name node)
    export_level ;
  let filename =
    Format.asprintf
      "%s.%a.%a"
      (Node.name node)
      pp_snapshot_history_mode
      history_mode
      pp_snapshot_export_format
      export_format
  in
  let* () =
    Node.snapshot_export
      ~export_level
      ~history_mode
      ~export_format
      node
      (snapshot_dir // filename)
  in
  Log.info
    "Node %s exported %s (level %d, format %s)"
    (Node.name node)
    filename
    export_level
    (Format.asprintf "%a" pp_snapshot_export_format export_format) ;
  return filename

(* Checks that the node's head, checkpoint, savepoint and caboose are
   the expected ones *)
let check_consistency_after_import node ~expected_head ~expected_checkpoint
    ~expected_savepoint ~expected_caboose =
  Log.info "Checking node consistency for %s" (Node.name node) ;
  let* block_head = Node.RPC.call node @@ RPC.get_chain_block () in
  let level = JSON.(block_head |-> "header" |-> "level" |> as_int) in
  let* {level = checkpoint; _} =
    Node.RPC.call node @@ RPC.get_chain_level_checkpoint ()
  in
  let* {level = savepoint; _} =
    Node.RPC.call node @@ RPC.get_chain_level_savepoint ()
  in
  let* {level = caboose; _} =
    Node.RPC.call node @@ RPC.get_chain_level_caboose ()
  in
  Check.((level = expected_head) int) ~error_msg:"expected level = %R, got %L" ;
  Check.((checkpoint = expected_checkpoint) int)
    ~error_msg:"expected checkpoint = %R, got %L" ;
  Check.((savepoint = expected_savepoint) int)
    ~error_msg:"expected savepoint = %R, got %L" ;
  Check.((caboose = expected_caboose) int)
    ~error_msg:"expected caboose = %R, got %L" ;
  unit

let check_blocks_availability node ~history_mode ~head ~savepoint ~caboose =
  (* The metadata of genesis is available anyway *)
  Log.info "Checking blocks availability for %s" (Node.name node) ;
  let* (_ : RPC.block_metadata) =
    Node.RPC.call node @@ RPC.get_chain_block_metadata ~block:"0" ()
  in
  let iter_block_range_s a b f =
    Lwt_list.iter_s f (range a b |> List.rev |> List.map string_of_int)
  in
  let expect_no_metadata block =
    (* Expects success, as the header must be stored. *)
    let* (_ : JSON.t) =
      Node.RPC.(call node @@ get_chain_block_header ~block ())
    in
    (* Expects failure, as the metadata must not be stored. *)
    let* {body; code} =
      Node.RPC.(call_json node @@ get_chain_block_metadata ~block ())
    in
    (* In the client, attempting to retrieve missing metadata outputs:
       Command failed: Unable to find block *)
    Check.(
      (code = 500) ~__LOC__ int ~error_msg:"Expected HTTP status %R, got %L.") ;
    let error_id = JSON.(body |=> 0 |-> "id" |> as_string) in
    Check.(
      (error_id = "store.metadata_not_found")
        ~__LOC__
        string
        ~error_msg:"Expected error id %R, got %L") ;
    unit
  in
  let expect_metadata block =
    (* Expects success, as the metadata must be stored. *)
    let* (_ : RPC.block_metadata) =
      Node.RPC.call node @@ RPC.get_chain_block_metadata ~block ()
    in
    unit
  in
  let expect_no_block block =
    let* {code; _} = Node.RPC.(call_raw node @@ get_chain_block ~block ()) in
    (* In the client, attempting to retrieve an unknown block outputs:
       Did not find service *)
    Check.(
      (code = 404) ~__LOC__ int ~error_msg:"Expected HTTP status %R, got %L.") ;
    unit
  in
  let* () =
    match history_mode with
    | Node.Full_history ->
        iter_block_range_s 1 (savepoint - 1) @@ expect_no_metadata
    | Node.Rolling_history ->
        if caboose <> 0 then
          iter_block_range_s 1 (caboose - 1) @@ expect_no_block
        else unit
  in
  let* () = iter_block_range_s savepoint head @@ expect_metadata in
  unit

let sync_all_nodes cluster expected_level =
  let* _ =
    Lwt_list.map_p (fun node -> Node.wait_for_level node expected_level) cluster
  in
  Log.info "All nodes has caught up on level %d!" expected_level ;
  unit

let bake_blocks node client ~blocks_to_bake =
  Log.info "Baking a batch of %d blocks on %s" blocks_to_bake (Node.name node) ;
  repeat blocks_to_bake @@ fun () ->
  Client.bake_for_and_wait
    ~endpoint:(Node node)
    ~node
    ~minimal_timestamp:true
    client

let export_import_and_check node ~export_level ~history_mode ~export_format
    ~max_op_ttl =
  let export_dirs = List.map Temp.dir ["first_export"; "second_export"] in
  let* final_node =
    Lwt_list.fold_left_s
      (fun node snapshot_dir ->
        let* filename =
          export_snapshot
            node
            ~export_level
            ~snapshot_dir
            ~history_mode
            ~export_format
        in
        let fresh_node_name =
          Format.asprintf
            "%a_node_from_%s"
            pp_snapshot_history_mode
            history_mode
            filename
        in
        let* fresh_node =
          Node.init
            ~name:fresh_node_name
            ~snapshot:(snapshot_dir // filename, false)
            node_arguments
        in
        let* () = Node.wait_for_ready fresh_node in
        let expected_checkpoint, expected_savepoint, expected_caboose =
          match history_mode with
          | Node.Full_history -> (export_level, export_level, 0)
          | Node.Rolling_history ->
              (export_level, export_level, max 0 (export_level - max_op_ttl))
        in
        let* () =
          check_consistency_after_import
            fresh_node
            ~expected_head:export_level
            ~expected_checkpoint
            ~expected_savepoint
            ~expected_caboose
        in
        let* () =
          check_blocks_availability
            fresh_node
            ~history_mode
            ~head:export_level
            ~savepoint:expected_savepoint
            ~caboose:expected_caboose
        in
        let* () = Node.terminate fresh_node in
        return fresh_node)
      node
      export_dirs
  in
  let* () = Node.terminate final_node in
  unit

(* This test aims to:
   - start 3 nodes: an archive, a full and a rolling one,
   - bake few blocks using the archive node as a baker,
   - export all kinds of snapshots (in terms of both history mode and
     export format) from each node,
   - import those snapshots and start the fresh nodes accordingly,
   - check the consistency and data availability of those fresh nodes,
   - re-export from the fresh nodes to re-import and re-check the imported data. *)
let test_export_import_snapshots =
  Protocol.register_test
    ~__FILE__
    ~title:"storage snapshot export and import"
    ~tags:["storage"; "snapshot"; "export"; "import"; Tag.memory_4k]
  @@ fun protocol ->
  let archive_node =
    Node.create
      ~name:"archive_node"
      (node_arguments @ Node.[History_mode Archive])
  in
  let full_node =
    Node.create
      ~name:"full_node"
      (node_arguments @ Node.[History_mode (Full None)])
  in
  let rolling_node =
    Node.create
      ~name:"rolling_node"
      (node_arguments @ Node.[History_mode (Rolling None)])
  in
  let cluster = [archive_node; full_node; rolling_node] in
  Cluster.clique cluster ;
  let* () = Cluster.start ~public:true cluster in
  let* client = Client.init ~endpoint:(Node archive_node) () in
  let* () = Client.activate_protocol_and_wait ~protocol client in
  let* preserved_cycles, blocks_per_cycle, max_op_ttl = get_constants client in
  (* Bake enough blocks so that the rolling node caboose is not at the
     genesis anymore. To do so, we need to bake at least 3 cycles,
     after activating the protocol, i.e 3*8 = 24 blocks. *)
  let blocks_to_bake = (preserved_cycles + 1) * blocks_per_cycle in
  let* () = bake_blocks archive_node client ~blocks_to_bake in
  let* archive_level = Node.get_level archive_node in
  let* () = sync_all_nodes cluster archive_level in
  (* Terminate all nodes to save resources. Note: we may consider
     that exporting a snapshot from a node that is running is an
     actual usecase (such exports are already done in other tests). *)
  let* () = Lwt_list.iter_p Node.terminate cluster in
  let export_import_and_check =
    export_import_and_check ~max_op_ttl ~export_level:archive_level
  in
  let* () =
    Lwt_list.iter_s
      (fun export_format ->
        Lwt_list.iter_s
          (fun (history_mode, nodes) ->
            Lwt_list.iter_s
              (fun node ->
                export_import_and_check node ~history_mode ~export_format)
              nodes)
          [
            (Node.Full_history, [archive_node; full_node]);
            (Node.Rolling_history, [archive_node; full_node; rolling_node]);
          ])
      Node.[Tar; Raw]
  in
  unit

let wait_for_complete_merge node target =
  let wait_for_starting_merge target =
    let filter json =
      let level = JSON.(json |-> "stop" |> as_int) in
      if level = target then Some () else None
    in
    Node.wait_for node "start_cementing_blocks.v0" filter
  in
  let wait_for_ending_merge () =
    Node.wait_for node "end_merging_stores.v0" @@ fun _json -> Some ()
  in
  let* () = wait_for_starting_merge target in
  wait_for_ending_merge ()

(* This test aims to export and import a rolling snapshot, bake some
   blocks and make sure that the checkpoint, savepoint and caboose are
   well dragged. *)
let test_drag_after_rolling_import =
  Protocol.register_test
    ~__FILE__
    ~title:"storage snapshot drag after rolling import"
    ~tags:["storage"; "snapshot"; "drag"; "rolling"; "import"]
  @@ fun protocol ->
  let archive_node =
    Node.create
      ~name:"archive_node"
      (node_arguments @ Node.[History_mode Archive])
  in
  (* The number of additional cycles is expected to reflect the
     default value from
     Shell_services.History_modes.default_additional_cycles. *)
  let additional_cycles = 1 in
  let rolling_node =
    Node.create
      ~name:"rolling_node"
      (node_arguments @ Node.[History_mode (Rolling (Some additional_cycles))])
  in
  let cluster = [archive_node; rolling_node] in
  Cluster.clique cluster ;
  let* () = Cluster.start ~public:true cluster in
  let* client = Client.init ~endpoint:(Node archive_node) () in
  let* () = Client.activate_protocol_and_wait ~protocol client in
  let* preserved_cycles, blocks_per_cycle, max_op_ttl = get_constants client in
  Log.info "Baking a few blocks"
  (* Baking enough blocks so that the caboose is not the genesis
     anymore (depending on the max_op_ttl)*) ;
  let blocks_to_bake = (1 * blocks_per_cycle) + max_op_ttl in
  let* () = bake_blocks archive_node client ~blocks_to_bake in
  let* archive_level = Node.get_level archive_node in
  let* () = sync_all_nodes [archive_node; rolling_node] archive_level in
  let* export_level = Node.get_level archive_node in
  let snapshot_dir = Temp.dir "snapshots_exports" in
  let history_mode = Node.Rolling_history in
  Log.info "Exporting snapshot at level %d" export_level ;
  let* filename =
    export_snapshot
      rolling_node
      ~export_level
      ~snapshot_dir
      ~history_mode
      ~export_format:Node.Tar
  in
  let fresh_node_name =
    Format.asprintf
      "%a_node_from_%s"
      pp_snapshot_history_mode
      history_mode
      filename
  in
  let* fresh_node =
    Node.init
      ~name:fresh_node_name
      ~snapshot:(snapshot_dir // filename, false)
      node_arguments
      ~event_sections_levels:[("node.store", `Info)]
  in
  (* Baking a few blocks so that the caboose is not the genesis
     anymore. *)
  let blocks_to_bake =
    (preserved_cycles + additional_cycles) * blocks_per_cycle
  in
  let expected_checkpoint, expected_savepoint, expected_caboose =
    (export_level, export_level, max 0 (export_level - max_op_ttl))
  in
  let* () =
    check_consistency_after_import
      fresh_node
      ~expected_head:export_level
      ~expected_checkpoint
      ~expected_savepoint
      ~expected_caboose
  in
  let* () =
    check_blocks_availability
      fresh_node
      ~history_mode
      ~head:export_level
      ~savepoint:expected_savepoint
      ~caboose:expected_caboose
  in
  let* () = bake_blocks archive_node client ~blocks_to_bake in
  let* expected_head = Node.get_level archive_node in
  let wait_for_merge_at_checkpoint =
    (* Waiting for the last cycle to be cemented before checking
       store's invariants. *)
    let expected_checkpoint =
      expected_head - (preserved_cycles * blocks_per_cycle)
    in
    wait_for_complete_merge fresh_node expected_checkpoint
  in
  let* () = Client.Admin.connect_address ~peer:fresh_node client in
  let* (_ : int) = Node.wait_for_level fresh_node expected_head in
  let* () = wait_for_merge_at_checkpoint in
  let expected_checkpoint, expected_savepoint, expected_caboose =
    match history_mode with
    | Node.Full_history -> Test.fail "testing only rolling mode"
    | Node.Rolling_history ->
        let checkpoint =
          expected_head - (preserved_cycles * blocks_per_cycle)
        in
        let savepoint =
          expected_head
          - ((preserved_cycles + additional_cycles) * blocks_per_cycle)
        in
        (checkpoint, savepoint, min savepoint (checkpoint - max_op_ttl))
  in
  let* () =
    check_consistency_after_import
      fresh_node
      ~expected_head
      ~expected_checkpoint
      ~expected_savepoint
      ~expected_caboose
  in
  (* Restart the node to invalidate the cache and avoid false
     positives. *)
  let* () = Node.terminate fresh_node in
  let* () = Node.run fresh_node node_arguments in
  let* () = Node.wait_for_ready fresh_node in
  let* () =
    check_blocks_availability
      fresh_node
      ~history_mode
      ~head:export_level
      ~savepoint:expected_savepoint
      ~caboose:expected_caboose
  in
  unit

(* Checks that the hash, level and version contained in the snapshot
   is consistent with regard to the exported data. *)
let test_info_command =
  Protocol.register_test
    ~__FILE__
    ~title:(Format.asprintf "storage snapshot info command")
    ~tags:["storage"; "snapshot"; "info"; "command"]
  @@ fun protocol ->
  let* node = Node.init ~name:"node" node_arguments in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Client.activate_protocol_and_wait ~protocol ~node client in
  let blocks_to_bake = 8 in
  let* () = bake_blocks node client ~blocks_to_bake in
  let* head = Node.RPC.call node @@ RPC.get_chain_block () in
  let head_level = JSON.(head |-> "header" |-> "level" |> as_int) in
  let snapshot_dir = Temp.dir "snapshots_exports" in
  let* filename =
    export_snapshot
      node
      ~export_level:head_level
      ~snapshot_dir
      ~history_mode:Node.Rolling_history
      ~export_format:Node.Tar
  in
  let expected_hash = JSON.(head |-> "hash" |> as_string) in
  let expected_level = head_level in
  (* This is expected to be updated as soon as a new snapshot version
     is released (referring to the Snapshot.Version.current_version
     from `lib_store/unix/snapshots`)*)
  let expected_version = 7 in
  Log.info "Checks the human formatted output" ;
  (* Get the info line, which is the second line. *)
  let* () =
    let p = Node.spawn_snapshot_info node (snapshot_dir // filename) in
    let* human_output = Process.check_and_read_stdout p in
    let human_output = List.nth (String.split_on_char '\n' human_output) 1 in
    let re =
      rex
        "chain \\w+, block hash (\\w+) at level (\\d+),.* \\(snapshot version \
         (\\d+)\\)"
    in
    match human_output =~*** re with
    | Some (hash, level, version) ->
        Check.(
          (hash = expected_hash)
            string
            ~error_msg:"expected block hash %R, got %L" ;
          (int_of_string level = expected_level)
            int
            ~error_msg:"expected block level %R, got %L" ;
          (int_of_string version = expected_version)
            int
            ~error_msg:"expected version \"%R\", got %L") ;
        unit
    | None -> Test.fail "Could not parse output %s" human_output
  in
  Log.info "Checks the JSON formatted output" ;
  let* () =
    let p =
      Node.spawn_snapshot_info ~json:true node (snapshot_dir // filename)
    in
    let* json_output = Process.check_and_read_stdout p in
    let json_output =
      JSON.parse ~origin:"node snapshot info --json" json_output
    in
    let json_snapshot_header = JSON.(json_output |-> "snapshot_header") in
    let hash = JSON.(json_snapshot_header |-> "block_hash" |> as_string) in
    let level = JSON.(json_snapshot_header |-> "level" |> as_int) in
    let version = JSON.(json_snapshot_header |-> "version" |> as_int) in
    Check.((hash = expected_hash) string)
      ~error_msg:"expected block hash %R not found" ;
    Check.((level = expected_level) int)
      ~error_msg:"expected block level %R not found" ;
    Check.((version = expected_version) int)
      ~error_msg:"expected version \"%R\" not found" ;
    unit
  in
  unit

let register ~protocols =
  test_export_import_snapshots protocols ;
  test_drag_after_rolling_import protocols ;
  test_info_command protocols
