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
   Component: Snapshot and store
   Invocation: dune exec tezt/tests/main.exe -- -f multinode_snapshot.ml
   Subject: Tests both the snapshot mechanism and the store's behaviour
*)

let batch_1 = 48

(* not enough bakes to drag the savepoint after snapshot import *)
let batch_2 = 48

(* enough bakes to drag the savepoint *)
let batch_3 = 32

let blocks_per_cycle = 8

let preserved_cycles = 2

let retained_cycles = 8

let node_arguments = Node.[Synchronisation_threshold 0]

let get_head_max_op_ttl node =
  let* head = RPC.call node @@ RPC.get_chain_block_metadata () in
  return head.max_operations_ttl

(*

Tests both the snapshot mechanism and the store's behaviour

TL;DR, how it works:
- bake few blocks using all history modes
- export all kinds of snapshots
- import all kinds of snapshots
- check consistency (the snapshot's window includes genesis)
- bake a few blocks
- check consistency (the checkpoints should not move yet)
- bake a few blocks
- check consistency (the checkpoints should have moved)
- export all kinds of snapshots
- import all kinds of snapshots
- check consistency (checkpoints should be still valid)
- bake a few blocks
- check consistency (the checkpoints should have moved)

*)
let test_storage_snapshot =
  Protocol.register_test
    ~__FILE__
    ~title:"storage snapshot, import and export"
    ~tags:["storage"; "snapshot"; "import"; "export"]
  @@ fun protocol ->
  let show_node_group ns =
    sf "[%s]" (String.concat ", " @@ List.map (fun n -> Node.name n) ns)
  in

  let bake_batch batch_idx baker_node baker_client node_group batch_size =
    Log.info
      "Bake batch %d using %s (%d blocks)"
      batch_idx
      (Node.name baker_node)
      batch_size ;

    let level_before = Node.get_level baker_node in
    let final_level = level_before + batch_size in

    let group_level_promise =
      List.map
        (fun node -> (node, Node.wait_for_level node final_level))
        node_group
    in

    let* () =
      repeat batch_size @@ fun () ->
      let* () =
        Client.bake_for_and_wait
          ~endpoint:(Node baker_node)
          ~node:baker_node
          ~minimal_timestamp:true
          baker_client
      in
      unit
    in

    Log.info
      "Waiting for nodes %s to catch up to level %d"
      (show_node_group node_group)
      final_level ;
    let* () =
      Lwt_list.iter_p
        (fun (node, p) ->
          let* (_ : int) = p in
          Log.info "%s catched up" (Node.name node) ;
          unit)
        group_level_promise
    in
    Log.info "All catched up!" ;

    return final_level
  in

  let snapshot_dir = Temp.dir "multinode_snapshots" in

  let export node history_mode ~export_level ~snapshot =
    let file = snapshot_dir // snapshot in
    let* () = Node.snapshot_export ~export_level ~history_mode node file in
    Log.info "Node %s exported %s" (Node.name node) file ;
    unit
  in

  let create_or_reset mode ~snapshot =
    let snapshot = snapshot_dir // snapshot in
    let* node_name =
      match mode with
      | `New name -> return ("node_" ^ name)
      | `Reset old_node ->
          let* () = Node.terminate old_node in
          return (Node.name old_node ^ "'")
    in
    let* node =
      Node.init ~name:node_name ~snapshot:(snapshot, false) node_arguments
    in
    Log.info
      "%s node %s and imported %s"
      (match mode with `New _ -> "Created" | `Reset _ -> "Reset")
      (Node.name node)
      snapshot ;
    return node
  in

  let restart node =
    let* () = Node.terminate node in
    let* () = Node.run node node_arguments in
    let* () = Node.wait_for_ready node in
    unit
  in

  (* Checks that the node's checkpoint, savepoint and caboose are the *)
  (* expected ones after a snapshot import. *)
  let node_consistency_after_import node ~expected_level ~expected_checkpoint
      ~expected_savepoint ~expected_caboose =
    let* block_head = RPC.call node @@ RPC.get_chain_block () in
    let level = JSON.(block_head |-> "header" |-> "level" |> as_int) in

    let* {level = checkpoint; _} =
      RPC.call node @@ RPC.get_chain_level_checkpoint ()
    in
    let* {level = savepoint; _} =
      RPC.call node @@ RPC.get_chain_level_savepoint ()
    in
    let* {level = caboose; _} =
      RPC.call node @@ RPC.get_chain_level_caboose ()
    in
    Check.((level = expected_level) int)
      ~error_msg:"expected level = %R, got %L" ;
    Check.((checkpoint = expected_checkpoint) int)
      ~error_msg:"expected checkpoint = %R, got %L" ;
    Check.((savepoint = expected_savepoint) int)
      ~error_msg:"expected savepoint = %R, got %L" ;
    Check.((caboose = expected_caboose) int)
      ~error_msg:"expected caboose = %R, got %L" ;
    (* Check that the metadata of genesis is available *)
    let* (_ : JSON.t) = RPC.call node @@ RPC.get_chain_block ~block:"0" () in
    unit
  in

  let iter_block_range_s a b f =
    Lwt_list.iter_s f (range a b |> List.rev |> List.map string_of_int)
  in

  (* Checks the availability of blocks and its metadata for a full node. *)
  (* We assume that: *)
  (* - genesis is available with metadata *)
  (* - all block headers are available, *)
  (* - blocks before the savepoint (excluded) have pruned metadata, *)
  (* - blocks from the savepoint (included) have metadata. *)
  let full_node_blocks_availability node ~savepoint ~head_level =
    (* The metadata of genesis is available *)
    let* (_ : JSON.t) = RPC.call node @@ RPC.get_chain_block ~block:"0" () in
    let* () =
      iter_block_range_s 1 (savepoint - 1) @@ fun block ->
      let* (_ : JSON.t) =
        RPC.call node @@ RPC.get_chain_block_header ~block ()
      in
      (* Expect failure *)
      let* {body; code} =
        RPC.call_json node @@ RPC.get_chain_block_metadata ~block ()
      in
      (* In the client, attempting to retrieve missing metadata outputs:

         Command failed: Unable to find block
      *)
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
    iter_block_range_s savepoint head_level @@ fun block ->
    let* (_ : RPC.block_metadata) =
      RPC.call node @@ RPC.get_chain_block_metadata ~block ()
    in
    unit
  in

  (*  Checks the availability of blocks and its metadata for a rolling node. *)
  (*  We assume that: *)
  (*  - genesis is available with metadata *)
  (*  - blocks before caboose (excluded) are unknown, *)
  (*  - blocks from the caboose (included) and before the savepoint (excluded) *)
  (*    have pruned metadata, *)
  (*  - blocks after the savepoint (included) have metadata. *)
  let rolling_node_blocks_availability node ~savepoint ~caboose ~head_level =
    (* the metadata of genesis is available *)
    let* (_ : JSON.t) = RPC.call node @@ RPC.get_chain_block ~block:"0" () in
    let* () =
      if caboose <> 0 then (
        iter_block_range_s 1 (caboose - 1) @@ fun block ->
        let* {code; _} = RPC.call_raw node @@ RPC.get_chain_block ~block () in
        (* In the client, attempting to retrieve an unknown block outputs:

             Did not find service
        *)
        Check.(
          (code = 404)
            ~__LOC__
            int
            ~error_msg:"Expected HTTP status %R, got %L.") ;
        unit)
      else unit
    in
    iter_block_range_s (savepoint + 1) head_level @@ fun block ->
    let* (_ : JSON.t) = RPC.call node @@ RPC.get_chain_block_header ~block () in
    unit
  in

  let node_archive =
    Node.create
      ~name:"node_archive"
      (node_arguments @ Node.[History_mode Archive])
  in
  let node_full =
    Node.create
      ~name:"node_full"
      (node_arguments @ Node.[History_mode (Full None)])
  in
  let node_rolling =
    Node.create
      ~name:"node_rolling"
      (node_arguments @ Node.[History_mode (Rolling None)])
  in
  let nodes_cluster1 = [node_archive; node_full; node_rolling] in
  Cluster.clique nodes_cluster1 ;
  let* () = Cluster.start ~public:true nodes_cluster1 in
  let* client = Client.init () in

  let nodes_group1 = [node_archive; node_full; node_rolling] in

  let* () =
    Client.activate_protocol ~endpoint:(Node node_archive) ~protocol client
  in

  (* Bake a few blocks *)
  let* head_level = bake_batch 1 node_archive client nodes_group1 batch_1 in
  let snapshot_level = head_level in

  (* ########################################################################### *)
  Log.info "Export all kinds of snapshots" ;
  let* () =
    export
      node_archive
      Full_history
      ~export_level:snapshot_level
      ~snapshot:"node_archive_batch_1.full"
  and* () =
    export
      node_archive
      Rolling_history
      ~export_level:snapshot_level
      ~snapshot:"node_archive_batch_1.rolling"
  and* () =
    export
      node_full
      Full_history
      ~export_level:snapshot_level
      ~snapshot:"node_full_batch_1.full"
  and* () =
    export
      node_full
      Rolling_history
      ~export_level:snapshot_level
      ~snapshot:"node_full_batch_1.rolling"
  and* () =
    export
      node_rolling
      Rolling_history
      ~export_level:snapshot_level
      ~snapshot:"node_rolling_batch_1.rolling"
  in

  Log.info "Import all kinds of snapshots" ;

  let* node_full_2 =
    (* New node: 3 *)
    create_or_reset (`New "full_2") ~snapshot:"node_archive_batch_1.full"
  and* node_rolling_2 =
    (* New node: 4 *)
    create_or_reset (`New "rolling_2") ~snapshot:"node_archive_batch_1.rolling"
  and* node_full =
    (* Reset node 1 *)
    create_or_reset (`Reset node_full) ~snapshot:"node_full_batch_1.full"
  and* node_rolling_3 =
    (* New node: 5 *)
    create_or_reset (`New "rolling_3") ~snapshot:"node_full_batch_1.rolling"
  and* node_rolling =
    (* Reset node 2 *)
    create_or_reset
      (`Reset node_rolling)
      ~snapshot:"node_rolling_batch_1.rolling"
  in
  let nodes_group2 =
    [
      node_archive;
      node_full;
      node_rolling;
      node_full_2;
      node_rolling_2;
      node_rolling_3;
    ]
  in

  let connect_clique ns =
    Lwt_list.iteri_s
      (fun index peer ->
        Lwt_list.iter_s
          (fun peer' ->
            Log.debug "Connecting %s to %s" (Node.name peer) (Node.name peer') ;
            Client.Admin.connect_address
              ~endpoint:(Node peer)
              ~peer:peer'
              client)
          (drop (index + 1) ns))
      ns
  in

  Log.info "Group 2: Connect nodes %s in clique" (show_node_group nodes_group2) ;
  let* () = connect_clique nodes_group2 in

  (* ########################################################################### *)
  Log.info "Check consistency 1" ;

  (*
    Check consistency of imported snapshots
    Do not factorize calls to ease debugging
   *)

  (* For the full nodes *)

  (* test_node_full_consistency_1 *)
  let* () =
    let node = node_full in
    Log.info "Check consistency (node_full, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = snapshot_level in
    let expected_checkpoint = expected_level in
    let expected_savepoint = expected_checkpoint in
    let expected_caboose = 0 in

    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      full_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~head_level:expected_level
    in
    unit
  in
  (* test_node_full_2_consistency_1 *)
  let* () =
    let node = node_full_2 in
    Log.info "Check consistency (node_full_2, %s)" (Node.name node) ;
    let* () = restart node in
    let expected_level = snapshot_level in
    let expected_checkpoint = expected_level in
    let expected_savepoint = expected_checkpoint in
    let expected_caboose = 0 in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      full_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~head_level:expected_level
    in
    unit
  in
  (* For the rolling nodes *)
  (* test_node_rolling_consistency_1 *)
  let* () =
    let node = node_rolling in
    Log.info "Check consistency (node_rolling, %s)" (Node.name node) ;
    let* () = restart node in
    let expected_level = snapshot_level in
    let expected_checkpoint = expected_level in
    let expected_savepoint = expected_checkpoint in
    let expected_caboose = 0 in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in
  (* test_node_rolling_2_consistency_1 *)
  let* () =
    let node = node_rolling_2 in
    Log.info "Check consistency (node_rolling_2, %s)" (Node.name node) ;
    let* () = restart node in
    let expected_level = snapshot_level in
    let expected_checkpoint = expected_level in
    let expected_savepoint = expected_checkpoint in
    let expected_caboose = 0 in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in
  (* test_node_rolling_3_consistency_1 *)
  let* () =
    let node = node_rolling_3 in
    Log.info "Check consistency (node_rolling_3, %s)" (Node.name node) ;
    let* () = restart node in
    let expected_level = snapshot_level in
    let expected_checkpoint = expected_level in
    let expected_savepoint = expected_checkpoint in
    let expected_caboose = 0 in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in

  (* ########################################################################### *)
  (* Bake a few blocks *)
  let* head_level = bake_batch 2 node_archive client nodes_group2 batch_2 in

  (* ########################################################################### *)
  Log.info "Check consistency 2: imported snapshots after > 5 baked cycles" ;
  Log.info
    "The savepoints of full and rolling nodes **have not** been dragged yet" ;

  (* For the full nodes *)

  (* test_node_full_consistency_2 *)
  let* () =
    let node = node_full in
    Log.info "Check consistency 2 (node_full, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    let expected_checkpoint =
      expected_level - (preserved_cycles * blocks_per_cycle)
    in
    let savepoint_when_imported = snapshot_level in
    let expected_savepoint = savepoint_when_imported in
    let expected_caboose = 0 in

    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      full_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~head_level:expected_level
    in
    unit
  in

  (* For the full nodes *)
  (* test_node_full_2_consistency_2 *)
  let* () =
    let node = node_full_2 in
    Log.info "Check consistency 2 (node_full_2, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint =
      expected_level - (preserved_cycles * blocks_per_cycle)
    in
    let savepoint_when_imported = snapshot_level in
    let expected_savepoint = savepoint_when_imported in
    let expected_caboose = 0 in

    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      full_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~head_level:expected_level
    in
    unit
  in

  (* For the rolling nodes *)

  (* The caboose of rolling mode were no dragged yet as *)
  (* (checkpoint - max_op_ttl(head)) < savepoint *)

  (* test_node_rolling_consistency_2 *)
  let* () =
    let node = node_rolling in
    Log.info "Check consistency 2 (node_rolling, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint =
      expected_level - (preserved_cycles * blocks_per_cycle)
    in
    let savepoint_when_imported = snapshot_level in
    let expected_savepoint = savepoint_when_imported in
    let* expected_caboose =
      let* max_op_ttl = get_head_max_op_ttl node in
      return @@ max (expected_checkpoint - max_op_ttl) 0
    in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in

  (* test_node_rolling_2_consistency_2 *)
  let* () =
    let node = node_rolling_2 in
    Log.info "Check consistency 2 (node_rolling_2, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint =
      expected_level - (preserved_cycles * blocks_per_cycle)
    in
    let savepoint_when_imported = snapshot_level in
    let expected_savepoint = savepoint_when_imported in
    let* expected_caboose =
      let* max_op_ttl = get_head_max_op_ttl node in
      return @@ max (expected_checkpoint - max_op_ttl) 0
    in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in

  (* test_node_rolling_3_consistency_2 *)
  let* () =
    let node = node_rolling_3 in
    Log.info "Check consistency 2 (node_rolling_3, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint =
      expected_level - (preserved_cycles * blocks_per_cycle)
    in
    let savepoint_when_imported = snapshot_level in
    let expected_savepoint = savepoint_when_imported in
    let* expected_caboose =
      let* max_op_ttl = get_head_max_op_ttl node in
      return @@ max (expected_checkpoint - max_op_ttl) 0
    in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in

  (* ########################################################################### *)
  (* Bake a few blocks *)
  let* head_level = bake_batch 3 node_archive client nodes_group2 batch_3 in
  let snapshot_level = head_level in

  (* ########################################################################### *)
  Log.info "Check consistency 3: imported snapshots after > 5 baked cycles" ;

  (* The savepoints of full and rolling nodes **have** been dragged *)

  (* For the full nodes *)

  (* test_node_full_consistency_3 *)
  let* () =
    let node = node_full in
    Log.info "Check consistency 3 (node_full, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint =
      expected_level - (preserved_cycles * blocks_per_cycle)
    in
    let expected_savepoint =
      expected_checkpoint - (retained_cycles * blocks_per_cycle)
    in
    let expected_caboose = 0 in

    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      full_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~head_level:expected_level
    in
    unit
  in

  (* test_node_full_2_consistency_3 *)
  let* () =
    let node = node_full_2 in
    Log.info "Check consistency 3 (node_full_2, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint =
      expected_level - (preserved_cycles * blocks_per_cycle)
    in
    let expected_savepoint =
      expected_checkpoint - (retained_cycles * blocks_per_cycle)
    in
    let expected_caboose = 0 in

    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      full_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~head_level:expected_level
    in
    unit
  in

  (* For the rolling nodes *)

  (* test_node_rolling_consistency_3 *)
  let* () =
    let node = node_rolling in
    Log.info "Check consistency 3 (node_rolling, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint =
      expected_level - (preserved_cycles * blocks_per_cycle)
    in
    let expected_savepoint =
      expected_checkpoint - (retained_cycles * blocks_per_cycle)
    in
    let* expected_caboose =
      let* max_op_ttl = get_head_max_op_ttl node in
      return @@ max (expected_checkpoint - max_op_ttl) 0
    in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in

  (* test_node_rolling_2_consistency_3 *)
  let* () =
    let node = node_rolling_2 in
    Log.info "Check consistency 3 (node_rolling_2, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint =
      expected_level - (preserved_cycles * blocks_per_cycle)
    in
    let expected_savepoint =
      expected_checkpoint - (retained_cycles * blocks_per_cycle)
    in
    let* expected_caboose =
      let* max_op_ttl = get_head_max_op_ttl node in
      return @@ max (expected_checkpoint - max_op_ttl) 0
    in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in

  (* test_node_rolling_3_consistency_3 *)
  let* () =
    let node = node_rolling_3 in
    Log.info "Check consistency 3 (node_rolling_3, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint =
      expected_level - (preserved_cycles * blocks_per_cycle)
    in
    let expected_savepoint =
      expected_checkpoint - (retained_cycles * blocks_per_cycle)
    in
    let* expected_caboose =
      let* max_op_ttl = get_head_max_op_ttl node in
      return @@ max (expected_checkpoint - max_op_ttl) 0
    in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in

  (* ########################################################################### *)
  Log.info "Re-export all kinds of snapshots" ;
  let* () =
    export
      node_archive
      Full_history
      ~export_level:snapshot_level
      ~snapshot:"node_archive_batch_3.full"
  and* () =
    export
      node_archive
      Rolling_history
      ~export_level:snapshot_level
      ~snapshot:"node_archive_batch_3.rolling"
  and* () =
    export
      node_full
      Full_history
      ~export_level:snapshot_level
      ~snapshot:"node_full_batch_3.full"
  and* () =
    export
      node_full
      Rolling_history
      ~export_level:snapshot_level
      ~snapshot:"node_full_batch_3.rolling"
  and* () =
    export
      node_rolling
      Rolling_history
      ~export_level:snapshot_level
      ~snapshot:"node_rolling_batch_3.rolling"
  in

  (* ########################################################################### *)
  Log.info "Re-import all kinds of snapshots" ;

  let* node_full_2 =
    (* Reset node: 3 *)
    create_or_reset (`Reset node_full_2) ~snapshot:"node_archive_batch_3.full"
  and* node_rolling_2 =
    (* Reset node: 4 *)
    create_or_reset
      (`Reset node_rolling_2)
      ~snapshot:"node_archive_batch_3.rolling"
  and* node_full =
    (* Reset node 1 *)
    create_or_reset (`Reset node_full) ~snapshot:"node_full_batch_3.full"
  and* node_rolling_3 =
    (* New node: 5 *)
    create_or_reset
      (`Reset node_rolling_3)
      ~snapshot:"node_full_batch_3.rolling"
  and* node_rolling =
    (* Reset node 2 *)
    create_or_reset
      (`Reset node_rolling)
      ~snapshot:"node_rolling_batch_3.rolling"
  in

  (* ########################################################################### *)
  Log.info "Check consistency 4: imported snapshots with > 5 cycles" ;

  (* For the full nodes *)

  (* test_node_full_consistency_4 *)
  let* () =
    let node = node_full in
    Log.info "Check consistency 4 (node_full, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint = expected_level in
    let expected_savepoint = expected_checkpoint in
    let expected_caboose = 0 in

    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      full_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~head_level:expected_level
    in
    unit
  in

  (* test_node_full_2_consistency_4 *)
  let* () =
    let node = node_full_2 in
    Log.info "Check consistency 4 (node_full_2, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    (* last allowed fork level of the head *)
    let expected_checkpoint = expected_level in
    let expected_savepoint = expected_checkpoint in
    let expected_caboose = 0 in

    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      full_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~head_level:expected_level
    in
    unit
  in

  (* For the rolling nodes *)

  (* test_node_rolling_consistency_4 *)
  let* () =
    let node = node_rolling in
    Log.info "Check consistency 4 (node_rolling, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    let expected_checkpoint = expected_level in
    let expected_savepoint = expected_checkpoint in
    let* expected_caboose =
      let* max_op_ttl = get_head_max_op_ttl node in
      return @@ (expected_checkpoint - max_op_ttl)
    in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in

  (* test_node_rolling_2_consistency_4 *)
  let* () =
    let node = node_rolling_2 in
    Log.info "Check consistency 4 (node_rolling_2, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    let expected_checkpoint = expected_level in
    let expected_savepoint = expected_checkpoint in
    let* expected_caboose =
      let* max_op_ttl = get_head_max_op_ttl node in
      return @@ (expected_checkpoint - max_op_ttl)
    in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in

  (* test_node_rolling_3_consistency_4 *)
  let* () =
    let node = node_rolling_3 in
    Log.info "Check consistency 4 (node_rolling_3, %s)." (Node.name node) ;
    let* () = restart node in
    let expected_level = head_level in
    let expected_checkpoint = expected_level in
    let expected_savepoint = expected_checkpoint in
    let* expected_caboose =
      let* max_op_ttl = get_head_max_op_ttl node in
      return @@ (expected_checkpoint - max_op_ttl)
    in
    let* () =
      node_consistency_after_import
        node
        ~expected_level
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
    in
    let* () =
      rolling_node_blocks_availability
        node
        ~savepoint:expected_savepoint
        ~caboose:expected_caboose
        ~head_level:expected_level
    in
    unit
  in

  unit

let register ~protocols = test_storage_snapshot protocols
