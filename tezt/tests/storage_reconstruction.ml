(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component: Storage reconstruction command
   Invocation: dune exec tezt/tests/main.exe -- -f storage_reconstruction.ml
   Subject: Tests the reconstruction mechanism
*)

let pp_snapshot_export_format fmt v =
  Format.fprintf fmt "%s" ((function Node.Tar -> "tar" | Raw -> "raw") v)

let pp_snapshot_history_mode fmt v =
  Format.fprintf
    fmt
    "%s"
    ((function
       | Node.Rolling_history -> "rolling" | Node.Full_history -> "full")
       v)

let get_constants ~protocol client =
  let* constants =
    Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_preservation_cycles =
    let v =
      if Protocol.number protocol > Protocol.number Protocol.Oxford then
        "blocks_preservation_cycles"
      else "preserved_cycles"
    in
    JSON.(constants |-> v |> as_int)
  in
  let* blocks_per_cycle =
    return JSON.(constants |-> "blocks_per_cycle" |> as_int)
  in
  let* max_op_ttl =
    return JSON.(constants |-> "max_operations_time_to_live" |> as_int)
  in
  return (blocks_preservation_cycles, blocks_per_cycle, max_op_ttl)

let export_snapshot node ~export_level ~snapshot_dir ~history_mode
    ~export_format =
  Log.info "Exporting snapshot for %s" (Node.name node) ;
  let history_mode_str =
    Format.asprintf "%a" pp_snapshot_history_mode history_mode
  in
  let export_format_str =
    Format.asprintf "%a" pp_snapshot_export_format export_format
  in
  let filename =
    Node.name node ^ "." ^ history_mode_str ^ "." ^ export_format_str
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

let bake_blocks node client ~blocks_to_bake =
  Log.info "Baking a batch of %d blocks on %s" blocks_to_bake (Node.name node) ;
  repeat blocks_to_bake @@ fun () ->
  Client.bake_for_and_wait
    ~endpoint:(Node node)
    ~node
    ~minimal_timestamp:true
    client

let check_levels node expected_head_level =
  Log.info "Checking head, savepoint and caboose levels for %s" (Node.name node) ;
  let* head_level =
    let* json = Node.RPC.call node @@ RPC.get_chain_block_header () in
    Lwt.return JSON.(json |-> "level" |> as_int)
  and* {level = savepoint; _} =
    Node.RPC.call node @@ RPC.get_chain_level_savepoint ()
  and* {level = caboose; _} =
    Node.RPC.call node @@ RPC.get_chain_level_caboose ()
  in
  Check.((head_level = expected_head_level) int)
    ~error_msg:"expected head level = %R, got %L" ;
  Check.((savepoint = 0) int) ~error_msg:"expected savepoint = %R, got %L" ;
  Check.((caboose = 0) int) ~error_msg:"expected caboose = %R, got %L" ;
  unit

let check_blocks_availability node head =
  Log.info "Checking blocks availability for %s" (Node.name node) ;
  let iter_block_range_s a b f =
    Lwt_list.iter_s f (range a b |> List.rev |> List.map string_of_int)
  in
  let expect_metadata block =
    let* (_ : RPC.block_metadata) =
      Node.RPC.call node @@ RPC.get_chain_block_metadata ~block ()
    in
    unit
  in
  let* () = iter_block_range_s 0 head @@ expect_metadata in
  unit

(* Tests the storage reconstruction command by reconstructing the
   storage of both a full node and an imported full snapshot, and
   then, by querying all the data that is expected to be
   reconstructed. *)
let test_storage_reconstruction =
  Protocol.register_test
    ~__FILE__
    ~title:(Format.asprintf "storage reconstruction")
    ~tags:["storage"; "reconstruction"]
  @@ fun protocol ->
  let node_arguments = Node.[Synchronisation_threshold 0] in
  let* node, client =
    Client.init_with_protocol
      ~name:"node"
      ~nodes_args:(node_arguments @ Node.[History_mode (Full None)])
      ~protocol
      `Client
      ()
  in
  let* blocks_preservation_cycles, blocks_per_cycle, max_op_ttl =
    get_constants ~protocol client
  in
  Log.info "Baking a few blocks so that the savepoint is still at genesis." ;
  let blocks_to_bake_1 = blocks_preservation_cycles * blocks_per_cycle in
  let* () = bake_blocks node client ~blocks_to_bake:blocks_to_bake_1 in
  Log.info "Terminate the node and start a reconstruction." ;
  let* () = Node.terminate node in
  let p = Node.spawn_reconstruct node in
  Log.info
    "The reconstruction should fail as there is nothing to reconstruct yet." ;
  let* () = Process.check_error ~msg:(rex "nothing to reconstruct") p in
  Log.info "Restart the node and continue the baking a bit." ;
  let* () = Node.run node node_arguments in
  let* () = Node.wait_for_ready node in
  Log.info "Bake enough blocks so that the savepoint is moved." ;
  let blocks_to_bake_2 = (2 * blocks_per_cycle) + max_op_ttl in
  let* () = bake_blocks node client ~blocks_to_bake:blocks_to_bake_2 in
  let head_level = blocks_to_bake_1 + blocks_to_bake_2 + 1 in
  Log.info "Export a snapshot from the non-reconstructed full node." ;
  let snapshot_dir = Temp.dir "snapshot_dir" in
  let snapshot_filename = "snapshot.full" in
  let* () =
    Node.snapshot_export
      ~export_level:head_level
      ~history_mode:Node.Full_history
      node
      (snapshot_dir // snapshot_filename)
  in
  Log.info "Terminate the node and start a reconstruction." ;
  let* () = Node.terminate node in
  let* () = Node.reconstruct node in
  Log.info "Restart as archive node and make sure it is healthy." ;
  let* () = Node.run node (node_arguments @ Node.[History_mode Archive]) in
  let* () = Node.wait_for_ready node in
  let* () = check_levels node head_level in
  let* () = check_blocks_availability node head_level in
  Log.info "Try to import and reconstruct the snapshot." ;
  let* imported_node =
    Node.init
      ~name:"imported_node"
      ~snapshot:(snapshot_dir // snapshot_filename, true)
      node_arguments
  in
  Log.info "Make sure it is healthy." ;
  let* () = check_levels imported_node head_level in
  let* () = check_blocks_availability imported_node head_level in
  unit

let register ~protocols = test_storage_reconstruction protocols
