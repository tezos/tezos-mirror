(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Storage upgrade procedure (octez-node upgrade storage)
   Invocation:   dune exec tezt/manual_tests/main.exe -- --file storage_upgrade.ml
   Subject:      Ensure that the storage upgrade is sound

   Note: this v.3.2 upgrade test aims to a semi-automatic tools that
   is not expected to be generic enough to be fully automatized.
*)

let dir_to_upgrade () =
  match Sys.getenv_opt "DIR_TO_UPGRADE" with
  | Some v -> v
  | None ->
      Test.fail
        "cannot find dir to upgrade directory. This directory should be \
         provided through the `DIR_TO_UPGRADE` environment variable"

type network_params = {
  network : Node.argument;
  (* cycle length must be a bit bigger than the actual size of a
     cycle. *)
  cycle_length : int;
  (* Stride is expected to be at least 2 times the number of cycles so
     that we query at least 2 blocks per cycle. *)
  stride : int;
}

let network_constants () =
  match Sys.getenv_opt "NETWORK" with
  | Some "pariscnet" ->
      let cycle_length = 128 in
      {
        network = Network "https://teztnets.com/pariscnet";
        cycle_length = cycle_length * 2;
        stride = cycle_length / 3;
      }
  | Some "mainnet" ->
      let cycle_length = 4_096 in
      {
        network = Network "mainnet";
        cycle_length = 24_576;
        stride = cycle_length / 3;
      }
  | Some _ | None ->
      Test.fail
        "Unknown network. You should use this test with `NETWORK` set to \
         `pariscnet` or `mainnet`"

let run_cmd cmd =
  let* s = Lwt_unix.system cmd in
  match s with
  | Unix.WEXITED 0 -> unit
  | Unix.WEXITED _ -> Test.fail "error in command [%s]" cmd
  | _ -> unit

let print_duration d =
  match d with
  | None -> assert false
  | Some d -> Log.info "Upgrade duration: %a@." Ptime.Span.pp d

let contiguous_blocks_query node ~start ~length =
  let rec loop level =
    let cpt = level - start in
    Log.info "Status %d/%d (%d %%)" cpt length (100 * cpt / length) ;
    if level >= start + length then unit
    else
      (* Client is too slow, using raw curl calls instead *)
      (* let* _ = Client.shell_header ~block:(string_of_int level) client in *)
      let cmd =
        Format.sprintf
          "curl http://127.0.0.1:%d/chains/main/blocks/%d/header"
          (Node.rpc_port node)
          level
      in
      let* () = run_cmd cmd in
      loop (level + 1)
  in
  Log.info "Checking every block from %d to %d" start (start + length) ;
  loop start

let sparse_blocks_query node ~last ~stride =
  let rec loop level cpt =
    let j = last / stride in
    Log.info "Status %d/%d (%d %%)" cpt (j + 1) (100 * cpt / j) ;
    if level >= last then unit
    else
      (* Client is too slow, using raw curl calls instead *)
      (* let* _ = Client.shell_header ~block:(string_of_int level) client in *)
      let cmd =
        Format.sprintf
          "curl http://127.0.0.1:%d/chains/main/blocks/%d/header"
          (Node.rpc_port node)
          level
      in
      let* () = run_cmd cmd in
      loop (level + stride) (cpt + 1)
  in
  Log.info "Checking 1 block out of %d, from %d to %d" stride 0 last ;
  loop 0 0

(* This manual test aims to be small utility to:
   - bench the upgrade storage procedure (v_3_2)
   - check the consistency of the storage after the upgrade

   To do so, given:
   - DIR_TO_UPGRADE: points to a data directory of version 3.1
   - NETWORK: specifies the network to target
   we are:
   - running the storage upgrade procedure
   - ensuring that a complete cycle is consistent -- by requesting
     several contiguous blocks so that we iter over at least 1 full
     cycle
   - ensuring that several blocks of every cycle are consistent -- by
     requesting at least 2 blocks of each cycle
*)
let test_v_3_2_storage_upgrade () =
  Test.register
    ~__FILE__
    ~title:"Check v.3.2 storage upgrade"
    ~tags:["storage"; "upgrade"; "storage_3_2"]
  @@ fun () ->
  let args = Node.[Connections 0] in
  let node_to_upgrade =
    Node.create ~name:"node_to_upgrade" ~data_dir:(dir_to_upgrade ()) args
  in
  let params = network_constants () in
  let* () = Node.config_update node_to_upgrade [params.network] in
  (* Bench the upgrade procedure *)
  let start = Unix.gettimeofday () in
  Log.info "Upgrading the storage" ;
  let* () = Node.upgrade_storage node_to_upgrade in
  let duration = Unix.gettimeofday () -. start in
  let d = Ptime.Span.of_float_s duration in
  let () = print_duration d in
  Log.info "Start the node to request blocks, ensuring consistency" ;
  let* () = Node.run node_to_upgrade args in
  let* () = Node.wait_for_ready node_to_upgrade in
  let* client = Client.init ~endpoint:(Node node_to_upgrade) () in
  let* current_head = Client.level client in
  let stride = params.stride in
  if stride > current_head then
    Test.fail
      "Not enough blocks (%d) to run the test. Provide at least %d blocks in \
       the data directory."
      current_head
      stride ;
  Log.info "Checking blocks consistency up to level %d@." current_head ;
  (* [start] is set to an arbitrary value as it only aims to be the
     query start point of enough blocks that we iter over several
     cycles. *)
  let* () =
    contiguous_blocks_query
      node_to_upgrade
      ~start:1_000
      ~length:params.cycle_length
  in
  let* () = sparse_blocks_query node_to_upgrade ~last:current_head ~stride in
  let () = print_duration d in
  unit

let snapshot_file () =
  match Sys.getenv_opt "SNAPSHOT_FILE" with
  | Some v -> v
  | None ->
      Test.fail
        "cannot find snapshot file. You should provide it with the \
         `SNAPSHOT_FILE` environment variable"

let snapshot_import_dir () =
  match Sys.getenv_opt "SNAPSHOT_IMPORT_DIR" with
  | Some v -> v
  | None ->
      Test.fail
        "cannot import snapshot dir. You should provide it with the \
         `SNAPSHOT_IMPORT_DIR` environment variable"

let check_snapshot_version ~expected_version node ~snapshot_file =
  let p = Node.spawn_snapshot_info ~json:true node snapshot_file in
  let* json_output = Process.check_and_read_stdout p in
  let json_output =
    JSON.parse ~origin:"node snapshot info --json" json_output
  in
  let json_snapshot_header = JSON.(json_output |-> "snapshot_header") in
  let version = JSON.(json_snapshot_header |-> "version" |> as_int) in
  Check.((version = expected_version) int)
    ~error_msg:"expected version \"%R\" not found" ;
  unit

let check_snapshot_import node ~node_args ~snapshot_file =
  let* () = Node.snapshot_import ~no_check:true node snapshot_file in
  let params = network_constants () in
  Log.info "Start the node to request blocks, ensuring consistency" ;
  let* () = Node.run node node_args in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint:(Node node) () in
  let* current_head = Client.level client in
  Log.info "Checking blocks consistency up to level %d@." current_head ;
  let stride = params.stride in
  (* [start] is set to an arbitrary value as it only aims to be the
     query start point of enough blocks that we iter over several
     cycles. *)
  let* () =
    contiguous_blocks_query node ~start:1_000 ~length:params.cycle_length
  in
  let* () = sparse_blocks_query node ~last:current_head ~stride in
  unit

let test_v_3_2_snapshot_backward_compatibility () =
  Test.register
    ~__FILE__
    ~title:"Check v.3.2 snapshot backward compatibility"
    ~tags:["storage"; "snapshot"; "storage_3_2"; "snapshot_v7"]
  @@ fun () ->
  let args = Node.[Connections 0] in
  let node = Node.create ~name:"node" ~data_dir:(snapshot_import_dir ()) args in
  let params = network_constants () in
  let* () = Node.config_init node [params.network] in
  let snapshot_file = snapshot_file () in
  (* Checking the snapshot version to be the legacy one. *)
  let* () =
    (* Legacy snapshot version is 7 *)
    check_snapshot_version ~expected_version:7 node ~snapshot_file
  in
  check_snapshot_import node ~snapshot_file ~node_args:args

let test_v_3_2_new_snapshot_import () =
  Test.register
    ~__FILE__
    ~title:"Check v.3.2 new snapshot"
    ~tags:["storage"; "snapshot"; "storage_v3_2"; "snapshot_v8"]
  @@ fun () ->
  let args = Node.[Connections 0] in
  let node = Node.create ~name:"node" ~data_dir:(snapshot_import_dir ()) args in
  let params = network_constants () in
  let* () = Node.config_init node [params.network] in
  let snapshot_file = snapshot_file () in
  (* Checking the snapshot version is the latest one. *)
  let* () =
    (* Current snapshot version is 8 *)
    check_snapshot_version ~expected_version:8 node ~snapshot_file
  in
  check_snapshot_import node ~snapshot_file ~node_args:args

let register () =
  test_v_3_2_storage_upgrade () ;
  test_v_3_2_snapshot_backward_compatibility () ;
  test_v_3_2_new_snapshot_import ()
