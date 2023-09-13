(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Filename.Infix
open Tezos_clic

let group =
  {
    name = "devtools";
    title = "Command for manager operation sequence extraction";
  }

let use_data_dir data_dir f =
  Lwt_lock_file.try_with_lock
    ~when_locked:(fun () ->
      failwith "Data directory is locked by another process")
    ~filename:(Data_version.lock_file data_dir)
  @@ f

let data_dir_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:"Octez data directory path"
    ~short:'D'
    ~long:"data-dir"
    ~placeholder:"data-dir-path"
    ~default:(Sys.getenv "HOME" // ".tezos-node")
    ( parameter @@ fun _ dn ->
      if Sys.file_exists dn && Sys.is_directory dn then return dn
      else failwith "%s does not exists or is not a directory" dn )

let output_dir_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:"Extraction output directory"
    ~short:'o'
    ~long:"output"
    ~placeholder:"output-path"
    ~default:(Unix.getcwd ())
    ( parameter @@ fun _ dn ->
      if Sys.file_exists dn && Sys.is_directory dn then return dn
      else failwith "%s does not exists or is not a directory" dn )

let operations_file_param =
  let open Lwt_result_syntax in
  param
    ~name:"operations-file"
    ~desc:"Operations file"
    ( parameter @@ fun _ dn ->
      if Sys.file_exists dn then return dn
      else failwith "File %s does not exists" dn )

let round_duration_arg =
  let open Lwt_result_syntax in
  arg
    ~doc:
      "Maximal round duration (in seconds) that the synchronisation heuristic \
       will target as current round."
    ~short:'r'
    ~long:"round-duration"
    ~placeholder:"seconds"
    ( parameter @@ fun _ s ->
      match int_of_string_opt s with
      | Some i when i > 0 -> return i
      | _ -> failwith "Parameter should be a positive integer literal" )

let op_per_mempool_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:
      "Number of operations that the injector will try to maintain in the \
       mempool at all time."
    ~short:'n'
    ~long:"op-per-mempool"
    ~default:"2500"
    ~placeholder:"integer"
    ( parameter @@ fun _ s ->
      match int_of_string_opt s with
      | Some i when i > 0 -> return i
      | _ -> failwith "Parameter should be a positive integer literal" )

let pp_spaced_int ppf i =
  let s = Format.sprintf "%d" i |> String.to_seq |> List.of_seq |> List.rev in
  List.fold_left
    (fun (i, acc) c ->
      if i mod 4 = 0 then (1, c :: ',' :: acc) else (succ i, c :: acc))
    (1, [])
    s
  |> snd |> List.to_seq |> String.of_seq |> Format.fprintf ppf "%s"

let info config store =
  let open Lwt_result_syntax in
  Format.printf
    "Network: %s@."
    (match config.Config_file.blockchain_network.alias with
    | None -> "mainnet"
    | Some s -> s) ;
  let chain_store = Store.main_chain_store store in
  let*! current_head = Store.Chain.current_head chain_store in
  let current_head = Store.Block.descriptor current_head in
  let*! checkpoint = Store.Chain.checkpoint chain_store in
  Format.printf
    "Current head: %a@."
    Store_types.pp_block_descriptor
    current_head ;
  Format.printf "Checkpoint: %a@." Store_types.pp_block_descriptor checkpoint ;
  Format.printf
    "Replayable history length: %a blocks@."
    pp_spaced_int
    Int32.(succ (sub (snd current_head) (snd checkpoint)) |> to_int) ;
  return_unit

let extract_history ~data_dir ~output_dir =
  let open Lwt_result_syntax in
  use_data_dir data_dir @@ fun () ->
  let* _, config = Shared_arg.resolve_data_dir_and_config_file ~data_dir () in
  let store_dir = data_dir // "store" in
  let context_dir = data_dir // "context" in
  let* store =
    Store.init
      ~store_dir
      ~context_dir
      ~allow_testchains:false
      ~readonly:true
      config.blockchain_network.genesis
  in
  let chain_store = Store.main_chain_store store in
  let* () = info config store in
  let* snapshotted_block =
    Store.Chain.block_of_identifier chain_store (`Alias (`Checkpoint, 0))
  in
  let snapshot_path =
    output_dir
    // ((Store.Block.hash snapshotted_block |> Block_hash.to_short_b58check)
       ^ ".rolling")
  in
  Format.printf "Extracting snapshot at checkpoint: '%s'@." snapshot_path ;
  let* () =
    Snapshots.export
      ~snapshot_path
      ~rolling:true
      ~block:(`Hash (Store.Block.hash snapshotted_block, 0))
      ~store_dir
      ~context_dir
      ~chain_name:config.blockchain_network.chain_name
      ~progress_display_mode:Animation.Auto
      Snapshots.Tar
      config.Config_file.blockchain_network.genesis
  in
  Format.printf "Snapshot extracted.@." ;
  let extracted_operations_path = output_dir // "operations" in
  Format.printf
    "Extracting manager operations succeeding snapshotted block in '%s'@."
    extracted_operations_path ;
  let total = ref 0 in
  let* () =
    let*! current_head = Store.Chain.current_head chain_store in
    let* path =
      let rec loop acc pred =
        if Block_hash.equal pred (Store.Block.hash snapshotted_block) then
          return acc
        else
          let* new_pred_opt =
            Store.Block.read_ancestor_hash chain_store ~distance:1 pred
          in
          match new_pred_opt with
          | None -> assert false
          | Some new_pred -> loop (pred :: acc) new_pred
      in
      loop
        [Store.Block.hash current_head]
        (Store.Block.predecessor current_head)
    in
    let*! chan = Lwt_io.open_file ~mode:Output extracted_operations_path in
    let len = List.length path in
    Animation.display_progress
      ~every:100
      ~progress_display_mode:Auto
      ~pp_print_step:(fun fmt i ->
        Format.fprintf
          fmt
          "Processing block manager operations: %a/%a blocks treated"
          pp_spaced_int
          i
          pp_spaced_int
          len)
    @@ fun step ->
    List.iter_es
      (fun bh ->
        let* block = Store.Block.read_block chain_store bh in
        let manager_operations =
          List.nth (Store.Block.operations block) 3 |> function
          | None -> []
          | Some opl -> opl
        in
        let*! () =
          List.iter_s
            (fun op ->
              incr total ;
              let bytes =
                Data_encoding.Binary.to_bytes_exn
                  (Data_encoding.dynamic_size Operation.encoding)
                  op
              in
              Lwt_io.write_from_exactly chan bytes 0 (Bytes.length bytes))
            manager_operations
        in
        let*! () = step () in
        return_unit)
      path
  in
  Format.printf "Total manager operations extracted: %a@." pp_spaced_int !total ;
  return_unit

let find_proto_tool (cctxt : Client_context.full) =
  let open Lwt_result_syntax in
  let* {current_protocol; _} =
    Tezos_shell_services.Chain_services.Blocks.protocols cctxt ()
  in
  match Protocol_hash.Map.find current_protocol !Sigs.all with
  | Some x -> return x
  | None ->
      failwith
        "No simulation tool found corresponding to %a"
        Protocol_hash.pp
        current_protocol

let sync_node (cctxt : Client_context.full) round_duration_target =
  let open Lwt_result_syntax in
  let* (module Tool) = find_proto_tool cctxt in
  let*! () = cctxt#message "Synchronizing the node to a low round time." in
  Tool.sync_node cctxt ?round_duration_target ()

let run_injector (cctxt : Client_context.full) ~op_per_mempool
    ~operations_file_path =
  let open Lwt_result_syntax in
  let* (module Tool) = find_proto_tool cctxt in
  Tool.start_injector cctxt ~op_per_mempool ~operations_file_path

let commands =
  let open Lwt_result_syntax in
  [
    command
      ~group
      ~desc:
        "Information on the current node's history and how much extractable \
         data is present."
      (args1 data_dir_arg)
      (fixed ["info"])
      (fun data_dir _cctxt ->
        use_data_dir data_dir @@ fun () ->
        let* _, config =
          Shared_arg.resolve_data_dir_and_config_file ~data_dir ()
        in
        let* store =
          Store.init
            ~readonly:true
            ~store_dir:(data_dir // "store")
            ~context_dir:(data_dir // "context")
            ~allow_testchains:false
            config.blockchain_network.genesis
        in
        info config store);
    command
      ~group
      ~desc:
        "Extract a snapshot from the node's checkpoint and gather in a file \
         the sequence of future operations that occurs after this checkpoint."
      (args2 data_dir_arg output_dir_arg)
      (fixed ["extract"; "history"])
      (fun (data_dir, output_dir) _cctxt ->
        extract_history ~data_dir ~output_dir);
    command
      ~group
      ~desc:
        "Synchronize a yes-node so that the current head round's duration is \
         low enough in order for yes-bakers to activate without having to wait \
         a significant amount of time."
      (args1 round_duration_arg)
      (fixed ["sync"])
      (fun round_duration_target (cctxt : Client_context.full) ->
        sync_node cctxt round_duration_target);
    command
      ~group
      ~desc:
        "Run a simulation scenario on a yes-node with the given operation file \
         by injecting operations in the node's mempool. This tool will try to \
         target 2500 manager operations present in the mempool at all time."
      (args1 op_per_mempool_arg)
      (prefixes ["run"; "simulation"] @@ operations_file_param @@ stop)
      (fun op_per_mempool operations_file_path (cctxt : Client_context.full) ->
        let* () = run_injector cctxt ~op_per_mempool ~operations_file_path in
        return_unit);
  ]

module Custom_client_config : Client_main_run.M = struct
  type t = Uri.t

  let default_base_dir = "/tmp"

  let global_options () =
    args1
      (default_arg
         ~long:"endpoint"
         ~short:'E'
         ~placeholder:"uri"
         ~doc:"HTTP(S) endpoint of the node RPC interface"
         ~default:"http://localhost:8732"
         (Client_config.endpoint_parameter ()))

  let parse_config_args ctx argv =
    let open Lwt_result_syntax in
    let* endpoint, remaining =
      Tezos_clic.parse_global_options (global_options ()) ctx argv
    in
    let open Client_config in
    let cfg : Cfg_file.t = {Cfg_file.default with endpoint = Some endpoint} in
    Lwt.return_ok
      ( {default_parsed_config_args with parsed_config_file = Some cfg},
        remaining )

  let default_chain = `Main

  let default_block = `Head 0

  let default_daily_logs_path = None

  let default_media_type = Tezos_rpc_http.Media_type.Command_line.Binary

  let other_registrations = None

  let clic_commands ~base_dir:_ ~config_commands:_ ~builtin_commands:_
      ~other_commands:_ ~require_auth:_ =
    commands

  let logger = None
end

let () =
  let select_commands _ctx _ = Lwt.return_ok commands in
  Client_main_run.run (module Custom_client_config) ~select_commands
