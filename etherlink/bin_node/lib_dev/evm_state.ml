(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type t = Pvm.State.t

let kernel_logs_directory ~data_dir = Filename.concat data_dir "kernel_logs"

let level_prefix = function
  | Events.Debug -> "[Debug]"
  | Info -> "[Info]"
  | Error -> "[Error]"
  | Fatal -> "[Fatal]"

let event_kernel_log ~kind ~msg =
  let is_level ~level msg =
    let prefix = level_prefix level in
    String.remove_prefix ~prefix msg |> Option.map (fun msg -> (level, msg))
  in
  let level_and_msg =
    Option.either_f (is_level ~level:Debug msg) @@ fun () ->
    Option.either_f (is_level ~level:Info msg) @@ fun () ->
    Option.either_f (is_level ~level:Error msg) @@ fun () ->
    is_level ~level:Fatal msg
  in
  Option.iter_s
    (fun (level, msg) -> Events.event_kernel_log ~level ~kind ~msg)
    level_and_msg

let execute ~pool ?execution_timestamp ?(wasm_pvm_fallback = false) ?profile
    ?(kind = Events.Application) ~data_dir ?(log_file = "kernel_log")
    ?(wasm_entrypoint = Tezos_scoru_wasm.Constants.wasm_entrypoint) ~config
    ~native_execution evm_state inbox =
  let open Lwt_result_syntax in
  let to_pvm_inbox = function
    | `Skip_stage_one -> List.to_seq [[]]
    | `Inbox inbox -> List.to_seq [inbox]
  in
  Octez_telemetry.Trace.with_tzresult ~service_name:"Evm_state" "execute"
  @@ fun _ ->
  let path = Filename.concat (kernel_logs_directory ~data_dir) log_file in
  let messages = ref [] in
  let write_debug =
    Tezos_scoru_wasm.Builtins.Printer
      (fun msg ->
        messages := msg :: !messages ;
        event_kernel_log ~kind ~msg)
  in
  let* evm_state =
    match profile with
    | Some Configuration.Minimal ->
        let filename =
          Filename.concat
            (kernel_logs_directory ~data_dir)
            (log_file ^ "_profile.csv")
        in
        let flags = Unix.[O_WRONLY; O_CREAT; O_TRUNC] in
        Lwt_io.with_file ~mode:Lwt_io.Output ~flags filename @@ fun oc ->
        let*! () = Events.replay_csv_available filename in
        let*! () = Lwt_io.fprintf oc "ticks_used\n" in
        let hooks =
          Tezos_scoru_wasm.Hooks.(
            no_hooks
            |> on_pvm_reboot (fun ticks ->
                   let*! () = Lwt_io.fprintf oc "%Ld\n" ticks in
                   Lwt_io.flush oc))
        in
        let inbox = to_pvm_inbox inbox in
        let* evm_state, _ticks, _inboxes, _level =
          Pvm.Kernel.eval
            ~hooks
            ~migrate_to:Proto_alpha
            ~write_debug
            ~wasm_entrypoint
            0l
            inbox
            config
            Inbox
            evm_state
        in
        return evm_state
    | Some Configuration.Flamegraph ->
        let* function_symbols = Pvm.Kernel.get_function_symbols evm_state in
        let inbox = to_pvm_inbox inbox in
        let* evm_state, _, _ =
          Pvm.Kernel.profile
            ~migrate_to:Proto_alpha
            ~collapse:false
            ~with_time:true
            ~no_reboot:false
            0l
            inbox
            {config with flamecharts_directory = data_dir}
            function_symbols
            evm_state
        in
        return evm_state
    | None ->
        (* The [inbox] parameter is inherited from the WASM debugger, where the
           inbox is a list of list of messages (because it supports running the
           fast exec for several Tezos level in a raw).

           As far as the EVM node is concerned, we only “emulate” one Tezos level
           at a time, so we only keep the first item ([inbox] is in parctise
           always a singleton). *)
        let*! evm_state =
          Lwt.catch
            (fun () ->
              Wasm_runtime.run
                ~pool
                ~trace_host_funs:config.trace_host_funs
                ?l1_timestamp:execution_timestamp
                ~preimages_dir:config.preimage_directory
                ?preimages_endpoint:config.preimage_endpoint
                ~native_execution
                ~entrypoint:wasm_entrypoint
                (Pvm.Wasm_internal.to_irmin_exn evm_state)
                config.destination
                inbox)
            (fun exn ->
              if wasm_pvm_fallback then
                let*! () = Events.wasm_pvm_fallback () in
                let inbox = to_pvm_inbox inbox in
                let*! res =
                  Pvm.Kernel.eval
                    ~migrate_to:Proto_alpha
                    ~write_debug
                    ~wasm_entrypoint
                    0l
                    inbox
                    config
                    Inbox
                    evm_state
                in
                match res with
                | Ok (tree, _, _, _) ->
                    Lwt.return (Pvm.Wasm_internal.to_irmin_exn tree)
                | Error _err ->
                    Stdlib.failwith "The WASM PVM raised an exception"
              else Lwt.reraise exn)
        in
        return @@ Pvm.Wasm_internal.of_irmin evm_state
  in
  (* The messages are accumulated during the execution and stored
     atomatically at the end to preserve their order. *)
  let*! () =
    Lwt_io.with_file
      ~flags:Unix.[O_WRONLY; O_CREAT; O_APPEND]
      ~perm:0o644
      ~mode:Output
      path
    @@ fun chan ->
    Lwt_io.atomic
      (fun chan ->
        let msgs = List.rev !messages in
        let*! () = List.iter_s (Lwt_io.write chan) msgs in
        Lwt_io.flush chan)
      chan
  in
  return evm_state

let flag_local_exec evm_state = Durable_storage.write Evm_node_flag () evm_state

let init_reboot_counter evm_state =
  let initial_reboot_counter =
    Data_encoding.(
      Binary.to_string_exn
        Little_endian.int32
        Z.(
          to_int32 @@ succ Tezos_scoru_wasm.Constants.maximum_reboots_per_input))
  in
  Pvm.Kernel.set_durable_value
    ~edit_readonly:true
    evm_state
    Durable_storage_path.reboot_counter
    initial_reboot_counter

let init ~kernel =
  let open Lwt_result_syntax in
  let evm_state = Pvm.State.empty (module Pvm.Irmin_context) () in
  let* evm_state =
    Pvm.Kernel.start ~tree:evm_state Tezos_scoru_wasm.Wasm_pvm_state.V3 kernel
  in
  (* The WASM Runtime completely ignores the reboot counter, but some versions
     of the Etherlink kernel will need it to exist. *)
  let*! evm_state = init_reboot_counter evm_state in
  return evm_state

let current_block_height ~chain_family evm_state =
  let open Lwt_result_syntax in
  let* current_block_number =
    Durable_storage.read_opt (Current_block_number chain_family) evm_state
  in
  match current_block_number with
  | None ->
      (* No block has been created yet and we are waiting for genesis,
         whose number will be [zero]. Since the semantics of
         [apply_unsigned_chunks] is to verify the block height has been
         incremented once, we default to [-1]. *)
      return (Qty Z.(pred zero))
  | Some qty -> return qty

let current_block_hash ~chain_family evm_state =
  let open Lwt_result_syntax in
  let* current_hash =
    Durable_storage.read_opt (Current_block_hash chain_family) evm_state
  in
  match current_hash with
  | Some h -> return h
  | None -> return (L2_types.genesis_parent_hash ~chain_family)

let execute_and_inspect ~pool ?wasm_pvm_fallback ~data_dir ?wasm_entrypoint
    ~config ~native_execution_policy
    ~input:
      Simulation.Encodings.
        {messages; insight_requests; log_kernel_debug_file; _} ctxt =
  let open Lwt_result_syntax in
  let native_execution =
    match native_execution_policy with
    | Configuration.Always | Rpcs_only -> true
    | Never -> false
  in
  let keys =
    List.map
      (function
        | Simulation.Encodings.Durable_storage_key l ->
            "/" ^ String.concat "/" l
        (* We use only `Durable_storage_key` in simulation. *)
        | _ -> assert false)
      insight_requests
  in
  (* Messages from simulation requests are already valid inputs. *)
  let* evm_state =
    execute
      ~pool
      ~native_execution
      ?wasm_pvm_fallback
      ~kind:Simulation
      ?log_file:log_kernel_debug_file
      ~data_dir
      ?wasm_entrypoint
      ~config
      ctxt
      (`Inbox messages)
  in
  let* values =
    List.map_es
      (fun key -> Durable_storage.read_opt (Raw_path key) evm_state)
      keys
  in
  return values

let store_blueprint_chunks ~blueprint_number evm_state
    (chunks : Sequencer_blueprint.unsigned_chunked_blueprint) =
  let open Lwt_result_syntax in
  let chunks = (chunks :> Sequencer_blueprint.unsigned_chunk list) in
  let nb_chunks = List.length chunks in
  let* version = Durable_storage.storage_version evm_state in
  let chunk_writes =
    List.map
      (fun (chunk : Sequencer_blueprint.unsigned_chunk) ->
        let (Qty number) = chunk.number in
        let value =
          (* We want to encode a [StoreBlueprint] (see blueprint_storage.rs
             in kernel_latest/kernel). The [StoreBlueprint] has two variants,
             and we want to store a [SequencerChunk] whose tag is 0. [Value ""]
             is the RLP-encoded for 0. *)
          Rlp.List [Rlp.Value (Bytes.of_string ""); Value chunk.value]
          |> Rlp.encode
        in
        ( Durable_storage.Blueprint_chunk
            {blueprint_number = number; chunk_index = chunk.chunk_index},
          value ))
      chunks
  in
  let* evm_state = Durable_storage.write_all chunk_writes evm_state in
  let* evm_state =
    Durable_storage.write
      (Blueprint_nb_chunks blueprint_number)
      nb_chunks
      evm_state
  in
  if version >= 39 then
    let* current_generation =
      Durable_storage.inspect_durable_and_decode_default
        ~default:Qty.zero
        evm_state
        (Durable_storage_path.Blueprint.current_generation
           ~storage_version:version)
        Ethereum_types.decode_number_le
    in
    let* evm_state =
      Durable_storage.write
        (Blueprint_generation blueprint_number)
        current_generation
        evm_state
    in
    return evm_state
  else return evm_state

type apply_result =
  | Apply_success of {
      evm_state : t;
      block : Ethereum_types.legacy_transaction_object L2_types.block;
      tezos_block : L2_types.Tezos_block.t option;
    }
  | Apply_failure

type block_in_progress = {
  timestamp : Time.Protocol.t;
  number : Ethereum_types.quantity;
  transactions_count : int32;
}

let encode ~tezos_x_tag inner =
  (Rlp.(List [Value (Rlp.encode_int tezos_x_tag); inner]), tezos_x_tag = 1)

let encode_inner ~inner_tag hash raw =
  let hash = Ethereum_types.hex_to_real_bytes hash in
  let bytes = String.to_bytes raw in
  Rlp.(List [Value hash; List [Value (Rlp.encode_int inner_tag); Value bytes]])

let execute_single_transaction ~storage_version ~data_dir ~pool
    ~native_execution ~config evm_state block_in_progress (Hash hash)
    (tx : Broadcast.transaction) =
  let open Lwt_result_syntax in
  Octez_telemetry.Trace.add_attrs (fun () ->
      Telemetry.Attributes.
        [Transaction.hash (Hash hash); Block.number block_in_progress.number]) ;
  let tezosx = Storage_version.tezosx_single_tx ~storage_version in
  let tx, read_receipt =
    match tx with
    | Broadcast.Delayed tx ->
        if tezosx then
          encode ~tezos_x_tag:1 (Evm_events.Delayed_transaction.to_rlp tx)
        else (Evm_events.Delayed_transaction.to_rlp tx, true)
    | Broadcast.Common (Evm tx) ->
        let inner = encode_inner ~inner_tag:1 hash tx in
        if tezosx then encode ~tezos_x_tag:1 inner else (inner, true)
    | Broadcast.Common (Michelson op) ->
        let inner = encode_inner ~inner_tag:1 hash op in
        if tezosx then encode ~tezos_x_tag:0 inner else (inner, false)
  in
  let rlp =
    Rlp.List
      [
        tx;
        Value (Ethereum_types.timestamp_to_bytes block_in_progress.timestamp);
        Value (Ethereum_types.encode_u256_le block_in_progress.number);
      ]
  in
  let* evm_state = Durable_storage.write Single_tx_input rlp evm_state in
  let* evm_state =
    execute
      ~pool
      ~native_execution
      ~data_dir
      ~config
      ~wasm_entrypoint:"single_tx_execution"
      evm_state
      (`Inbox [])
  in
  if read_receipt then
    let* read_res =
      Durable_storage.read_opt
        (Raw_path
           (Durable_storage_path.Block.current_receipts
              ~root:Durable_storage_path.etherlink_safe_root))
        evm_state
    in
    match read_res with
    | Some bytes ->
        let receipt =
          Transaction_receipt.decode_last_from_list
            Ethereum_types.(Block_hash (Hex (String.make 64 '0')))
            bytes
        in
        return (L2_types.Ethereum receipt, evm_state)
    | None ->
        failwith
          "No value found in context where transactions receipts should be \
           stored"
  else return (L2_types.Tezos, evm_state)

let execute_entrypoint ~data_dir ~pool ~native_execution_policy ~config
    evm_state ~input_path ~input ~output_path ~entrypoint =
  let open Lwt_result_syntax in
  let* evm_state =
    Durable_storage.write (Raw_path input_path) input evm_state
  in
  let output_path_parts =
    String.split_on_char '/' output_path |> List.filter (fun s -> s <> "")
  in
  let execution_input =
    Simulation.Encodings.
      {
        messages = [];
        reveal_pages = None;
        insight_requests = [Durable_storage_key output_path_parts];
        log_kernel_debug_file = None;
      }
  in
  let* bytes_opt_l =
    execute_and_inspect
      ~pool
      ~native_execution_policy
      ~data_dir
      ~config
      ~wasm_entrypoint:entrypoint
      ~input:execution_input
      evm_state
  in
  match bytes_opt_l with
  | [Some bytes] -> return bytes
  | _ -> failwith "No value found at the entrypoint output path %s" output_path

let retrieve_block_at_root ~chain_family evm_state =
  let open Lwt_result_syntax in
  let* storage_version = Durable_storage.storage_version evm_state in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    Durable_storage.read_opt (Current_block chain_family) evm_state
  else
    let* current_block_hash = current_block_hash ~chain_family evm_state in
    Durable_storage.read_opt
      (Block_by_hash (chain_family, current_block_hash))
      evm_state

(** [retrieve_block ~chain_family evm_state] returns 0, 1, or 2 blocks
    as a (first_block * second_block option) option as follows:

    - The first block's kind and the path at which it is read depends
      on the [chain_family] argument; if the chain family is Michelson,
      the block is read at the Tezlink root path and expected to be a
      Tezos block, if the chain family is EVM, the block is read at the
      Etherlink root path and expected to be an Ethereum block.

    - The second block is attempted to be read at the Tezos X Michelson
      runtime's path, if something is found at that path, it is always
      expected to be a Tezos block.

    - If the fist block cannot be read or deserialized, the second read
      is ignored and this function returns [None].

*)
let retrieve_block ~chain_family evm_state =
  let open Lwt_result_syntax in
  let* block_opt = retrieve_block_at_root ~chain_family evm_state in
  let* tezos_block_opt =
    Durable_storage.read_opt Tezosx_tezos_current_block evm_state
  in
  let tezos_block =
    match tezos_block_opt with
    | Some (L2_types.Tez block) -> Some block
    | Some (L2_types.Eth _) -> None
    | None -> None
  in
  return (Option.map (fun block -> (block, tezos_block)) block_opt)

let assemble_block (type f) ~pool ~data_dir
    ~(chain_family : f L2_types.chain_family) ~config ~timestamp ~number
    ~native_execution evm_state =
  let open Lwt_result_syntax in
  let* () =
    match chain_family with
    | EVM -> return_unit
    | Michelson ->
        failwith "Assemble block is not supported for Michelson chain family"
  in
  let rlp =
    Rlp.List
      [
        Value (Ethereum_types.timestamp_to_bytes timestamp);
        Value (Ethereum_types.encode_u256_le number);
      ]
  in
  let* evm_state = Durable_storage.write Assemble_block_input rlp evm_state in
  let* evm_state =
    execute
      ~pool
      ~native_execution
      ~data_dir
      ~config
      ~wasm_entrypoint:"assemble_block"
      evm_state
      (`Inbox [])
  in
  let (Qty height) = number in
  let before_height = Z.pred height in
  let* block = retrieve_block ~chain_family evm_state in
  match block with
  | Some (block, tezos_block) ->
      let (Qty after_height) = L2_types.block_number block in
      if Z.(equal (succ before_height) after_height) then
        return (Apply_success {evm_state; block; tezos_block})
      else return Apply_failure
  | None -> return Apply_failure

let export_gas_used ~log_file ~data_dir
    (block : legacy_transaction_object L2_types.block) =
  let (Qty gas) =
    match block with Eth {gasUsed; _} -> gasUsed | _ -> Qty.zero
  in
  let filename =
    Filename.concat (kernel_logs_directory ~data_dir) (log_file ^ "_profile.csv")
  in
  let flags = Unix.[O_WRONLY; O_CREAT; O_APPEND] in
  Lwt_io.with_file ~mode:Lwt_io.Output ~flags filename @@ fun oc ->
  Lwt_io.fprintf oc "gas_used\n%Ld\n" (Z.to_int64 gas)

let apply_unsigned_chunks ~pool ?wasm_pvm_fallback ?log_file ?profile ~data_dir
    ~chain_family ~config ~native_execution_policy evm_state
    (chunks : Sequencer_blueprint.unsigned_chunked_blueprint) =
  let open Lwt_result_syntax in
  let* (Qty before_height) = current_block_height ~chain_family evm_state in
  let* evm_state =
    store_blueprint_chunks
      ~blueprint_number:(Z.succ before_height)
      evm_state
      chunks
  in
  let* evm_state =
    execute
      ~pool
      ~native_execution:(native_execution_policy = Configuration.Always)
      ?wasm_pvm_fallback
      ?profile
      ~data_dir
      ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
      ~config
      ?log_file
      evm_state
      `Skip_stage_one
  in
  let* block = retrieve_block ~chain_family evm_state in
  match block with
  | Some (block, tezos_block) ->
      let (Qty after_height) = L2_types.block_number block in
      let*! () =
        match (profile, log_file) with
        | Some Configuration.Minimal, Some log_file ->
            export_gas_used ~log_file ~data_dir block
        | _ -> Lwt_syntax.return_unit
      in
      if Z.(equal (succ before_height) after_height) then
        return (Apply_success {evm_state; block; tezos_block})
      else return Apply_failure
  | _ -> return Apply_failure

let clear_delayed_inbox evm_state =
  Durable_storage.delete_dir Delayed_inbox evm_state

let wasm_pvm_version state = Pvm.Kernel.get_wasm_version state

let irmin_store_path ~data_dir = Filename.Infix.(data_dir // "store")

let preload_kernel ~pool evm_state =
  let open Lwt_syntax in
  let* loaded =
    Wasm_runtime.preload_kernel ~pool (Pvm.Wasm_internal.to_irmin_exn evm_state)
  in
  if loaded then
    let* version_result = Durable_storage.read Kernel_version evm_state in
    let version =
      match version_result with Ok v -> v | Error _ -> "(unknown)"
    in
    Events.preload_kernel version
  else return_unit

let get_delayed_inbox_item evm_state hash =
  let open Lwt_result_syntax in
  let* storage_version = Durable_storage.storage_version evm_state in
  let* bytes =
    Durable_storage.read_opt
      (Raw_path
         (Durable_storage_path.Delayed_transaction.transaction
            ~storage_version
            hash))
      evm_state
  in
  let*? bytes =
    Option.to_result ~none:[error_of_fmt "missing delayed inbox item"] bytes
  in
  let*? rlp_item = Rlp.decode bytes in
  match rlp_item with
  | Rlp.(List (rlp_item :: _)) ->
      let*? res =
        Option.to_result ~none:[error_of_fmt "cannot parse delayed inbox item"]
        @@ Evm_events.Delayed_transaction.of_rlp_content
             ~transaction_tag:"\x01"
             ~fa_deposit_tag:"\x03"
             ~operation_tag:"\x04"
             hash
             rlp_item
      in
      return res
  | _ -> failwith "invalid delayed inbox item"

let clear_events evm_state = Durable_storage.delete_dir Evm_events evm_state

let clear_block_storage chain_family block evm_state =
  let open Lwt_result_syntax in
  let* storage_version = Durable_storage.storage_version evm_state in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    return evm_state
  else
    (* We have 2 path to clear related to block storage:
     1. The predecessor block.
     2. The indexes (which is a bit trickier).

     We can remove only the predecessor block because the current block is
     necessary to produce the next block. Block production starts by reading
     the head to retrieve information such as parent block hash.
  *)
    let block_parent = L2_types.block_parent block in
    let block_number = L2_types.block_number block in
    let (Qty number) = block_number in
    (* Handles case (1.). *)
    let* evm_state =
      if number > Z.zero then
        Durable_storage.delete
          (Block_by_hash (chain_family, block_parent))
          evm_state
      else return evm_state
    in
    (* Handles case (2.). *)
    let* evm_state =
      (* The opcode BLOCKHASH must return the hash of the last 256 complete blocks,
       see the yellow paper for the specification.
       The kernel uses the indexable storage to retrieve the last 256 blocks,
       so we garbage collect only what's possible. *)
      let to_keep = Z.of_int 256 in
      if number >= to_keep then
        Durable_storage.delete
          (Block_index (chain_family, Nth (Z.sub number to_keep)))
          evm_state
      else return evm_state
    in
    (* Receipts are not necessary for the kernel, we can just remove
     the directories. *)
    let* evm_state =
      Durable_storage.delete_dir Transaction_receipts evm_state
    in
    let* evm_state = Durable_storage.delete_dir Transaction_objects evm_state in
    return evm_state

let delayed_inbox_hashes evm_state =
  let open Lwt_result_syntax in
  let* keys = Durable_storage.subkeys Delayed_transactions evm_state in
  let hashes =
    (* Remove the empty, meta keys *)
    List.filter_map
      (fun key ->
        if key = "" || key = "meta" then None
        else Some (Ethereum_types.hash_of_string key))
      keys
  in
  return hashes
