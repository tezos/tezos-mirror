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

let modify ?edit_readonly ~key ~value evm_state =
  Pvm.Kernel.set_durable_value ?edit_readonly evm_state key value

let flag_local_exec evm_state =
  modify evm_state ~key:Durable_storage_path.evm_node_flag ~value:""

let init_reboot_counter evm_state =
  let initial_reboot_counter =
    Data_encoding.(
      Binary.to_string_exn
        Little_endian.int32
        Z.(
          to_int32 @@ succ Tezos_scoru_wasm.Constants.maximum_reboots_per_input))
  in
  modify
    ~edit_readonly:true
    ~key:Durable_storage_path.reboot_counter
    ~value:initial_reboot_counter
    evm_state

let init ~kernel =
  let open Lwt_result_syntax in
  let evm_state = Pvm.State.empty (module Irmin_context) () in
  let* evm_state =
    Pvm.Kernel.start ~tree:evm_state Tezos_scoru_wasm.Wasm_pvm_state.V3 kernel
  in
  (* The WASM Runtime completely ignores the reboot counter, but some versions
     of the Etherlink kernel will need it to exist. *)
  let*! evm_state = init_reboot_counter evm_state in
  let*! evm_state = flag_local_exec evm_state in
  return evm_state

let inspect evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* value = Pvm.Kernel.find_key_in_durable evm_state key in
  Option.map_s Tezos_lazy_containers.Chunked_byte_vector.to_bytes value

let subkeys evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* durable = Pvm.Kernel.wrap_as_durable_storage evm_state in
  let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
  Tezos_scoru_wasm.Durable.list durable key

let exists evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* durable = Pvm.Kernel.wrap_as_durable_storage evm_state in
  let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
  Tezos_scoru_wasm.Durable.exists durable key

let read state key =
  let open Lwt_result_syntax in
  let*! res = inspect state key in
  return res

let storage_version state = Durable_storage.storage_version (read state)

let kernel_version evm_state =
  let open Lwt_syntax in
  let+ version = inspect evm_state Durable_storage_path.kernel_version in
  match version with Some v -> Bytes.unsafe_to_string v | None -> "(unknown)"

let current_block_height ~root evm_state =
  let open Lwt_syntax in
  let* current_block_number =
    inspect evm_state (Durable_storage_path.Block.current_number ~root)
  in
  match current_block_number with
  | None ->
      (* No block has been created yet and we are waiting for genesis,
         whose number will be [zero]. Since the semantics of
         [apply_unsigned_chunks] is to verify the block height has been
         incremented once, we default to [-1]. *)
      return (Qty Z.(pred zero))
  | Some current_block_number ->
      let (Qty current_block_number) = decode_number_le current_block_number in
      return (Qty current_block_number)

let current_block_hash ~chain_family evm_state =
  let open Lwt_result_syntax in
  let root = Durable_storage_path.root_of_chain_family chain_family in
  let*! current_hash =
    inspect evm_state (Durable_storage_path.Block.current_hash ~root)
  in
  match current_hash with
  | Some h -> return (decode_block_hash h)
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
  let*! values = List.map_p (fun key -> inspect evm_state key) keys in
  return values

let store_blueprint_chunk evm_state (chunk : Sequencer_blueprint.unsigned_chunk)
    =
  let open Lwt_result_syntax in
  let (Qty number) = chunk.number in
  let key =
    Durable_storage_path.Blueprint.chunk
      ~blueprint_number:number
      ~chunk_index:chunk.chunk_index
  in
  let value =
    (* We want to encode a [StoreBlueprint] (see blueprint_storage.rs in
       kernel_latest/kernel). The [StoreBlueprint] has two variants, and we
       want to store a [SequencerChunk] whose tag is 0. [Value ""] is the
       RLP-encoded for 0. *)
    Rlp.List [Rlp.Value (Bytes.of_string ""); Value chunk.value]
    |> Rlp.encode |> Bytes.to_string
  in
  let*! evm_state = modify ~key ~value evm_state in
  return evm_state

let store_blueprint_chunks ~blueprint_number evm_state
    (chunks : Sequencer_blueprint.unsigned_chunked_blueprint) =
  let open Lwt_result_syntax in
  let chunks = (chunks :> Sequencer_blueprint.unsigned_chunk list) in
  let nb_chunks = List.length chunks in
  let* evm_state = List.fold_left_es store_blueprint_chunk evm_state chunks in
  let*! evm_state =
    modify
      ~key:(Durable_storage_path.Blueprint.nb_chunks ~blueprint_number)
      ~value:(Z.to_bits (Z.of_int nb_chunks))
      evm_state
  in
  let* version = storage_version evm_state in
  if version >= 39 then
    let* current_generation =
      Durable_storage.inspect_durable_and_decode_default
        ~default:Qty.zero
        (read evm_state)
        Durable_storage_path.Blueprint.current_generation
        Ethereum_types.decode_number_le
    in
    let*! evm_state =
      modify
        ~key:(Durable_storage_path.Blueprint.generation ~blueprint_number)
        ~value:(encode_u256_le current_generation |> Bytes.to_string)
        evm_state
    in
    return evm_state
  else return evm_state

type apply_result =
  | Apply_success of {
      evm_state : t;
      block : Ethereum_types.legacy_transaction_object L2_types.block;
    }
  | Apply_failure

let apply_unsigned_chunks ~pool ?wasm_pvm_fallback ?log_file ?profile ~data_dir
    ~chain_family ~config ~native_execution_policy evm_state
    (chunks : Sequencer_blueprint.unsigned_chunked_blueprint) =
  let open Lwt_result_syntax in
  let root = Durable_storage_path.root_of_chain_family chain_family in
  let*! (Qty before_height) = current_block_height ~root evm_state in
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
  let root = Durable_storage_path.root_of_chain_family chain_family in
  let* storage_version = storage_version evm_state in
  let* block =
    if not (Storage_version.legacy_storage_compatible ~storage_version) then
      let*! bytes =
        inspect evm_state (Durable_storage_path.Block.current_block ~root)
      in
      return (Option.map (L2_types.block_from_bytes ~chain_family) bytes)
    else
      let* current_block_hash = current_block_hash ~chain_family evm_state in
      let*! bytes =
        inspect
          evm_state
          (Durable_storage_path.Block.by_hash ~root current_block_hash)
      in
      return (Option.map (L2_types.block_from_bytes ~chain_family) bytes)
  in

  let export_gas_used (Qty gas) =
    match (profile, log_file) with
    | Some Configuration.Minimal, Some log_file ->
        let filename =
          Filename.concat
            (kernel_logs_directory ~data_dir)
            (log_file ^ "_profile.csv")
        in
        let flags = Unix.[O_WRONLY; O_CREAT; O_APPEND] in
        Lwt_io.with_file ~mode:Lwt_io.Output ~flags filename @@ fun oc ->
        Lwt_io.fprintf oc "gas_used\n%Ld\n" (Z.to_int64 gas)
    | _ -> Lwt.return_unit
  in
  match block with
  | Some block ->
      let (Qty after_height) = L2_types.block_number block in
      let gas =
        match block with Eth {gasUsed; _} -> gasUsed | _ -> Qty.zero
      in
      let*! () = export_gas_used gas in
      if Z.(equal (succ before_height) after_height) then
        return (Apply_success {evm_state; block})
      else return Apply_failure
  | _ -> return Apply_failure

let delete ~kind evm_state path =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn path in
  let* pvm_state = Pvm.Kernel.decode evm_state in
  let* durable = Tezos_scoru_wasm.Durable.delete ~kind pvm_state.durable key in
  Pvm.Kernel.encode {pvm_state with durable} evm_state

let clear_delayed_inbox evm_state =
  delete ~kind:Directory evm_state Durable_storage_path.delayed_inbox

let wasm_pvm_version state = Pvm.Kernel.get_wasm_version state

let irmin_store_path ~data_dir = Filename.Infix.(data_dir // "store")

let preload_kernel ~pool evm_state =
  let open Lwt_syntax in
  let* loaded =
    Wasm_runtime.preload_kernel ~pool (Pvm.Wasm_internal.to_irmin_exn evm_state)
  in
  if loaded then
    let* version = kernel_version evm_state in
    Events.preload_kernel version
  else return_unit

let get_delayed_inbox_item evm_state hash =
  let open Lwt_result_syntax in
  let*! bytes =
    inspect
      evm_state
      (Durable_storage_path.Delayed_transaction.transaction hash)
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

let clear_events evm_state =
  delete ~kind:Directory evm_state Durable_storage_path.Evm_events.events

let clear_block_storage chain_family block evm_state =
  let open Lwt_syntax in
  let* storage_version = Lwt_result.get_exn (storage_version evm_state) in
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
    let root = Durable_storage_path.root_of_chain_family chain_family in
    let block_parent = L2_types.block_parent block in
    let block_number = L2_types.block_number block in
    let (Qty number) = block_number in
    (* Handles case (1.). *)
    let* evm_state =
      if number > Z.zero then
        let pred_block_path =
          Durable_storage_path.Block.by_hash ~root block_parent
        in
        delete ~kind:Value evm_state pred_block_path
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
        let index_path =
          Durable_storage_path.Indexes.block_by_number
            ~root
            (Nth (Z.sub number to_keep))
        in
        delete ~kind:Value evm_state index_path
      else return evm_state
    in
    (* Receipts are not necessary for the kernel, we can just remove
     the directories. *)
    let* evm_state =
      delete
        ~kind:Directory
        evm_state
        Durable_storage_path.Transaction_receipt.receipts
    in
    let* evm_state =
      delete
        ~kind:Directory
        evm_state
        Durable_storage_path.Transaction_object.objects
    in
    return evm_state

let delayed_inbox_hashes evm_state =
  let open Lwt_syntax in
  let* keys =
    subkeys evm_state Durable_storage_path.Delayed_transaction.hashes
  in
  let hashes =
    (* Remove the empty, meta keys *)
    List.filter_map
      (fun key ->
        if key = "" || key = "meta" then None
        else Some (Ethereum_types.hash_of_string key))
      keys
  in
  return hashes
