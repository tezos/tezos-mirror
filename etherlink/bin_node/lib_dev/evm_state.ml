(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type config = {config : Config.config; wasm_runtime : bool}

module Bare_context = struct
  module Tree = Irmin_context.Tree

  type t = Irmin_context.rw

  type index = Irmin_context.rw_index

  type nonrec tree = Irmin_context.tree

  let init ?patch_context:_ ?readonly:_ ?index_log_size:_ path =
    let open Lwt_syntax in
    let* res = Irmin_context.load ~cache_size:100_000 Read_write path in
    match res with
    | Ok res -> return res
    | Error _ -> Lwt.fail_with "could not initialize the context"

  let empty index = Irmin_context.empty index
end

type t = Irmin_context.PVMState.value

module Wasm_utils =
  Wasm_utils.Make (Tezos_tree_encoding.Encodings_util.Make (Bare_context))
module Wasm = Wasm_debugger.Make (Wasm_utils)

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

let execute ?(wasm_pvm_fallback = false) ?(profile = false)
    ?(kind = Events.Application) ~data_dir ?(log_file = "kernel_log")
    ?(wasm_entrypoint = Tezos_scoru_wasm.Constants.wasm_entrypoint) ~config
    evm_state inbox =
  let open Lwt_result_syntax in
  let path = Filename.concat (kernel_logs_directory ~data_dir) log_file in
  let inbox = List.map (function `Input s -> s) inbox in
  let inbox = List.to_seq [inbox] in
  let messages = ref [] in
  let write_debug =
    Tezos_scoru_wasm.Builtins.Printer
      (fun msg ->
        messages := msg :: !messages ;
        event_kernel_log ~kind ~msg)
  in
  let eval evm_state =
    if profile then
      let* evm_state, _, _ =
        Wasm.Commands.profile
          ~migrate_to:Proto_alpha
          ~collapse:false
          ~with_time:true
          ~no_reboot:false
          0l
          inbox
          {config.config with flamecharts_directory = data_dir}
          Custom_section.FuncMap.empty
          evm_state
      in
      return evm_state
    else if not config.wasm_runtime then
      let hooks =
        Tezos_scoru_wasm.Hooks.(
          no_hooks
          |> fast_exec_fallback wasm_pvm_fallback
          |> disable_fast_exec_invalid_kernel_check)
      in
      let* evm_state, _, _, _ =
        Wasm.Commands.eval
          ~migrate_to:Proto_alpha
          ~hooks
          ~write_debug
          ~wasm_entrypoint
          0l
          inbox
          config.config
          Inbox
          evm_state
      in
      return evm_state
    else
      (* The [inbox] parameter is inherited from the WASM debugger, where the
         inbox is a list of list of messages (because it supports running the
         fast exec for several Tezos level in a raw).

         As far as the EVM node is concerned, we only “emulate” one Tezos level
         at a time, so we only keep the first item ([inbox] is in parctise
         always a singleton). *)
      let inbox = match Seq.uncons inbox with Some (x, _) -> x | _ -> [] in
      let*! evm_state =
        Wasm_runtime.run
          ~preimages_dir:config.config.preimage_directory
          ~entrypoint:wasm_entrypoint
          evm_state
          config.config.destination
          inbox
      in
      return evm_state
  in
  let* evm_state = eval evm_state in
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
  Wasm.set_durable_value ?edit_readonly evm_state key value

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
  let evm_state = Irmin_context.PVMState.empty () in
  let* evm_state =
    Wasm.start ~tree:evm_state Tezos_scoru_wasm.Wasm_pvm_state.V3 kernel
  in
  (* The WASM Runtime completely ignores the reboot counter, but some versions
     of the Etherlink kernel will need it to exist. *)
  let*! evm_state = init_reboot_counter evm_state in
  let*! evm_state = flag_local_exec evm_state in
  return evm_state

let inspect evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* value = Wasm.Commands.find_key_in_durable evm_state key in
  Option.map_s Tezos_lazy_containers.Chunked_byte_vector.to_bytes value

let subkeys evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* durable = Wasm_utils.wrap_as_durable_storage evm_state in
  let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
  Tezos_scoru_wasm.Durable.list durable key

let exists evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* durable = Wasm_utils.wrap_as_durable_storage evm_state in
  let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
  Tezos_scoru_wasm.Durable.exists durable key

let kernel_version evm_state =
  let open Lwt_syntax in
  let+ version = inspect evm_state Durable_storage_path.kernel_version in
  match version with Some v -> Bytes.unsafe_to_string v | None -> "(unknown)"

let current_block_height evm_state =
  let open Lwt_syntax in
  let* current_block_number =
    inspect evm_state Durable_storage_path.Block.current_number
  in
  match current_block_number with
  | None ->
      (* No block has been created yet and we are waiting for genesis,
         whose number will be [zero]. Since the semantics of [apply_blueprint]
         is to verify the block height has been incremented once, we default to
         [-1]. *)
      return (Qty Z.(pred zero))
  | Some current_block_number ->
      let (Qty current_block_number) = decode_number_le current_block_number in
      return (Qty current_block_number)

let current_block_hash evm_state =
  let open Lwt_result_syntax in
  let*! current_hash =
    inspect evm_state Durable_storage_path.Block.current_hash
  in
  match current_hash with
  | Some h -> return (decode_block_hash h)
  | None -> return genesis_parent_hash

(* The Fast Execution engine relies on Lwt_preemptive to execute Wasmer in
   dedicated worker threads (`Lwt_preemptive.detach`), while pushing to lwt
   event loop the resolutions of host functions (notably interacting with
   Irmin) (`Lwt_preemptive.run_in_main`).

   This means that parallel executions of the Etherlink kernel using the Fast
   Execution engine competes for the access of the event loop. In practice, it
   means it is possible for an attacker to slow down block production through
   RPC calls leading to execute the Etherlink kernel (typically,
   `eth_estimateGas`).

   Under the hood, `Lwt_preemptive` relies on a pool of 4 workers (implemented
   using native threads). With this patch, we ensure 1 of these workers is
   always available to the block production, by limiting the concurrent call to
   `execute_and_inspect` to 3. *)
let pool = Lwt_pool.create 3 (fun () -> Lwt.return_unit)

let execute_and_inspect ?wasm_pvm_fallback ~data_dir ?wasm_entrypoint ~config
    ~input:
      Simulation.Encodings.
        {messages; insight_requests; log_kernel_debug_file; _} ctxt =
  let open Lwt_result_syntax in
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
  let messages = List.map (fun s -> `Input s) messages in
  let* evm_state =
    Lwt_pool.use pool @@ fun () ->
    execute
      ?wasm_pvm_fallback
      ~kind:Simulation
      ?log_file:log_kernel_debug_file
      ~data_dir
      ?wasm_entrypoint
      ~config
      ctxt
      messages
  in
  let*! values = List.map_p (fun key -> inspect evm_state key) keys in
  return values

type apply_result =
  | Apply_success of {evm_state : t; block : Ethereum_types.block}
  | Apply_failure

let apply_blueprint ?wasm_pvm_fallback ?log_file ?profile ~data_dir ~config
    evm_state (blueprint : Blueprint_types.payload) =
  let open Lwt_result_syntax in
  let exec_inputs =
    List.map
      (function `External payload -> `Input ("\001" ^ payload))
      blueprint
  in
  let*! (Qty before_height) = current_block_height evm_state in
  let* evm_state =
    execute
      ?wasm_pvm_fallback
      ?profile
      ~data_dir
      ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
      ~config
      ?log_file
      evm_state
      exec_inputs
  in
  let* block_hash = current_block_hash evm_state in
  let* block =
    let*! bytes =
      inspect evm_state (Durable_storage_path.Block.by_hash block_hash)
    in
    return (Option.map Ethereum_types.block_from_rlp bytes)
  in
  match block with
  | Some ({number = Qty after_height; _} as block)
    when Z.(equal (succ before_height) after_height) ->
      return (Apply_success {evm_state; block})
  | _ -> return Apply_failure

let delete ~kind evm_state path =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn path in
  let* pvm_state =
    Wasm_utils.Ctx.Tree_encoding_runner.decode
      Tezos_scoru_wasm.Wasm_pvm.pvm_state_encoding
      evm_state
  in
  let* durable = Tezos_scoru_wasm.Durable.delete ~kind pvm_state.durable key in
  Wasm_utils.Ctx.Tree_encoding_runner.encode
    Tezos_scoru_wasm.Wasm_pvm.pvm_state_encoding
    {pvm_state with durable}
    evm_state

let clear_delayed_inbox evm_state =
  delete ~kind:Directory evm_state Durable_storage_path.delayed_inbox

let wasm_pvm_version state = Wasm_utils.Wasm.get_wasm_version state

let irmin_store_path ~data_dir = Filename.Infix.(data_dir // "store")

let preload_kernel evm_state =
  let open Lwt_syntax in
  let* pvm_state =
    Wasm_utils.Ctx.Tree_encoding_runner.decode
      Tezos_scoru_wasm.Wasm_pvm.pvm_state_encoding
      evm_state
  in
  let hooks =
    Tezos_scoru_wasm.Hooks.(no_hooks |> disable_fast_exec_invalid_kernel_check)
  in
  let* () =
    Tezos_scoru_wasm_fast.Exec.preload_kernel ~hooks pvm_state.durable
  in
  let* version = kernel_version evm_state in
  Events.preload_kernel version

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
             hash
             rlp_item
      in
      return res
  | _ -> failwith "invalid delayed inbox item"

let clear_block_storage block evm_state =
  let open Lwt_syntax in
  (* We have 2 path to clear related to block storage:
     1. The predecessor block.
     2. The indexes (which is a bit trickier).

     We can remove only the predecessor block because the current block is
     necessary to produce the next block. Block production starts by reading
     the head to retrieve information such as parent block hash.
  *)
  let (Qty number) = block.number in
  (* Handles case (1.). *)
  let* evm_state =
    if number > Z.zero then
      let pred_block_path = Durable_storage_path.Block.by_hash block.parent in
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
