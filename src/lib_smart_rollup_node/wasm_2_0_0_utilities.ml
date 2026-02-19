(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let load_context ~data_dir (module Plugin : Protocol_plugin_sig.S) mode =
  let (module C) = Plugin.Pvm.context Wasm_2_0_0 in
  Context.load
    (module C)
    ~cache_size:0
    mode
    (Configuration.default_context_dir data_dir)

(** [get_wasm_pvm_state ctxt block_hash context_hash] reads the WASM PVM state
    in [ctxt] for the given [context_hash].*)
let get_wasm_pvm_state context block_hash context_hash =
  let open Lwt_result_syntax in
  (* Now, we can checkout the state of the rollup of the given block hash *)
  let*! ctxt = Context.checkout context context_hash in
  match ctxt with
  | None ->
      tzfail
        (Rollup_node_errors.Cannot_checkout_context
           (block_hash, Some context_hash))
  | Some ctxt -> (
      let*! state = Context.PVMState.find ctxt in
      match state with
      | Some s -> return (ctxt, s)
      | None ->
          failwith "No PVM state found for block %a" Block_hash.pp block_hash)

(** [decode_value tree] decodes a durable storage value from the given tree. *)
let decode_value ~(pvm : (module Pvm_plugin_sig.S)) tree =
  let open Lwt_syntax in
  let module Pvm : Pvm_plugin_sig.S = (val pvm) in
  let* cbv =
    Pvm.Wasm_2_0_0.decode_durable_state
      Tezos_lazy_containers.Chunked_byte_vector.encoding
      tree
  in
  Tezos_lazy_containers.Chunked_byte_vector.to_string cbv

(** Returns whether the value under the current key should be dumped. *)
let check_dumpable_path key =
  match key with
  (* The /readonly subpath cannot be set by the user and is reserved to the PVM.
     The kernel code is rewritten by the installer or the debugger, as such it
     doesn't need to be part of the dump. Any value under these two won't be
     part of the dump. *)
  | "readonly" :: _ | "kernel" :: "boot.wasm" :: _ -> `Nothing
  | l -> (
      match List.rev l with
      | "@" :: path -> `Value (List.rev path)
      | _ -> `Nothing)

(** [print_set_value] dumps a value in the YAML format of the installer. *)
let set_value_instr ~(pvm : (module Pvm_plugin_sig.S)) key tree =
  let open Lwt_syntax in
  let full_key = String.concat "/" key in
  let+ value = decode_value ~pvm tree in
  Installer_config.Set {value; to_ = "/" ^ full_key}

(* [generate_durable_storage tree] folds on the keys in the durable storage and
   their values and generates as set of instructions out of it. The order is not
   specified. *)
let generate_durable_storage ~(plugin : (module Protocol_plugin_sig.S)) state =
  let open Lwt_syntax in
  let tree = !(Context_wrapper.Irmin.of_node_pvmstate state) in
  let durable_path = "durable" :: [] in
  let module Plugin : Protocol_plugin_sig.S = (val plugin) in
  let* path_exists = Plugin.Pvm.Wasm_2_0_0.proof_mem_tree tree durable_path in
  if path_exists then
    (* This fold on the tree rather than on the durable storage representation
       directly. It would probably be safer but the durable storage does not
       implement a folding function yet. *)
    let* instrs =
      Plugin.Pvm.Wasm_2_0_0.proof_fold_tree
        tree
        durable_path
        ~order:`Undefined
        ~init:[]
        ~f:(fun key tree acc ->
          match check_dumpable_path key with
          | `Nothing -> return acc
          | `Value key ->
              let+ instr = set_value_instr ~pvm:(module Plugin.Pvm) key tree in
              instr :: acc)
    in
    return_ok instrs
  else failwith "The durable storage is not available in the tree\n%!"

let dump_durable_storage ~block ~data_dir ~file =
  let open Lwt_result_syntax in
  let* store = Store.init Read_only ~data_dir in
  let get name load =
    let* value = load () in
    match value with
    | Some v -> return v
    | None -> failwith "%s not found in the rollup node storage" name
  in
  let hash_from_level l =
    get (Format.asprintf "Block hash for level %ld" l) (fun () ->
        Store.L2_levels.find store l)
  in
  let block_from_hash h =
    get (Format.asprintf "Block with hash %a" Block_hash.pp h) (fun () ->
        Store.L2_blocks.find store h)
  in
  let get_l2_head () =
    get "Processed L2 head" (fun () -> Store.State.L2_head.get store)
  in
  let* block_hash, block_level =
    match block with
    | `Genesis -> failwith "Genesis not supported"
    | `Head 0 -> get_l2_head ()
    | `Head offset ->
        let* _, level = get_l2_head () in
        let l = Int32.(sub level (of_int offset)) in
        let* h = hash_from_level l in
        return (h, l)
    | `Alias (_, _) -> failwith "Alias not supported"
    | `Hash (h, 0) ->
        let* {header = {block_hash; level; _}; _} = block_from_hash h in
        return (block_hash, level)
    | `Hash (h, offset) ->
        let* block = block_from_hash h in
        let l = Int32.(sub block.header.level (of_int offset)) in
        let* h = hash_from_level l in
        return (h, l)
    | `Level l ->
        let* h = hash_from_level l in
        return (h, l)
  in
  let* (plugin : (module Protocol_plugin_sig.S)) =
    Protocol_plugins.proto_plugin_for_level_with_store store block_level
  in
  let* context_hash = Store.L2_blocks.find_context store block_hash in
  let* context_hash =
    match context_hash with
    | None -> tzfail Rollup_node_errors.Cannot_checkout_l2_header
    | Some c -> return c
  in
  let* context = load_context ~data_dir plugin Access_mode.Read_only in
  let* _ctxt, state = get_wasm_pvm_state context block_hash context_hash in
  let* instrs = generate_durable_storage ~plugin state in
  let* () = Installer_config.to_file instrs ~output:file in
  return_unit

let preload_kernel (node_ctxt : _ Node_context.t) header =
  let open Lwt_result_syntax in
  let* _ctxt, pvm_state =
    get_wasm_pvm_state
      node_ctxt.context
      header.Sc_rollup_block.block_hash
      header.context
  in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_level node_ctxt header.level
  in
  let tree = !(Context_wrapper.Irmin.of_node_pvmstate pvm_state) in
  let*! durable =
    Plugin.Pvm.Wasm_2_0_0.decode_durable_state
      Tezos_scoru_wasm.Tree_state.durable_storage_encoding
      tree
  in
  let*! () =
    Tezos_scoru_wasm_fast.Exec.preload_kernel
      ~hooks:Tezos_scoru_wasm.Hooks.no_hooks
      durable
  in
  return_unit

let patch_durable_storage ~data_dir ~key ~value =
  let open Lwt_result_syntax in
  (* Loads the state of the head. *)
  let* _lock = Node_context_loader.lock ~data_dir in
  let* store = Store.init Read_write ~data_dir in
  let* ({header = {block_hash; level = block_level; _}; _} as l2_block) =
    let* r = Store.L2_blocks.find_head store in
    match r with
    | Some v -> return v
    | None ->
        failwith "Processed L2 head is not found in the rollup node storage"
  in
  let* ((module Plugin) as plugin) =
    Protocol_plugins.proto_plugin_for_level_with_store store block_level
  in
  let* () =
    fail_when
      (Option.is_some l2_block.header.commitment_hash)
      (Rollup_node_errors.Patch_durable_storage_on_commitment block_level)
  in
  let* index = load_context ~data_dir plugin Access_mode.Read_write in
  let* context, state =
    get_wasm_pvm_state index block_hash l2_block.header.context
  in

  (* Patches the state via an unsafe patch. *)
  let* () =
    Plugin.Pvm.Unsafe.apply_patch
      Kind.Wasm_2_0_0
      state
      (Pvm_patches.Patch_durable_storage {key; value})
  in

  (* PVM state is was modified in place, replace it in Irmin context or check
     integrity in RISC-V context. *)
  let*! () = Context.PVMState.set context state in
  let*! new_commit = Context.commit context in
  let new_l2_block =
    {l2_block with header = {l2_block.header with context = new_commit}}
  in
  Store.L2_blocks.store store new_l2_block

let hooks ~check_invalid_kernel =
  let open Tezos_scoru_wasm.Hooks in
  let hooks =
    no_hooks |> on_fast_exec_panicked Interpreter_event.fast_exec_panic
  in
  if check_invalid_kernel then hooks
  else disable_fast_exec_invalid_kernel_check hooks
