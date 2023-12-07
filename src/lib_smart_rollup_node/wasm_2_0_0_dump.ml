(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [get_wasm_pvm_state ~l2_header data_dir] reads the WASM PVM state in
    [data_dir] for the given [l2_header].*)
let get_wasm_pvm_state ~(l2_header : Sc_rollup_block.header) data_dir =
  let open Lwt_result_syntax in
  let context_hash = l2_header.context in
  let block_hash = l2_header.block_hash in
  (* Now, we can checkout the state of the rollup of the given block hash *)
  let* context =
    Context.load
      (module Irmin_context)
      ~cache_size:0
      Tezos_layer2_store.Store_sigs.Read_only
      (Configuration.default_context_dir data_dir)
  in
  let*! ctxt = Context.checkout context context_hash in
  let* ctxt =
    match ctxt with
    | None ->
        tzfail
          (Rollup_node_errors.Cannot_checkout_context
             (block_hash, Some context_hash))
    | Some ctxt -> return ctxt
  in
  let*! state = Context.PVMState.find ctxt in
  match state with
  | Some s -> return s
  | None -> failwith "No PVM state found for block %a" Block_hash.pp block_hash

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
let generate_durable_storage ~(plugin : (module Protocol_plugin_sig.S)) tree =
  let open Lwt_syntax in
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
  let* store =
    Store.load
      Tezos_layer2_store.Store_sigs.Read_only
      ~index_buffer_size:0
      ~l2_blocks_cache_size:5
      (Configuration.default_storage_dir data_dir)
  in
  let get name load =
    let* value = load () in
    match value with
    | Some v -> return v
    | None -> failwith "%s not found in the rollup node storage" name
  in
  let hash_from_level l =
    get (Format.asprintf "Block hash for level %ld" l) (fun () ->
        Store.Levels_to_hashes.find store.levels_to_hashes l)
  in
  let block_from_hash h =
    get (Format.asprintf "Block with hash %a" Block_hash.pp h) (fun () ->
        Store.L2_blocks.read store.l2_blocks h)
  in
  let get_l2_head () =
    get "Processed L2 head" (fun () -> Store.L2_head.read store.l2_head)
  in
  let* block_hash, block_level =
    match block with
    | `Genesis -> failwith "Genesis not supported"
    | `Head 0 ->
        let* {header = {block_hash; level; _}; _} = get_l2_head () in
        return (block_hash, level)
    | `Head offset ->
        let* {header = {level; _}; _} = get_l2_head () in
        let l = Int32.(sub level (of_int offset)) in
        let* h = hash_from_level l in
        return (h, l)
    | `Alias (_, _) -> failwith "Alias not supported"
    | `Hash (h, 0) ->
        let* _block, {block_hash; level; _} = block_from_hash h in
        return (block_hash, level)
    | `Hash (h, offset) ->
        let* _block, block_header = block_from_hash h in
        let l = Int32.(sub block_header.level (of_int offset)) in
        let* h = hash_from_level l in
        return (h, l)
    | `Level l ->
        let* h = hash_from_level l in
        return (h, l)
  in
  let* (plugin : (module Protocol_plugin_sig.S)) =
    Protocol_plugins.proto_plugin_for_level_with_store store block_level
  in
  let* l2_header = Store.L2_blocks.header store.l2_blocks block_hash in
  let* l2_header =
    match l2_header with
    | None -> tzfail Rollup_node_errors.Cannot_checkout_l2_header
    | Some header -> return header
  in
  let* state = get_wasm_pvm_state ~l2_header data_dir in
  let* instrs = generate_durable_storage ~plugin state in
  let*? contents =
    if Filename.check_suffix file ".yaml" then Installer_config.emit_yaml instrs
    else
      Ok
        (Data_encoding.Json.construct Installer_config.encoding instrs
        |> Data_encoding.Json.to_string)
  in
  let*! () =
    Lwt_io.with_file ~mode:Lwt_io.Output file (fun oc ->
        Lwt_io.write_from_string_exactly oc contents 0 (String.length contents))
  in
  return_unit
