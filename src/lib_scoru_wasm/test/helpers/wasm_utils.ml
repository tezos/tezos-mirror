(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Tezos_webassembly_interpreter
open Tezos_scoru_wasm
open Test_encodings_util
open Tezos_lazy_containers
module Wasm = Wasm_pvm.Make (Tree)

let parse_module code =
  let def = Parse.string_to_module code in
  match def.it with
  | Script.Textual m -> m
  | _ -> Stdlib.failwith "Failed to parse WebAssembly module"

let wat2wasm code =
  let modul = parse_module code in
  Encode.encode modul

let default_max_tick = 100000L

let initial_tree ?(max_tick = default_max_tick)
    ?(max_reboots = Constants.maximum_reboots_per_input) ?(from_binary = false)
    code =
  let open Lwt.Syntax in
  let max_tick_Z = Z.of_int64 max_tick in
  let* empty_tree = empty_tree () in
  let* boot_sector = if from_binary then Lwt.return code else wat2wasm code in
  let* tree = Wasm.install_boot_sector boot_sector empty_tree in
  let* tree = Wasm.Internal_for_tests.set_max_nb_ticks max_tick_Z tree in
  Wasm.Internal_for_tests.set_maximum_reboots_per_input max_reboots tree

let eval_until_stuck ?(max_steps = 20000L) tree =
  let open Lwt.Syntax in
  let rec go counter tree =
    let* tree, _ = Wasm.compute_step_many ~max_steps tree in
    let* stuck = Wasm.Internal_for_tests.is_stuck tree in
    match stuck with
    | Some stuck -> Lwt_result.return (stuck, tree)
    | _ ->
        if counter > 0L then go (Int64.pred counter) tree
        else failwith "Failed to get stuck in time"
  in
  go max_steps tree

let rec eval_until_input_requested ?(max_steps = Int64.max_int) tree =
  let open Lwt_syntax in
  let* info = Wasm.get_info tree in
  match info.input_request with
  | No_input_required ->
      let* tree, _ = Wasm.compute_step_many ~max_steps tree in
      eval_until_input_requested tree
  | Input_required -> return tree
  | Reveal_required _ -> return tree

let rec eval_until_init tree =
  let open Lwt_syntax in
  let* state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  match state_after_first_message with
  | Stuck _ | Init _ -> return tree
  | _ ->
      let* tree = Wasm.compute_step tree in
      eval_until_init tree

let set_input_step message message_counter tree =
  let input_info =
    Wasm_pvm_state.
      {
        inbox_level =
          Option.value_f ~default:(fun () -> assert false)
          @@ Tezos_base.Bounded.Non_negative_int32.of_value 0l;
        message_counter = Z.of_int message_counter;
      }
  in
  Wasm.set_input_step input_info message tree

let pp_state fmt state =
  let pp_s s = Format.fprintf fmt "%s" s in
  match state with
  | Wasm_pvm_state.Internal_state.Decode _ -> pp_s "Decode"
  | Eval _ -> pp_s "Eval"
  | Stuck e ->
      Format.fprintf fmt "Stuck (%a)" Test_wasm_pvm_encodings.pp_error_state e
  | Init _ -> pp_s "Init"
  | Snapshot -> pp_s "Snapshot"
  | Link _ -> pp_s "Link"

(** [check_error kind reason error] checks a Wasm PVM error [error] is of a
    given [kind] with a possible [reason].

    - If [kind] is [None], returns true.

    - If [reason] is [None], it simply check the given kind, otherwise it
    actually check the reason in the error. *)
let check_error ?expected_kind ?expected_reason error =
  let check_reason actual_reason =
    match expected_reason with
    | None -> true
    | _ ->
        Option.map Wasm_pvm_errors.truncate_message expected_reason
        = actual_reason
  in
  match (expected_kind, error) with
  | Some `Decode, Wasm_pvm_errors.Decode_error {explanation; _} ->
      check_reason explanation
  | Some `Init, Init_error {explanation; _} -> check_reason explanation
  | Some `Link, Link_error explanation -> check_reason (Some explanation)
  | Some `Eval, Eval_error {explanation; _} -> check_reason explanation
  | Some `Invalid_state, Invalid_state explanation ->
      check_reason (Some explanation)
  (* Unknown_error encapsulate a raw exception produced by `Printexc.to_string`.
     It depends on the backend, if there are registered printers or not, it is
     not safe to rely on its string representation. *)
  | Some `Unknown, Unknown_error _ -> true
  | Some `Too_many_ticks, Too_many_ticks -> true
  | Some `Too_many_reboots, Too_many_reboots -> true
  (* The expected step doesn't corresponds to the actual stuck step. *)
  | Some _, _ -> false
  (* No check to do, we simply assume the PVM is in a stuck state. *)
  | None, _ -> true

let is_stuck ?step ?reason = function
  | Wasm_pvm_state.Internal_state.Stuck err ->
      check_error ?expected_kind:step ?expected_reason:reason err
  | _ -> false

let wrap_as_durable_storage tree =
  let open Lwt.Syntax in
  let+ tree =
    Tree_encoding_runner.decode
      Tezos_tree_encoding.(scope ["durable"] wrapped_tree)
      tree
  in
  Tezos_webassembly_interpreter.Durable_storage.of_tree
  @@ Tezos_tree_encoding.Wrapped.wrap tree

let make_durable list_key_vals =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let* tree =
    Tree_encoding_runner.encode
      (Tezos_tree_encoding.value ["durable"; "_"; "keep_me"] Data_encoding.bool)
      true
      tree
  in
  let* durable = wrap_as_durable_storage tree in
  let+ tree =
    List.fold_left
      (fun acc (key, value) ->
        let* tree = acc in
        let key = Durable.key_of_string_exn key in
        Durable.write_value_exn tree key 0L value)
      (Lwt.return @@ Durable.of_storage_exn durable)
      list_key_vals
  in
  Durable.to_storage tree

let make_module_inst list_key_vals src =
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memory = Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l}) in
  let _ =
    List.fold_left
      (fun acc key ->
        let _ = Memory.store_bytes memory acc key in
        Int32.add acc @@ Int32.of_int (String.length key))
      src
      list_key_vals
  in
  let memories = Lazy_vector.Int32Vector.cons memory module_inst.memories in
  let module_inst = {module_inst with memories} in
  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;
  (module_reg, module_key, Host_funcs.all)

let retrieve_memory module_reg =
  let open Lwt_syntax in
  let* (module_inst : Instance.module_inst) =
    Instance.ModuleMap.get "test" module_reg
  in
  let memories = module_inst.memories in
  if Lazy_vector.Int32Vector.num_elements memories = 1l then
    Lazy_vector.Int32Vector.get 0l memories
  else assert false

module Kernels = struct
  (* Kernel failing at `kernel_next` invocation. *)
  let unreachable_kernel = "unreachable"
end

let test_with_kernel kernel (test : string -> (unit, _) result Lwt.t) () =
  let open Tezt.Base in
  let open Lwt_result_syntax in
  (* Reading files using `Tezt_lib` can be fragile and not future-proof, see
     issue https://gitlab.com/tezos/tezos/-/issues/3746. *)
  let kernel_file =
    project_root // Filename.dirname __FILE__ // "../wasm_kernels"
    // (kernel ^ ".wasm")
  in
  let* () =
    Lwt_io.with_file ~mode:Lwt_io.Input kernel_file (fun channel ->
        let*! kernel = Lwt_io.read channel in
        test kernel)
  in
  return_unit
