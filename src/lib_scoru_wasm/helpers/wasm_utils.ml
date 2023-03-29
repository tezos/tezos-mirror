(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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
open Encodings_util
open Tezos_lazy_containers
module Wasm = Wasm_pvm.Make (Tree)
module Wasm_fast = Tezos_scoru_wasm_fast.Pvm.Make (Tree)

let parse_module code =
  let def = Parse.string_to_module code in
  match def.it with
  | Script.Textual m -> m
  | _ -> Stdlib.failwith "Failed to parse WebAssembly module"

let wat2wasm code =
  let modul = parse_module code in
  Encode.encode modul

let default_max_tick = 100000L

let production_max_tick = 11_000_000_000L

let default_outbox_validity_period = 10l

let default_outbox_message_limit = Z.of_int32 100l

let initial_tree ~version ?(ticks_per_snapshot = default_max_tick)
    ?(max_reboots = Constants.maximum_reboots_per_input) ?(from_binary = false)
    ?(outbox_validity_period = default_outbox_validity_period)
    ?(outbox_message_limit = default_outbox_message_limit) code =
  let open Lwt.Syntax in
  let max_tick_Z = Z.of_int64 ticks_per_snapshot in
  let* tree = empty_tree () in
  let* tree = Wasm.initial_state version tree in
  let* boot_sector = if from_binary then Lwt.return code else wat2wasm code in
  let* tree =
    Wasm.install_boot_sector
      ~ticks_per_snapshot:max_tick_Z
      ~outbox_validity_period
      ~outbox_message_limit
      boot_sector
      tree
  in
  Wasm.Internal_for_tests.set_maximum_reboots_per_input max_reboots tree

let reveal_builtins =
  Tezos_scoru_wasm.Builtins.
    {
      reveal_preimage =
        (fun _hash ->
          Stdlib.failwith
            "reveal_preimage is not available out of the box in tests");
      reveal_metadata =
        (fun () ->
          Stdlib.failwith
            "reveal_metadata is not available out of the box in tests");
    }

let eval_until_stuck ?(reveal_builtins = reveal_builtins) ?write_debug
    ?(max_steps = 20000L) tree =
  let open Lwt.Syntax in
  let rec go counter tree =
    let* tree, _ =
      Wasm.compute_step_many ~reveal_builtins ?write_debug ~max_steps tree
    in
    let* stuck = Wasm.Internal_for_tests.is_stuck tree in
    match stuck with
    | Some stuck -> Lwt_result.return (stuck, tree)
    | _ ->
        if counter > 0L then go (Int64.pred counter) tree
        else failwith "Failed to get stuck in time"
  in
  go max_steps tree

(* This function relies on the invariant that `compute_step_many` will always
   stop at a Snapshot or an input request, and never start another
   `kernel_run`. *)
let rec eval_to_snapshot ?(reveal_builtins = reveal_builtins) ?write_debug
    ?(max_steps = Int64.max_int) tree =
  let open Lwt_syntax in
  let eval tree =
    let* tree, _ =
      Wasm.compute_step_many
        ~reveal_builtins
        ?write_debug
        ~stop_at_snapshot:true
        ~max_steps
        tree
    in
    let* state = Wasm.Internal_for_tests.get_tick_state tree in
    match state with
    | Snapshot | Collect -> return tree
    | _ -> eval_to_snapshot ~max_steps ~reveal_builtins ?write_debug tree
  in
  let* info = Wasm.get_info tree in
  match info.input_request with
  | No_input_required -> eval tree
  | Input_required | Reveal_required _ ->
      Stdlib.failwith "Cannot reach snapshot point"

(** [eval_until_input_requested tree] will either
    - return tree if input is required
    - or run compute_step_many to reach a point where input is required *)
let eval_until_input_requested ?(reveal_builtins = Some reveal_builtins)
    ?write_debug ?after_fast_exec ?(fast_exec = false)
    ?(max_steps = Int64.max_int) tree =
  let open Lwt_syntax in
  let run =
    if fast_exec then
      Wasm_fast.Internal_for_tests.compute_step_many_with_hooks ?after_fast_exec
    else Wasm.compute_step_many
  in
  let* info = Wasm.get_info tree in
  match info.input_request with
  | No_input_required ->
      let* tree, _ = run ?reveal_builtins ?write_debug ~max_steps tree in
      return tree
  | Input_required | Reveal_required _ -> return tree

let eval_until_input_or_reveal_requested =
  eval_until_input_requested ~reveal_builtins:None

let input_info level message_counter =
  Wasm_pvm_state.
    {
      inbox_level =
        Option.value_f ~default:(fun () -> assert false)
        @@ Tezos_base.Bounded.Non_negative_int32.of_value level;
      message_counter;
    }

let new_message_counter () =
  let c = ref Z.zero in
  fun () ->
    c := Z.succ !c ;
    Z.pred !c

let set_sol_input level tree =
  let sol_input =
    Pvm_input_kind.(
      Internal_for_tests.to_binary_input (Internal Start_of_level) None)
  in
  Wasm.set_input_step (input_info level Z.zero) sol_input tree

let set_protocol_migration_input proto level tree =
  let sol_input =
    Pvm_input_kind.(
      Internal_for_tests.to_binary_input
        (Internal (Protocol_migration proto))
        None)
  in
  Wasm.set_input_step (input_info level Z.one) sol_input tree

let set_info_per_level_input ?(migration_block = false) level tree =
  let block_hash = Block_hash.zero in
  let timestamp = Time.Protocol.epoch in
  let info_res =
    Data_encoding.(
      Binary.to_string
        (tup2 Time.Protocol.encoding Block_hash.encoding)
        (timestamp, block_hash))
  in
  match info_res with
  | Ok info ->
      let info_per_level_input =
        Pvm_input_kind.(
          Internal_for_tests.to_binary_input
            (Internal Info_per_level)
            (Some info))
      in
      Wasm.set_input_step
        (input_info level (if migration_block then Z.of_int 2 else Z.one))
        info_per_level_input
        tree
  | Error _ ->
      (* There's no reason the encoding has failed, but we return the tree
         anyway *)
      Stdlib.failwith "Info_per_level encoding has failed, this is impossible"

(* Puts a message into the inbox, where the message already includes
   Internal vs External etc. information in the payload.
*)
let set_raw_message level counter message tree =
  Wasm.set_input_step (input_info level counter) message tree

let set_internal_message level counter message tree =
  let encoded_message =
    Pvm_input_kind.(
      Internal_for_tests.to_binary_input (Internal Transfer) (Some message))
  in
  Wasm.set_input_step (input_info level counter) encoded_message tree

let set_eol_input level counter tree =
  let sol_input =
    Pvm_input_kind.(
      Internal_for_tests.to_binary_input (Internal End_of_level) None)
  in
  Wasm.set_input_step (input_info level counter) sol_input tree

let set_inputs_step ?migrate_to set_internal_message messages level tree =
  let open Lwt_syntax in
  let next_message_counter = new_message_counter () in
  let (_ : Z.t) = next_message_counter () in
  let* tree = set_sol_input level tree in
  let* tree =
    match migrate_to with
    | Some proto ->
        let+ tree = set_protocol_migration_input proto level tree in
        let (_ : Z.t) = next_message_counter () in
        tree
    | None -> return tree
  in
  let (_ : Z.t) = next_message_counter () in
  let* tree =
    set_info_per_level_input
      ~migration_block:(Option.is_some migrate_to)
      level
      tree
  in
  let* tree =
    List.fold_left_s
      (fun tree message ->
        set_internal_message level (next_message_counter ()) message tree)
      tree
      messages
  in
  set_eol_input level (next_message_counter ()) tree

let set_full_input_step_gen ?migrate_to set_internal_message messages level tree
    =
  let open Lwt_syntax in
  let* tree =
    set_inputs_step ?migrate_to set_internal_message messages level tree
  in
  eval_to_snapshot ~max_steps:Int64.max_int tree

let set_full_input_step ?migrate_to =
  set_full_input_step_gen ?migrate_to set_internal_message

let set_full_raw_input_step ?migrate_to =
  set_full_input_step_gen set_raw_message ?migrate_to

let set_empty_inbox_step ?migrate_to level tree =
  set_full_input_step ?migrate_to [] level tree

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

(** [eval_to_result tree] tries to evaluates the PVM until the next `SK_Result`
    or `SK_Trap`, and stops in case of reveal tick or input tick. It has the
    property that the memory hasn't been flushed yet and can be inspected. *)
let eval_to_result ?write_debug ?reveal_builtins tree =
  let open Lwt_syntax in
  let should_compute pvm_state =
    let+ input_request_val = Wasm_vm.get_info pvm_state in
    match (input_request_val.input_request, pvm_state.tick_state) with
    | Reveal_required _, _ | Input_required, _ -> false
    | ( No_input_required,
        Eval
          {
            config =
              {
                step_kont =
                  Tezos_webassembly_interpreter.Eval.(
                    SK_Result _ | SK_Trapped _);
                _;
              };
            _;
          } ) ->
        false
    | No_input_required, _ -> true
  in
  let* pvm_state =
    Tree_encoding_runner.decode Wasm_pvm.pvm_state_encoding tree
  in
  Wasm.Internal_for_tests.compute_step_many_until
    ?write_debug
    ?reveal_builtins
    ~max_steps:(Z.to_int64 pvm_state.max_nb_ticks)
    should_compute
    tree

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

let pp_interpreter_error out
    Wasm_pvm_errors.{raw_exception = Truncated raw_exception; explanation} =
  Format.fprintf
    out
    "@[<hv 2>{ raw_exception: %s; explanation: %s }@]"
    raw_exception
    (match explanation with
    | None -> "None"
    | Some (Truncated s) -> "Some: " ^ s)

let pp_fallback_cause out = function
  | Wasm_pvm_errors.Decode_cause error ->
      Format.fprintf out "@[<hv 2>Decode_cause %a@]" pp_interpreter_error error
  | Wasm_pvm_errors.Link_cause (Truncated error) ->
      Format.fprintf out "@[<hv 2>Link_cause %s@]" error
  | Wasm_pvm_errors.Init_cause error ->
      Format.fprintf out "@[<hv 2>Init_cause %a@]" pp_interpreter_error error

let pp_error_state out = function
  | Wasm_pvm_errors.Eval_error error ->
      Format.fprintf out "@[<hv 2>Eval_error %a@]" pp_interpreter_error error
  | Wasm_pvm_errors.Decode_error error ->
      Format.fprintf out "@[<hv 2>Decode_error %a@]" pp_interpreter_error error
  | Wasm_pvm_errors.Link_error (Truncated error) ->
      Format.fprintf out "@[<hv 2>Link_error %s@]" error
  | Wasm_pvm_errors.Init_error error ->
      Format.fprintf out "@[<hv 2>Init_error %a@]" pp_interpreter_error error
  | Wasm_pvm_errors.Invalid_state (Truncated err) ->
      Format.fprintf out "@[<hv 2>Invalid_state (%s)@]" err
  | Wasm_pvm_errors.Unknown_error (Truncated err) ->
      Format.fprintf out "@[<hv 2>Unknown_error (%s)@]" err
  | Wasm_pvm_errors.Too_many_ticks ->
      Format.fprintf out "@[<hv 2>Too_many_ticks@]"
  | Wasm_pvm_errors.Too_many_reboots ->
      Format.fprintf out "@[<hv 2>Too_many_reboots@]"
  | Wasm_pvm_errors.No_fallback_kernel cause ->
      Format.fprintf
        out
        "@[<hv 2>No_fallback_kernel (%a)@]"
        pp_fallback_cause
        cause

let print_error_state = Format.asprintf "%a" pp_error_state

let pp_state fmt state =
  let pp_s s = Format.fprintf fmt "%s" s in
  match state with
  | Wasm_pvm_state.Internal_state.Snapshot -> pp_s "Snapshot"
  | Decode _ -> pp_s "Decode"
  | Eval
      {
        config =
          {step_kont = Tezos_webassembly_interpreter.Eval.(SK_Result _); _};
        _;
      } ->
      pp_s "Evaluation succeeded"
  | Eval
      {
        config =
          {step_kont = Tezos_webassembly_interpreter.Eval.(SK_Trapped msg); _};
        _;
      } ->
      Format.fprintf fmt "Evaluation failed (%s)" msg.it
  | Eval _ -> Format.fprintf fmt "Eval"
  | Stuck e -> Format.fprintf fmt "Stuck (%a)" pp_error_state e
  | Init _ -> pp_s "Init"
  | Collect -> pp_s "Collect"
  | Link _ -> pp_s "Link"
  | Padding -> pp_s "Padding"

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
  | ( Some `No_fallback_decode,
      Wasm_pvm_errors.No_fallback_kernel
        (Wasm_pvm_errors.Decode_cause {explanation; _}) ) ->
      check_reason explanation
  | Some `Init, Init_error {explanation; _} -> check_reason explanation
  | ( Some `No_fallback_init,
      Wasm_pvm_errors.No_fallback_kernel
        (Wasm_pvm_errors.Init_cause {explanation; _}) ) ->
      check_reason explanation
  | Some `Link, Link_error explanation -> check_reason (Some explanation)
  | ( Some `No_fallback_link,
      Wasm_pvm_errors.No_fallback_kernel
        (Wasm_pvm_errors.Link_cause explanation) ) ->
      check_reason (Some explanation)
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
  Tezos_webassembly_interpreter.Durable_storage.of_tree tree

let has_stuck_flag tree =
  let open Lwt_syntax in
  let* durable = wrap_as_durable_storage tree in
  let durable = Durable.of_storage_exn durable in
  let+ allows_stuck = Durable.(find_value durable Constants.stuck_flag_key) in
  Option.is_some allows_stuck

let make_durable list_key_vals =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let* tree =
    Tree_encoding_runner.encode
      (Tezos_tree_encoding.value ["durable"; "@"; "keep_me"] Data_encoding.bool)
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

let make_module_inst ~version list_key_vals src =
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
  (module_reg, module_key, Host_funcs.all ~version)

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
  (* Kernel failing at `kernel_run` invocation. *)
  let unreachable_kernel = "unreachable"
end

let project_root =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some x -> x
  | None -> (
      match Sys.getenv_opt "PWD" with
      | Some x -> x
      | None ->
          (* For some reason, under [dune runtest], [PWD] and
             [getcwd] have different values. [getcwd] is in
             [_build/default], and [PWD] is where [dune runtest] was
             executed, which is closer to what we want. *)
          Sys.getcwd ())

let ( // ) = Filename.concat

let test_with_kernel kernel (test : string -> (unit, _) result Lwt.t) () =
  let open Lwt_result_syntax in
  let kernel_file =
    project_root // Filename.dirname __FILE__ // "../test/wasm_kernels"
    // (kernel ^ ".wasm")
  in

  let* () =
    Lwt_io.with_file ~mode:Lwt_io.Input kernel_file (fun channel ->
        let*! kernel = Lwt_io.read channel in
        test kernel)
  in
  return_unit

let read_test_messages names =
  let locate_file name =
    project_root // Filename.dirname __FILE__ // "../test/messages" // name
  in
  List.map_s
    (fun name ->
      let message_file = locate_file name in
      Lwt_io.with_file ~mode:Lwt_io.Input message_file Lwt_io.read)
    names

(** Can be passed to be used as a host function
    [compute_step_many ~write_debug:write_debug_on_stdout ...] *)
let write_debug_on_stdout =
  Tezos_scoru_wasm.Builtins.Printer
    (fun msg -> Lwt.return @@ Format.printf "%s\n%!" msg)
