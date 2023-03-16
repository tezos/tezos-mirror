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

module Wasm = Tezos_webassembly_interpreter
open Wasm_pvm_state.Internal_state

let version_for_protocol : Pvm_input_kind.protocol -> Wasm_pvm_state.version =
  function
  | Proto_alpha -> V1

let link_finished (ast : Wasm.Ast.module_) offset =
  offset >= Wasm.Ast.Vector.num_elements ast.it.imports

let eval_has_finished = function
  | Eval {config = {step_kont = Wasm.Eval.(SK_Result _); _}; _} -> true
  | Padding -> true
  (* explicit pattern matching to avoid new states introducing silent bugs *)
  | Snapshot | Decode _ | Link _ | Init _ | Eval _ | Collect | Stuck _ -> false

let ticks_to_snapshot {current_tick; last_top_level_call; max_nb_ticks; _} =
  let open Z in
  max_nb_ticks - one - (current_tick - last_top_level_call)
(* The max number of tick is offsetted by 1, which corresponds to the input
   tick. *)

let is_time_for_snapshot pvm_state =
  Z.Compare.(ticks_to_snapshot pvm_state <= Z.zero)

let has_reboot_flag durable =
  let open Lwt_syntax in
  let+ allows_reboot =
    Lwt.catch
      (fun () -> Durable.(find_value durable Constants.reboot_flag_key))
      (function exn -> raise exn)
  in
  allows_reboot <> None

let has_stuck_flag durable =
  let open Lwt_syntax in
  let+ stuck = Durable.(find_value durable Constants.stuck_flag_key) in
  Option.is_some stuck

let has_upgrade_error_flag durable =
  let open Lwt_syntax in
  let+ error = Durable.(find_value durable Constants.upgrade_error_flag_key) in
  Option.is_some error

let get_wasm_version {durable; _} =
  let open Lwt_syntax in
  let* cbv = Durable.find_value_exn durable Constants.version_key in
  let+ bytes = Tezos_lazy_containers.Chunked_byte_vector.to_bytes cbv in
  Data_encoding.Binary.of_bytes_exn Wasm_pvm_state.version_encoding bytes

let stack_size_limit = function Wasm_pvm_state.V0 -> 300 | V1 -> 2048

let patch_flags_on_eval_successful durable =
  let open Lwt_syntax in
  (* We have an empty set of admin instructions, but need to wait until we can restart *)
  let* has_stuck_flag = has_stuck_flag durable in
  let* durable =
    if has_stuck_flag then
      Durable.(delete ~edit_readonly:true durable Constants.stuck_flag_key)
    else Lwt.return durable
  in
  let* has_upgrade_error_flag = has_upgrade_error_flag durable in
  let+ durable =
    if has_upgrade_error_flag then
      Durable.(
        delete ~edit_readonly:true durable Constants.upgrade_error_flag_key)
    else Lwt.return durable
  in
  durable

let mark_for_reboot {reboot_counter; durable; _} =
  let open Lwt_syntax in
  let+ has_reboot_flag = has_reboot_flag durable in
  if has_reboot_flag then
    if Z.Compare.(reboot_counter <= Z.zero) then `Forcing_yield else `Reboot
  else `Yielding

(* Returns true is a fallback kernel is available, and it's different to
   the currently running kernel. *)
let has_fallback_kernel durable =
  let open Lwt_syntax in
  let* kernel_hash = Durable.hash durable Constants.kernel_key in
  let+ fallback_hash = Durable.hash durable Constants.kernel_fallback_key in
  Option.is_some fallback_hash && kernel_hash <> fallback_hash

let initial_boot_state () =
  Decode
    (Tezos_webassembly_interpreter.Decode.initial_decode_kont
       ~name:Constants.wasm_main_module_name)

let save_fallback_kernel durable =
  let open Lwt.Syntax in
  let* kernel_hash = Durable.hash durable Constants.kernel_key in
  let* kernel_fallback_hash =
    Durable.hash durable Constants.kernel_fallback_key
  in
  if kernel_hash <> kernel_fallback_hash then
    Durable.copy_tree_exn
      durable
      ~edit_readonly:true
      Constants.kernel_key
      Constants.kernel_fallback_key
  else Lwt.return durable

let unsafe_next_tick_state ~version ~stack_size_limit host_funcs
    ({buffers; durable; tick_state; _} as pvm_state) =
  let open Lwt_syntax in
  let return ?(status = Running) ?(durable = durable) state =
    Lwt.return (durable, state, status)
  in
  match tick_state with
  | Stuck ((Decode_error _ | Init_error _ | Link_error _) as e) ->
      let cause =
        match e with
        | Decode_error e -> Wasm_pvm_errors.Decode_cause e
        | Link_error e -> Wasm_pvm_errors.Link_cause e
        | Init_error e -> Wasm_pvm_errors.Init_cause e
        | _ -> assert false
      in
      let* has_fallback = has_fallback_kernel durable in
      if has_fallback then
        let* durable =
          Durable.copy_tree_exn
            durable
            Constants.kernel_fallback_key
            Constants.kernel_key
        in
        let* durable =
          Durable.write_value_exn
            ~edit_readonly:true
            durable
            Constants.upgrade_error_flag_key
            0L
            ""
        in
        return ~durable Padding
      else return ~status:Failing (Stuck (No_fallback_kernel cause))
  | Stuck e -> return ~status:Failing (Stuck e)
  | Snapshot -> return (initial_boot_state ())
  | Collect ->
      return
        ~status:Failing
        (Stuck (Wasm_pvm_errors.invalid_state "Collect is a input tick"))
  | Padding when is_time_for_snapshot pvm_state -> (
      let* reboot_status = mark_for_reboot pvm_state in
      match reboot_status with
      | `Reboot -> return ~status:Reboot Snapshot
      | `Forcing_yield -> return ~status:Forcing_yield Collect
      | `Yielding -> return ~status:Yielding Collect)
  | _ when is_time_for_snapshot pvm_state ->
      (* Execution took too many ticks *)
      return ~status:Failing (Stuck Too_many_ticks)
  | Decode {module_kont = MKStop ast_module; _} ->
      return
        (Link
           {
             ast_module;
             externs = Wasm.Instance.Vector.empty ();
             imports_offset = 0l;
           })
  | Decode m ->
      let* kernel = Durable.find_value_exn durable Constants.kernel_key in
      let* m =
        Tezos_webassembly_interpreter.Decode.module_step
          ~allow_floats:false
          kernel
          m
      in
      return (Decode m)
  | Link {ast_module; externs; imports_offset}
    when link_finished ast_module imports_offset ->
      let self = Wasm.Instance.Module_key Constants.wasm_main_module_name in
      let module_reg = Wasm.Instance.ModuleMap.create () in
      (* The module instance will be registered as [self] in
         [module_reg] during the initialization. *)
      return (Init {self; ast_module; init_kont = IK_Start externs; module_reg})
  | Link {ast_module; externs; imports_offset} ->
      let* {it = {module_name; item_name; _}; _} =
        Wasm.Ast.Vector.get imports_offset ast_module.it.imports
      in
      if module_name = Constants.wasm_host_funcs_virual_module then
        match Host_funcs.lookup_opt ~version item_name with
        | Some extern ->
            let externs, _ = Wasm.Ast.Vector.append extern externs in
            return
              (Link
                 {
                   ast_module;
                   externs;
                   imports_offset = Int32.succ imports_offset;
                 })
        | None ->
            return
              ~status:Failing
              (Stuck (Wasm_pvm_errors.link_error `Item ~module_name ~item_name))
      else
        return
          ~status:Failing
          (Stuck (Wasm_pvm_errors.link_error `Module ~module_name ~item_name))
  | Init {self; ast_module = _; init_kont = IK_Stop; module_reg} -> (
      let* module_inst =
        Wasm.Instance.ModuleMap.get Constants.wasm_main_module_name module_reg
      in
      let* extern =
        Lwt.catch
          (fun () ->
            let+ extern =
              Wasm.Instance.NameMap.get
                Constants.wasm_entrypoint
                module_inst.Wasm.Instance.exports
            in
            Some extern)
          (function
            | Tezos_lazy_containers.Lazy_map.UnexpectedAccess -> return_none
            | exn -> raise exn)
      in
      match extern with
      | Some (Wasm.Instance.ExternFunc main_func) ->
          let admin_instr' = Wasm.Eval.Invoke main_func in
          let admin_instr = Wasm.Source.{it = admin_instr'; at = no_region} in
          (* Clear the values and the locals in the frame. *)
          let config =
            Wasm.Eval.config
              ~stack_size_limit
              self
              (Tezos_lazy_containers.Lazy_vector.Int32Vector.empty ())
              (Tezos_lazy_containers.Lazy_vector.Int32Vector.singleton
                 admin_instr)
          in
          (* Set kernel - now known to be valid - as fallback kernel,
             if it is not already *)
          let* durable = save_fallback_kernel durable in
          return ~durable (Eval {config; module_reg})
      | _ ->
          (* We require a function with the name [main] to be exported
             rather than any other structure. *)
          return
            ~status:Failing
            (Stuck
               (Wasm_pvm_errors.invalid_state
                  (Format.sprintf
                     "Invalid_module: no `%s` function exported"
                     Constants.wasm_entrypoint))))
  | Init {self; ast_module; init_kont; module_reg} ->
      let* init_kont =
        Wasm.Eval.init_step
          ~stack_size_limit
          ~filter_exports:true
          ~check_module_exports:Exports_memory_0
          ~module_reg
          ~self
          buffers
          host_funcs
          ast_module
          init_kont
      in
      return (Init {self; ast_module; init_kont; module_reg})
  | Padding -> return Padding
  | Eval {config = {step_kont = Wasm.Eval.(SK_Trapped _msg); _}; _} ->
      let* durable =
        Durable.write_value_exn
          ~edit_readonly:true
          durable
          Constants.stuck_flag_key
          0L
          ""
      in
      return ~durable Padding
  | _ when eval_has_finished tick_state ->
      (* We have an empty set of admin instructions, but need to wait until we can restart *)
      let* durable = patch_flags_on_eval_successful durable in
      return ~durable Padding
  | Eval {config; module_reg} ->
      (* Continue execution. *)
      let store = Durable.to_storage durable in
      let* store', config =
        Wasm.Eval.step ~host_funcs ~durable:store module_reg config buffers
      in
      let durable' = Durable.of_storage ~default:durable store' in
      return ~durable:durable' (Eval {config; module_reg})

let exn_to_stuck pvm_state exn =
  let error = Wasm_pvm_errors.extract_interpreter_error exn in
  let wasm_error =
    match error with
    | `Interpreter error -> (
        match pvm_state.tick_state with
        | Decode _ -> Wasm_pvm_errors.Decode_error error
        | Link _ -> Link_error error.Wasm_pvm_errors.raw_exception
        | Init _ -> Init_error error
        | Eval _ -> Eval_error error
        | Snapshot | Stuck _ | Collect | Padding ->
            Unknown_error error.raw_exception)
    | `Unknown raw_exception -> Unknown_error raw_exception
  in
  Lwt.return (Stuck wasm_error)

let next_tick_state ~version ~stack_size_limit host_function_registry pvm_state
    =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      unsafe_next_tick_state
        ~version
        ~stack_size_limit
        host_function_registry
        pvm_state)
    (fun exn ->
      let+ tick_state = exn_to_stuck pvm_state exn in
      (pvm_state.durable, tick_state, Failing))

let next_last_top_level_call {current_tick; last_top_level_call; _} = function
  | Forcing_yield | Yielding | Reboot -> Z.succ current_tick
  | Failing | Running -> last_top_level_call

let next_reboot_counter {reboot_counter; maximum_reboots_per_input; _} status =
  match status with
  | Reboot -> Z.pred reboot_counter
  | Forcing_yield | Yielding | Failing ->
      Z.succ maximum_reboots_per_input (* one is used to read the inbox *)
  | Running -> reboot_counter

(** When successfully restarting the VM, we can remove the
    [Too_many_reboot] flag if it exists. On the contrary, we create it
    in case we are failing. *)
let patch_too_many_reboot_flag durable =
  let open Lwt_syntax in
  function
  | Yielding ->
      Durable.delete
        ~edit_readonly:true
        durable
        Constants.too_many_reboot_flag_key
  | Forcing_yield ->
      Durable.(
        write_value_exn
          ~edit_readonly:true
          durable
          Constants.too_many_reboot_flag_key
          0L
          "")
  | _ -> return durable

(** When rebooting, we can remove the [Reboot] flag (because it has
    achieved its purpose). On the contrary, when [Yielding] or
    [Forcing_yield], we set the flag, because we will want to reboot
    once the inbox is loaded. *)
let patch_reboot_flag durable =
  let open Lwt_syntax in
  function
  | Reboot ->
      Durable.delete ~edit_readonly:true durable Constants.reboot_flag_key
  | Forcing_yield | Yielding ->
      Durable.(write_value_exn durable Constants.reboot_flag_key 0L "")
  | _ -> return durable

(** When rebooting, we update the new [reboot_counter] exposed to the
    kernel.  *)
let patch_reboot_counter durable reboot_counter =
  let open Lwt_syntax in
  function
  | Running | Failing -> return durable
  | Reboot | Forcing_yield | Yielding ->
      (* Deleting first allows to not have to fetch the previous
         contents, which is a full chunks. This reduces the size of
         the resulting proof. *)
      let* durable =
        Durable.delete ~edit_readonly:true durable Constants.reboot_counter_key
      in
      Durable.write_value_exn
        ~edit_readonly:true
        durable
        Constants.reboot_counter_key
        0L
        (* We use a standard encoding of a 32-bit integers, instead of
           the ad-hoc Z encoding, to make the life of the kernel
           developers easier. *)
        Data_encoding.(
          Binary.to_string_exn
            Data_encoding_utils.Little_endian.int32
            (Z.to_int32 reboot_counter))

(** Every time the kernel yields, we reset the input buffer. *)
let clean_up_input_buffer buffers =
  let open Tezos_webassembly_interpreter in
  function
  | Forcing_yield | Yielding -> Input_buffer.reset buffers.Eval.input | _ -> ()

(** [compute_step pvm_state] does one computation step on [pvm_state].
    Returns the new state.
*)
let compute_step_with_host_functions ~version ~stack_size_limit registry
    pvm_state =
  let open Lwt_syntax in
  (* Calculate the next tick state. *)
  let* durable, tick_state, status =
    next_tick_state ~version ~stack_size_limit registry pvm_state
  in
  let current_tick = Z.succ pvm_state.current_tick in
  let last_top_level_call = next_last_top_level_call pvm_state status in
  let reboot_counter = next_reboot_counter pvm_state status in
  let* durable = patch_too_many_reboot_flag durable status in
  let* durable = patch_reboot_flag durable status in
  let* durable = patch_reboot_counter durable reboot_counter status in
  let () = clean_up_input_buffer pvm_state.buffers status in
  let pvm_state =
    {
      pvm_state with
      tick_state;
      durable;
      current_tick;
      last_top_level_call;
      reboot_counter;
    }
  in
  return pvm_state

let compute_step pvm_state =
  let open Lwt_syntax in
  let* version = get_wasm_version pvm_state in
  let stack_size_limit = stack_size_limit version in
  compute_step_with_host_functions
    ~version
    ~stack_size_limit
    (Host_funcs.all ~version)
    pvm_state

let compute_step_with_debug ~write_debug pvm_state =
  let open Lwt_syntax in
  let* version = get_wasm_version pvm_state in
  let registry =
    match write_debug with
    | Builtins.Printer _ -> Host_funcs.all_debug ~version ~write_debug
    | Noop -> Host_funcs.all ~version
  in
  compute_step_with_host_functions
    ~version
    ~stack_size_limit:(stack_size_limit version)
    registry
    pvm_state

let input_request pvm_state =
  match pvm_state.tick_state with
  | Stuck (Decode_error _ | Init_error _ | Link_error _) ->
      (* These stuck states are recovered on the next tick by
         the fallback mechanism. *)
      Wasm_pvm_state.No_input_required
  | Stuck _ -> Wasm_pvm_state.Input_required
  | Snapshot -> Wasm_pvm_state.No_input_required
  | Collect -> Wasm_pvm_state.Input_required
  | Eval {config; _} -> (
      match Tezos_webassembly_interpreter.Eval.is_reveal_tick config with
      | Some reveal -> Wasm_pvm_state.Reveal_required reveal
      | None -> Wasm_pvm_state.No_input_required)
  | _ -> Wasm_pvm_state.No_input_required

let is_top_level_padding pvm_state =
  match pvm_state.tick_state with
  | Padding -> not @@ is_time_for_snapshot pvm_state
  | _ -> false

let measure_executed_ticks (transition : pvm_state -> pvm_state Lwt.t)
    (initial_state : pvm_state) : (pvm_state * int64) Lwt.t =
  let open Lwt.Syntax in
  let open Z in
  let+ final_state = transition initial_state in
  let ticks_executed = final_state.current_tick - initial_state.current_tick in
  (final_state, to_int64 ticks_executed)

let reveal_step payload pvm_state =
  let open Lwt_syntax in
  let return tick_state =
    Lwt.return
      {pvm_state with current_tick = Z.succ pvm_state.current_tick; tick_state}
  in
  match pvm_state.tick_state with
  | Eval {config; module_reg} ->
      let* config =
        Tezos_webassembly_interpreter.Eval.reveal_step
          Host_funcs.Aux.reveal
          module_reg
          payload
          config
      in
      return (Eval {config; module_reg})
  | Snapshot ->
      return
        (Stuck (Wasm_pvm_errors.invalid_state "No reveal expected during start"))
  | Decode _ ->
      return
        (Stuck
           (Wasm_pvm_errors.invalid_state "No reveal expected during decoding"))
  | Link _ ->
      return
        (Stuck (Wasm_pvm_errors.invalid_state "No reveal expected during link"))
  | Init _ ->
      return
        (Stuck
           (Wasm_pvm_errors.invalid_state
              "No reveal expected during initialization"))
  | Collect ->
      return
        (Stuck
           (Wasm_pvm_errors.invalid_state
              "No reveal expected during collecting"))
  | Stuck _ | Padding -> return pvm_state.tick_state

let compute_step_many_until ?(max_steps = 1L) ?reveal_builtins
    ?(write_debug = Builtins.Noop) should_continue pvm_state =
  let open Lwt.Syntax in
  assert (max_steps > 0L) ;
  let* version = get_wasm_version pvm_state in
  let stack_size_limit = stack_size_limit version in
  let host_function_registry =
    match write_debug with
    | Builtins.Printer _ -> Host_funcs.all_debug ~version ~write_debug
    | Noop -> Host_funcs.all ~version
  in
  let compute_step_with_reveal =
    match reveal_builtins with
    | Some reveal_builtins -> (
        fun pvm_state ->
          let info = input_request pvm_state in
          match info with
          | Reveal_required (Reveal_raw_data req) ->
              let* res = reveal_builtins.Builtins.reveal_preimage req in
              reveal_step (Bytes.of_string res) pvm_state
          | Reveal_required Reveal_metadata ->
              let* res = reveal_builtins.reveal_metadata () in
              reveal_step (Bytes.of_string res) pvm_state
          | _ ->
              compute_step_with_host_functions
                ~version
                ~stack_size_limit
                host_function_registry
                pvm_state)
    | None ->
        compute_step_with_host_functions
          ~version
          ~stack_size_limit
          host_function_registry
  in
  let rec go steps_left pvm_state =
    let* continue = should_continue pvm_state in
    if steps_left > 0L && continue then
      if is_top_level_padding pvm_state then
        (* We're in the top-level padding after the evaluation has
           finished. That means we can skip up to the tick before the
           snapshot in one go. *)
        let bulk_ticks =
          Z.(min (ticks_to_snapshot pvm_state) (of_int64 steps_left))
        in
        let pvm_state =
          {
            pvm_state with
            tick_state = Padding;
            current_tick = Z.add pvm_state.current_tick bulk_ticks;
          }
        in
        go (Int64.sub steps_left (Z.to_int64 bulk_ticks)) pvm_state
      else
        let* pvm_state = compute_step_with_reveal pvm_state in
        go (Int64.pred steps_left) pvm_state
    else Lwt.return pvm_state
  in
  let one_or_more_steps pvm_state =
    (* Make sure we perform at least 1 step. The assertion above ensures that
       we were asked to perform at least 1. *)
    let* pvm_state =
      compute_step_with_host_functions
        ~version
        ~stack_size_limit
        host_function_registry
        pvm_state
    in
    go (Int64.pred max_steps) pvm_state
  in
  measure_executed_ticks one_or_more_steps pvm_state

let should_compute ?reveal_builtins pvm_state =
  let input_request_val = input_request pvm_state in
  match input_request_val with
  | Input_required -> false
  | No_input_required -> true
  | Reveal_required _ -> Option.is_some reveal_builtins

let compute_step_many ?reveal_builtins ?write_debug ?(stop_at_snapshot = false)
    ~max_steps pvm_state =
  compute_step_many_until
    ~max_steps
    ?reveal_builtins
    ?write_debug
    (fun pvm_state ->
      Lwt.return
        (* should_compute && (stop_at_snapshot -> tick_state <> snapshot) *)
        (should_compute ?reveal_builtins pvm_state
        && ((not stop_at_snapshot) || pvm_state.tick_state <> Snapshot)))
    pvm_state

let update_output_buffer pvm_state level =
  let output_buffer = pvm_state.buffers.Wasm.Eval.output in
  if Wasm.Output_buffer.is_initialized output_buffer then
    Wasm.Output_buffer.move_outbox_forward output_buffer
  else Wasm.Output_buffer.initialize_outbox output_buffer level

let set_input_step input_info message pvm_state =
  let open Lwt_syntax in
  let open Wasm_pvm_state in
  let {inbox_level; message_counter} = input_info in
  let raw_level = Bounded.Non_negative_int32.to_value inbox_level in
  let return ?(durable = pvm_state.durable) x = Lwt.return (durable, x) in
  let return_stuck state_name =
    return
      (Stuck
         (Wasm_pvm_errors.invalid_state
         @@ Format.sprintf "No input required during %s" state_name))
  in
  let next_tick_state () =
    match pvm_state.tick_state with
    | Collect -> (
        let* () =
          Wasm.Input_buffer.(
            enqueue
              pvm_state.buffers.input
              {raw_level; message_counter; payload = String.to_bytes message})
        in
        match Pvm_input_kind.from_raw_input message with
        | Internal End_of_level -> return Padding
        | Internal (Protocol_migration proto) ->
            let* durable =
              Durable.set_value_exn
                ~edit_readonly:true
                pvm_state.durable
                Constants.version_key
                (Data_encoding.Binary.to_string_exn
                   Wasm_pvm_state.version_encoding
                   (version_for_protocol proto))
            in
            return ~durable Collect
        | Internal Start_of_level ->
            update_output_buffer pvm_state raw_level ;
            return Collect
        | _ -> return Collect)
    | Stuck _ -> return pvm_state.tick_state
    | Snapshot -> return_stuck "start"
    | Decode _ -> return_stuck "decoding"
    | Link _ -> return_stuck "link"
    | Init _ -> return_stuck "initialization"
    | Eval _ -> return_stuck "evaluation"
    | Padding -> return_stuck "padding"
  in
  let+ durable, tick_state =
    Lwt.catch next_tick_state (fun exn ->
        let+ tick_state = exn_to_stuck pvm_state exn in
        (pvm_state.durable, tick_state))
  in
  (* Increase the current tick counter and update last input *)
  {
    pvm_state with
    durable;
    tick_state;
    current_tick = Z.succ pvm_state.current_tick;
    last_input_info = Some input_info;
  }

let get_output output_info output =
  let open Lwt_syntax in
  let open Wasm_pvm_state in
  let {outbox_level; message_index} = output_info in
  let outbox_level = Bounded.Non_negative_int32.to_value outbox_level in
  let+ payload =
    Wasm.Output_buffer.get_message
      output
      Wasm.Output_buffer.{outbox_level; message_index}
  in
  Bytes.to_string payload

let get_info ({current_tick; last_input_info; _} as pvm_state) =
  let open Lwt_syntax in
  let input_request = input_request pvm_state in
  return
  @@ Wasm_pvm_state.
       {current_tick; last_input_read = last_input_info; input_request}

module Internal_for_tests = struct
  let compute_step_many_with_hooks ?reveal_builtins ?write_debug
      ?after_fast_exec:_ =
    compute_step_many ?reveal_builtins ?write_debug
end
