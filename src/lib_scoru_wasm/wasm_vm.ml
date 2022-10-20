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

let link_finished (ast : Wasm.Ast.module_) offset =
  offset >= Wasm.Ast.Vector.num_elements ast.it.imports

let eval_has_finished = function
  | Eval {step_kont = Wasm.Eval.(SK_Result _); _} -> true
  | _ -> false

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

let mark_for_reboot reboot_counter durable =
  let open Lwt_syntax in
  if Z.Compare.(reboot_counter <= Z.zero) then return Failing
  else
    let+ has_reboot_flag = has_reboot_flag durable in
    if has_reboot_flag then Reboot else Restarting

let initial_boot_state () =
  Decode
    (Tezos_webassembly_interpreter.Decode.initial_decode_kont
       ~name:Constants.wasm_main_module_name)

let unsafe_next_tick_state ({buffers; durable; tick_state; _} as pvm_state) =
  let open Lwt_syntax in
  let return ?(status = Running) ?(durable = durable) state =
    Lwt.return (durable, state, status)
  in
  match tick_state with
  | Stuck e -> return ~status:Failing (Stuck e)
  | Snapshot ->
      let* has_reboot_flag = has_reboot_flag durable in
      if has_reboot_flag then
        let* durable = Durable.(delete durable Constants.reboot_flag_key) in
        return ~durable (initial_boot_state ())
      else
        return
          ~status:Failing
          (Stuck
             (Wasm_pvm_errors.invalid_state "snapshot is an input tick state"))
  | _ when eval_has_finished tick_state && is_time_for_snapshot pvm_state ->
      (* We have an empty set of admin instructions, we can either reboot
         without consuming another input if the flag has been set or read a
         new input. *)
      let* status = mark_for_reboot pvm_state.reboot_counter durable in
      (* Execution took too many reboot *)
      if status = Failing then return ~status (Stuck Too_many_reboots)
      else return ~status Snapshot
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
      let* m = Tezos_webassembly_interpreter.Decode.module_step kernel m in
      return (Decode m)
  | Link {ast_module; externs; imports_offset}
    when link_finished ast_module imports_offset ->
      let self = Wasm.Instance.Module_key Constants.wasm_main_module_name in
      let module_reg = Wasm.Instance.ModuleMap.create () in
      (* The module instance will be registered as [self] in
         [module_reg] during the initialization. *)
      return (Init {self; ast_module; init_kont = IK_Start externs; module_reg})
  | Link {ast_module; externs; imports_offset} -> (
      let* {it = {module_name; item_name; _}; _} =
        Wasm.Ast.Vector.get imports_offset ast_module.it.imports
      in
      match (module_name, Host_funcs.lookup_opt item_name) with
      | "rollup_safe_core", Some extern ->
          let externs, _ = Wasm.Ast.Vector.append extern externs in
          return
            (Link
               {ast_module; externs; imports_offset = Int32.succ imports_offset})
      | "rollup_safe_core", None ->
          return
            ~status:Failing
            (Stuck (Wasm_pvm_errors.link_error `Item ~module_name ~item_name))
      | _, _ ->
          return
            ~status:Failing
            (Stuck (Wasm_pvm_errors.link_error `Module ~module_name ~item_name))
      )
  | Init {self; ast_module = _; init_kont = IK_Stop; module_reg} -> (
      let* module_inst =
        Wasm.Instance.ModuleMap.get Constants.wasm_main_module_name module_reg
      in
      let* extern =
        Wasm.Instance.NameMap.get
          Constants.wasm_entrypoint
          module_inst.Wasm.Instance.exports
      in
      match extern with
      | Wasm.Instance.ExternFunc main_func ->
          let admin_instr' = Wasm.Eval.Invoke main_func in
          let admin_instr = Wasm.Source.{it = admin_instr'; at = no_region} in
          (* Clear the values and the locals in the frame. *)
          let eval_config =
            Wasm.Eval.config
              Host_funcs.all
              self
              module_reg
              (Tezos_lazy_containers.Lazy_vector.Int32Vector.empty ())
              (Tezos_lazy_containers.Lazy_vector.Int32Vector.singleton
                 admin_instr)
          in
          return ~status:Starting (Eval eval_config)
      | _ ->
          (* We require a function with the name [main] to be exported
             rather than any other structure. *)
          return
            ~status:Failing
            (Stuck
               (Wasm_pvm_errors.invalid_state
                  "Invalid_module: no `main` function exported")))
  | Init {self; ast_module; init_kont; module_reg} ->
      let* init_kont =
        Wasm.Eval.init_step
          ~filter_exports:true
          ~check_module_exports:Exports_memory_0
          ~module_reg
          ~self
          buffers
          Host_funcs.all
          ast_module
          init_kont
      in
      return (Init {self; ast_module; init_kont; module_reg})
  | _ when eval_has_finished tick_state ->
      (* We have an empty set of admin instructions, but need to wait until we can restart *)
      return tick_state
  | Eval {step_kont = Wasm.Eval.(SK_Trapped msg); _} ->
      return
        ~status:Failing
        (Stuck
           (Wasm_pvm_errors.Eval_error
              {
                raw_exception =
                  Wasm_pvm_errors.truncate_message "trapped execution";
                explanation = Some (Wasm_pvm_errors.truncate_message msg.it);
              }))
  | Eval eval_config ->
      (* Continue execution. *)
      let store = Durable.to_storage durable in
      let* store', eval_config =
        Wasm.Eval.step ~durable:store eval_config buffers
      in
      let durable' = Durable.of_storage ~default:durable store' in
      return ~durable:durable' (Eval eval_config)

let next_tick_state pvm_state =
  let to_stuck exn =
    let error = Wasm_pvm_errors.extract_interpreter_error exn in
    let wasm_error =
      match error with
      | `Interpreter error -> (
          match pvm_state.tick_state with
          | Decode _ -> Wasm_pvm_errors.Decode_error error
          | Link _ -> Link_error error.Wasm_pvm_errors.raw_exception
          | Init _ -> Init_error error
          | Eval _ -> Eval_error error
          | Stuck _ | Snapshot -> Unknown_error error.raw_exception)
      | `Unknown raw_exception -> Unknown_error raw_exception
    in
    Lwt.return (pvm_state.durable, Stuck wasm_error, Failing)
  in
  Lwt.catch (fun () -> unsafe_next_tick_state pvm_state) to_stuck

let next_last_top_level_call {current_tick; last_top_level_call; _} = function
  | Restarting | Reboot -> Z.succ current_tick
  | Starting | Failing | Running -> last_top_level_call

let next_reboot_counter {reboot_counter; maximum_reboots_per_input; _} status =
  match status with
  | Reboot -> Z.pred reboot_counter
  | Restarting | Failing -> maximum_reboots_per_input
  | Starting | Running -> reboot_counter

(** [compute_step pvm_state] does one computation step on [pvm_state].
    Returns the new state.
*)
let compute_step pvm_state =
  let open Lwt_syntax in
  (* Calculate the next tick state. *)
  let* durable, tick_state, status = next_tick_state pvm_state in
  let current_tick = Z.succ pvm_state.current_tick in
  let last_top_level_call = next_last_top_level_call pvm_state status in
  let reboot_counter = next_reboot_counter pvm_state status in
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

let input_request pvm_state =
  let open Lwt_syntax in
  match pvm_state.tick_state with
  | Stuck _ -> return Wasm_pvm_state.Input_required
  | Snapshot ->
      let+ has_reboot_flag = has_reboot_flag pvm_state.durable in
      if has_reboot_flag then Wasm_pvm_state.No_input_required
      else Wasm_pvm_state.Input_required
  | Eval config -> (
      match Tezos_webassembly_interpreter.Eval.is_reveal_tick config with
      | Some reveal -> return (Wasm_pvm_state.Reveal_required reveal)
      | None -> return Wasm_pvm_state.No_input_required)
  | _ -> return Wasm_pvm_state.No_input_required

let is_top_level_padding pvm_state =
  eval_has_finished pvm_state.tick_state && not (is_time_for_snapshot pvm_state)

let measure_executed_ticks (transition : pvm_state -> pvm_state Lwt.t)
    (initial_state : pvm_state) : (pvm_state * int64) Lwt.t =
  let open Lwt.Syntax in
  let open Z in
  let+ final_state = transition initial_state in
  let ticks_executed = final_state.current_tick - initial_state.current_tick in
  (final_state, to_int64 ticks_executed)

let compute_step_many_until ?(max_steps = 1L) should_continue =
  let open Lwt.Syntax in
  assert (max_steps > 0L) ;
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
            current_tick = Z.add pvm_state.current_tick bulk_ticks;
          }
        in
        go (Int64.sub steps_left (Z.to_int64 bulk_ticks)) pvm_state
      else
        let* pvm_state = compute_step pvm_state in
        go (Int64.pred steps_left) pvm_state
    else Lwt.return pvm_state
  in
  let one_or_more_steps pvm_state =
    (* Make sure we perform at least 1 step. The assertion above ensures that
       we were asked to perform at least 1. *)
    let* pvm_state = compute_step pvm_state in
    go (Int64.pred max_steps) pvm_state
  in
  measure_executed_ticks one_or_more_steps

let should_compute pvm_state =
  let open Lwt.Syntax in
  let+ input_request_val = input_request pvm_state in
  match input_request_val with
  | Reveal_required _ | Input_required -> false
  | No_input_required -> true

let compute_step_many ~max_steps pvm_state =
  compute_step_many_until ~max_steps should_compute pvm_state

let set_input_step input_info message pvm_state =
  let open Lwt_syntax in
  let open Wasm_pvm_state in
  let {inbox_level; message_counter} = input_info in
  let raw_level = Bounded.Non_negative_int32.to_value inbox_level in
  let+ tick_state =
    match pvm_state.tick_state with
    | Snapshot ->
        let+ () =
          Wasm.Input_buffer.(
            enqueue
              pvm_state.buffers.input
              {raw_level; message_counter; payload = String.to_bytes message})
        in
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/3157
           The goal is to read a complete inbox. *)
        (* Go back to decoding *)
        initial_boot_state ()
    | Decode _ ->
        Lwt.return
          (Stuck
             (Wasm_pvm_errors.invalid_state "No input required during decoding"))
    | Link _ ->
        Lwt.return
          (Stuck (Wasm_pvm_errors.invalid_state "No input required during link"))
    | Init _ ->
        Lwt.return
          (Stuck
             (Wasm_pvm_errors.invalid_state
                "No input required during initialization"))
    | Eval _ ->
        Lwt.return
          (Stuck
             (Wasm_pvm_errors.invalid_state
                "No input required during evaluation"))
    | Stuck _ -> Lwt.return pvm_state.tick_state
  in
  (* Increase the current tick counter and mark that no input is required. *)
  {pvm_state with tick_state; current_tick = Z.succ pvm_state.current_tick}

let reveal_step payload pvm_state =
  let open Lwt_syntax in
  let open Tezos_webassembly_interpreter in
  let return tick_state =
    Lwt.return
      {pvm_state with current_tick = Z.succ pvm_state.current_tick; tick_state}
  in
  match pvm_state.tick_state with
  | Eval config ->
      let* config = Eval.reveal_step config.module_reg payload config in
      return (Eval config)
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
  | Snapshot ->
      return
        (Stuck
           (Wasm_pvm_errors.invalid_state
              "No reveal expected during snapshotting"))
  | Stuck _ -> return pvm_state.tick_state

let get_output output_info output =
  let open Lwt_syntax in
  let open Wasm_pvm_state in
  let {outbox_level; message_index} = output_info in
  let outbox_level = Bounded.Non_negative_int32.to_value outbox_level in
  let+ payload = Wasm.Output_buffer.get output outbox_level message_index in
  Bytes.to_string payload

let get_info ({current_tick; last_input_info; _} as pvm_state) =
  let open Lwt_syntax in
  let+ input_request = input_request pvm_state in
  Wasm_pvm_state.
    {current_tick; last_input_read = last_input_info; input_request}
