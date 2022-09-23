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

(* The name by which the module is registered. This can be anything as long
   as we use the same name to lookup from the registry. *)
let wasm_main_module_name = "main"

(* This is the name of the main function of the module. We require the
   kernel to expose a function named [kernel_next]. *)
let wasm_entrypoint = "kernel_next"

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3590
   An appropriate number should be used,
   currently 100 times the nb of ticks it takes tx_kernel to init, deposit, then withdraw
   (so 100x 2 billion ticks) *)
let wasm_max_tick = Z.of_int 200_000_000_000

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3157
   Find an appropriate number of reboots per inputs.
*)
let maximum_reboots_per_input = Z.of_int 10

(* Flag used in the durable storage by the kernel to ask a reboot from the PVM
   without consuming an input. *)
let reboot_flag_key = Durable.key_of_string_exn "/kernel/env/reboot"

module Wasm = Tezos_webassembly_interpreter

type tick_state =
  | Decode of Tezos_webassembly_interpreter.Decode.decode_kont
  | Link of {
      ast_module : Wasm.Ast.module_;
      externs : Wasm.Instance.extern Wasm.Instance.Vector.t;
      imports_offset : int32;
    }
  | Init of {
      self : Wasm.Instance.module_key;
      ast_module : Tezos_webassembly_interpreter.Ast.module_;
      init_kont : Tezos_webassembly_interpreter.Eval.init_kont;
      module_reg : Wasm.Instance.module_reg;
    }
  | Eval of Wasm.Eval.config
  | Stuck of Wasm_pvm_errors.t
  | Snapshot

type computation_status = Starting | Restarting | Running | Failing | Reboot

type pvm_state = {
  last_input_info : Wasm_pvm_sig.input_info option;
      (** Info about last read input. *)
  current_tick : Z.t;  (** Current tick of the PVM. *)
  reboot_counter : Z.t;  (** Number of reboots for the current input. *)
  durable : Durable.t;  (** The durable storage of the PVM. *)
  buffers : Wasm.Eval.buffers;
      (** Input and outut buffers used by the PVM host functions. *)
  tick_state : tick_state;  (** The current tick state. *)
  last_top_level_call : Z.t;
      (** Last tick corresponding to a top-level call. *)
  max_nb_ticks : Z.t;  (** Number of ticks between top level call. *)
  maximum_reboots_per_input : Z.t;  (** Number of reboots between two inputs. *)
}

module Make (T : Tezos_tree_encoding.TREE) :
  Gather_floppies.S
    with type tree = T.tree
     and type tick_state = tick_state
     and type pvm_state = pvm_state = struct
  module Raw = struct
    type tree = T.tree

    type nonrec tick_state = tick_state

    type nonrec pvm_state = pvm_state

    module Tree_encoding_runner = Tezos_tree_encoding.Runner.Make (T)
    module Parsing = Binary_parser_encodings

    let host_funcs =
      let registry = Wasm.Host_funcs.empty () in
      Host_funcs.register_host_funcs registry ;
      registry

    let tick_state_encoding =
      let open Tezos_tree_encoding in
      tagged_union
        ~default:(fun () -> Snapshot)
        (value [] Data_encoding.string)
        [
          case
            "decode"
            Parsing.Decode.encoding
            (function Decode m -> Some m | _ -> None)
            (fun m -> Decode m);
          case
            "link"
            (tup3
               ~flatten:true
               (scope ["ast_module"]
               @@ Parsing.(no_region_encoding Module.module_encoding))
               (scope
                  ["externs"]
                  (int32_lazy_vector
                     (value [] Data_encoding.int32)
                     Wasm_encoding.extern_encoding))
               (value ["imports_offset"] Data_encoding.int32))
            (function
              | Link {ast_module; externs; imports_offset} ->
                  Some (ast_module, externs, imports_offset)
              | _ -> None)
            (fun (ast_module, externs, imports_offset) ->
              Link {ast_module; externs; imports_offset});
          case
            "init"
            (tup4
               ~flatten:true
               (scope ["self"] Wasm_encoding.module_key_encoding)
               (scope ["ast_module"]
               @@ Parsing.(no_region_encoding Module.module_encoding))
               (scope
                  ["init_kont"]
                  (Init_encodings.init_kont_encoding ~host_funcs))
               (scope ["modules"] Wasm_encoding.module_instances_encoding))
            (function
              | Init {self; ast_module; init_kont; module_reg} ->
                  Some (self, ast_module, init_kont, module_reg)
              | _ -> None)
            (fun (self, ast_module, init_kont, module_reg) ->
              Init {self; ast_module; init_kont; module_reg});
          case
            "eval"
            (Wasm_encoding.config_encoding ~host_funcs)
            (function Eval eval_config -> Some eval_config | _ -> None)
            (fun eval_config -> Eval eval_config);
          case
            "stuck"
            (value [] Wasm_pvm_errors.encoding)
            (function Stuck err -> Some err | _ -> None)
            (fun err -> Stuck err);
          case
            "snapshot"
            (value [] Data_encoding.unit)
            (function Snapshot -> Some () | _ -> None)
            (fun () -> Snapshot);
        ]

    let durable_buffers_encoding =
      Tezos_tree_encoding.(
        scope ["pvm"; "buffers"] Wasm_encoding.buffers_encoding)

    let pvm_state_encoding =
      let open Tezos_tree_encoding in
      conv
        (fun ( last_input_info,
               current_tick,
               reboot_counter,
               durable,
               buffers,
               tick_state,
               last_top_level_call,
               max_nb_ticks,
               maximum_reboots_per_input ) ->
          {
            last_input_info;
            current_tick;
            reboot_counter =
              Option.value ~default:maximum_reboots_per_input reboot_counter;
            durable;
            buffers =
              (*`Gather_floppies` uses `get_info`, that decodes the state of the
                PVM, which at the start of the rollup doesn't exist. *)
              Option.value_f
                ~default:Tezos_webassembly_interpreter.Eval.buffers
                buffers;
            tick_state;
            last_top_level_call;
            max_nb_ticks;
            maximum_reboots_per_input;
          })
        (fun {
               last_input_info;
               current_tick;
               reboot_counter;
               durable;
               buffers;
               tick_state;
               last_top_level_call;
               max_nb_ticks;
               maximum_reboots_per_input;
             } ->
          ( last_input_info,
            current_tick,
            Some reboot_counter,
            durable,
            Some buffers,
            tick_state,
            last_top_level_call,
            max_nb_ticks,
            maximum_reboots_per_input ))
        (tup9
           ~flatten:true
           (value_option ["wasm"; "input"] Wasm_pvm_sig.input_info_encoding)
           (value ~default:Z.zero ["wasm"; "current_tick"] Data_encoding.n)
           (value_option ["wasm"; "reboot_counter"] Data_encoding.n)
           (scope ["durable"] Durable.encoding)
           (option durable_buffers_encoding)
           (scope ["wasm"] tick_state_encoding)
           (value
              ~default:Z.zero
              ["pvm"; "last_top_level_call"]
              Data_encoding.n)
           (value
              ~default:wasm_max_tick
              ["pvm"; "max_nb_ticks"]
              Data_encoding.n)
           (value
              ~default:maximum_reboots_per_input
              ["pvm"; "maximum_reboots_per_input"]
              Data_encoding.n))

    let kernel_key = Durable.key_of_string_exn "/kernel/boot.wasm"

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
          (fun () -> Durable.(find_value durable reboot_flag_key))
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
           ~name:wasm_main_module_name)

    let unsafe_next_tick_state ({buffers; durable; tick_state; _} as pvm_state)
        =
      let open Lwt_syntax in
      let return ?(status = Running) ?(durable = durable) state =
        Lwt.return (durable, state, status)
      in
      match tick_state with
      | Stuck e -> return ~status:Failing (Stuck e)
      | Snapshot ->
          let* has_reboot_flag = has_reboot_flag durable in
          if has_reboot_flag then
            let* durable = Durable.(delete durable reboot_flag_key) in
            return ~durable (initial_boot_state ())
          else
            return
              ~status:Failing
              (Stuck
                 (Wasm_pvm_errors.invalid_state
                    "snapshot is an input tick state"))
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
          let* kernel = Durable.find_value_exn durable kernel_key in
          let* m = Tezos_webassembly_interpreter.Decode.module_step kernel m in
          return (Decode m)
      | Link {ast_module; externs; imports_offset}
        when link_finished ast_module imports_offset ->
          let self = Wasm.Instance.Module_key wasm_main_module_name in
          let module_reg = Wasm.Instance.ModuleMap.create () in
          (* The module instance will be registered as [self] in
             [module_reg] during the initialization. *)
          return
            (Init {self; ast_module; init_kont = IK_Start externs; module_reg})
      | Link {ast_module; externs; imports_offset} -> (
          let* {it = {module_name; item_name; _}; _} =
            Wasm.Ast.Vector.get imports_offset ast_module.it.imports
          in
          match (module_name, Host_funcs.lookup_opt item_name) with
          | "rollup_safe_core", Some extern ->
              let externs, _ = Wasm.Ast.Vector.append extern externs in
              return
                (Link
                   {
                     ast_module;
                     externs;
                     imports_offset = Int32.succ imports_offset;
                   })
          | "rollup_safe_core", None ->
              return
                ~status:Failing
                (Stuck
                   (Wasm_pvm_errors.link_error `Item ~module_name ~item_name))
          | _, _ ->
              return
                ~status:Failing
                (Stuck
                   (Wasm_pvm_errors.link_error `Module ~module_name ~item_name))
          )
      | Init {self; ast_module = _; init_kont = IK_Stop; module_reg} -> (
          let* module_inst =
            Wasm.Instance.ModuleMap.get wasm_main_module_name module_reg
          in
          let* extern =
            Wasm.Instance.NameMap.get
              wasm_entrypoint
              module_inst.Wasm.Instance.exports
          in
          match extern with
          | Wasm.Instance.ExternFunc main_func ->
              let admin_instr' = Wasm.Eval.Invoke main_func in
              let admin_instr =
                Wasm.Source.{it = admin_instr'; at = no_region}
              in
              (* Clear the values and the locals in the frame. *)
              let eval_config =
                Wasm.Eval.config
                  host_funcs
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
              host_funcs
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

    let next_last_top_level_call {current_tick; last_top_level_call; _} =
      function
      | Restarting | Reboot -> Z.succ current_tick
      | Starting | Failing | Running -> last_top_level_call

    let next_reboot_counter {reboot_counter; maximum_reboots_per_input; _}
        status =
      match status with
      | Reboot -> Z.pred reboot_counter
      | Restarting | Failing -> maximum_reboots_per_input
      | Starting | Running -> reboot_counter

    let compute_step_inner pvm_state =
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
      | Stuck _ -> return Wasm_pvm_sig.Input_required
      | Snapshot ->
          let+ has_reboot_flag = has_reboot_flag pvm_state.durable in
          if has_reboot_flag then Wasm_pvm_sig.No_input_required
          else Wasm_pvm_sig.Input_required
      | Eval config -> (
          match Tezos_webassembly_interpreter.Eval.is_reveal_tick config with
          | Some reveal -> return (Wasm_pvm_sig.Reveal_required reveal)
          | None -> return Wasm_pvm_sig.No_input_required)
      | _ -> return Wasm_pvm_sig.No_input_required

    let is_top_level_padding pvm_state =
      eval_has_finished pvm_state.tick_state
      && not (is_time_for_snapshot pvm_state)

    let decode tree = Tree_encoding_runner.decode pvm_state_encoding tree

    let encode pvm_state tree =
      let open Lwt.Syntax in
      (* {{Note tick state clean-up}}

         The "wasm" directory in the Irmin tree of the PVM is used to
         maintain a lot of information across tick, but as of now, it
         was never cleaned up. It meant that the tree would become
         crowded with data that were no longer needed.

         It turns out it is very simple to clean up, thanks to subtree
         move.  Because we keep in the lazy-containers the original
         subtree, and we inject it prior to updating read keys, the
         tree-encoding library does not rely on the input tree at
         encoding time.

         With this, we gain an additional 5% of proof size in the
         worst tick of the computation.wasm kernel. *)
      let* tree = T.remove tree ["wasm"] in
      Tree_encoding_runner.encode pvm_state_encoding pvm_state tree

    let compute_step_many_until_pvm_state ?(max_steps = 1L) should_continue
        pvm_state =
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
            let* pvm_state = compute_step_inner pvm_state in
            go (Int64.pred steps_left) pvm_state
        else Lwt.return pvm_state
      in
      (* Make sure we perform at least 1 step. The assertion above ensures that
         we were asked to perform at least 1. *)
      let* pvm_state = compute_step_inner pvm_state in
      go (Int64.pred max_steps) pvm_state

    let compute_step_many_until ?(max_steps = 1L) should_continue tree =
      let open Lwt.Syntax in
      assert (max_steps > 0L) ;

      let* pvm_state = decode tree in

      (* Make sure we perform at least 1 step. The assertion above ensures that
         we were asked to perform at least 1. *)
      let* pvm_state =
        compute_step_many_until_pvm_state ~max_steps should_continue pvm_state
      in

      encode pvm_state tree

    let compute_step_many ~max_steps tree =
      let open Lwt_syntax in
      let should_continue pvm_state =
        let* input_request_val = input_request pvm_state in
        match input_request_val with
        | Reveal_required _ | Input_required -> return false
        | No_input_required -> return true
      in
      compute_step_many_until ~max_steps should_continue tree

    let compute_step tree = compute_step_many ~max_steps:1L tree

    let get_output output_info tree =
      let open Lwt_syntax in
      let open Wasm_pvm_sig in
      let {outbox_level; message_index} = output_info in
      let outbox_level = Bounded.Non_negative_int32.to_value outbox_level in
      let* candidate =
        Tree_encoding_runner.decode
          (Tezos_tree_encoding.option durable_buffers_encoding)
          tree
      in
      try
        match candidate with
        | Some {output; _} ->
            let+ payload =
              Wasm.Output_buffer.get output outbox_level message_index
            in
            Some (Bytes.to_string payload)
        | None -> Lwt.return None
      with _ -> Lwt.return None

    let get_info tree =
      let open Lwt_syntax in
      let* ({current_tick; last_input_info; _} as pvm) =
        Tree_encoding_runner.decode pvm_state_encoding tree
      in
      let+ input_request = input_request pvm in
      Wasm_pvm_sig.
        {current_tick; last_input_read = last_input_info; input_request}

    let set_input_step input_info message tree =
      let open Lwt_syntax in
      let open Wasm_pvm_sig in
      let {inbox_level; message_counter} = input_info in
      let raw_level = Bounded.Non_negative_int32.to_value inbox_level in
      let level = Int32.to_string raw_level in
      let id = Z.to_string message_counter in
      let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      let* tick_state =
        match pvm_state.tick_state with
        | Snapshot ->
            let+ () =
              Wasm.Input_buffer.(
                enqueue
                  pvm_state.buffers.input
                  {
                    (* This is to distinguish (0) Inbox inputs from (1)
                       DAL/Slot_header inputs. *)
                    rtype = 0l;
                    raw_level;
                    message_counter;
                    payload = String.to_bytes message;
                  })
            in
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/3157
               The goal is to read a complete inbox. *)
            (* Go back to decoding *)
            initial_boot_state ()
        | Decode _ ->
            Lwt.return
              (Stuck
                 (Wasm_pvm_errors.invalid_state
                    "No input required during decoding"))
        | Link _ ->
            Lwt.return
              (Stuck
                 (Wasm_pvm_errors.invalid_state "No input required during link"))
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
      (* See {{Note tick state clean-up}} *)
      let* tree = T.remove tree ["wasm"] in
      (* Encode the input in the tree under [input/level/id]. *)
      let* tree =
        Tree_encoding_runner.encode
          (Tezos_tree_encoding.value ["input"; level; id] Data_encoding.string)
          message
          tree
      in
      (* Increase the current tick counter and mark that no input is required. *)
      let pvm_state =
        {
          pvm_state with
          tick_state;
          current_tick = Z.succ pvm_state.current_tick;
        }
      in
      (* Encode the new pvm-state in the tree. *)
      Tree_encoding_runner.encode pvm_state_encoding pvm_state tree

    let reveal_step payload tree =
      let open Lwt_syntax in
      let open Tezos_webassembly_interpreter in
      let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      let* pvm_state =
        let return tick_state =
          Lwt.return
            {
              pvm_state with
              current_tick = Z.succ pvm_state.current_tick;
              tick_state;
            }
        in
        match pvm_state.tick_state with
        | Eval config ->
            let* config = Eval.reveal_step config.module_reg payload config in
            return (Eval config)
        | Decode _ ->
            return
              (Stuck
                 (Wasm_pvm_errors.invalid_state
                    "No reveal expected during decoding"))
        | Link _ ->
            return
              (Stuck
                 (Wasm_pvm_errors.invalid_state
                    "No reveal expected during link"))
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
      in
      Tree_encoding_runner.encode pvm_state_encoding pvm_state tree

    module Internal_for_tests = struct
      let get_tick_state tree =
        let open Lwt_syntax in
        let+ pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
        pvm_state.tick_state

      let get_module_instance_exn tree =
        let open Lwt_syntax in
        let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
        match pvm_state.tick_state with
        | Eval config ->
            Wasm.Instance.ModuleMap.get wasm_main_module_name config.module_reg
        | _ -> raise (Invalid_argument "get_module_instance")

      let is_stuck tree =
        let open Lwt.Syntax in
        let* pvm = Tree_encoding_runner.decode pvm_state_encoding tree in
        match pvm.tick_state with
        | Stuck error -> Lwt.return_some error
        | _ -> Lwt.return_none

      let decode = decode

      let encode = encode

      let compute_step_many_until_pvm_state = compute_step_many_until_pvm_state

      let compute_step_many_until = compute_step_many_until

      let set_max_nb_ticks n tree =
        let open Lwt_syntax in
        let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
        let pvm_state = {pvm_state with max_nb_ticks = n} in
        Tree_encoding_runner.encode pvm_state_encoding pvm_state tree

      let set_maximum_reboots_per_input n tree =
        let open Lwt_syntax in
        let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
        let pvm_state = {pvm_state with maximum_reboots_per_input = n} in
        Tree_encoding_runner.encode pvm_state_encoding pvm_state tree

      let reset_reboot_counter tree =
        let open Lwt_syntax in
        let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
        let pvm_state =
          {pvm_state with reboot_counter = pvm_state.maximum_reboots_per_input}
        in
        Tree_encoding_runner.encode pvm_state_encoding pvm_state tree
    end
  end

  include Gather_floppies.Make (T) (Raw)
end
