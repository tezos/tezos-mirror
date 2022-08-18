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

module Wasm = Tezos_webassembly_interpreter

type tick_state =
  | Decode of Tezos_webassembly_interpreter.Decode.decode_kont
  | Eval of Wasm.Eval.config

type pvm_state = {
  last_input_info : Wasm_pvm_sig.input_info option;
      (** Info about last read input. *)
  current_tick : Z.t;  (** Current tick of the PVM. *)
  kernel : Lazy_containers.Chunked_byte_vector.Lwt.t;  (** The loaded kernel. *)
  module_reg : Wasm.Instance.module_reg;
      (** Module registry of the loaded kernel. *)
  tick_state : tick_state;  (** The current tick state. *)
  input_request : Wasm_pvm_sig.input_request;
      (** Signals whether or not the PVM needs input. *)
}

module Make (T : Tree_encoding.TREE) :
  Gather_floppies.S with type tree = T.tree = struct
  module Raw = struct
    type tree = T.tree

    module Tree_encoding = Tree_encoding.Make (T)

    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3568
       The [Wasm_encoding] functor is already used in
       [Binary_parser_encodings].
       Ideally, we would make [Binary_parser_encodings.Make] reexpose
       the [Wasm_encoding] module it computes. However, since we have
       a short-term solution to remove the functor layer of
       [Tree_encoding], we leave the code as-is. *)
    module Wasm_encoding = Wasm_encoding.Make (Tree_encoding)
    module Parsing = Binary_parser_encodings.Make (Tree_encoding)

    let host_funcs =
      let registry = Wasm.Host_funcs.empty () in
      Host_funcs.register_host_funcs registry ;
      registry

    let tick_state_encoding =
      let open Tree_encoding in
      tagged_union
        ~default:
          (Decode
             (Tezos_webassembly_interpreter.Decode.initial_decode_kont
                ~name:wasm_main_module_name))
        (value [] Data_encoding.string)
        [
          case
            "decode"
            Parsing.Decode.encoding
            (function Decode m -> Some m | _ -> None)
            (fun m -> Decode m);
          case
            "eval"
            (Wasm_encoding.config_encoding ~host_funcs)
            (function Eval eval_config -> Some eval_config | _ -> None)
            (fun eval_config -> Eval eval_config);
        ]

    let input_request_encoding =
      Tree_encoding.conv
        (function
          | true -> Wasm_pvm_sig.Input_required
          | false -> Wasm_pvm_sig.No_input_required)
        (function
          | Wasm_pvm_sig.Input_required -> true
          | Wasm_pvm_sig.No_input_required -> false)
        (Tree_encoding.value ~default:false [] Data_encoding.bool)

    let pvm_state_encoding =
      let open Tree_encoding in
      conv
        (fun ( last_input_info,
               current_tick,
               kernel,
               module_reg,
               tick_state,
               input_request ) ->
          {
            last_input_info;
            current_tick;
            kernel;
            module_reg;
            tick_state;
            input_request;
          })
        (fun {
               last_input_info;
               current_tick;
               kernel;
               module_reg;
               tick_state;
               input_request;
             } ->
          ( last_input_info,
            current_tick,
            kernel,
            module_reg,
            tick_state,
            input_request ))
        (tup6
           ~flatten:true
           (value_option ["wasm"; "input"] Wasm_pvm_sig.input_info_encoding)
           (value ~default:Z.zero ["wasm"; "current_tick"] Data_encoding.n)
           (scope ["durable"; "kernel"; "boot.wasm"] chunked_byte_vector)
           (scope ["modules"] Wasm_encoding.module_instances_encoding)
           (scope ["wasm"] tick_state_encoding)
           (scope ["input"; "consuming"] input_request_encoding))

    let next_tick_state {module_reg; kernel; tick_state; _} =
      let open Lwt_syntax in
      match tick_state with
      | Decode {module_kont = MKStop ast_module; _} ->
          let self = Wasm.Instance.Module_key wasm_main_module_name in
          (* The module instance is registered in [self] that contains the
             module registry, why we can ignore the result here. *)
          let* _module_inst =
            Wasm.Eval.init ~module_reg ~self host_funcs ast_module []
          in
          let eval_config = Wasm.Eval.config host_funcs self [] [] in
          Lwt.return (Eval eval_config)
      | Decode m ->
          let+ m = Tezos_webassembly_interpreter.Decode.module_step kernel m in
          Decode m
      | Eval ({Wasm.Eval.frame; code; _} as eval_config) -> (
          match code with
          | _values, [] ->
              (* We have an empty set of admin instructions so we create one
                 that invokes the main function. *)
              let* module_inst =
                Wasm.Instance.ModuleMap.get wasm_main_module_name module_reg
              in
              let* main_name =
                Wasm.Instance.Vector.to_list @@ Wasm.Utf8.decode wasm_entrypoint
              in
              let* extern =
                Wasm.Instance.NameMap.get
                  main_name
                  module_inst.Wasm.Instance.exports
              in
              let main_func =
                match extern with
                | Wasm.Instance.ExternFunc func -> func
                | _ ->
                    (* We require a function with the name [main] to be exported
                       rather than any other structure. *)
                    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3448
                       Avoid throwing exceptions.
                       Possibly use a a new state to indicate, such as
                       [Invalid_module].
                    *)
                    assert false
              in
              let admin_instr' = Wasm.Eval.Invoke main_func in
              let admin_instr =
                Wasm.Source.{it = admin_instr'; at = no_region}
              in
              (* Clear the values and the locals in the frame. *)
              let code = ([], [admin_instr]) in
              let eval_config =
                {
                  eval_config with
                  Wasm.Eval.frame = {frame with locals = []};
                  code;
                }
              in
              Lwt.return (Eval eval_config)
          | _ ->
              (* Continue execution. *)
              let* eval_config = Wasm.Eval.step module_reg eval_config in
              Lwt.return (Eval eval_config))

    let compute_step tree =
      let open Lwt_syntax in
      let* pvm_state = Tree_encoding.decode pvm_state_encoding tree in
      (* Calculate the next tick state. *)
      let* tick_state = next_tick_state pvm_state in
      let input_request =
        match pvm_state.tick_state with
        | Eval {code = _, []; _} ->
            (* Ask for more input if the kernel has yielded (empty admin
               instructions). *)
            Wasm_pvm_sig.Input_required
        | _ -> Wasm_pvm_sig.No_input_required
      in
      (* Update the tick state and input-request and increment the current tick *)
      let pvm_state =
        {
          pvm_state with
          tick_state;
          input_request;
          current_tick = Z.succ pvm_state.current_tick;
        }
      in
      Tree_encoding.encode pvm_state_encoding pvm_state tree

    let get_output _ _ = Lwt.return ""

    let get_info tree =
      let open Lwt_syntax in
      let* {current_tick; last_input_info; input_request; _} =
        Tree_encoding.decode pvm_state_encoding tree
      in
      Lwt.return
        Wasm_pvm_sig.
          {current_tick; last_input_read = last_input_info; input_request}

    let set_input_step input_info message tree =
      let open Lwt_syntax in
      let open Wasm_pvm_sig in
      let {inbox_level; message_counter} = input_info in
      let raw_level = Bounded.Int32.NonNegative.to_int32 inbox_level in
      let level = Int32.to_string raw_level in
      let id = Z.to_string message_counter in
      let* pvm_state = Tree_encoding.decode pvm_state_encoding tree in
      let* () =
        match pvm_state.tick_state with
        | Eval {input; _} ->
            Wasm.Input_buffer.(
              enqueue
                input
                {
                  (* This is to distinguish (0) Inbox inputs from (1)
                     DAL/Slot_header inputs. *)
                  rtype = 0l;
                  raw_level;
                  message_counter;
                  payload = String.to_bytes message;
                })
        | Decode _ ->
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/3448
                Avoid throwing exceptions.
                Possibly use a a new state to indicate, such as [Stuck].
            *)
            assert false
      in
      (* Encode the input in the tree under [input/level/id]. *)
      let* tree =
        Tree_encoding.encode
          (Tree_encoding.value ["input"; level; id] Data_encoding.string)
          message
          tree
      in
      (* Increase the current tick counter and mark that no input is required. *)
      let pvm_state =
        {
          pvm_state with
          current_tick = Z.succ pvm_state.current_tick;
          input_request = Wasm_pvm_sig.No_input_required;
        }
      in
      (* Encode the new pvm-state in the tree. *)
      Tree_encoding.encode pvm_state_encoding pvm_state tree
  end

  include Gather_floppies.Make (T) (Raw)
end
