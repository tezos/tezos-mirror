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
  kernel : Lazy_containers.Chunked_byte_vector.Lwt.t;
  current_tick : Z.t;
  last_input_info : Wasm_pvm_sig.input_info option;
  module_reg : Wasm.Instance.module_reg;
  tick : tick_state;
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

    let pvm_state_encoding =
      let open Tree_encoding in
      conv
        (fun (current_tick, kernel, last_input_info, tick, module_reg) ->
          {current_tick; kernel; last_input_info; tick; module_reg})
        (fun {current_tick; kernel; last_input_info; tick; module_reg} ->
          (current_tick, kernel, last_input_info, tick, module_reg))
        (tup5
           ~flatten:true
           (value ~default:Z.zero ["wasm"; "current_tick"] Data_encoding.n)
           (scope ["durable"; "kernel"; "boot.wasm"] chunked_byte_vector)
           (value_option ["wasm"; "input"] Wasm_pvm_sig.input_info_encoding)
           (scope ["wasm"] tick_state_encoding)
           (scope ["modules"] Wasm_encoding.module_instances_encoding))

    let status_encoding =
      Tree_encoding.value ["input"; "consuming"] Data_encoding.bool

    let next_state state =
      let open Lwt_syntax in
      match state.tick with
      | Decode {module_kont = MKStop ast_module; _} ->
          let self = Wasm.Instance.Module_key wasm_main_module_name in
          (* The module instance is registered in [self] that contains the
             module registry, why we can ignore the result here. *)
          let* _module_inst =
            Wasm.Eval.init
              ~module_reg:state.module_reg
              ~self
              host_funcs
              ast_module
              []
          in
          let eval_config = Wasm.Eval.config host_funcs self [] [] in
          Lwt.return {state with tick = Eval eval_config}
      | Decode m ->
          let+ m =
            Tezos_webassembly_interpreter.Decode.module_step state.kernel m
          in
          {state with tick = Decode m}
      | Eval ({Wasm.Eval.frame; code; _} as eval_config) -> (
          match code with
          | _values, [] ->
              (* We have an empty set of admin instructions so we create one
                 that invokes the main function. *)
              let* module_inst =
                Wasm.Instance.ModuleMap.get
                  wasm_main_module_name
                  state.module_reg
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
              Lwt.return {state with tick = Eval eval_config}
          | _ ->
              (* Continue execution. *)
              let* eval_config = Wasm.Eval.step state.module_reg eval_config in
              Lwt.return {state with tick = Eval eval_config})

    let compute_step tree =
      let open Lwt_syntax in
      let* state = Tree_encoding.decode pvm_state_encoding tree in
      let* state = next_state state in
      let state = {state with current_tick = Z.succ state.current_tick} in
      let want_more_input =
        match state.tick with
        | Eval {code = _, []; _} ->
            (* Ask for more input if the kernel has yielded (empty admin
               instructions). *)
            true
        | _ -> false
      in
      let* tree = Tree_encoding.encode status_encoding want_more_input tree in
      Tree_encoding.encode pvm_state_encoding state tree

    let get_output _ _ = Lwt.return ""

    (* TODO: #3444
       Create a may-fail tree-encoding-decoding combinator.
       https://gitlab.com/tezos/tezos/-/issues/3444
    *)
    (* TODO: #3448
       Remove the mention of exceptions from lib_scoru_wasm Make signature.
       Add try_with or similar to catch exceptions and put the machine in a
       stuck state instead. https://gitlab.com/tezos/tezos/-/issues/3448
    *)
    let current_tick_encoding =
      Tree_encoding.value ["wasm"; "current_tick"] Data_encoding.z

    let level_encoding =
      Tree_encoding.value ["input"; "level"] Bounded.Int32.NonNegative.encoding

    let id_encoding = Tree_encoding.value ["input"; "id"] Data_encoding.z

    let last_input_read_encoder =
      Tree_encoding.tup2 ~flatten:true level_encoding id_encoding

    let inp_encoding level id =
      Tree_encoding.value ["input"; level; id] Data_encoding.string

    let get_info tree =
      let open Lwt_syntax in
      let* waiting =
        try Tree_encoding.decode status_encoding tree
        with _ -> Lwt.return false
      in
      let input_request =
        if waiting then Wasm_pvm_sig.Input_required
        else Wasm_pvm_sig.No_input_required
      in
      let* input =
        try
          let* t = Tree_encoding.decode last_input_read_encoder tree in
          Lwt.return @@ Some t
        with _ -> Lwt.return_none
      in
      let last_input_read =
        Option.map
          (fun (inbox_level, message_counter) ->
            Wasm_pvm_sig.{inbox_level; message_counter})
          input
      in
      let* current_tick =
        try Tree_encoding.decode current_tick_encoding tree
        with _ -> Lwt.return Z.zero
      in
      Lwt.return Wasm_pvm_sig.{current_tick; last_input_read; input_request}

    let set_input_step input_info message tree =
      let open Lwt_syntax in
      let open Wasm_pvm_sig in
      let {inbox_level; message_counter} = input_info in
      let raw_level = Bounded.Int32.NonNegative.to_int32 inbox_level in
      let level = Int32.to_string raw_level in
      let id = Z.to_string message_counter in
      let* current_tick = Tree_encoding.decode current_tick_encoding tree in
      let* state = Tree_encoding.decode pvm_state_encoding tree in
      let* () =
        match state.tick with
        | Eval config ->
            Wasm.Input_buffer.(
              enqueue
                config.input
                {
                  (* This is to distinguish (0) Inbox inputs from (1)
                     DAL/Slot_header inputs. *)
                  rtype = 0l;
                  raw_level;
                  message_counter;
                  payload = String.to_bytes message;
                })
        | _ ->
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/3448
                Avoid throwing exceptions.
                Possibly use a a new state to indicate, such as [Stuck].
            *)
            assert false
      in
      let* tree = Tree_encoding.encode pvm_state_encoding state tree in
      let* tree =
        Tree_encoding.encode current_tick_encoding (Z.succ current_tick) tree
      in
      let* tree = Tree_encoding.encode status_encoding false tree in
      Tree_encoding.encode (inp_encoding level id) message tree
  end

  include Gather_floppies.Make (T) (Raw)
end
