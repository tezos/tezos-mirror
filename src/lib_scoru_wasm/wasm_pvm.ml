(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

open Wasm_pvm_state.Internal_state
module Wasm = Tezos_webassembly_interpreter
module Parsing = Binary_parser_encodings

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
              (Init_encodings.init_kont_encoding ~host_funcs:Host_funcs.all))
           (scope ["modules"] Wasm_encoding.module_instances_encoding))
        (function
          | Init {self; ast_module; init_kont; module_reg} ->
              Some (self, ast_module, init_kont, module_reg)
          | _ -> None)
        (fun (self, ast_module, init_kont, module_reg) ->
          Init {self; ast_module; init_kont; module_reg});
      case
        "eval"
        (Wasm_encoding.config_encoding ~host_funcs:Host_funcs.all)
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
  Tezos_tree_encoding.(scope ["pvm"; "buffers"] Wasm_encoding.buffers_encoding)

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
       (value ~default:Z.zero ["pvm"; "last_top_level_call"] Data_encoding.n)
       (value
          ~default:Constants.wasm_max_tick
          ["pvm"; "max_nb_ticks"]
          Data_encoding.n)
       (value
          ~default:Constants.maximum_reboots_per_input
          ["pvm"; "maximum_reboots_per_input"]
          Data_encoding.n))

module Make (T : Tezos_tree_encoding.TREE) :
  Wasm_pvm_sig.S with type tree = T.tree = struct
  module Raw = struct
    type tree = T.tree

    module Tree_encoding_runner = Tezos_tree_encoding.Runner.Make (T)

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

    let install_boot_sector bs tree =
      let bs = Tezos_lazy_containers.Chunked_byte_vector.of_string bs in
      Tree_encoding_runner.encode
        Tezos_tree_encoding.(
          scope ["durable"; "kernel"; "boot.wasm"; "_"] chunked_byte_vector)
        bs
        tree

    let compute_step_many_until ?(max_steps = 1L) should_continue tree =
      let open Lwt.Syntax in
      let* pvm_state = decode tree in
      let* pvm_state, executed_ticks =
        Wasm_vm.compute_step_many_until ~max_steps should_continue pvm_state
      in
      let* tree = encode pvm_state tree in
      Lwt.return (tree, executed_ticks)

    let compute_step_many ~max_steps tree =
      let open Lwt.Syntax in
      let* pvm_state = decode tree in
      let* pvm_state, executed_ticks =
        Wasm_vm.compute_step_many ~max_steps pvm_state
      in
      let+ tree = encode pvm_state tree in
      (tree, executed_ticks)

    let compute_step tree =
      let open Lwt.Syntax in
      let* initial_state = decode tree in
      let* final_state = Wasm_vm.compute_step initial_state in
      encode final_state tree

    let get_output output_info tree =
      let open Lwt_syntax in
      let* candidate =
        Tree_encoding_runner.decode
          (Tezos_tree_encoding.option durable_buffers_encoding)
          tree
      in
      Lwt.catch
        (fun () ->
          match candidate with
          | Some {output; _} ->
              let+ result = Wasm_vm.get_output output_info output in
              Some result
          | None -> Lwt.return None)
        (fun _ -> Lwt.return None)

    let get_info tree =
      let open Lwt_syntax in
      let* pvm_state = decode tree in
      Wasm_vm.get_info pvm_state

    let set_input_step input_info message tree =
      let open Lwt_syntax in
      let* pvm_state = decode tree in
      let* pvm_state = Wasm_vm.set_input_step input_info message pvm_state in
      encode pvm_state tree

    let reveal_step payload tree =
      let open Lwt_syntax in
      let* pvm_state = decode tree in
      let* pvm_state = Wasm_vm.reveal_step payload pvm_state in
      encode pvm_state tree

    module Internal_for_benchmark = struct
      let decode = decode

      let encode = encode

      let compute_step_many_until = compute_step_many_until

      let compute_step_many_until_pvm_state = Wasm_vm.compute_step_many_until

      let eval_has_finished = Wasm_vm.eval_has_finished
    end

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
            Wasm.Instance.ModuleMap.get
              Constants.wasm_main_module_name
              config.module_reg
        | _ -> raise (Invalid_argument "get_module_instance")

      let is_stuck tree =
        let open Lwt.Syntax in
        let* pvm = Tree_encoding_runner.decode pvm_state_encoding tree in
        match pvm.tick_state with
        | Stuck error -> Lwt.return_some error
        | _ -> Lwt.return_none

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

      let get_output_buffer tree =
        let open Lwt.Syntax in
        let+ pvm = Tree_encoding_runner.decode pvm_state_encoding tree in
        pvm.buffers.output

      let get_input_buffer tree =
        let open Lwt.Syntax in
        let+ pvm = Tree_encoding_runner.decode pvm_state_encoding tree in
        pvm.buffers.input
    end
  end

  include Raw
end
