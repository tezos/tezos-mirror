(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Wasm_pvm_state.Internal_state
module Parsing = Binary_parser_encodings
open Tezos_lazy_containers

let durable_scope = ["durable"]

let durable_buffers_encoding =
  Tezos_tree_encoding.(scope ["pvm"; "buffers"] Wasm_encoding.buffers_encoding)

let durable_storage_encoding =
  Tezos_tree_encoding.(scope durable_scope Durable.encoding)

let output_buffer_parameters_encoding =
  Tezos_tree_encoding.(
    conv
      (fun (validity_period, message_limit) ->
        Wasm_pvm_state.Internal_state.{validity_period; message_limit})
      (fun {validity_period; message_limit} -> (validity_period, message_limit))
      (tup2
         ~flatten:true
         (value ["pvm"; "outbox_validity_period"] Data_encoding.int32)
         (value ["pvm"; "outbox_message_limit"] Data_encoding.z)))

let tick_state_encoding =
  let open Tezos_tree_encoding in
  let open Wasm_pvm_state.Internal_state in
  tagged_union
    ~default:(fun () -> Collect)
    (value [] Data_encoding.string)
    [
      case
        "snapshot"
        (return ())
        (function Snapshot -> Some () | _ -> None)
        (fun () -> Snapshot);
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
              (Lazy_vector.Int32Vector.encoding
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
           (scope ["init_kont"] Init_encodings.init_kont_encoding)
           (scope ["modules"] Wasm_encoding.module_instances_encoding))
        (function
          | Init {self; ast_module; init_kont; module_reg} ->
              Some (self, ast_module, init_kont, module_reg)
          | _ -> None)
        (fun (self, ast_module, init_kont, module_reg) ->
          Init {self; ast_module; init_kont; module_reg});
      case
        "eval"
        (tup2
           ~flatten:true
           (scope ["config"] @@ Wasm_encoding.config_encoding)
           (scope ["modules"] Wasm_encoding.module_instances_encoding))
        (function
          | Eval {config; module_reg} -> Some (config, module_reg) | _ -> None)
        (fun (config, module_reg) -> Eval {config; module_reg});
      case
        "stuck"
        (value [] Wasm_pvm_errors.encoding)
        (function Stuck err -> Some err | _ -> None)
        (fun err -> Stuck err);
      case
        "collect"
        (return ())
        (function Collect -> Some () | _ -> None)
        (fun () -> Collect);
      case
        "padding"
        (return ())
        (function Padding -> Some () | _ -> None)
        (fun () -> Padding);
    ]

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
           maximum_reboots_per_input,
           output_buffer_parameters )
       ->
      Wasm_pvm_state.Internal_state.
        {
          last_input_info;
          current_tick;
          reboot_counter =
            Option.value
              ~default:(Z.succ maximum_reboots_per_input)
              (* One is used to read the inbox *)
              reboot_counter;
          durable;
          buffers =
            (*`Gather_floppies` uses `get_info`, that decodes the state of the
            PVM, which at the start of the rollup doesn't exist. *)
            Option.value_f
              ~default:
                (default_buffers
                   output_buffer_parameters.validity_period
                   output_buffer_parameters.message_limit)
              buffers;
          tick_state;
          last_top_level_call;
          max_nb_ticks;
          maximum_reboots_per_input;
          output_buffer_parameters;
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
           output_buffer_parameters;
         }
       ->
      ( last_input_info,
        current_tick,
        Some reboot_counter,
        durable,
        Some buffers,
        tick_state,
        last_top_level_call,
        max_nb_ticks,
        maximum_reboots_per_input,
        output_buffer_parameters ))
    (tup10
       ~flatten:true
       (value_option ["wasm"; "input"] Wasm_pvm_sig.input_info_encoding)
       (value ~default:Z.zero ["wasm"; "current_tick"] Data_encoding.n)
       (value_option ["wasm"; "reboot_counter"] Data_encoding.n)
       durable_storage_encoding
       (option durable_buffers_encoding)
       (scope ["wasm"] tick_state_encoding)
       (value ~default:Z.zero ["pvm"; "last_top_level_call"] Data_encoding.n)
       (value ["pvm"; "max_nb_ticks"] Data_encoding.n)
       (value
          ~default:Constants.maximum_reboots_per_input
          ["pvm"; "maximum_reboots_per_input"]
          Data_encoding.n)
       output_buffer_parameters_encoding)

module Make (T : Tezos_tree_encoding.TREE) :
  Wasm_pvm_sig.STATE with type state = T.tree = struct
  type state = T.tree

  module Tree_encoding_runner = Tezos_tree_encoding.Runner.Make (T)

  module Encoding_runner = struct
    let encode pvm_state state =
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
      let* state = T.remove state ["wasm"] in
      Tree_encoding_runner.encode pvm_state_encoding pvm_state state

    let decode state = Tree_encoding_runner.decode pvm_state_encoding state

    let encode_durable_storage durable state =
      Tree_encoding_runner.encode durable_storage_encoding durable state

    let decode_durable_storage state =
      Tree_encoding_runner.decode durable_storage_encoding state

    let decode_buffers state =
      Tree_encoding_runner.decode
        (Tezos_tree_encoding.option durable_buffers_encoding)
        state
  end

  module Internal_for_tests = struct
    let insert_failure state =
      let open Lwt_syntax in
      let add n = T.add state ["failures"; string_of_int n] Bytes.empty in
      let* n = T.length state ["failures"] in
      add n
  end
end
