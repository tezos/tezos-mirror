(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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
open Tezos_lazy_containers

let durable_scope = ["durable"]

let tick_state_encoding =
  let open Tezos_tree_encoding in
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

let durable_buffers_encoding =
  Tezos_tree_encoding.(scope ["pvm"; "buffers"] Wasm_encoding.buffers_encoding)

let durable_storage_encoding =
  Tezos_tree_encoding.(scope durable_scope Durable.encoding)

let default_buffers validity_period message_limit () =
  Tezos_webassembly_interpreter.Eval.
    {
      input = Tezos_webassembly_interpreter.Input_buffer.alloc ();
      output =
        Tezos_webassembly_interpreter.Output_buffer.alloc
          ~validity_period
          ~message_limit
          ~last_level:None;
    }

let output_buffer_parameters_encoding =
  Tezos_tree_encoding.(
    conv
      (fun (validity_period, message_limit) -> {validity_period; message_limit})
      (fun {validity_period; message_limit} -> (validity_period, message_limit))
      (tup2
         ~flatten:true
         (value ["pvm"; "outbox_validity_period"] Data_encoding.int32)
         (value ["pvm"; "outbox_message_limit"] Data_encoding.z)))

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

module type CONTEXT_PROOF = sig
  type context

  module Wrapped_tree : Tezos_tree_encoding.TREE

  val empty_tree : unit -> Wrapped_tree.tree

  val tree_hash :
    Wrapped_tree.tree -> Tezos_crypto.Hashed.Smart_rollup_state_hash.t Lwt.t

  type proof

  val proof_encoding : proof Data_encoding.t

  val proof_start_state : proof -> Tezos_crypto.Hashed.Smart_rollup_state_hash.t

  val proof_stop_state : proof -> Tezos_crypto.Hashed.Smart_rollup_state_hash.t

  val produce_proof :
    context ->
    Wrapped_tree.tree ->
    (Wrapped_tree.tree -> (Wrapped_tree.tree * 'a) Lwt.t) ->
    (proof * 'a) option Lwt.t

  val verify_proof :
    proof ->
    (Wrapped_tree.tree -> (Wrapped_tree.tree * 'a) Lwt.t) ->
    (Wrapped_tree.tree * 'a) option Lwt.t

  val cast_read_only : proof -> proof
end

module Make_machine_with_vm
    (Wasm_vm : Wasm_vm_sig.S)
    (T : Tezos_tree_encoding.TREE) :
  Wasm_pvm_sig.Machine with type state = T.tree = struct
  type state = T.tree

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

  let initial_state version empty_tree =
    let open Lwt.Syntax in
    let* durable =
      Tree_encoding_runner.decode durable_storage_encoding empty_tree
    in
    let version_str =
      Data_encoding.Binary.to_string_exn Wasm_pvm_state.version_encoding version
    in
    let* durable =
      Durable.set_value_exn
        ~edit_readonly:true
        durable
        Constants.version_key
        version_str
    in
    Tree_encoding_runner.encode durable_storage_encoding durable empty_tree

  let install_boot_sector ~ticks_per_snapshot ~outbox_validity_period
      ~outbox_message_limit bs tree =
    let open Lwt_syntax in
    let open Tezos_tree_encoding in
    let* durable =
      Tree_encoding_runner.decode (scope durable_scope Durable.encoding) tree
    in
    let reboot_flag_key = Durable.key_of_string_exn "/kernel/env/reboot" in
    let kernel_key = Durable.key_of_string_exn "/kernel/boot.wasm" in
    let* durable = Durable.set_value_exn durable reboot_flag_key "" in
    let* durable = Durable.set_value_exn durable kernel_key bs in
    let pvm : pvm_state =
      {
        last_input_info = None;
        current_tick = Z.zero;
        reboot_counter = Z.succ Constants.maximum_reboots_per_input;
        durable;
        buffers = default_buffers outbox_validity_period outbox_message_limit ();
        tick_state = Collect;
        last_top_level_call = Z.zero;
        max_nb_ticks = ticks_per_snapshot;
        maximum_reboots_per_input = Constants.maximum_reboots_per_input;
        output_buffer_parameters =
          {
            validity_period = outbox_validity_period;
            message_limit = outbox_message_limit;
          };
      }
    in
    encode pvm tree

  let compute_step_many ?reveal_builtins ?hooks ?write_debug ?stop_at_snapshot
      ~wasm_entrypoint ~max_steps tree =
    let open Lwt.Syntax in
    let* pvm_state = decode tree in
    let* pvm_state, executed_ticks =
      Wasm_vm.compute_step_many
        ?reveal_builtins
        ?hooks
        ?write_debug
        ?stop_at_snapshot
        ~wasm_entrypoint
        ~max_steps
        pvm_state
    in
    let+ tree = encode pvm_state tree in
    (tree, executed_ticks)

  let compute_step_with_debug ~wasm_entrypoint ~write_debug tree =
    let open Lwt.Syntax in
    let* initial_state = decode tree in
    let* final_state =
      Wasm_vm.compute_step_with_debug
        ~wasm_entrypoint
        ~write_debug
        initial_state
    in
    encode final_state tree

  let compute_step ~wasm_entrypoint tree =
    compute_step_with_debug tree ~wasm_entrypoint ~write_debug:Noop

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
        | None -> Lwt.return_none)
      (fun _ -> Lwt.return_none)

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

  let get_wasm_version tree =
    let open Lwt.Syntax in
    let* pvm = Tree_encoding_runner.decode pvm_state_encoding tree in
    Wasm_vm.get_wasm_version pvm

  module Unsafe = struct
    let get_max_nb_ticks tree =
      let open Lwt_syntax in
      let+ pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      pvm_state.max_nb_ticks

    let set_max_nb_ticks n tree =
      let open Lwt_syntax in
      let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      let pvm_state = {pvm_state with max_nb_ticks = n} in
      Tree_encoding_runner.encode pvm_state_encoding pvm_state tree

    let durable_set ~key ~value tree =
      let open Lwt_syntax in
      let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      let key = Durable.key_of_string_exn key in
      let* durable = Durable.set_value_exn pvm_state.durable key value in
      let pvm_state = {pvm_state with durable} in
      Tree_encoding_runner.encode pvm_state_encoding pvm_state tree

    let set_pvm_version ~(version : Wasm_pvm_state.version) tree =
      let open Lwt_syntax in
      let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      let* durable =
        Durable.set_value_exn
          ~edit_readonly:true
          pvm_state.durable
          Constants.version_key
          (Data_encoding.Binary.to_string_exn
             Wasm_pvm_state.version_encoding
             version)
      in
      let pvm_state = {pvm_state with durable} in
      let pvm_state = Wasm_vm.Unsafe.apply_migration version pvm_state in
      Tree_encoding_runner.encode pvm_state_encoding pvm_state tree
  end

  module Internal_for_tests = struct
    include Unsafe

    let insert_failure tree =
      let open Lwt_syntax in
      let add n = T.add tree ["failures"; string_of_int n] Bytes.empty in
      let* n = T.length tree ["failures"] in
      add n

    let get_tick_state tree =
      let open Lwt_syntax in
      let+ pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      pvm_state.tick_state

    let get_module_instance_exn tree =
      let open Lwt_syntax in
      let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      match pvm_state.tick_state with
      | Eval {module_reg; _} | Init {module_reg; _} ->
          Wasm.Instance.ModuleMap.get Constants.wasm_main_module_name module_reg
      | _ -> raise (Invalid_argument "get_module_instance")

    let is_stuck tree =
      let open Lwt.Syntax in
      let* pvm = Tree_encoding_runner.decode pvm_state_encoding tree in
      match pvm.tick_state with
      | Stuck error -> Lwt.return_some error
      | _ -> Lwt.return_none

    let set_maximum_reboots_per_input n tree =
      let open Lwt_syntax in
      let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      let pvm_state =
        {
          pvm_state with
          maximum_reboots_per_input = n;
          reboot_counter = Z.(min (succ n) pvm_state.reboot_counter);
        }
      in
      Tree_encoding_runner.encode pvm_state_encoding pvm_state tree

    let decr_reboot_counter tree =
      let open Lwt_syntax in
      let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      let pvm_state =
        {pvm_state with reboot_counter = Z.pred pvm_state.reboot_counter}
      in
      Tree_encoding_runner.encode pvm_state_encoding pvm_state tree

    let reset_reboot_counter tree =
      let open Lwt_syntax in
      let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      let pvm_state =
        {
          pvm_state with
          reboot_counter = Z.succ pvm_state.maximum_reboots_per_input;
        }
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

    let compute_step_many_until ~wasm_entrypoint ?max_steps ?hooks
        ?reveal_builtins ?write_debug should_compute tree =
      let open Lwt.Syntax in
      let* pvm_state = Tree_encoding_runner.decode pvm_state_encoding tree in
      let* pvm_state, ticks =
        Wasm_vm.compute_step_many_until
          ~wasm_entrypoint
          ?max_steps
          ?hooks
          ?reveal_builtins
          ?write_debug
          should_compute
          pvm_state
      in
      let+ tree =
        Tree_encoding_runner.encode pvm_state_encoding pvm_state tree
      in
      (tree, ticks)
  end
end

module Make_machine = Make_machine_with_vm (Wasm_vm)

module Make_pvm_machine_with_vm
    (Wasm_vm : Wasm_vm_sig.S)
    (Context : CONTEXT_PROOF) :
  Wasm_pvm_sig.S
    with type context = Context.context
     and type state = Context.Wrapped_tree.tree
     and type proof = Context.proof = struct
  include Make_machine_with_vm (Wasm_vm) (Context.Wrapped_tree)
  include Context

  type state = Context.Wrapped_tree.tree

  let empty_state = empty_tree

  let state_hash = Context.tree_hash
end

module Make_pvm_machine = Make_pvm_machine_with_vm (Wasm_vm)

module Context_in_memory = struct
  module Context = Tezos_context_memory.Context_binary

  type context = Context.context

  type Tezos_tree_encoding.tree_instance += PVM_tree of Context.tree

  module Wrapped_tree = struct
    include Context.Tree

    type tree = Context.tree

    let select = function
      | PVM_tree t -> t
      | _ -> raise Tezos_tree_encoding.Incorrect_tree_type

    let wrap t = PVM_tree t
  end

  let empty_tree () = Context.make_empty_context () |> Wrapped_tree.empty

  let tree_hash tree =
    Context.Tree.hash tree
    |> Tezos_crypto.Hashed.Smart_rollup_state_hash.context_hash_to_state_hash
    |> Lwt.return

  type proof = Context.Proof.(tree t)

  let proof_encoding =
    Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree2
    .tree_proof_encoding

  let kinded_hash_to_state_hash = function
    | `Value hash | `Node hash ->
        Tezos_crypto.Hashed.Smart_rollup_state_hash.context_hash_to_state_hash
          hash

  let proof_start_state proof =
    kinded_hash_to_state_hash proof.Context.Proof.before

  let proof_stop_state proof =
    kinded_hash_to_state_hash proof.Context.Proof.after

  let cast_read_only proof = Context.Proof.{proof with after = proof.before}

  let produce_proof context tree step =
    let open Lwt_syntax in
    let* context = Context.add_tree context [] tree in
    let* _hash = Context.commit ~time:Time.Protocol.epoch context in
    let index = Context.index context in
    match Context.Tree.kinded_key tree with
    | Some k ->
        let* p = Context.produce_tree_proof index k step in
        return_some p
    | None -> return_none

  type error += Expected_binary_proof

  let () =
    register_error_kind
      `Permanent
      ~id:"Expected_binary_proof"
      ~title:"Expected binary proof"
      ~description:"An invalid proof has been submitted"
      Data_encoding.empty
      (function Expected_binary_proof -> Some () | _ -> None)
      (fun () -> Expected_binary_proof)

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4386 Extracted and
   adapted from {!Tezos_context_memory}. Ideally, this function should
   be exported there.

   In a nutshell, the context library exposed by the environment is
   implemented such that it can verify proofs generated by both
   [Context] and [Context_binary], and the only thing that
   differentiate these proofs from its perspective is the second bit
   of the [version] field of the proof.

   To ensure we only consider proofs computed against a binary tree,
   we check said bit. This prevents a 32-ary proof to be accepted by
   the protocol in the case where a given key-value store has the same
   hash with both [Context] and [Context_binary] (something that
   happens when the tree contains only one entry). *)
  let check_is_binary proof =
    let extract_bit v mask = Compare.Int.(v land mask <> 0) in
    let binary_mask = 0b10 in
    let is_binary = extract_bit proof.Context.Proof.version binary_mask in
    error_unless is_binary Expected_binary_proof

  let verify_proof p f =
    let open Lwt_option_syntax in
    let*? () = Result.to_option (check_is_binary p) in
    Lwt.map Result.to_option (Context.verify_tree_proof p f)
end

module Wasm_pvm_in_memory :
  Wasm_pvm_sig.S
    with type context = Context_in_memory.context
     and type state = Context_in_memory.Wrapped_tree.tree
     and type proof = Context_in_memory.proof =
  Make_pvm_machine (Context_in_memory)
