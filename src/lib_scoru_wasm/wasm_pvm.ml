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

module Make_machine_with_vm (Wasm_vm : Wasm_vm_sig.S) (S : Wasm_pvm_sig.STATE) :
  Wasm_pvm_sig.Machine with type state = S.state = struct
  include S

  let initial_state version empty_state =
    let open Lwt.Syntax in
    let* durable = S.Encoding_runner.decode_durable_storage empty_state in
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
    S.Encoding_runner.encode_durable_storage durable empty_state

  let install_boot_sector ~ticks_per_snapshot ~outbox_validity_period
      ~outbox_message_limit bs state =
    let open Lwt_syntax in
    let* durable = S.Encoding_runner.decode_durable_storage state in
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
    S.Encoding_runner.encode pvm state

  let compute_step_many ?reveal_builtins ?hooks ?write_debug ?stop_at_snapshot
      ~wasm_entrypoint ~max_steps state =
    let open Lwt.Syntax in
    let* pvm_state = S.Encoding_runner.decode state in
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
    let+ state = S.Encoding_runner.encode pvm_state state in
    (state, executed_ticks)

  let compute_step_with_debug ~wasm_entrypoint ~write_debug state =
    let open Lwt.Syntax in
    let* pvm_state = S.Encoding_runner.decode state in
    let* pvm_state =
      Wasm_vm.compute_step_with_debug ~wasm_entrypoint ~write_debug pvm_state
    in
    S.Encoding_runner.encode pvm_state state

  let compute_step ~wasm_entrypoint state =
    compute_step_with_debug state ~wasm_entrypoint ~write_debug:Noop

  let get_output output_info state =
    let open Lwt_syntax in
    let* candidate = S.Encoding_runner.decode_buffers state in
    Lwt.catch
      (fun () ->
        match candidate with
        | Some {output; _} ->
            let+ result = Wasm_vm.get_output output_info output in
            Some result
        | None -> Lwt.return_none)
      (fun _ -> Lwt.return_none)

  let get_info state =
    let open Lwt_syntax in
    let* pvm_state = S.Encoding_runner.decode state in
    Wasm_vm.get_info pvm_state

  let set_input_step input_info message state =
    let open Lwt_syntax in
    let* pvm_state = S.Encoding_runner.decode state in
    let* pvm_state = Wasm_vm.set_input_step input_info message pvm_state in
    S.Encoding_runner.encode pvm_state state

  let reveal_step payload state =
    let open Lwt_syntax in
    let* pvm_state = S.Encoding_runner.decode state in
    let* pvm_state = Wasm_vm.reveal_step payload pvm_state in
    S.Encoding_runner.encode pvm_state state

  let get_wasm_version state =
    let open Lwt.Syntax in
    let* pvm = S.Encoding_runner.decode state in
    Wasm_vm.get_wasm_version pvm

  module Unsafe = struct
    let get_max_nb_ticks state =
      let open Lwt_syntax in
      let+ pvm_state = S.Encoding_runner.decode state in
      pvm_state.max_nb_ticks

    let set_max_nb_ticks n state =
      let open Lwt_syntax in
      let* pvm_state = S.Encoding_runner.decode state in
      let pvm_state = {pvm_state with max_nb_ticks = n} in
      S.Encoding_runner.encode pvm_state state

    let durable_set ~key ~value state =
      let open Lwt_syntax in
      let* pvm_state = S.Encoding_runner.decode state in
      let key = Durable.key_of_string_exn key in
      let* durable = Durable.set_value_exn pvm_state.durable key value in
      let pvm_state = {pvm_state with durable} in
      S.Encoding_runner.encode pvm_state state

    let set_pvm_version ~(version : Wasm_pvm_state.version) state =
      let open Lwt_syntax in
      let* pvm_state = S.Encoding_runner.decode state in
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
      S.Encoding_runner.encode pvm_state state
  end

  module Internal_for_tests = struct
    include Unsafe

    let insert_failure state = S.Internal_for_tests.insert_failure state

    let get_tick_state state =
      let open Lwt_syntax in
      let+ pvm_state = S.Encoding_runner.decode state in
      pvm_state.tick_state

    let get_module_instance_exn state =
      let open Lwt_syntax in
      let* pvm_state = S.Encoding_runner.decode state in
      match pvm_state.tick_state with
      | Eval {module_reg; _} | Init {module_reg; _} ->
          Wasm.Instance.ModuleMap.get Constants.wasm_main_module_name module_reg
      | _ -> raise (Invalid_argument "get_module_instance")

    let is_stuck state =
      let open Lwt.Syntax in
      let* pvm = S.Encoding_runner.decode state in
      match pvm.tick_state with
      | Stuck error -> Lwt.return_some error
      | _ -> Lwt.return_none

    let set_maximum_reboots_per_input n state =
      let open Lwt_syntax in
      let* pvm_state = S.Encoding_runner.decode state in
      let pvm_state =
        {
          pvm_state with
          maximum_reboots_per_input = n;
          reboot_counter = Z.(min (succ n) pvm_state.reboot_counter);
        }
      in
      S.Encoding_runner.encode pvm_state state

    let decr_reboot_counter state =
      let open Lwt_syntax in
      let* pvm_state = S.Encoding_runner.decode state in
      let pvm_state =
        {pvm_state with reboot_counter = Z.pred pvm_state.reboot_counter}
      in
      S.Encoding_runner.encode pvm_state state

    let reset_reboot_counter state =
      let open Lwt_syntax in
      let* pvm_state = S.Encoding_runner.decode state in
      let pvm_state =
        {
          pvm_state with
          reboot_counter = Z.succ pvm_state.maximum_reboots_per_input;
        }
      in
      S.Encoding_runner.encode pvm_state state

    let get_output_buffer state =
      let open Lwt.Syntax in
      let+ pvm = S.Encoding_runner.decode state in
      pvm.buffers.output

    let get_input_buffer state =
      let open Lwt.Syntax in
      let+ pvm = S.Encoding_runner.decode state in
      pvm.buffers.input

    let compute_step_many_until ~wasm_entrypoint ?max_steps ?hooks
        ?reveal_builtins ?write_debug should_compute state =
      let open Lwt.Syntax in
      let* pvm_state = S.Encoding_runner.decode state in
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
      let+ state = S.Encoding_runner.encode pvm_state state in
      (state, ticks)
  end
end

module Make_machine = Make_machine_with_vm (Wasm_vm)

module Make_pvm_machine_with_vm
    (Wasm_vm : Wasm_vm_sig.S)
    (State : Wasm_pvm_sig.STATE_PROOF) :
  Wasm_pvm_sig.S
    with type context = State.context
     and type state = State.state
     and type proof = State.proof = struct
  include State
  include Make_machine_with_vm (Wasm_vm) (State)
end

module Make_pvm_machine = Make_pvm_machine_with_vm (Wasm_vm)
