(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2024 TriliTech <contact@trili.tech>                    *)
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

open Protocol
open Alpha_context

type unsafe_patch =
  | Increase_max_nb_ticks of int64
  | Patch_durable_storage of {key : string; value : string}
  | Patch_PVM_version of {version : Tezos_scoru_wasm.Wasm_pvm_state.version}

(* FIXME: https://linear.app/tezos/issue/L2-1427
   Inert placeholder keeping this protocol single-storage: the empty
   config leaves the [Nds_host_functions] gate closed, so NDS never
   activates. *)
module Fast_pvm_params = Tezos_scoru_wasm_fast.Pvm.Default_params
module Wasm_fast_pvm_machine =
  Tezos_scoru_wasm_fast.Pvm.Make_pvm (Fast_pvm_params) (Wasm_2_0_0_irmin_state)

module Wasm_pvm_on_disk = Sc_rollup.Wasm_2_0_0PVM.Make_pvm (struct
  include Wasm_fast_pvm_machine

  let compute_step =
    compute_step ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
end)

module Impl : Pvm_sig.S with type Unsafe_patches.t = unsafe_patch = struct
  include Wasm_pvm_on_disk
  module Ctxt_wrapper = Context_wrapper.Irmin

  type repo = Ctxt_wrapper.repo

  let kind = Sc_rollup.Kind.Wasm_2_0_0

  let new_dissection = Game_helpers.Wasm.new_dissection

  module Inspect_durable_state = struct
    let lookup state keys =
      Wasm_2_0_0_irmin_state.Storage.find_value
        state
        ("/" ^ String.concat "/" keys)
  end

  let string_of_status : status -> string = function
    | Waiting_for_input_message -> "Waiting for input message"
    | Waiting_for_reveal (Sc_rollup.Reveal_raw_data hash) ->
        Format.asprintf
          "Waiting for preimage reveal %a"
          Sc_rollup_reveal_hash.pp
          hash
    | Waiting_for_reveal Sc_rollup.Reveal_metadata -> "Waiting for metadata"
    | Waiting_for_reveal (Sc_rollup.Request_dal_page page_id) ->
        Format.asprintf "Waiting for page data %a" Dal.Page.pp page_id
    | Waiting_for_reveal Sc_rollup.Reveal_dal_parameters ->
        "Waiting for DAL parameters"
    | Computing -> "Computing"
    | Waiting_for_reveal (Request_adal_page _) ->
        (* ADAL/FIXME: https://gitlab.com/tezos/tezos/-/milestones/410

           To be implemented. *)
        assert false

  let eval_many ?(check_invalid_kernel = true) ?(fallback_to_slow_vm = true)
      ~reveal_builtins ~write_debug ~is_reveal_enabled:_ =
    Wasm_fast_pvm_machine.compute_step_many
      ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
      ~reveal_builtins
      ~write_debug
      ~hooks:
        (Wasm_2_0_0_utilities.hooks ~check_invalid_kernel ~fallback_to_slow_vm)

  (** WASM PVM Mutable API works by holding a reference to an immutable state
      and wrapping all immutable functionality around the reference *)
  module Mutable_state :
    Pvm_sig.MUTABLE_STATE_S
      with type hash = hash
       and type repo = repo
       and type status = status
       and type t = Ctxt_wrapper.mut_state = struct
    include Irmin_context.PVMState

    type t = state ref

    type hash = Sc_rollup.State_hash.t

    type repo = Ctxt_wrapper.repo

    type nonrec status = status

    let get_tick state = get_tick !state

    let state_hash state = state_hash !state

    let get_current_level state =
      let open Lwt_syntax in
      let+ level = get_current_level !state in
      Option.map Raw_level.to_int32 level

    let get_outbox level state =
      get_outbox (Raw_level.of_int32_exn level) !state

    let get_status ~is_reveal_enabled state =
      get_status ~is_reveal_enabled !state

    let set_initial_state ~empty =
      let open Lwt_syntax in
      let+ state = initial_state ~empty:!empty in
      empty := state

    let install_boot_sector state boot_sector =
      let open Lwt_syntax in
      let+ new_state = install_boot_sector !state boot_sector in
      state := new_state

    let is_input_state ~is_reveal_enabled state =
      is_input_state ~is_reveal_enabled !state

    let set_input input state =
      let open Lwt_syntax in
      let* imm_state = set_input input !state in
      state := imm_state ;
      return_unit

    let eval_many ?check_invalid_kernel ?fallback_to_slow_vm ~reveal_builtins
        ~write_debug ~is_reveal_enabled ?stop_at_snapshot ~max_steps mut_state =
      let open Lwt_syntax in
      let* imm_state, steps =
        eval_many
          ?check_invalid_kernel
          ?fallback_to_slow_vm
          ~reveal_builtins
          ~write_debug
          ~is_reveal_enabled
          ?stop_at_snapshot
          ~max_steps
          !mut_state
      in
      mut_state := imm_state ;
      return steps

    module Inspect_durable_state = struct
      let lookup state keys = Inspect_durable_state.lookup !state keys
    end

    module Internal_for_tests = struct
      let insert_failure state =
        let open Lwt_syntax in
        let* imm_state = Internal_for_tests.insert_failure !state in
        state := imm_state ;
        return_unit
    end
  end

  module Unsafe_patches = struct
    type t = unsafe_patch

    let of_patch (p : Pvm_patches.unsafe_patch) =
      match p with
      | Increase_max_nb_ticks max_nb_ticks ->
          Ok (Increase_max_nb_ticks max_nb_ticks)
      | Patch_durable_storage {key; value} ->
          Ok (Patch_durable_storage {key; value})
      | Patch_PVM_version {version} -> (
          let version_opt =
            Tezos_scoru_wasm.Wasm_pvm_state.version_of_string version
          in
          match version_opt with
          | Some version -> Ok (Patch_PVM_version {version})
          | None ->
              invalid_arg
                (Format.sprintf
                   "Unsafe patch: unknown WASM PVM version %s"
                   version))

    let apply state unsafe_patch =
      let open Lwt_syntax in
      match unsafe_patch with
      | Increase_max_nb_ticks max_nb_ticks ->
          let* registered_max_nb_ticks =
            Wasm_fast_pvm_machine.Unsafe.get_max_nb_ticks state
          in
          let max_nb_ticks = Z.of_int64 max_nb_ticks in
          if Z.Compare.(max_nb_ticks < registered_max_nb_ticks) then
            Format.ksprintf
              invalid_arg
              "Decreasing tick limit of WASM PVM from %s to %s is not allowed"
              (Z.to_string registered_max_nb_ticks)
              (Z.to_string max_nb_ticks) ;
          Wasm_fast_pvm_machine.Unsafe.set_max_nb_ticks max_nb_ticks state
      | Patch_durable_storage {key; value} ->
          Wasm_fast_pvm_machine.Unsafe.durable_set ~key ~value state
      | Patch_PVM_version {version} ->
          Wasm_fast_pvm_machine.Unsafe.set_pvm_version ~version state

    let apply_mutable state patch =
      let open Lwt_syntax in
      let+ patched_state = apply !state patch in
      state := patched_state
  end
end

include Impl

(* FIXME: https://linear.app/tezos/issue/L2-1427
   Currently unused: nothing instantiates this PVM yet.  It reuses
   {!Wasm_2_0_0_dual_state.Fast_pvm_params} (empty config, no activation
   factory), so the [Nds_host_functions] gate stays closed and NDS never
   activates; wiring the node to open the dual context backend and pass a
   live activation factory is the remaining work. *)

(** Dual-storage instantiation of the WASM PVM, packaging the shared
    {!Wasm_2_0_0_dual_state} backend (the [Irmin + NDS] context and the dual
    state/proof machine) as a {!Pvm_sig.S}. *)
module Impl_dual_state : Pvm_sig.S with type Unsafe_patches.t = unsafe_patch =
struct
  module Dual_state = Wasm_2_0_0_dual_state.Dual_state

  type repo = Wasm_2_0_0_dual_state.repo

  module Wasm_dual_pvm :
    Sc_rollup.Wasm_2_0_0PVM.S
      with type context = ([`Read | `Write], repo) Context_sigs.raw_index
       and type state = Dual_state.state
       and type proof = Dual_state.proof =
  Sc_rollup.Wasm_2_0_0PVM.Make_pvm (struct
    include Wasm_2_0_0_dual_state.Machine

    let compute_step =
      compute_step ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
  end)

  include Wasm_dual_pvm
  module Ctxt_wrapper = Context.Wrapper.Make (Wasm_2_0_0_dual_state.Dual_context)

  (* Reused verbatim from {!Impl}: backend-independent. *)
  let kind = Impl.kind

  let new_dissection = Impl.new_dissection

  module Inspect_durable_state = struct
    let lookup (state : Dual_state.state) keys =
      let open Lwt_syntax in
      let key =
        Tezos_scoru_wasm.Durable.key_of_string_exn ("/" ^ String.concat "/" keys)
      in
      let* durable =
        Wasm_2_0_0_irmin_state.Encoding_runner.decode_durable_storage
          state.Dual_state.irmin
      in
      Tezos_scoru_wasm.Durable.find_value_as_bytes durable key
  end

  let string_of_status : status -> string = function
    | Waiting_for_input_message -> "Waiting for input message"
    | Waiting_for_reveal (Sc_rollup.Reveal_raw_data hash) ->
        Format.asprintf
          "Waiting for preimage reveal %a"
          Sc_rollup_reveal_hash.pp
          hash
    | Waiting_for_reveal Sc_rollup.Reveal_metadata -> "Waiting for metadata"
    | Waiting_for_reveal (Sc_rollup.Request_dal_page page_id) ->
        Format.asprintf "Waiting for page data %a" Dal.Page.pp page_id
    | Waiting_for_reveal Sc_rollup.Reveal_dal_parameters ->
        "Waiting for DAL parameters"
    | Computing -> "Computing"
    | Waiting_for_reveal (Request_adal_page _) ->
        (* ADAL/FIXME: https://gitlab.com/tezos/tezos/-/milestones/410

           To be implemented. *)
        assert false

  let eval_many ?(check_invalid_kernel = true) ?(fallback_to_slow_vm = true)
      ~reveal_builtins ~write_debug ~is_reveal_enabled:_ =
    Wasm_2_0_0_dual_state.Machine.compute_step_many
      ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
      ~reveal_builtins
      ~write_debug
      ~hooks:
        (Wasm_2_0_0_utilities.hooks ~check_invalid_kernel ~fallback_to_slow_vm)

  (** Mirror of {!Impl.Mutable_state} over the dual state: a reference
      to an immutable dual state, with all immutable functionality
      wrapped around the reference. *)
  module Mutable_state :
    Pvm_sig.MUTABLE_STATE_S
      with type hash = hash
       and type repo = repo
       and type status = status
       and type t = Ctxt_wrapper.mut_state = struct
    include Wasm_2_0_0_dual_state.Dual_context.PVMState

    type t = Wasm_2_0_0_dual_state.Dual_context.mut_state

    type hash = Sc_rollup.State_hash.t

    type nonrec repo = repo

    type nonrec status = status

    let get_tick state = get_tick (Dual_state.read state)

    let state_hash state = state_hash (Dual_state.read state)

    let get_current_level state =
      let open Lwt_syntax in
      let+ level = get_current_level (Dual_state.read state) in
      Option.map Raw_level.to_int32 level

    let get_outbox level state =
      get_outbox (Raw_level.of_int32_exn level) (Dual_state.read state)

    let get_status ~is_reveal_enabled state =
      get_status ~is_reveal_enabled (Dual_state.read state)

    let set_initial_state ~empty =
      let open Lwt_syntax in
      let+ state = initial_state ~empty:(Dual_state.read empty) in
      Dual_state.write empty state

    let install_boot_sector state boot_sector =
      let open Lwt_syntax in
      let+ new_state =
        install_boot_sector (Dual_state.read state) boot_sector
      in
      Dual_state.write state new_state

    let is_input_state ~is_reveal_enabled state =
      is_input_state ~is_reveal_enabled (Dual_state.read state)

    let set_input input state =
      let open Lwt_syntax in
      let* imm_state = set_input input (Dual_state.read state) in
      Dual_state.write state imm_state ;
      return_unit

    let eval_many ?check_invalid_kernel ?fallback_to_slow_vm ~reveal_builtins
        ~write_debug ~is_reveal_enabled ?stop_at_snapshot ~max_steps mut_state =
      let open Lwt_syntax in
      let* imm_state, steps =
        eval_many
          ?check_invalid_kernel
          ?fallback_to_slow_vm
          ~reveal_builtins
          ~write_debug
          ~is_reveal_enabled
          ?stop_at_snapshot
          ~max_steps
          (Dual_state.read mut_state)
      in
      Dual_state.write mut_state imm_state ;
      return steps

    module Inspect_durable_state = struct
      let lookup state keys =
        Inspect_durable_state.lookup (Dual_state.read state) keys
    end

    module Internal_for_tests = struct
      let insert_failure state =
        let open Lwt_syntax in
        let* imm_state =
          Internal_for_tests.insert_failure (Dual_state.read state)
        in
        Dual_state.write state imm_state ;
        return_unit
    end
  end

  module Unsafe_patches = struct
    type t = unsafe_patch

    (* Reused verbatim from {!Impl}: depends only on [unsafe_patch]. *)
    let of_patch = Impl.Unsafe_patches.of_patch

    let apply state unsafe_patch =
      let open Lwt_syntax in
      match unsafe_patch with
      | Increase_max_nb_ticks max_nb_ticks ->
          let* registered_max_nb_ticks =
            Wasm_2_0_0_dual_state.Machine.Unsafe.get_max_nb_ticks state
          in
          let max_nb_ticks = Z.of_int64 max_nb_ticks in
          if Z.Compare.(max_nb_ticks < registered_max_nb_ticks) then
            Format.ksprintf
              invalid_arg
              "Decreasing tick limit of WASM PVM from %s to %s is not allowed"
              (Z.to_string registered_max_nb_ticks)
              (Z.to_string max_nb_ticks) ;
          Wasm_2_0_0_dual_state.Machine.Unsafe.set_max_nb_ticks
            max_nb_ticks
            state
      | Patch_durable_storage {key; value} ->
          Wasm_2_0_0_dual_state.Machine.Unsafe.durable_set ~key ~value state
      | Patch_PVM_version {version} ->
          Wasm_2_0_0_dual_state.Machine.Unsafe.set_pvm_version ~version state

    let apply_mutable state patch =
      let open Lwt_syntax in
      let+ patched_state = apply (Dual_state.read state) patch in
      Dual_state.write state patched_state
  end
end
