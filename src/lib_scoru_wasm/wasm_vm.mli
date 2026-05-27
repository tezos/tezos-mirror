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

open Wasm_pvm_state.Internal_state

module type S = sig
  include Wasm_vm_sig.S

  (** [eval_has_finished tick_state] returns [true] if the evaluation phase
      has finished successfully. *)
  val eval_has_finished : tick_state -> bool

  (** [patch_flags_on_eval_successful durable] clears flags set by
      previous attempted runs of kernel_run. Once an evaluation has
      succeeded, these can be safely deleted. *)
  val patch_flags_on_eval_successful : Durable.t -> Durable.t Lwt.t

  (** [should_compute pvm_state] probes whether it is possible to continue
      with more computational steps. *)
  val should_compute : ?reveal_builtins:Builtins.reveals -> pvm_state -> bool

  (** [has_reboot_flag durable] checks if the reboot flag is set in the
      durable storage. *)
  val has_reboot_flag : Durable.t -> bool Lwt.t

  (** [has_activate_nds_flag durable] checks if the kernel has set the
      sentinel under {!Constants.activate_nds_flag_key} requesting
      activation of the NDS at the next reboot. *)
  val has_activate_nds_flag : Durable.t -> bool Lwt.t

  (** [maybe_activate_nds ~version ~current_level storage] is the
      kernel-driven activation trigger invoked at the [Padding] reboot
      boundary (regardless of whether the kernel requested a reboot, so
      a yield-only kernel activates at that boundary rather than waiting
      for the next level's reboot).  It is a no-op unless all of the
      following hold:
      [storage] is [Durable_only]; [version] is at least [V6] (the PVM
      version that introduces the NDS host functions, matching
      {!Host_funcs.lookup_opt}); the kernel has set the sentinel at
      {!Constants.activate_nds_flag_key}; and the [Nds_host_functions]
      feature is enabled at [current_level] in [Params.config].  When
      it fires, the storage is upgraded to [Dual] using a fresh NDS
      handle built by [Params.make_empty_nds].  The sentinel is left
      in place: once storage is [Dual] its value is never read again,
      so there is nothing to consume and clearing it would only add a
      storage write for no observable effect.  In every other case the
      storage is returned unchanged — also leaving the sentinel intact
      when set so a future PVM with the gates open can still observe
      the kernel's request.

      Once storage is [Dual] the function is a pure no-op with no
      storage access: a sentinel re-written by the kernel after
      activation is ignored, not consumed.  The version and feature
      gates are checked before the sentinel is read, so when NDS is
      unavailable the function touches no storage at all even though it
      runs at every reboot boundary. *)
  val maybe_activate_nds :
    version:Wasm_pvm_state.version ->
    current_level:int32 ->
    pvm_storage ->
    pvm_storage Lwt.t

  (** [mark_for_reboot reboot_counter durable] figures out the computational
      status with respect to what the PVM shall do next. *)
  val mark_for_reboot :
    pvm_state -> [`Forcing_yield | `Reboot | `Yielding] Lwt.t

  (** [next_reboot_counter pvm_state status] computes the next reboot
      counter. *)
  val next_reboot_counter : pvm_state -> computation_status -> Z.t

  (** [save_fallback_kernel durable] stores the current kernel as a fallback
      kernel. *)
  val save_fallback_kernel : Durable.t -> Durable.t Lwt.t
end

(** Per-instance VM parameters: the {!Wasm_pvm_config.t} feature flags
    consulted by the evaluator, and an optional fresh-NDS factory
    invoked at the [Padding -> Snapshot] reboot boundary when the
    kernel's activation sentinel is observed.

    [make_empty_nds = None] is a promise from the caller that the
    [Nds_host_functions] gate never opens for this instantiation
    (typically because [config] never enables it). If that promise
    is broken — the kernel sets the sentinel and the gate is open
    at a reboot boundary — {!maybe_activate_nds} raises rather than
    silently dropping the activation request. *)
module type Params = sig
  val config : Wasm_pvm_config.t

  val make_empty_nds : (unit -> Nds.t) option
end

(** [Make_vm (Params)] is the WASM VM parameterised by a {!Params}. *)
module Make_vm (Params : Params) : S
