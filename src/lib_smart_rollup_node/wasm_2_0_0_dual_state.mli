(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>          *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(** Dual-storage instantiation of the WASM PVM: the shared Irmin durable
    {!Wasm_2_0_0_irmin_state} paired with the on-disk NDS backend, through the
    generic dual-state functor. Reusing {!Wasm_2_0_0_irmin_state} as the Irmin
    half means there is no second Irmin backend to keep aligned with the
    protocol's proof format.

    The node-facing [state] is the dual record
    [{irmin : Wasm_2_0_0_irmin_state.state; nds : Dual_state.nds_state}]: the
    context backend {!Dual_context} wraps {!Irmin_context} (which does all the
    storage work) and reconstructs the NDS half from the [/pvm/nds_hash]
    marker on checkout.

    This module is protocol-agnostic; the protocol plugin packages {!Machine}
    into a [Pvm_sig.S] through [Sc_rollup.Wasm_2_0_0PVM.Make_pvm]. *)

(** The dual PVM state and its composite proof. *)
module Dual_state : sig
  (** Activation tag carried alongside the Irmin tree: pre-activation states
      carry no NDS handle, post-activation ones carry the live handle. *)
  type nds_state = Inactive | Active of Octez_riscv_nds_common.Nds.t

  (** Durable path of the NDS marker, relative to the PVM state root: the
      registry hash the encoder stamps and {!Dual_context} checkouts read
      to reconstruct the NDS half. *)
  val nds_hash_path : string list

  (** The node-facing dual state: the Irmin durable tree paired with the NDS
      activation tag.  An [Active] handle is intrinsically mutable — values
      obtained from {!to_imm} are genuine snapshots (the handle is severed);
      values built by hand share whatever handle was put in them. *)
  type state = {irmin : Wasm_2_0_0_irmin_state.state; nds : nds_state}

  (** Mutable counterpart of [state], for in-place PVM evaluation. *)
  type mut_state = {
    mutable irmin : Wasm_2_0_0_irmin_state.state;
    mutable nds : nds_state;
  }

  (** [to_imm m] snapshots [m], severing the NDS handle (copy-on-write):
      later evaluation through [m] leaves the snapshot unchanged. *)
  val to_imm : mut_state -> state

  (** [from_imm s] is a fresh mutable state seeded from [s], severing the
      NDS handle symmetrically to {!to_imm}. *)
  val from_imm : state -> mut_state

  (** [read m] is [m]'s current state {b without} severing the NDS handle —
      for the PVM's read / evaluate / {!write} cycle; use {!to_imm} for
      snapshots. *)
  val read : mut_state -> state

  (** [write m s] installs [s] into [m] (both fields, no copy), including
      the [Inactive -> Active] activation flip. *)
  val write : mut_state -> state -> unit

  (** Per-step composite proof: an Irmin proof of the durable transition plus,
      post-activation, an NDS proof of the step's host-function operations. *)
  type proof
end

(** Opaque handle to the dual context backend's storage: the Irmin repository
    plus the rocksdb NDS repository, opened side by side by
    {!Dual_context.load}. *)
type repo

(** Context backend over [Irmin + NDS]. Delegates all storage work and hashing
    to {!Irmin_context} on the Irmin half — the [/pvm/nds_hash] marker stamped
    by the dual-state encoder makes the Irmin tree commit to the NDS half, so
    hashes, commits and GC are exactly the Irmin ones — and reconstructs the
    NDS handle from that marker on every checkout. *)
module Dual_context :
  Context_sigs.S
    with type repo = repo
     and type state = Dual_state.state
     and type mut_state = Dual_state.mut_state
     and type hash = Irmin_context.hash

(** [make_empty_nds index] mints a fresh empty [Normal]-mode handle backed
    by [index]'s rocksdb repository — the activation-boundary factory the
    kernel-driven NDS activation installs.

    TODO: https://linear.app/tezos/issue/L2-1427
    Currently exercised only by tests; the node wiring will pass it (closed
    over the loaded index) to [wasm_pvm_machine_dual]. *)
val make_empty_nds :
  _ Dual_context.index -> unit -> Octez_riscv_nds_common.Nds.t

(** The fast dual-state WASM PVM machine, over the [Irmin + NDS] dual state.
    Its proof context is the dual context backend's [repo] handle (the proof
    delegates to the Irmin half; the NDS prove session operates on the live
    handle carried by the state). The protocol [Make_pvm] consumer specialises
    [compute_step] to the WASM entrypoint, as it does for the single-state
    PVM.

    Beware: this machine is built from [Fast_pvm_params] — an empty
    [Wasm_pvm_config] and no [make_empty_nds] factory — so it never crosses the
    NDS activation boundary. Its state stays [Inactive] and every proof is
    [Irmin_only] (byte-identical to a bare Irmin tree proof); it is therefore a
    dual-state machine in its types only. Activating the NDS half on a
    kernel request needs a machine built at runtime with
    [wasm_pvm_machine_dual ~config ~make_empty_nds]: a [config] whose
    [Nds_host_functions] gate opens at the activation level, and
    [make_empty_nds index] closed over the loaded index (see L2-1427). *)
module Machine :
  Tezos_scoru_wasm.Wasm_pvm_sig.S
    with type state = Dual_state.state
     and type proof = Dual_state.proof
     and type context = ([`Read | `Write], repo) Context_sigs.raw_index
