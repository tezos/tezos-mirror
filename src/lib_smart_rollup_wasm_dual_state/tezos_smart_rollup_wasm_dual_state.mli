(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025-2026 Nomadic Labs <contact@nomadic-labs.com> *)
(*                                                                           *)
(*****************************************************************************)

(** Dual-storage WASM PVM backend, parameterized over an Irmin durable
    ({!Tezos_scoru_wasm.Wasm_pvm_sig.STATE_PROOF}) and an NDS
    implementation ({!NDS_BACKEND}).

    Pairs the Irmin durable with an NDS handle.  The phase model, the
    [/pvm/nds_hash] marker, the [Irmin_only] / [Dual] proof
    dispatch, and the soundness cross-checks are defined here,
    independent of both backends. *)

(** Operations a concrete NDS backend must provide; everything else in
    the dual-state logic is backend-agnostic. *)
module type NDS_BACKEND = sig
  (** NDS proofs: (de)serialisation plus the start/stop hashes
      cross-checked against the [/pvm/nds_hash] marker. *)
  module Proof : Octez_riscv_nds_common.Intf.PROOF

  (** An in-progress [Prove]-mode session.  An NDS handle runs in one of
      three modes: [Normal] during live kernel execution, [Prove] to
      record a step's host-function operations, and [Verify] to replay
      them against a proof.  {!open_prove_session} opens this session;
      {!produce_proof} seals it into a {!Proof.t}. *)
  type prove_session

  (** [open_prove_session nds] switches the live [Normal]-mode handle
      [nds] into [Prove] mode, returning the {!prove_session} and the
      [Prove]-mode handle to install in the state.  Running the step
      against that handle records its host-function calls into the
      session; {!produce_proof} then seals them into the proof.  Raises
      [Invalid_argument] if [nds] is not in [Normal] mode — only
      kernel-driven execution should hold a non-[Normal] handle. *)
  val open_prove_session :
    Octez_riscv_nds_common.Nds.t -> prove_session * Octez_riscv_nds_common.Nds.t

  (** Mint the NDS proof at the end of an {!open_prove_session}. *)
  val produce_proof : prove_session -> Proof.t

  (** [open_verify_session proof] opens a [Verify]-mode session against
      [proof], returning the [Verify]-mode handle to install in the state;
      replaying the step against it checks each host-function call against
      [proof]. *)
  val open_verify_session : Proof.t -> Octez_riscv_nds_common.Nds.t

  (** [copy nds] returns a handle independent of [nds]: mutations to
      either handle are invisible to the other; both stay backed by the
      same underlying store.  Copy-on-write — at most one lazy copy of
      the registry state, deferred to the next mutation of either side.

      Only [Normal]-mode handles can be copied — [Prove]/[Verify]
      handles are transient in-step sessions and never live in a state
      at rest.  Raises [Invalid_argument] otherwise (same precedent as
      {!open_prove_session}). *)
  val copy : Octez_riscv_nds_common.Nds.t -> Octez_riscv_nds_common.Nds.t
end

(** Content hash of an empty NDS registry — the expected
    [/pvm/nds_hash] marker on the (host-function-free) activation tick.

    A constant of the NDS hashing scheme, not of any backend: a proof
    produced against one backend is verified against another (the
    rollup node proves on disk, the protocol verifies in memory), so
    the activation-tick cross-checks only make sense if every backend
    hashes the empty registry identically.  Defined once here rather
    than required from each {!NDS_BACKEND}. *)
val empty_registry_hash : bytes

(** [Make (Irmin) (Backend)] specialises the dual-state WASM PVM to a
    concrete Irmin durable and NDS [Backend].

    {1 State}

    [state] is a record [{irmin; nds}].  The encoder keeps the halves
    consistent by writing the active registry's hash to the durable at
    [/pvm/nds_hash] on every [Dual] encode, so [state_hash] (the
    Irmin tree's hash) already commits to the NDS state — no external
    composition.  Pre-activation states never write the path. *)
module Make
    (Irmin : sig
      include Tezos_scoru_wasm.Wasm_pvm_sig.STATE_PROOF

      (** The compact encoding underlying [proof_encoding] (which must
          be its [Compact.make ~tag_size:`Uint8]).  The dual machine
          joins its tag space in a [Compact.union] so [Irmin_only]
          proofs stay byte-identical to bare Irmin proofs. *)
      val proof_compact_encoding : proof Data_encoding.Compact.t

      val find_value : state -> string list -> bytes option Lwt.t

      val set_value : state -> string list -> bytes -> state Lwt.t
    end)
    (Backend : NDS_BACKEND) : sig
  module Dual_state : sig
    (** Activation tag carried alongside the Irmin tree: [Inactive] is
        pre-activation, [Active nds] carries the live NDS handle. *)
    type nds_state = Inactive | Active of Octez_riscv_nds_common.Nds.t

    (** The dual state: the Irmin durable tree paired with the NDS
        activation tag.

        {b Beware}: an [Active] handle is intrinsically mutable, so a
        [state] value is only as immutable as the discipline around it.
        Values obtained from {!to_imm} are genuine snapshots (the handle
        is severed); values built by hand share whatever handle was put
        in them. *)
    type state = {irmin : Irmin.state; nds : nds_state}

    (** Mutable counterpart of [state], for in-place PVM evaluation:
        [irmin] is replaced whole on every step (the tree is
        persistent); [nds] is replaced on the activation flip and
        mutated through the live handle during kernel execution. *)
    type mut_state = {mutable irmin : Irmin.state; mutable nds : nds_state}

    (** [to_imm m] snapshots [m]: the [Active] NDS handle is severed
        with {!NDS_BACKEND.copy}, so later evaluation through [m] leaves
        the snapshot unchanged.  Copy-on-write, at most one lazy copy. *)
    val to_imm : mut_state -> state

    (** [from_imm s] is a fresh mutable state seeded from [s]; the
        [Active] NDS handle is severed with {!NDS_BACKEND.copy}, so
        evaluation through the result leaves [s] unchanged. *)
    val from_imm : state -> mut_state

    (** [read m] is [m]'s current state {b without} severing the NDS
        handle — the result aliases [m]'s live handle.  For the PVM's
        mutable-state wrapper (read, evaluate, {!write} back); use
        {!to_imm} for snapshots. *)
    val read : mut_state -> state

    (** [write m s] installs [s] into [m] (both fields, no copy) —
        including the [Inactive -> Active] activation flip, which cannot
        propagate through handle aliasing since [Inactive] carries no
        handle. *)
    val write : mut_state -> state -> unit

    (** Durable path of the NDS marker, relative to the PVM state root.
        Consumers reconstructing the NDS half from a persisted state
        (e.g. a context backend restoring on checkout) read the marker
        at this path. *)
    val nds_hash_path : string list

    (** Composite proof: an Irmin proof of the durable transition plus an
        NDS proof of the step's host-function operations. *)
    type dual_proof = private {
      irmin_proof : Irmin.proof;
      nds_proof : Backend.Proof.t;
          (** Its [start_state] / [stop_state] are cross-checked against
              the [/pvm/nds_hash] marker before and after the
              step. *)
    }

    (** Per-step proof variant: [Irmin_only] for steps with no NDS
        operations (pre-activation and the activation tick), [Dual] for
        post-activation steps.

        {b Proof model (per step)}

        {t
          | Pre-state | Post-state     | Variant      | NDS proof                       |
          |-----------|----------------|--------------|---------------------------------|
          | Inactive  | Inactive       | [Irmin_only] | none                            |
          | Inactive  | Active (fresh) | [Irmin_only] | none, uses the empty nds hash   |
          | Active    | Active         | [Dual]       | Prove session over the registry |
        }

        Deactivation ([Active -> Inactive]) is forbidden and rejected by
        the encoder.  The activation tick is [Irmin_only] because its only
        NDS effect — a fresh empty registry — is reconstructible from the
        [/pvm/nds_hash] marker the Irmin proof already attests to.

        {b Soundness}

        - [Irmin_only]: the Irmin proof attests to the durable transition;
          the verifier reads [/pvm/nds_hash] from the post-tree to recover
          the NDS phase (absent ⇒ [Inactive]; empty-registry hash ⇒
          [Active(fresh)]; anything else ⇒ rejected).
        - [Dual]: the [Verify]-mode replay checks each host-function call
          against the proof, and the proof's start/stop hashes are
          cross-checked against the pre/post [/pvm/nds_hash] marker.

        {b Wire format}

        [proof_encoding] is a [Compact.union] joining the tag space of
        the bare Irmin proof compact: an [Irmin_only] proof is
        byte-identical to a bare [Irmin.proof_encoding] — pre-activation,
        single-state and dual-state nodes exchange proofs freely — while
        a [Dual] proof claims a tag-byte value no bare proof uses, so a
        legacy decoder rejects it outright.  Like the bare encoding it
        extends, the union is [`Dynamic]-classified and composes inside
        consumers' [obj]s (e.g. the protocol machine's output-proof
        encoding). *)
    type proof = private Irmin_only of Irmin.proof | Dual of dual_proof

    include
      Tezos_scoru_wasm.Wasm_pvm_sig.STATE_PROOF
        with type state := state
         and type context = Irmin.context
         and type proof := proof

    module Internal_for_tests : sig
      val insert_failure : state -> state Lwt.t

      (** Build a [Dual] proof from its halves. *)
      val make_dual_proof :
        irmin_proof:Irmin.proof -> nds_proof:Backend.Proof.t -> proof

      (** Stamp the NDS-hash marker into the Irmin state, for tests that
          set up an [Active] state's marker directly. *)
      val write_nds_hash : Irmin.state -> bytes -> Irmin.state Lwt.t

      (** Read the NDS-hash marker from the Irmin state, or [None] when
          the [/pvm/nds_hash] path is unset (a pre-activation state). *)
      val read_nds_hash : Irmin.state -> bytes option Lwt.t
    end
  end

  (** [wasm_pvm_machine_dual ~config ~make_empty_nds] returns a WASM
      PVM machine over {!Dual_state}.

      [make_empty_nds] is the activation-boundary factory: the
      [Normal]-mode handle the VM installs when the kernel opens the
      NDS gate.  It is a runtime argument (not part of {!NDS_BACKEND})
      because it is the only place a backend's store handle enters the
      machine — e.g. the on-disk backend closes it over the rocksdb
      repository the node opened at startup. *)
  val wasm_pvm_machine_dual :
    config:Tezos_scoru_wasm.Wasm_pvm_config.t ->
    make_empty_nds:(unit -> Octez_riscv_nds_common.Nds.t) ->
    (module Tezos_scoru_wasm.Wasm_pvm_sig.S
       with type context = Dual_state.context
        and type state = Dual_state.state
        and type proof = Dual_state.proof)
end
