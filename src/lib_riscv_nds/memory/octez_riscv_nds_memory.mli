(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** In-memory implementation of the RISC-V new durable storage.

    All data is held in RAM with no persistence. State is lost when the
    registry value is garbage collected. This variant is intended for fast
    execution without disk I/O. *)

open Intf

(** Proof objects for the in-memory durable storage. *)
module Proof : PROOF

(** Normal mode: registry and database operations. *)
module Normal : sig
  module Registry : sig
    include REGISTRY

    (** [create ()] allocates a new, empty in-memory registry with no
        databases. Use {!resize} to add databases. *)
    val create : unit -> t

    (** Immutable snapshot of a registry.  Only obtainable through
        {!to_imm}; carries no operations of its own. *)
    type imm

    (** [to_imm registry] captures the current contents of [registry] as
        an immutable snapshot.  Mutations to [registry] after the call
        are not observable through the snapshot.  Copy-on-write: eager
        (one copy now) if [registry] holds unshared state, free if it is
        already shared with another handle, after which the next mutation
        on either side pays the copy. *)
    val to_imm : t -> imm

    (** [from_imm imm] recovers a live registry from a snapshot.  The
        returned registry is independent of every other registry
        recovered from the same snapshot (copy-on-write, as {!to_imm}). *)
    val from_imm : imm -> t

    (** [duplicate registry] is [from_imm (to_imm registry)]: an
        independent registry holding the same contents.  Mutations to
        either handle are invisible to the other.  Copies the registry
        state up to twice for unshared state (once in {!to_imm}, once on
        the duplicate's first mutation); acceptable here as this is not a
        hot path. *)
    val duplicate : t -> t
  end

  include NORMAL with module Registry := Registry
end

(** [make_empty_normal_nds ()] allocates a fresh, empty in-memory
    normal-mode registry and wraps it as an opaque {!Nds.t} handle.
    Convenience factory for callers that need a backend-erased empty
    NDS — most commonly the [make_empty_nds] field of a
    {!Wasm_vm.Make_vm} instantiation. *)
val make_empty_normal_nds : unit -> Nds.t

(** Prove mode: registry, database, and proof lifecycle. *)
module Prove :
  PROVE
    with type normal_registry := Normal.Registry.t
     and type Proof.t = Proof.t

(** Verify mode: registry, database, and verify entry point.

    Operations are replayed against a proof. A divergence from the proof
    (data absent from it) raises {!Nds_errors.Verification_failed}, to be
    caught at the {!Nds.with_verification} boundary. *)
module Verify : VERIFY with type proof := Proof.t

(** {2 Typed mode-switching}

    These constructors extend the common {!Nds.tag} type with this
    backend's three mode tags, refining the wrapped registry type to a
    concrete in-memory variant.  They let backend-aware callers recover
    the concrete {!Registry.t} from an opaque {!Nds.t} via the
    [unwrap_*] helpers below. *)

type _ Nds.tag +=
  | Normal_tag : Normal.Registry.t Nds.tag
  | Prove_tag : Prove.Registry.t Nds.tag
  | Verify_tag : Verify.Registry.t Nds.tag

(** [unwrap_normal nds] returns [Some r] if [nds] was wrapped with
    {!Normal_tag}, [None] otherwise.  Callers that need the concrete
    {!Normal.Registry.t} (e.g. to call {!Prove.start_proof}) use this
    helper to escape the existential. *)
val unwrap_normal : Nds.t -> Normal.Registry.t option

(** [unwrap_prove nds] returns [Some r] if [nds] was wrapped with
    {!Prove_tag}, [None] otherwise. *)
val unwrap_prove : Nds.t -> Prove.Registry.t option

(** [unwrap_verify nds] returns [Some r] if [nds] was wrapped with
    {!Verify_tag}, [None] otherwise. *)
val unwrap_verify : Nds.t -> Verify.Registry.t option
