(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** On-disk implementation of the RISC-V new durable storage.

    All data is persisted to disk via a repository. State survives
    process restarts. This variant is used by the rollup node for
    production storage. *)

open Intf

(** On-disk repository handle.

    A repository is the root of the on-disk storage.  Opening a
    repository gives access to the commit history and allows creating
    or checking out registries. *)
module Repo : sig
  type t

  (** [create path] opens (or creates) a repository at [path]. *)
  val create : string -> t
end

(** Proof objects for the on-disk durable storage.

    {b Note}: Currently a stub (TZX-113). *)
module Proof : PROOF

(** Normal mode: registry and database operations. *)
module Normal : sig
  module Registry : sig
    include REGISTRY

    (** [create repo] creates a new, empty registry backed by [repo]. *)
    val create : Repo.t -> t

    (** [commit registry] persists the current state and returns a
        commit identifier that can be used with {!checkout}. *)
    val commit : t -> bytes

    (** [checkout repo commit_id] restores the registry to the state
        recorded at [commit_id]. *)
    val checkout : Repo.t -> bytes -> t
  end

  include NORMAL with module Registry := Registry
end

(** Prove mode: registry, database, and proof lifecycle.

    {b Note}: Currently a stub (TZX-113). All operations will raise a
    runtime error if called. *)
module Prove :
  PROVE
    with type normal_registry := Normal.Registry.t
     and type Proof.t = Proof.t

(** Verify mode: registry, database, and verify entry point.

    {b Note}: Currently a stub (TZX-114). All operations will raise a
    runtime error if called. *)
module Verify : VERIFY with type proof := Proof.t

(** {2 Typed mode-switching}

    These constructors extend the common {!Nds.tag} type with this
    backend's three mode tags, refining the wrapped registry type to a
    concrete on-disk variant.  They let backend-aware callers recover
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
