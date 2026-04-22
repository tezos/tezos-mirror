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

(** Proof objects for the in-memory durable storage.

    {b Note}: Currently a stub (TZX-113). *)
module Proof : PROOF

(** Normal mode: registry and database operations. *)
module Normal : sig
  module Registry : sig
    include
      REGISTRY
        with type invalid_argument_error := Nds_errors.invalid_argument_error

    (** [create ()] allocates a new, empty in-memory registry with no
        databases. Use {!resize} to add databases. *)
    val create : unit -> t
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
