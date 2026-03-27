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

(** Normal mode: registry and database operations. *)
module Normal : sig
  module Registry : sig
    include REGISTRY

    (** [create ()] allocates a new, empty in-memory registry with no
        databases. Use {!resize} to add databases. *)
    val create : unit -> t
  end

  (** Database key-value operations on a {!Registry.t}. See
      {!Octez_riscv_nds_common.DATABASE} for the full specification. *)
  module Database : DATABASE with type registry := Registry.t
end
