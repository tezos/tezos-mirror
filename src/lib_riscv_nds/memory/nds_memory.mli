(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** In-memory implementation of the RISC-V new durable storage. *)

type error +=
  | Invalid_argument of
      Octez_riscv_nds_memory_api.Octez_riscv_durable_storage_in_memory_api
      .invalid_argument_error

module Registry : sig
  (** Abstract type representing an in-memory durable storage registry.
      A registry manages a collection of databases. *)
  type t

  (** [size registry] returns the number of databases currently held in
      [registry]. *)
  val size : t -> int64

  (** [resize registry n] adjusts [registry] to contain exactly [n]
      databases. Growing creates new empty databases; shrinking removes
      databases from the end. *)
  val resize : t -> int64 -> unit tzresult

  (** [copy_database registry ~src ~dst] duplicates all contents of the database
      at index [src] to index [dst] in [registry]. The source database remains
      intact. Copying to the same index is a no-op. *)
  val copy_database : t -> src:int64 -> dst:int64 -> unit tzresult

  (** [move_database registry ~src ~dst] transfers all contents of the database
      at index [src] to index [dst] in [registry]. The source database is
      replaced with an empty database. Moving to the same index is a no-op. *)
  val move_database : t -> src:int64 -> dst:int64 -> unit tzresult
end
