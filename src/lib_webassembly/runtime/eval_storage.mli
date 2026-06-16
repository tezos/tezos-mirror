(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Storage context for the WASM interpreter's eval chain.

    [Durable_only] provides only the Irmin-backed durable storage.
    [Dual] additionally carries a New Durable Storage handle. *)
type t =
  | Durable_only of Durable_storage.t
  | Dual of {durable : Durable_storage.t; nds : Octez_riscv_nds_common.Nds.t}

(** Extract the durable storage from either variant. *)
val durable_of : t -> Durable_storage.t

(** Construct a [Durable_only] eval storage. *)
val durable_only : Durable_storage.t -> t

(** Construct a [Dual] eval storage. *)
val dual : Durable_storage.t -> Octez_riscv_nds_common.Nds.t -> t

(** Replace the durable storage, preserving the variant. *)
val update_durable : t -> Durable_storage.t -> t

(** Replace the NDS handle. No-op for [Durable_only]. *)
val update_nds : t -> Octez_riscv_nds_common.Nds.t -> t
