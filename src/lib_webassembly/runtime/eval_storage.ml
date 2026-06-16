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

let durable_of = function Durable_only d -> d | Dual {durable; _} -> durable

let durable_only d = Durable_only d

let dual d n = Dual {durable = d; nds = n}

let update_durable storage d =
  match storage with
  | Durable_only _ -> Durable_only d
  | Dual {nds; _} -> Dual {durable = d; nds}

let update_nds storage n =
  match storage with
  | Durable_only _ -> storage
  | Dual {durable; _} -> Dual {durable; nds = n}
