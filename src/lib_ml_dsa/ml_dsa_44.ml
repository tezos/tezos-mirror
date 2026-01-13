(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* TODO: This library currently re-exports the bindings to the libcrux_ml_dsa crate. *)
(* It should instead implement a high-level OCaml interface on top of it. *)
include Octez_libcrux_ml_dsa
