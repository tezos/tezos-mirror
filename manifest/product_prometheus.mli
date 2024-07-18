(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Trili Tech <contact@trili.tech>              *)
(*                                                                           *)
(*****************************************************************************)

val product_source : string list

val prometheus : Manifest.target

val prometheus_app : Manifest.target

val prometheus_app_unix : Manifest.target
