(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Trili Tech <contact@trili.tech>              *)
(*                                                                           *)
(*****************************************************************************)

val product_source : string list

val resto : Manifest.target

val resto_directory : Manifest.target

val resto_cohttp : Manifest.target

val resto_cohttp_client : Manifest.target

val resto_cohttp_server : Manifest.target

val resto_acl : Manifest.target

val resto_cohttp_self_serving_client : Manifest.target
