(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Trili Tech <contact@trili.tech>              *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

(* Ultimately, resto will use the local sources from `resto/`.
   For now we still use the opam package (see below, uses of
   `external_lib`). This temporary state is to minimise disruptions
   for other developers and reducing the size of MRs. *)
let resto_version = V.(exactly "1.2")

let resto = external_lib ~js_compatible:true "resto" resto_version

let resto_acl = external_lib "resto-acl" resto_version

let resto_cohttp = external_lib "resto-cohttp" resto_version

let resto_cohttp_client = external_lib "resto-cohttp-client" resto_version

let resto_cohttp_self_serving_client =
  external_lib "resto-cohttp-self-serving-client" resto_version

let resto_cohttp_server = external_lib "resto-cohttp-server" resto_version

let resto_directory =
  external_lib ~js_compatible:true "resto-directory" resto_version
