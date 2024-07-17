(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Trili Tech <contact@trili.tech>              *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

(* Ultimately, prometheus will use the local sources from `prometheus/`.
   For now we still use the opam package (see below, uses of
   `external_lib`). This temporary state is to minimise disruptions
   for other developers and reducing the size of MRs. *)
let prometheus = external_lib "prometheus" V.(exactly "1.2")

let prometheus_app = external_lib "prometheus-app" V.(exactly "1.2")

let prometheus_app_unix = external_sublib prometheus_app "prometheus-app.unix"
