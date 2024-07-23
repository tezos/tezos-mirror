(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Trili Tech <contact@trili.tech>              *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

(* Ultimately, cohttp will use the local sources from `cohttp/`.
   For now we still use the opam package (see below, uses of
   `external_lib`). This temporary state is to minimise disruptions
   for other developers and reducing the size of MRs. *)
let cohttp_lwt = external_lib "cohttp-lwt" V.(exactly "5.3.0")

let cohttp_lwt_unix = external_lib "cohttp-lwt-unix" V.(exactly "5.3.0")
