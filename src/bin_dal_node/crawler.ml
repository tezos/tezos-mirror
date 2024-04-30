(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

module Crawler_lib = Octez_crawler.Layer_1

type t = {l1 : Crawler_lib.t}

let start ~name ~chain ~reconnection_delay ?protocols cctxt =
  let open Lwt_syntax in
  let* l1 =
    Crawler_lib.start ~name ~chain ~reconnection_delay ?protocols cctxt
  in
  return {l1}
