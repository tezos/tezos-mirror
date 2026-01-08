(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let data_dir_element = Sigs.data_dir_element

module Lwt = Lwt_metrics
module Unix = Unix_metrics

(** Default to Lwt implementation (not compatible with OCaml5 domains). *)
include Lwt
