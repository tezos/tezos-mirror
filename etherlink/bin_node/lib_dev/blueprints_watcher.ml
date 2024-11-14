(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let blueprint_watcher : Blueprint_types.with_events Lwt_watcher.input =
  Lwt_watcher.create_input ()

let notify = Lwt_watcher.notify blueprint_watcher

let create_stream () = Lwt_watcher.create_stream blueprint_watcher
