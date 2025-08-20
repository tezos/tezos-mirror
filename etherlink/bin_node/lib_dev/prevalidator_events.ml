(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = Events.section @ ["prevalidator"]

let is_ready =
  declare_0
    ~section
    ~name:"prevalidator_is_ready"
    ~msg:"prevalidator is ready"
    ~level:Notice
    ()

let cannot_start =
  declare_0
    ~section
    ~name:"prevalidator_cannot_start"
    ~msg:
      "prevalidator cannot start with current node data dir, will retry later"
    ~level:Warning
    ()

let is_ready = emit is_ready

let cannot_start = emit cannot_start
