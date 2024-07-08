(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Internal_event.Simple

let section = Events.section

let started =
  declare_0
    ~section
    ~name:"threshold_encryption_preblocks_monitor_started"
    ~msg:"Threshold encryption preblocks monitor has been started."
    ~level:Notice
    ()

let shutdown =
  declare_0
    ~section
    ~name:"shutting_down_threshold_encryption_preblocks_monitor"
    ~msg:"Stopping the Threshold encryption preblocks monitor."
    ~level:Notice
    ()

let received_preblock =
  declare_1
    ~section
    ~name:"received_preblock"
    ~msg:
      "Received a new preblock for blueprint number {number} from the DSN node."
    ~level:Debug
    ("number", Ethereum_types.quantity_encoding)

let started = emit started

let shutdown = emit shutdown

let received_preblock n = emit received_preblock n
