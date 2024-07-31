(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Internal_event.Simple

let section = Events.section @ ["signal_publisher"]

let publisher_ready =
  declare_0
    ~section
    ~name:"signal_publisher_is_ready"
    ~msg:"Signal publisher is ready"
    ~level:Info
    ()

let publisher_shutdown =
  declare_0
    ~section
    ~name:"signal_publisher_shutting_down"
    ~msg:"Stopping the signals publisher worker"
    ~level:Info
    ()

let publisher_is_ready () = emit publisher_ready ()

let publisher_shutdown () = emit publisher_shutdown ()
