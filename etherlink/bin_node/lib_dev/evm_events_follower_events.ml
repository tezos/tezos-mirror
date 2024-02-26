(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  open Internal_event.Simple

  let section = Events.section

  let started =
    declare_0
      ~section
      ~name:"evm_events_follower_started"
      ~msg:"Evm events follower has been started"
      ~level:Notice
      ()

  let unreadable_event =
    declare_2
      ~section
      ~name:"evm_events_unreadable_event"
      ~msg:"Evm events follower could not parse event {index} of level {level}"
      ~level:Error
      ("index", Data_encoding.int31)
      ("level", Data_encoding.int32)

  let new_event =
    declare_1
      ~section
      ~name:"evm_events_new_event"
      ~msg:"Evm events follower: applying {event}"
      ~level:Notice
      ~pp1:Ethereum_types.Evm_events.pp
      ("event", Ethereum_types.Evm_events.encoding)

  let pp_int32 fmt i = Format.fprintf fmt "%ld" i

  let shutdown =
    declare_0
      ~section
      ~name:"shutting_down_evm_events_follower"
      ~msg:"Stopping the evm events follower"
      ~level:Notice
      ()
end

let started = Internal_event.Simple.emit Event.started

let shutdown = Internal_event.Simple.emit Event.shutdown

let unreadable_event (index, level) =
  Internal_event.Simple.emit Event.unreadable_event (index, level)

let new_event event = Internal_event.Simple.emit Event.new_event event
