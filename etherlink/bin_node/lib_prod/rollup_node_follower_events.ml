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
      ~name:"rollup_node_follower_started"
      ~msg:"Rollup node follower has been started"
      ~level:Notice
      ()

  let new_block =
    declare_1
      ~section
      ~name:"rollup_node_follower_new_block"
      ~msg:"Rollup node follower detected a new block (level: {block})"
      ~level:Info
      ("block", Data_encoding.int32)

  let shutdown =
    declare_0
      ~section
      ~name:"shutting_down_rollup_node_follower"
      ~msg:"Stopping the rollup node follower"
      ~level:Notice
      ()

  let connection_lost =
    declare_0
      ~section
      ~name:"rollup_node_follower_connection_lost"
      ~msg:"Connection with the rollup node has been lost"
      ~level:Error
      ()
end

let started = Internal_event.Simple.emit Event.started

let new_block level = Internal_event.Simple.emit Event.new_block level

let shutdown = Internal_event.Simple.emit Event.shutdown

let connection_lost = Internal_event.Simple.emit Event.connection_lost
