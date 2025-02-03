(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  open Internal_event.Simple

  let section = Events.section

  let disabled =
    declare_0
      ~section
      ~name:"rollup_node_follower_disabled"
      ~msg:"rollup node follower is disabled"
      ~level:Notice
      ()

  let started =
    declare_0
      ~section
      ~name:"rollup_node_follower_started"
      ~msg:"rollup node follower has been started"
      ~level:Notice
      ()

  let new_block =
    declare_1
      ~section
      ~name:"rollup_node_follower_new_block"
      ~msg:"rollup node follower detected a new block (level {block})"
      ~level:Info
      ("block", Data_encoding.int32)

  let shutdown =
    declare_0
      ~section
      ~name:"shutting_down_rollup_node_follower"
      ~msg:"stopping the rollup node follower"
      ~level:Notice
      ()

  let connection_acquired =
    declare_0
      ~section
      ~name:"rollup_node_follower_connection_acquired"
      ~msg:"rollup node follower connected to the rollup node"
      ~level:Info
      ()

  let connection_lost =
    declare_0
      ~section
      ~name:"rollup_node_follower_connection_lost"
      ~msg:"connection with the rollup node has been lost"
      ~level:Error
      ()

  let trying_reconnection =
    declare_1
      ~section
      ~name:"rollup_node_follower_trying_reconnection"
      ~msg:
        "waiting {duration} seconds before trying to reconnect to the rollup \
         node"
      ~level:Info
      ("duration", Data_encoding.float)

  let connection_failed =
    declare_1
      ~section
      ~name:"rollup_node_follower_connection_failed"
      ~msg:"connection with the rollup node failed with {trace}"
      ~level:Error
      ~pp1:Error_monad.pp_print_trace
      ("trace", Events.trace_encoding)

  let stream_failed =
    declare_1
      ~section
      ~name:"rollup_node_follower_stream_failed"
      ~msg:"stream from the rollup node failed with {trace}"
      ~level:Error
      ~pp1:Error_monad.pp_print_trace
      ("trace", Events.trace_encoding)
end

let disabled = Internal_event.Simple.emit Event.disabled

let started = Internal_event.Simple.emit Event.started

let new_block level = Internal_event.Simple.emit Event.new_block level

let shutdown = Internal_event.Simple.emit Event.shutdown

let connection_lost = Internal_event.Simple.emit Event.connection_lost

let trying_reconnection duration =
  Internal_event.Simple.emit Event.trying_reconnection duration

let connection_acquired = Internal_event.Simple.emit Event.connection_acquired

let connection_failed = Internal_event.Simple.emit Event.connection_failed

let stream_failed = Internal_event.Simple.emit Event.stream_failed
