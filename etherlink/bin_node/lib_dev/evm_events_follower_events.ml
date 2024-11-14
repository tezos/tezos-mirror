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
      ~level:Debug
      ~pp1:Evm_events.pp
      ("event", Evm_events.encoding)

  let pp_int32 fmt i = Format.fprintf fmt "%ld" i

  let shutdown =
    declare_0
      ~section
      ~name:"shutting_down_evm_events_follower"
      ~msg:"Stopping the evm events follower"
      ~level:Notice
      ()

  let diverged =
    declare_3
      ~section
      ~name:"evm_events_follower_diverged"
      ~msg:
        "The rollup diverged, blueprint {level} leaded to block hash \
         {expected_hash}, but locally has {found_hash}."
      ~level:Error
      ("level", Data_encoding.n)
      ("expected_hash", Ethereum_types.block_hash_encoding)
      ("found_hash", Ethereum_types.block_hash_encoding)

  let upstream_blueprint_applied =
    declare_2
      ~section
      ~name:"evm_events_follower_upstream_blueprint_applied"
      ~msg:
        "The rollup node kernel applied blueprint {level} leading to creating \
         block {hash}."
      ~level:Notice
      ("level", Data_encoding.n)
      ("hash", Ethereum_types.block_hash_encoding)

  let missing_blueprint =
    declare_2
      ~section
      ~name:"evm_events_follower_missing_blueprint"
      ~msg:
        "The rollup diverged, blueprint {level} not found in local state \
         (block hash: {expected_hash})."
      ~level:Error
      ("level", Data_encoding.n)
      ("expected_hash", Ethereum_types.block_hash_encoding)

  let rollup_node_ahead =
    declare_1
      ~section
      ~name:"evm_events_follower_rollup_node_ahead"
      ~msg:"Blueprint {level} was confirmed before we received it."
      ~level:Warning
      ("level", Data_encoding.n)

  let out_of_sync =
    declare_2
      ~section
      ~name:"evm_events_follower_out_of_sync"
      ~msg:
        "Evm node sequencer received finalized level {received} but was \
         expected {expected}"
      ~level:Error
      ("received", Data_encoding.int32)
      ("expected", Data_encoding.int32)
end

let started = Internal_event.Simple.emit Event.started

let shutdown = Internal_event.Simple.emit Event.shutdown

let unreadable_event (index, level) =
  Internal_event.Simple.emit Event.unreadable_event (index, level)

let new_event event = Internal_event.Simple.emit Event.new_event event

let diverged divergence = Internal_event.Simple.emit Event.diverged divergence

let upstream_blueprint_applied level_hash =
  Internal_event.Simple.emit Event.upstream_blueprint_applied level_hash

let missing_blueprint divergence =
  Internal_event.Simple.emit Event.missing_blueprint divergence

let rollup_node_ahead Ethereum_types.(Qty level) =
  Internal_event.Simple.emit Event.rollup_node_ahead level

let out_of_sync ~received ~expected =
  Internal_event.Simple.emit Event.out_of_sync (received, expected)
