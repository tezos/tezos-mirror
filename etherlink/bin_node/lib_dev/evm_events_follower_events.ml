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
      ~msg:"evm events follower has been started"
      ~level:Notice
      ()

  let unreadable_event =
    declare_2
      ~section
      ~name:"evm_events_unreadable_event"
      ~msg:"evm events follower could not parse event {index} of level {level}"
      ~level:Error
      ("index", Data_encoding.int31)
      ("level", Data_encoding.int32)

  let new_event =
    declare_1
      ~section
      ~name:"evm_events_new_event"
      ~msg:"{event}"
      ~level:Debug
      ~pp1:Evm_events.pp
      ("event", Evm_events.encoding)

  let pp_int32 fmt i = Format.fprintf fmt "%ld" i

  let shutdown =
    declare_0
      ~section
      ~name:"shutting_down_evm_events_follower"
      ~msg:"stopping the evm events follower"
      ~level:Notice
      ()

  let diverged =
    declare_3
      ~section
      ~name:"evm_events_follower_diverged"
      ~msg:
        "rollup node diverged on level {level}, confirmed {expected_hash} \
         instead of {found_hash}"
      ~level:Error
      ("level", Data_encoding.n)
      ("expected_hash", Ethereum_types.block_hash_encoding)
      ("found_hash", Ethereum_types.block_hash_encoding)

  let upstream_blueprint_applied =
    declare_2
      ~section
      ~name:"evm_events_follower_upstream_blueprint_applied"
      ~msg:"rollup node confirmed block {level}"
      ~level:Notice
      ("level", Data_encoding.n)
      ("hash", Ethereum_types.block_hash_encoding)

  let missing_blueprint =
    declare_2
      ~section
      ~name:"evm_events_follower_missing_blueprint"
      ~msg:"rollup node diverged at {level}, {expected_hash} not found locally"
      ~level:Error
      ("level", Data_encoding.n)
      ("expected_hash", Ethereum_types.block_hash_encoding)

  let rollup_node_ahead =
    declare_1
      ~section
      ~name:"evm_events_follower_rollup_node_ahead"
      ~msg:"rollup node confirmed block {level} before we received it"
      ~level:Warning
      ("level", Data_encoding.n)

  let out_of_sync =
    declare_2
      ~section
      ~name:"evm_events_follower_out_of_sync"
      ~msg:
        "rollup node confimed block {received}, but we have applied block \
         {expected}"
      ~level:Error
      ("received", Data_encoding.int32)
      ("expected", Data_encoding.int32)

  let worker_request_failed =
    declare_2
      ~section
      ~name:"evm_events_request_failed"
      ~msg:"request {view} failed: {errors}"
      ~level:Warning
      ("view", Evm_events_follower_types.Request.encoding)
      ~pp1:Evm_events_follower_types.Request.pp
      ("errors", Events.trace_encoding)
      ~pp2:Error_monad.pp_print_trace

  let unexpected_key =
    declare_1
      ~section
      ~name:"evm_events_follower_unexpected_key"
      ~msg:"unexpected key in {key} /evm/events"
      ~level:Warning
      ("key", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let unexpected_number_of_events =
    declare_2
      ~section
      ~name:"evm_events_follower_unexpected_number_of_events"
      ~msg:
        "unexpected number of events in /evm/events, fetched {fetched}, \
         expected {expected}"
      ~level:Warning
      ("expected", Data_encoding.int31)
      ("fetched", Data_encoding.int31)

  let rollup_level_already_processed =
    declare_1
      ~section
      ~name:"evm_events_follower_rollup_level_already_processed"
      ~msg:"rollup node level {level} was already processed, skipping it"
      ~level:Info
      ("level", Data_encoding.int32)

  let fallback =
    declare_1
      ~section
      ~name:"evm_events_follower_fallback"
      ~msg:
        "rollup node does not support fetching all events, falling back to \
         fetching one event per RPC because of {error}"
      ~level:Warning
      ("error", Events.trace_encoding)
      ~pp1:pp_print_top_error_of_trace

  let event_flush_delayed_inbox =
    declare_2
      ~section
      ~name:"flush_delayed_inbox"
      ~msg:
        "rollup node flushed the delayed inbox at level {level} ({timestamp})"
      ~level:Notice
      ("timestamp", Time.Protocol.encoding)
      ("level", Data_encoding.n)
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

let worker_request_failed request_view errs =
  Internal_event.Simple.emit Event.worker_request_failed (request_view, errs)

let unexpected_key key = Internal_event.Simple.emit Event.unexpected_key key

let unexpected_number_of_events ~expected ~fetched =
  Internal_event.Simple.emit
    Event.unexpected_number_of_events
    (expected, fetched)

let rollup_level_is_already_processed rollup_level =
  Internal_event.Simple.emit Event.rollup_level_already_processed rollup_level

let fallback e = Internal_event.Simple.emit Event.fallback e

let flush_delayed_inbox ~timestamp Ethereum_types.(Qty level) =
  Internal_event.Simple.emit Event.event_flush_delayed_inbox (timestamp, level)
