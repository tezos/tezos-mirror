(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = Events.section

let publisher_ready =
  declare_0
    ~section
    ~name:"blueprint_publisher_is_ready"
    ~msg:"blueprint publisher is ready"
    ~level:Info
    ()

let publisher_shutdown =
  declare_0
    ~section
    ~name:"blueprint_publisher_shutdown"
    ~msg:"blueprint publishing is shutting down"
    ~level:Info
    ()

let blueprint_application =
  declare_7
    ~name:"blueprint_application"
    ~section
    ~msg:"head is now {level}, applied in {process_time}{timestamp}"
    ~level:Notice
    ~pp2:(fun fmt timestamp ->
      let timestamp = Time.System.of_protocol_exn timestamp in
      if Metrics.is_bootstrapping () then
        let now = Time.System.now () in
        let age = Ptime.diff now timestamp in
        Format.fprintf fmt " (%a old)" Ptime.Span.pp age
      else ())
    ~pp7:Time.System.Span.pp_hum
    ("level", Data_encoding.n)
    ("timestamp", Time.Protocol.rfc_encoding)
    ("txs_nb", Data_encoding.int31)
    ("gas_used", Data_encoding.n)
    ("block_hash", Ethereum_types.block_hash_encoding)
    ("execution_gas", Data_encoding.n)
    ("process_time", Time.System.Span.encoding)

let blueprint_replayed =
  declare_4
    ~section
    ~name:"blueprint_replayed"
    ~msg:
      "blueprint {level} consuming {execution_gas} gas replayed in \
       {process_time} {diverged} divergence"
    ~level:Notice
    ~pp3:Time.System.Span.pp_hum
    ~pp4:(fun fmt diverged ->
      Format.pp_print_string fmt (if diverged then "with" else "without"))
    ("level", Data_encoding.n)
    ("execution_gas", Data_encoding.n)
    ("process_time", Time.System.Span.encoding)
    ("diverged", Data_encoding.bool)

let blueprint_injection =
  declare_1
    ~section
    ~name:"blueprint_injection"
    ~msg:"injecting a blueprint for level {level}"
    ~level:Info
    ("level", Data_encoding.n)

let blueprint_injection_on_inbox =
  declare_1
    ~section
    ~name:"blueprint_injection_on_inbox"
    ~msg:"injecting on the shared inbox a blueprint for level {level}"
    ~level:Debug
    ("level", Data_encoding.n)

let blueprint_injection_on_DAL =
  declare_2
    ~section
    ~name:"blueprint_injection_on_DAL"
    ~msg:
      "injecting on the DAL a blueprint for level {level} containing \
       {nb_chunks} chunks"
    ~level:Debug
    ("level", Data_encoding.n)
    ("nb_chunks", Data_encoding.int31)

let blueprint_injection_failure =
  declare_2
    ~section
    ~name:"blueprint_injection_failure"
    ~msg:"injecting a blueprint for level {level} failed with {trace}"
    ~pp2:Error_monad.pp_print_trace
    ~level:Error
    ("level", Data_encoding.n)
    ("trace", Events.trace_encoding)

let blueprint_catchup =
  declare_2
    ~section
    ~name:"blueprint_catchup"
    ~msg:"catching-up from level {min} to {max}"
    ~level:Notice
    ("min", Data_encoding.n)
    ("max", Data_encoding.n)

let blueprint_production =
  declare_2
    ~section
    ~name:"blueprint_production"
    ~msg:"produced a blueprint for level {level} in {process_time}"
    ~level:Info
    ~pp2:Ptime.Span.pp
    ("level", Data_encoding.n)
    ("process_time", Time.System.Span.encoding)

let invalid_blueprint_produced =
  declare_1
    ~section
    ~name:"blueprint_invalid"
    ~msg:"produced an invalid blueprint at level {level}"
    ~level:Error
    ("level", Data_encoding.n)

let invalid_blueprint_applied =
  declare_1
    ~section
    ~name:"blueprint_invalid_applied"
    ~msg:"failed to apply received blueprint for level {level}"
    ~level:Error
    ("level", Data_encoding.n)

let unexpected_blueprint_from_remote_node =
  declare_2
    ~section
    ~name:"unexpected_blueprint_from_remote_node"
    ~msg:"remote node sent blueprint {number} instead of {expected}"
    ~level:Warning
    ("number", Data_encoding.n)
    ("expected", Data_encoding.n)

let missing_blueprints =
  declare_3
    ~section
    ~name:"missing_blueprints"
    ~msg:"store is missing {count} blueprints in the range [{from}; {to_}]"
    ~level:Error
    ("count", Data_encoding.int31)
    ("from", Data_encoding.n)
    ("to_", Data_encoding.n)

let worker_request_failed =
  declare_2
    ~section
    ~name:"blueprints_publisher_request_failed"
    ~msg:"request {view} failed: {errors}"
    ~level:Error
    ("view", Blueprints_publisher_types.Request.encoding)
    ~pp1:Blueprints_publisher_types.Request.pp
    ("errors", Events.trace_encoding)
    ~pp2:Error_monad.pp_print_trace

let follower_failed =
  declare_1
    ~section
    ~name:"blueprints_follower_failed"
    ~msg:"blueprint follower failed with {trace}"
    ~level:Fatal
    ~pp1:Error_monad.pp_print_trace
    ("trace", Events.trace_encoding)

let publisher_is_ready () = emit publisher_ready ()

let publisher_shutdown () = emit publisher_shutdown ()

let blueprint_injected level = emit blueprint_injection level

let blueprint_injected_on_inbox level = emit blueprint_injection_on_inbox level

let blueprint_injected_on_DAL ~level ~nb_chunks =
  emit blueprint_injection_on_DAL (level, nb_chunks)

let blueprint_injection_failed level trace =
  emit blueprint_injection_failure (level, trace)

let blueprint_applied block execution_gas process_time =
  let open Ethereum_types in
  match block with
  | L2_types.Eth block ->
      let count_txs = function
        | TxHash l -> List.length l
        | TxFull l -> List.length l
      in
      emit
        blueprint_application
        ( Qty.to_z block.number,
          block.timestamp |> Qty.to_z |> Z.to_int64 |> Time.Protocol.of_seconds,
          count_txs block.transactions,
          Qty.to_z block.gasUsed,
          block.hash,
          execution_gas,
          process_time )
  | Tez block ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7866 *)
      emit
        blueprint_application
        ( Z.of_int32 block.level,
          block.timestamp,
          0,
          Z.zero,
          block.hash,
          execution_gas,
          process_time )

let blueprint_replayed ~execution_gas:(Ethereum_types.Qty execution_gas)
    ~process_time ~diverged (Ethereum_types.Qty level) =
  emit blueprint_replayed (level, execution_gas, process_time, diverged)

let invalid_blueprint_produced level = emit invalid_blueprint_produced level

let invalid_blueprint_applied level = emit invalid_blueprint_applied level

let catching_up min max = emit blueprint_catchup (min, max)

let missing_blueprints count Ethereum_types.(Qty from) Ethereum_types.(Qty to_)
    =
  emit missing_blueprints (count, from, to_)

let blueprint_production Ethereum_types.(Qty level) time =
  emit blueprint_production (level, time)

let unexpected_blueprint_from_remote_node ~received:Ethereum_types.(Qty level)
    ~expected:Ethereum_types.(Qty expected) =
  emit unexpected_blueprint_from_remote_node (level, expected)

let worker_request_failed request_view errs =
  emit worker_request_failed (request_view, errs)

let follower_failed trace = emit follower_failed trace
