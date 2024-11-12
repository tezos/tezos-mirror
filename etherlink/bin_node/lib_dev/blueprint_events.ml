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
    ~msg:"Blueprint publisher is ready"
    ~level:Info
    ()

let publisher_shutdown =
  declare_0
    ~section
    ~name:"blueprint_publisher_shutdown"
    ~msg:"Blueprint publishing is shutting down"
    ~level:Info
    ()

let blueprint_application =
  declare_6
    ~name:"blueprint_application"
    ~section
    ~msg:
      "Applied a blueprint for level {level} at {timestamp} containing \
       {txs_nb} transactions for {gas_used} gas leading to creating block \
       {block_hash} in {process_time}."
    ~level:Notice
    ~pp6:Time.System.Span.pp_hum
    ("level", Data_encoding.n)
    ("timestamp", Time.Protocol.rfc_encoding)
    ("txs_nb", Data_encoding.int31)
    ("gas_used", Data_encoding.n)
    ("block_hash", Ethereum_types.block_hash_encoding)
    ("process_time", Time.System.Span.encoding)

let blueprint_injection =
  declare_1
    ~section
    ~name:"blueprint_injection"
    ~msg:"Injecting a blueprint for level {level}"
    ~level:Info
    ("level", Data_encoding.n)

let blueprint_injection_on_inbox =
  declare_1
    ~section
    ~name:"blueprint_injection_on_inbox"
    ~msg:"Injecting on the shared inbox a blueprint for level {level}"
    ~level:Debug
    ("level", Data_encoding.n)

let blueprint_injection_on_DAL =
  declare_2
    ~section
    ~name:"blueprint_injection_on_DAL"
    ~msg:
      "Injecting on the DAL a blueprint for level {level} containing \
       {nb_chunks} chunks"
    ~level:Debug
    ("level", Data_encoding.n)
    ("nb_chunks", Data_encoding.int31)

let blueprint_injection_failure =
  declare_2
    ~section
    ~name:"blueprint_injection_failure"
    ~msg:"Injecting a blueprint for level {level} failed with {trace}"
    ~pp2:Error_monad.pp_print_trace
    ~level:Error
    ("level", Data_encoding.n)
    ("trace", Error_monad.trace_encoding)

let blueprint_catchup =
  declare_2
    ~section
    ~name:"blueprint_catchup"
    ~msg:"Catching-up from level {min} to {max}"
    ~level:Notice
    ("min", Data_encoding.n)
    ("max", Data_encoding.n)

let blueprint_proposal =
  declare_2
    ~section
    ~name:"blueprint_proposal"
    ~msg:"Crafted a blueprint proposal for level {level} in {process_time}"
    ~level:Debug
    ~pp2:Ptime.Span.pp
    ("level", Data_encoding.n)
    ("process_time", Time.System.Span.encoding)

let blueprint_production =
  declare_2
    ~section
    ~name:"blueprint_production"
    ~msg:"Produced a blueprint for level {level} in {process_time}"
    ~level:Info
    ~pp2:Ptime.Span.pp
    ("level", Data_encoding.n)
    ("process_time", Time.System.Span.encoding)

let invalid_blueprint =
  declare_1
    ~section
    ~name:"blueprint_invalid"
    ~msg:"Produced an invalid blueprint at level {level}"
    ~level:Error
    ("level", Data_encoding.n)

let missing_blueprints =
  declare_3
    ~section
    ~name:"missing_blueprints"
    ~msg:"Store is missing {count} blueprints in the range [{from}; {to_}]"
    ~level:Error
    ("count", Data_encoding.int31)
    ("from", Data_encoding.n)
    ("to_", Data_encoding.n)

let worker_request_failed =
  declare_2
    ~section
    ~name:"request_failed"
    ~msg:"[Warning]: Request {view} failed: {errors}"
    ~level:Warning
    ("view", Blueprints_publisher_types.Request.encoding)
    ~pp1:Blueprints_publisher_types.Request.pp
    ("errors", Error_monad.trace_encoding)
    ~pp2:Error_monad.pp_print_trace

let publisher_is_ready () = emit publisher_ready ()

let publisher_shutdown () = emit publisher_shutdown ()

let blueprint_injected level = emit blueprint_injection level

let blueprint_injected_on_inbox level = emit blueprint_injection_on_inbox level

let blueprint_injected_on_DAL ~level ~nb_chunks =
  emit blueprint_injection_on_DAL (level, nb_chunks)

let blueprint_injection_failed level trace =
  emit blueprint_injection_failure (level, trace)

let blueprint_applied block process_time =
  let open Ethereum_types in
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
      process_time )

let invalid_blueprint_produced level = emit invalid_blueprint level

let catching_up min max = emit blueprint_catchup (min, max)

let missing_blueprints count Ethereum_types.(Qty from) Ethereum_types.(Qty to_)
    =
  emit missing_blueprints (count, from, to_)

let blueprint_proposal Ethereum_types.(Qty level) time =
  emit blueprint_proposal (level, time)

let blueprint_production Ethereum_types.(Qty level) time =
  emit blueprint_production (level, time)

let worker_request_failed request_view errs =
  emit worker_request_failed (request_view, errs)
