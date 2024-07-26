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
  declare_2
    ~name:"blueprint_application"
    ~section
    ~msg:
      "Applied a blueprint for level {level} leading to creating block \
       {block_hash}"
    ~level:Notice
    ("level", Data_encoding.n)
    ("block_hash", Ethereum_types.block_hash_encoding)

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
  declare_1
    ~section
    ~name:"blueprint_injection_on_DAL"
    ~msg:"Injecting on the DAL a blueprint for level {level}"
    ~level:Debug
    ("level", Data_encoding.n)

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

let publisher_is_ready () = emit publisher_ready ()

let publisher_shutdown () = emit publisher_shutdown ()

let blueprint_injected level = emit blueprint_injection level

let blueprint_injected_on_inbox level = emit blueprint_injection_on_inbox level

let blueprint_injected_on_DAL level = emit blueprint_injection_on_DAL level

let blueprint_injection_failed level trace =
  emit blueprint_injection_failure (level, trace)

let blueprint_applied (level, hash) = emit blueprint_application (level, hash)

let invalid_blueprint_produced level = emit invalid_blueprint level

let catching_up min max = emit blueprint_catchup (min, max)

let missing_blueprints count Ethereum_types.(Qty from) Ethereum_types.(Qty to_)
    =
  emit missing_blueprints (count, from, to_)

let blueprint_proposal Ethereum_types.(Qty level) time =
  emit blueprint_proposal (level, time)

let blueprint_production Ethereum_types.(Qty level) time =
  emit blueprint_production (level, time)
