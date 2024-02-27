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

let blueprint_injection_failure =
  declare_1
    ~section
    ~name:"blueprint_injection_failure"
    ~msg:"Injecting a blueprint for level {level} failed"
    ~level:Error
    ("level", Data_encoding.n)

let blueprint_catchup =
  declare_2
    ~section
    ~name:"blueprint_catchup"
    ~msg:"Catching-up from level {min} to {max}"
    ~level:Notice
    ("min", Data_encoding.n)
    ("max", Data_encoding.n)

let invalid_blueprint =
  declare_1
    ~section
    ~name:"blueprint_invalid"
    ~msg:"Produced an invalid blueprint at level {level}"
    ~level:Error
    ("level", Data_encoding.n)

let missing_blueprint =
  declare_1
    ~section
    ~name:"blueprint_blueprint"
    ~msg:"Could not fetch the blueprint for level {level}"
    ~level:Error
    ("level", Data_encoding.n)

let publisher_is_ready () = emit publisher_ready ()

let publisher_shutdown () = emit publisher_shutdown ()

let blueprint_injected level = emit blueprint_injection level

let blueprint_injection_failed level = emit blueprint_injection_failure level

let blueprint_applied (level, hash) = emit blueprint_application (level, hash)

let invalid_blueprint_produced level = emit invalid_blueprint level

let catching_up min max = emit blueprint_catchup (min, max)

let missing_blueprint level = emit missing_blueprint level
