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

let tracking =
  declare_3
    ~section
    ~name:"signal_publisher_tracking"
    ~msg:
      "The injection id {injector_op_hash} associated with a blueprint \
       injected for level {level} and slot index {slot_index} is being tracked \
       by the signal publisher."
    ~level:Debug
    ("injector_op_hash", Tezos_crypto.Hashed.Injector_operations_hash.encoding)
    ("level", Data_encoding.z)
    ("slot_index", Data_encoding.int8)

let publisher_is_ready () = emit publisher_ready ()

let publisher_shutdown () = emit publisher_shutdown ()

let tracking ~injector_op_hash ~level ~slot_index =
  emit tracking (injector_op_hash, level, slot_index)
