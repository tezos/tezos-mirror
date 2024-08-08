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

let untracking =
  declare_1
    ~section
    ~name:"signal_publisher_untracking"
    ~msg:
      "The injection id {injector_op_hash} is not longer tracked by the signal \
       publisher."
    ~level:Debug
    ("injector_op_hash", Tezos_crypto.Hashed.Injector_operations_hash.encoding)

let commited_or_included_injection_id =
  declare_2
    ~section
    ~name:"signal_publisher_commited_or_included_injector_id"
    ~msg:
      "The injection id {injector_op_hash} has been reported commited or \
       included, and finalized, by the rollup node and published at L1 level \
       {publish_level}."
    ~level:Debug
    ("injector_op_hash", Tezos_crypto.Hashed.Injector_operations_hash.encoding)
    ("publish_level", Data_encoding.int32)

let publisher_is_ready () = emit publisher_ready ()

let publisher_shutdown () = emit publisher_shutdown ()

let tracking ~injector_op_hash ~level ~slot_index =
  emit tracking (injector_op_hash, level, slot_index)

let untracking ~injector_op_hash = emit untracking injector_op_hash

let commited_or_included_injection_id ~injector_op_hash ~published_level =
  emit commited_or_included_injection_id (injector_op_hash, published_level)
