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
    ~msg:"signal publisher is ready"
    ~level:Info
    ()

let publisher_shutdown =
  declare_0
    ~section
    ~name:"signal_publisher_shutting_down"
    ~msg:"stopping the signals publisher worker"
    ~level:Info
    ()

let untracking =
  declare_1
    ~section
    ~name:"signal_publisher_untracking"
    ~msg:
      "the injection id {injector_op_hash} is not longer tracked by the signal \
       publisher"
    ~level:Debug
    ("injector_op_hash", Tezos_crypto.Hashed.Injector_operations_hash.encoding)

let commited_or_included_injection_id =
  declare_2
    ~section
    ~name:"signal_publisher_commited_or_included_injector_id"
    ~msg:
      "the injection id {injector_op_hash} has been reported commited or \
       included, and finalized, by the rollup node and published at L1 level \
       {publish_level}"
    ~level:Debug
    ("injector_op_hash", Tezos_crypto.Hashed.Injector_operations_hash.encoding)
    ("publish_level", Data_encoding.int32)

let signal_signed =
  declare_2
    ~section
    ~name:"signal_publisher_signal_signed"
    ~msg:
      "signed the following signals: {signals} (slot_index, published_level) \
       for smart rollup address {smart_rollup_address}"
    ~level:Info
    ( "signals",
      Data_encoding.(
        list (obj2 (req "slot_index" uint8) (req "published_level" int32))) )
    ("smart_rollup_address", Tezos_crypto.Hashed.Smart_rollup_address.encoding)
    ~pp1:
      Format.(
        pp_print_list ~pp_sep:pp_print_space (fun fmt (k, v) ->
            fprintf fmt "(%d, %ld)" k v))

let report_ready_operations =
  declare_2
    ~section
    ~name:"signal_publisher_report_ready_operations"
    ~msg:
      "among {operations} injected DAL operations, {ready_operations} are \
       ready to be signaled"
    ~level:Info
    ("operations", Data_encoding.uint16)
    ("ready_operations", Data_encoding.uint16)

let publisher_is_ready () = emit publisher_ready ()

let publisher_shutdown () = emit publisher_shutdown ()

let untracking ~injector_op_hash = emit untracking injector_op_hash

let commited_or_included_injection_id ~injector_op_hash ~published_level =
  emit commited_or_included_injection_id (injector_op_hash, published_level)

let signal_signed ~signals ~smart_rollup_address =
  emit signal_signed (signals, smart_rollup_address)

let report_ready_operations ~operations ~ready_operations =
  emit report_ready_operations (operations, ready_operations)
