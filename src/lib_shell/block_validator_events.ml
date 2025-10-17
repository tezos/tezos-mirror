(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = ["validator"; "block"]

let validation_and_application_success =
  declare_2
    ~section
    ~name:"validation_and_application_success"
    ~msg:"block {block} validated and applied in {worker_status}"
    ~level:Info
    ~pp1:Block_hash.pp_short
    ~pp2:Worker_types.pp_status_completed
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)

let previously_validated_and_applied =
  declare_1
    ~section
    ~name:"previously_validated_and_applied"
    ~msg:"previously validated and applied block {hash} (after pipe)"
    ~level:Debug
    ~pp1:Block_hash.pp
    ("hash", Block_hash.encoding)

let previously_invalid_block =
  declare_2
    ~section
    ~name:"previously_invalid_block"
    ~msg:"previously invalid block {hash} with: {errors}"
    ~level:Debug
    ~pp1:Block_hash.pp
    ~pp2:pp_print_top_error_of_trace
    ("hash", Block_hash.encoding)
    ("errors", Error_monad.trace_encoding)

let validation_or_application_failure =
  declare_3
    ~section
    ~name:"validation_or_application_failed"
    ~msg:
      "validation or application of block {block} failed ({worker_status}): \
       {errors}"
    ~level:Notice
    ~pp1:Block_hash.pp_short
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Error_monad.trace_encoding)

let application_failure =
  declare_3
    ~section
    ~name:"application_failed"
    ~msg:"application of block {block} failed, {worker_status}: {errors}"
    ~level:Notice
    ~pp1:Block_hash.pp_short
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Error_monad.trace_encoding)

let applying_block =
  declare_1
    ~section
    ~name:"applying_block"
    ~msg:"applying block {hash}"
    ~level:Debug
    ~pp1:Block_hash.pp
    ("hash", Block_hash.encoding)

let preapplication_success =
  declare_2
    ~section
    ~name:"preapplication_success"
    ~msg:"block at level {level} successfully pre-applied in {worker_status}"
    ~level:Notice
    ~pp1:(fun fmt -> Format.fprintf fmt "%li")
    ~pp2:Worker_types.pp_status_completed
    ("level", Data_encoding.int32)
    ("worker_status", Worker_types.request_status_encoding)

let preapplication_failure =
  declare_3
    ~section
    ~name:"preapplication_failed"
    ~msg:
      "pre-application of block at level {level} failed ({worker_status}): \
       {errors}"
    ~level:Warning
    ~pp1:(fun fmt -> Format.fprintf fmt "%li")
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("level", Data_encoding.int32)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Error_monad.trace_encoding)

let application_failure_after_validation =
  declare_3
    ~section
    ~name:"application_failure_after_validation"
    ~level:Warning
    ~msg:
      "application of block {block} failed but validation succeeded, \
       {worker_status}: {errors}"
    ~pp1:Block_hash.pp_short
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Tezos_rpc.Error.encoding)

let validation_failure =
  declare_3
    ~section
    ~name:"validation_failure"
    ~level:Notice
    ~msg:"validation of block {block} failed, {worker_status}: {errors}"
    ~pp1:Block_hash.pp_short
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Tezos_rpc.Error.encoding)

let validation_canceled =
  declare_2
    ~section
    ~name:"validation_canceled"
    ~level:Info
    ~msg:"validation of block {block} canceled, {worker_status}"
    ~pp1:Block_hash.pp_short
    ~pp2:Worker_types.pp_status
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)

let commit_block_failure =
  declare_3
    ~section
    ~name:"commit_block_failure"
    ~level:Notice
    ~msg:"commit of block {block} failed, {worker_status}: {errors}"
    ~pp1:Block_hash.pp_short
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Tezos_rpc.Error.encoding)

let validated_block =
  declare_1
    ~section
    ~name:"validated_block"
    ~level:Info
    ~msg:"validated block {hash}"
    ~pp1:Block_hash.pp_short
    ("hash", Block_hash.encoding)

let validating_block =
  declare_5
    ~section
    ~name:"validating_block"
    ~level:Info
    ~msg:
      "{hash} with level {level}, parent {parent}, fitness {fitness}, \
       timestamp {timestamp}"
    ~pp1:Block_hash.pp
    ("hash", Block_hash.encoding)
    ("level", Data_encoding.int32)
    ~pp3:Block_hash.pp_short
    ("parent", Block_hash.encoding)
    ~pp4:Fitness.pp
    ("fitness", Fitness.encoding)
    ~pp5:Time.Protocol.pp_hum
    ("timestamp", Time.Protocol.encoding)

let could_not_find_context =
  declare_1
    ~section
    ~name:"could_not_find_context"
    ~level:Debug
    ~msg:"could not find context for block {hash}"
    ~pp1:Block_hash.pp_short
    ("hash", Block_hash.encoding)

let context_error_at_block_application =
  declare_2
    ~section
    ~name:"context_error_at_block_application"
    ~msg:"Application of block {hash} failed on context error: {error}"
    ~level:Warning
    ~pp1:Block_hash.pp_short
    ~pp2:Error_monad.pp_print_trace
    ("hash", Block_hash.encoding)
    ("error", Error_monad.trace_encoding)

let retry_block_application =
  declare_1
    ~section
    ~name:"retry_block_application"
    ~msg:"retry block {hash} application"
    ~level:Notice
    ~pp1:Block_hash.pp_short
    ("hash", Block_hash.encoding)

let stopping_node_missing_context_key =
  declare_0
    ~section
    ~name:"stopping_node_missing_context_key"
    ~level:Error
    ~msg:"critical context error: stopping the node gracefully."
    ()
