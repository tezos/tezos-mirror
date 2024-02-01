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

let validation_success =
  declare_2
    ~section
    ~name:"validation_success"
    ~msg:"block {block} validated in {worker_status}"
    ~level:Info
    ~pp1:Block_hash.pp
    ~pp2:Worker_types.pp_status_completed
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)

let validation_failure =
  declare_3
    ~section
    ~name:"validation_failed"
    ~msg:"validation of block {block} failed ({worker_status}): {errors}"
    ~level:Notice
    ~pp1:Block_hash.pp
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Error_monad.trace_encoding)

let previously_validated =
  declare_1
    ~section
    ~name:"previously_validated"
    ~msg:"previously validated block {hash} (after pipe)"
    ~level:Debug
    ~pp1:Block_hash.pp
    ("hash", Block_hash.encoding)

let validating_block =
  declare_1
    ~section
    ~name:"validating_block"
    ~msg:"validating block {hash}"
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
    ~level:Notice
    ~pp1:(fun fmt -> Format.fprintf fmt "%li")
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("level", Data_encoding.int32)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Error_monad.trace_encoding)

let validation_failure_after_precheck =
  declare_3
    ~section
    ~name:"validation_failure_after_precheck"
    ~level:Notice
    ~msg:
      "validation of block {block} failed but precheck succeeded, \
       {worker_status}: {errors}"
    ~pp1:Block_hash.pp
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Tezos_rpc.Error.encoding)

let precheck_failure =
  declare_3
    ~section
    ~name:"precheck_failure"
    ~level:Notice
    ~msg:"precheck of block {block} failed, {worker_status}: {errors}"
    ~pp1:Block_hash.pp
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("block", Block_hash.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Tezos_rpc.Error.encoding)

let prechecked_block =
  declare_1
    ~section
    ~name:"prechecked_block"
    ~level:Info
    ~msg:"prechecked block {hash}"
    ~pp1:Block_hash.pp
    ("hash", Block_hash.encoding)

let prechecking_block =
  declare_1
    ~section
    ~name:"prechecking_block"
    ~level:Debug
    ~msg:"prechecking block {hash}"
    ~pp1:Block_hash.pp
    ("hash", Block_hash.encoding)

let could_not_find_context =
  declare_1
    ~section
    ~name:"could_not_find_context"
    ~level:Debug
    ~msg:"could not find context for block {hash}"
    ~pp1:Block_hash.pp
    ("hash", Block_hash.encoding)

let stopping_node_missing_irmin_key =
  declare_0
    ~section
    ~name:"stopping_node_missing_irmin_key"
    ~level:Error
    ~msg:"critical irmin error: stopping the node gracefully."
    ()
