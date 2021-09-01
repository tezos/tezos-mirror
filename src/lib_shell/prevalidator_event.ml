(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Request = Prevalidator_worker_state.Request

let section = ["prevalidator"]

include Internal_event.Simple

let request_failed =
  declare_3
    ~section
    ~name:"request_failed"
    ~msg:"request {view} failed ({worker_status}): {errors}"
    ~level:Notice
    ( "view",
      (* We use [Data_encoding.dynamic_size] because the
         [Request.encoding] is of type [variable length] while the
         other encodings are
         [Data_encoding.dynamic_Size]. [Data_encoding] requires us to
         cast the first one explicitely. *)
      Data_encoding.dynamic_size Request.encoding )
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Error_monad.trace_encoding)

let invalid_mempool_filter_configuration =
  declare_0
    ~section
    ~name:"invalid_mempool_filter_configuration"
    ~msg:"invalid mempool filter configuration"
    ~level:Debug
    ()

let unparsable_operation =
  declare_1
    ~section
    ~name:"unparsable_operation"
    ~msg:"unparsable operation {oph}"
    ~level:Debug
    ("oph", Operation_hash.encoding)

let processing_n_operations =
  declare_1
    ~section
    ~name:"processing_n_operations"
    ~msg:"processing {count} operations"
    ~level:Debug
    ("count", Data_encoding.int31)

let fetching_operation =
  declare_1
    ~section
    ~name:"fetching_operation"
    ~msg:"fetching operation"
    ~level:Debug
    ("oph", Operation_hash.encoding)

let operation_included =
  declare_1
    ~section
    ~name:"operation_included"
    ~msg:"operation {oph} included before being prevalidated"
    ~level:Debug
    ("oph", Operation_hash.encoding)

let operations_not_flushed =
  declare_1
    ~section
    ~name:"operations_not_flushed"
    ~msg:"{count} operations were not washed by the flush"
    ~level:Debug
    ("count", Data_encoding.int31)

let request_completed_notice =
  declare_2
    ~section
    ~name:"request_completed_notice"
    ~msg:"{view} {worker_status}"
    ~level:Notice
    ("view", Request.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ~pp1:Request.pp
    ~pp2:Worker_types.pp_status

(* FIXME https://gitlab.com/tezos/tezos/-/issues/1266

   The level duplication is an intermediate solution. Those events are
   "worker" related and should be handled properly by the worker.

   To do so, the level should be associated directly with the request
   view instead. *)
let request_completed_debug =
  declare_2
    ~section
    ~name:"request_completed_debug"
    ~msg:"{view} {worker_status}"
    ~level:Debug
    ("view", Request.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ~pp1:Request.pp
    ~pp2:Worker_types.pp_status

let ban_operation_encountered =
  declare_2
    ~section
    ~name:"banned_operation_encountered"
    ~msg:"{origin}: banned {oph} encountered"
    ~level:Notice
    ("origin", Data_encoding.string)
    ("oph", Operation_hash.encoding)

let operation_not_fetched =
  declare_1
    ~section
    ~name:"operation_not_fetched"
    ~msg:"Operation {oph} was not fetched"
    ~level:Debug
    ("oph", Operation_hash.encoding)
