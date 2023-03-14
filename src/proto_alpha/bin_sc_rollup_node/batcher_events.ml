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

let section = ["sc_rollup_node"; "batcher"]

let queue =
  declare_1
    ~section
    ~name:"queue"
    ~msg:"adding {nb_messages} to queue"
    ~level:Notice
    ("nb_messages", Data_encoding.int31)

let batched =
  declare_2
    ~section
    ~name:"batched"
    ~msg:"batched {nb_messages} messages into {nb_batches} batches"
    ~level:Notice
    ("nb_batches", Data_encoding.int31)
    ("nb_messages", Data_encoding.int31)

module Worker = struct
  open Batcher_worker_types

  let section = section @ ["worker"]

  let request_failed =
    declare_3
      ~section
      ~name:"request_failed"
      ~msg:"request {view} failed ({worker_status}): {errors}"
      ~level:Warning
      ("view", Request.encoding)
      ~pp1:Request.pp
      ("worker_status", Worker_types.request_status_encoding)
      ~pp2:Worker_types.pp_status
      ("errors", Error_monad.trace_encoding)
      ~pp3:Error_monad.pp_print_trace

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
end
