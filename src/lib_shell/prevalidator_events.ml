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
    ~msg:"request {view} failed {worker_status}: {errors}"
    ~level:Notice
    ~pp1:Request.pp
    ~pp2:Worker_types.pp_status
    ~pp3:Error_monad.pp_print_trace
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
    ~level:Warning
    ()

let unparsable_operation =
  declare_1
    ~section
    ~name:"unparsable_operation"
    ~msg:"unparsable operation {oph}"
    ~level:Debug
    ~pp1:Operation_hash.pp
    ("oph", Operation_hash.encoding)

let processing_operations =
  declare_0
    ~section
    ~name:"processing_operations"
    ~msg:"processing operations"
    ~level:Debug
    ()

let fetching_operation =
  declare_1
    ~section
    ~name:"fetching_operation"
    ~msg:"fetching operation {oph}"
    ~level:Debug
    ~pp1:Operation_hash.pp
    ("oph", Operation_hash.encoding)

let operation_included =
  declare_1
    ~section
    ~name:"operation_included"
    ~msg:"operation {oph} included before being prevalidated"
    ~level:Debug
    ~pp1:Operation_hash.pp
    ("oph", Operation_hash.encoding)

let operations_to_reclassify =
  declare_1
    ~section
    ~name:"operations_to_reclassify"
    ~msg:"{count} operations set to be reeclassified after the flush"
    ~level:Debug
    ~pp1:Format.pp_print_int
    ("count", Data_encoding.int31)

let operation_reclassified =
  declare_1
    ~section
    ~name:"operation_reclassified"
    ~msg:"operation {oph} reclassified"
    ~level:Debug
    ~pp1:Operation_hash.pp
    ("oph", Operation_hash.encoding)

let operation_injected =
  declare_1
    ~section
    ~name:"operation_injected"
    ~msg:"operation {oph} injected "
    ~level:Notice
    ~pp1:Operation_hash.pp
    ("oph", Operation_hash.encoding)

let operation_banned =
  declare_1
    ~section
    ~name:"operation_banned"
    ~msg:"operation {oph} banned"
    ~level:Notice
    ~pp1:Operation_hash.pp
    ("oph", Operation_hash.encoding)

let request_completed_info =
  declare_2
    ~section
    ~name:"request_completed_info"
    ~msg:"{view} in {worker_status}"
    ~level:Info
    ~pp1:Request.pp
    ~pp2:Worker_types.pp_status_completed
    ("view", Request.encoding)
    ("worker_status", Worker_types.request_status_encoding)

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
    ~pp1:Request.pp
    ~pp2:Worker_types.pp_status
    ("view", Request.encoding)
    ("worker_status", Worker_types.request_status_encoding)

type origin = Peer of P2p_peer_id.t | Arrived | Injected | Leftover

let origin_encoding : origin Data_encoding.t =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"notified"
        (Tag 0)
        P2p_peer_id.encoding
        (function Peer peer_id -> Some peer_id | _ -> None)
        (fun peer_id -> Peer peer_id);
      case
        ~title:"leftover"
        (Tag 1)
        (constant "leftover")
        (function Leftover -> Some () | _ -> None)
        (fun () -> Leftover);
      case
        ~title:"arrived"
        (Tag 2)
        (constant "arrived")
        (function Arrived -> Some () | _ -> None)
        (fun () -> Arrived);
      case
        ~title:"injected"
        (Tag 3)
        (constant "injected")
        (function Injected -> Some () | _ -> None)
        (fun () -> Injected);
    ]

let pp_origin fmt = function
  | Peer peer_id -> Format.fprintf fmt "notified by %a" P2p_peer_id.pp peer_id
  | Leftover -> Format.fprintf fmt "leftover from previous run"
  | Arrived -> Format.fprintf fmt "arrived"
  | Injected -> Format.fprintf fmt "injected"

let ban_operation_encountered =
  declare_2
    ~section
    ~name:"banned_operation_encountered"
    ~msg:"{origin}: banned {oph} encountered"
    ~level:Notice
    ~pp1:pp_origin
    ~pp2:Operation_hash.pp
    ("origin", origin_encoding)
    ("oph", Operation_hash.encoding)

let operation_not_fetched =
  declare_1
    ~section
    ~name:"operation_not_fetched"
    ~msg:"Operation {oph} was not fetched"
    ~level:Debug
    ~pp1:Operation_hash.pp
    ("oph", Operation_hash.encoding)
