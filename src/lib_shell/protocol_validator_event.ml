(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Declares logging events for [protocol_validator] *)

include Internal_event.Simple

let section = ["node"; "validator"]

let validator_terminated =
  declare_0
    ~section
    ~name:"validator_terminated"
    ~msg:"validator terminated"
    ~level:Notice
    ()

let unexpected_worker_error =
  declare_1
    ~section
    ~name:"unexpected_worker_error"
    ~msg:"unexpected worker error: {trace}"
    ~level:Notice
    ~pp1:pp_print_top_error_of_trace
    ("trace", trace_encoding)

let previously_validated_protocol =
  declare_1
    ~section
    ~name:"previously_validated_protocol"
    ~msg:"protocol {hash} already validated, ignoring"
    ~level:Debug
    ("hash", Protocol_hash.encoding)

let pushing_protocol_validation =
  declare_1
    ~section
    ~name:"pushing_protocol_validation"
    ~msg:"pushing validation request for protocol {hash}"
    ~level:Debug
    ("hash", Protocol_hash.encoding)

let fetching_protocol =
  declare_2
    ~section
    ~name:"fetching_protocol"
    ~msg:"fetching protocol {hash}"
    ~level:Notice
    ("hash", Protocol_hash.encoding)
    ("source", Data_encoding.option P2p_peer.Id.encoding)
