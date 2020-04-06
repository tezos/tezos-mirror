(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Declares logging events for [node] *)

include Internal_event.Simple

let section_root = ["node"]

let section = section_root

let p2p_event =
  declare_2
    ~section
    ~name:"shell-node"
    ~msg:"shell-node initialization: {status}"
    ~level:Notice
    ("time-stamp", Data_encoding.float)
    ("status", Data_encoding.string)

let emit_tagged (f : (float * string) t) (status : string) =
  let time_stamp = Unix.gettimeofday () in
  emit f (time_stamp, status)

let section = section_root @ ["protocol_store"]

let store_protocol_already_included =
  declare_1
    ~section
    ~name:"store_protocol_already_included"
    ~msg:"protocol {protocol} is already in store: nothing to do"
    ~level:Info
    ("protocol", Protocol_hash.encoding)

let store_protocol_missing_files =
  declare_1
    ~section
    ~name:"store_protocol_missing_files"
    ~msg:"protocol {protocol} won't be stored: missing source files"
    ~level:Info
    ("protocol", Protocol_hash.encoding)

let store_protocol_incorrect_hash =
  declare_1
    ~section
    ~name:"store_protocol_incorrect_hash"
    ~msg:"protocol {protocol} won't be stored: wrong hash"
    ~level:Info
    ("protocol", Protocol_hash.encoding)

let store_protocol_success =
  declare_1
    ~section
    ~name:"store_successful_store"
    ~msg:"protocol {protocol} successfully stored"
    ~level:Info
    ("protocol", Protocol_hash.encoding)

let section = section_root @ ["storage_consistency"]

let storage_corrupted_context_detected =
  declare_0
    ~section
    ~name:"corrupted_context_detected"
    ~msg:
      "context corruption detected: restoring integrity, this may take a while"
    ~level:Error
    ()

let storage_restored_context_integrity =
  declare_1
    ~section
    ~name:"restored_context_integrity"
    ~msg:"successfully restored context integrity - repaired {entries} entries"
    ~level:Notice
    ("entries", Data_encoding.int31)

let storage_context_already_consistent =
  declare_0
    ~section
    ~name:"context_already_consistent"
    ~msg:"no corruption detected while scanning the context."
    ~level:Notice
    ()

let storage_restore_context_integrity_error =
  declare_1
    ~section
    ~name:"restore_context_integrity_error"
    ~msg:"error while scanning the context: {trace}"
    ~level:Notice
    ~pp1:pp_print_error_first
    ("trace", trace_encoding)

let section = section_root @ ["shutdown"]

let shutdown_p2p_layer =
  declare_0
    ~section
    ~name:"shutdown_p2p"
    ~msg:"shutting down the p2p layer"
    ~level:Info
    ()

let shutdown_ddb =
  declare_0
    ~section
    ~name:"shutdown_ddb"
    ~msg:"shutting down the distributed database"
    ~level:Info
    ()

let shutdown_validator =
  declare_0
    ~section
    ~name:"shutdown_validator"
    ~msg:"shutting down the validator"
    ~level:Info
    ()

let shutdown_state =
  declare_0 ~section ~name:"shutdown_state" ~msg:"closing state" ~level:Info ()
