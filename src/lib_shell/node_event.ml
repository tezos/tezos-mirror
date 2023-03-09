(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
  declare_1
    ~section
    ~name:"p2p-initialization"
    ~msg:"p2p initialization: {status}"
    ~level:Notice
    ("status", Data_encoding.string)

let section = section_root @ ["protocol_store"]

let store_protocol_already_included =
  declare_1
    ~section
    ~name:"store_protocol_already_included"
    ~msg:"protocol {protocol} is already in store: nothing to do"
    ~level:Debug
    ("protocol", Protocol_hash.encoding)

let store_protocol_missing_files =
  declare_1
    ~section
    ~name:"store_protocol_missing_files"
    ~msg:"protocol {protocol} won't be stored: missing source files"
    ~level:Warning
    ("protocol", Protocol_hash.encoding)

let store_protocol_incorrect_hash =
  declare_1
    ~section
    ~name:"store_protocol_incorrect_hash"
    ~msg:"protocol {protocol} won't be stored: wrong hash"
    ~level:Warning
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
    ~msg:"context corruption detected"
    ~level:Error
    ()

let storage_context_already_consistent =
  declare_0
    ~section
    ~name:"context_already_consistent"
    ~msg:"no corruption detected while scanning the context."
    ~level:Info
    ()

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

let shutdown_store =
  declare_0 ~section ~name:"shutdown_store" ~msg:"closing store" ~level:Info ()
