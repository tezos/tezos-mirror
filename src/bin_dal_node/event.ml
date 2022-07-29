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

let section = ["dal"; "node"]

let starting_node =
  declare_0
    ~section
    ~name:"starting_dal_node"
    ~msg:"Starting the DAL node"
    ~level:Notice
    ()

let shutdown_node =
  declare_1
    ~section
    ~name:"stopping_dal_node"
    ~msg:"Stopping DAL node"
    ~level:Notice
    ("exit_status", Data_encoding.int8)

let store_is_ready =
  declare_0
    ~section
    ~name:"dal_node_store_is_ready"
    ~msg:"The DAL node store is ready"
    ~level:Notice
    ()

let rpc_server_is_ready =
  declare_2
    ~section
    ~name:"dal_node_rpc_server_is_ready"
    ~msg:"The DAL node is listening to {addr}:{port}"
    ~level:Notice
    ("addr", Data_encoding.string)
    ("port", Data_encoding.uint16)

let node_is_ready =
  declare_0
    ~section
    ~name:"dal_node_is_ready"
    ~msg:"The DAL node is ready"
    ~level:Notice
    ()

let data_dir_not_found =
  declare_1
    ~section
    ~name:"dal_node_no_data_dir"
    ~msg:
      "The DAL node data directory {path} doesn't exists. Create using: \
       init-config --data-dir={path} "
    ~level:Error
    ("path", Data_encoding.(string))

let stored_slot =
  declare_2
    ~section
    ~name:"stored_slot"
    ~msg:"Slot stored: size {size}, shards {shards}"
    ~level:Notice
    ("size", Data_encoding.int31)
    ("shards", Data_encoding.int31)

let fetched_slot =
  declare_2
    ~section
    ~name:"fetched_slot"
    ~msg:"Slot fetched: size {size}, shards {shards}"
    ~level:Notice
    ("size", Data_encoding.int31)
    ("shards", Data_encoding.int31)
