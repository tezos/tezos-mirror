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

open Tezos_rpc
open Protocol
open Alpha_context

let sc_rollup_address () =
  RPC_service.get_service
    ~description:"Smart-contract rollup address"
    ~query:RPC_query.empty
    ~output:Sc_rollup.Address.encoding
    RPC_path.(open_root / "sc_rollup_address")

let current_tezos_head () =
  RPC_service.get_service
    ~description:"Tezos head known to the smart-contract rollup node"
    ~query:RPC_query.empty
    ~output:(Data_encoding.option Block_hash.encoding)
    RPC_path.(open_root / "tezos_head")

let current_tezos_level () =
  RPC_service.get_service
    ~description:"Tezos level known to the smart-contract rollup node"
    ~query:RPC_query.empty
    ~output:(Data_encoding.option Data_encoding.int32)
    RPC_path.(open_root / "tezos_level")

let current_inbox () =
  RPC_service.get_service
    ~description:"Current inbox"
    ~query:RPC_query.empty
    ~output:Sc_rollup.Inbox.encoding
    RPC_path.(open_root / "inbox")

let current_ticks () =
  RPC_service.get_service
    ~description:"Current number of ticks for current level"
    ~query:RPC_query.empty
    ~output:Data_encoding.z
    RPC_path.(open_root / "ticks")

let current_total_ticks () =
  RPC_service.get_service
    ~description:"Current total number of ticks"
    ~query:RPC_query.empty
    ~output:Sc_rollup.Tick.encoding
    RPC_path.(open_root / "total_ticks")

let current_num_messages () =
  RPC_service.get_service
    ~description:"Current number of messages"
    ~query:RPC_query.empty
    ~output:Data_encoding.z
    RPC_path.(open_root / "current_num_messages")

let current_state_hash () =
  RPC_service.get_service
    ~description:"Current state hash"
    ~query:RPC_query.empty
    ~output:Sc_rollup.State_hash.encoding
    RPC_path.(open_root / "state_hash")

let last_stored_commitment () =
  RPC_service.get_service
    ~description:"Last commitment computed by the node"
    ~query:RPC_query.empty
    ~output:(Data_encoding.option Sc_rollup.Commitment.encoding)
    RPC_path.(open_root / "last_stored_commitment")

let last_published_commitment () =
  RPC_service.get_service
    ~description:"Last commitment published by the node"
    ~query:RPC_query.empty
    ~output:(Data_encoding.option Sc_rollup.Commitment.encoding)
    RPC_path.(open_root / "last_published_commitment")

let current_status () =
  RPC_service.get_service
    ~description:"Current PVM status"
    ~query:RPC_query.empty
    ~output:Data_encoding.string
    RPC_path.(open_root / "status")
