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
