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
open Tezos_crypto_dal

let split_query =
  let open RPC_query in
  query (fun fill_x00 -> fill_x00)
  |+ flag "fill" (fun fill_x00 -> fill_x00)
  |> seal

let split_slot () =
  RPC_service.post_service
    ~description:"Split and store a slot"
    ~query:split_query
    ~input:Data_encoding.string
    ~output:Data_encoding.string
      (* see [Slot_manager.Slot_header.to_b58check] *)
    RPC_path.(open_root / "slot" / "split")

let slot_query =
  let open RPC_query in
  query (fun trim_x00 -> trim_x00)
  |+ flag "trim" (fun trim_x00 -> trim_x00)
  |> seal

let slot () =
  RPC_service.get_service
    ~description:"Show content of a slot"
    ~query:slot_query
    ~output:Data_encoding.string
    RPC_path.(open_root / "slot" / "content" /: Cryptobox.Commitment.rpc_arg)

let slot_pages () =
  RPC_service.get_service
    ~description:"Fetch slot as list of pages"
    ~query:RPC_query.empty
    ~output:(Data_encoding.list Data_encoding.string)
    RPC_path.(open_root / "slot" / "pages" /: Cryptobox.Commitment.rpc_arg)

let shard () =
  let shard_arg = RPC_arg.int in
  RPC_service.get_service
    ~description:"Fetch shard as bytes"
    ~query:RPC_query.empty
    ~output:Cryptobox.shard_encoding
    RPC_path.(open_root / "shard" /: Cryptobox.Commitment.rpc_arg /: shard_arg)
