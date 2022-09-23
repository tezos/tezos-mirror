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
  let open Tezos_rpc.RPC_query in
  query (fun fill_x00 -> fill_x00)
  |+ flag "fill" (fun fill_x00 -> fill_x00)
  |> seal

let split_slot () =
  Tezos_rpc.RPC_service.post_service
    ~description:"Split and store a slot"
    ~query:split_query
    ~input:Data_encoding.bytes
    ~output:Data_encoding.string
      (* see [Slot_manager.Slot_header.to_b58check] *)
    Tezos_rpc.RPC_path.(open_root / "slot" / "split")

let slot_query =
  let open Tezos_rpc.RPC_query in
  query (fun trim_x00 -> trim_x00)
  |+ flag "trim" (fun trim_x00 -> trim_x00)
  |> seal

let slot () =
  Tezos_rpc.RPC_service.get_service
    ~description:"Show content of a slot"
    ~query:slot_query
    ~output:Data_encoding.bytes
    Tezos_rpc.RPC_path.(
      open_root / "slot" / "content" /: Cryptobox.Commitment.rpc_arg)

let slot_pages () =
  Tezos_rpc.RPC_service.get_service
    ~description:"Fetch slot as list of pages"
    ~query:Tezos_rpc.RPC_query.empty
    ~output:(Data_encoding.list Data_encoding.bytes)
    Tezos_rpc.RPC_path.(
      open_root / "slot" / "pages" /: Cryptobox.Commitment.rpc_arg)

let stored_slot_headers () =
  Tezos_rpc.RPC_service.get_service
    ~description:"List slot headers for a given block hash"
    ~query:Tezos_rpc.RPC_query.empty
    ~output:
      Data_encoding.(
        list
          (obj2
             (req "index" int31)
             (req "slot_header" Cryptobox.Commitment.encoding)))
    Tezos_rpc.RPC_path.(open_root / "stored_slot_headers" /: Block_hash.rpc_arg)

let shard () =
  let shard_arg = Tezos_rpc.RPC_arg.int in
  Tezos_rpc.RPC_service.get_service
    ~description:"Fetch shard as bytes"
    ~query:Tezos_rpc.RPC_query.empty
    ~output:Cryptobox.shard_encoding
    Tezos_rpc.RPC_path.(
      open_root / "shard" /: Cryptobox.Commitment.rpc_arg /: shard_arg)

let monitor_slot_headers () =
  Tezos_rpc.RPC_service.get_service
    ~description:"Monitor stored slot headers"
    ~query:Tezos_rpc.RPC_query.empty
    ~output:
      Data_encoding.(obj1 (req "slot_header" Cryptobox.Commitment.encoding))
    Tezos_rpc.RPC_path.(open_root / "monitor_slot_headers")
