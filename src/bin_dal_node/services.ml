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
    ~output:Cryptobox.commitment_encoding
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
    RPC_path.(
      open_root / "slot" / "content" /: Slot_manager.Slot_header.rpc_arg)

let shard () =
  let shard_arg = RPC_arg.int in
  RPC_service.get_service
    ~description:"Fetch shard as bytes"
    ~query:RPC_query.empty
    ~output:Cryptobox.shard_encoding
    RPC_path.(
      open_root / "shard" /: Slot_manager.Slot_header.rpc_arg /: shard_arg)

let handle_split_slot cryptobox_setup store fill slot =
  let slot = String.to_bytes slot in
  let slot = if fill then Slot_manager.Utils.fill_x00 slot else slot in
  Slot_manager.split_and_store cryptobox_setup store slot

type error += Invalid_slot_header of string (* FIXME: registration *)

let handle_slot store (_, commitment) trim () =
  let open Lwt_result_syntax in
  let* slot = Slot_manager.get_slot store commitment in
  let slot = if trim then Slot_manager.Utils.trim_x00 slot else slot in
  return (String.of_bytes slot)

let handle_shard store ((_, commitment), shard) () () =
  let open Lwt_result_syntax in
  let* shard = Slot_manager.get_shard store commitment shard in
  return shard
