(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

let evm_type =
  "or (or (pair bytes (ticket (pair nat (option bytes)))) bytes) bytes"

let no_0x s =
  if String.starts_with ~prefix:"0x" s then String.sub s 2 (String.length s - 2)
  else s

let normalize s = String.lowercase_ascii @@ no_0x s

let u16_to_bytes n =
  let bytes = Bytes.make 2 'a' in
  Bytes.set_uint16_le bytes 0 n ;
  Bytes.to_string bytes

let leftPad32 s =
  let s = no_0x s in
  let len = String.length s in
  String.make (64 - len) '0' ^ s

let add_0x s = "0x" ^ s

let mapping_position index map_position =
  Tezos_crypto.Hacl.Hash.Keccak_256.digest
    (Hex.to_bytes
       (`Hex (leftPad32 index ^ leftPad32 (string_of_int map_position))))
  |> Hex.of_bytes |> Hex.show |> add_0x

let hex_string_to_int x = `Hex x |> Hex.to_string |> Z.of_bits |> Z.to_int

let next_rollup_node_level ~sc_rollup_node ~node ~client =
  let* () = Client.bake_for_and_wait client in
  let* level = Node.get_level node in
  Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node level

let next_evm_level ~evm_node ~sc_rollup_node ~node ~client =
  match Evm_node.mode evm_node with
  | Proxy _ -> next_rollup_node_level ~sc_rollup_node ~node ~client
  | Sequencer _ ->
      let open Rpc.Syntax in
      let* _ = Rpc.produce_block evm_node in
      let*@ level = Rpc.block_number evm_node in
      return (Int32.to_int level)
