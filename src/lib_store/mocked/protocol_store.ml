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

module Protocol_table = Protocol_hash.Table

type t = Protocol.t Protocol_table.t

let mem table protocol_hash = Protocol_hash.Table.mem table protocol_hash

let all table = Protocol_table.to_seq_keys table |> Protocol_hash.Set.of_seq

let raw_store store protocol_hash bytes =
  if mem store protocol_hash then Lwt.return_none
  else
    let protocol_opt = Protocol.of_bytes bytes in
    match protocol_opt with
    | None -> Lwt.return_none
    | Some proto ->
        Protocol_table.add store protocol_hash proto ;
        Lwt.return_some protocol_hash

let store store protocol_hash protocol =
  if mem store protocol_hash then Lwt.return_none
  else (
    Protocol_table.add store protocol_hash protocol ;
    Lwt.return_some protocol_hash)

let read store protocol_hash =
  Lwt.return @@ Protocol_table.find_opt store protocol_hash

let init _store_dir = Lwt.return @@ Protocol_table.create 11
