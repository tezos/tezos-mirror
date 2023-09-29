(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = {a : int32; b : int32}

let invariant {a; b} = Compare.Int32.(a >= 0l && b >= 0l)

let create a b =
  let st = {a; b} in
  if invariant st then Some st else None

let encoding =
  Data_encoding.conv
    (fun {a; b} -> (a, b))
    (fun (a, b) -> {a; b})
    Data_encoding.(obj2 (req "demo_a" int32) (req "demo_b" int32))

let encoding_length =
  match Data_encoding.Binary.fixed_length encoding with
  | None ->
      assert false
  | Some length ->
      length

let state_key = ["state"]

let get_state context =
  let open Lwt_syntax in
  let+ state = Context.find context state_key in
  match state with
  | None ->
      assert false
  | Some encoded_state -> (
    match Data_encoding.Binary.of_bytes_opt encoding encoded_state with
    | Some x ->
        x
    | None ->
        assert false )

let update_state context state =
  let encoded_state = Data_encoding.Binary.to_bytes_exn encoding state in
  Context.add context state_key encoded_state
