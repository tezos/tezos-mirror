(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

open Protocol.Alpha_context

type t = {
  contents : Tx_rollup_message.t list;
  cumulated_size : int;
  hash : Tx_rollup_inbox.hash;
}

let pp fmt {contents; cumulated_size; hash} =
  Format.fprintf
    fmt
    "tx rollup inbox: %d messages using %d bytes with hash %a"
    (List.length contents)
    cumulated_size
    Tx_rollup_inbox.pp_hash
    hash

let encoding =
  let open Data_encoding in
  conv
    (fun {contents; cumulated_size; hash} -> (contents, cumulated_size, hash))
    (fun (contents, cumulated_size, hash) -> {contents; cumulated_size; hash})
    (obj3
       (req "contents" @@ list Tx_rollup_message.encoding)
       (req "cumulated_size" int31)
       (req "hash" Tx_rollup_inbox.hash_encoding))

let to_protocol_inbox {contents; cumulated_size; hash} =
  Tx_rollup_inbox.
    {
      contents = List.map Tx_rollup_message.hash_uncarbonated contents;
      cumulated_size;
      hash;
    }
