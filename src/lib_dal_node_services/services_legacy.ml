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

let slot_pages =
  Tezos_rpc.Service.get_service
    ~description:"Fetch slot as list of pages"
    ~query:Tezos_rpc.Query.empty
    ~output:(Data_encoding.list Data_encoding.bytes)
    Tezos_rpc.Path.(
      open_root / "slot" / "pages" /: Cryptobox.Commitment.rpc_arg)

let shard =
  let shard_arg = Tezos_rpc.Arg.int in
  Tezos_rpc.Service.get_service
    ~description:"Fetch shard as bytes"
    ~query:Tezos_rpc.Query.empty
    ~output:Cryptobox.shard_encoding
    Tezos_rpc.Path.(
      open_root / "shard" /: Cryptobox.Commitment.rpc_arg /: shard_arg)

let shards =
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4111
     Bound size of the input *)
  Tezos_rpc.Service.post_service
    ~description:"Fetch shards as bytes"
    ~query:Tezos_rpc.Query.empty
    ~output:(Data_encoding.list Cryptobox.shard_encoding)
    ~input:Data_encoding.(list int31)
    Tezos_rpc.Path.(open_root / "shards" /: Cryptobox.Commitment.rpc_arg)
