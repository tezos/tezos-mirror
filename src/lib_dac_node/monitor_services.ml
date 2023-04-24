(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

module S = struct
  let root_hashes =
    Tezos_rpc.Service.get_service
      ~description:
        "Monitor a stream of root hashes that are produced by another dac node \
         responsible for the serialization of the dac payload (coordinator).  "
      ~query:Tezos_rpc.Query.empty
      ~output:Dac_plugin.non_proto_encoding_unsafe
      Tezos_rpc.Path.(open_root / "monitor" / "root_hashes")

  let certificate =
    Tezos_rpc.Service.get_service
      ~description:
        "Monitor a stream of updates to certificates for a given root hash. \
         Every time a new signature for the root hash is received by the \
         coordinator node, the corresponding certificate is updated and \
         streamed via this endpoint. This monitor endpoint guarantees at least \
         once delivery, i.e. a certificate update could be streamed multiple \
         times."
      ~query:Tezos_rpc.Query.empty
      ~output:Certificate_repr.encoding
      Tezos_rpc.Path.(
        open_root / "monitor" / "certificate" /: Dac_plugin.raw_hash_rpc_arg)
end

let root_hashes dac_node_cctxt =
  Tezos_rpc.Context.make_streamed_call S.root_hashes dac_node_cctxt () () ()

let certificate dac_node_cctxt root_hash =
  Tezos_rpc.Context.make_streamed_call
    S.certificate
    dac_node_cctxt
    ((), root_hash)
    ()
    ()
