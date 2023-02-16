(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold  <contact@tmarigold.dev>                      *)
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

(* A variant of [Sc_rollup_reveal_hash.encoding] that prefers hex
   encoding over b58check encoding for JSON. *)
let root_hash_encoding ((module P) : Dac_plugin.t) =
  let binary = P.encoding in
  Data_encoding.(
    splitted
      ~binary
      ~json:
        (conv_with_guard
           P.to_hex
           (fun str ->
             Result.of_option ~error:"Not a valid hash" (P.of_hex str))
           (string' Plain)))

let store_preimage_request_encoding =
  let pagination_scheme_encoding = Pages_encoding.pagination_scheme_encoding in
  Data_encoding.(
    obj2
      (req "payload" Data_encoding.(bytes' Hex))
      (req "pagination_scheme" pagination_scheme_encoding))

let store_preimage_response_encoding ctx =
  Data_encoding.(
    obj2
      (req "root_hash" (root_hash_encoding ctx))
      (req "external_message" (bytes' Hex)))

let external_message_query =
  let open Tezos_rpc.Query in
  query (fun hex_string -> hex_string)
  |+ opt_field "external_message" Tezos_rpc.Arg.string (fun s -> s)
  |> seal

let dac_store_preimage ctx =
  Tezos_rpc.Service.post_service
    ~description:"Split DAC reveal data"
    ~query:Tezos_rpc.Query.empty
    ~input:store_preimage_request_encoding
    ~output:(store_preimage_response_encoding ctx)
    Tezos_rpc.Path.(open_root / "store_preimage")

(* DAC/FIXME: https://gitlab.com/tezos/tezos/-/issues/4263
   remove this endpoint once end-to-end tests are in place. *)
let verify_external_message_signature :
    ([`GET], unit, unit, string option, unit, bool) Tezos_rpc.Service.service =
  Tezos_rpc.Service.get_service
    ~description:"Verify signature of an external message to inject in L1"
    ~query:external_message_query
    ~output:Data_encoding.bool
    Tezos_rpc.Path.(open_root / "verify_signature")

let retrieve_preimage ((module P) : Dac_plugin.t) =
  Tezos_rpc.Service.get_service
    ~description:"Retrieves a page by its page hash and returns its contents"
    ~query:Tezos_rpc.Query.empty
    ~output:Data_encoding.bytes
    Tezos_rpc.Path.(open_root / "preimage" /: P.hash_rpc_arg)
