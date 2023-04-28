(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.ch>                      *)
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

open Protocol
open Alpha_context
open Protocol_client_context

let to_json_and_bytes branch message =
  let op =
    ( Environment.Operation.{branch},
      Contents_list (Single (Failing_noop message)) )
  in
  let encoding = Operation.unsigned_encoding_with_legacy_attestation_name in
  ( Data_encoding.Json.construct encoding op,
    Data_encoding.Binary.to_bytes_exn encoding op )

let sign_message (cctxt : #full) ~src_sk ~block ~message =
  let json, bytes = to_json_and_bytes block message in
  cctxt#message "signed content: @[%a@]" Data_encoding.Json.pp json
  >>= fun () ->
  Client_keys.sign cctxt ~watermark:Signature.Generic_operation src_sk bytes

let check_message (cctxt : #full) ~block ~key_locator ~quiet ~message ~signature
    =
  let json, bytes = to_json_and_bytes block message in
  (if quiet then Lwt.return_unit
  else cctxt#message "checked content: @[%a@]" Data_encoding.Json.pp json)
  >>= fun () ->
  Client_keys.check
    ~watermark:Signature.Generic_operation
    key_locator
    signature
    bytes
