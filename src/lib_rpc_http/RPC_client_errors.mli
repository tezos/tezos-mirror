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

type rpc_error =
  | Empty_answer
  | Connection_failed of string
  | Bad_request of string
  | Forbidden
  | Method_not_allowed of RPC_service.meth list
  | Unsupported_media_type of string option
  | Not_acceptable of {proposed : string; acceptable : string}
  | Unexpected_status_code of {
      code : Cohttp.Code.status_code;
      content : string;
      media_type : string option;
    }
  | Unexpected_content_type of {
      received : string;
      acceptable : string list;
      body : string;
    }
  | Unexpected_content of {
      content : string;
      media_type : string;
      error : string;
    }
  | OCaml_exception of string
  | Unauthorized_host of string option
  | Unauthorized_uri

type error +=
  | Request_failed of {meth : RPC_service.meth; uri : Uri.t; error : rpc_error}
