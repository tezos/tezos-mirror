(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Errors from EIP-1474 *)

open Rpc_encodings

type t = JSONRPC.error

let parse_error msg =
  JSONRPC.{code = -32700; message = "Parse error: " ^ msg; data = None}

let invalid_request reason =
  JSONRPC.{code = -32600; message = reason; data = None}

let method_not_found method_ =
  JSONRPC.
    {code = -32601; message = "Method not found"; data = Some (`String method_)}

let invalid_params reason =
  JSONRPC.{code = -32602; message = reason; data = None}

let internal_error ?data reason =
  JSONRPC.{code = -32603; message = reason; data}

let invalid_input =
  JSONRPC.{code = -32000; message = "Invalid input"; data = None}

let resource_not_found reason =
  JSONRPC.{code = -32001; message = reason; data = None}

let resource_unavailable ?data reason =
  JSONRPC.
    {
      code = -32002;
      message = reason;
      data = Option.map (fun s -> `String s) data;
    }

let transaction_rejected reason hash =
  JSONRPC.
    {
      code = -32003;
      message = reason;
      data =
        hash
        |> Option.map
             (Data_encoding.Json.construct Ethereum_types.hash_encoding);
    }

let method_not_supported method_ =
  JSONRPC.
    {
      code = -32004;
      message = "Method not supported";
      data = Some (`String method_);
    }

let method_disabled method_ =
  JSONRPC.
    {code = -32007; message = "Method disabled"; data = Some (`String method_)}

let limit_exceeded reason hash =
  JSONRPC.
    {
      code = -32005;
      message = reason;
      data =
        hash
        |> Option.map
             (Data_encoding.Json.construct Ethereum_types.hash_encoding);
    }

let json_rpc_version_not_supported reason =
  JSONRPC.{code = -32006; message = reason; data = None}

let trace_transaction_not_found hash =
  internal_error
    (Format.asprintf "Transaction %a not found" Ethereum_types.pp_hash hash)

let trace_block_not_found block_number =
  resource_unavailable
    (Format.asprintf
       "Block %a unavailable for replay"
       Ethereum_types.pp_quantity
       block_number)

let trace_not_found = internal_error "Trace not available"

let tracer_not_implemented s =
  resource_unavailable
    ~data:s
    (Format.asprintf "Tracer is not available for the endpoint")
