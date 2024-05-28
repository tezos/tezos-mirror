(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

module Context = Environment_context.Context
module Register = Environment_context.Register

let err_implementation_mismatch =
  Environment_context.err_implementation_mismatch

type validation_result = Environment_context.validation_result = {
  context : Context.t;
  fitness : Fitness.t;
  message : string option;
  max_operations_ttl : int;
  last_finalized_block_level : Int32.t;
  last_preserved_block_level : Int32.t;
}

type quota = Environment_context.quota = {max_size : int; max_op : int option}

type rpc_context = Environment_context.rpc_context = {
  block_hash : Tezos_crypto.Hashed.Block_hash.t;
  block_header : Block_header.shell_header;
  context : Context.t;
}

module type PROTOCOL = Environment_protocol_T.PROTOCOL

module V0 = Environment_V0
module V1 = Environment_V1
module V2 = Environment_V2
module V3 = Environment_V3
module V4 = Environment_V4
module V5 = Environment_V5
module V6 = Environment_V6
module V7 = Environment_V7
module V8 = Environment_V8
module V9 = Environment_V9
module V10 = Environment_V10
module V11 = Environment_V11
module V12 = Environment_V12
module Memory_context = Memory_context
module Proxy_context = Proxy_context
module Proxy_delegate = Proxy_delegate

module Internal_for_tests = struct
  module Environment_protocol_T_test = Environment_protocol_T_test
  module Environment_cache = Environment_cache
end
