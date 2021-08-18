(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Protocol_client_context
open Tezos_micheline

let get_contract_manager (cctxt : #full) contract =
  let open Micheline in
  let open Michelson_v1_primitives in
  Client_proto_context.get_storage
    cctxt
    ~chain:cctxt#chain
    ~block:cctxt#block
    contract
  >>=? function
  | None -> cctxt#error "This is not a smart contract."
  | Some storage -> (
      match root storage with
      | Prim (_, D_Pair, [Bytes (_, bytes); _], _) | Bytes (_, bytes) -> (
          match
            Data_encoding.Binary.of_bytes_opt
              Signature.Public_key_hash.encoding
              bytes
          with
          | Some k -> return k
          | None ->
              cctxt#error
                "Cannot find a manager key in contracts storage (decoding \
                 bytes failed).\n\
                 Transfer from scripted contract are currently only supported \
                 for \"manager\" contract.")
      | Prim (_, D_Pair, [String (_, value); _], _) | String (_, value) -> (
          match Signature.Public_key_hash.of_b58check_opt value with
          | Some k -> return k
          | None ->
              cctxt#error
                "Cannot find a manager key in contracts storage (\"%s\" is not \
                 a valid key).\n\
                 Transfer from scripted contract are currently only supported \
                 for \"manager\" contract."
                value)
      | _raw_storage ->
          cctxt#error
            "Cannot find a manager key in contracts storage (wrong storage \
             format : @[%a@]).\n\
             Transfer from scripted contract are currently only supported for \
             \"manager\" contract."
            Michelson_v1_printer.print_expr
            storage)
