(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

val build_raw_rpc_directory_with_validator :
  (module Block_services.PROTO) ->
  (module Registered_protocol.T) ->
  (Store.chain_store * Store.Block.t) Tezos_rpc.Directory.directory

val build_raw_rpc_directory_without_validator :
  (module Block_services.PROTO) ->
  (module Registered_protocol.T) ->
  (Store.chain_store * Store.Block.t) Tezos_rpc.Directory.directory

(** [build_rpc_directory_with_validator chain_store block_services]
    builds an [RPC_directory.t] that contains all RPCs in
    [block_services], especially the one that depends on the
    validator.

    @raises Error when validator is not running.  *)
val build_rpc_directory_with_validator :
  Store.chain_store -> Block_services.block -> 'a Tezos_rpc.Directory.t Lwt.t

(** Same as [build_rpc_directory_with_validator] except that the
    returned [RPC_directory.t] contains only RPCs that do **not**
    depend on the validator. *)
val build_rpc_directory_without_validator :
  Store.chain_store -> Block_services.block -> 'a Tezos_rpc.Directory.t Lwt.t
