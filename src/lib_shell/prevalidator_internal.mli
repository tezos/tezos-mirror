(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4113

   This file is part of the implementation of the new mempool, which
   uses features of the protocol that only exist since Lima.

   When you modify this file, consider whether you should also change
   the files that implement the legacy mempool for Kathmandu. They all
   start with the "legacy" prefix and will be removed when Lima is
   activated on Mainnet. *)

(** Create a prevalidator instance for a specific protocol
    ([Filter.Proto] where [module Filter : Shell_plugin.FILTER]).

    The protocol must be Lima (environment V7) or a more recent
    version. For Kathmandu and older protocols, use
    {!Legacy_prevalidator_internal.make} instead.

    This function is wrapped in {!Prevalidator.create}. *)
val make :
  Shell_limits.prevalidator_limits ->
  Distributed_db.chain_db ->
  Tezos_crypto.Chain_id.t ->
  (module Shell_plugin.FILTER) ->
  Prevalidator_internal_common.t
