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

module type PROTOCOL_V0 = functor
  (Env : Tezos_protocol_environment_sigs.V0.T)
  -> Env.Updater.PROTOCOL

module type PROTOCOL_V1 = functor
  (Env : Tezos_protocol_environment_sigs.V1.T)
  -> Env.Updater.PROTOCOL

module type PROTOCOL_V2 = functor
  (Env : Tezos_protocol_environment_sigs.V2.T)
  -> Env.Updater.PROTOCOL

module type PROTOCOL_V3 = functor
  (Env : Tezos_protocol_environment_sigs.V3.T)
  -> Env.Updater.PROTOCOL

module type PROTOCOL_V4 = functor
  (Env : Tezos_protocol_environment_sigs.V4.T)
  -> Env.Updater.PROTOCOL

module type PROTOCOL_V5 = functor
  (Env : Tezos_protocol_environment_sigs.V5.T)
  -> Env.Updater.PROTOCOL

module type PROTOCOL_V6 = functor
  (Env : Tezos_protocol_environment_sigs.V6.T)
  -> Env.Updater.PROTOCOL

module type PROTOCOL_V7 = functor
  (Env : Tezos_protocol_environment_sigs.V7.T)
  -> Env.Updater.PROTOCOL

module type PROTOCOL_V8 = functor
  (Env : Tezos_protocol_environment_sigs.V8.T)
  -> Env.Updater.PROTOCOL

module type PROTOCOL_V9 = functor
  (Env : Tezos_protocol_environment_sigs.V9.T)
  -> Env.Updater.PROTOCOL

module type PROTOCOL_V10 = functor
  (Env : Tezos_protocol_environment_sigs.V10.T)
  -> Env.Updater.PROTOCOL

module VersionTable = Protocol_hash.Table

type proto_env =
  | V0 of (module PROTOCOL_V0)
  | V1 of (module PROTOCOL_V1)
  | V2 of (module PROTOCOL_V2)
  | V3 of (module PROTOCOL_V3)
  | V4 of (module PROTOCOL_V4)
  | V5 of (module PROTOCOL_V5)
  | V6 of (module PROTOCOL_V6)
  | V7 of (module PROTOCOL_V7)
  | V8 of (module PROTOCOL_V8)
  | V9 of (module PROTOCOL_V9)
  | V10 of (module PROTOCOL_V10)

let versions : proto_env VersionTable.t = VersionTable.create 20

let register hash proto =
  let hash = Protocol_hash.of_b58check_exn hash in
  VersionTable.add versions hash proto

let mem hash = VersionTable.mem versions hash

let get hash = VersionTable.find versions hash
