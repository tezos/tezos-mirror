(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Cryptographic signatures are versioned to expose different versions to
    different protocols, depending on the support.  *)

(** [V0] supports Ed25519, Secp256k1, and P256. *)
module V0 = Signature_v0

(** [V1] supports Ed25519, Secp256k1, P256 and BLS. *)
module V1 = Signature_v1

(** The type of conversion modules from one version to another. *)
module type CONV = sig
  module V_from : S.COMMON_SIGNATURE

  module V_to : S.COMMON_SIGNATURE

  val public_key_hash : V_from.Public_key_hash.t -> V_to.Public_key_hash.t

  val public_key : V_from.Public_key.t -> V_to.Public_key.t

  val secret_key : V_from.Secret_key.t -> V_to.Secret_key.t

  val signature : V_from.t -> V_to.t
end

(** The module [V_latest] is to be used by the shell and points to the latest
    available version of signatures. *)
module V_latest : module type of V1

include module type of V_latest

(** Converting from signatures of {!V0} to {!V_latest}. *)
module Of_V0 : CONV with module V_from := V0 and module V_to := V_latest

(** Converting from signatures of {!V1} to {!V_latest}. *)
module Of_V1 : CONV with module V_from := V1 and module V_to := V_latest
