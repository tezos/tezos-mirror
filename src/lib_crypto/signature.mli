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

type version = Version_0 | Version_1 | Version_2

val version_encoding : version Data_encoding.t

val version_arg : version Resto.Arg.t

(** The type of conversion modules from one version to another. *)
module type CONV = sig
  module V_from : S.COMMON_SIGNATURE

  module V_to : S.COMMON_SIGNATURE

  val public_key_hash : V_from.Public_key_hash.t -> V_to.Public_key_hash.t

  val public_key : V_from.Public_key.t -> V_to.Public_key.t

  val secret_key : V_from.Secret_key.t -> V_to.Secret_key.t

  val signature : V_from.t -> V_to.t
end

(** The type of {e partial} conversion modules from one version to another. *)
module type CONV_OPT = sig
  module V_from : S.COMMON_SIGNATURE

  module V_to : S.COMMON_SIGNATURE

  val public_key_hash :
    V_from.Public_key_hash.t -> V_to.Public_key_hash.t option

  val public_key : V_from.Public_key.t -> V_to.Public_key.t option

  val secret_key : V_from.Secret_key.t -> V_to.Secret_key.t option

  val signature : V_from.t -> V_to.t option

  val get_public_key :
    V_from.Public_key.t -> V_to.Public_key.t Error_monad.tzresult

  val get_public_key_exn : V_from.Public_key.t -> V_to.Public_key.t

  val get_public_key_hash :
    V_from.Public_key_hash.t -> V_to.Public_key_hash.t Error_monad.tzresult

  val get_public_key_hash_exn :
    V_from.Public_key_hash.t -> V_to.Public_key_hash.t

  val get_secret_key :
    V_from.Secret_key.t -> V_to.Secret_key.t Error_monad.tzresult

  val get_secret_key_exn : V_from.Secret_key.t -> V_to.Secret_key.t

  val get_signature : V_from.t -> V_to.t Error_monad.tzresult

  val get_signature_exn : V_from.t -> V_to.t
end

(** The module [V_latest] is to be used by the shell and points to the latest
    available version of signatures. *)
module V_latest : sig
  val version : version

  include module type of Signature_v2
end

(** [V0] supports Ed25519, Secp256k1, and P256. *)
module V0 : sig
  val version : version

  include module type of Signature_v0

  (** Converting from signatures of {!V_latest} to {!V0}. *)
  module Of_V_latest :
    CONV_OPT with module V_from := V_latest and module V_to := Signature_v0
end

(** [V1] supports Ed25519, Secp256k1, P256, and BLS (aug). *)
module V1 : sig
  val version : version

  include module type of Signature_v1

  (** Converting from signatures of {!V_latest} to {!V1}. *)
  module Of_V_latest :
    CONV_OPT with module V_from := V_latest and module V_to := Signature_v1
end

(** [V2] supports Ed25519, Secp256k1, P256, and BLS (aug). *)
module V2 : sig
  val version : version

  include module type of Signature_v2

  (** Converting from signatures of {!V_latest} to {!V2}. *)
  module Of_V_latest :
    CONV_OPT with module V_from := V_latest and module V_to := Signature_v2
end

include module type of V_latest

(** Converting from signatures of {!V_latest} to {!V_latest}. This module
    implements conversions which are the identity, so total, but we keep the
    signature as {!CONV_OPT} for compatibility with {!V0.Of_V_latest} and
    {!V1.Of_V_latest} and to ease snapshotting. *)
module Of_V_latest :
  CONV_OPT with module V_from := V_latest and module V_to := V_latest

(** Converting from signatures of {!V0} to {!V_latest}. *)
module Of_V0 : CONV with module V_from := V0 and module V_to := V_latest

(** Converting from signatures of {!V1} to {!V_latest}. *)
module Of_V1 : CONV with module V_from := V1 and module V_to := V_latest

(** Converting from signatures of {!V2} to {!V_latest}. *)
module Of_V2 : CONV with module V_from := V2 and module V_to := V_latest
