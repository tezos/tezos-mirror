(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Tezos - BLS12-381 cryptography *)

include
  S.AGGREGATE_SIGNATURE
    with type Public_key.t = Bls12_381_signature.MinPk.pk
     and type Secret_key.t = Bls12_381_signature.sk
     and type t = Bls12_381_signature.MinPk.signature
     and type watermark = Bytes.t

include
  S.THRESHOLD_SIGNATURE
    with type Public_key.t = Public_key.t
     and type Secret_key.t = Secret_key.t
     and type t = t
     and type watermark = watermark

include S.RAW_DATA with type t := t

(** Module to access/expose the primitives of BLS12-381 *)
module Primitive : sig
  module Fr : S.PRIME_FIELD with type t = Bls12_381.Fr.t

  module G1 : S.CURVE with type Scalar.t = Fr.t

  module G2 : S.CURVE with type Scalar.t = Fr.t

  val pairing_check : (G1.t * G2.t) list -> bool
end

val sign_aug : ?watermark:watermark -> Bls12_381_signature.sk -> watermark -> t

val check_aug : ?watermark:watermark -> Public_key.t -> t -> watermark -> bool

val pop_prove : Bls12_381_signature.sk -> Bytes.t

val pop_verify : Public_key.t -> Bytes.t -> bool
