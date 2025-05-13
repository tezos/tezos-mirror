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

type pkh =
  | Pkh_with_version of
      Tezos_crypto.Signature.Public_key_hash.t * Tezos_crypto.Signature.version
  | Pkh of Tezos_crypto.Signature.Public_key_hash.t

module type Authenticated_signing_request = sig
  type t = {
    pkh : pkh;
    data : Bytes.t;
    signature : Tezos_crypto.Signature.t option;
  }

  val to_sign :
    pkh:Tezos_crypto.Signature.Public_key_hash.t -> data:Bytes.t -> Bytes.t

  val encoding : t Data_encoding.t
end

module Sign : sig
  module Request : Authenticated_signing_request

  module Response : sig
    type t = Tezos_crypto.Signature.t

    val encoding : t Data_encoding.t
  end
end

module type Authenticated_request = sig
  type t = {
    pkh : Tezos_crypto.Signature.Public_key_hash.t;
    data : Bytes.t;
    signature : Tezos_crypto.Signature.t option;
  }

  val to_sign :
    pkh:Tezos_crypto.Signature.Public_key_hash.t -> data:Bytes.t -> Bytes.t

  val encoding : t Data_encoding.t
end

module Deterministic_nonce : sig
  module Request : Authenticated_request

  module Response : sig
    type t = Bytes.t

    val encoding : t Data_encoding.t
  end
end

module Deterministic_nonce_hash : sig
  module Request : Authenticated_request

  module Response : sig
    type t = Bytes.t

    val encoding : t Data_encoding.t
  end
end

module Supports_deterministic_nonces : sig
  module Request : sig
    type t = Tezos_crypto.Signature.Public_key_hash.t

    val encoding : t Data_encoding.t
  end

  module Response : sig
    type t = bool

    val encoding : t Data_encoding.t
  end
end

module Public_key : sig
  module Request : sig
    type t = Tezos_crypto.Signature.Public_key_hash.t

    val encoding : t Data_encoding.t
  end

  module Response : sig
    type t = Tezos_crypto.Signature.Public_key.t

    val encoding : t Data_encoding.t
  end
end

module Authorized_keys : sig
  module Response : sig
    type t =
      | No_authentication
      | Authorized_keys of Tezos_crypto.Signature.Public_key_hash.t list

    val encoding : t Data_encoding.t
  end
end

module Known_keys : sig
  module Response : sig
    type t = Tezos_crypto.Signature.Public_key_hash.t list

    val encoding : t Data_encoding.t
  end
end

module Bls_prove_possession : sig
  module Request : sig
    type t = Tezos_crypto.Signature.Public_key_hash.t

    val encoding : t Data_encoding.t
  end

  module Response : sig
    type t = Tezos_crypto.Signature.Bls.t

    val encoding : t Data_encoding.t
  end
end

module Request : sig
  type t =
    | Sign of Sign.Request.t
    | Public_key of Public_key.Request.t
    | Authorized_keys
    | Deterministic_nonce of Deterministic_nonce.Request.t
    | Deterministic_nonce_hash of Deterministic_nonce_hash.Request.t
    | Supports_deterministic_nonces of Supports_deterministic_nonces.Request.t
    | Known_keys
    | Bls_prove_possession of Bls_prove_possession.Request.t

  val encoding : t Data_encoding.t
end
