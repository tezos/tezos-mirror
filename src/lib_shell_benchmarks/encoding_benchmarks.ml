(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Encoding_benchmarks_helpers

(* ------------------------------------------------------------------------- *)

module Make_elliptic_curve_encoding_benchmarks (A : sig
  val algo : Signature.algo
end) =
struct
  let algo_name =
    match A.algo with
    | Signature.Ed25519 -> "ed25519"
    | Signature.Secp256k1 -> "secp256k1"
    | Signature.P256 -> "p256"

  module Sampler = Crypto_samplers.Make_finite_key_pool (struct
    let size = 256

    let algo = `Algo A.algo
  end)

  (* encoding benches *)

  let public_key_encoding =
    make_encode_fixed_size
      ~name:("ENCODING_PUBLIC_KEY_" ^ algo_name)
      ~encoding:Signature.Public_key.encoding
      ~generator:Sampler.pk

  let () = Registration.register public_key_encoding

  let public_key_to_b58check =
    make_encode_fixed_size_to_string
      ~name:("B58CHECK_ENCODING_PUBLIC_KEY_" ^ algo_name)
      ~to_string:Signature.Public_key.to_b58check
      ~generator:Sampler.pk

  let () = Registration.register public_key_to_b58check

  let public_key_hash_encoding =
    make_encode_fixed_size
      ~name:("ENCODING_PUBLIC_KEY_HASH_" ^ algo_name)
      ~encoding:Signature.Public_key_hash.encoding
      ~generator:Sampler.pkh

  let () = Registration.register public_key_hash_encoding

  let public_key_hash_to_b58check =
    make_encode_fixed_size_to_string
      ~name:("B58CHECK_ENCODING_PUBLIC_KEY_HASH_" ^ algo_name)
      ~to_string:Signature.Public_key_hash.to_b58check
      ~generator:Sampler.pkh

  let () = Registration.register public_key_hash_to_b58check

  let secret_key_encoding =
    make_encode_fixed_size
      ~name:("ENCODING_SECRET_KEY_" ^ algo_name)
      ~encoding:Signature.Public_key_hash.encoding
      ~generator:Sampler.pkh

  let () = Registration.register secret_key_encoding

  let secret_key_to_b58check =
    make_encode_fixed_size_to_string
      ~name:("B58CHECK_ENCODING_SECRET_KEY_" ^ algo_name)
      ~to_string:Signature.Secret_key.to_b58check
      ~generator:Sampler.sk

  let () = Registration.register secret_key_to_b58check

  let signature_encoding =
    make_encode_fixed_size
      ~name:("ENCODING_SIGNATURE_" ^ algo_name)
      ~encoding:Signature.encoding
      ~generator:(fun _rng_state -> Signature.zero)

  let () = Registration.register signature_encoding

  let signature_to_b58check =
    make_encode_fixed_size_to_string
      ~name:("B58CHECK_ENCODING_SIGNATURE_" ^ algo_name)
      ~to_string:Signature.to_b58check
      ~generator:(fun _rng_state -> Signature.zero)

  let () = Registration.register signature_to_b58check

  (* decoding benches *)

  let public_key_decoding =
    make_decode_fixed_size
      ~name:("DECODING_PUBLIC_KEY_" ^ algo_name)
      ~encoding:Signature.Public_key.encoding
      ~generator:Sampler.pk

  let () = Registration.register public_key_decoding

  let public_key_from_b58check =
    make_decode_fixed_size_from_string
      ~name:("B58CHECK_DECODING_PUBLIC_KEY_" ^ algo_name)
      ~to_string:Signature.Public_key.to_b58check
      ~from_string:Signature.Public_key.of_b58check_exn
      ~generator:Sampler.pk

  let () = Registration.register public_key_from_b58check

  let public_key_hash_decoding =
    make_decode_fixed_size
      ~name:("DECODING_PUBLIC_KEY_HASH_" ^ algo_name)
      ~encoding:Signature.Public_key_hash.encoding
      ~generator:Sampler.pkh

  let () = Registration.register public_key_hash_decoding

  let public_key_hash_from_b58check =
    make_decode_fixed_size_from_string
      ~name:("B58CHECK_DECODING_PUBLIC_KEY_HASH_" ^ algo_name)
      ~to_string:Signature.Public_key_hash.to_b58check
      ~from_string:Signature.Public_key_hash.of_b58check_exn
      ~generator:Sampler.pkh

  let () = Registration.register public_key_hash_from_b58check

  let secret_key_decoding =
    make_decode_fixed_size
      ~name:("DECODING_SECRET_KEY_" ^ algo_name)
      ~encoding:Signature.Secret_key.encoding
      ~generator:Sampler.sk

  let () = Registration.register secret_key_decoding

  let secret_key_from_b58check =
    make_decode_fixed_size_from_string
      ~name:("B58CHECK_DECODING_SECRET_KEY_" ^ algo_name)
      ~to_string:Signature.Secret_key.to_b58check
      ~from_string:Signature.Secret_key.of_b58check_exn
      ~generator:Sampler.sk

  let () = Registration.register secret_key_from_b58check

  let signature_decoding =
    make_decode_fixed_size
      ~name:("DECODING_SIGNATURE_" ^ algo_name)
      ~encoding:Signature.encoding
      ~generator:(fun _rng_state -> Signature.zero)

  let () = Registration.register signature_decoding

  let signature_from_b58check =
    make_decode_fixed_size_from_string
      ~name:("B58CHECK_DECODING_SIGNATURE_" ^ algo_name)
      ~to_string:Signature.to_b58check
      ~from_string:Signature.of_b58check_exn
      ~generator:(fun _rng_state -> Signature.zero)

  let () = Registration.register signature_from_b58check
end

module Ed25519 = Make_elliptic_curve_encoding_benchmarks (struct
  let algo = Signature.Ed25519
end)

module Secp256k1 = Make_elliptic_curve_encoding_benchmarks (struct
  let algo = Signature.Secp256k1
end)

module P256 = Make_elliptic_curve_encoding_benchmarks (struct
  let algo = Signature.P256
end)

let chain_id_encoding =
  make_encode_fixed_size
    ~name:"ENCODING_CHAIN_ID"
    ~encoding:Chain_id.encoding
    ~generator:(fun rng_state ->
      Chain_id.hash_bytes
        [Base_samplers.bytes ~size:{min = 32; max = 32} rng_state])

let () = Registration.register chain_id_encoding

let chain_id_decoding =
  make_encode_fixed_size
    ~name:"DECODING_CHAIN_ID"
    ~encoding:Chain_id.encoding
    ~generator:(fun rng_state ->
      Chain_id.hash_bytes
        [Base_samplers.bytes ~size:{min = 32; max = 32} rng_state])

let () = Registration.register chain_id_decoding

let chain_id_readable_encoding =
  make_encode_fixed_size_to_string
    ~name:"B58CHECK_ENCODING_CHAIN_ID"
    ~to_string:Chain_id.to_b58check
    ~generator:(fun rng_state ->
      Chain_id.hash_bytes
        [Base_samplers.bytes ~size:{min = 32; max = 32} rng_state])

let () = Registration.register chain_id_readable_encoding

let chain_id_readable_decoding =
  make_decode_fixed_size_from_string
    ~name:"B58CHECK_DECODING_CHAIN_ID"
    ~to_string:Chain_id.to_b58check
    ~from_string:Chain_id.of_b58check_exn
    ~generator:(fun rng_state ->
      Chain_id.hash_bytes
        [Base_samplers.bytes ~size:{min = 32; max = 32} rng_state])

let () = Registration.register chain_id_readable_decoding
