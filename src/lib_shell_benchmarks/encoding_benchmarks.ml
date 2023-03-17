(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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
open Benchmarks_shell

open Encoding_benchmarks_helpers.Make (struct
  let file = __FILE__
end)
(* ------------------------------------------------------------------------- *)

module Make_elliptic_curve_encoding_benchmarks (A : sig
  val algo : Tezos_crypto.Signature.algo
end) =
struct
  let algo_name =
    match A.algo with
    | Tezos_crypto.Signature.Ed25519 -> "ed25519"
    | Tezos_crypto.Signature.Secp256k1 -> "secp256k1"
    | Tezos_crypto.Signature.P256 -> "p256"
    | Tezos_crypto.Signature.Bls -> "bls"

  module Sampler = Crypto_samplers.Make_finite_key_pool (struct
    let size = 256

    let algo = `Algo A.algo
  end)

  (* encoding benches *)

  let public_key_encoding =
    make_encode_fixed_size
      ~name:("ENCODING_PUBLIC_KEY_" ^ algo_name)
      ~encoding:Tezos_crypto.Signature.Public_key.encoding
      ~generator:Sampler.pk
      ()

  let () = Registration.register public_key_encoding

  let public_key_to_b58check =
    make_encode_fixed_size_to_string
      ~name:("B58CHECK_ENCODING_PUBLIC_KEY_" ^ algo_name)
      ~to_string:Tezos_crypto.Signature.Public_key.to_b58check
      ~generator:Sampler.pk
      ()

  let () = Registration.register public_key_to_b58check

  let public_key_hash_encoding =
    make_encode_fixed_size
      ~name:("ENCODING_PUBLIC_KEY_HASH_" ^ algo_name)
      ~encoding:Tezos_crypto.Signature.Public_key_hash.encoding
      ~generator:Sampler.pkh
      ()

  let () = Registration.register public_key_hash_encoding

  let public_key_hash_to_b58check =
    make_encode_fixed_size_to_string
      ~name:("B58CHECK_ENCODING_PUBLIC_KEY_HASH_" ^ algo_name)
      ~to_string:Tezos_crypto.Signature.Public_key_hash.to_b58check
      ~generator:Sampler.pkh
      ()

  let () = Registration.register public_key_hash_to_b58check

  let secret_key_encoding =
    make_encode_fixed_size
      ~name:("ENCODING_SECRET_KEY_" ^ algo_name)
      ~encoding:Tezos_crypto.Signature.Public_key_hash.encoding
      ~generator:Sampler.pkh
      ()

  let () = Registration.register secret_key_encoding

  let secret_key_to_b58check =
    make_encode_fixed_size_to_string
      ~name:("B58CHECK_ENCODING_SECRET_KEY_" ^ algo_name)
      ~to_string:Tezos_crypto.Signature.Secret_key.to_b58check
      ~generator:Sampler.sk
      ()

  let () = Registration.register secret_key_to_b58check

  let signature_encoding =
    make_encode_fixed_size
      ~name:("ENCODING_SIGNATURE_" ^ algo_name)
      ~encoding:Tezos_crypto.Signature.encoding
      ~generator:(fun _rng_state -> Tezos_crypto.Signature.zero)
      ()

  let () = Registration.register signature_encoding

  let signature_to_b58check =
    make_encode_fixed_size_to_string
      ~name:("B58CHECK_ENCODING_SIGNATURE_" ^ algo_name)
      ~to_string:Tezos_crypto.Signature.to_b58check
      ~generator:(fun _rng_state -> Tezos_crypto.Signature.zero)
      ()

  let () = Registration.register signature_to_b58check

  (* decoding benches *)

  let public_key_decoding =
    make_decode_fixed_size
      ~name:("DECODING_PUBLIC_KEY_" ^ algo_name)
      ~encoding:Tezos_crypto.Signature.Public_key.encoding
      ~generator:Sampler.pk
      ()

  let () = Registration.register public_key_decoding

  let public_key_from_b58check =
    make_decode_fixed_size_from_string
      ~name:("B58CHECK_DECODING_PUBLIC_KEY_" ^ algo_name)
      ~to_string:Tezos_crypto.Signature.Public_key.to_b58check
      ~from_string:Tezos_crypto.Signature.Public_key.of_b58check_exn
      ~generator:Sampler.pk
      ()

  let () = Registration.register public_key_from_b58check

  let public_key_hash_decoding =
    make_decode_fixed_size
      ~name:("DECODING_PUBLIC_KEY_HASH_" ^ algo_name)
      ~encoding:Tezos_crypto.Signature.Public_key_hash.encoding
      ~generator:Sampler.pkh
      ()

  let () = Registration.register public_key_hash_decoding

  let public_key_hash_from_b58check =
    make_decode_fixed_size_from_string
      ~name:("B58CHECK_DECODING_PUBLIC_KEY_HASH_" ^ algo_name)
      ~to_string:Tezos_crypto.Signature.Public_key_hash.to_b58check
      ~from_string:Tezos_crypto.Signature.Public_key_hash.of_b58check_exn
      ~generator:Sampler.pkh
      ()

  let () = Registration.register public_key_hash_from_b58check

  let secret_key_decoding =
    make_decode_fixed_size
      ~name:("DECODING_SECRET_KEY_" ^ algo_name)
      ~encoding:Tezos_crypto.Signature.Secret_key.encoding
      ~generator:Sampler.sk
      ()

  let () = Registration.register secret_key_decoding

  let secret_key_from_b58check =
    make_decode_fixed_size_from_string
      ~name:("B58CHECK_DECODING_SECRET_KEY_" ^ algo_name)
      ~to_string:Tezos_crypto.Signature.Secret_key.to_b58check
      ~from_string:Tezos_crypto.Signature.Secret_key.of_b58check_exn
      ~generator:Sampler.sk
      ()

  let () = Registration.register secret_key_from_b58check

  let signature_decoding =
    make_decode_fixed_size
      ~name:("DECODING_SIGNATURE_" ^ algo_name)
      ~encoding:Tezos_crypto.Signature.encoding
      ~generator:(fun _rng_state -> Tezos_crypto.Signature.zero)
      ()

  let () = Registration.register signature_decoding

  let signature_from_b58check =
    make_decode_fixed_size_from_string
      ~name:("B58CHECK_DECODING_SIGNATURE_" ^ algo_name)
      ~to_string:Tezos_crypto.Signature.to_b58check
      ~from_string:Tezos_crypto.Signature.of_b58check_exn
      ~generator:(fun _rng_state -> Tezos_crypto.Signature.zero)
      ()

  let () = Registration.register signature_from_b58check
end

module Ed25519 = Make_elliptic_curve_encoding_benchmarks (struct
  let algo = Tezos_crypto.Signature.Ed25519
end)

module Secp256k1 = Make_elliptic_curve_encoding_benchmarks (struct
  let algo = Tezos_crypto.Signature.Secp256k1
end)

module P256 = Make_elliptic_curve_encoding_benchmarks (struct
  let algo = Tezos_crypto.Signature.P256
end)

module Bls = Make_elliptic_curve_encoding_benchmarks (struct
  let algo = Tezos_crypto.Signature.Bls
end)

let chain_id_encoding =
  make_encode_fixed_size
    ~name:"ENCODING_CHAIN_ID"
    ~encoding:Chain_id.encoding
    ~generator:(fun rng_state ->
      Chain_id.hash_bytes
        [Base_samplers.bytes ~size:{min = 32; max = 32} rng_state])
    ()

let () = Registration.register chain_id_encoding

let chain_id_decoding =
  make_encode_fixed_size
    ~name:"DECODING_CHAIN_ID"
    ~encoding:Chain_id.encoding
    ~generator:(fun rng_state ->
      Chain_id.hash_bytes
        [Base_samplers.bytes ~size:{min = 32; max = 32} rng_state])
    ()

let () = Registration.register chain_id_decoding

let chain_id_readable_encoding =
  make_encode_fixed_size_to_string
    ~name:"B58CHECK_ENCODING_CHAIN_ID"
    ~to_string:Chain_id.to_b58check
    ~generator:(fun rng_state ->
      Chain_id.hash_bytes
        [Base_samplers.bytes ~size:{min = 32; max = 32} rng_state])
    ()

let () = Registration.register chain_id_readable_encoding

let chain_id_readable_decoding =
  make_decode_fixed_size_from_string
    ~name:"B58CHECK_DECODING_CHAIN_ID"
    ~to_string:Chain_id.to_b58check
    ~from_string:Chain_id.of_b58check_exn
    ~generator:(fun rng_state ->
      Chain_id.hash_bytes
        [Base_samplers.bytes ~size:{min = 32; max = 32} rng_state])
    ()

let () = Registration.register chain_id_readable_decoding

(** The following benchmarks are not used directly to define precise
    gas models but the inferred gas constants may be used for
    over-approximating other costs or simply as reference points. *)

let nat_encoding =
  make_encode_variable_size
    ~name:"ENCODING_NAT"
    ~encoding:Data_encoding.n
    ~generator:(fun rng_state ->
      let nbytes =
        Base_samplers.sample_in_interval ~range:{min = 0; max = 32000} rng_state
      in
      let n = Base_samplers.uniform_nat ~nbytes rng_state in
      (n, {bytes = nbytes}))
    ()

let () = Registration.register nat_encoding

let nat_decoding =
  make_decode_variable_size
    ~name:"DECODING_NAT"
    ~encoding:Data_encoding.n
    ~generator:(fun rng_state ->
      let nbytes =
        Base_samplers.sample_in_interval ~range:{min = 0; max = 32000} rng_state
      in
      let n = Base_samplers.uniform_nat ~nbytes rng_state in
      (n, {bytes = nbytes}))
    ()

let () = Registration.register nat_decoding

let int_encoding =
  make_encode_variable_size
    ~name:"ENCODING_INT"
    ~encoding:Data_encoding.z
    ~generator:(fun rng_state ->
      let nbytes =
        Base_samplers.sample_in_interval ~range:{min = 0; max = 32000} rng_state
      in
      let n = Base_samplers.uniform_int ~nbytes rng_state in
      (n, {bytes = nbytes}))
    ()

let () = Registration.register int_encoding

let int_decoding =
  make_decode_variable_size
    ~name:"DECODING_INT"
    ~encoding:Data_encoding.z
    ~generator:(fun rng_state ->
      let nbytes =
        Base_samplers.sample_in_interval ~range:{min = 0; max = 32000} rng_state
      in
      let n = Base_samplers.uniform_int ~nbytes rng_state in
      (n, {bytes = nbytes}))
    ()

let () = Registration.register int_decoding

let string_encoding =
  make_encode_variable_size
    ~name:"ENCODING_STRING"
    ~encoding:Data_encoding.string
    ~generator:(fun rng_state ->
      let nbytes =
        Base_samplers.sample_in_interval ~range:{min = 0; max = 32000} rng_state
      in
      let s = Base_samplers.uniform_string ~nbytes rng_state in
      (s, {bytes = nbytes}))
    ()

let () = Registration.register string_encoding

let string_decoding =
  make_decode_variable_size
    ~name:"DECODING_STRING"
    ~encoding:Data_encoding.string
    ~generator:(fun rng_state ->
      let nbytes =
        Base_samplers.sample_in_interval ~range:{min = 0; max = 32000} rng_state
      in
      let s = Base_samplers.uniform_string ~nbytes rng_state in
      (s, {bytes = nbytes}))
    ()

let () = Registration.register string_decoding

let adversarial_string_list_encoding =
  make_encode_variable_size
    ~name:"ENCODING_STRING_LIST_ADVERSARIAL"
    ~encoding:Data_encoding.(list string)
    ~generator:(fun rng_state ->
      let length =
        Base_samplers.sample_in_interval ~range:{min = 0; max = 32000} rng_state
      in
      let l =
        Stdlib.List.init length (fun _ ->
            String.make 1 (Base_samplers.uniform_byte rng_state))
      in
      (l, {bytes = Data_encoding.(Binary.length (list string) l)}))
    ()

let () = Registration.register adversarial_string_list_encoding

let adversarial_string_list_decoding =
  make_decode_variable_size
    ~name:"DECODING_STRING_LIST_ADVERSARIAL"
    ~encoding:Data_encoding.(list string)
    ~generator:(fun rng_state ->
      let length =
        Base_samplers.sample_in_interval ~range:{min = 0; max = 32000} rng_state
      in
      let l =
        Stdlib.List.init length (fun _ ->
            String.make 1 (Base_samplers.uniform_byte rng_state))
      in
      (l, {bytes = Data_encoding.(Binary.length (list string) l)}))
    ()

let () = Registration.register adversarial_string_list_decoding
