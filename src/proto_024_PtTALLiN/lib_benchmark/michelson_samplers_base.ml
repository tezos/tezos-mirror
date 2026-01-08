(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Sampling_helpers

(** Parameters for basic samplers *)
type parameters = {
  int_size : Base_samplers.range;
  string_size : Base_samplers.range;
  bytes_size : Base_samplers.range;
}

(** Encoding for basic samplers parameters *)
let parameters_encoding =
  let open Data_encoding in
  let range = Base_samplers.range_encoding in
  conv
    (fun {int_size; string_size; bytes_size} ->
      (int_size, string_size, bytes_size))
    (fun (int_size, string_size, bytes_size) ->
      {int_size; string_size; bytes_size})
    (obj3
       (req "int_size" range)
       (req "string_size" range)
       (req "bytes_size" range))

(** A module of type [S] packs samplers used to construct basic Michelson values. *)
module type S = sig
  val int : Script_int.z Script_int.num sampler

  val nat : Script_int.n Script_int.num sampler

  val signature : Tezos_crypto.Signature.t sampler

  val string : Script_string.t sampler

  val bytes : bytes sampler

  val tez : Alpha_context.Tez.t sampler

  val timestamp : Script_timestamp.t sampler
end

(* Samplers for basic Michelson types. *)

module Make (P : sig
  val parameters : parameters
end) : S = struct
  let int rng_state =
    let i = Base_samplers.int ~size:P.parameters.int_size rng_state in
    Script_int.of_zint i

  let nat rng_state =
    let i = Base_samplers.nat ~size:P.parameters.int_size rng_state in
    Script_int.abs (Script_int.of_zint i)

  let signature rng_state =
    let i = Random.State.int rng_state 5 in
    match i with
    | 0 -> (
        let open Tezos_crypto.Signature.Ed25519 in
        let bytes = Base_samplers.uniform_bytes ~nbytes:size rng_state in
        match of_bytes_opt bytes with
        | None -> assert false
        | Some s -> Tezos_crypto.Signature.of_ed25519 s)
    | 1 -> (
        let open Tezos_crypto.Signature.Secp256k1 in
        let bytes = Base_samplers.uniform_bytes ~nbytes:size rng_state in
        match of_bytes_opt bytes with
        | None -> assert false
        | Some s -> Tezos_crypto.Signature.of_secp256k1 s)
    | 2 -> (
        let open Tezos_crypto.Signature.P256 in
        let bytes = Base_samplers.uniform_bytes ~nbytes:size rng_state in
        match of_bytes_opt bytes with
        | None -> assert false
        | Some s -> Tezos_crypto.Signature.of_p256 s)
    | 3 ->
        (* BLS checks that signatures are on the curve so we need to generate real
           ones by signing a message. *)
        let open Tezos_crypto.Signature.Bls in
        let msg = Base_samplers.uniform_bytes ~nbytes:32 rng_state in
        let seed = Base_samplers.uniform_bytes ~nbytes:32 rng_state in
        let _, _, sk = generate_key ~seed () in
        Tezos_crypto.Signature.of_bls (sign sk msg)
    | _ ->
        let open Tezos_crypto.Signature in
        let bytes =
          Base_samplers.uniform_bytes
            ~nbytes:Tezos_crypto.Signature.Ed25519.size
            rng_state
        in
        Unknown bytes

  let string rng_state =
    let s =
      Base_samplers.readable_ascii_string
        ~size:P.parameters.string_size
        rng_state
    in
    match Protocol.Script_string.of_string s with
    | Ok s -> s
    | Error _ -> assert false

  let bytes = Base_samplers.bytes ~size:P.parameters.bytes_size

  let tez rng_state =
    let i = Random.State.int64 rng_state (Int64.of_int max_int) in
    match Protocol.Alpha_context.Tez.of_mutez i with
    | Some res -> res
    | None -> assert false

  let timestamp rng_state =
    let i = Base_samplers.int ~size:P.parameters.int_size rng_state in
    Script_timestamp.of_zint i
end
