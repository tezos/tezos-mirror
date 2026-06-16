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

(** Tezos Protocol Implementation - Random number generation

    This is not expected to be a good cryptographic random number
    generator. In particular this is supposed to be used in situations
    where the seed is a globally known information.

    The only expected property is: It should be difficult to find a
    seed such that the generated sequence is a given one. *)

(** {2 Random Generation} *)

(** A random seed, to derive random sequences from *)
type seed

(** A VDF discriminant and challenge *)
type vdf_setup = Vdf.discriminant * Vdf.challenge

(** A VDF result, to derive a seed from *)
type vdf_solution = Vdf.result * Vdf.proof

val pp_solution : Format.formatter -> vdf_solution -> unit

(** Compare only the first element of two vdf_solution, that are
    of [Vdf.result]. *)
val compare_vdf_solution : vdf_solution -> vdf_solution -> int

val generate_vdf_setup :
  seed_discriminant:seed -> seed_challenge:seed -> vdf_setup

val verify : vdf_setup -> Int64.t -> vdf_solution -> bool option

val vdf_to_seed : seed -> vdf_solution -> seed

(** {2 Entropy} *)

(** A nonce for adding entropy to the generator *)
type nonce

(** Add entropy to the seed generator *)
val update_seed : seed -> nonce -> seed

(** Use a byte sequence as a nonce *)
val make_nonce : bytes -> nonce tzresult

(** Compute the hash of a nonce *)
val hash : nonce -> Nonce_hash.t

(** [check_hash nonce hash] is true if the nonce correspond to the hash *)
val check_hash : nonce -> Nonce_hash.t -> bool

(** For using nonce hashes as keys in the hierarchical database *)
val nonce_hash_key_part : Nonce_hash.t -> string list -> string list

(** Returns a new seed by hashing the one passed with a constant. *)
val deterministic_seed : seed -> seed

(** [initial_seeds n] generates the first [n] seeds for which there are no nonces.
    The first seed is a constant value. The kth seed is the hash of seed (k-1)
    concatenated with a constant. If an [initial_seed] is provided, the
    {i first} seed is created using it as the first one. *)
val initial_seeds : ?initial_seed:State_hash.t -> int -> seed list

(** {2 Serializers} *)

val nonce_encoding : nonce Data_encoding.t

val seed_encoding : seed Data_encoding.t

val vdf_setup_encoding : vdf_setup Data_encoding.t

val vdf_solution_encoding : vdf_solution Data_encoding.t

type seed_status = RANDAO_seed | VDF_seed

val seed_status_encoding : seed_status Data_encoding.t
