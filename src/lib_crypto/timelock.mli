(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** [Timelock] is a set of functions to handle time-locking a value and opening
    time-locked values.

    A time-locked value can be opened slowly by anyone doing a fixed number of
    sequential operations.

    In the interface of this module, this fixed number is consistently named
    [time] and is represented by an integer.

    Once opened via the slow method a proof of opening can be produced to avoid
    having to do so again. This proof is verifiable in logarithmic time.

    In order to time-lock an arbitrary sequence of bytes, we
       1. encrypt the bytes with a symmetric key, and then
       2. we time-lock the symmetric key itself.

   This module implements a scheme inspired by:
   Time-lock puzzles and timed release - Rivest, Shamir, Wagner
   https://people.csail.mit.edu/rivest/pubs/RSW96.pdf
*)

(** We will time-lock symmetric keys to then handle arbitrary bytes *)
type symmetric_key

(** Locked value that can be accessed with a number of sequential operations.
    It is concretely a member of the RSA group. *)
type locked_value

(** Function taking as input a string and returning Some locked_value if the
    element is in the RSA group with RSA2048 as modulus, None otherwise. *)
val to_locked_value_opt : string -> locked_value option

(** Function taking as input a string and returning a locked_value with no
    check. *)
val to_locked_value_unsafe : string -> locked_value

(** Member of the RSA group that we will lock. In our case it represents a
    symmetric key. *)
type unlocked_value

(** VDF proof (Wesolowski). *)
type vdf_proof

(** A symmetric ciphertext and message authentication code, containing the
    bytes we want to protect *)
type ciphertext

(** Tuple of the RSA group comprising the locked and unlocked values as well as
    (Wesolowski) proof that the unlocked value indeed correspond to the locked
    one. *)
type vdf_tuple = {
  locked_value : locked_value;
  unlocked_value : unlocked_value;
  vdf_proof : vdf_proof;
}

(** Proof that the opening of a value is the claimed value.
    It is concretely an optional vdf_tuple and a member of the RSA
    group. *)
type timelock_proof = {vdf_tuple : vdf_tuple; nonce : Z.t}

(** Generates almost uniformly an integer mod n.
    It is in the RSA group with overwhelming probability.
    We use this since we want to lock symmetric keys, not pre-determined
    messages.

    @raise Failure if there is not enough entropy available. *)
val gen_locked_value_unsafe : unit -> locked_value

(** Returns None if [rsa_public] is not RSA2048, otherwise
    returns Some [gen_locked_value_unsafe] [rsa_public]. *)
val gen_locked_value_opt : unit -> locked_value option

(** Hashes a number mod n to a symmetric key for authenticated encryption,
    where the number is unlocked_value**nonce mod rsa2048. *)
val timelock_proof_to_symmetric_key : timelock_proof -> symmetric_key

(** Unlock a timelock value and produces a proof certifying that the result is
    indeed what had been locked. *)
val unlock_and_prove : time:int -> locked_value -> timelock_proof

(** Produces a proof certifying that the result is indeed what had been locked. *)
val prove : time:int -> locked_value -> unlocked_value -> timelock_proof

(** Verifies that [locked_value] indeed contains [unlocked_value] with
    parameters  [time:int]. *)
val verify : time:int -> locked_value -> timelock_proof -> bool

(** Precomputes a [vdf_tuple] given a [time:int] and optionally [locked_value].
    If [precompute_path] is given, it will instead read [vdf_tuple] locally and
    if not found, will write the newly computed [vdf_tuple] there. *)
val precompute_timelock :
  ?locked_value:locked_value option ->
  ?precompute_path:string option ->
  time:int ->
  unit ->
  vdf_tuple

(** Randomizes a [vdf_tuple] given a [time:int]
    (to verify the [vdf_tuple] is correct). *)
val proof_of_vdf_tuple : time:int -> vdf_tuple -> locked_value * timelock_proof

(** encrypt using authenticated encryption, i.e. ciphertext contains
    a ciphertext and a message authentication code. *)
val encrypt : symmetric_key -> bytes -> ciphertext

(** Checks the message authentication code. If correct decrypt the
    ciphertext, otherwise returns None. *)
val decrypt : symmetric_key -> ciphertext -> bytes option

val ciphertext_encoding : ciphertext Data_encoding.t

val vdf_tuple_encoding : vdf_tuple Data_encoding.t

val proof_encoding : timelock_proof Data_encoding.t

(* -------- Exposed to the protocol -------- *)

(** Contains a value (the decryption of the ciphertext) that can be provably
    recovered in [time] sequential operation. *)
type chest = {locked_value : locked_value; ciphertext : ciphertext}

val chest_encoding : chest Data_encoding.t

(** Provably opens a chest in a short time. *)
type chest_key = timelock_proof

val chest_key_encoding : chest_key Data_encoding.t

(** Result of the opening of a chest.
    The opening can fail in two ways which we distinguish to blame the right
    party. One can provide a false unlocked_value or unlocked_proof, in which
    case we return [Bogus_opening] and the provider of the chest key is at
    fault. Otherwise we return [Correct payload] where [payload] is
    the content that had originally been put in the chest. *)

type opening_result = Correct of Bytes.t | Bogus_opening

(** Takes a chest, chest key and time and tries to recover the underlying
    plaintext. See the documentation of opening_result. *)
val open_chest : chest -> chest_key -> time:int -> opening_result

(** Gives the size of the underlying plaintext in a chest in bytes.
    Used for gas accounting*)
val get_plaintext_size : chest -> int

module Internal_for_tests : sig
  val locked_value_to_z : locked_value -> Z.t

  val rsa2048 : Z.t

  val unlocked_value_to_z : unlocked_value -> Z.t

  val vdf_proof_to_z : vdf_proof -> Z.t

  val prove_wesolowski : time:int -> locked_value -> unlocked_value -> vdf_proof

  val verify_wesolowski : time:int -> vdf_tuple -> bool

  val to_vdf_tuple_unsafe : Z.t -> Z.t -> Z.t -> vdf_tuple

  val hash_to_prime : time:int -> locked_value -> unlocked_value -> Z.t
end

(*----End protocol exposure -----*)

(** High level function which given a [payload], [time] and optionally a
    [precomputed_path], generates a [chest] and [chest_key].
    The [payload] corresponds to the message to timelock while the [time]
    corresponds to the difficulty in opening the chest. Beware, it does not
    correspond to a duration per se but to the number of iteration needed.
    The optional [precomputed_path] is a local path where to read or write some
    auxiliary information to generate the chest quickly. *)
val create_chest_and_chest_key :
  ?precompute_path:string option ->
  payload:Bytes.t ->
  time:int ->
  unit ->
  chest * chest_key

(** High level function which unlock the value and create the time-lock
    proof. *)
val create_chest_key : chest -> time:int -> chest_key

(**  ----- !!!!! Do not use for wallets: the RNG is not safe !!!!----
     Sampler for the gasbenchmarks. Takes an Ocaml RNG state as arg for
     reproducibility. *)
val chest_sampler :
  rng_state:Random.State.t ->
  plaintext_size:int ->
  time:int ->
  chest * chest_key
