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

    A time-locked value can either be opened quickly by the locker itself
    (i.e., the one possessing the RSA secret), or slowly by anyone doing a
    fixed number of sequential operations.

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

(** !!! WARNING !!!

    WE USE 2048 RSA KEYS WHICH DO NOT PROVIDE THE CLASSICAL 128 BITS OF
    SECURITY. WE ALLOW OURSELVES TO DO THAT SINCE WE DO NOT EXPOSE KEYS FOR A
    LONG TIME. YOU ARE RESPONSIBLE FOR NOT REUSING OLD KEYS

*)

(** We will time-lock symmetric keys to then handle arbitrary bytes *)
type symmetric_key

(** RSA public key to define a group in which we will work.
    The key is an integer n = p*q with p,q primes number. The group we work in is
    the set of inversible mod n. *)
type rsa_public

(** RSA secret key of the from p,q. Indicates the cardinal of the group. *)
type rsa_secret

(** Proof that the opening of a value is the claimed value.
    It is concretely a member of the RSA group. *)
type timelock_proof

(** Locked value that can be quickly access with a secret or slowly-access with
    a number of sequential operations.
    It is concretely a member of the RSA group. *)
type locked_value

(** Member of the RSA group that we will lock. In our case it represents a
    symmetric key. *)
type unlocked_value

(** A symmetric ciphertext and message authentication code, containing the bytes
    we want to protect *)
type ciphertext

(** Generates random RSA keys of 2048 bits.
    The size works only if we use them for a small amount of time.
    !!! NEW KEYS SHOULD BE GENERATED FOR EACH LOCKING !!!

    @raise Failure if there is not enough entropy available. *)
val gen_rsa_keys : unit -> rsa_public * rsa_secret

(** Generates almost uniformly an integer mod n.
    It is in the RSA group with overwhelming probability.
    We use this since we want to lock symmetric keys, not pre-determined messages.

    @raise Failure if there is not enough entropy available. *)
val gen_locked_value : rsa_public -> locked_value

(** Hashes a number mod n to a symmetric key for authenticated encryption. *)
val unlocked_value_to_symmetric_key : unlocked_value -> symmetric_key

(** Unlock a value using RSA secret and hash the result to derive a symmetric key using
    [unlocked_value_to_symmetric_key] *)
val locked_value_to_symmetric_key_with_secret :
  rsa_secret -> time:int -> locked_value -> symmetric_key

(** Unlock a value using the RSA secret. *)
val unlock_with_secret :
  rsa_secret -> time:int -> locked_value -> unlocked_value

(** Unlock a value using the RSA secret. Also produces a proof certifying
    that the result is indeed what had been locked. *)
val unlock_and_prove_with_secret :
  rsa_secret -> time:int -> locked_value -> unlocked_value * timelock_proof

(** Unlock a value the slow way, without the RSA secret. Also produces a proof certifying
    that the result is indeed what had been locked. *)
val unlock_and_prove_without_secret :
  rsa_public -> time:int -> locked_value -> unlocked_value * timelock_proof

val prove_without_secret :
  rsa_public -> time:int -> locked_value -> unlocked_value -> timelock_proof

val prove_with_secret :
  rsa_secret -> time:int -> locked_value -> unlocked_value -> timelock_proof

(** Verifies that [locked_value] indeed contains [unlocked_value] with parameters [rsa_public] and [time:Z.t]. *)
val verify_timelock :
  rsa_public ->
  time:int ->
  locked_value ->
  unlocked_value ->
  timelock_proof ->
  bool

(** Receives a claim opening with a proof.
    If the proof is valid hashes the opening using [unlocked_value_to_symmetric_key],
    returns None otherwise. *)
val locked_value_to_symmetric_key_with_proof :
  rsa_public ->
  time:int ->
  unlocked_value ->
  locked_value ->
  timelock_proof ->
  symmetric_key option

(** encrypt using authenticated encryption, i.e. ciphertext contains
    a ciphertext and a message authentication code. *)
val encrypt : symmetric_key -> bytes -> ciphertext

(** Checks the message authentication code. If correct decrypt the
    ciphertext, otherwise returns None. *)
val decrypt : symmetric_key -> ciphertext -> bytes option

val ciphertext_encoding : ciphertext Data_encoding.t

val proof_encoding : timelock_proof Data_encoding.t

(*------Exposed to the protocol----------*)

(** Contains a value (the decryption of the ciphertext) that can be provably
    recovered in [time] sequential operation or with the rsa secret. *)
type chest = {
  locked_value : locked_value;
  rsa_public : rsa_public;
  ciphertext : ciphertext;
}

val chest_encoding : chest Data_encoding.t

(** Provably opens a chest in a short time. *)
type chest_key = {unlocked_value : unlocked_value; proof : timelock_proof}

val chest_key_encoding : chest_key Data_encoding.t

(** Result of the opening of a chest.
    The opening can fail in two ways which we distinguish to blame the right party.
    One can provide a false unlocked_value or unlocked_proof, in which case
    we return [Bogus_opening] and the provider of the chest key is at fault.
    Othewise, one can lock the wrong key or put garbage in the ciphertext in which case
    we return [Bogus_cipher] and the provider of the chest is at fault.
    Otherwise we return [Correct payload] where [payload] is the content that had
    originally been put in the chest. *)

type opening_result = Correct of Bytes.t | Bogus_cipher | Bogus_opening

(** Takes a chest, chest key and time and tries to recover the underlying
    plaintext. See the documentation of opening_result. *)
val open_chest : chest -> chest_key -> time:int -> opening_result

(** Gives the size of the underlying plaintext in a chest in bytes.
    Used for gas accounting*)
val get_plaintext_size : chest -> int
(*----End protocol exposure -----*)

(** High level function which takes care of generating the locked value, the RSA
    parameters, and encrypt the payload. Also returns the chest key *)
val create_chest_and_chest_key :
  payload:Bytes.t -> time:int -> chest * chest_key

(** High level function which unlock the value and create the time-lock proof. *)
val create_chest_key : chest -> time:int -> chest_key

(**  ----- !!!!! Do not use for wallets: the RNG is not safe !!!!----
     Sampler for the gasbenchmarks. Takes an Ocaml RNG state as arg for
     reproducibility. *)
val chest_sampler :
  rng_state:Random.State.t ->
  plaintext_size:int ->
  time:int ->
  chest * chest_key
