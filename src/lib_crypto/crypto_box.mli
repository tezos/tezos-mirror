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

(** Tezos - X25519/XSalsa20-Poly1305 cryptography *)

type nonce

val nonce_size : int

val zero_nonce : nonce

val random_nonce : unit -> nonce

val increment_nonce : ?step:int -> nonce -> nonce

(** [generate_nonces ~incoming ~sent_msg ~recv_msg] generates two
    nonces by hashing (Blake2B) the arguments. The nonces should be
    used to initialize the encryption on the communication
    channels. Because an attacker cannot control both messages,
    it cannot determine the nonces that will be used to encrypt
    the messages. The sent message should contains a random nonce,
    and we should never send the exact same message twice. *)
val generate_nonces :
  incoming:bool -> sent_msg:Bytes.t -> recv_msg:Bytes.t -> nonce * nonce

(** Size of the message authentication tag. *)
val tag_length : int

module Secretbox : sig
  type key

  val unsafe_of_bytes : Bytes.t -> key

  (** [secretbox key msg nonce] encrypts and authenticates the data in
      [msg] using [key] and [nonce] and returns the authentication tag and the
      ciphertext in one buffer. For this reason, the returned buffer will be
      [tag_length] longer than [msg]. *)
  val secretbox : key -> Bytes.t -> nonce -> Bytes.t

  (** [secretbox_open key cmsg nonce] verifies and decrypts [cmsg] using [key]
      and [nonce] and returns the plaintext if successful. As above,
      the returned buffer will be [tag_length] shorter than [cmsg]. *)
  val secretbox_open : key -> Bytes.t -> nonce -> Bytes.t option
end

type target

val default_target : target

val make_target : float -> target

type secret_key

type public_key

module Public_key_hash : S.HASH

type channel_key

val hash : public_key -> Public_key_hash.t

(** Generates both a secret key and its corresponding public
    key, along with a hash of the public key. *)
val random_keypair : unit -> secret_key * public_key * Public_key_hash.t

(** [precompute pk sk] computes a channel key from the sender's [sk] and the
    recipient's [pk]. *)
val precompute : secret_key -> public_key -> channel_key

(** [fast_box k nonce msg] authenticates and encrypts [msg] and returns both
    the message authentication tag and the ciphertext. For this reason, the
    returned buffer will be [tagbytes] longer than [msg]. *)
val fast_box : channel_key -> nonce -> Bytes.t -> Bytes.t

(** [fast_box_open k nonce cmsg] attempts to verify and decrypt [cmsg] and
    if successful returns the plaintext. As above, the returned buffer will be
    [tagbytes] shorter than [cmsg]. *)
val fast_box_open : channel_key -> nonce -> Bytes.t -> Bytes.t option

(** [fast_box_noalloc k nonce tag buf] authenticates and encrypts in-place
    the contents of [buf] using [k] and [nonce] and writes the message
    authentication tag in [tag]. *)
val fast_box_noalloc : channel_key -> nonce -> Bytes.t -> Bytes.t -> unit

(** [fast_box_open_noalloc k nonce tag buf] attempts to verify and decrypt
    the contents of [buf] in-place using [k], [nonce], and [tag] and
    returns true if successful. *)
val fast_box_open_noalloc : channel_key -> nonce -> Bytes.t -> Bytes.t -> bool

val check_proof_of_work : public_key -> nonce -> target -> bool

val generate_proof_of_work : ?max:int -> public_key -> target -> nonce

val public_key_to_bytes : public_key -> Bytes.t

val public_key_of_bytes : Bytes.t -> public_key

val public_key_size : int

val secret_key_size : int

val public_key_encoding : public_key Data_encoding.t

val secret_key_encoding : secret_key Data_encoding.t

val nonce_encoding : nonce Data_encoding.t

(** [neuterize sk] generates the corresponding public key of [sk] *)
val neuterize : secret_key -> public_key

(** [equal a b] tests keys for equality *)
val equal : public_key -> public_key -> bool

val pp_pk : Format.formatter -> public_key -> unit
