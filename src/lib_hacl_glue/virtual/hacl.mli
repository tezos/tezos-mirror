(* The MIT License (MIT)
 *
 *   Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 *   Permission is hereby granted, free of charge, to any person obtaining a copy
 *   of this software and associated documentation files (the "Software"), to deal
 *   in the Software without restriction, including without limitation the rights
 *   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *   copies of the Software, and to permit persons to whom the Software is
 *   furnished to do so, subject to the following conditions:
 *
 *   The above copyright notice and this permission notice shall be included in all
 *   copies or substantial portions of the Software.
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *   SOFTWARE. *)

type secret

type public

module Rand : sig
  (** [write buf] fills the contents of [buf] with random bytes *)
  val write : Bytes.t -> unit

  (** [gen len] is a random buffer of length [len]. *)
  val gen : int -> Bytes.t
end

module Hash : sig
  module type DIRECT_HASH = sig
    (** [size] of the digest *)
    val size : int

    (** [digest msg] hashes [msg] to generate a [size] bytes long digest *)
    val digest : Bytes.t -> Bytes.t
  end

  module type INCREMENTAL_HASH = sig
    (** For hashes that offer an incremental interface, the internal state
        of the algorithm is kept in an object of type [state] which is
        initialised using [init] and needs to be stored and passed as an
        argument to the other functions in the interface. It is automatically
        freed when no longer required. *)
    type state

    (** [init ()] initializes and returns an internal state of the hash
        algorithm *)
    val init : unit -> state

    (** [update st msg] updates the internal state of the hash algorithm [st]
        with buffer [msg]. *)
    val update : state -> Bytes.t -> unit

    (** [finish st] generates and returns a digest, without invalidating the
        state, meaning that further calls to [update] can be made *)
    val finish : state -> Bytes.t
  end

  module type S = sig
    include DIRECT_HASH

    include INCREMENTAL_HASH

    module HMAC : sig
      (** [digest key msg] HMAC based on the hash algorithm of the module.
          The size of the produced tag is the same as the size of the hash
          digest. *)
      val digest : key:Bytes.t -> msg:Bytes.t -> Bytes.t
    end
  end

  module SHA256 : S

  module SHA512 : S

  module SHA3_256 : DIRECT_HASH

  module SHA3_512 : DIRECT_HASH

  module Keccak_256 : DIRECT_HASH
end

module Blake2b : sig
  type t

  (** This type could be unboxed. However, forward dependencies may
     hash values of this type, and unboxing this type would give a
     different hash, hence breaking the compatibility. *)
  type hash = Hash of Bytes.t

  (** [direct ?key inbuf len] is the Blake2b hash of length [len],
      using [key] if present. *)
  val direct : ?key:Bytes.t -> Bytes.t -> int -> hash
end

module Nonce : sig
  (** Functions for generating and incrementing nonces used in the NaCl API. *)
  type t = Bytes.t

  val size : int

  val gen : unit -> t

  val increment : ?step:int -> t -> t

  val of_bytes : Bytes.t -> t option

  val of_bytes_exn : Bytes.t -> t
end

module Secretbox : sig
  type key

  (** Size of the secret key *)
  val keybytes : int

  (** Size of the message authentication tag *)
  val tagbytes : int

  (** @raise Invalid_argument if argument is not [keybytes] bytes long *)
  val unsafe_of_bytes : Bytes.t -> key

  (** @raise Invalid_argument if argument is not [keybytes] bytes long *)
  val blit_of_bytes : Bytes.t -> int -> key

  (** [genkey ()] generates a secretbox key. *)
  val genkey : unit -> key

  (** [secretbox key nonce msg cmsg] encrypts and authenticates the data in
      [msg] using [key] and [nonce] and outputs the authentication tag and
      the ciphertext in one buffer [cmsg]. For this reason, [csmg] needs to be
      [tagbytes] longer than [msg]. *)
  val secretbox :
    key:key -> nonce:Bytes.t -> msg:Bytes.t -> cmsg:Bytes.t -> unit

  (** [secretbox_open key nonce cmsg msg] verifies and decrypts [cmsg] using
      [key] and [nonce] and outputs the plaintext in [msg]. As above, [msg] is
      expected to be [tagbytes] shorter than [cmsg]. Returns true if operation
      has succeeded, false otherwise. *)
  val secretbox_open :
    key:key -> nonce:Bytes.t -> cmsg:Bytes.t -> msg:Bytes.t -> bool
end

module Box : sig
  (** To increase performance, Box uses the precomputation interface, in which
      rather than passing the public and secret keys separately each time, it
      first computes a combined key thus avoiding to repeat this step for every
      call. *)
  type combined

  type _ key

  (** Size of the secret key *)
  val skbytes : int

  (** Size of the public key *)
  val pkbytes : int

  (** Size of the combined key *)
  val ckbytes : int

  (** Size of the message authentication tag *)
  val tagbytes : int

  val equal : 'a key -> 'a key -> bool

  (** [unsafe_to_bytes k] is the internal [Bytes.t] where the key
      is stored. DO NOT MODIFY. *)
  val unsafe_to_bytes : _ key -> Bytes.t

  val blit_to_bytes : _ key -> ?pos:int -> Bytes.t -> unit

  (** @raise Invalid_argument if argument is not [skbytes] bytes long *)
  val unsafe_sk_of_bytes : Bytes.t -> secret key

  (** @raise Invalid_argument if argument is not [pkbytes] bytes long *)
  val unsafe_pk_of_bytes : Bytes.t -> public key

  (** @raise Invalid_argument if argument is not [ckbytes] bytes long *)
  val unsafe_ck_of_bytes : Bytes.t -> combined key

  (** @raise Invalid_argument if [pos] is outside the buffer or the buffer
      is less than [skbytes] bytes long *)
  val of_seed : ?pos:int -> Bytes.t -> secret key

  (** [neuterize sk] generates the corresponding public key of [sk]. *)
  val neuterize : secret key -> public key

  (** [keypair] generates both a secret key and its corresponding public key. *)
  val keypair : unit -> public key * secret key

  (** [dh pk sk] computes the combined key from the sender's [sk] and the
      recipient's [pk]. *)
  val dh : public key -> secret key -> combined key

  (** [box k nonce msg cmsg] authenticates and encrypts [msg] and writes both
      the message authentication tag and the ciphertext in [cmsg]. For this
      reason, [csmg] needs to be [tagbytes] longer than [msg]. *)
  val box :
    k:combined key -> nonce:Bytes.t -> msg:Bytes.t -> cmsg:Bytes.t -> unit

  (** [box_open key nonce cmsg msg] attempts to verify and decrypt [cmsg] and
      if successful writes the plaintext in [msg]. As above, [msg] is expected
      to be [tagbytes] shorter than [cmsg]. Returns true if operation has
      succeeded, false otherwise. *)
  val box_open :
    k:combined key -> nonce:Bytes.t -> cmsg:Bytes.t -> msg:Bytes.t -> bool

  (** [box_noalloc k nonce tag buf] authenticates and encrypts in-place
      the contents of [buf] using [k] and [nonce] and writes the message
      authentication tag in [tag]. *)
  val box_noalloc :
    k:combined key -> nonce:Bytes.t -> tag:Bytes.t -> buf:Bytes.t -> unit

  (** [box_open_noalloc k nonce tag buf] attempts to verify and decrypt
      the contents of [buf] in-place using [k], [nonce], and [tag] and
      returns true if successful. *)
  val box_open_noalloc :
    k:combined key -> nonce:Bytes.t -> tag:Bytes.t -> buf:Bytes.t -> bool
end

module type SIGNATURE = sig
  type _ key

  (** Size of a signature*)
  val size : int

  (** Size of a public key *)
  val pk_size : int

  (** Size of a secret key *)
  val sk_size : int

  val compare : 'a key -> 'a key -> int

  val equal : 'a key -> 'a key -> bool

  val sk_of_bytes : Bytes.t -> secret key option

  val pk_of_bytes : Bytes.t -> public key option

  (** This function does not check that the resulting pk is valid. It is
      slightly faster than pk_of_bytes but should only be used to
      deserialize bytes that were serialized from a pk. E.g. the pks in
      the storage of the protocol. *)
  val pk_of_bytes_without_validation : Bytes.t -> public key option

  (** [neuterize sk] generates the corresponding public key of [sk] *)
  val neuterize : 'a key -> public key

  (** [keypair] generates both a secret key and its corresponding public key *)
  val keypair : unit -> public key * secret key

  (** [to_bytes key] returns the contents of the key. For P-256 public keys, it
      returns the compressed form (see hacl.ml for more details) *)
  val to_bytes : _ key -> Bytes.t

  (** [blit_to_bytes key ?pos buf] copies all of the bytes of [key]
      into [buf] starting at position [pos]. As above, P-256 public keys are
      written in compressed form. *)
  val blit_to_bytes : _ key -> ?pos:int -> Bytes.t -> unit

  (** [sign sk msg] returns the signature of [msg] with [sk] *)
  val sign : sk:secret key -> msg:Bytes.t -> Bytes.t

  (** [verify pk msg signature] attempts to verify [msg] with [signature] and
      returns true if successful *)
  val verify : pk:public key -> msg:Bytes.t -> signature:Bytes.t -> bool
end

module Ed25519 : SIGNATURE

module P256 : SIGNATURE
