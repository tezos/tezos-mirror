(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** The type of a reveal hash. *)
type t

(** The hashing schemes supported by the reveal hash. *)
type supported_hashes = Blake2B

(** A Map module for storing reveal-hash-indexed values. *)
module Map : Map.S with type key = t

(** [size ~scheme] returns the size of reveal hashes using the [scheme]
      specified in input. *)
val size : scheme:supported_hashes -> int

(** [zero ~scheme] returns the reveal hash corresponding to the zero hash
      for the [scheme] specified in input. *)
val zero : scheme:supported_hashes -> t

(** Formatting function for reveal-hashes. *)
val pp : Format.formatter -> t -> unit

(** [equal hash1 hash2] checks if the two reveal-hashes [hash1] and [hash2]
      are equal. This function must preserve the equality of individual
      supported hashing schemes. If [hash1] and [hash2] are hashes obtained
      from the same supported hashing scheme, then the [equal] function from
      that hashing scheme is used to determine whether they are equivalent.
      Otherwise, they are different. *)
val equal : t -> t -> bool

(** [compare hash1 hash2] compares the values of the reveal hashes [hash1]
      and [hash2]. This function must preserve the ordering of individual
      supported hashing scheme. If [hash1] and [hash2] are reveal-hashes
      obtained from the same hashing scheme, then [compare hash1 hash2]
      should return the same result of the compare function exposed
      by the hash module corresponding to their hashing scheme. *)
val compare : t -> t -> int

(** The encoding of reveal hashes. *)
val encoding : t Data_encoding.t

(** [hash_string ~scheme ?key strings] hashes [strings] using the
    supported hashing [scheme] given in input. *)
val hash_string : scheme:supported_hashes -> ?key:string -> string list -> t

(** [hash_bytes ~scheme ?key strings] hashes [bytes] using the
    supported hashing [scheme] given in input. *)
val hash_bytes : scheme:supported_hashes -> ?key:bytes -> bytes list -> t

(** [scheme_of_hash] hash returns the supported hashing scheme
    that was used to obtain [hash]. *)
val scheme_of_hash : t -> supported_hashes

val of_hex : string -> t option

val to_hex : t -> string

val rpc_arg : t RPC_arg.t
