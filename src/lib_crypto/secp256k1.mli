(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

(** Tezos - Secp256k1 cryptography *)

include S.SIGNATURE with type watermark = Bytes.t

include S.RAW_DATA with type t := t

(** Signs the Keccak256 hash of bytes, instead of BLAKE2b. *)
val sign_keccak256 : Secret_key.t -> bytes -> t

(** Check the Keccak256 hash of bytes, instead of BLAKE2b. *)
val check_keccak256 : Public_key.t -> t -> bytes -> bool

(** [recover_caller signature msg] recovers the caller of signature
    [signature] on message [msg]. The "caller" in this context is
    the rightmost 160-bits of the Keccak-256 hash of the corresponding
    ECDSA public key. *)
val recover : bytes -> bytes -> (bytes, string) result

(** [eth_address_of_public_key pk] computes the Ethereum address of an
    Externally Owned Account (EOA) using this public key. *)
val eth_address_of_public_key : Public_key.t -> bytes
