(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com                *)
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

(** Contains a value (the decryption of the ciphertext) that can be provably
recovered in [time] sequential operation or with the rsa secret. *)
type chest

val chest_encoding : chest Data_encoding.t

(** Provably opens a chest in a short time. *)
type chest_key

val chest_key_encoding : chest_key Data_encoding.t

(** Result of the opening of a chest.
    The opening can fail if one provides a false unlocked_value or
    unlocked_proof, in which case we return [Bogus_opening] and the provider of
    the chest key is at fault.
    Otherwise we return [Correct payload] where payload was what had
    originally been put in the chest. *)
type opening_result = Correct of Bytes.t | Bogus_opening

(** Takes a chest, chest key and time and tries to recover the underlying
    plaintext. See the documentation of opening_result. *)
val open_chest : chest -> chest_key -> time:int -> opening_result

(** Gives the size of the underlying plaintext in a chest in bytes.
    Used for gas accounting*)
val get_plaintext_size : chest -> int
