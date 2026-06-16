(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Size of a group element, also called form, in bytes *)
val form_size_bytes : int

(** Size of the class group discriminant in bytes *)
val discriminant_size_bytes : int

(** Class group discriminant, prime number uniquely defining a class group *)
type discriminant

(** VDF challenge *)
type challenge

(** VDF result *)
type result

(** VDF proof *)
type proof

(** VDF difficulty, that is log of the number of group element compositions
    done in the prover *)
type difficulty = Int64.t

val discriminant_to_bytes : discriminant -> bytes

val discriminant_of_bytes_opt : bytes -> discriminant option

val challenge_to_bytes : challenge -> bytes

val challenge_of_bytes_opt : bytes -> challenge option

val result_to_bytes : result -> bytes

val result_of_bytes_opt : bytes -> result option

val proof_to_bytes : proof -> bytes

val proof_of_bytes_opt : bytes -> proof option

(** [generate_discriminant ?seed size], function generating a
    discriminant/group *)
val generate_discriminant : ?seed:Bytes.t -> int -> discriminant

(** [generate_challenge discriminant seed], function generating a class group
    element used as a VDF challenge *)
val generate_challenge : discriminant -> Bytes.t -> challenge

(** [prove_vdf discriminant challenge difficulty], function taking a class
    group/discriminant, a vdf challenge and a difficulty and returning a vdf
    result and proof *)
val prove : discriminant -> challenge -> difficulty -> result * proof

(** [verify_vdf discriminant challenge difficulty result proof] function taking
    a class group/discriminant, a vdf challenge, difficulty, result and proof and
    returning true if the proof verifies else false

    @raise Invalid_argument when inputs are invalid *)
val verify : discriminant -> challenge -> difficulty -> result -> proof -> bool
