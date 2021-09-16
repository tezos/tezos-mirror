(* MIT License
*
* Copyright (c) 2020 Danny Willems <be.danny.willems@gmail.com>
* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE. *)

(** Type of the public keys *)
type pk

(* Not abstracting the type to avoid to write (de)serialisation routines *)
type signature = Bytes.t

(** Build a value of type [pk] without performing any check on the input.
    It is safe to use this function when verifying a signature as the
    signature function verifies if the point is in the prime subgroup. Using
    [unsafe_pk_of_bytes] removes a verification performed twice when used
    [pk_of_bytes_exn] or [pk_of_bytes_opt].

    The expected bytes format are the compressed form of a point on G1. *)

val unsafe_pk_of_bytes : Bytes.t -> pk

(** Build a value of type [pk] safely, i.e. the function checks the bytes
    given in parameters represents a point on the curve and in the prime subgroup.
    Return [None] if the bytes are not in the correct format or does
    not represent a point in the prime subgroup.

    The expected bytes format are the compressed form of a point on G1.
*)
val pk_of_bytes_opt : Bytes.t -> pk option

(** Returns a bytes representation of a value of type [pk]. The output is the
    compressed form a the point G1.t the [pk] represents.
*)
val pk_to_bytes : pk -> Bytes.t

(** [aggregate_signature_opt signatures] aggregates the signatures [signatures], following
    https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-2.8.
    Return [None] if [INVALID] is expected in the specification
*)
val aggregate_signature_opt : Bytes.t list -> Bytes.t option

val verify : pk -> Bytes.t -> signature -> bool

val aggregate_verify : (pk * Bytes.t) list -> signature -> bool
