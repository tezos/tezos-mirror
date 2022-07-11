(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Dal_cryptobox_sigs

(** Because of the shell/protocol separation, cryptographic primitives
   need to be splitted. An interface, called the {!module:Verifier}
   aims to be provided for the economic protocol. The other interface,
   called the {!module:Builder} is for the shell.

    A [Verifier], has hinted by the name, mainly needs to check
   proofs:

    1. A proof that a commitment is valid

    2. A proof that a segment is valid

   A technicality is that the economic protocol is able to configure
   those cryptographic primitives via several constants.  Also, an SRS
   (aka trusted setup) is required.

   It is the responsibility of the shell and the protocol to ensure
   that both the [Verifier] and the [Builder] as instantiated with the
   same parameters and use the same trusted setup. *)

module Verifier : functor (C : CONSTANTS) -> sig
  include SRS

  include COMMITMENT with type srs := srs

  include SEGMENT with type commitment := commitment and type srs := srs
end

module Builder (C : CONSTANTS) : sig
  include module type of Verifier (C)

  include POLYNOMIAL with type srs := srs and type commitment := commitment

  include
    SHARD
      with type srs := srs
       and type polynomial := polynomial
       and type commitment := commitment
       and module IntMap := IntMap

  include
    PROOF
      with type srs := srs
       and type commitment := commitment
       and type polynomial := polynomial
       and type commitment_proof := commitment_proof
       and type segment_proof := segment_proof
       and type shard_proof := shard_proof
end
