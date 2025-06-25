(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** This module provides types and functions to manipulate nonces.

    A nonce is a byte sequence of fixed length, which is supposed to be random
    and used only once, provided by a block producer and used to generate a
    random seed (see {!module:Seed_repr}). *)

type t = Seed_repr.nonce

type nonce = t

type error +=
  | Too_late_revelation
  | Too_early_revelation
  | Already_revealed_nonce
  | Inconsistent_nonce

val encoding : nonce Data_encoding.t

type unrevealed = Storage.Seed.unrevealed_nonce = {
  nonce_hash : Nonce_hash.t;
  delegate : Signature.Public_key_hash.t;
}

type status = Unrevealed of unrevealed | Revealed of Seed_repr.nonce

val get : Raw_context.t -> Level_repr.t -> status tzresult Lwt.t

type nonce_presence = No_nonce_expected | Nonce_expected of status

val check : Raw_context.t -> Level_repr.t -> nonce_presence tzresult Lwt.t

val record_hash : Raw_context.t -> unrevealed -> Raw_context.t tzresult Lwt.t

(** Checks that a nonce revelation operation can be safely applied.

    @return [Error Too_early_revelation] if the current cycle is the
    cycle 0 or if the previous cycle is lesser than the cycle of the
    input level.

    @return [Error Too_late_revelation] if the previous cycle is
    greater than the cycle of the input level. This error is also
    returned if the current level cycle position is greater or equal to
    the nonce revelation threshold.

    @return [Error Already_revealed_nonce] if a nonce is already
    revealed in the context for the input level.

    @return [Error Inconsistent_nonce] if the hash of the input nonce
    does not correspond to the nonce recover from the context for the
    given level. *)
val check_unrevealed :
  Raw_context.t -> Level_repr.t -> nonce -> unit tzresult Lwt.t

val reveal :
  Raw_context.t -> Level_repr.t -> nonce -> Raw_context.t tzresult Lwt.t

val of_bytes : bytes -> nonce tzresult

val hash : nonce -> Nonce_hash.t

val check_hash : nonce -> Nonce_hash.t -> bool
