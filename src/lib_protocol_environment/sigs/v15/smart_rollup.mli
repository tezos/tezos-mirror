(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** Smart rollup addresses *)
module Address : S.HASH

(** Smart rollup commitment hashes *)
module Commitment_hash : S.HASH

(** Smart rollup PVM state hashes. Refer to
    {!Tezos_crypto.Hashed.Smart_rollup_state_hash} in
    [src/lib_crypto/smart_rollup_state_hash.mli] for documentation. *)
module State_hash : sig
  include S.HASH

  val context_hash_to_state_hash : Context_hash.t -> t

  type unreachable__use_context_hash_to_state_hash

  val hash_bytes : unreachable__use_context_hash_to_state_hash

  val hash_string : unreachable__use_context_hash_to_state_hash
end

(** Smart rollup inbox hashes *)
module Inbox_hash : S.HASH

(** Smart rollup merkelized payload hashes' hash *)
module Merkelized_payload_hashes_hash : S.HASH
