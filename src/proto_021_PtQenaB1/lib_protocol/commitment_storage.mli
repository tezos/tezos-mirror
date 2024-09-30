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

(** [exists ctxt bpkh] returns true iff [bpkh] is associated to a non null
    commitment. *)
val exists : Raw_context.t -> Blinded_public_key_hash.t -> bool Lwt.t

(** [committed_amount ctxt bpkh] return the commitment associated to [bpkh], or
    [Tez_repr.zero] if [bpkh] has no associated commitment. *)
val committed_amount :
  Raw_context.t -> Blinded_public_key_hash.t -> Tez_repr.t tzresult Lwt.t

val increase_commitment_only_call_from_token :
  Raw_context.t ->
  Blinded_public_key_hash.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

val decrease_commitment_only_call_from_token :
  Raw_context.t ->
  Blinded_public_key_hash.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

val fold :
  Raw_context.t ->
  order:[`Sorted | `Undefined] ->
  init:'a ->
  f:(Blinded_public_key_hash.t -> Tez_repr.t -> 'a -> 'a Lwt.t) ->
  'a Lwt.t
