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

(** Type of an identity, comprising a [peer_id], a cryptographic key pair, and a
    proof of work stamp with enough difficulty so that the network
    accept this identity as genuine. *)
type t = {
  peer_id : P2p_peer.Id.t;
  public_key : Tezos_crypto.Crypto_box.public_key;
  secret_key : Tezos_crypto.Crypto_box.secret_key;
  proof_of_work_stamp : Tezos_crypto.Crypto_box.nonce;
}

val encoding : t Data_encoding.t

(** [generate pow_target] is a freshly minted identity whose proof of
    work stamp difficulty is at least equal to [pow_target].

    The argument [yield_every] (defaults to [500]) introduces a call to
    [Lwt.pause] every that many operations. *)
val generate : ?yield_every:int -> Tezos_crypto.Crypto_box.pow_target -> t Lwt.t

(** [generate_with_bound pow_target] is a freshly minted identity whose proof of
    work stamp difficulty is at least equal to [pow_target].

    The optional argument [max] sets a maximum number of attempts. If that many
    attempts are made without finding a successful pow, the function fails with
    [Not_found]. *)
val generate_with_bound :
  ?yield_every:int -> ?max:int -> Tezos_crypto.Crypto_box.pow_target -> t Lwt.t

(** [generate_with_pow_target_0 pk] generates a proof of work for the
    public key [pk] following a (hard-coded) 0 proof-of-work target.

    NOTICE: This function is meant for generating dummy identities. It is useful
    for tests and other such controlled environment but it is not suitable for
    generating identities for an open network. *)
val generate_with_pow_target_0 : unit -> t
