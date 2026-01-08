(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [Nonce_bitset] registers known nonces from transactions that went
    through the tx_queue from a specific sender address. With this
    structure it's easy to do bookkeeping of address' nonce without
    going through all the transactions of the queue.

    The invariants are that for any nonce_bitset [nb]:

    - When creating [nb], [nb.next_nonce] is the next valid nonce for
    the state.

    - When adding a nonce [n] to [nb], [n] must be superior or equal
    to [nb.next_nonce]. This is enforced by the validation in
    {!Validate.is_tx_valid}.

    - [nb.next_nonce] can only increase over time. This is enforced by
    [shift] and [offset].
 *)

(** [t] allows to register for a given address all nonces that are
      currently used by transaction in the tx_queue. *)
type t = {
  next_nonce : Z.t;
      (** [next_nonce] is the base value for any position found in
            {!field:bitset}. It’s set to be the next expected nonce
            for a given address, which is the nonce found in the
            backend. *)
  bitset : Tezos_base.Bitset.t;
}

(** [create ~next_nonce] creates a {!t} struct with empty [bitset]. *)
val create : next_nonce:Z.t -> t

(** [offset ~nonce1 ~nonce2] computes the difference between
      [nonce1] and [nonce2].

      Fails if [nonce2 > nonce1] or if the difference between the two is
      more than [Int.max_int]. *)
val offset : nonce1:Z.t -> nonce2:Z.t -> int tzresult

(** [add bitset_nonce ~nonce] adds the nonce [nonce] to
    [bitset_nonce]. *)
val add : t -> nonce:Z.t -> t tzresult

(** [add_many bitset_nonce ~nonce ~length] adds the nonces [nonce],
    [nonce+1], ..., [nonce+length-1] to [bitset_nonce]. *)
val add_many : t -> nonce:Z.t -> length:int -> t tzresult

(** [remove bitset_nonce ~nonce] removes the nonce [nonce] from
    [bitset_nonce].

    If [nonce] is strictly inferior to [bitset_nonce.next_nonce] then
    it's a no-op because nonce can't exist in the bitset. *)
val remove : t -> nonce:Z.t -> t tzresult

(** [remove_many bitset_nonce ~nonce ~length] removes the nonces
    [nonce], [nonce+1], ..., [nonce+length-1] from [bitset_nonce].

    If [nonce] is strictly inferior to [bitset_nonce.next_nonce] then
    it's a no-op because some nonces can't exist in the bitset. *)
val remove_many : t -> nonce:Z.t -> length:int -> t tzresult

(** [shift bitset_nonce ~nonce] shifts the bitset of [bitset_nonce]
      so the next_nonce is now [nonce]. Shifting the bitset means
      that nonces that are inferior to [nonce] are dropped.

      Fails if [nonce] is strictly inferior to
      [bitset_nonce.next_nonce]. *)
val shift : t -> nonce:Z.t -> t tzresult

(** [is_empty bitset_nonce] checks if the bitset is empty, i.e. no
      position is at 1. *)
val is_empty : t -> bool

(** [next_gap bitset_nonce] returns the next available nonce. *)
val next_gap : t -> Z.t

(** [shift_then_next_gap bitset_nonce ~shift_nonce] calls [shift
    ~nonce:shift_nonce] then [!next_gap bitset_nonce]. *)
val shift_then_next_gap : t -> shift_nonce:Z.t -> Z.t tzresult
