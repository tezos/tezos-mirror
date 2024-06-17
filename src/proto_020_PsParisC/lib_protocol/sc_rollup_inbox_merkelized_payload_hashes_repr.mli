(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type error += Merkelized_payload_hashes_proof_error of string

module Hash : S.HASH with type t = Smart_rollup.Merkelized_payload_hashes_hash.t

(** A type representing the head of a merkelized list of
    {!Sc_rollup_inbox_message_repr.serialized} message. It contains the hash of
    the payload and the index on the list. *)
type t

val encoding : t Data_encoding.t

type merkelized_and_payload = {
  merkelized : t;
  payload : Sc_rollup_inbox_message_repr.serialized;
}

(** A [History.t] is a lookup table of {!merkelized_and_payload}s. Payloads are
    indexed by their hash {!Hash.t}. This history is needed in order to produce
    {!proof}.

    A subtlety of this [history] type is that it is customizable depending on
    how much of the inbox history you actually want to remember, using the
    [capacity] parameter. In the L1 we use this with [capacity] set to zero,
    which makes it immediately forget an old level as soon as we move to the
    next. By contrast, the rollup node uses a history that is sufficiently large
    to be able to take part in all potential refutation games occurring during
    the challenge period. *)
module History : sig
  include
    Bounded_history_repr.S
      with type key = Hash.t
       and type value = merkelized_and_payload

  val no_history : t
end

(** [hash merkelized] is the hash of [merkelized]. It is used as key to remember
    a merkelized payload hash in an {!History.t}. *)
val hash : t -> Hash.t

(** [remember history merkelized payload] remembers the [{merkelized; payload}]
    in [history] with key [hash merkelized]. *)
val remember :
  History.t ->
  t ->
  Sc_rollup_inbox_message_repr.serialized ->
  History.t tzresult

(** [genesis_no_history payload] is the initial merkelized payload hashes with
    index 0. *)
val genesis_no_history : Sc_rollup_inbox_message_repr.serialized -> t

(** [genesis history payload] is the initial merkelized payload hashes with
    index 0. It is remembered in [history] using [remember]. *)
val genesis :
  History.t ->
  Sc_rollup_inbox_message_repr.serialized ->
  (History.t * t) tzresult

(** [add_payload_no_history merkelized payload] creates a new {!t} with [payload]
    and [merkelized] as ancestor (i.e. [index = succ (get_index
    merkelized)]). *)
val add_payload_no_history : t -> Sc_rollup_inbox_message_repr.serialized -> t

(** [add_payload] is identical to {!add_payload_no_history} but the resulting
    [merkelized] is remembered in [history] with [remember]. *)
val add_payload :
  History.t ->
  t ->
  Sc_rollup_inbox_message_repr.serialized ->
  (History.t * t) tzresult

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit

(** [get_payload_hash merkelized] returns the
    {!Sc_rollup_inbox_message_repr.serialized} payload's hash of
    [merkelized]. *)
val get_payload_hash : t -> Sc_rollup_inbox_message_repr.Hash.t

(** [get_index merkelized] returns the index of [merkelized]. *)
val get_index : t -> Z.t

(** Given two t [(a, b)] and a {!Sc_rollup_inbox_message_repr.serialized}
    [payload], a [proof] guarantees that [payload] hash is equal to [a] and that
    [a] is an ancestor of [b]; i.e. [get_index a < get_index b]. *)
type proof = private t list

val pp_proof : Format.formatter -> proof -> unit

val proof_encoding : proof Data_encoding.t

(** [produce_proof history ~index into_] returns a {!merkelized_and_payload}
    with index [index] and a proof that it is an ancestor of [into_]. Returns
    [None] if no merkelized payload with [index] is found (either in the
    [history] or [index] is not inferior to [get_index into_]). *)
val produce_proof :
  History.t -> index:Z.t -> t -> (merkelized_and_payload * proof) option

(** [verify_proof proof] returns [(a, b)] where [proof] validates that [a] is an
    ancestor of [b]. Fails when [proof] is not a valid inclusion proof. *)
val verify_proof : proof -> (t * t) tzresult

module Internal_for_tests : sig
  (** [find_predecessor_payload history ~index latest_merkelized] looks for the
      {!t} with [index] that is an ancestor of [latest_merkelized]. *)
  val find_predecessor_payload : History.t -> index:Z.t -> t -> t option

  val make_proof : t list -> proof
end
