(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

(** Unique identifier for a key. *)
module Key_id : sig
  type t

  (** Only use at library frontiers *)
  val to_pkh : t -> Signature.public_key_hash

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  module Table : sig
    include Hashtbl.SeededS with type key = t

    val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
  end
end

(** A key that is fully known to the baker.

    This means that the alias has both an associated public key and
    secret key uri in the client wallet, and also that the key was
    provided to the baker command line (or no keys were provided at
    all, so that the baker defaulted to using all keys in the client
    wallet).

    May be used as a delegate's manager key, consensus key, and/or
    companion key. *)
module Key : sig
  type t = private {
    alias : string;
    id : Key_id.t;
    public_key : Signature.public_key;
    secret_key_uri : Client_keys.sk_uri;
  }

  val make :
    alias:string ->
    public_key:Signature.public_key ->
    public_key_hash:Signature.public_key_hash ->
    secret_key_uri:Client_keys.sk_uri ->
    t

  (** Partial encoding for {!t} that omits the secret key to avoid
      leaking it in event logs (because {!Client_keys.sk_uri} contains
      the plain secret key when the key is unencrypted).

      Warning: As a consequence, decoding from this encoding will
      always fail. *)
  val encoding_for_logging__cannot_decode : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val is_bls : t -> bool

  module Set : Set.S with type elt = t
end

(** Unique identifier for a delegate. *)
module Delegate_id : module type of Key_id

(** A delegate on behalf of which the baker should bake and attest.

    The consensus key must be fully known.

    The companion key may be [None] for multiple reasons:
    - if the delegate has not registered any companion key
    - if the companion key is not needed for the current cycle because
      either aggregations are disabled or the consensus key is not a BLS
      key
    - if the companion key is not known to the client wallet or has
      not been provided to the baker command line

    The manager key may or may not be known to the baker; this does
    not affect the baker's functionalities, but is tracked in the
    [manager_key] type for logging purposes. *)
module Delegate : sig
  type manager_key

  type t = private {
    manager_key : manager_key;
    consensus_key : Key.t;
    companion_key : Key.t option;
  }

  val delegate_id : t -> Delegate_id.t

  (** Partial encoding for {!t} that omits secret keys to avoid
      leaking them in event logs; see
      {!Key.encoding_for_logging__cannot_decode}.

      Warning: As a consequence, decoding from this encoding will
      always fail. *)
  val encoding_for_logging__cannot_decode : t Data_encoding.t

  (** Prints the delegate's manager key (with its alias if known,
      otherwise just the pkh), consensus key if it is different from
      the manager key, and companion key if there is one. *)
  val pp : Format.formatter -> t -> unit

  (** Same as {!pp} except that the companion key is omitted even if
      there is one. *)
  val pp_without_companion_key : Format.formatter -> t -> unit

  (** Builds a {!t} from an element of the output of
      {!Node_rpc.get_validators}, if the consensus key is present in
      [known_keys]; otherwise, returns [None].

      If the consensus key is a known BLS key and the validator
      argument contains a companion key but that companion key is not
      in [known_keys], emits an error event but nevertheless returns a
      {!t} where [companion_key = None]. (This function is in Lwt to
      be able to emit this event.) *)
  val of_validator :
    known_keys:Key.Set.t -> RPC.Validators.delegate -> t option Lwt.t
end

(** A prequorum consists of a level, a round, a block_payload_hash and the list
    of preattestations that has a total voting power higher than the protocol
    threshold. *)
type prequorum = {
  level : int32;
  round : Round.t;
  block_payload_hash : Block_payload_hash.t;
  preattestations : packed_operation list;
}

type block_info = {
  hash : Block_hash.t;
  shell : Block_header.shell_header;
  payload_hash : Block_payload_hash.t;
  payload_round : Round.t;
  round : Round.t;
  prequorum : prequorum option;
  quorum : packed_operation list;
  payload : Operation_pool.payload;
  grandparent : Block_hash.t;
}

type proposal = {block : block_info; predecessor : block_info}

(** A delegate slot consists of the delegate's consensus key, its public key
    hash, its first slot, and its attesting power at some level. *)
type delegate_info = {
  delegate : Delegate.t;
  attestation_slot : Slot.t;
  attesting_power : int64;
}

(** An association list between delegates and promises for their DAL
    attestations at some level (as obtained through the [get_attestable_slots]
    RPC). See usage in {!level_state}. *)
type dal_attestable_slots =
  (Delegate_id.t
  * Tezos_dal_node_services.Types.attestable_slots tzresult Lwt.t)
  list
