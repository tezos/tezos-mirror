(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Shared module type signatures for the RISC-V new durable storage.

    The durable storage is a crash-resistant key-value store organised as a
    registry of independent databases. Each database is an isolated key-space
    backed by a Merkle tree, providing content-addressed hashing and
    persistence through commits.

    Keys are byte sequences of at most 256 bytes. Values are arbitrary byte
    sequences.

    Two kinds of errors may occur:
    - {b Operational errors} are fatal and indicate that the system may not
      be in a consistent state (e.g. I/O failures, encoding errors). They
      are raised as OCaml exceptions.
    - {b Invalid argument errors} are deterministic and indicate incorrect
      usage (e.g. key not found, offset too large). The state is left
      coherent. They are returned in the [Error] case of the [result]. *)

(** Common signature for proof objects. *)
module type PROOF = sig
  type t

  (** [start_state proof] returns the hash of the state before the
      operations recorded by [proof]. *)
  val start_state : t -> bytes

  (** [stop_state proof] returns the hash of the state after the
      operations recorded by [proof]. *)
  val stop_state : t -> bytes

  (** [serialise proof] serialises [proof] to a byte sequence. *)
  val serialise : t -> bytes

  (** [deserialise bytes] attempts to deserialise a proof from [bytes]. *)
  val deserialise : bytes -> t tzresult
end

(** Common signature for registry operations across all modes.

    A registry manages a collection of databases. Each database is
    identified by a zero-based index.

    The error type [invalid_argument_error] is left abstract so that
    different modes can instantiate it with the appropriate concrete
    error type: {!Nds_errors.invalid_argument_error} for Normal and
    Prove modes, {!Nds_errors.verification_argument_error} for Verify
    mode. *)
module type REGISTRY = sig
  type t

  (** Mode-specific invalid-argument error type for registry
      operations. *)
  type invalid_argument_error

  (** [hash registry] computes the Merkle root hash of [registry],
      derived from the root hashes of all its databases. *)
  val hash : t -> (bytes, invalid_argument_error) result

  (** [size registry] returns the number of databases currently held in
      [registry]. *)
  val size : t -> (int64, invalid_argument_error) result

  (** [resize registry n] adjusts [registry] to contain exactly [n]
      databases. The size can only change by one at a time; call this
      function in a loop for larger adjustments. Growing appends a new
      empty database; shrinking drops the last database. Returns an
      error if [|current_size - n| > 1]. *)
  val resize : t -> int64 -> (unit, invalid_argument_error) result

  (** [copy_database registry ~src ~dst] duplicates all contents of the
      database at index [src] into index [dst], completely replacing the
      previous contents of [dst]. The source database is unchanged.
      Copying to the same index is a no-op. Returns an error if [src] or
      [dst] is out of bounds. *)
  val copy_database :
    t -> src:int64 -> dst:int64 -> (unit, invalid_argument_error) result

  (** [move_database registry ~src ~dst] transfers the database at index
      [src] to index [dst], completely replacing the previous contents of
      [dst]. The source is replaced with a fresh empty database. Moving
      to the same index is a no-op. Returns an error if [src] or [dst]
      is out of bounds. *)
  val move_database :
    t -> src:int64 -> dst:int64 -> (unit, invalid_argument_error) result

  (** [clear registry db_index] replaces the database at [db_index] with
      a fresh empty database. Returns an error if [db_index] is out of
      bounds. *)
  val clear : t -> int64 -> (unit, invalid_argument_error) result
end

(** Common signature for database key-value operations.

    Each database is an isolated key-space. Keys are byte sequences of
    at most 256 bytes. See {!REGISTRY} for the rationale behind the
    abstract [invalid_argument_error] type. *)
module type DATABASE = sig
  type registry

  (** Mode-specific invalid-argument error type for database
      operations. *)
  type invalid_argument_error

  (** [exists registry ~db_index ~key] returns [true] if [key] exists in
      the database at [db_index]. Returns an error if [db_index] is out
      of bounds or [key] exceeds 256 bytes. *)
  val exists :
    registry ->
    db_index:int64 ->
    key:bytes ->
    (bool, invalid_argument_error) result

  (** [set registry ~db_index ~key ~value] associates [key] with [value]
      in the database at [db_index], replacing any previous value
      entirely. Returns an error if [db_index] is out of bounds or [key]
      exceeds 256 bytes. *)
  val set :
    registry ->
    db_index:int64 ->
    key:bytes ->
    value:bytes ->
    (unit, invalid_argument_error) result

  (** [write registry ~db_index ~key ~offset ~value] writes [value]
      starting at byte position [offset] within the existing value
      associated with [key]. The value is extended if the write goes past
      the current end but is never truncated. Writing at offset [0L] to a
      non-existent key creates it. Returns the number of bytes written
      (i.e. [Bytes.length value]). Returns an error if the key does not
      exist and [offset] is non-zero, if [offset] exceeds the current
      value length, if [key] exceeds 256 bytes, or if [db_index] is out
      of bounds. *)
  val write :
    registry ->
    db_index:int64 ->
    key:bytes ->
    offset:int64 ->
    value:bytes ->
    (int64, invalid_argument_error) result

  (** [read registry ~db_index ~key ~offset ~len] reads up to [len]
      bytes starting at byte position [offset] from the value associated
      with [key]. The returned byte sequence may be shorter than [len] if
      the value ends before [offset + len]. Returns an error if the key
      does not exist, if [offset] exceeds the value length, if [key]
      exceeds 256 bytes, or if [db_index] is out of bounds. *)
  val read :
    registry ->
    db_index:int64 ->
    key:bytes ->
    offset:int64 ->
    len:int64 ->
    (bytes, invalid_argument_error) result

  (** [value_length registry ~db_index ~key] returns the length in bytes
      of the value associated with [key]. Returns an error if the key
      does not exist, if [key] exceeds 256 bytes, or if [db_index] is
      out of bounds. *)
  val value_length :
    registry ->
    db_index:int64 ->
    key:bytes ->
    (int64, invalid_argument_error) result

  (** [delete registry ~db_index ~key] removes [key] and its associated
      value. Deleting a non-existent key is a silent no-op (the root hash
      is unchanged). Returns an error if [key] exceeds 256 bytes or
      [db_index] is out of bounds. *)
  val delete :
    registry ->
    db_index:int64 ->
    key:bytes ->
    (unit, invalid_argument_error) result

  (** [hash registry ~db_index] returns the Merkle root hash of the
      database at [db_index]. Returns an error if [db_index] is out of
      bounds. *)
  val hash :
    registry -> db_index:int64 -> (bytes, invalid_argument_error) result
end

(** Normal mode: registry + database, with errors instantiated to
    {!Nds_errors.invalid_argument_error}. *)
module type NORMAL = sig
  module Registry :
    REGISTRY
      with type invalid_argument_error := Nds_errors.invalid_argument_error

  module Database :
    DATABASE
      with type registry := Registry.t
       and type invalid_argument_error := Nds_errors.invalid_argument_error
end

(** Prove mode: registry + database + proof lifecycle, with errors
    instantiated to {!Nds_errors.invalid_argument_error}.

    In prove mode, every operation on the registry and database is recorded
    so that a proof can be produced attesting to the correctness of the
    computation. *)
module type PROVE = sig
  type normal_registry

  module Registry :
    REGISTRY
      with type invalid_argument_error := Nds_errors.invalid_argument_error

  module Database :
    DATABASE
      with type registry := Registry.t
       and type invalid_argument_error := Nds_errors.invalid_argument_error

  module Proof : PROOF

  (** [start_proof registry] begins a proving session by transitioning
      [registry] into prove mode. Subsequent operations on the returned
      registry are recorded for the proof. *)
  val start_proof : normal_registry -> Registry.t

  (** [produce_proof registry] ends the proving session and returns a
      proof attesting to the operations performed since {!start_proof}. *)
  val produce_proof : Registry.t -> Proof.t
end

(** Verify mode: registry + database + verify entry point, with errors
    instantiated to {!Nds_errors.verification_argument_error} so that
    proof-mismatch failures can be reported in addition to the usual
    invalid-argument errors.

    In verify mode, operations are replayed against a proof to confirm
    they produce the expected state transition. *)
module type VERIFY = sig
  type proof

  module Registry :
    REGISTRY
      with type invalid_argument_error := Nds_errors.verification_argument_error

  module Database :
    DATABASE
      with type registry := Registry.t
       and type invalid_argument_error := Nds_errors.verification_argument_error

  (** [start_verify proof] creates a verify-mode registry that replays
      operations against [proof] to validate the recorded state
      transition. *)
  val start_verify : proof -> Registry.t
end
