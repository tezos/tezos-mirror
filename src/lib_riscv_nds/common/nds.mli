(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Type-erased NDS handle.

    Wraps any concrete backend satisfying {!Intf.NORMAL} into a single
    existential type so that callers can work with NDS generically.
    Since all backends share the common
    {!Nds_errors.invalid_argument_error} type in their results, no error
    conversion is required.

    Each wrap also carries an extensible {!tag} that lets backend-aware
    callers recover the concrete registry value via a backend-defined
    {!unpack} pattern match.  The tag taxonomy lives in each backend
    (e.g. memory's [Normal_tag | Prove_tag | Verify_tag]) so the common
    layer never names backend-specific types. *)

(** Extensible tag carrying type equality between the tag and the
    wrapped registry.  Backends extend this type with their own
    constructors that refine ['a] to a concrete [Registry.t]. *)
type 'a tag = ..

(** Opaque NDS handle that erases the concrete backend type. *)
type t

(** [wrap tag impl value] packages a concrete NDS backend [impl] and its
    state [value] into an opaque {!t} handle, carrying the [tag] that
    identifies the concrete backend mode.

    The [tag], [impl] and [value] are constrained to share the same
    [Registry.t], so a pattern match on [tag] inside the owning backend
    refines the type of [value] back to the concrete registry type. *)
val wrap : 'a tag -> (module Intf.NORMAL with type Registry.t = 'a) -> 'a -> t

(** Existential view of a wrapped handle, re-exposing the three
    components ([tag], [impl], [value]) for a pattern match in the
    owning backend.  This is the type-safe alternative to a direct
    [unwrap : 'a tag -> t -> 'a option], which cannot be written in the
    common layer because matching extensible-tag constructors here
    would force the common layer to enumerate every backend's mode. *)
type packed =
  | Packed : {
      tag : 'a tag;
      impl : (module Intf.NORMAL with type Registry.t = 'a);
      value : 'a;
    }
      -> packed

(** [unpack t] re-exposes the three components of [t] for a typed
    pattern match in the owning backend.  Within a [match] on the
    returned [tag], OCaml refines the existentially-bound type
    variable, so [value] becomes usable at its concrete registry
    type. *)
val unpack : t -> packed

(** {2 Dispatchers}

    Each function below unpacks the existential and delegates to the
    wrapped backend. *)

(** [size t] returns the number of databases in the registry. *)
val size : t -> int64

(** [resize t n] adjusts the registry to contain exactly [n] databases. *)
val resize : t -> int64 -> (unit, Nds_errors.invalid_argument_error) result

(** [copy_database t ~src ~dst] duplicates database [src] to [dst]. *)
val copy_database :
  t ->
  src:int64 ->
  dst:int64 ->
  (unit, Nds_errors.invalid_argument_error) result

(** [move_database t ~src ~dst] moves database [src] to [dst]. *)
val move_database :
  t ->
  src:int64 ->
  dst:int64 ->
  (unit, Nds_errors.invalid_argument_error) result

(** [clear t db_index] removes all entries from database [db_index]. *)
val clear : t -> int64 -> (unit, Nds_errors.invalid_argument_error) result

(** [registry_hash t] returns the Merkle root hash of the registry. *)
val registry_hash : t -> bytes

(** [exists t ~db_index ~key] checks whether [key] exists in database
    [db_index]. *)
val exists :
  t ->
  db_index:int64 ->
  key:bytes ->
  (bool, Nds_errors.invalid_argument_error) result

(** [read t ~db_index ~key ~offset ~len] reads bytes from database
    [db_index]. *)
val read :
  t ->
  db_index:int64 ->
  key:bytes ->
  offset:int64 ->
  len:int64 ->
  (bytes, Nds_errors.invalid_argument_error) result

(** [write t ~db_index ~key ~offset ~value] writes [value] starting at
    [offset] into the value associated with [key] in database
    [db_index]. Returns the number of bytes written (i.e.
    [Bytes.length value]). *)
val write :
  t ->
  db_index:int64 ->
  key:bytes ->
  offset:int64 ->
  value:bytes ->
  (int64, Nds_errors.invalid_argument_error) result

(** [set t ~db_index ~key ~value] replaces the value of [key] in
    database [db_index]. *)
val set :
  t ->
  db_index:int64 ->
  key:bytes ->
  value:bytes ->
  (unit, Nds_errors.invalid_argument_error) result

(** [delete t ~db_index ~key] removes [key] from database [db_index]. *)
val delete :
  t ->
  db_index:int64 ->
  key:bytes ->
  (unit, Nds_errors.invalid_argument_error) result

(** [value_length t ~db_index ~key] returns the length of the value for
    [key] in database [db_index]. *)
val value_length :
  t ->
  db_index:int64 ->
  key:bytes ->
  (int64, Nds_errors.invalid_argument_error) result

(** [hash t ~db_index] returns the Merkle root hash of database
    [db_index]. *)
val hash :
  t -> db_index:int64 -> (bytes, Nds_errors.invalid_argument_error) result

(** {2 Verify-mode boundary} *)

(** [with_verification t f] runs [f t] catching
    {!Nds_errors.Verification_failed}.  For Normal- and Prove-backed
    [t] the [Error] arm is structurally unreachable but the return
    type stays uniform.  The intended use is wrapping a kernel-step
    replay so a mid-step proof mismatch becomes a clean
    [Error verification_error] at a single, well-defined boundary,
    rather than each call site needing to install its own
    [Lwt.catch]. *)
val with_verification :
  t -> (t -> 'a Lwt.t) -> ('a, Nds_errors.verification_error) result Lwt.t
