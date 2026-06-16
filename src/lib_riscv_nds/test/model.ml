(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Pure OCaml reference model for the RISC-V New Durable Storage (NDS),
    implementing the same key-value semantics as the real backends. Used
    as the reference side of the bisimulation tests.

    {b Authoritative spec.} The upstream specification is the Linear document:
    {{:https://linear.app/tezos/document/durable-storage-system-f65c23199aef}
    Durable Storage System}

    This model follows the {b OCaml interface} defined in
    {!Intf.REGISTRY} and {!Intf.DATABASE} rather than the Rust-level
    spec directly. Where the two diverge the discrepancy is documented
    in the relevant function's docstring. Known differences between the
    Rust-level spec and the OCaml bindings:

    - {b [set]}: the Rust spec returns the number of bytes written
      (a prefix of the input). The OCaml binding returns [unit]
      (always writes the full value).

    {b Hashing.} Hashes are computed with BLAKE2b over the sorted
    serialisation of key-value pairs. These hashes are deterministic
    and collision-resistant but do NOT match the Merkle hashes from
    the real backends — they only need to satisfy the generic hash
    properties (determinism, sensitivity, revert, etc.) when run
    through [backend_tests]. In particular, the Rust spec notes that
    database root hashes are {i operation-order sensitive} (the AVL
    Merkle tree depends on insertion order). This model's hash is
    order-independent since it hashes the sorted map contents. *)

open Nds_errors
module Bytes_map = Map.Make (Bytes)

(** A registry is an ordered collection of databases, each being an
    isolated key-space represented as a sorted map from byte sequence
    keys to byte sequence values.

    Invariants:
    - Keys are byte sequences of at most 256 bytes.
    - Values are arbitrary byte sequences.
    - Databases are identified by zero-based indices. *)
type registry = {mutable dbs : Bytes.t Bytes_map.t array}

let fail e = Error e

let fail_when cond e = if cond then Error e else Ok ()

(** [check_db_in_bounds r db] fails with [Invalid_argument
    Database_index_out_of_bounds] when [db] is not a valid zero-based index into
    [r.dbs]. *)
let check_db_in_bounds r db =
  let i = Int64.to_int db in
  if i >= 0 && i < Array.length r.dbs then Ok ()
  else fail Database_index_out_of_bounds

(** Keys must be at most 256 bytes. *)
let check_key_valid key = fail_when (Bytes.length key > 256) Key_too_long

(** Validate key length and database index. Key length is checked
    first: if the key exceeds 256 bytes the error is [Key_too_long],
    otherwise if the database index is out of bounds the error is
    [Database_index_out_of_bounds]. Returns [None] when both are valid. *)
let check_key_db r key db =
  let open Result_syntax in
  let* () = check_key_valid key in
  let* () = check_db_in_bounds r db in
  return_unit

(** BLAKE2b-256 hash of a single database.

    The database contents are serialised by iterating over the
    [Bytes_map] (which yields keys in lexicographic order) and
    emitting, for each key-value pair, the key length as a big-endian
    32-bit integer, then the key bytes, then the value length as a
    big-endian 32-bit integer, then the value bytes. The BLAKE2b hash
    is taken over the resulting byte sequence.

    This hash is deterministic and collision-resistant but does NOT
    match the Merkle hashes produced by the real backends. *)
let hash_db db =
  let buf = Buffer.create 256 in
  Bytes_map.iter
    (fun k v ->
      let klen = Bytes.length k in
      let vlen = Bytes.length v in
      Buffer.add_int32_be buf (Int32.of_int klen) ;
      Buffer.add_bytes buf k ;
      Buffer.add_int32_be buf (Int32.of_int vlen) ;
      Buffer.add_bytes buf v)
    db ;
  let (Tezos_hacl.Hacl.Blake2b.Hash h) =
    Tezos_hacl.Hacl.Blake2b.direct (Bytes.of_string (Buffer.contents buf)) 32
  in
  h

module Registry = struct
  type t = registry

  (** [hash r] computes the registry hash by concatenating the
      per-database hashes (in index order) and taking a BLAKE2b-256
      hash of the result. *)
  let hash r =
    let buf = Buffer.create (32 * Array.length r.dbs) in
    Array.iter (fun db -> Buffer.add_bytes buf (hash_db db)) r.dbs ;
    let (Tezos_hacl.Hacl.Blake2b.Hash h) =
      Tezos_hacl.Hacl.Blake2b.direct (Bytes.of_string (Buffer.contents buf)) 32
    in
    Ok h

  (** [size r] returns the number of databases in the registry. *)
  let size r = Ok (Int64.of_int (Array.length r.dbs))

  (** [resize r n] adjusts the registry to contain exactly [n]
      databases.

      Spec:
      - The size can only change by at most one at a time.
        Returns [Registry_resize_too_large] if [|current_size - n| > 1].
      - Growing appends a fresh empty database.
      - Shrinking drops the last database. *)
  let resize r n =
    let open Result_syntax in
    let cur = Array.length r.dbs in
    let target = Int64.to_int n in
    let+ () = fail_when (abs (target - cur) > 1) Registry_resize_too_large in
    if target > cur then r.dbs <- Array.append r.dbs [|Bytes_map.empty|]
    else if target < cur then r.dbs <- Array.sub r.dbs 0 target

  (** [copy_database r ~src ~dst] duplicates all contents of the
      database at index [src] into index [dst].

      Spec:
      - The previous contents of [dst] are completely replaced.
      - The source database is unchanged.
      - Copying to the same index is a no-op (the assignment is
        idempotent since the map is shared).
      - Returns [Database_index_out_of_bounds] if [src] or [dst] is
        out of bounds. *)
  let copy_database r ~src ~dst =
    let open Result_syntax in
    let* () = check_db_in_bounds r src in
    let+ () = check_db_in_bounds r dst in
    r.dbs.(Int64.to_int dst) <- r.dbs.(Int64.to_int src)

  (** [move_database r ~src ~dst] transfers the database at index
      [src] to index [dst].

      Spec:
      - The previous contents of [dst] are completely replaced.
      - The source is replaced with a fresh empty database.
      - Moving to the same index is a no-op: the guard [si <> di]
        ensures neither the destination nor the source is modified.
      - Returns [Database_index_out_of_bounds] if [src] or [dst] is
        out of bounds. *)
  let move_database r ~src ~dst =
    let open Result_syntax in
    let* () = check_db_in_bounds r src in
    let+ () = check_db_in_bounds r dst in
    let si = Int64.to_int src in
    let di = Int64.to_int dst in
    (* When src = dst, the move is a no-op: the database stays as is. *)
    if si <> di then (
      r.dbs.(di) <- r.dbs.(si) ;
      r.dbs.(si) <- Bytes_map.empty)

  (** [clear r db] replaces the database at [db] with a fresh empty
      database.

      Spec:
      - Returns [Database_index_out_of_bounds] if [db] is out of
        bounds. *)
  let clear r db =
    let open Result_syntax in
    let+ () = check_db_in_bounds r db in
    r.dbs.(Int64.to_int db) <- Bytes_map.empty
end

module Database = struct
  (** [exists r ~db_index ~key] returns [true] iff [key] is present in
      the database at [db_index].

      Spec (OCaml binding — differs from Rust-level spec):
      - Returns [Key_too_long] if [Bytes.length key > 256].
      - Returns [Database_index_out_of_bounds] if [db_index] is out of
        bounds. *)
  let exists r ~db_index ~key =
    let open Result_syntax in
    let+ () = check_key_db r key db_index in
    let i = Int64.to_int db_index in
    Bytes_map.mem key r.dbs.(i)

  (** [set r ~db_index ~key ~value] associates [key] with [value],
      replacing any previous value entirely.

      Spec (OCaml binding — differs from Rust-level spec):
      - Returns [Key_too_long] if [Bytes.length key > 256].
      - Returns [Database_index_out_of_bounds] if [db_index] is out of
        bounds.
      - The value is copied so that later mutations of the input
        buffer do not affect the stored value.

      Note: the Rust spec returns the number of bytes written (a
      prefix of [data] may be stored). The OCaml binding always stores
      the full value and returns [unit]. *)
  let set r ~db_index ~key ~value =
    let open Result_syntax in
    let+ () = check_key_db r key db_index in
    let i = Int64.to_int db_index in
    r.dbs.(i) <- Bytes_map.add key (Bytes.copy value) r.dbs.(i)

  (** [write r ~db_index ~key ~offset ~value] writes [value] starting
      at byte position [offset] within the existing value of [key].

      Spec (OCaml binding — differs from Rust-level spec):
      - If the key does not exist and [offset = 0], the key is created
        with [value] as its contents.
      - If the key does not exist and [offset <> 0], returns
        [Offset_too_large].
      - If the key exists and [offset > current value length], returns
        [Offset_too_large].
      - The value is extended with zero bytes if the write goes past
        the current end, but is never truncated:
        [new_len = max cur_len (offset + Bytes.length value)].
      - Returns the number of bytes written (i.e. [Bytes.length value]).
      - Returns [Key_too_long] if [Bytes.length key > 256].
      - Returns [Database_index_out_of_bounds] if [db_index] is out of
        bounds.

      Note: the Rust spec treats non-existent keys as having implicit
      length 0, so writing at [offset > 0] to an absent key would be
      an offset-too-large error. The OCaml binding returns
      [Key_not_found] instead, distinguishing "key absent" from
      "offset past end". *)
  let write r ~db_index ~key ~offset ~value =
    let open Result_syntax in
    let* () = check_key_db r key db_index in
    let i = Int64.to_int db_index in
    let off = Int64.to_int offset in
    match Bytes_map.find_opt key r.dbs.(i) with
    | None ->
        (* Key absent: only offset 0 is allowed, creating the key. *)
        let+ () = fail_when (off <> 0) Offset_too_large in
        r.dbs.(i) <- Bytes_map.add key (Bytes.copy value) r.dbs.(i) ;
        Int64.of_int (Bytes.length value)
    | Some cur ->
        let cur_len = Bytes.length cur in
        (* Offset must not exceed the current value length. *)
        let+ () = fail_when (off > cur_len) Offset_too_large in
        (* Extend but never truncate. *)
        let new_len = max cur_len (off + Bytes.length value) in
        let buf = Bytes.make new_len '\000' in
        (* Copy existing content, then overlay the write. *)
        Bytes.blit cur 0 buf 0 cur_len ;
        Bytes.blit value 0 buf off (Bytes.length value) ;
        r.dbs.(i) <- Bytes_map.add key buf r.dbs.(i) ;
        Int64.of_int (Bytes.length value)

  (** [read r ~db_index ~key ~offset ~len] reads up to [len] bytes
      starting at byte position [offset] from the value of [key].

      Spec (OCaml binding — differs from Rust-level spec):
      - The returned byte sequence may be shorter than [len] if the
        value ends before [offset + len]:
        [actual_len = min len (value_length - offset)].
      - Returns [Key_not_found] if the key does not exist.
      - Returns [Offset_too_large] if [offset > value length].
      - Returns [Key_too_long] if [Bytes.length key > 256].
      - Returns [Database_index_out_of_bounds] if [db_index] is out of
        bounds.

      Note: the Rust spec fails when [offset >= value_length]. The
      OCaml binding uses strict [>], so [offset = value_length]
      succeeds and returns an empty byte sequence. *)
  let read r ~db_index ~key ~offset ~len:requested_len =
    let open Result_syntax in
    let* () = check_key_db r key db_index in
    let i = Int64.to_int db_index in
    match Bytes_map.find_opt key r.dbs.(i) with
    | None -> fail Key_not_found
    | Some v ->
        let off = Int64.to_int offset in
        let vlen = Bytes.length v in
        let+ () = fail_when (off > vlen) Offset_too_large in
        (* Clamp to the remaining bytes after offset. *)
        let actual_len = min (Int64.to_int requested_len) (vlen - off) in
        Bytes.sub v off actual_len

  (** [value_length r ~db_index ~key] returns the length in bytes of
      the value associated with [key].

      Spec:
      - Returns [Key_not_found] if the key does not exist.
      - Returns [Key_too_long] if [Bytes.length key > 256].
      - Returns [Database_index_out_of_bounds] if [db_index] is out of
        bounds. *)
  let value_length r ~db_index ~key =
    let open Result_syntax in
    let* () = check_key_db r key db_index in
    let i = Int64.to_int db_index in
    match Bytes_map.find_opt key r.dbs.(i) with
    | None -> fail Key_not_found
    | Some v -> return (Int64.of_int (Bytes.length v))

  (** [delete r ~db_index ~key] removes [key] and its associated value.

      Spec:
      - Deleting a non-existent key is a silent no-op: the map's
        [remove] is idempotent, so the hash is unchanged.
      - Returns [Key_too_long] if [Bytes.length key > 256].
      - Returns [Database_index_out_of_bounds] if [db_index] is out of
        bounds. *)
  let delete r ~db_index ~key =
    let open Result_syntax in
    let+ () = check_key_db r key db_index in
    let i = Int64.to_int db_index in
    r.dbs.(i) <- Bytes_map.remove key r.dbs.(i)

  (** [hash r ~db_index] returns the hash of the database at
      [db_index].

      Spec:
      - Returns [Database_index_out_of_bounds] if [db_index] is out of
        bounds. *)
  let hash r ~db_index =
    let open Result_syntax in
    let+ () = check_db_in_bounds r db_index in
    hash_db r.dbs.(Int64.to_int db_index)
end

let name = "model"

let create_registry_with_dbs n = {dbs = Array.init n (fun _ -> Bytes_map.empty)}

(** Iterate over all key-value pairs across all databases. Used by
    [check_state] to enumerate the reference state for comparison. *)
let iter_all_keys r f =
  Array.iteri
    (fun i db ->
      let db_index = Int64.of_int i in
      Bytes_map.iter (fun key value -> f ~db_index ~key ~value) db)
    r.dbs
