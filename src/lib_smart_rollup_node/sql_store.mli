(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Type of store (a handler to the underlying database). *)
type 'a t

(** Read/write store {!t}. *)
type rw = Store_sigs.rw t

(** Read only store {!t}. *)
type ro = Store_sigs.ro t

(** Name of SQLite file in data directory. *)
val sqlite_file_name : string

(** [init mode ~data_dir] initializes the store and returns it. *)
val init : 'a Store_sigs.mode -> data_dir:string -> 'a t tzresult Lwt.t

(** Close the store by freeing all resources and closing database
    connections. *)
val close : _ t -> unit Lwt.t

(** Returns a read-only version of the store. *)
val readonly : _ t -> ro

(** {2 Modules for storing data in the store}

    Each module maps to a table in the database. Every read and write function
    has two parameters: [store] and an optional [?conn].

    When [~conn] is provided, the database connection is used directly to
    execute the queries. If not, a new (or old) connection is taken from the
    pool of [store].
*)

(** Storage containing commitments and corresponding commitment hashes that the
    rollup node has knowledge of. *)
module Commitments : sig
  (** [store ?conn s commitment] stores a new [commitment] associated to its
      hash and returns it. *)
  val store :
    ?conn:Sqlite.conn -> rw -> Commitment.t -> Commitment.Hash.t tzresult Lwt.t

  (** Retrieve a commitment by its hash. *)
  val find :
    ?conn:Sqlite.conn ->
    _ t ->
    Commitment.Hash.t ->
    Commitment.t option tzresult Lwt.t

  (** Retrieve a commitment corresponding to the LCC, the last cemented
      commitment. *)
  val find_lcc :
    ?conn:Sqlite.conn -> _ t -> unit -> Commitment.t option tzresult Lwt.t

  (** Retrieve the last commitment published by the rollup node
      (as known from the handled L1 blocks). *)
  val find_lpc : ?conn:Sqlite.conn -> _ t -> Commitment.t option tzresult Lwt.t
end

(** Storage mapping commitment hashes to the level when they were published by
    the rollup node. *)
module Commitments_published_at_levels : sig
  type publication_levels = {
    first_published_at_level : int32;
        (** The level at which this commitment was first published. *)
    published_at_level : int32 option;
        (** The level at which we published this commitment. If
            [first_published_at_level <> published_at_level] it means that the
            commitment is republished. *)
  }

  (** [register ?conn s hash levels] stores the publication levels for
      commitment whose hash is [hash].  *)
  val register :
    ?conn:Sqlite.conn ->
    rw ->
    Commitment.Hash.t ->
    publication_levels ->
    unit tzresult Lwt.t

  (** Retrieve the publication levels for a commitment by its hash. *)
  val get :
    ?conn:Sqlite.conn ->
    _ t ->
    Commitment.Hash.t ->
    publication_levels option tzresult Lwt.t

  (** Retrieve the level at which a commitment was first published. *)
  val get_first_published_level :
    ?conn:Sqlite.conn -> _ t -> Commitment.Hash.t -> int32 option tzresult Lwt.t
end
