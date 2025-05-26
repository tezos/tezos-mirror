(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Name of the SQLite3 file. *)
val sqlite_file_name : string

(** A direct connection to the database, allowing to interact with it. *)
type conn = Sqlite.conn

module Schemas : sig
  (** [get_all conn] returns the list of SQL statements allowing to recreate
      the tables in the current store. *)
  val get_all : conn -> string list tzresult Lwt.t
end

module Skip_list_cells : sig
  open Dal_proto_types

  (** A handler to the DAL node's skip list cells SQLite3 database. *)
  type t

  (** [init ~data_dir ~perm ()] returns a handler to the DAL node store
    located under [data_dir]. If no store is located in [data_dir], an
    empty store is created.

    If [perm] is [`Read_only], then SQL requests requiring write access will
    fail. With [`Read_write], they will succeed as expected. *)
  val init :
    data_dir:string ->
    perm:[`Read_only | `Read_write] ->
    unit ->
    t tzresult Lwt.t

  (** [close t] closes the store by freeing all resources and closing
    database connections. *)
  val close : t -> unit Lwt.t

  (** [use t k] executes [k] with a fresh connection to [t]. *)
  val use : t -> (conn -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

  (** [find_opt ?conn store hash] returns the cell associated to [hash] in
      the [store], if any. Uses the [conn] if provided (defaults to
      [None]). *)
  val find_opt :
    ?conn:conn ->
    t ->
    Skip_list_hash.t ->
    Skip_list_cell.t option tzresult Lwt.t

  (** [find_by_slot_id_opt ?conn store ~attested_level ~slot_index] returns the cell
      associated to ([attested_level], [slot_index]) in the [store], if
      any. Uses the [conn] if provided (defaults to [None]). *)
  val find_by_slot_id_opt :
    ?conn:conn ->
    t ->
    attested_level:int32 ->
    slot_index:int ->
    Skip_list_cell.t option tzresult Lwt.t

  (** [insert ?conn store ~attested_level values] inserts the given
      list of [values] associated to the given [attested_level] in the
      [store]. Any existing value is overridden. Uses the [conn] if
      provided (defaults to [None]). *)
  val insert :
    ?conn:conn ->
    t ->
    attested_level:int32 ->
    (Skip_list_hash.t * Skip_list_cell.t) list ->
    unit tzresult Lwt.t

  (** [remove ?conn store ~attested_level] removes any data related to
      [attested_level] from the [store]. Uses the [conn] if provided
      (defaults to [None]). *)
  val remove : ?conn:conn -> t -> attested_level:int32 -> unit tzresult Lwt.t

  (** [schemas t] returns the list of SQL statements
      allowing to recreate the tables of the store [t]. *)
  val schemas : t -> string list tzresult Lwt.t

  (** Internal functions for testing purpose.  *)
  module Internal_for_tests : sig
    val skip_list_hash_exists :
      ?conn:conn -> t -> Dal_proto_types.Skip_list_hash.t -> bool tzresult Lwt.t
  end
end
