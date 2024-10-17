(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** A handler to the DAL node's SQLite3 database. *)
type t = Sqlite.t

(** A direct connection to the database, allowing to interact with it. *)
type conn = Sqlite.conn

(** [use db k] executes [k] with a fresh connection to [db]. *)
val use : t -> (conn -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

(** [init ~data_dir ~perm ()] returns a handler to the DAL node store
    located under [data_dir]. If no store is located in [data_dir], an
    empty store is created.

    If [perm] is [`Read_only], then SQL requests requiring write access will
    fail. With [`Read_write], they will succeed as expected. *)
val init :
  data_dir:string -> perm:[`Read_only | `Read_write] -> unit -> t tzresult Lwt.t

(** Name of the SQLite3 file. *)
val sqlite_file_name : string

module Schemas : sig
  (** [get_all conn] returns the list of SQL statements allowing to recreate
      the tables in the current store. *)
  val get_all : conn -> string list tzresult Lwt.t
end

module Skip_list_cells : sig
  open Dal_proto_types

  (** [find ?conn store hash] returns the cell associated to [hash] in
      the [store], if any. Uses the [conn] if provided (defaults to
      [None]). *)
  val find :
    ?conn:conn -> t -> Skip_list_hash.t -> Skip_list_cell.t tzresult Lwt.t

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
end
