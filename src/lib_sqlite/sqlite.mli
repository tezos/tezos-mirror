(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** A handler to the database. *)
type t

(** A direct connection to the database, allowing to interact with it. *)
type conn

(** Permission mode for database access.

    Note that SQLite uses file-level locking for write operations. When using
    [Read_write] mode, write operations will acquire an exclusive lock on the
    database file, preventing other processes from writing to it
    simultaneously. In this case the pool size is always 1.
    In [Read_only] mode, multiple processes can read concurrently. *)
type perm = Read_only of {pool_size : int} | Read_write

(** {2 Initialization and backup} *)

(** [init ~path ~perm migratrion] returns a handler to the database
    located at [path] and executes the given [migration] code.

    If [sqlite_journal_mode] is [`Force mode], then the journal mode of the
    SQLite database is updated if necessary to match the requested
    configuration. With [`Identity], the journal mode is left untouched.

    If [perm] is [`Read_only], then SQL requests requiring write access will
    fail. With [`Read_write], they will succeed as expected.

    [pool_size] defaults to 8 and is the size of the connection pool.

    If [?max_conn_reuse_count:n] is provided, every connection in the pool will
    be reused at most [n] times, after which it will be disposed of.
*)
val init :
  path:string ->
  perm:perm ->
  ?max_conn_reuse_count:int ->
  ?register:(Sqlite3.db -> unit) ->
  (conn -> unit tzresult Lwt.t) ->
  t tzresult Lwt.t

val close : t -> unit Lwt.t

(** Rebuild the database in [output_db_file] using the
    {{:https://www.sqlite.org/lang_vacuum.html}[VACUUM] sqlite command}. This
    function is useful to backup the database. *)
val vacuum : conn:conn -> output_db_file:string -> unit tzresult Lwt.t

(** Vacuums the database itself after removing lot of data
    {{:https://www.sqlite.org/lang_vacuum.html}[VACUUM] sqlite command}. *)
val vacuum_self : conn:conn -> unit tzresult Lwt.t

(** {2 Database connections} *)

(** [use db k] executes [k] with a fresh connection to [db]. *)
val use : t -> (conn -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

(** [with_transaction conn k] wraps the accesses to the database from [conn] made
    in the continuation [k] within
    {{:https://www.sqlite.org/lang_transaction.html}a SQL transaction}. If [k]
    fails, the transaction is rollbacked. Otherwise, the transaction is
    committed. *)
val with_transaction : conn -> (conn -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

(** [assert_in_transaction conn] raises an exception if a transaction has not
    been started with [conn].

    @raise Assert_failure *)
val assert_in_transaction : conn -> unit

(** {2 Database low level queries} *)

(** Module for creating SQL requests with metadata for tracing and debugging.
    This is a wrapper around Caqti's request system with additional metadata. *)
module Request : sig
  (** Type of a SQL request with input type ['a], output type ['b], and
      multiplicity ['m]. The multiplicity indicates how many rows the request
      is expected to return:
      - [`Zero]: No rows
      - [`One]: Exactly one row
      - [`Many]: Zero or more rows *)
  type ('a, 'b, +'m) t constraint 'm = [< `Zero | `One | `Many]

  (** [a ->. unit] creates a request that takes input of type [a] and returns no
      rows. Useful for INSERT, UPDATE, DELETE operations.

      @param name Optional name for the request for tracing
      @param table Optional table name this request operates on
      @param attrs Optional function to extract OpenTelemetry attributes from
        the input
      @param oneshot If true, the request will not be prepared
      @return A request that expects no result rows *)
  val ( ->. ) :
    'a Caqti_type.t ->
    unit Caqti_type.t ->
    ?name:string ->
    ?table:string ->
    ?attrs:('a -> Opentelemetry.key_value list) ->
    ?oneshot:bool ->
    string ->
    ('a, unit, [`Zero]) t

  (** [a ->! b] creates a request that takes input of type [a] and returns
      exactly one row of type [b]. Useful for SELECT queries that must return
      exactly one result.

      @param name Optional name for the request for tracing
      @param table Optional table name this request operates on
      @param attrs Optional function to extract OpenTelemetry attributes from
        the input
      @param oneshot If true, the request will not be prepared
      @return A request that expects exactly one result row *)
  val ( ->! ) :
    'a Caqti_type.t ->
    'b Caqti_type.t ->
    ?name:string ->
    ?table:string ->
    ?attrs:('a -> Opentelemetry.key_value list) ->
    ?oneshot:bool ->
    string ->
    ('a, 'b, [`One]) t

  (** [a ->? b] creates a request that takes input of type [a] and returns zero
      or one row of type [b]. Useful for SELECT queries that may not find a
      result.

      @param name Optional name for the request for tracing
      @param table Optional table name this request operates on
      @param attrs Optional function to extract OpenTelemetry attributes from
        the input
      @param oneshot If true, the request will not be prepared
      @return A request that expects zero or one result row *)
  val ( ->? ) :
    'a Caqti_type.t ->
    'b Caqti_type.t ->
    ?name:string ->
    ?table:string ->
    ?attrs:('a -> Opentelemetry.key_value list) ->
    ?oneshot:bool ->
    string ->
    ('a, 'b, [`One | `Zero]) t

  (** [a ->* b] creates a request that takes input of type [a] and returns any
      number of rows of type [b]. Useful for SELECT queries that return multiple
      results.

      @param name Optional name for the request for tracing
      @param table Optional table name this request operates on
      @param attrs Optional function to extract OpenTelemetry attributes from
        the input
      @param oneshot If true, the request will not be prepared
      @return A request that can return any number of result rows *)
  val ( ->* ) :
    'a Caqti_type.t ->
    'b Caqti_type.t ->
    ?name:string ->
    ?table:string ->
    ?attrs:('a -> Opentelemetry.key_value list) ->
    ?oneshot:bool ->
    string ->
    ('a, 'b, [`Many | `One | `Zero]) t
end

(** Caqti convenience functions wrapped in the Tezos error monad. See
    {!Caqti_connection_sig.Convenience}.  *)
module Db : sig
  (** Type representing a direct connection to the database with tracing
      capabilities. This is used internally by the connection handling
      functions. *)
  type conn

  (** [exec req x] performs [req] with parameters [x] and checks that no rows
      are returned. *)
  val exec :
    ?scope:Opentelemetry.Scope.t ->
    conn ->
    ('a, unit, [< `Zero]) Request.t ->
    'a ->
    unit tzresult Lwt.t

  (** [find req x] performs [req] with parameters [x], checks that a single row
      is retured, and returns it. *)
  val find :
    ?scope:Opentelemetry.Scope.t ->
    conn ->
    ('a, 'b, [< `One]) Request.t ->
    'a ->
    'b tzresult Lwt.t

  (** [find_opt req x] performs [req] with parameters [x] and returns either
      [None] if no rows are returned or [Some y] if a single now [y] is returned
      and fails otherwise. *)
  val find_opt :
    ?scope:Opentelemetry.Scope.t ->
    conn ->
    ('a, 'b, [< `One | `Zero]) Request.t ->
    'a ->
    'b option tzresult Lwt.t

  (** [collect_list request x] performs a [req] with parameters [x] and returns
      a list of rows in order of retrieval.  The accumulation is tail recursive
      but slightly less efficient than {!rev_collect_list}. *)
  val collect_list :
    ?scope:Opentelemetry.Scope.t ->
    conn ->
    ('a, 'b, [< `Many | `One | `Zero]) Request.t ->
    'a ->
    'b list tzresult Lwt.t

  (** [rev_collect_list request x] performs [request] with parameters [x] and
      returns a list of rows in the reverse order of retrieval.  The
      accumulation is tail recursive and slighly more efficient than
      {!collect_list}. *)
  val rev_collect_list :
    ?scope:Opentelemetry.Scope.t ->
    conn ->
    ('a, 'b, [< `Many | `One | `Zero]) Request.t ->
    'a ->
    'b list tzresult Lwt.t

  (** [fold req f x acc] performs [req] with parameters [x] and passes [acc]
      through the composition of [f y] across the result rows [y] in the order
      of retrieval. *)
  val fold :
    ?scope:Opentelemetry.Scope.t ->
    conn ->
    ('a, 'b, [< `Many | `One | `Zero]) Request.t ->
    ('b -> 'c -> 'c) ->
    'a ->
    'c ->
    'c tzresult Lwt.t

  (** [fold_s req f x acc] performs [req] with parameters [x] and passes [acc]
      through the monadic composition of [f y] across the returned rows [y] in
      the order of retrieval.

      Please be aware of possible deadlocks when using resources from the
      callback.  In particular, if the same connection pool is invoked as the
      one used to obtain the current connection, it will deadlock if the pool
      has just run out of connections.  An alternative is to collect the rows
      first e.g. with {!fold} and do the nested queries after exiting.*)
  val fold_s :
    ?scope:Opentelemetry.Scope.t ->
    conn ->
    ('a, 'b, [< `Many | `One | `Zero]) Request.t ->
    ('b -> 'c -> ('c, Caqti_error.t) result Lwt.t) ->
    'a ->
    'c ->
    'c tzresult Lwt.t

  (** [iter_s req f x] performs [req] with parameters [x] and sequences calls to
      [f y] for each result row [y] in the order of retrieval.

      Please see the warning in {!fold_s} about resource usage in the
      callback. *)
  val iter_s :
    ?scope:Opentelemetry.Scope.t ->
    conn ->
    ('a, 'b, [< `Many | `One | `Zero]) Request.t ->
    ('b -> (unit, Caqti_error.t) result Lwt.t) ->
    'a ->
    unit tzresult Lwt.t
end

(** [with_connection conn k] allows to wraps atomic low level accesses to the
    database from [conn]. [with_connection] can be used in the continuation of
    {!with_transaction}. *)
val with_connection : conn -> (Db.conn -> 'a) -> 'a
