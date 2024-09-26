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

(** {2 Initialization and backup} *)

(** [init ~path ~perm migratrion] returns a handler to the database located at
    [path] and executes the given [migration] code.

    If [sqlite_journal_mode] is [`Force mode], then the journal mode of the
    SQLite database is updated if necessary to match the requested
    configuration. With [`Identity], the journal mode is left untouched.

    If [perm] is [`Read_only], then SQL requests requiring write access will
    fail. With [`Read_write], they will succeed as expected. *)
val init :
  path:string ->
  perm:[`Read_only | `Read_write] ->
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

(** [with_connection conn k] allows to wraps atomic low level accesses to the
    database from [conn]. [with_connection] can be used in the continuation of
    {!with_transaction}. *)
val with_connection : conn -> ((module Caqti_lwt.CONNECTION) -> 'a) -> 'a

(** {2 Database low level queries} *)

(** Caqti convenience functions wrapped in the Tezos error monad. See
    {!Caqti_connection_sig.Convenience}.  *)
module Db : sig
  (** [exec req x] performs [req] with parameters [x] and checks that no rows
      are returned. *)
  val exec :
    (module Caqti_lwt.CONNECTION) ->
    ('a, unit, [< `Zero]) Caqti_request.t ->
    'a ->
    unit tzresult Lwt.t

  (** [find req x] performs [req] with parameters [x], checks that a single row
      is retured, and returns it. *)
  val find :
    (module Caqti_lwt.CONNECTION) ->
    ('a, 'b, [< `One]) Caqti_request.t ->
    'a ->
    'b tzresult Lwt.t

  (** [find_opt req x] performs [req] with parameters [x] and returns either
      [None] if no rows are returned or [Some y] if a single now [y] is returned
      and fails otherwise. *)
  val find_opt :
    (module Caqti_lwt.CONNECTION) ->
    ('a, 'b, [< `One | `Zero]) Caqti_request.t ->
    'a ->
    'b option tzresult Lwt.t

  (** [collect_list request x] performs a [req] with parameters [x] and returns
      a list of rows in order of retrieval.  The accumulation is tail recursive
      but slightly less efficient than {!rev_collect_list}. *)
  val collect_list :
    (module Caqti_lwt.CONNECTION) ->
    ('a, 'b, [< `Many | `One | `Zero]) Caqti_request.t ->
    'a ->
    'b list tzresult Lwt.t

  (** [rev_collect_list request x] performs [request] with parameters [x] and
      returns a list of rows in the reverse order of retrieval.  The
      accumulation is tail recursive and slighly more efficient than
      {!collect_list}. *)
  val rev_collect_list :
    (module Caqti_lwt.CONNECTION) ->
    ('a, 'b, [< `Many | `One | `Zero]) Caqti_request.t ->
    'a ->
    'b list tzresult Lwt.t

  (** [fold req f x acc] performs [req] with parameters [x] and passes [acc]
      through the composition of [f y] across the result rows [y] in the order
      of retrieval. *)
  val fold :
    (module Caqti_lwt.CONNECTION) ->
    ('a, 'b, [< `Many | `One | `Zero]) Caqti_request.t ->
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
    (module Caqti_lwt.CONNECTION) ->
    ('a, 'b, [< `Many | `One | `Zero]) Caqti_request.t ->
    ('b -> 'c -> ('c, Caqti_error.t) result Lwt.t) ->
    'a ->
    'c ->
    'c tzresult Lwt.t

  (** [iter_s req f x] performs [req] with parameters [x] and sequences calls to
      [f y] for each result row [y] in the order of retrieval.

      Please see the warning in {!fold_s} about resource usage in the
      callback. *)
  val iter_s :
    (module Caqti_lwt.CONNECTION) ->
    ('a, 'b, [< `Many | `One | `Zero]) Caqti_request.t ->
    ('b -> (unit, Caqti_error.t) result Lwt.t) ->
    'a ->
    unit tzresult Lwt.t
end
