type t = {
  mutable bytes_read : int;
  mutable nb_reads : int;
  mutable bytes_written : int;
  mutable nb_writes : int;
  mutable nb_merge : int;
  mutable nb_replace : int;
}

val reset_stats : unit -> unit

val get : unit -> t

val add_read : int -> unit

val add_write : int -> unit

val incr_nb_merge : unit -> unit
