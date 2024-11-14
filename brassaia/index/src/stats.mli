open! Import

type t = {
  mutable bytes_read : int;
  mutable nb_reads : int;
  mutable bytes_written : int;
  mutable nb_writes : int;
  mutable nb_merge : int;
  mutable merge_durations : float list;
  mutable nb_replace : int;
  mutable replace_durations : float list;
  mutable nb_sync : int;
  mutable time_sync : float;
  mutable lru_hits : int;
  mutable lru_misses : int;
}
(** The type for stats for an index I.

    - [bytes_read] is the number of bytes read from disk;
    - [nb_reads] is the number of reads from disk;
    - [bytes_written] is the number of bytes written to disk;
    - [nb_writes] is the number of writes to disk;
    - [nb_merge] is the number of times a merge occurred;
    - [merge_durations] lists how much time the last 10 merge operations took
      (in microseconds);
    - [nb_replace] is the number of calls to [I.replace];
    - [replace_durations] lists how much time replace operations took. Each
      element is an average of [n] consecutive replaces, where [n] is the
      [sampling_interval] specified when calling [end_replace].
    - [time_sync] is the duration of the latest call to sync. *)

val get : unit -> t
val reset_stats : unit -> unit
val add_read : int -> unit
val add_write : int -> unit
val incr_nb_merge : unit -> unit
val incr_nb_replace : unit -> unit
val incr_nb_sync : unit -> unit
val incr_nb_lru_hits : unit -> unit
val incr_nb_lru_misses : unit -> unit

module Make (_ : Platform.CLOCK) : sig
  val start_replace : unit -> unit
  val end_replace : sampling_interval:int -> unit
  val sync_with_timer : (unit -> unit) -> unit
  val add_merge_duration : Mtime.Span.t -> unit
end
