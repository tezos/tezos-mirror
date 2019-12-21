type t = {
  mutable bytes_read : int;
  mutable nb_reads : int;
  mutable bytes_written : int;
  mutable nb_writes : int;
  mutable nb_merge : int;
  mutable nb_replace : int;
}

let fresh_stats () =
  {
    bytes_read = 0;
    nb_reads = 0;
    bytes_written = 0;
    nb_writes = 0;
    nb_merge = 0;
    nb_replace = 0;
  }

let stats = fresh_stats ()

let reset_stats () =
  stats.bytes_read <- 0;
  stats.nb_reads <- 0;
  stats.bytes_written <- 0;
  stats.nb_writes <- 0;
  stats.nb_merge <- 0;
  stats.nb_replace <- 0

let get () = stats

let incr_bytes_read n = stats.bytes_read <- stats.bytes_read + n

let incr_bytes_written n = stats.bytes_written <- stats.bytes_written + n

let incr_nb_reads () = stats.nb_reads <- succ stats.nb_reads

let incr_nb_writes () = stats.nb_writes <- succ stats.nb_writes

let incr_nb_merge () = stats.nb_merge <- succ stats.nb_merge

let add_read n =
  incr_bytes_read n;
  incr_nb_reads ()

let add_write n =
  incr_bytes_written n;
  incr_nb_writes ()
