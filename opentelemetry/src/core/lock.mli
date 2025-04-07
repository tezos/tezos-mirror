val set_mutex : lock:(unit -> unit) -> unlock:(unit -> unit) -> unit
(** Set a pair of lock/unlock functions that are used to
    protect access to global state, if needed. By default these do nothing. *)

val with_lock : (unit -> 'a) -> 'a
(** Call [f()] while holding the mutex defined {!set_mutex}, then
    release the mutex. *)
