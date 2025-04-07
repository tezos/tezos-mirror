(** Ambient context.

    The ambient context, like the Matrix, is everywhere around you.

    It is responsible for keeping track of that context in a manner that's consistent with
    the program's choice of control flow paradigm:

    - for synchronous/threaded/direct style code, {b TLS} ("thread local storage") keeps
      track of a global variable per thread. Each thread has its own copy of the variable
      and updates it independently of other threads.

    - for Lwt, any ['a Lwt.t] created inside the [with_binding k v (fun _ -> …)] will
      inherit the [k := v] assignment.

    - for Eio, fibers created inside [with_binding k v (fun () -> …)] will inherit the
      [k := v] assignment. This is consistent with the structured concurrency approach of
      Eio.

    The only data stored by this storage is a {!Hmap.t}, ie a heterogeneous map. Various
    users (libraries, user code, etc.) can create their own {!key} to store what they are
    interested in, without affecting other parts of the storage. *)

module Types := Opentelemetry_ambient_context_types

module type STORAGE = Types.STORAGE

type storage = (module STORAGE)

val default_storage : storage

val get_current_storage : unit -> storage

val set_storage_provider : storage -> unit

type 'a key
(** A key that can be mapped to values of type ['a] in the ambient context. *)

val compare_key : int -> int -> int
(** Total order on keys *)

val create_key : unit -> 'a key
(** Create a new fresh key, distinct from any previously created key. *)

val get : 'a key -> 'a option
(** Get the current value for a given key, or [None] if no value was associated with the
    key in the ambient context. *)

val with_binding : 'a key -> 'a -> (unit -> 'r) -> 'r
(** [with_binding k v cb] calls [cb()] in a context in which [k] is bound to [v]. This
    does not affect storage outside of [cb()]. *)

val without_binding : 'a key -> (unit -> 'b) -> 'b
(** [without_binding k cb] calls [cb()] in a context where [k] has no binding (possibly
    shadowing the current ambient binding of [k] if it exists). *)
