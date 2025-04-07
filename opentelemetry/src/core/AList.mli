(** Atomic list *)

type 'a t

val get : 'a t -> 'a list
(** Snapshot *)

val is_empty : _ t -> bool

val make : unit -> 'a t

val add : 'a t -> 'a -> unit

val pop_all : 'a t -> 'a list
