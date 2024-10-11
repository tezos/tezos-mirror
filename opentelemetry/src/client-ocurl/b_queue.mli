(** Basic Blocking Queue *)

type 'a t

val create : unit -> _ t

exception Closed

val push : 'a t -> 'a -> unit
(** [push q x] pushes [x] into [q], and returns [()].
    @raise Closed if [close q] was previously called.*)

val pop : 'a t -> 'a
(** [pop q] pops the next element in [q]. It might block until an element comes.
   @raise Closed if the queue was closed before a new element was available. *)

val pop_all : 'a t -> 'a Queue.t -> unit
(** [pop_all q into] pops all the elements of [q]
  and moves them into [into]. It might block until an element comes.
   @raise Closed if the queue was closed before a new element was available. *)

val close : _ t -> unit
(** Close the queue, meaning there won't be any more [push] allowed. *)
