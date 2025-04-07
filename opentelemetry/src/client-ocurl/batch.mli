(** List of lists with length *)

type 'a t

val create : unit -> 'a t

val push : 'a t -> 'a list -> unit

val len : _ t -> int

val time_started : _ t -> Mtime.t
(** Time at which the batch most recently became non-empty *)

val pop_all : 'a t -> 'a list list
