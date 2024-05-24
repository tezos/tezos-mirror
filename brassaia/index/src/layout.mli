(** Defines the namespacing of the various IO instances required by Index. *)

open! Import

val log : root:string -> string
val log_async : root:string -> string
val data : root:string -> string
val lock : root:string -> string
val merge : root:string -> string
