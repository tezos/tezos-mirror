(** Configure and manipulate a “root-path.”   *)

type t

val make : string -> t

val pp : Format.formatter -> t -> unit

(** Query the configured root-path. *)
val root : < paths : t ; .. > -> string

val cli_term :
  ?option_name:string -> default_root:string -> unit -> t Cmdliner.Term.t
