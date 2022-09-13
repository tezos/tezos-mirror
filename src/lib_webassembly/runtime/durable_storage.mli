(** The type of kernel's durable storage. *)
type t

exception Durable_empty

val empty : t

val of_tree : Lazy_containers.Lazy_map.tree -> t

(** @raise Durable_empty *)
val to_tree_exn : t -> Lazy_containers.Lazy_map.tree

val to_tree : t -> Lazy_containers.Lazy_map.tree option
