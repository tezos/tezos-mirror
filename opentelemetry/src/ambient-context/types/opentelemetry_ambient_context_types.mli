(** Storage implementation.

    There is a singleton storage for a given program, responsible for providing ambient
    context to the rest of the program. *)

type 'a key = 'a Hmap.key

module type STORAGE = sig
  val name : string
  (** Name of the storage implementation. *)

  val get_map : unit -> Hmap.t option
  (** Get the hmap from the current ambient context, or [None] if there is no ambient
      context. *)

  val with_map : Hmap.t -> (unit -> 'b) -> 'b
  (** [with_hmap h cb] calls [cb()] in an ambient context in which [get_map()] will return
      [h]. Once [cb()] returns, the storage is reset to its previous value. *)

  val create_key : unit -> 'a key
  (** Create a new storage key, guaranteed to be distinct from any previously created key. *)

  val get : 'a key -> 'a option

  val with_binding : 'a key -> 'a -> (unit -> 'b) -> 'b

  val without_binding : 'a key -> (unit -> 'b) -> 'b
end

type storage = (module STORAGE)
