
(**
  A lazy map is a fixed-size key-value association where each value is created
  dynamically.

  In many ways this data structure mimics an array, but its lookup has
  logarithmic time complexity and it supports non-int keys - hence "map".
*)

(** [KeyS] is the qualifier signature for key types in the lazy map. *)
module type KeyS = sig
  include Map.OrderedType

  val zero : t

  val add : t -> t -> t

  val sub : t -> t -> t

  val to_string : t -> string
end

(* [S] is the signature of an instantiated lazy map. *)
module type S = sig
  type key

  module Map : Map.S with type key = key

  type 'a t

  (** [num_elements map] returns the maximum number of elements in the lazy
      map. *)
  val num_elements : 'a t -> key

  (** [create ?values num_elements produce_value] produces a lazy map with
      [num_elements] entries where each is created using [produce_value].
      [values] may be provided to supply an initial set of entries. *)
  val create : ?values:'a Map.t -> ?produce_value:(key -> 'a) -> key -> 'a t

  (** [get key map] retrieves the element at [key] and for the case where that
      value needed to be instantiated first, it also returns the potentially
      extended [map]. *)
  val get : key -> 'a t -> 'a * 'a t

  (** [set key value map] sets the element at [key] to [value]. *)
  val set : key -> 'a -> 'a t -> 'a t

  (** [grow delta ?produce_value map] creates a new lazy map that has [delta]
      more items than [map]. This also retains all values that have previously
      existed. New values will be created with [produce_values] if provided. *)
  val grow : ?produce_value:(key -> 'a) -> key -> 'a t -> 'a t
end

(** [OutOfBounds] is raised when accessing an entry out of bounds. *)
exception OutOfBounds

(** [UnexpectedAccess] is raised in the default of the [produce_value] argument
    to [S.create]. *)
exception UnexpectedAccess

module Make (Key : KeyS) : S with type key = Key.t

(** [Make] generates a lazy map module using a given [Key] module. *)
module Mutable : sig
  (** [S] is the signature of a mutable lazy map module. *)
  module type S = sig
    type key

    module Map : S with type key = key

    type 'a t

    val num_elements : 'a t -> key

    val create : ?values:('a Map.Map.t) -> ?produce_value:(key -> 'a) -> key -> 'a t

    val get : key -> 'a t -> 'a

    val set : key -> 'a -> 'a t -> unit

    val grow : ?produce_value:(key -> 'a) -> key -> 'a t -> unit

    val snapshot : 'a t -> 'a Map.t
  end
  module Make (Key : KeyS) : S with type key = Key.t
end
