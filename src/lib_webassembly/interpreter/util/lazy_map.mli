(**
  A lazy map is a fixed-size key-value association where each value is created
  dynamically.

  In many ways this data structure mimics an array, but its lookup has
  logarithmic time complexity and it supports non-int keys - hence "map".
*)

(** [KeyS] is the qualifier signature for key types in the lazy map.
    Externally visible and accessible keys of the lazy map are always
    non-negative. However, the lazy map implementation may internally use
    negative keys therefore modules of type [KeyS] must support them.
*)
module type KeyS = sig
  include Map.OrderedType

  val zero : t

  val add : t -> t -> t

  val sub : t -> t -> t

  val pred : t -> t

  val succ : t -> t

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

  (** [of_list values] creates a map where each association is the index in the
      list to its value. The first item's key is [zero], the second is
      [succ zero] and so on. *)
  val of_list : 'a list -> 'a t

  (** [get key map] retrieves the element at [key].

      @raises Memory_exn.Bounds when trying to access an invalid key  *)
  val get : key -> 'a t -> 'a

  (** [set key value map] sets the element at [key] to [value].

      @raises Memory_exn.Bounds when trying to set an invalid key *)
  val set : key -> 'a -> 'a t -> 'a t

  (** [cons value map] prepends a value to the front. That value can then be
      accessed using the [zero] key.

      Time complexity: O(log(instantiated_elements_in_map)) *)
  val cons : 'a -> 'a t -> 'a t

  (** [grow delta ?produce_value map] creates a new lazy map that has [delta]
      more items than [map]. This also retains all values that have previously
      existed. New values will be created with [produce_values] if provided,
      starting with [Key.zero] for the new values. *)
  val grow : ?produce_value:(key -> 'a) -> key -> 'a t -> 'a t

  (** [concat lhs rhs] Concatenates two lazy maps. *)
  val concat : 'a t -> 'a t -> 'a t
end

(** [UnexpectedAccess] is raised in the default of the [produce_value] argument
    to [S.create]. *)
exception UnexpectedAccess

module Make (Key : KeyS) : S with type key = Key.t

module IntMap : S with type key = int

module Int32Map : S with type key = int32

module Int64Map : S with type key = int64

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

  module IntMap : S with type key = int

  module Int32Map : S with type key = int32

  module Int64Map : S with type key = int64
end
