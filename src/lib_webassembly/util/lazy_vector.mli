(**
  A lazy vector is a fixed-size key-value association where each value is
  created dynamically.

  In many ways this data structure mimics an array, but its lookup has
  logarithmic time complexity and it supports non-int keys.
*)

module Effect : sig
  module type S = sig
    include Lazy_map.Effect.S

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Identity : S with type 'a t = 'a

  module Lwt : S with type 'a t = 'a Lwt.t
end

(** [KeyS] is the qualifier signature for key types in the lazy vector.
    Externally visible and accessible keys of the lazy vector are always
    non-negative. However, the lazy vector implementation may internally use
    negative keys therefore modules of type [KeyS] must support them.
*)
module type KeyS = sig
  include Map.OrderedType

  val unsigned_compare : t -> t -> int

  val zero : t

  val add : t -> t -> t

  val sub : t -> t -> t

  val pred : t -> t

  val succ : t -> t

  val to_string : t -> string
end

(* [S] is the signature of an instantiated lazy vector. *)
module type S = sig
  type key

  type 'a effect

  type 'a producer = key -> 'a effect

  module Map : Lazy_map.S with type key = key and type 'a effect = 'a effect

  type 'a t

  (** [pp pp_value] gives you a pretty-printer. This function is a witness of
      internal mutation. *)
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  (** [to_string show vector] generates a string representation of [vector] by
      using [show] for its values. Like [pp] this function is witness of
      internal mutation. *)
  val to_string : ('a -> string) -> 'a t -> string

  (** [num_elements vector] returns the maximum number of elements in the lazy
      vector. *)
  val num_elements : 'a t -> key

  (** [create ?values ?produce_value num_elements] produces a lazy vector with
      [num_elements] entries where each is created using [produce_value].
      [values] may be provided to supply an initial set of entries. *)
  val create : ?values:'a Map.Map.t -> ?produce_value:'a producer -> key -> 'a t

  (** [of_list values] creates a vector where each association is the index in
      the list to its value. The first item's key is [zero], the second is
      [succ zero] and so on. *)
  val of_list : 'a list -> 'a t

  (** [get key vector] retrieves the element at [key].

      @raises Memory_exn.Bounds when trying to access an invalid key  *)
  val get : key -> 'a t -> 'a effect

  (** [set key value vector] sets the element at [key] to [value].

      @raises Memory_exn.Bounds when trying to set an invalid key *)
  val set : key -> 'a -> 'a t -> 'a t

  (** [cons value vector] prepends a value to the front. That value can then be
      accessed using the [zero] key.

      Time complexity: O(log(instantiated_elements_in_vector)) *)
  val cons : 'a -> 'a t -> 'a t

  (** [grow delta ?produce_value vector] creates a new lazy vector that has
      [delta] more items than [vector]. This also retains all values that have
      previously been created. New values will be created with [produce_values]
      if provided, starting with [Key.zero] for the new values. *)
  val grow : ?produce_value:'a producer -> key -> 'a t -> 'a t

  (** [concat lhs rhs] Concatenates two lazy vectors. *)
  val concat : 'a t -> 'a t -> 'a t

  (** [to_list vector] extracts all values of the given [vector] and
      collects them in a list.  *)
  val to_list : 'a t -> 'a list effect
end

module Make (Effect : Effect.S) (Key : KeyS) :
  S with type key = Key.t and type 'a effect = 'a Effect.t

module IntVector : S with type key = int and type 'a effect = 'a

module Int32Vector : S with type key = int32 and type 'a effect = 'a

module Int64Vector : S with type key = int64 and type 'a effect = 'a

module LwtIntVector : S with type key = int and type 'a effect = 'a Lwt.t

module LwtInt32Vector : S with type key = int32 and type 'a effect = 'a Lwt.t

module LwtInt64Vector : S with type key = int64 and type 'a effect = 'a Lwt.t

(** [Make] generates a lazy vector module using a given [Key] module. *)
module Mutable : sig
  (** [S] is the signature of a mutable lazy vector module. *)
  module type S = sig
    type key

    type 'a effect

    module Vector : S with type key = key and type 'a effect = 'a effect

    type 'a t

    val num_elements : 'a t -> key

    val of_immutable : 'a Vector.t -> 'a t

    val create :
      ?values:'a Vector.Map.Map.t ->
      ?produce_value:'a Vector.producer ->
      key ->
      'a t

    val get : key -> 'a t -> 'a Vector.effect

    val set : key -> 'a -> 'a t -> unit

    val grow : ?produce_value:'a Vector.producer -> key -> 'a t -> unit

    val snapshot : 'a t -> 'a Vector.t
  end

  module Make (Effect : Effect.S) (Key : KeyS) :
    S with type key = Key.t and type 'a effect = 'a Effect.t

  module IntVector : S with type key = int and type 'a effect = 'a

  module Int32Vector : S with type key = int32 and type 'a effect = 'a

  module Int64Vector : S with type key = int64 and type 'a effect = 'a

  module LwtIntVector : S with type key = int and type 'a effect = 'a Lwt.t

  module LwtInt32Vector : S with type key = int32 and type 'a effect = 'a Lwt.t

  module LwtInt64Vector : S with type key = int64 and type 'a effect = 'a Lwt.t
end
