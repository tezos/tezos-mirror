(**
  A lazy map is a key-value association where each value is created dynamically.
*)

module Effect : sig
  module type S = sig
    type 'a t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val return : 'a -> 'a t
  end

  module Identity : S with type 'a t = 'a

  module Lwt : S with type 'a t = 'a Lwt.t
end

(** [KeyS] is the qualifier signature for key types in the lazy map.
    Externally visible and accessible keys of the lazy map are always
    non-negative. However, the lazy map implementation may internally use
    negative keys therefore modules of type [KeyS] must support them.
*)
module type KeyS = sig
  include Map.OrderedType

  val to_string : t -> string
end

(* [S] is the signature of an instantiated lazy map. *)
module type S = sig
  type key

  type 'a effect

  type 'a producer = key -> 'a effect

  module Map : Map.S with type key = key

  type 'a t

  (** [pp pp_value] gives you a pretty-printer. This function is a witness of
      internal mutation. *)
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  (** [to_string show map] generates a string representation of [map] by using
      [show] for its values. Like [pp] this function is witness of internal
      mutation. *)
  val to_string : ('a -> string) -> 'a t -> string

  (** [create ?values num_elements produce_value] produces a lazy map with
      [num_elements] entries where each is created using [produce_value].
      [values] may be provided to supply an initial set of entries. *)
  val create : ?values:'a Map.t -> ?produce_value:'a producer -> unit -> 'a t

  (** [get key map] retrieves the element at [key].

      @raises Memory_exn.Bounds when trying to access an invalid key  *)
  val get : key -> 'a t -> 'a effect

  (** [set key value map] sets the element at [key] to [value].

      @raises Memory_exn.Bounds when trying to set an invalid key *)
  val set : key -> 'a -> 'a t -> 'a t

  (** [merge_into ?choose_producer ?map_key source dest] produces a new lazy map
      by merging [source] into [dest].

      The keys of [source] can be transformed using [map_key] prior to merging.
      By default no keys will be transformed.

      If a [key] was instantiated in [dest] it will also be present in the
      resulting lazy map.

      [choose_producer source_producer dest_producer] will be
      used to determine the new producer function - by default the [dest]
      producer is chosen. *)
  val merge_into :
    ?map_key:(key -> key) ->
    ?choose_producer:('a producer -> 'a producer -> 'a producer) ->
    'a t ->
    'a t ->
    'a t

  (** [with_producer morph] lifts a morphism for a [producer] to one on [t]. *)
  val with_producer : ('a producer -> 'a producer) -> 'a t -> 'a t
end

(** [UnexpectedAccess] is raised in the default of the [produce_value] argument
    to [S.create]. *)
exception UnexpectedAccess

module Make (Effect : Effect.S) (Key : KeyS) :
  S with type key = Key.t and type 'a effect = 'a Effect.t

module IntMap : S with type key = int and type 'a effect = 'a

module Int32Map : S with type key = int32 and type 'a effect = 'a

module Int64Map : S with type key = int64 and type 'a effect = 'a

module LwtIntMap : S with type key = int and type 'a effect = 'a Lwt.t

module LwtInt32Map : S with type key = int32 and type 'a effect = 'a Lwt.t

module LwtInt64Map : S with type key = int64 and type 'a effect = 'a Lwt.t

(** [Make] generates a lazy map module using a given [Key] module. *)
module Mutable : sig
  (** [S] is the signature of a mutable lazy map module. *)
  module type S = sig
    type key

    type 'a effect

    module Map : S with type key = key and type 'a effect = 'a effect

    type 'a t

    val of_immutable : 'a Map.t -> 'a t

    val create :
      ?values:'a Map.Map.t -> ?produce_value:'a Map.producer -> unit -> 'a t

    val get : key -> 'a t -> 'a Map.effect

    val set : key -> 'a -> 'a t -> unit

    val snapshot : 'a t -> 'a Map.t
  end

  module Make (Effect : Effect.S) (Key : KeyS) :
    S with type key = Key.t and type 'a effect = 'a Effect.t

  module IntMap : S with type key = int and type 'a effect = 'a

  module Int32Map : S with type key = int32 and type 'a effect = 'a

  module Int64Map : S with type key = int64 and type 'a effect = 'a

  module LwtIntMap : S with type key = int and type 'a effect = 'a Lwt.t

  module LwtInt32Map : S with type key = int32 and type 'a effect = 'a Lwt.t

  module LwtInt64Map : S with type key = int64 and type 'a effect = 'a Lwt.t
end
