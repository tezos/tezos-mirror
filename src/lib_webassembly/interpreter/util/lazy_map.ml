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

(** [Make] generates a lazy map module using a given [Key] module. *)
module Make (Key : KeyS) : S with type key = Key.t = struct
  module Map = Map.Make (Key)

  type key = Key.t

  type 'a t = {
    num_elements: Key.t;
    produce_value : Key.t -> 'a;
    values : 'a Map.t
  }

  let num_elements map = map.num_elements

  let def_produce_value _ = raise UnexpectedAccess

  let create ?(values = Map.empty) ?(produce_value = def_produce_value) num_elements =
    { num_elements; produce_value; values }

  let get key map =
    if
      Key.compare key map.num_elements >= 0 || Key.compare key Key.zero < 0
    then
      raise OutOfBounds;
    match Map.find_opt key map.values with
    | None ->
      (* Need to create the missing key-value association. *)
      let value = map.produce_value key in
      let values = Map.add key value map.values in
      value, { map with values }
    | Some value ->
      value, map

  let set key value map =
    if
      Key.compare key map.num_elements >= 0 || Key.compare key Key.zero < 0
    then
      raise OutOfBounds;
    { map with values = Map.add key value map.values }

  let grow ?produce_value delta map =
    (* Make sure we only grow and never shrink. *)
    if Key.compare delta Key.zero > 0 then
      let produce_value =
        match produce_value with
        | Some produce_new_value ->
          fun key ->
            if Key.compare key map.num_elements >= 0 then
              produce_new_value key
            else
              map.produce_value key
        | None -> map.produce_value
      in
      let num_elements = Key.add map.num_elements delta in
      { map with produce_value; num_elements }
    else
      map
end

(** [Mutable] exposes an imperative API for lazy maps. *)
module Mutable = struct
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

  (** [Make] generates a lazy map module with imperative API. *)
  module Make (Key : KeyS) : S with type key = Key.t = struct
    module Map = Make (Key)

    type key = Map.key

    type 'a t = 'a Map.t ref

    let num_elements map_ref = Map.num_elements !map_ref

    let create ?values ?produce_value num_elements =
      ref (Map.create ?values ?produce_value num_elements )

    let get key map_ref =
      let value, map = Map.get key !map_ref in
      map_ref := map;
      value

    let set key value map_ref =
      map_ref := Map.set key value !map_ref

    let grow ?produce_value delta map_ref =
      map_ref := Map.grow ?produce_value delta !map_ref

    let snapshot map_ref = !map_ref
  end
end
