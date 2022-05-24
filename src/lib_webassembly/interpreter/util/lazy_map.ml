module type KeyS = sig
  include Map.OrderedType

  val zero : t

  val add : t -> t -> t

  val sub : t -> t -> t

  val succ : t -> t

  val to_string : t -> string
end

module type S = sig
  type key

  module Map : Map.S with type key = key

  type 'a t

  val num_elements : 'a t -> key

  val create : ?values:'a Map.t -> ?produce_value:(key -> 'a) -> key -> 'a t

  val of_list : 'a list -> 'a t

  val get : key -> 'a t -> 'a

  val set : key -> 'a -> 'a t -> 'a t

  val grow : ?produce_value:(key -> 'a) -> key -> 'a t -> 'a t
end

exception UnexpectedAccess

module Make (Key : KeyS) : S with type key = Key.t = struct
  module Map = Map.Make (Key)

  type key = Key.t

  type 'a t = {
    num_elements: Key.t;
    produce_value : Key.t -> 'a;
    mutable values : 'a Map.t
  }

  let num_elements map = map.num_elements

  let def_produce_value _ = raise UnexpectedAccess

  let create ?(values = Map.empty) ?(produce_value = def_produce_value) num_elements =
    { num_elements; produce_value; values }

  let of_list values =
    let fold (map, len) value =
      Map.add len value map, Key.succ len
    in
    let values, num_elements =
      List.fold_left fold (Map.empty, Key.zero) values
    in
    create ~values num_elements

  let get key map =
    if
      Key.compare key map.num_elements >= 0 || Key.compare key Key.zero < 0
    then
      raise Memory_exn.Bounds;
    match Map.find_opt key map.values with
    | None ->
      (* Need to create the missing key-value association. *)
      let value = map.produce_value key in
      map.values <- Map.add key value map.values;
      value
    | Some value ->
      value

  let set key value map =
    if
      Key.compare key map.num_elements >= 0 || Key.compare key Key.zero < 0
    then
      raise Memory_exn.Bounds;
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

module IntMap = Make (Int)

module Int64Map = Make (Int64)

module Mutable = struct
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

  module Make (Key : KeyS) : S with type key = Key.t = struct
    module Map = Make (Key)

    type key = Map.key

    type 'a t = 'a Map.t ref

    let num_elements map_ref = Map.num_elements !map_ref

    let create ?values ?produce_value num_elements =
      ref (Map.create ?values ?produce_value num_elements )

    let get key map_ref = Map.get key !map_ref

    let set key value map_ref =
      map_ref := Map.set key value !map_ref

    let grow ?produce_value delta map_ref =
      map_ref := Map.grow ?produce_value delta !map_ref

    let snapshot map_ref = !map_ref
  end

  module IntMap = Make (Int)

  module Int64Map = Make (Int64)
end
