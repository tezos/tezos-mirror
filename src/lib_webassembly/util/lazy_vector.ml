module Effect = struct
  module type S = sig
    include Lazy_map.Effect.S

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Identity : S with type 'a t = 'a = struct
    include Lazy_map.Effect.Identity

    let ( let* ) x f = f x
  end

  module Lwt : S with type 'a t = 'a Lwt.t = struct
    include Lazy_map.Effect.Lwt

    let ( let* ) = Lwt.bind
  end
end

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

module type S = sig
  type key

  type 'a effect

  type 'a producer = key -> 'a effect

  module Map : Lazy_map.S with type key = key and type 'a effect = 'a effect

  type 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val to_string : ('a -> string) -> 'a t -> string

  val num_elements : 'a t -> key

  val create : ?values:'a Map.Map.t -> ?produce_value:'a producer -> key -> 'a t

  val empty : unit -> 'a t

  val singleton : 'a -> 'a t

  val of_list : 'a list -> 'a t

  val get : key -> 'a t -> 'a effect

  val set : key -> 'a -> 'a t -> 'a t

  val cons : 'a -> 'a t -> 'a t

  val grow : ?produce_value:'a producer -> key -> 'a t -> 'a t

  val concat : 'a t -> 'a t -> 'a t

  val to_list : 'a t -> 'a list effect

  val loaded_bindings : 'a t -> (key * 'a) list
end

module ZZ : KeyS with type t = Z.t = struct
  include Z

  (** Note that, in fixed sized integers we need to use a specialized `unsigned`
       version of compare. This is because, internally, some keys can be
       represented by negative integers (using wraparound). For example after a
       while the value of num_elements will surpass max_int and so it will
       become negative Nevertheless we still want this to represent large
       unsigned integers (up until 2*max_int). In the case of Z this is not an
       issue as there is no wraparound.*)
  let unsigned_compare = Z.compare
end

module Make (Effect : Effect.S) (Key : KeyS) :
  S with type key = Key.t and type 'a effect = 'a Effect.t = struct
  module Map = Lazy_map.Make (Effect) (Key)

  type key = Key.t

  type 'a effect = 'a Effect.t

  type 'a producer = key -> 'a effect

  type 'a t = {first : key; num_elements : key; values : 'a Map.t}

  let pp pp_value fmt map =
    Format.fprintf
      fmt
      "@[<hv 2>{ first = %s;@ num_elements = %s;@ values = %a }@]"
      (Key.to_string map.first)
      (Key.to_string map.num_elements)
      (Map.pp pp_value)
      map.values

  let to_string show_value map =
    let pp_value fmt value = Format.pp_print_string fmt (show_value value) in
    Format.asprintf "%a" (pp pp_value) map

  let num_elements map = map.num_elements

  let create ?values ?produce_value num_elements =
    let values = Map.create ?values ?produce_value () in
    {first = Key.zero; num_elements; values}

  let empty () = create Key.zero

  let of_list values =
    let fold (map, len) value = (Map.Map.add len value map, Key.succ len) in
    let values, num_elements =
      List.fold_left fold (Map.Map.empty, Key.zero) values
    in
    create ~values num_elements

  let invalid_key key map = Key.unsigned_compare key map.num_elements >= 0

  let get key map =
    if invalid_key key map then raise Memory_exn.Bounds ;
    let key = Key.add map.first key in
    Map.get key map.values

  let set key value map =
    if invalid_key key map then raise Memory_exn.Bounds ;
    let key = Key.add map.first key in
    {map with values = Map.set key value map.values}

  let singleton value = create Key.(succ zero) |> set Key.zero value

  let cons value map =
    let first = Key.pred map.first in
    let values = Map.set first value map.values in
    let num_elements = Key.succ map.num_elements in
    {first; values; num_elements}

  let grow ?produce_value delta map =
    if
      Key.unsigned_compare (Key.add delta map.num_elements) map.num_elements < 0
    then raise Memory_exn.SizeOverflow ;

    let map_produce_value old_produce_value =
      match produce_value with
      | Some produce_new_value ->
          let boundary = Key.add map.num_elements map.first in
          fun key ->
            if Key.compare key boundary >= 0 then
              (* Normalize the key so that it is relative to the boundary.
                  The first new value will be produced with
                  [produce_value Key.zero]. *)
              let key = Key.sub key boundary in
              produce_new_value key
            else old_produce_value key
      | None -> old_produce_value
    in
    let values = Map.with_producer map_produce_value map.values in
    let num_elements = Key.add map.num_elements delta in
    {map with values; num_elements}

  let concat lhs rhs =
    let boundary = Key.add lhs.first lhs.num_elements in
    let choose_producer rhs_produce_value lhs_produce_value key =
      if Key.compare key boundary >= 0 then
        rhs_produce_value (Key.sub key boundary |> Key.add rhs.first)
      else lhs_produce_value key
    in
    let num_elements = Key.add lhs.num_elements rhs.num_elements in
    let rhs_offset = Key.sub boundary rhs.first in
    let values =
      Map.merge_into
        ~choose_producer
        ~map_key:(Key.add rhs_offset)
        rhs.values
        lhs.values
    in
    {lhs with num_elements; values}

  let to_list map =
    let open Effect in
    let rec unroll acc index =
      if Key.unsigned_compare index Key.zero > 0 then
        let* prefix = get index map in
        (unroll [@ocaml.tailcall]) (prefix :: acc) (Key.pred index)
      else
        let* prefix = get Key.zero map in
        return (prefix :: acc)
    in
    (* The empty vector is not correctly taken into account otherwise, since
       `pred zero` = `-1`, which is an invalid key according to
       {!invalid_key}. *)
    if map.num_elements = Key.zero then return []
    else (unroll [@ocaml.tailcall]) [] (Key.pred map.num_elements)

  let loaded_bindings m = Map.loaded_bindings m.values
end

module Int = struct
  include Int

  let unsigned_compare n m = compare (n - min_int) (m - min_int)
end

module IntVector = Make (Effect.Identity) (Int)
module Int32Vector = Make (Effect.Identity) (Int32)
module Int64Vector = Make (Effect.Identity) (Int64)
module LwtIntVector = Make (Effect.Lwt) (Int)
module LwtInt32Vector = Make (Effect.Lwt) (Int32)
module LwtInt64Vector = Make (Effect.Lwt) (Int64)
module LwtZVector = Make (Effect.Lwt) (ZZ)

module Mutable = struct
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

    val cons : 'a -> 'a t -> unit

    val snapshot : 'a t -> 'a Vector.t
  end

  module Make (Effect : Effect.S) (Key : KeyS) :
    S with type key = Key.t and type 'a effect = 'a Effect.t = struct
    module Vector = Make (Effect) (Key)

    type key = Vector.key

    type 'a effect = 'a Vector.effect

    type 'a t = 'a Vector.t ref

    let num_elements map_ref = Vector.num_elements !map_ref

    let of_immutable = ref

    let create ?values ?produce_value num_elements =
      of_immutable (Vector.create ?values ?produce_value num_elements)

    let get key map_ref = Vector.get key !map_ref

    let set key value map_ref = map_ref := Vector.set key value !map_ref

    let grow ?produce_value delta map_ref =
      map_ref := Vector.grow ?produce_value delta !map_ref

    let cons a map_ref = map_ref := Vector.cons a !map_ref

    let snapshot map_ref = !map_ref
  end

  module IntVector = Make (Effect.Identity) (Int)
  module Int32Vector = Make (Effect.Identity) (Int32)
  module Int64Vector = Make (Effect.Identity) (Int64)
  module LwtIntVector = Make (Effect.Lwt) (Int)
  module LwtInt32Vector = Make (Effect.Lwt) (Int32)
  module LwtInt64Vector = Make (Effect.Lwt) (Int64)
  module LwtZVector = Make (Effect.Lwt) (ZZ)
end
