module Effect = struct
  module type S = sig
    type 'a t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val return : 'a -> 'a t
  end

  module Identity : S with type 'a t = 'a = struct
    type 'a t = 'a

    let ( let+ ) x f = f x

    let return = Fun.id
  end

  module Lwt : S with type 'a t = 'a Lwt.t = struct
    type 'a t = 'a Lwt.t

    let ( let+ ) = Lwt.Syntax.(let+)

    let return = Lwt.return
  end
end

module type KeyS = sig
  include Map.OrderedType

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

  module Map : Map.S with type key = key

  type 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val to_string : ('a -> string) -> 'a t -> string

  val num_elements : 'a t -> key

  val create : ?values:'a Map.t -> ?produce_value:'a producer -> key -> 'a t

  val of_list : 'a list -> 'a t

  val get : key -> 'a t -> 'a effect

  val set : key -> 'a -> 'a t -> 'a t

  val cons : 'a -> 'a t -> 'a t

  val grow : ?produce_value:'a producer -> key -> 'a t -> 'a t

  val concat : 'a t -> 'a t -> 'a t
end

exception UnexpectedAccess

module Make (Effect : Effect.S) (Key : KeyS) : S with
  type key = Key.t and
  type 'a effect = 'a Effect.t =
struct
  module Map = Map.Make (Key)

  type key = Key.t

  type 'a effect = 'a Effect.t

  type 'a producer = key -> 'a effect

  type 'a t = {
    first : key;
    num_elements : key;
    produce_value : 'a producer;
    mutable values : 'a Map.t
  }

  let pp pp_value =
    let pp_values fmt (first, values) =
      Map.bindings values
      |> Format.fprintf fmt "@[<hv>%a@]"
           (Format.pp_print_list
              ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
              (fun ppf (k, v) ->
                 Format.fprintf ppf "%s => %a" Key.(to_string (sub k first)) pp_value v))
    in
    fun fmt map ->
      Format.fprintf
        fmt
        "@[<hv 2>{ first = %s;@ num_elements = %s;@ values = @[<hv 2>[ %a ]@] }@]"
        (Key.to_string map.first)
        (Key.to_string map.num_elements)
        pp_values
        (map.first, map.values)

  let to_string show_value map =
    let pp_value fmt value = Format.pp_print_string fmt (show_value value) in
    Format.asprintf "%a" (pp pp_value) map

  let num_elements map = map.num_elements

  let def_produce_value _ = raise UnexpectedAccess

  let create ?(values = Map.empty) ?(produce_value = def_produce_value) num_elements =
    { first = Key.zero; num_elements; produce_value; values }

  let of_list values =
    let fold (map, len) value =
      Map.add len value map, Key.succ len
    in
    let values, num_elements =
      List.fold_left fold (Map.empty, Key.zero) values
    in
    create ~values num_elements

  let invalid_key key map =
    Key.compare key map.num_elements >= 0 || Key.compare key Key.zero < 0

  let get key map =
    let open Effect in
    if invalid_key key map then raise Memory_exn.Bounds;
    let key = Key.add map.first key in
    match Map.find_opt key map.values with
    | None ->
      (* Need to create the missing key-value association. *)
      let+ value = map.produce_value key in
      map.values <- Map.add key value map.values;
      value
    | Some value ->
      return value

  let set key value map =
    if invalid_key key map then raise Memory_exn.Bounds;
    let key = Key.add map.first key in
    { map with values = Map.add key value map.values }

  let cons value map =
    let first = Key.pred map.first in
    let values = Map.add first value map.values in
    let num_elements = Key.succ map.num_elements in
    { map with first; values; num_elements }

  let grow ?produce_value delta map =
    (* Make sure we only grow and never shrink. *)
    if Key.compare delta Key.zero > 0 then
      let produce_value =
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
            else
              map.produce_value key
        | None -> map.produce_value
      in
      let num_elements = Key.add map.num_elements delta in
      { map with produce_value; num_elements }
    else
      map

  let concat lhs rhs =
    let boundary = Key.add lhs.first lhs.num_elements in
    let produce_value key =
      if Key.compare key boundary >= 0 then
        rhs.produce_value (Key.sub key boundary |> Key.add rhs.first)
      else
        lhs.produce_value key
    in
    let num_elements = Key.add lhs.num_elements rhs.num_elements in
    let rhs_offset = Key.sub boundary rhs.first in
    let values =
      Map.fold
        (fun rhs_key -> Map.add (Key.add rhs_key rhs_offset))
        rhs.values (* fold subject *)
        lhs.values (* accumulator *)
    in
    { lhs with num_elements; produce_value; values }
end

module IntMap = Make (Effect.Identity) (Int)

module Int32Map = Make (Effect.Identity) (Int32)

module Int64Map = Make (Effect.Identity) (Int64)

module LwtIntMap = Make (Effect.Lwt) (Int)

module LwtInt32Map = Make (Effect.Lwt) (Int32)

module LwtInt64Map = Make (Effect.Lwt) (Int64)

module Mutable = struct
  module type S = sig
    type key

    type 'a effect

    module Map : S with type key = key and type 'a effect = 'a effect

    type 'a t

    val num_elements : 'a t -> key

    val of_immutable : 'a Map.t -> 'a t

    val create : ?values:'a Map.Map.t -> ?produce_value:'a Map.producer -> key -> 'a t

    val get : key -> 'a t -> 'a Map.effect

    val set : key -> 'a -> 'a t -> unit

    val grow : ?produce_value:'a Map.producer -> key -> 'a t -> unit

    val snapshot : 'a t -> 'a Map.t
  end

  module Make (Effect : Effect.S) (Key : KeyS) : S with
    type key = Key.t and
    type 'a effect = 'a Effect.t =
  struct
    module Map = Make (Effect) (Key)

    type key = Map.key

    type 'a effect = 'a Map.effect

    type 'a t = 'a Map.t ref

    let num_elements map_ref = Map.num_elements !map_ref

    let of_immutable = ref

    let create ?values ?produce_value num_elements =
      of_immutable (Map.create ?values ?produce_value num_elements)

    let get key map_ref = Map.get key !map_ref

    let set key value map_ref =
      map_ref := Map.set key value !map_ref

    let grow ?produce_value delta map_ref =
      map_ref := Map.grow ?produce_value delta !map_ref

    let snapshot map_ref = !map_ref
  end

  module IntMap = Make (Effect.Identity) (Int)

  module Int32Map = Make (Effect.Identity) (Int32)

  module Int64Map = Make (Effect.Identity) (Int64)

  module LwtIntMap = Make (Effect.Lwt) (Int)

  module LwtInt32Map = Make (Effect.Lwt) (Int32)

  module LwtInt64Map = Make (Effect.Lwt) (Int64)
end
