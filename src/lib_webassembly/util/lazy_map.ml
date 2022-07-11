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

    let ( let+ ) = Lwt.Syntax.( let+ )

    let return = Lwt.return
  end
end

module type KeyS = sig
  include Map.OrderedType

  val to_string : t -> string
end

module type S = sig
  type key

  type 'a effect

  type 'a producer = key -> 'a effect

  module Map : Map.S with type key = key

  type 'a t

  val string_of_key : key -> string

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val to_string : ('a -> string) -> 'a t -> string

  val create : ?values:'a Map.t -> ?produce_value:'a producer -> unit -> 'a t

  val get : key -> 'a t -> 'a effect

  val set : key -> 'a -> 'a t -> 'a t

  val merge_into :
    ?map_key:(key -> key) ->
    ?choose_producer:('a producer -> 'a producer -> 'a producer) ->
    'a t ->
    'a t ->
    'a t

  val with_producer : ('a producer -> 'a producer) -> 'a t -> 'a t

  val loaded_bindings : 'a t -> (key * 'a) list
end

exception UnexpectedAccess

module Make (Effect : Effect.S) (Key : KeyS) :
  S with type key = Key.t and type 'a effect = 'a Effect.t = struct
  module Map = Map.Make (Key)

  type key = Key.t

  type 'a effect = 'a Effect.t

  type 'a producer = key -> 'a effect

  type 'a t = {produce_value : 'a producer; mutable values : 'a Map.t}

  let string_of_key = Key.to_string

  let pp pp_value =
    let pp_values fmt values =
      Map.bindings values
      |> Format.fprintf
           fmt
           "@[<hv>%a@]"
           (Format.pp_print_list
              ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
              (fun ppf (k, v) ->
                Format.fprintf ppf "%s => %a" (Key.to_string k) pp_value v))
    in
    fun fmt map ->
      Format.fprintf
        fmt
        "@[<hv 2>{ values = @[<hv 2>[ %a ]@] }@]"
        pp_values
        map.values

  let to_string show_value map =
    let pp_value fmt value = Format.pp_print_string fmt (show_value value) in
    Format.asprintf "%a" (pp pp_value) map

  let def_produce_value _ = raise UnexpectedAccess

  let create ?(values = Map.empty) ?(produce_value = def_produce_value) () =
    {produce_value; values}

  let get key map =
    let open Effect in
    match Map.find_opt key map.values with
    | None ->
        (* Need to create the missing key-value association. *)
        let+ value = map.produce_value key in
        map.values <- Map.add key value map.values ;
        value
    | Some value -> return value

  let set key value map = {map with values = Map.add key value map.values}

  let merge_into ?(map_key = Fun.id) ?(choose_producer = fun _ dest -> dest) src
      dest =
    let produce_value = choose_producer src.produce_value dest.produce_value in
    let values =
      Map.fold
        (fun src_key -> Map.add (map_key src_key))
        src.values (* fold subject *)
        dest.values (* accumulator *)
    in
    {produce_value; values}

  let with_producer morph map =
    {map with produce_value = morph map.produce_value}

  let loaded_bindings m = Map.bindings m.values
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

    val of_immutable : 'a Map.t -> 'a t

    val create :
      ?values:'a Map.Map.t -> ?produce_value:'a Map.producer -> unit -> 'a t

    val get : key -> 'a t -> 'a Map.effect

    val set : key -> 'a -> 'a t -> unit

    val snapshot : 'a t -> 'a Map.t
  end

  module Make (Effect : Effect.S) (Key : KeyS) :
    S with type key = Key.t and type 'a effect = 'a Effect.t = struct
    module Map = Make (Effect) (Key)

    type key = Map.key

    type 'a effect = 'a Map.effect

    type 'a t = 'a Map.t ref

    let of_immutable = ref

    let create ?values ?produce_value unit =
      of_immutable (Map.create ?values ?produce_value unit)

    let get key map_ref = Map.get key !map_ref

    let set key value map_ref = map_ref := Map.set key value !map_ref

    let snapshot map_ref = !map_ref
  end

  module IntMap = Make (Effect.Identity) (Int)
  module Int32Map = Make (Effect.Identity) (Int32)
  module Int64Map = Make (Effect.Identity) (Int64)
  module LwtIntMap = Make (Effect.Lwt) (Int)
  module LwtInt32Map = Make (Effect.Lwt) (Int32)
  module LwtInt64Map = Make (Effect.Lwt) (Int64)
end
