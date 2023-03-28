(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module type KeyS = sig
  include Map.OrderedType

  val to_string : t -> string
end

module type S = sig
  type key

  type 'a producer = key -> 'a Lwt.t

  module Map : Map.S with type key = key

  type 'a t

  val origin : 'a t -> Tezos_tree_encoding.wrapped_tree option

  val string_of_key : key -> string

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val to_string : ('a -> string) -> 'a t -> string

  val create :
    ?values:'a Map.t ->
    ?produce_value:'a producer ->
    ?origin:Tezos_tree_encoding.wrapped_tree ->
    unit ->
    'a t

  val get : key -> 'a t -> 'a Lwt.t

  val set : key -> 'a -> 'a t -> 'a t

  val remove : key -> 'a t -> 'a t

  val dup : 'a t -> 'a t

  val loaded_bindings : 'a t -> (key * 'a option) list
end

exception UnexpectedAccess

module Make (Key : KeyS) : S with type key = Key.t = struct
  module Map = Map.Make (Key)

  type key = Key.t

  type 'a producer = key -> 'a Lwt.t

  type 'a t = {
    origin : Tezos_tree_encoding.wrapped_tree option;
    produce_value : 'a producer;
    mutable values : 'a option Map.t;
  }

  let origin {origin; _} = origin

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
                Format.fprintf
                  ppf
                  "%s => %a"
                  (Key.to_string k)
                  (Format.pp_print_option pp_value)
                  v))
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

  let create ?(values = Map.empty) ?(produce_value = def_produce_value) ?origin
      () =
    let values = Map.map Option.some values in
    {produce_value; values; origin}

  let get key map =
    let open Lwt.Syntax in
    match Map.find_opt key map.values with
    | None ->
        (* Need to create the missing key-value association. *)
        let+ value = map.produce_value key in
        map.values <- Map.add key (Some value) map.values ;
        value
    | Some None -> (* The key was removed *) raise UnexpectedAccess
    | Some (Some value) -> Lwt.return value

  let set key value map =
    {map with values = Map.add key (Some value) map.values}

  let remove key map = {map with values = Map.add key None map.values}

  let dup {origin; produce_value; values} = {origin; produce_value; values}

  let loaded_bindings m = Map.bindings m.values
end

module LwtIntMap = Make (Int)
module LwtInt32Map = Make (Int32)
module LwtInt64Map = Make (Int64)

module Mutable = struct
  module type S = sig
    type key

    module Map : S with type key = key

    type 'a t

    val of_immutable : 'a Map.t -> 'a t

    val create :
      ?values:'a Map.Map.t ->
      ?produce_value:'a Map.producer ->
      ?origin:Tezos_tree_encoding.wrapped_tree ->
      unit ->
      'a t

    val get : key -> 'a t -> 'a Lwt.t

    val set : key -> 'a -> 'a t -> unit

    val remove : key -> 'a t -> unit

    val snapshot : 'a t -> 'a Map.t
  end

  module Make (Key : KeyS) : S with type key = Key.t = struct
    module Map = Make (Key)

    type key = Map.key

    type 'a t = 'a Map.t ref

    let of_immutable = ref

    let create ?values ?produce_value ?origin unit =
      of_immutable (Map.create ?values ?produce_value ?origin unit)

    let get key map_ref = Map.get key !map_ref

    let set key value map_ref = map_ref := Map.set key value !map_ref

    let remove key map_ref = map_ref := Map.remove key !map_ref

    let snapshot map_ref = !map_ref
  end

  module LwtIntMap = Make (Int)
  module LwtInt32Map = Make (Int32)
  module LwtInt64Map = Make (Int64)
end
