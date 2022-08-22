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

exception Bounds

exception SizeOverflow

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

  type 'a producer = key -> 'a Lwt.t

  module Map : Lazy_map.S with type key = key

  type 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val to_string : ('a -> string) -> 'a t -> string

  val string_of_key : key -> string

  val num_elements : 'a t -> key

  val create :
    ?first_key:key ->
    ?values:'a Map.Map.t ->
    ?produce_value:'a producer ->
    ?origin:Lazy_map.tree ->
    key ->
    'a t

  val origin : 'a t -> Lazy_map.tree option

  val empty : unit -> 'a t

  val singleton : 'a -> 'a t

  val of_list : 'a list -> 'a t

  val get : key -> 'a t -> 'a Lwt.t

  val set : key -> 'a -> 'a t -> 'a t

  val cons : 'a -> 'a t -> 'a t

  val grow : ?default:(unit -> 'a) -> key -> 'a t -> 'a t

  val append : 'a -> 'a t -> 'a t * key

  val concat : 'a t -> 'a t -> 'a t Lwt.t

  val unsafe_concat : 'a t -> 'a t -> 'a t

  val to_list : 'a t -> 'a list Lwt.t

  val loaded_bindings : 'a t -> (key * 'a) list

  val first_key : 'a t -> key
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

module Make (Key : KeyS) : S with type key = Key.t = struct
  module Map = Lazy_map.Make (Key)

  type key = Key.t

  type 'a producer = key -> 'a Lwt.t

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

  let string_of_key = Key.to_string

  let num_elements map = map.num_elements

  let create ?(first_key = Key.zero) ?values ?produce_value ?origin num_elements
      =
    let values = Map.create ?values ?produce_value ?origin () in
    {first = first_key; num_elements; values}

  let origin {values; _} = Map.origin values

  let empty () = create Key.zero

  let of_list values =
    let fold (map, len) value = (Map.Map.add len value map, Key.succ len) in
    let values, num_elements =
      List.fold_left fold (Map.Map.empty, Key.zero) values
    in
    create ~values num_elements

  let invalid_key key map = Key.unsigned_compare key map.num_elements >= 0

  let get key map =
    if invalid_key key map then raise Bounds ;
    let key = Key.add map.first key in
    Map.get key map.values

  let set key value map =
    if invalid_key key map then raise Bounds ;
    let key = Key.add map.first key in
    {map with values = Map.set key value map.values}

  let singleton value = create Key.(succ zero) |> set Key.zero value

  let cons value map =
    let first = Key.pred map.first in
    let values = Map.set first value map.values in
    let num_elements = Key.succ map.num_elements in
    {first; values; num_elements}

  let append_opt elt map =
    let num_elements = map.num_elements in
    let map = {map with num_elements = Key.succ num_elements} in
    let map =
      match elt with Some elt -> set num_elements elt map | None -> map
    in
    (map, num_elements)

  let append elt map = append_opt (Some elt) map

  let rec grow ?default delta map =
    if Key.(delta <= zero) then map
    else
      let map, _ = append_opt (Option.map (fun f -> f ()) default) map in
      grow ?default Key.(pred delta) map

  let to_list map =
    let open Lwt.Syntax in
    let rec unroll acc index =
      if Key.unsigned_compare index Key.zero > 0 then
        let* prefix = get index map in
        (unroll [@ocaml.tailcall]) (prefix :: acc) (Key.pred index)
      else
        let* prefix = get Key.zero map in
        Lwt.return (prefix :: acc)
    in
    (* The empty vector is not correctly taken into account otherwise, since
       `pred zero` = `-1`, which is an invalid key according to
       {!invalid_key}. *)
    if map.num_elements = Key.zero then Lwt.return []
    else (unroll [@ocaml.tailcall]) [] (Key.pred map.num_elements)

  let concat lhs rhs =
    let open Lwt.Syntax in
    let* lhs = to_list lhs in
    let+ rhs = to_list rhs in
    of_list (lhs @ rhs)

  let loaded_bindings m = Map.loaded_bindings m.values

  let unsafe_concat lhs rhs =
    let lhs = loaded_bindings lhs |> List.map snd in
    let rhs = loaded_bindings rhs |> List.map snd in
    of_list (lhs @ rhs)

  let first_key vector = vector.first
end

module Int = struct
  include Int

  let unsigned_compare n m = compare (n - min_int) (m - min_int)
end

module LwtIntVector = Make (Int)
module LwtInt32Vector = Make (Int32)
module LwtInt64Vector = Make (Int64)
module LwtZVector = Make (ZZ)

module Mutable = struct
  module type ImmutableS = S

  module type S = sig
    type key

    module Vector : S with type key = key

    type 'a t

    val num_elements : 'a t -> key

    val of_immutable : 'a Vector.t -> 'a t

    val create :
      ?values:'a Vector.Map.Map.t ->
      ?produce_value:'a Vector.producer ->
      ?origin:Lazy_map.tree ->
      key ->
      'a t

    val origin : 'a t -> Lazy_map.tree option

    val get : key -> 'a t -> 'a Lwt.t

    val set : key -> 'a -> 'a t -> unit

    val grow : ?default:(unit -> 'a) -> key -> 'a t -> unit

    val append : 'a -> 'a t -> key

    val cons : 'a -> 'a t -> unit

    val snapshot : 'a t -> 'a Vector.t
  end

  module Make (Vector : ImmutableS) :
    S with type key = Vector.key and module Vector = Vector = struct
    module Vector = Vector

    type key = Vector.key

    type 'a t = 'a Vector.t ref

    let num_elements map_ref = Vector.num_elements !map_ref

    let of_immutable = ref

    let create ?values ?produce_value ?origin num_elements =
      of_immutable (Vector.create ?values ?produce_value ?origin num_elements)

    let origin vector = Vector.origin !vector

    let get key map_ref = Vector.get key !map_ref

    let set key value map_ref = map_ref := Vector.set key value !map_ref

    let grow ?default delta map_ref =
      map_ref := Vector.grow ?default delta !map_ref

    let append elt map_ref =
      let new_map, i = Vector.append elt !map_ref in
      map_ref := new_map ;
      i

    let cons a map_ref = map_ref := Vector.cons a !map_ref

    let snapshot map_ref = !map_ref
  end

  module LwtIntVector = Make (LwtIntVector)
  module LwtInt32Vector = Make (LwtInt32Vector)
  module LwtInt64Vector = Make (LwtInt64Vector)
  module LwtZVector = Make (LwtZVector)
end
