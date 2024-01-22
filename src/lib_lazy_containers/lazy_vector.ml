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
    ?origin:Tezos_tree_encoding.wrapped_tree ->
    key ->
    'a t

  val origin : 'a t -> Tezos_tree_encoding.wrapped_tree option

  val empty : unit -> 'a t

  val singleton : 'a -> 'a t

  val of_list : 'a list -> 'a t

  val get : key -> 'a t -> 'a Lwt.t

  val set : key -> 'a -> 'a t -> 'a t

  val cons : 'a -> 'a t -> 'a t

  val split : 'a t -> key -> 'a t * 'a t

  val grow : ?default:(unit -> 'a) -> key -> 'a t -> 'a t

  val drop : 'a t -> 'a t

  val pop : 'a t -> ('a * 'a t) Lwt.t

  val prepend_list : 'a list -> 'a t -> 'a t

  val append : 'a -> 'a t -> 'a t * key

  val concat : 'a t -> 'a t -> 'a t Lwt.t

  val unsafe_concat : 'a t -> 'a t -> 'a t

  val to_list : 'a t -> 'a list Lwt.t

  val loaded_bindings : 'a t -> (key * 'a option) list

  val first_key : 'a t -> key

  val encoding :
    key Tezos_tree_encoding.t ->
    'a Tezos_tree_encoding.t ->
    'a t Tezos_tree_encoding.t
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

module Make_no_enc (Key : KeyS) = struct
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

  let overflow k1 k2 = Key.unsigned_compare k1 (Key.add k1 k2) > 0

  let cons value map =
    if overflow map.num_elements (Key.succ Key.zero) then raise SizeOverflow
    else
      let first = Key.pred map.first in
      let values = Map.set first value map.values in
      let num_elements = Key.succ map.num_elements in
      {first; values; num_elements}

  let split vec at =
    if
      Key.(
        unsigned_compare at zero < 0
        || unsigned_compare (num_elements vec) at < 0)
    then raise Bounds
    else
      ( {first = vec.first; num_elements = at; values = Map.dup vec.values},
        {
          first = Key.(add vec.first at);
          num_elements = Key.(sub vec.num_elements at);
          values = Map.dup vec.values;
        } )

  let append_opt elt map =
    if overflow map.num_elements (Key.succ Key.zero) then raise SizeOverflow
    else
      let num_elements = map.num_elements in
      let map = {map with num_elements = Key.succ num_elements} in
      let map =
        match elt with Some elt -> set num_elements elt map | None -> map
      in
      (map, num_elements)

  (* This version of drop simply doesn't check for bounds, but is used in
     functions actually checking the bounds, to prevent doing it twice. *)
  let unsafe_drop map =
    let values = Map.remove map.first map.values in
    {
      first = Key.succ map.first;
      num_elements = Key.pred map.num_elements;
      values;
    }

  let drop map =
    if Key.(unsigned_compare zero map.num_elements < 0) then unsafe_drop map
    else raise Bounds

  let pop map =
    let open Lwt.Syntax in
    if Key.(unsigned_compare zero map.num_elements < 0) then
      let+ x = get Key.zero map in
      (x, unsafe_drop map)
    else raise Bounds

  let append elt map = append_opt (Some elt) map

  let prepend_list es es0 =
    let es = List.rev es in
    let rec aux v = function x :: rst -> aux (cons x v) rst | [] -> v in
    aux es0 es

  let rec grow ?default delta map =
    if overflow map.num_elements delta then raise SizeOverflow
    else if Key.(delta <= zero) then map
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
    if map.num_elements = Key.zero then Lwt.return_nil
    else (unroll [@ocaml.tailcall]) [] (Key.pred map.num_elements)

  let concat lhs rhs =
    let open Lwt.Syntax in
    if overflow lhs.num_elements rhs.num_elements then raise SizeOverflow
    else
      let* lhs = to_list lhs in
      let+ rhs = to_list rhs in
      of_list (lhs @ rhs)

  let loaded_bindings m = Map.loaded_bindings m.values

  let unsafe_concat lhs rhs =
    let lhs = loaded_bindings lhs |> List.map snd in
    let rhs = loaded_bindings rhs |> List.map snd in
    of_list (List.filter_map Fun.id (lhs @ rhs))

  let first_key vector = vector.first
end

module Make (Key : KeyS) : S with type key = Key.t = struct
  module No_enc = Make_no_enc (Key)
  module Encoding = Tezos_tree_encoding.Lazy_vector_encoding.Make (No_enc)
  include No_enc

  let encoding = Encoding.lazy_vector
end

module Int = struct
  include Int

  let unsigned_compare n m = compare (n - min_int) (m - min_int)
end

module IntVector = Make (Int)
module Int32Vector = Make (Int32)
module Int64Vector = Make (Int64)
module ZVector = Make (ZZ)

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
      ?origin:Tezos_tree_encoding.wrapped_tree ->
      key ->
      'a t

    val origin : 'a t -> Tezos_tree_encoding.wrapped_tree option

    val get : key -> 'a t -> 'a Lwt.t

    val set : key -> 'a -> 'a t -> unit

    val grow : ?default:(unit -> 'a) -> key -> 'a t -> unit

    val append : 'a -> 'a t -> key

    val cons : 'a -> 'a t -> unit

    val drop : 'a t -> unit

    val pop : 'a t -> 'a Lwt.t

    val reset : 'a t -> unit

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

    let drop map_ref = map_ref := Vector.drop !map_ref

    let pop map_ref =
      let open Lwt.Syntax in
      let+ v, map = Vector.pop !map_ref in
      map_ref := map ;
      v

    let reset map_ref = map_ref := Vector.empty ()

    let snapshot map_ref = !map_ref
  end

  module IntVector = Make (IntVector)
  module Int32Vector = Make (Int32Vector)
  module Int64Vector = Make (Int64Vector)
  module ZVector = Make (ZVector)
end
