(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Tezos_webassembly_interpreter

exception Uninitialized_self_ref

type key = string list

module type S = sig
  type tree

  type 'a map

  type vector_key

  type 'a vector

  type chunked_byte_vector

  type ('tag, 'a) case

  module Decoding : Tree_decoding.S with type tree = tree

  module Encoding : Tree_encoding.S with type tree = tree

  type 'a t

  val encode : 'a t -> 'a -> tree -> tree Lwt.t

  val decode : 'a t -> tree -> 'a Lwt.t

  val custom : 'a Encoding.t -> 'a Decoding.t -> 'a t

  val conv : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

  val conv_lwt : ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'a t -> 'b t

  val tup2 : flatten:bool -> 'a t -> 'b t -> ('a * 'b) t

  val tup3 : flatten:bool -> 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  val tup4 :
    flatten:bool -> 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  val tup5 :
    flatten:bool ->
    'a t ->
    'b t ->
    'c t ->
    'd t ->
    'e t ->
    ('a * 'b * 'c * 'd * 'e) t

  val tup6 :
    flatten:bool ->
    'a t ->
    'b t ->
    'c t ->
    'd t ->
    'e t ->
    'f t ->
    ('a * 'b * 'c * 'd * 'e * 'f) t

  val tup7 :
    flatten:bool ->
    'a t ->
    'b t ->
    'c t ->
    'd t ->
    'e t ->
    'f t ->
    'g t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g) t

  val tup8 :
    flatten:bool ->
    'a t ->
    'b t ->
    'c t ->
    'd t ->
    'e t ->
    'f t ->
    'g t ->
    'h t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t

  val raw : key -> bytes t

  val value : key -> 'a Data_encoding.t -> 'a t

  val value_option : key -> 'a Data_encoding.t -> 'a option t

  val scope : key -> 'a t -> 'a t

  val lazy_mapping : 'a t -> 'a map t

  val lazy_vector : vector_key t -> 'a t -> 'a vector t

  val chunk : Chunked_byte_vector.Chunk.t t

  val chunked_byte_vector : chunked_byte_vector t

  val case : 'tag -> 'b t -> ('a -> 'b option) -> ('b -> 'a) -> ('tag, 'a) case

  val case_lwt :
    'tag ->
    'b t ->
    ('a -> 'b Lwt.t option) ->
    ('b -> 'a Lwt.t) ->
    ('tag, 'a) case

  val tagged_union : 'tag t -> ('tag, 'a) case list -> 'a t

  val option : 'a t -> 'a option t

  val with_self_reference : ('a Lazy.t -> 'a t) -> 'a t
end

module Make
    (M : Lazy_map.S with type 'a effect = 'a Lwt.t)
    (V : Lazy_vector.S with type 'a effect = 'a Lwt.t)
    (C : Chunked_byte_vector.S with type 'a effect = 'a Lwt.t)
    (T : Tree.S) :
  S
    with type tree = T.tree
     and type 'a map = 'a M.t
     and type vector_key = V.key
     and type 'a vector = 'a V.t
     and type chunked_byte_vector = C.t = struct
  module Encoding = Tree_encoding.Make (T)
  module Decoding = Tree_decoding.Make (T)
  module E = Encoding
  module D = Decoding

  type tree = T.tree

  type vector_key = V.key

  type 'a vector = 'a V.t

  type 'a map = 'a M.t

  type chunked_byte_vector = C.t

  type 'a encoding = 'a E.t

  type 'a decoding = 'a D.t

  type 'a t = {encode : 'a encoding; decode : 'a decoding}

  let custom encode decode = {encode; decode}

  let conv d e {encode; decode} =
    {encode = E.contramap e encode; decode = D.map d decode}

  let conv_lwt d e {encode; decode} =
    {encode = E.contramap_lwt e encode; decode = D.map_lwt d decode}

  let scope key {encode; decode} =
    {encode = E.scope key encode; decode = D.scope key decode}

  let tup2_ a b =
    {
      encode = E.tup2 a.encode b.encode;
      decode = D.Syntax.both a.decode b.decode;
    }

  let tup3_ a b c =
    conv
      (fun (a, (b, c)) -> (a, b, c))
      (fun (a, b, c) -> (a, (b, c)))
      (tup2_ a (tup2_ b c))

  let tup4_ a b c d =
    conv
      (fun (a, (b, c, d)) -> (a, b, c, d))
      (fun (a, b, c, d) -> (a, (b, c, d)))
      (tup2_ a (tup3_ b c d))

  let tup5_ a b c d e =
    conv
      (fun (a, (b, c, d, e)) -> (a, b, c, d, e))
      (fun (a, b, c, d, e) -> (a, (b, c, d, e)))
      (tup2_ a (tup4_ b c d e))

  let tup6_ a b c d e f =
    conv
      (fun (a, (b, c, d, e, f)) -> (a, b, c, d, e, f))
      (fun (a, b, c, d, e, f) -> (a, (b, c, d, e, f)))
      (tup2_ a (tup5_ b c d e f))

  let tup7_ a b c d e f g =
    conv
      (fun (a, (b, c, d, e, f, g)) -> (a, b, c, d, e, f, g))
      (fun (a, b, c, d, e, f, g) -> (a, (b, c, d, e, f, g)))
      (tup2_ a (tup6_ b c d e f g))

  let tup8_ a b c d e f g h =
    conv
      (fun (a, (b, c, d, e, f, g, h)) -> (a, b, c, d, e, f, g, h))
      (fun (a, b, c, d, e, f, g, h) -> (a, (b, c, d, e, f, g, h)))
      (tup2_ a (tup7_ b c d e f g h))

  (* This is to allow for either flat composition of tuples or  where each
     element of the tuple is wrapped under an index node. *)
  let flat_or_wrap ~flatten ix enc =
    if flatten then enc else scope [string_of_int ix] enc

  let tup2 ~flatten a b =
    tup2_ (flat_or_wrap ~flatten 1 a) (flat_or_wrap ~flatten 2 b)

  let tup3 ~flatten a b c =
    tup3_
      (flat_or_wrap ~flatten 1 a)
      (flat_or_wrap ~flatten 2 b)
      (flat_or_wrap ~flatten 3 c)

  let tup4 ~flatten a b c d =
    tup4_
      (flat_or_wrap ~flatten 1 a)
      (flat_or_wrap ~flatten 2 b)
      (flat_or_wrap ~flatten 3 c)
      (flat_or_wrap ~flatten 4 d)

  let tup5 ~flatten a b c d e =
    tup5_
      (flat_or_wrap ~flatten 1 a)
      (flat_or_wrap ~flatten 2 b)
      (flat_or_wrap ~flatten 3 c)
      (flat_or_wrap ~flatten 4 d)
      (flat_or_wrap ~flatten 5 e)

  let tup6 ~flatten a b c d e f =
    tup6_
      (flat_or_wrap ~flatten 1 a)
      (flat_or_wrap ~flatten 2 b)
      (flat_or_wrap ~flatten 3 c)
      (flat_or_wrap ~flatten 4 d)
      (flat_or_wrap ~flatten 5 e)
      (flat_or_wrap ~flatten 6 f)

  let tup7 ~flatten a b c d e f g =
    tup7_
      (flat_or_wrap ~flatten 1 a)
      (flat_or_wrap ~flatten 2 b)
      (flat_or_wrap ~flatten 3 c)
      (flat_or_wrap ~flatten 4 d)
      (flat_or_wrap ~flatten 5 e)
      (flat_or_wrap ~flatten 6 f)
      (flat_or_wrap ~flatten 7 g)

  let tup8 ~flatten a b c d e f g h =
    tup8_
      (flat_or_wrap ~flatten 1 a)
      (flat_or_wrap ~flatten 2 b)
      (flat_or_wrap ~flatten 3 c)
      (flat_or_wrap ~flatten 4 d)
      (flat_or_wrap ~flatten 5 e)
      (flat_or_wrap ~flatten 6 f)
      (flat_or_wrap ~flatten 7 g)
      (flat_or_wrap ~flatten 8 h)

  let encode {encode; _} value tree = E.run encode value tree

  let decode {decode; _} tree = D.run decode tree

  let raw key = {encode = E.raw key; decode = D.raw key}

  let value key de = {encode = E.value key de; decode = D.value key de}

  let value_option key de = value key (Data_encoding.option de)

  let lazy_mapping value =
    let to_key k = [M.string_of_key k] in
    let encode =
      E.contramap M.loaded_bindings (E.lazy_mapping to_key value.encode)
    in
    let decode =
      D.map
        (fun produce_value -> M.create ~produce_value ())
        (D.lazy_mapping to_key value.decode)
    in
    {encode; decode}

  let lazy_vector with_key value =
    let to_key k = [V.string_of_key k] in
    let encode =
      E.contramap
        (fun vector ->
          (V.loaded_bindings vector, V.num_elements vector, V.first_key vector))
        (E.tup3
           (E.lazy_mapping to_key value.encode)
           (E.scope ["length"] with_key.encode)
           (E.scope ["head"] with_key.encode))
    in
    let decode =
      D.map
        (fun (produce_value, len, head) ->
          V.create ~produce_value ~first_key:head len)
        (let open D.Syntax in
        let+ x = D.lazy_mapping to_key value.decode
        and+ y = D.scope ["length"] with_key.decode
        and+ z = D.scope ["head"] with_key.decode in
        (x, y, z))
    in
    {encode; decode}

  let chunk =
    let open Chunked_byte_vector.Chunk in
    conv of_bytes to_bytes (raw [])

  let chunked_byte_vector =
    let to_key k = [Int64.to_string k] in
    let encode =
      E.contramap
        (fun vector -> (C.loaded_chunks vector, C.length vector))
        (E.tup2
           (E.lazy_mapping to_key chunk.encode)
           (E.value ["length"] Data_encoding.int64))
    in
    let decode =
      D.map
        (fun (get_chunk, len) -> C.create ~get_chunk len)
        (let open D.Syntax in
        let+ x = D.lazy_mapping to_key chunk.decode
        and+ y = D.value ["length"] Data_encoding.int64 in
        (x, y))
    in
    {encode; decode}

  type ('tag, 'a) case =
    | Case : {
        tag : 'tag;
        probe : 'a -> 'b Lwt.t option;
        extract : 'b -> 'a Lwt.t;
        delegate : 'b t;
      }
        -> ('tag, 'a) case

  let case_lwt tag delegate probe extract = Case {tag; delegate; probe; extract}

  let case tag delegate probe extract =
    case_lwt
      tag
      delegate
      (fun x -> Option.map Lwt.return @@ probe x)
      (fun x -> Lwt.return @@ extract x)

  let tagged_union {encode; decode} cases =
    let to_encode_case (Case {tag; delegate; probe; extract = _}) =
      E.case_lwt tag delegate.encode probe
    in
    let to_decode_case (Case {tag; delegate; extract; probe = _}) =
      D.case_lwt tag delegate.decode extract
    in
    let encode = E.tagged_union encode (List.map to_encode_case cases) in
    let decode = D.tagged_union decode (List.map to_decode_case cases) in
    {encode; decode}

  let option enc =
    tagged_union
      (value [] Data_encoding.string)
      [
        case "Some" enc Fun.id Option.some;
        case
          "None"
          (value [] Data_encoding.unit)
          (function None -> Some () | _ -> None)
          (fun () -> None);
      ]

  let with_self_reference f =
    (* Mutable reference to the current value. *)
    let current = ref None in
    (* Sets the current value. *)
    let set_current value =
      current := Some value ;
      value
    in
    (* Gets the current value from the ref. This should only be called once
       the  encoding/decoding steps have already constructed a value and the ref
       has been updated. *)
    let get_current () =
      match !current with
      | Some value -> value
      | None -> raise Uninitialized_self_ref
    in
    (* Intercepts the encoding and decoding steps to update the reference to the
       current module. *)
    conv set_current set_current (f (lazy (get_current ())))
end
