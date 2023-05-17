(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Lang_stdlib

module Encodings (L : LIB) = struct
  open L

  (**
    Encoding type for encapsulating encoding/decoding/input functions.
    This type enables us to use more structured types for data
    in circuits.
    For that, encoding is parameterized by 3 types:
    - 'oh is the type of the high-level OCaml representation
    - 'u is the unpacked type, i.e. a collection of atomic reprs.
    - `p is the packed type, i.e the inner type of Plompiler's repr.

    For example, for the representation of a point (pair of scalars),
    one might have:
    {[
    ( {x:int; y:int},
      scalar repr * scalar repr,
      scalar * scalar
    ) encoding
    ]}

    The first type, the record [{x:int; y:int}], represents an OCaml point,
    which becomes the argument taken by the [input] function.

    The second type, [scalar repr * scalar repr], is an unpacking of the
    encoding. This is used for the result of [decode]. We can use any type
    isomorphic to [scalar repr * scalar repr] here.

    The last type must be [scalar * scalar], as an encoding of a point will be
    of the type [(scalar * scalar) repr].
  *)

  type ('oh, 'u, 'p) encoding = {
    encode : 'u -> 'p repr;
    decode : 'p repr -> 'u;
    input : 'oh -> 'p Input.t;
    of_input : 'p Input.t -> 'oh;
  }

  (**
    The function [conv] defines conversions for [encoding]s, by changing
    the higher-level ['u] and and ['oh] types.
  *)
  let conv :
      ('u1 -> 'u0) ->
      ('u0 -> 'u1) ->
      ('o1 -> 'o0) ->
      ('o0 -> 'o1) ->
      ('o0, 'u0, 'p) encoding ->
      ('o1, 'u1, 'p) encoding =
   fun f g fi gi e ->
    let encode a = e.encode @@ f a in
    let decode b = g @@ e.decode b in
    let input x = e.input @@ fi x in
    let of_input x = gi @@ e.of_input x in
    {encode; decode; input; of_input}

  let with_implicit_bool_check :
      ('p repr -> bool repr t) -> ('o, 'u, 'p) encoding -> ('o, 'u, 'p) encoding
      =
   fun bc e ->
    {e with input = (fun x -> Input.with_implicit_bool_check bc @@ e.input x)}

  let with_assertion :
      ('p repr -> unit repr t) -> ('o, 'u, 'p) encoding -> ('o, 'u, 'p) encoding
      =
   fun assertion e ->
    {e with input = (fun x -> Input.with_assertion assertion @@ e.input x)}

  (** Encoding combinators *)
  let scalar_encoding =
    let encode x = x in
    let decode x = x in
    let input = Input.scalar in
    let of_input = Input.to_scalar in
    {encode; decode; input; of_input}

  let bool_encoding =
    let encode x = x in
    let decode x = x in
    let input = Input.bool in
    let of_input = Input.to_bool in
    {encode; decode; input; of_input}

  let list_encoding (e : _ encoding) =
    let encode a = to_list @@ List.map e.encode a in
    let decode x = List.map e.decode (of_list x) in
    let input a = Input.list @@ List.map e.input a in
    let of_input x = List.map e.of_input (Input.to_list x) in
    {encode; decode; input; of_input}

  (* Encoding for lists, where we keep the repr of the list itself, not a list
     of repr *)
  let atomic_list_encoding :
      ('a, 'b repr, 'c) encoding -> ('a list, 'b list repr, 'c list) encoding =
   fun e ->
    let encode a = to_list @@ List.map e.encode (of_list a) in
    let decode x = to_list @@ List.map e.decode (of_list x) in
    let input a = Input.list @@ List.map e.input a in
    let of_input x = List.map e.of_input (Input.to_list x) in
    {encode; decode; input; of_input}

  let obj2_encoding (el : _ encoding) (er : _ encoding) =
    let encode (a, b) = pair (el.encode a) (er.encode b) in
    let decode p =
      let a, b = of_pair p in
      (el.decode a, er.decode b)
    in
    let input (a, f) = Input.pair (el.input a) (er.input f) in
    let of_input p =
      let a, b = Input.to_pair p in
      (el.of_input a, er.of_input b)
    in
    {encode; decode; input; of_input}

  let atomic_obj2_encoding :
      ('a, 'b repr, 'c) encoding ->
      ('d, 'e repr, 'f) encoding ->
      ('a * 'd, ('b * 'e) repr, 'c * 'f) encoding =
   fun el er ->
    let encode p =
      let a, b = of_pair p in
      pair (el.encode a) (er.encode b)
    in
    let decode p =
      let a, b = of_pair p in
      pair (el.decode a) (er.decode b)
    in
    let input (a, f) = Input.pair (el.input a) (er.input f) in
    let of_input p =
      let a, b = Input.to_pair p in
      (el.of_input a, er.of_input b)
    in
    {encode; decode; input; of_input}

  let obj3_encoding e0 e1 e2 = obj2_encoding e0 (obj2_encoding e1 e2)

  let atomic_obj3_encoding e0 e1 e2 =
    atomic_obj2_encoding e0 (atomic_obj2_encoding e1 e2)

  let obj4_encoding e0 e1 e2 e3 = obj2_encoding e0 (obj3_encoding e1 e2 e3)

  let atomic_obj4_encoding e0 e1 e2 e3 =
    atomic_obj2_encoding e0 (atomic_obj3_encoding e1 e2 e3)

  let obj5_encoding e0 e1 e2 e3 e4 =
    obj2_encoding e0 (obj4_encoding e1 e2 e3 e4)

  let atomic_obj5_encoding e0 e1 e2 e3 e4 =
    atomic_obj2_encoding e0 (atomic_obj4_encoding e1 e2 e3 e4)

  let obj6_encoding e0 e1 e2 e3 e4 e5 =
    obj2_encoding e0 (obj5_encoding e1 e2 e3 e4 e5)

  let atomic_obj6_encoding e0 e1 e2 e3 e4 e5 =
    atomic_obj2_encoding e0 (atomic_obj5_encoding e1 e2 e3 e4 e5)

  let obj7_encoding e0 e1 e2 e3 e4 e5 e6 =
    obj2_encoding e0 (obj6_encoding e1 e2 e3 e4 e5 e6)

  let atomic_obj7_encoding e0 e1 e2 e3 e4 e5 e6 =
    atomic_obj2_encoding e0 (atomic_obj6_encoding e1 e2 e3 e4 e5 e6)

  let obj8_encoding e0 e1 e2 e3 e4 e5 e6 e7 =
    obj2_encoding e0 (obj7_encoding e1 e2 e3 e4 e5 e6 e7)

  let atomic_obj8_encoding e0 e1 e2 e3 e4 e5 e6 e7 =
    atomic_obj2_encoding e0 (atomic_obj7_encoding e1 e2 e3 e4 e5 e6 e7)

  let obj9_encoding e0 e1 e2 e3 e4 e5 e6 e7 e8 =
    obj2_encoding e0 (obj8_encoding e1 e2 e3 e4 e5 e6 e7 e8)

  let atomic_obj9_encoding e0 e1 e2 e3 e4 e5 e6 e7 e8 =
    atomic_obj2_encoding e0 (atomic_obj8_encoding e1 e2 e3 e4 e5 e6 e7 e8)

  let obj10_encoding e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 =
    obj2_encoding e0 (obj9_encoding e1 e2 e3 e4 e5 e6 e7 e8 e9)

  let atomic_obj10_encoding e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 =
    atomic_obj2_encoding e0 (atomic_obj9_encoding e1 e2 e3 e4 e5 e6 e7 e8 e9)
end
