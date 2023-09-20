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

open Plompiler
open Plonk_test
module CS = Plonk.Circuit
open Helpers

(**
 Example of the usage of encodings.
*)

module Encoding : Test =
functor
  (L : LIB)
  ->
  struct
    open L

    open Utils (L)

    (**
      [foo] is a circuit that takes a list of lines (pairs of 3D points)
      and simply increments by 1 the second coordinate of every point.
      This example shows how this simple task requires a great deal of packing
      and unpacking.
    *)

    let _foo :
        ((scalar * (scalar * scalar)) * (scalar * (scalar * scalar))) list repr ->
        ((scalar * (scalar * scalar)) * (scalar * (scalar * scalar))) list repr
        t =
     fun a ->
      let a = of_list a in
      foldM
        (fun acc l ->
          let p0, p1 = of_pair l in
          let p0_x, aux = of_pair p0 in
          let p0_y, p0_z = of_pair aux in
          let p1_x, aux = of_pair p1 in
          let p1_y, p1_z = of_pair aux in

          let* p0_y = Num.add_constant S.one p0_y in
          let* p1_y = Num.add_constant S.one p1_y in

          let aux = pair p0_y p0_z in
          let p0 = pair p0_x aux in
          let aux = pair p1_y p1_z in
          let p1 = pair p1_x aux in
          let l = pair p0 p1 in
          ret @@ to_list (l :: of_list acc))
        (to_list [])
        a

    (**
      Now we'll define the same circuit using Encodings.
      We'll start by defining encodings for points and lines.
    *)

    open Encodings

    (** [point] is type of the high-level OCaml representation *)
    type point = {x : int; y : int; z : int}

    (** [point_u] is an unpacked Plompiler representaion of [point].
        In general, for a record type [t], [t_u] will be a record type
        with the same field names as [t], but holding Plompiler reprs.
        It can be seen as an unpacking of the low-leve Plompiler
        representation, which in this case will be
        [(scalar * (scalar * scalar)) repr].
    *)
    type point_u = {x : scalar repr; y : scalar repr; z : scalar repr}

    let s_of_int x = S.of_z (Z.of_int x)

    let s_to_int s = Z.to_int (S.to_z s)

    (** [point_encoding] provides us three functions:
      - [encode : point_u -> (scalar * (scalar * scalar)) repr]
      - [decode : (scalar * (scalar * scalar)) repr -> point_u]
      - [input : point -> (scalar * (scalar * scalar)) Input.t ]

    The first two are used for packing/unpacking the date type in a circuit,
    while the third one is helpful for processing its inputs.
    *)
    let point_encoding : (point, point_u, _) encoding =
      conv
        (fun {x; y; z} -> (x, (y, z)))
        (fun (x, (y, z)) -> {x; y; z})
        (fun ({x; y; z} : point) -> (s_of_int x, (s_of_int y, s_of_int z)))
        (fun (x, (y, z)) -> {x = s_to_int x; y = s_to_int y; z = s_to_int z})
        (obj3_encoding scalar_encoding scalar_encoding scalar_encoding)

    type line = {p0l : point; p1l : point}

    type line_u = {p0 : point_u; p1 : point_u}

    let line_encoding : (line, line_u, _) encoding =
      conv
        (fun {p0; p1} -> (p0, p1))
        (fun (p0, p1) -> {p0; p1})
        (fun {p0l; p1l} -> (p0l, p1l))
        (fun (p0l, p1l) -> {p0l; p1l})
        (obj2_encoding point_encoding point_encoding)

    (* Now we provide an equivalent definition for [foo], using the encodings. *)
    let _foo' a =
      let a = of_list a in
      foldM
        (fun acc l ->
          let l = line_encoding.decode l in

          let* p0_y = Num.add_constant S.one l.p0.y in
          let* p1_y = Num.add_constant S.one l.p1.y in

          let l =
            line_encoding.encode
            @@ {p0 = {l.p0 with y = p0_y}; p1 = {l.p1 with y = p1_y}}
          in
          ret @@ to_list (l :: of_list acc))
        (to_list [])
        a

    let test_foo a expected () =
      let* a = input ~kind:`Public a in
      let* expected = input expected in
      let* o = _foo' a in
      assert_equal o expected

    let pz : point = {x = 0; y = 0; z = 0}

    let po : point = {x = 0; y = 1; z = 0}

    let lz = {p0l = pz; p1l = pz}

    let lo = {p0l = po; p1l = po}

    let inpt = Input.list @@ [line_encoding.input lz]

    let expected = Input.list @@ [line_encoding.input lo]

    let tests_foo =
      [test ~name:"Encoding.test_foo" ~valid:true @@ test_foo inpt expected]

    let first_is_zero p =
      let p = point_encoding.decode p in
      (* We want the intermediate variable here, to test the
         behaviour of input_flag *)
      let* z = Num.zero in
      assert_equal p.x z

    let test_assertion a b () =
      let* _a = input ~kind:`Public a in
      let* _b = input b in
      ret unit

    let enc' = with_assertion first_is_zero point_encoding

    let p1 : point = {x = 1; y = 1; z = 0}

    let tests_assertion =
      [
        test ~name:"Encoding.test_assertion" ~valid:true
        @@ test_assertion (enc'.input pz) (enc'.input po);
        test ~name:"Encoding.test_assertion" ~valid:false
        @@ test_assertion (enc'.input p1) (enc'.input po);
      ]

    let tests = tests_foo @ tests_assertion
  end

let tests =
  let both = [("Encoding", (module Encoding : Test))] in
  List.map (fun (name, m) -> Alcotest.test_case name `Quick (to_test m)) both
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      both
