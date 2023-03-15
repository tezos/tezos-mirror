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

module Vectors = struct
  let g =
    ( S.of_string
        "0x187d2619ff114316d237e86684fb6e3c6b15e9b924fa4e322764d3177508297a",
      S.of_string
        "0x6230c613f1b460e026221be21cf4eabd5a8ea552db565cb18d3cabc39761eb9b" )

  let g2 =
    ( S.of_string
        "0x35d84b4bbb504c30df6005260b550f1b00db242e7c2dee016712bd2b0f13281b",
      S.of_string
        "0x412ef797a253102e1bd0d6a40e717a0abfa0b7c56d0ef92a81cb8292fdc6f6d5" )

  (* https://github.com/zcash/librustzcash/blob/1b4aab0b76d465e4fe548f230b7c3ebdc398a1a5/zcash_primitives/src/constants.rs *)
  let valid_points =
    [
      (S.zero, S.one);
      ( S.of_string
          "0x73c016a42ded9578b5ea25de7ec0e3782f0c718f6f0fbadd194e42926f661b51",
        S.of_string
          "0x289e87a2d3521b5779c9166b837edc5ef9472e8bc04e463277bfabd432243cca"
      );
      ( S.of_string
          "0x15a36d1f0f390d8852a35a8c1908dd87a361ee3fd48fdf77b9819dc82d90607e",
        S.of_string
          "0x015d8c7f5b43fe33f7891142c001d9251f3abeeb98fad3e87b0dc53c4ebf1891"
      );
      ( S.of_string
          "0x664321a58246e2f6eb69ae39f5c84210bae8e5c46641ae5c76d6f7c2b67fc475",
        S.of_string
          "0x362e1500d24eee9ee000a46c8e8ce8538bb22a7f1784b49880ed502c9793d457"
      );
      ( S.of_string
          "0x323a6548ce9d9876edc5f4a9cff29fd57d02d50e654b87f24c767804c1c4a2cc",
        S.of_string
          "0x2f7ee40c4b56cad891070acbd8d947b75103afa1a11f6a8584714beca33570e9"
      );
      ( S.of_string
          "0x3bd2666000b5479689b64b4e03362796efd5931305f2f0bf46809430657f82d1",
        S.of_string
          "0x494bc52103ab9d0a397832381406c9e5b3b9d8095859d14c99968299c3658aef"
      );
      ( S.of_string
          "0x63447b2ba31bb28ada049746d76d3ee51d9e5ca21135ff6fcb3c023258d32079",
        S.of_string
          "0x64ec4689e8bfb6e564cdb1070a136a28a80200d2c66b13a7436082119f8d629a"
      );
    ]
end

module JubjubEdwards (L : LIB) = struct
  module MecJubJub = Mec.Curve.Jubjub.AffineEdwards
  open L

  let point_of_mec p =
    let u = MecJubJub.get_u_coordinate p in
    let v = MecJubJub.get_v_coordinate p in
    ( S.of_bytes_exn (MecJubJub.Base.to_bytes u),
      S.of_bytes_exn (MecJubJub.Base.to_bytes v) )

  open Helpers.Utils (L)

  open Plompiler.JubjubEdwards (L)

  let add_circuit p q expected () =
    let* p = input_point ~kind:`Public p in
    let* q = input_point ~kind:`Public q in
    let* r = input_point expected in
    let* o = add p q in
    assert_equal o r

  let test_vectors =
    List.init 7 (fun _ -> (MecJubJub.random (), MecJubJub.random ()))

  let inputs =
    let open Vectors in
    (* 0 + 0 = 0 *)
    (id, id) :: (* 0 + g = g *)
                (id, g) :: (* g + g = g2 *)
                           (g, g)
    :: (* p + q = r *)
       List.map (fun (p, q) -> (point_of_mec p, point_of_mec q)) test_vectors

  let expected =
    let open Vectors in
    (* 0 + 0 = 0 *)
    id :: (* 0 + g = g *)
          g :: (* g + g = g2 *)
               g2
    :: (* p + q = r *)
       List.map
         (fun (input1, input2) ->
           let res = MecJubJub.add input1 input2 in
           point_of_mec res)
         test_vectors

  let wrong = List.map (fun _ -> (S.random (), S.random ())) expected

  let test_add =
    List.map2
      (fun (p1, p2) e ->
        test ~valid:true ~name:"Edwards.test_add" @@ add_circuit p1 p2 e)
      inputs
      expected
    @ List.map2
        (fun (p1, p2) w -> test ~valid:false @@ add_circuit p1 p2 w)
        inputs
        wrong

  let cond_add_circuit p q b expected () =
    let* p = input_point ~kind:`Public p in
    let* q = input_point ~kind:`Public q in
    let* b = input ~kind:`Public (Input.bool b) in
    let* r = input_point expected in
    let* o = cond_add p q b in
    assert_equal o r

  let inputs =
    List.map
      (fun (p, q) -> (point_of_mec p, point_of_mec q, Random.bool ()))
      test_vectors

  let expected =
    List.map2
      (fun (input1, input2) (_, _, b) ->
        let res = if b then MecJubJub.add input1 input2 else input1 in
        point_of_mec res)
      test_vectors
      inputs

  let wrong = List.map (fun _ -> (S.random (), S.random ())) expected

  let test_cond_add =
    List.map2
      (fun (p1, p2, b) e ->
        test ~valid:true ~name:"Edwards.test_cond_add"
        @@ cond_add_circuit p1 p2 b e)
      inputs
      expected
    @ List.map2
        (fun (p1, p2, b) w -> test ~valid:false @@ cond_add_circuit p1 p2 b w)
        inputs
        wrong

  let input_bytes bytes = input Input.(list (List.map bool bytes))

  let scalar_mul_circuit p input_b expected () =
    let* p = input_point ~kind:`Public p in
    let* b = input_bytes input_b in
    let* r = input_point expected in
    let* o = scalar_mul b p in
    assert_equal o r

  let test_vectors =
    let size = Z.log2 S.order in
    List.init 7 (fun _ ->
        (MecJubJub.random (), List.init size (fun _ -> Stdlib.Random.bool ())))

  let inputs =
    let open Vectors in
    (* g^0 = 0 *)
    (g, [false]) :: (* g^1 = g *)
                    (g, [true])
    :: (* g^2 = g2 *)
       (g, [false; true])
    :: (* p + q = r *)
       List.map
         (fun (input_p, input_b) -> (point_of_mec input_p, input_b))
         test_vectors

  let expected =
    let open Vectors in
    (* g^0 = 0 *)
    id :: (* g^1 = g *)
          g :: (* g^2 = g2 *)
               g2
    :: (* p + q = r *)
       List.map
         (fun (input_p, input_b) ->
           let exponent = Plompiler.Utils.bool_list_to_scalar input_b in
           let exponent = MecJubJub.Scalar.of_bytes_exn (S.to_bytes exponent) in
           let res = MecJubJub.mul input_p exponent in
           point_of_mec res)
         test_vectors

  let wrong = List.map (fun _ -> (S.random (), S.random ())) expected

  let test_scalar_mul =
    List.map2
      (fun (p, b) e ->
        test ~valid:true ~name:"Edwards.test_scalar_mul"
        @@ scalar_mul_circuit p b e)
      inputs
      expected
    @ List.map2
        (fun (p, b) w -> test ~valid:false @@ scalar_mul_circuit p b w)
        inputs
        wrong

  let multi_scalar_mul_circuit ps bs expected () =
    let* p = mapM (fun p -> input_point ~kind:`Public p) ps in
    let* b = mapM (fun b -> input_bytes b) bs in
    let* r = input_point expected in
    let* o = multi_scalar_mul (to_list b) (to_list p) in
    assert_equal o r

  let test_vectors =
    let size = Z.log2 S.order in
    List.init 5 (fun i ->
        ( List.init (2 + i) (fun _ -> MecJubJub.random ()),
          List.init (2 + i) (fun _ ->
              List.init size (fun _ -> Stdlib.Random.bool ())) ))

  let inputs =
    (* Pi p_i^s_i = r *)
    List.map
      (fun (points, exponents) ->
        let points = List.map point_of_mec points in
        (points, exponents))
      test_vectors

  let expected =
    List.map
      (fun (points, exponents) ->
        let multiplied =
          List.map2
            (fun input_p input_b ->
              let exponent = Plompiler.Utils.bool_list_to_scalar input_b in
              let exponent =
                MecJubJub.Scalar.of_bytes_exn (S.to_bytes exponent)
              in
              MecJubJub.mul input_p exponent)
            points
            exponents
        in
        let summed =
          List.fold_left
            (fun acc p -> MecJubJub.add acc p)
            MecJubJub.zero
            multiplied
        in
        point_of_mec summed)
      test_vectors

  let wrong = List.map (fun _ -> (S.random (), S.random ())) expected

  let test_multi_scalar_mul =
    List.map2
      (fun (p, b) e ->
        test ~valid:true ~name:"Edwards.test_multi_scalar_mul"
        @@ multi_scalar_mul_circuit p b e)
      inputs
      expected
    @ List.map2
        (fun (p, b) w ->
          test ~valid:false ~name:"Edwards.test_multi_scalar_mul"
          @@ multi_scalar_mul_circuit p b w)
        inputs
        wrong

  let is_on_curve_circuit p () =
    let* p = input_point ~kind:`Public p in
    let* b = is_on_curve p in
    Bool.assert_true b

  let test_vectors = List.init 10 (fun _ -> MecJubJub.random ())

  let inputs =
    let open Vectors in
    valid_points @ List.map point_of_mec test_vectors

  let wrong = List.map (fun _ -> (S.random (), S.random ())) inputs

  let test_is_on_curve =
    List.map
      (fun i ->
        test ~valid:true ~name:"Edwards.test_is_on_curve"
        @@ is_on_curve_circuit i)
      inputs
    @ List.map (fun w -> test ~valid:false @@ is_on_curve_circuit w) wrong

  let tests =
    test_add @ test_cond_add @ test_scalar_mul @ test_is_on_curve
    @ test_multi_scalar_mul
end

let tests =
  let open Helpers in
  let both = [("JubJubEdward", (module JubjubEdwards : Test))] in
  List.map (fun (name, m) -> Alcotest.test_case name `Quick (to_test m)) both
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      both
