(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Edwards25519 (L : LIB) = struct
  module MecEd25519 = Mec.Curve.Curve25519.AffineEdwards
  open L

  let nb_tests = 1

  let point_of_mec p : Z.t * Z.t =
    let u = MecEd25519.get_u_coordinate p in
    let v = MecEd25519.get_v_coordinate p in
    (MecEd25519.Base.to_z u, MecEd25519.Base.to_z v)

  let point_of_mec2 p q =
    let p = point_of_mec p in
    let q = point_of_mec q in
    (p, q)

  let point_of_mec3 p q r =
    let p = point_of_mec p in
    let q = point_of_mec q in
    let r = point_of_mec r in
    (p, q, r)

  open Helpers.Utils (L)

  open Plompiler.Edwards25519 (L)

  let add_circuit p q expected () =
    let* p = input_point ~kind:`Public p in
    let* q = input_point ~kind:`Public q in
    let* r = input_point expected in
    let* o = add p q in
    assert_equal o r

  let test_vectors =
    List.init nb_tests (fun _ -> (MecEd25519.random (), MecEd25519.random ()))

  let inputs =
    (* 0 + 0 = 0 *)
    (MecEd25519.zero, MecEd25519.zero)
    :: (* 0 + g = g *)
       (MecEd25519.zero, MecEd25519.one)
    :: (* p + q = r *)
       test_vectors

  let expected =
    (* 0 + 0 = 0 *)
    MecEd25519.zero
    :: (* 0 + g = g *)
       MecEd25519.one
    :: (* p + q = r *)
       List.map
         (fun (input1, input2) -> MecEd25519.add input1 input2)
         test_vectors

  let wrong = List.map (fun _ -> MecEd25519.random ()) expected

  let test_add =
    List.map2
      (fun (p1, p2) e ->
        let p1, p2, e = point_of_mec3 p1 p2 e in
        test ~valid:true ~name:"Edwards25519.test_add" @@ add_circuit p1 p2 e)
      inputs
      expected
    @ List.map2
        (fun (p1, p2) w ->
          let p1, p2, w = point_of_mec3 p1 p2 w in
          test ~valid:false @@ add_circuit p1 p2 w)
        inputs
        wrong

  let cond_add_circuit p q b expected () =
    let* p = input_point ~kind:`Public p in
    let* q = input_point ~kind:`Public q in
    let* b = input ~kind:`Public (Input.bool b) in
    let* r = input_point expected in
    let* o = cond_add p q b in
    assert_equal o r

  let inputs = List.map (fun (p, q) -> (p, q, Random.bool ())) test_vectors

  let expected =
    List.map
      (fun (input1, input2, b) ->
        if b then MecEd25519.add input1 input2 else input1)
      inputs

  let wrong = List.map (fun _ -> MecEd25519.random ()) expected

  let test_cond_add =
    List.map2
      (fun (p1, p2, b) e ->
        let p1, p2, e = point_of_mec3 p1 p2 e in
        test ~valid:true ~name:"Edwards25519.test_cond_add"
        @@ cond_add_circuit p1 p2 b e)
      inputs
      expected
    @ List.map2
        (fun (p1, p2, b) w ->
          let p1, p2, w = point_of_mec3 p1 p2 w in
          test ~valid:false @@ cond_add_circuit p1 p2 b w)
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
    (*     let size = Z.log2 MecEd25519.Scalar.order in *)
    let size = 5 in
    List.init nb_tests (fun _ ->
        (MecEd25519.random (), List.init size (fun _ -> Stdlib.Random.bool ())))

  let inputs =
    (* g^0 = 0 *)
    (MecEd25519.one, [false])
    :: (* g^1 = g *)
       (MecEd25519.one, [true])
    :: (* p + q = r *)
       test_vectors

  let expected =
    (* g^0 = 0 *)
    MecEd25519.zero
    :: (* g^1 = g *)
       MecEd25519.one
    :: (* p + q = r *)
       List.map
         (fun (input_p, input_b) ->
           let exponent = Plompiler.Utils.bool_list_to_scalar input_b in
           let exponent =
             MecEd25519.Scalar.of_bytes_exn (S.to_bytes exponent)
           in
           MecEd25519.mul input_p exponent)
         test_vectors

  let wrong = List.map (fun _ -> MecEd25519.random ()) expected

  let test_scalar_mul =
    List.map2
      (fun (p, b) e ->
        let p, e = point_of_mec2 p e in
        test ~valid:true ~name:"Edwards25519.test_scalar_mul"
        @@ scalar_mul_circuit p b e)
      inputs
      expected
    @ List.map2
        (fun (p, b) w ->
          let p, w = point_of_mec2 p w in
          test ~valid:false @@ scalar_mul_circuit p b w)
        inputs
        wrong

  let multi_scalar_mul_circuit ps bs expected () =
    let* p = mapM (fun p -> input_point ~kind:`Public p) ps in
    let* b = mapM (fun b -> input_bytes b) bs in
    let* r = input_point expected in
    let* o = multi_scalar_mul (to_list b) (to_list p) in
    assert_equal o r

  let test_vectors =
    (*     let size = Z.log2 MecEd25519.Scalar.order in *)
    let size = 5 in
    List.init nb_tests (fun i ->
        ( List.init (2 + i) (fun _ -> MecEd25519.random ()),
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
                MecEd25519.Scalar.of_bytes_exn (S.to_bytes exponent)
              in
              MecEd25519.mul input_p exponent)
            points
            exponents
        in
        let summed =
          List.fold_left
            (fun acc p -> MecEd25519.add acc p)
            MecEd25519.zero
            multiplied
        in
        point_of_mec summed)
      test_vectors

  let wrong = List.map (fun _ -> point_of_mec @@ MecEd25519.random ()) expected

  let test_multi_scalar_mul =
    List.map2
      (fun (p, b) e ->
        test ~valid:true ~name:"Edwards25519.test_multi_scalar_mul"
        @@ multi_scalar_mul_circuit p b e)
      inputs
      expected
    @ List.map2
        (fun (p, b) w ->
          test ~valid:false ~name:"Edwards25519.test_multi_scalar_mul"
          @@ multi_scalar_mul_circuit p b w)
        inputs
        wrong

  let is_on_curve_circuit p () =
    let* p = input_point ~kind:`Public p in
    let* b = is_on_curve p in
    Bool.assert_true b

  let test_vectors = List.init nb_tests (fun _ -> MecEd25519.random ())

  let inputs = List.map point_of_mec test_vectors

  let wrong =
    List.map
      (fun _ ->
        ( MecEd25519.Base.(to_z @@ random ()),
          MecEd25519.Base.(to_z @@ random ()) ))
      inputs

  let test_is_on_curve =
    List.map
      (fun i ->
        test ~valid:true ~name:"Edwards25519.test_is_on_curve"
        @@ is_on_curve_circuit i)
      inputs
    @ List.map (fun w -> test ~valid:false @@ is_on_curve_circuit w) wrong

  let bytes_of_hex = Plompiler.Utils.bytes_of_hex

  let to_compressed_bytes_circuit ~expected p () =
    let* z_exp = input ~kind:`Public expected in
    let* p = input_point ~kind:`Public p in
    let* z = to_compressed_bytes p in
    assert_equal z z_exp

  let test_to_compressed_bytes =
    List.map
      (fun (x, expected, valid) ->
        let name = "Edwards25519.test_to_compressed_bytes" in
        let expected = Bytes.input_bytes ~le:true @@ bytes_of_hex expected in
        let x = point_of_mec @@ Curve.of_bytes_exn @@ bytes_of_hex x in
        test ~valid ~name (to_compressed_bytes_circuit ~expected x))
      [
        ( "02bdcd8654ffa945b9e9e334176f23189885cf8db4d1653f83689ddca23a2161fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025",
          "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025",
          true );
        ( "4f445bba8b44933201eda162798f8f091a79b65f2c16dbf9666ef59fd00ea5396291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac32c",
          "6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac",
          true );
      ]

  let tests =
    test_add @ test_cond_add @ test_scalar_mul @ test_is_on_curve
    @ test_multi_scalar_mul @ test_to_compressed_bytes
end

let tests =
  let open Helpers in
  let both = [("Edwards25519", (module Edwards25519 : Test))] in
  List.map (fun (name, m) -> Alcotest.test_case name `Quick (to_test m)) both
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      both
