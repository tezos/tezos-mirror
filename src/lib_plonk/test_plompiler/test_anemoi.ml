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

module Anemoi_test (L : LIB) = struct
  open L

  open Utils (L)

  module Hash1 = AnemoiJive_128_1 (L)
  module Hash2 = Anemoi128.V (L)

  let test_vectors = [(S.one, S.one); (S.one, S.(one + one))]

  let inputs : (scalar Input.t * scalar Input.t) list =
    List.map (fun (x, y) -> (Input.scalar x, Input.scalar y)) test_vectors

  module AnemoiPerm = Bls12_381_hash.Permutation.Anemoi

  let parameters = AnemoiPerm.Parameters.security_128_state_size_2

  let anemoi_instance = AnemoiPerm.allocate_ctxt parameters

  let matrix = AnemoiPerm.Parameters.get_matrix parameters

  (* Test naive implementation of Anemoi *)
  let test_round x y expected_x expected_y () =
    let* x = input ~kind:`Public x in
    let* y = input ~kind:`Public y in
    let* expected_x = input expected_x in
    let* expected_y = input expected_y in
    (* use round constants #0, #1 *)
    let* x, y = Hash1.init_state_for_rounds x y in
    (* use round constants #2, #3 *)
    let* xx, yy, _ = Hash1.round (x, y, 2) in
    let* bx = equal xx expected_x in
    let* by = equal yy expected_y in
    let b = Bool.band bx by in
    with_bool_check b

  let tests_round =
    let expected () =
      List.map
        (fun v ->
          let x, y = v in
          let res =
            AnemoiPerm.set_state anemoi_instance [|x; y|] ;
            AnemoiPerm.apply_one_round anemoi_instance 0 ;
            AnemoiPerm.apply_constants_addition anemoi_instance 1 ;
            AnemoiPerm.apply_linear_layer anemoi_instance ;
            AnemoiPerm.get_state anemoi_instance
          in
          (Input.scalar res.(0), Input.scalar res.(1)))
        test_vectors
    in
    let wrong =
      List.map
        (fun _ -> (Input.scalar @@ S.random (), Input.scalar @@ S.random ()))
        (expected ())
    in
    List.map2
      (fun (x, y) (ex, ey) ->
        test ~valid:true ~name:"Anemoi.test_round.valid" @@ test_round x y ex ey)
      inputs
      (expected ())
    @ List.map2
        (fun (x, y) (wx, wy) ->
          test ~valid:false ~name:"Anemoi.test_round.invalid"
          @@ test_round x y wx wy)
        inputs
        wrong

  let test_anemoi_compress x y expected () =
    let* x = input ~kind:`Public x in
    let* y = input ~kind:`Public y in
    let* expected = input expected in
    let* o = Hash1.compress x y in
    with_bool_check (equal o expected)

  let tests_compress =
    let expected () =
      let direct (x, y) =
        let state =
          AnemoiPerm.set_state anemoi_instance [|x; y|] ;
          AnemoiPerm.apply_permutation anemoi_instance ;
          AnemoiPerm.get_state anemoi_instance
        in
        S.(state.(0) + state.(1) + x + y)
      in
      List.map (fun v -> Input.scalar @@ direct v) test_vectors
    in
    let wrong = List.map (fun _ -> Input.scalar @@ S.random ()) (expected ()) in
    List.map2
      (fun (x, y) e ->
        test ~valid:true ~name:"Anemoi.test_compress.valid"
        @@ test_anemoi_compress x y e)
      inputs
      (expected ())
    @ List.map2
        (fun (x, y) w ->
          test ~valid:false ~name:"Anemoi.test_compress.invalid"
          @@ test_anemoi_compress x y w)
        inputs
        wrong

  (* Test implementation with two rounds in 5 constraints *)
  let test_double_round x y expected_x expected_y () =
    let* x = input ~kind:`Public x in
    let* y = input ~kind:`Public y in
    let* expected_x = input expected_x in
    let* expected_y = input expected_y in
    (* use round constants #0, #1 *)
    let* x, y = Hash1.init_state_for_rounds x y in
    (* use round constants #2, #3, #4, #5 *)
    let* xx, yy, _ = Hash1.double_round (x, y, 2) in
    let* bx = equal xx expected_x in
    let* by = equal yy expected_y in
    let b = Bool.band bx by in
    with_bool_check b

  let tests_double_round =
    let expected () =
      List.map
        (fun v ->
          let x, y = v in
          let res =
            AnemoiPerm.set_state anemoi_instance [|x; y|] ;
            AnemoiPerm.apply_one_round anemoi_instance 0 ;
            AnemoiPerm.apply_one_round anemoi_instance 1 ;
            AnemoiPerm.apply_constants_addition anemoi_instance 2 ;
            AnemoiPerm.apply_linear_layer anemoi_instance ;
            AnemoiPerm.get_state anemoi_instance
          in
          (Input.scalar res.(0), Input.scalar res.(1)))
        test_vectors
    in
    let wrong =
      List.map
        (fun _ -> (Input.scalar @@ S.random (), Input.scalar @@ S.random ()))
        (expected ())
    in
    List.map2
      (fun (x, y) (ex, ey) ->
        test ~valid:true ~name:"Anemoi.test_double_round.valid"
        @@ test_double_round x y ex ey)
      inputs
      (expected ())
    @ List.map2
        (fun (x, y) (wx, wy) ->
          test ~valid:false ~name:"Anemoi.test_double_round.invalid"
          @@ test_double_round x y wx wy)
        inputs
        wrong

  let test_anemoi_compress_two x y expected () =
    let* x = input ~kind:`Public x in
    let* y = input ~kind:`Public y in
    let* expected = input expected in
    let* o = Hash1.compress_two x y in
    with_bool_check (equal o expected)

  let tests_compress_two =
    let expected () =
      let direct (x, y) =
        let state =
          AnemoiPerm.set_state anemoi_instance [|x; y|] ;
          AnemoiPerm.apply_permutation anemoi_instance ;
          AnemoiPerm.get_state anemoi_instance
        in
        S.(state.(0) + state.(1) + x + y)
      in
      List.map (fun v -> Input.scalar @@ direct v) test_vectors
    in
    let wrong = List.map (fun _ -> Input.scalar @@ S.random ()) (expected ()) in
    List.map2
      (fun (x, y) e ->
        test ~valid:true ~name:"Anemoi.test_compress_two.valid"
        @@ test_anemoi_compress_two x y e)
      inputs
      (expected ())
    @ List.map2
        (fun (x, y) w ->
          test ~valid:false ~name:"Anemoi.test_compress_two.invalid"
          @@ test_anemoi_compress_two x y w)
        inputs
        wrong

  (* Test with custom gates for 2 rounds in 4 constraints *)
  let test_anemoi_hash inputs expected () =
    let* expected = input ~kind:`Public expected in
    let* inputs = input inputs in
    let* o = Hash2.digest inputs in
    with_bool_check (equal o expected)

  let tests_hash =
    let test_vectors = [[S.one; S.one]; [S.one; S.one; S.one; S.one]] in

    let inputs : scalar list Input.input list =
      List.map
        (fun l -> List.map (fun x -> Input.scalar x) l |> Input.list)
        test_vectors
    in
    let expected () =
      let direct l = Anemoi128.P.direct (Array.of_list l) in
      let direct2 l =
        let anemoi_instance = Anemoi128.P.init () in
        let anemoi_instance = Anemoi128.P.digest anemoi_instance l in
        Anemoi128.P.get anemoi_instance
      in
      List.map
        (fun v ->
          let exp = direct v in
          assert (S.(eq exp (direct2 (Array.of_list v)))) ;
          Input.scalar @@ exp)
        test_vectors
    in
    let wrong = List.map (fun _ -> Input.scalar @@ S.random ()) (expected ()) in
    List.map2
      (fun l e ->
        test ~valid:true ~name:"Anemoi.test_compress.valid"
        @@ test_anemoi_hash l e)
      inputs
      (expected ())
    @ List.map2
        (fun l w ->
          test ~valid:false ~name:"Anemoi.test_compress.invalid"
          @@ test_anemoi_hash l w)
        inputs
        wrong

  let tests =
    tests_round @ tests_compress @ tests_double_round @ tests_compress_two
    @ tests_hash
end

let tests =
  let both = [("Anemoi", (module Anemoi_test : Test))] in
  List.map (fun (name, m) -> Alcotest.test_case name `Quick (to_test m)) both
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      both
