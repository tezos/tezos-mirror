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

module Poseidon_test
    (Mec : Plompiler__Hash_sig.P_HASH) (P : functor (L : LIB) -> sig
      val digest :
        ?input_length:int -> L.scalar list L.repr -> L.scalar L.repr L.t
    end)
    (L : LIB) =
struct
  open L

  open Utils (L)

  module P = P (L)

  let test_poseidon ?(fixed_length = false) inputs expected () =
    let* inputs = input ~kind:`Public inputs in
    let* expected = input expected in
    let input_length =
      if fixed_length then Some (List.length @@ of_list inputs) else None
    in
    let* o = P.digest ?input_length inputs in
    with_bool_check (equal o expected)

  let test_vectors =
    [[|S.one|]; Array.init 4 (fun i -> S.of_string @@ string_of_int (i + 1))]

  let inputs : scalar list Input.t list =
    List.map
      (fun v -> Input.(list @@ List.map scalar (Array.to_list v)))
      test_vectors

  let expected ?(fixed_length = false) () =
    let direct inputs =
      let input_length =
        if fixed_length then Some (Array.length inputs) else None
      in
      let ctx = Mec.init ?input_length () in
      let ctx = Mec.digest ctx inputs in
      Mec.get ctx
    in
    List.map (fun v -> Input.scalar @@ direct v) test_vectors

  let wrong = List.map (fun _ -> Input.scalar @@ S.random ()) (expected ())

  let tests_variable_length =
    List.map2
      (fun i e ->
        test ~valid:true ~name:"Poseidon.test_poseidon" @@ test_poseidon i e)
      inputs
      (expected ())
    @ List.map2
        (fun i w ->
          test ~name:"Poseidon.test_poseidon" ~valid:false @@ test_poseidon i w)
        inputs
        wrong

  let tests_fixed_length =
    List.map2
      (fun i e ->
        test ~valid:true ~name:"Poseidon.test_poseidon.fixed_length"
        @@ test_poseidon ~fixed_length:true i e)
      inputs
      (expected ~fixed_length:true ())
    @ List.map2
        (fun i w ->
          test ~valid:false ~name:"Poseidon.test_poseidon"
          @@ test_poseidon ~fixed_length:true i w)
        inputs
        wrong

  let tests = tests_variable_length @ tests_fixed_length
end

let tests =
  let open Gadget in
  let both =
    [
      ( "Poseidon128",
        (module Poseidon_test (Poseidon128.P) (Poseidon128.V) : Test) );
      ("Poseidon252", (module Poseidon_test (Poseidon252.P) (Poseidon252.V)));
      ("PoseidonFull", (module Poseidon_test (PoseidonFull.P) (PoseidonFull.V)));
    ]
  in
  List.map (fun (name, m) -> Alcotest.test_case name `Quick (to_test m)) both
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      both
