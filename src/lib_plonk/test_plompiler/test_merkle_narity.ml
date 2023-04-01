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
module Hash = Poseidon252.P

let n = 3

module MerklePos (L : LIB) = struct
  open L

  open Utils (L)

  open Merkle_narity.V (Poseidon252.V) (L)

  module Helper = Merkle_narity.P (Hash)

  let test_merkle witness root =
    let* witness = input ~kind:`Public witness in
    let* root = input root in
    let* o = merkle_proof n witness root in
    Bool.assert_true o

  let height = 5

  let tree = Helper.generate_tree n height

  let root = Helper.root tree

  let test_merkle_pos pos () =
    let path, leaf = Helper.proof_path_narity pos tree in
    let witness =
      Input.(
        pair
          (scalar leaf)
          (list
             (List.map
                (fun (sl, dir) ->
                  pair (list @@ List.map scalar sl) (scalar dir))
                path)))
    in
    let root = Input.scalar root in
    test_merkle witness root

  let tests_merkle_pos =
    let pos = [0; 1; 2; 1; 2] in
    test ~valid:true ~name:"MerkleNarity.test_merkle_pos" @@ test_merkle_pos pos

  let test_merkle_wrong_leaf () =
    let pos = [0; 1; 2; 1; 2] in
    let path, _ = Helper.proof_path_narity pos tree in
    let witness =
      Input.(
        pair
          (scalar root)
          (list
             (List.map
                (fun (sl, dir) ->
                  pair (list @@ List.map scalar sl) (scalar dir))
                path)))
    in
    let root = Input.scalar root in
    test_merkle witness root

  let tests = [tests_merkle_pos] @ [test ~valid:false test_merkle_wrong_leaf]
end

let tests =
  [
    Alcotest.test_case "Merkle pos" `Quick (to_test (module MerklePos : Test));
    Alcotest.test_case
      "Merkle pos plonk"
      `Slow
      (to_test ~plonk:(module Plonk.Main_protocol) (module MerklePos : Test));
  ]
