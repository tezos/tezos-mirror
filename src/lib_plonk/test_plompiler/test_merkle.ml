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
module Merkle = Merkle (Poseidon252)
module Helper = Merkle.P

module MerklePath (L : LIB) = struct
  open Utils (L)

  open Merkle.V (L)

  let left = Merkle.left

  let right = Merkle.right

  let tree = Helper.generate_tree 2

  let root = Helper.root tree

  let test_path_correctness () =
    let open Helper in
    let hash a = Hash.direct a in
    let r =
      S.of_string
        "40895470603270371738190901050353228005886571814752512228619736449827845164505"
    in
    let wl =
      S.of_string
        "3424181894117710793412228511600041687987227889466046940028281425652819848308"
    in
    let wr =
      S.of_string
        "7957136680924550432598373971480555041766263219276812040814026910179063320538"
    in

    let wll = S.of_string "0" in

    let wlr = S.of_string "1" in

    assert (S.(hash [|wll; wlr|] = wl)) ;

    let wrl = S.of_string "2" in

    let wrr = S.of_string "3" in

    assert (S.(hash [|wrl; wrr|] = wr)) ;

    assert (S.(hash [|wl; wr|] = r)) ;

    let tree =
      Node (r, Node (wl, Leaf wll, Leaf wlr), Node (wr, Leaf wrl, Leaf wrr))
    in
    let w0, path0 = proof_path 0 tree in
    let exp_path0 = [(wlr, left); (wr, left)] in
    assert (S.eq w0 wll) ;

    let w1, path1 = proof_path 1 tree in
    let exp_path1 = [(wll, right); (wr, left)] in
    assert (S.eq w1 wlr) ;

    let w2, path2 = proof_path 2 tree in
    let exp_path2 = [(wrr, left); (wl, right)] in
    assert (S.eq w2 wrl) ;

    let w3, path3 = proof_path 3 tree in
    let exp_path3 = [(wrl, right); (wl, right)] in
    assert (S.eq w3 wrr) ;

    List.iter2
      (fun path exp_path ->
        List.iter2
          (fun (h, d) (h', d') -> assert (S.eq h h' && d = d'))
          path
          exp_path)
      [path0; path1; path2; path3]
      [exp_path0; exp_path1; exp_path2; exp_path3]

  let test = test_path_correctness
end

(* Doesn't really depend on evaluator, this is a pure test *)
module MPath = MerklePath (LibCircuit)

module MerklePos (L : LIB) = struct
  open L

  open Utils (L)

  open Merkle.V (L)

  let test_merkle path leaf root =
    let* path = input ~kind:`Public path in
    let path = path_encoding.decode path in
    let* leaf = input ~kind:`Public leaf in
    let* root = input root in
    let* o = merkle_proof path leaf root in
    Bool.assert_true o

  let tree = Helper.generate_tree 2

  let root = Helper.root tree

  let test_merkle_pos pos () =
    let leaf, path = Helper.proof_path pos tree in
    let leaf = Input.scalar leaf in
    let path = path_encoding.input path in
    let root = Input.scalar root in
    test_merkle path leaf root

  let tests_merkle_pos =
    List.init 4 (fun i ->
        test ~valid:true ~name:"Merkle.test_merkle_pos" @@ test_merkle_pos i)

  let test_merkle_wrong_leaf () =
    let _, path = Helper.proof_path 2 tree in
    let leaf = Input.scalar root in
    let path = path_encoding.input path in
    let root = Input.scalar root in
    test_merkle path leaf root

  let tests = tests_merkle_pos @ [test ~valid:false test_merkle_wrong_leaf]
end

let tests =
  [
    Alcotest.test_case "Merkle path" `Quick MPath.test;
    Alcotest.test_case "Merkle pos" `Quick (to_test (module MerklePos : Test));
    Alcotest.test_case
      "Merkle pos plonk"
      `Slow
      (to_test ~plonk:(module Plonk.Main_protocol) (module MerklePos : Test));
  ]
