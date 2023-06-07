(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Crypto
    Invocation:   dune exec src/lib_crypto/test/main.exe
    Subject:      Merkle tree
*)

open Utils.Infix

type tree = Empty | Leaf of int | Node of tree * tree

let rec pp_tree fmt = function
  | Empty -> Format.pp_print_string fmt "Empty"
  | Leaf n -> Format.fprintf fmt "Leaf %d" n
  | Node (t1, t2) -> Format.fprintf fmt "Node (%a) (%a)" pp_tree t1 pp_tree t2

let testable_tree = Alcotest.testable pp_tree ( = )

let rec list_of_tree = function
  | Empty -> ([], 0)
  | Leaf x -> ([x], 1)
  | Node (x, y) ->
      let x, sx = list_of_tree x and y, sy = list_of_tree y in
      assert (sx = sy) ;
      (x @ y, sx + sy)

module Merkle = Blake2B.Generic_Merkle_tree (struct
  type t = tree

  type elt = int

  let empty = Empty

  let leaf i = Leaf i

  let node x y = Node (x, y)
end)

(** [compare_list xs ys] returns true if [ys = xs @ tl], where [tl] is
    a (potentially empty) repetition of the last element of [xs];

    e.g., [compare_list [1;2;3] [1;2;3]],
          [compare_list [1;2;3] [1;2;3;3;3]] both return true.
*)
let rec compare_list xs ys =
  match (xs, ys) with
  | [], [] -> true
  | [x], y :: ys when x = y -> ys = [] || compare_list xs ys
  | x :: xs, y :: ys when x = y -> compare_list xs ys
  | _, _ -> false

let check_size i =
  let l = 0 -- i in
  let l2, _ = list_of_tree (Merkle.compute l) in
  if compare_list l l2 then ()
  else
    Format.kasprintf
      failwith
      "Failed for %d: %a"
      i
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";")
         Format.pp_print_int)
      l2

(** A Merkle tree - computed from a range list - that is recast into
    a list, yields the same range list.
*)
let test_compute _ = List.iter check_size (0 -- 99)

(** Tests {!Generic_merkle_tree.compute} with examples from its docstring *)
let test_compute_examples _ =
  let examples =
    (* description, input list, computed tree *)
    [
      ("empty", [], Empty);
      ("leaft", [1], Leaf 1);
      ("padded", [1; 2; 3], Node (Node (Leaf 1, Leaf 2), Node (Leaf 3, Leaf 3)));
      ("full", [1; 2; 3; 3], Node (Node (Leaf 1, Leaf 2), Node (Leaf 3, Leaf 3)));
      ( "full 2",
        [1; 2; 3; 4],
        Node (Node (Leaf 1, Leaf 2), Node (Leaf 3, Leaf 4)) );
    ]
  in
  List.iter
    (fun (desc, list, tree) ->
      Alcotest.check testable_tree desc (Merkle.compute list) tree)
    examples

let check_path i =
  let l = 0 -- i in
  let orig = Merkle.compute l in
  List.iter
    (fun j ->
      let path = Merkle.compute_path l j in
      let found, pos = Merkle.check_path path j in
      if found = orig && j = pos then ()
      else Format.kasprintf failwith "Failed for %d in %d." j i)
    l

let rec pp_path fmt =
  let open Merkle in
  function
  | Left (p, r) -> Format.fprintf fmt "Left (%a, %a)" pp_path p pp_tree r
  | Right (l, p) -> Format.fprintf fmt "Right (%a, %a)" pp_tree l pp_path p
  | Op -> Format.pp_print_string fmt "Op"

let testable_path = Alcotest.testable pp_path ( = )

(** Checks paths in the generated Merkle trees. For each element,
    compute its path. Then use check_path to reconstruct the tree and
    compute the position of the element. Assert that the reconstructed
    tree equals the original tree, and that computed position equals
    the original position of the element. *)
let test_path _ = List.iter check_path (0 -- 128)

(** Tests {!Generic_Merkle_tree.compute_path} and
   {!Generic_Merkle_tree.check_path} with examples from its docstring *)
let test_path_examples _ =
  Alcotest.check
    testable_path
    "path to 3rd element"
    (Merkle.compute_path [4; 5; 6; 7] 2)
    (Right (Node (Leaf 4, Leaf 5), Left (Op, Leaf 7))) ;
  let t, idx =
    Merkle.check_path (Right (Node (Leaf 4, Leaf 5), Left (Op, Leaf 7))) 6
  in
  Alcotest.check
    testable_tree
    "recompute tree: tree"
    t
    (Merkle.compute [4; 5; 6; 7]) ;
  Alcotest.check Alcotest.int "recompute tree: index" 2 idx

let tests =
  [
    ( "merkle",
      [
        ("compute", `Quick, test_compute);
        ("compute_examples", `Quick, test_compute_examples);
        ("path", `Quick, test_path);
        ("path_examples", `Quick, test_path_examples);
      ] );
  ]
