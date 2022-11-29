(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* This test is adapted from the one in [data-encoding]. *)

type tree = Leaf | Node of tree * forest

and forest = Empty | Children of tree * forest

(* This reference is used to record applications of the fixpoint functions in
   [mu]. In particular, we check that the memoisation works by asserting that
   the reference doesn't grow too much. ("Too much" being somewhat arbitrary.)
*)
let tree_points = ref 0

let forest_points = ref 0

let tree_encoding, forest_encoding =
  let open Json_encoding in
  let mu_tree forest_enc =
    mu "tree" @@ fun tree_enc ->
    incr tree_points ;
    union
      [
        case
          ~title:"Leaf"
          (string_enum [("leaf", ())])
          (function Leaf -> Some () | _ -> None)
          (fun () -> Leaf);
        case
          ~title:"Node"
          (obj2 (req "first" tree_enc) (req "forest" forest_enc))
          (function Node (tree, forest) -> Some (tree, forest) | _ -> None)
          (fun (tree, forest) -> Node (tree, forest));
      ]
  in
  let mu_forest =
    mu "forest" @@ fun forest_enc ->
    incr forest_points ;
    union
      [
        case
          ~title:"Empty"
          (string_enum [("empty", ())])
          (function Empty -> Some () | _ -> None)
          (fun () -> Empty);
        case
          ~title:"Children"
          (obj2 (req "tree" (mu_tree forest_enc)) (req "rest" forest_enc))
          (function Children (t, f) -> Some (t, f) | _ -> None)
          (fun (t, f) -> Children (t, f));
      ]
  in
  (mu_tree mu_forest, mu_forest)

let tree_points_done = !tree_points

let forest_points_done = !forest_points

let points () =
  (* fixpointing functions are not called before they are used *)
  assert (tree_points_done = 0) ;
  assert (tree_points_done = 0) ;
  ()

let tiny_test () =
  let open Json_encoding in
  let (_ : Json_repr.ezjsonm) = construct forest_encoding Empty in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding (Node (Leaf, Empty)) in
  let (_ : Json_repr.ezjsonm) =
    construct forest_encoding (Children (Leaf, Empty))
  in
  let (_ : Json_repr.ezjsonm) =
    construct tree_encoding (Node (Node (Leaf, Empty), Empty))
  in
  let (_ : Json_repr.ezjsonm) =
    construct forest_encoding (Children (Node (Leaf, Empty), Empty))
  in
  (* calls are memoised so they can't be too many of them *)
  assert (!tree_points <= 2) ;
  assert (!forest_points <= 1) ;
  ()

let flip_flop () =
  let open Json_encoding in
  let flip = Node (Leaf, Empty) in
  let flop = Leaf in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding flip in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding flip in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding flop in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding flip in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding flop in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding flip in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding flop in
  assert (!tree_points <= 2) ;
  assert (!forest_points <= 1) ;
  ()

let big_test () =
  let open Json_encoding in
  let f =
    Children
      ( Node (Node (Leaf, Children (Leaf, Empty)), Children (Leaf, Empty)),
        Children (Leaf, Empty) )
  in
  let (_ : Json_repr.ezjsonm) = construct forest_encoding f in
  let t = Node (Leaf, f) in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding t in
  let t = Node (t, f) in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding t in
  let (_ : Json_repr.ezjsonm) = construct forest_encoding f in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding t in
  let f = Children (t, f) in
  let (_ : Json_repr.ezjsonm) = construct forest_encoding f in
  let f = Children (t, f) in
  let (_ : Json_repr.ezjsonm) = construct forest_encoding f in
  let t = Node (t, f) in
  let (_ : Json_repr.ezjsonm) = construct tree_encoding t in
  assert (!tree_points <= 2) ;
  assert (!forest_points <= 1) ;
  ()

let tests =
  [
    ("points", `Quick, points);
    ("tiny", `Quick, tiny_test);
    ("flip-flop", `Quick, flip_flop);
    ("big", `Quick, big_test);
  ]

let () =
  Random.self_init () ;
  Alcotest.run "json-data-encoding" [("mu", tests)]
