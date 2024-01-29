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

(*

   In the following example of mutually recursive encodings, the
   memoization of fix applications of [mu] is a bit subtle.

   When the [fix] function of [mu_forest] is evaluated the first
   time, it triggers the evaluation of [mu_tree forest_enc] which
   has its own memoization state. Since the [fix] function of
   [mu_forest] is memoized, this memoization state is also preserved
   between each usage of [mu_forest] (as long as [mu_forest]'s [fix]
   is applied to the same argument). Therefore, the memoization of
   the [fix] of [mu_tree forest_enc] will occur as expected.

*)
type tree = Leaf | Node of tree * forest

and forest = Empty | Children of tree * forest

(* This reference is used to record applications of the fixpoint functions in
   [mu]. In particular, we check that the memoisation works by asserting that
   the reference doesn't grow too much. ("Too much" being somewhat arbitrary.)
*)
let tree_points = ref 0

let forest_points = ref 0

let tree_encoding, forest_encoding =
  let open Data_encoding in
  let mu_tree forest_enc =
    mu "tree" @@ fun tree_enc ->
    incr tree_points ;
    union
      [
        case
          (Tag 0)
          ~title:"Leaf"
          empty
          (function Leaf -> Some () | _ -> None)
          (fun () -> Leaf);
        case
          (Tag 1)
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
          (Tag 0)
          ~title:"Empty"
          empty
          (function Empty -> Some () | _ -> None)
          (fun () -> Empty);
        case
          (Tag 1)
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
  (* [mu_tree] is called twice, each call creates one [Mu] node, each
      instantiation of [Mu] can call the fixpointing function at most twice. *)
  assert (tree_points_done <= 4) ;
  (* [mu_forest] is evaluated once, the evaluation creates one [Mu] node, each
      instantiation of [Mu] can call the fixpointing function at most twice. *)
  assert (forest_points_done <= 2) ;
  ()

let tiny_test () =
  let open Data_encoding in
  let (_ : bytes) = Binary.to_bytes_exn forest_encoding Empty in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding (Node (Leaf, Empty)) in
  let (_ : bytes) =
    Binary.to_bytes_exn forest_encoding (Children (Leaf, Empty))
  in
  let (_ : bytes) =
    Binary.to_bytes_exn tree_encoding (Node (Node (Leaf, Empty), Empty))
  in
  let (_ : bytes) =
    Binary.to_bytes_exn forest_encoding (Children (Node (Leaf, Empty), Empty))
  in
  (* Calls to encoding do not invalidate the memoisation: points are the same *)
  assert (!tree_points = tree_points_done) ;
  assert (!forest_points = forest_points_done) ;
  ()

let flip_flop () =
  let open Data_encoding in
  let flip = Node (Leaf, Empty) in
  let flop = Leaf in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding flip in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding flip in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding flop in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding flip in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding flop in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding flip in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding flop in
  (* Calls to encoding do not invalidate the memoisation: points are the same *)
  assert (!tree_points = tree_points_done) ;
  assert (!forest_points = forest_points_done) ;
  ()

let big_test () =
  let open Data_encoding in
  let f =
    Children
      ( Node (Node (Leaf, Children (Leaf, Empty)), Children (Leaf, Empty)),
        Children (Leaf, Empty) )
  in
  let (_ : bytes) = Binary.to_bytes_exn forest_encoding f in
  let t = Node (Leaf, f) in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding t in
  let t = Node (t, f) in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding t in
  let (_ : bytes) = Binary.to_bytes_exn forest_encoding f in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding t in
  let f = Children (t, f) in
  let (_ : bytes) = Binary.to_bytes_exn forest_encoding f in
  let f = Children (t, f) in
  let (_ : bytes) = Binary.to_bytes_exn forest_encoding f in
  let t = Node (t, f) in
  let (_ : bytes) = Binary.to_bytes_exn tree_encoding t in
  (* Calls to encoding do not invalidate the memoisation: points are the same *)
  assert (!tree_points = tree_points_done) ;
  assert (!forest_points = forest_points_done) ;
  ()

type foo = Foo of foo option

let doesnt_terminate () =
  let es =
    let open Data_encoding in
    [
      (fun () ->
        mu "foo" (fun e ->
            conv_with_guard
              Option.some
              (Option.to_result ~none:"NONE")
              (option e)));
      (fun () ->
        mu "fool" (fun e -> conv (fun (Foo x) -> x) (fun x -> Foo x) (option e)));
      (fun () ->
        mu "foolish" (fun e ->
            conv
              (fun (Foo x) -> x)
              (fun x -> Foo x)
              (obj1 (req "foolishness" (option e)))));
    ]
  in
  let check f =
    match f () with
    | exception Invalid_argument _ -> ()
    | _ -> failwith "Expected to not terminate but did"
  in
  List.iter check es

let discriminated_option e =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        (Tag 1)
        (obj1 (req "some" e))
        ~title:"Some"
        (fun x -> x)
        (fun x -> Some x);
      case
        (Tag 0)
        (obj1 (req "none" unit))
        ~title:"None"
        (function None -> Some () | Some _ -> None)
        (fun () -> None);
    ]

type assocassoc = Datum of int | Assoc of (string * assocassoc) list

type leftright =
  | Left of leftright * bytes
  | Right of string * leftright
  | Unit

let terminates () =
  let es =
    let open Data_encoding in
    [
      (fun () ->
        mu "foocustom" (fun e ->
            conv (fun (Foo x) -> x) (fun x -> Foo x) (discriminated_option e)));
      (fun () -> mu "funid" Fun.id);
    ]
  in
  let es2 =
    let open Data_encoding in
    [
      (fun () ->
        list
          (mu "foocustom" (fun e ->
               conv (fun (Foo x) -> x) (fun x -> Foo x) (discriminated_option e))));
      (fun () ->
        list
          (tup1
             (mu "foocustom" (fun e ->
                  conv
                    (fun (Foo x) -> x)
                    (fun x -> Foo x)
                    (discriminated_option e)))));
      (fun () ->
        list
          (conv
             (fun x -> ((), x))
             snd
             (tup2
                empty
                (mu "foocustom" (fun e ->
                     conv
                       (fun (Foo x) -> x)
                       (fun x -> Foo x)
                       (discriminated_option e))))));
      (fun () ->
        list
          (mu "foocustom" (fun e ->
               conv
                 (fun x -> ((), x))
                 snd
                 (tup2
                    empty
                    (conv
                       (fun (Foo x) -> x)
                       (fun x -> Foo x)
                       (discriminated_option e))))));
      (fun () -> mu "funid" Fun.id);
    ]
  in
  let es3 =
    let open Data_encoding in
    [
      (fun () ->
        mu "assocassoc" (fun e ->
            union
              [
                case
                  (Tag 0)
                  ~title:"Datum"
                  uint8
                  (function Datum i -> Some i | _ -> None)
                  (fun i -> Datum i);
                case
                  (Tag 1)
                  ~title:"Assoc"
                  (assoc e)
                  (function Assoc a -> Some a | _ -> None)
                  (fun a -> Assoc a);
              ]));
      (fun () ->
        mu "assocmanual" (fun e ->
            union
              [
                case
                  (Tag 0)
                  ~title:"Datum"
                  uint8
                  (function Datum i -> Some i | _ -> None)
                  (fun i -> Datum i);
                case
                  (Tag 1)
                  ~title:"Assoc"
                  (list (tup2 string e))
                  (function Assoc a -> Some a | _ -> None)
                  (fun a -> Assoc a);
              ]));
      (fun () ->
        mu "assocmanual" (fun e ->
            union
              [
                case
                  (Tag 0)
                  ~title:"Datum"
                  uint8
                  (function Datum i -> Some i | _ -> None)
                  (fun i -> Datum i);
                case
                  (Tag 1)
                  ~title:"Assoc"
                  (list (tup2 (Bounded.string 22) e))
                  (function Assoc a -> Some a | _ -> None)
                  (fun a -> Assoc a);
              ]));
    ]
  in
  let es4 =
    let open Data_encoding in
    [
      (fun () ->
        mu "rightleft" (fun e ->
            union
              [
                case
                  (Tag 0)
                  ~title:"Left"
                  (obj2 (req "leftright" e) (req "right" (Bounded.bytes 99)))
                  (function Left (lr, i) -> Some (lr, i) | _ -> None)
                  (fun (lr, i) -> Left (lr, i));
                case
                  (Tag 1)
                  ~title:"Right"
                  (obj2 (req "left" (Bounded.string 4)) (req "leftright" e))
                  (function Right (i, lr) -> Some (i, lr) | _ -> None)
                  (fun (i, lr) -> Right (i, lr));
                case
                  (Tag 2)
                  ~title:"Unit"
                  unit
                  (function Unit -> Some () | _ -> None)
                  (fun () -> Unit);
              ]));
    ]
  in
  let check f = ignore @@ f () in
  List.iter check es ;
  List.iter check es2 ;
  List.iter check es3 ;
  List.iter check es4

let tests =
  [
    ("points", `Quick, points);
    ("tiny", `Quick, tiny_test);
    ("flip-flop", `Quick, flip_flop);
    ("big", `Quick, big_test);
    ("doesnt_terminate", `Quick, doesnt_terminate);
    ("terminates", `Quick, terminates);
  ]
