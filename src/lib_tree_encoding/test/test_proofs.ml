(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs  <contact@nomadic-labs.com>               *)
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
    Component:    Tree_encoding
    Invocation:   dune exec src/lib_tree_encoding/test/test_tree_encoding.exe \
                  -- test "^Proofs$"
    Subject:      Proof-related tests for the tree-encoding library
*)

open Tztest
open Tezos_lazy_containers
module Tree_encoding = Test_encoding.Tree_encoding
module Context = Test_encoding.Context
module Vector = Lazy_vector.IntVector

type t = int Vector.t * int Vector.t

let int_encoding = Tree_encoding.value [] Data_encoding.int31

let encoding =
  let open Tree_encoding in
  tup3
    ~flatten:false
    (Lazy_vector.IntVector.encoding int_encoding int_encoding)
    (Lazy_vector.IntVector.encoding int_encoding int_encoding)
    (Lazy_vector.IntVector.encoding int_encoding int_encoding)

let proof_size proof =
  Data_encoding.Binary.length
    Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V1.Tree2
    .tree_proof_encoding
    proof

let assert_proof_size proof max_size = assert (proof_size proof < max_size)

let prepare_context () =
  let open Lwt_syntax in
  let+ index = Context.init "/tmp" in
  let context = Context.empty index in
  let tree = Context.Tree.empty context in
  (context, tree)

let produce_proof context tree k =
  let open Lwt_syntax in
  let* context = Context.add_tree context [] tree in
  let* _hash = Context.commit ~time:Time.Protocol.epoch context in
  let index = Context.index context in
  (* produce a proof for the next steps *)
  let+ proof, _ =
    match Context.Tree.kinded_key tree with
    | Some key -> Context.produce_tree_proof index key k
    | None ->
        Stdlib.failwith "could not produce the inputs of produce_tree_proof"
  in
  proof

let make_vector f size =
  List.fold_left (fun v x -> Vector.set x (f x) v) (Vector.create size)
  @@ Stdlib.List.init size Fun.id

let test_move_subtrees () =
  let open Lwt_syntax in
  (* initializing large data *)
  let v1 = make_vector (fun x -> x) 1_000 in
  let v2 = make_vector (fun x -> x * 1_000) 2_000 in
  let v3 = make_vector (fun x -> x - 20) 2_000 in
  let* context, tree = prepare_context () in
  (* encoding *)
  let* tree = Tree_encoding.encode encoding (v1, v2, v3) tree in
  (* commit tree *)
  let* proof =
    produce_proof context tree (fun tree ->
        (* decoding *)
        let* v1, v2, v3 = Tree_encoding.decode encoding tree in
        (* swap, encode *)
        let+ tree' = Tree_encoding.encode encoding (v3, v1, v2) tree in
        (tree', ()))
  in
  (* This constant is set arbitrarily low. It’s basically a budget for
     a dozens of 32-byte hashes, which should be more than enough to read three
     lazy vectors in a tree. *)
  assert_proof_size proof 400 ;
  (* We check that we can experimentally conclude that the proof size
     cost of accessing keys in the swapped vectors is linear in the
     number of access. *)
  let set_keys n v =
    List.fold_left
      (fun v x -> Vector.set x (x + 1) v)
      v
      (Stdlib.List.init n Fun.id)
  in
  (* Proof with reading the complete list (setting a key on top of
     that does not change a thing). *)
  let* big_proof =
    produce_proof context tree (fun tree ->
        (* decoding *)
        let* v1, v2, v3 = Tree_encoding.decode encoding tree in
        let* l = Vector.to_list v1 in
        let v1 = Vector.of_list l in
        (* swap, encode *)
        let+ tree' = Tree_encoding.encode encoding (v3, v1, v2) tree in
        (tree', ()))
  in
  let check_proof_size n =
    (* Proof with swap *)
    let* small_proof =
      produce_proof context tree (fun tree ->
          (* decoding *)
          let* v1, v2, v3 = Tree_encoding.decode encoding tree in
          let v1 = set_keys n v1 in
          (* swap, encode *)
          let+ tree' = Tree_encoding.encode encoding (v3, v1, v2) tree in
          (tree', ()))
    in
    (* We check an experimentally defined law: 350 is the cost of
       reading the three vectors info (first_key, num_elements), and
       500 is the cost of accessing one element in the vector. *)
    assert_proof_size small_proof (350 + (n * 500)) ;
    (* We check that  *)
    assert (proof_size small_proof < proof_size big_proof) ;
    Lwt.return ()
  in
  let* () = check_proof_size 2 in
  let* () = check_proof_size 10 in
  let* () = check_proof_size 100 in

  Lwt.return_ok ()

let test_move_and_read_subtrees () =
  let open Lwt_syntax in
  let v = make_vector (fun x -> x) 5_000 in
  let* context, tree = prepare_context () in
  let from_encoding =
    Tree_encoding.(
      scope ["from"] (Lazy_vector.IntVector.encoding int_encoding int_encoding))
  in
  let to_encoding =
    Tree_encoding.(
      scope ["to"] (Lazy_vector.IntVector.encoding int_encoding int_encoding))
  in
  let* tree = Tree_encoding.encode from_encoding v tree in
  let* proof =
    produce_proof context tree (fun tree ->
        let* v = Tree_encoding.decode from_encoding tree in
        let v = Vector.set 0 30 v in
        let+ tree' = Tree_encoding.encode to_encoding v tree in
        (tree', ()))
  in
  (* This constant has been set by experimenting. It encompasses
     reading three values in the tree, one in a dense vector. This is
     still way smaller than recreating the subtree from scratch. *)
  assert_proof_size proof 800 ;
  Lwt.return_ok ()

(* Ensure that copying arbitrary subtrees is safe w.r.t. proof size. *)
let test_copy_subtrees () =
  let open Lwt_syntax in
  let get_proof vec_size =
    let v = make_vector (fun x -> x) vec_size in
    let* context, tree = prepare_context () in
    let vec_encoding =
      Lazy_vector.IntVector.encoding int_encoding int_encoding
    in
    let* tree =
      Tree_encoding.encode
        (Tree_encoding.scope ["from"; "until"; "one"] vec_encoding)
        v
        tree
    in
    let* tree =
      Tree_encoding.encode
        (Tree_encoding.scope ["from"; "until"; "two"] vec_encoding)
        v
        tree
    in
    produce_proof context tree (fun tree ->
        let* v = Tree_encoding.decode Tree_encoding.wrapped_tree tree in
        let* x = Tree_encoding.Wrapped.length v ["from"; "until"] in
        assert (x = 2) ;
        let* y = Tree_encoding.Wrapped.length v ["to"] in
        assert (y = 0) ;
        let* s = Tree_encoding.Wrapped.find_tree v ["from"; "until"] in
        let s = match s with Some s -> s | None -> assert false in
        let* v' = Tree_encoding.Wrapped.add_tree v ["to"; "gether"] s in
        let* x = Tree_encoding.Wrapped.length v' ["from"; "until"] in
        assert (x = 2) ;
        let* y = Tree_encoding.Wrapped.length v' ["to"] in
        assert (y = 1) ;
        let+ tree' = Tree_encoding.encode Tree_encoding.wrapped_tree v' tree in
        (tree', ()))
  in
  let* proof_large_vec = get_proof 10_000 in
  let* proof_small_vec = get_proof 10 in
  let plarge_size = proof_size proof_large_vec in
  let psmall_size = proof_size proof_small_vec in

  (* The proof size should not depend on the size of the tree. *)
  assert (plarge_size = psmall_size) ;
  (* This constant has been set by experimenting. It encompasses
     two paths and a hash. *)
  assert_proof_size proof_large_vec 200 ;
  Lwt.return_ok ()

let tests =
  [
    tztest "Move subtrees" `Quick test_move_subtrees;
    tztest "Decode, set, and move subtree" `Quick test_move_and_read_subtrees;
    tztest "Copy subtrees" `Quick test_copy_subtrees;
  ]
