(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

open Context.Proof
open Lib_test.Qcheck2_helpers
open QCheck2

module Store = struct
  open Tezos_context_encoding.Context
  include
    Irmin_pack.Make_ext (Irmin_pack.Version.V1) (Conf) (Node) (Commit)
      (Metadata)
      (Contents)
      (Path)
      (Branch)
      (Hash)
end

module Proof = Tezos_context_helpers.Context.Make_proof (Store)
open Proof

module Gen = struct
  include Gen

  let step : step t = string_size ~gen:printable (int_range 3 10)

  let value : value t =
    let+ s = string_size ~gen:char (int_range 3 10) in
    Bytes.unsafe_of_string s

  let hash =
    let+ s = string_size ~gen:char (return Context_hash.size) in
    Context_hash.of_string_exn s

  let inode gen_a =
    let* length = int_range 1 10 in
    let+ proofs =
      list_size (int_bound 3 >|= ( + ) 1) (pair (int_bound 10) gen_a)
    in
    (* no invariant assurance at all :-P *)
    {length; proofs}

  let inode_extender gen_a =
    let* length = int_range 1 10 in
    let* segment = list_size (int_bound 5 >|= ( + ) 1) (int_bound 4) in
    let+ proof = gen_a in
    {length; segment; proof}

  let rec inode_tree depth =
    if depth <= 0 then
      let+ hash = hash in
      Blinded_inode hash
    else
      int_bound 3 >>= function
      | 0 ->
          let+ hash = hash in
          Blinded_inode hash
      | 1 ->
          let+ xs =
            list_size
              (int_bound 3 >|= ( + ) 1)
              (pair step (tree (depth - 1)))
          in
          Inode_values xs
      | 2 ->
          let+ i = inode (inode_tree (depth - 1)) in
          Inode_tree i
      | 3 ->
          let+ i = inode_extender (inode_tree (depth - 1)) in
          (Inode_extender i : inode_tree)
      | _ -> assert false

  and tree depth : tree t =
    if depth <= 0 then
      int_bound 2 >>= function
      | 0 ->
          let+ v = value in
          (Value v : tree)
      | 1 ->
          let+ h = hash in
          Blinded_value h
      | 2 ->
          let+ h = hash in
          Blinded_node h
      | _ -> assert false
    else
      int_bound 5 >>= function
      | 0 ->
          let+ v = value in
          (Value v : tree)
      | 1 ->
          let+ h = hash in
          Blinded_value h
      | 2 ->
          let+ xs =
            list_size (int_bound 4 >|= ( + ) 1)
            @@ pair step (tree (depth - 1))
          in
          (Node xs : tree)
      | 3 ->
          let+ h = hash in
          Blinded_node h
      | 4 ->
          let+ i = inode (inode_tree (depth - 1)) in
          (Inode i : tree)
      | 5 ->
          let+ i = inode_extender (inode_tree (depth - 1)) in
          Extender i
      | _ -> assert false

  let kinded_hash =
    let* h = hash in
    bool >|= function true -> `Value h | false -> `Node h

  let tree_proof =
    let* version = int_bound 3 in
    let* kh1 = kinded_hash in
    let* kh2 = kinded_hash in
    let+ state = tree 8 in
    {version; before = kh1; after = kh2; state}

  module Stream = struct
    open Stream

    let elt =
      int_bound 3 >>= function
      | 0 ->
          let+ v = value in
          Value v
      | 1 ->
          let+ sks =
            list_size (int_bound 4 >|= ( + ) 1) @@ pair step kinded_hash
          in
          Node sks
      | 2 ->
          let+ i = inode hash in
          Inode i
      | 3 ->
          let+ i = inode_extender hash in
          Inode_extender i
      | _ -> assert false

    let t : Stream.t Gen.t =
      let+ xs = list_size (int_bound 10 >|= ( + ) 1) elt in
      List.to_seq xs
  end

  let stream_proof =
    let* version = int_bound 3 in
    let* kh1 = kinded_hash in
    let* kh2 = kinded_hash in
    let+ state = Stream.t in
    {version; before = kh1; after = kh2; state}
end

let encoding_test enc eq data =
  let b = Data_encoding.Binary.to_bytes_exn enc data in
  let data' = Data_encoding.Binary.of_bytes_exn enc b in
  if not @@ eq data data' then false
  else
    let j = Data_encoding.Json.construct enc data in
    Format.eprintf "%a@." Data_encoding.Json.pp j ;
    let data' = Data_encoding.Json.destruct enc j in
    eq data data'

let test_sample () =
  let sample =
    let bytes s = Bytes.of_string s in
    let tree_a : tree = Value (bytes "a") in
    let ch =
      Context_hash.of_bytes_exn (bytes "01234567890123456789012345678901")
    in
    let tree_b =
      Extender
        {
          length = 10;
          segment = [0; 1; 0; 1];
          proof = Inode_tree {length = 8; proofs = [(0, Blinded_inode ch)]};
        }
    in
    let inode_tree : inode_tree = Inode_values [("a", tree_a); ("b", tree_b)] in
    let tree = Inode {length = 100; proofs = [(0, inode_tree)]} in
    {version = 1; before = `Value ch; after = `Node ch; state = tree}
  in
  assert (encoding_test tree_proof_encoding ( = ) sample)

let test_random_tree_proof =
  QCheck2.Test.make
    ~name:"tree_proof_encoding"
    Gen.tree_proof
    (encoding_test tree_proof_encoding ( = ))

let test_random_stream_proof =
  QCheck2.Test.make
    ~name:"stream_proof_encoding"
    Gen.stream_proof
    (encoding_test
       stream_proof_encoding
       (fun (* stream proof uses Seq.t *)
              a b ->
         a.version = b.version && a.before = b.before && a.after = b.after
         && List.of_seq a.state = List.of_seq b.state))

let tests_random = [test_random_tree_proof; test_random_stream_proof]

let () =
  Alcotest.run
    "test_merkle_proof"
    [
      ("sample", [("sample", `Quick, test_sample)]);
      ("random", qcheck_wrap tests_random);
    ]
