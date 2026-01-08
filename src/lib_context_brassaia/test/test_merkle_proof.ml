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

(* Testing
   -------
   Component:    Context
   Invocation:   dune exec src/lib_context/test/main.exe \
                  -- --file test_merkle_proof.ml
   Subject:      Test merkle proof.
*)

open Tezos_context_disk
open Context.Proof
open Qcheck2_helpers
open QCheck2

module Gen = struct
  include Gen

  let step : step t = string_size ~gen:printable (int_range 3 10)

  let value : value t =
    let+ s = string_size ~gen:char (int_range 3 10) in
    Bytes.unsafe_of_string s

  let hash =
    let+ s = string_size ~gen:char (return Context_hash.size) in
    Context_hash.of_string_exn s

  let rec comb n xs =
    match (n, xs) with
    | 0, _ -> Gen.return []
    | _, [] -> assert false
    | 1, [x] -> Gen.return [x]
    | n, x :: xs ->
        (* prob.  n / length xs *)
        let* m = int_bound (List.length (x :: xs) - 1) in
        if m < n then
          let+ ys = comb (n - 1) xs in
          x :: ys
        else comb n xs
end

module Proof32 (Encoding : Tezos_context_sigs.Context.PROOF_ENCODING) = struct
  module Encoding = Encoding

  module Gen = struct
    include Gen

    let inode width gen_a =
      let* length = int_range 1 1000_000 in
      let* n = int_bound (min 32 (max 5 width) - 1) in
      let n = n + 1 in
      let* indices = comb n (Stdlib.List.init 32 (fun x -> x)) in
      let+ proofs =
        flatten_l
        @@ List.map
             (fun i ->
               let+ a = gen_a in
               (i, a))
             indices
      in
      {length; proofs}

    let inode_extender gen_a =
      let* length = int_range 1 10 in
      let* size = int_bound 5 in
      let size = size + 1 in
      let* segment = list_repeat size (int_bound 4) in
      let+ proof = gen_a in
      {length; segment; proof}

    let rec inode_tree (depth, width) =
      if depth <= 0 then
        let+ hash in
        Blinded_inode hash
      else
        oneof
          [
            (let+ hash in
             Blinded_inode hash);
            (let* size = int_bound 3 in
             let size = size + 1 in
             let+ xs = list_repeat size (pair step (tree (depth - 1, width))) in
             Inode_values xs);
            (let+ i = inode width (inode_tree (depth - 1, width / 2)) in
             Inode_tree i);
            (let+ i = inode_extender (inode_tree (depth - 1, width)) in
             (Inode_extender i : inode_tree));
          ]

    and tree (depth, width) : tree t =
      if depth <= 0 then
        oneof
          [
            (let+ v = value in
             (Value v : tree));
            (let+ h = hash in
             Blinded_value h);
            (let+ h = hash in
             Blinded_node h);
          ]
      else
        oneof
          [
            (let+ v = value in
             (Value v : tree));
            (let+ h = hash in
             Blinded_value h);
            (let* size = int_bound 4 in
             let size = size + 1 in
             let+ xs =
               list_repeat size @@ pair step (tree (depth - 1, width))
             in
             (Node xs : tree));
            (let+ h = hash in
             Blinded_node h);
            (let+ i = inode width (inode_tree (depth - 1, width / 2)) in
             (Inode i : tree));
            (let+ i = inode_extender (inode_tree (depth - 1, width)) in
             Extender i);
          ]

    let kinded_hash =
      let* h = hash in
      oneofl [`Value h; `Node h]

    let tree_proof =
      let* version = int_bound 3 in
      let* kh1 = kinded_hash in
      let* kh2 = kinded_hash in
      let+ state = tree (5, 64) in
      {version; before = kh1; after = kh2; state}

    module Stream = struct
      open Stream

      let elt =
        oneof
          [
            (let+ v = value in
             Value v);
            (let* size = int_bound 4 in
             let size = size + 1 in
             let+ sks = list_repeat size @@ pair step kinded_hash in
             Node sks);
            (let max_indices = 5 in
             let+ i = inode max_indices hash in
             Inode i);
            (let+ i = inode_extender hash in
             Inode_extender i);
          ]

      let t : Stream.t Gen.t =
        let* size = int_bound 10 in
        let size = size + 1 in
        let+ xs = list_repeat size elt in
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
      (* Format.eprintf "%a@." Data_encoding.Json.pp j ; *)
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
            segment = [0; 1; 0; 31];
            proof = Inode_tree {length = 8; proofs = [(0, Blinded_inode ch)]};
          }
      in
      let tree_c =
        Extender
          {
            length = 10;
            segment = [1; 2; 3; 4; 5; 6; 7; 8];
            proof = Inode_values [("z", tree_b)];
          }
      in
      let inode_tree : inode_tree =
        Inode_values [("a", tree_a); ("b", tree_b); ("c", tree_c)]
      in
      let tree = Inode {length = 100; proofs = [(0, inode_tree)]} in
      {version = 1; before = `Value ch; after = `Node ch; state = tree}
    in
    assert (encoding_test Encoding.tree_proof_encoding ( = ) sample) ;

    let sample_large_inode_tree inode_proof_size =
      let bytes s = Bytes.of_string s in
      let tree_a c : tree = Value (bytes @@ "a" ^ c) in
      let ch =
        Context_hash.of_bytes_exn (bytes "01234567890123456789012345678901")
      in
      let inode_tree i : inode_tree =
        let c = string_of_int i in
        Inode_values [(c ^ "a", tree_a c)]
      in
      let proofs =
        let rec aux i acc =
          if i >= inode_proof_size then acc
          else aux (i + 1) ((i * 15 mod 32, inode_tree i) :: acc)
        in
        List.sort (fun (i, _) (j, _) -> compare i j) (aux 0 [])
      in
      let tree = Inode {length = 100; proofs} in
      {version = 1; before = `Value ch; after = `Node ch; state = tree}
    in
    assert (
      encoding_test
        Encoding.tree_proof_encoding
        ( = )
        (sample_large_inode_tree 20)) ;
    assert (
      encoding_test
        Encoding.tree_proof_encoding
        ( = )
        (sample_large_inode_tree 32))

  let test_random_tree_proof =
    QCheck2.Test.make
      ~name:"tree_proof_encoding"
      Gen.tree_proof
      (encoding_test Encoding.tree_proof_encoding ( = ))

  let test_random_stream_proof =
    QCheck2.Test.make
      ~name:"stream_proof_encoding"
      Gen.stream_proof
      (encoding_test
         Encoding.stream_proof_encoding
         (fun
           (* stream proof uses Seq.t *)
             a
           b
         ->
           a.version = b.version && a.before = b.before && a.after = b.after
           && List.of_seq a.state = List.of_seq b.state))

  let tests_random = [test_random_tree_proof; test_random_stream_proof]
end

module Proof2 (Encoding : Tezos_context_sigs.Context.PROOF_ENCODING) = struct
  module Encoding = Encoding

  module Gen = struct
    include Gen

    let inode _width gen_a =
      let* length = int_range 1 1000_000 in
      let* n = int_bound 1 in
      let n = n + 1 in
      let* indices = comb n [0; 1] in
      let+ proofs =
        flatten_l
        @@ List.map
             (fun i ->
               let+ a = gen_a in
               (i, a))
             indices
      in
      {length; proofs}

    let inode_extender gen_a =
      let* length = int_range 1 10 in
      let* size = int_bound 5 in
      let size = size + 1 in
      let* segment = list_repeat size (int_bound 1) in
      let+ proof = gen_a in
      {length; segment; proof}

    let rec inode_tree (depth, width) =
      if depth <= 0 then
        let+ hash in
        Blinded_inode hash
      else
        oneof
          [
            (let+ hash in
             Blinded_inode hash);
            (let* size = int_bound 3 in
             let size = size + 1 in
             let+ xs = list_repeat size (pair step (tree (depth - 1, width))) in
             Inode_values xs);
            (let+ i = inode width (inode_tree (depth - 1, width / 2)) in
             Inode_tree i);
            (let+ i = inode_extender (inode_tree (depth - 1, width)) in
             (Inode_extender i : inode_tree));
          ]

    and tree (depth, width) : tree t =
      if depth <= 0 then
        oneof
          [
            (let+ v = value in
             (Value v : tree));
            (let+ h = hash in
             Blinded_value h);
            (let+ h = hash in
             Blinded_node h);
          ]
      else
        oneof
          [
            (let+ v = value in
             (Value v : tree));
            (let+ h = hash in
             Blinded_value h);
            (let* size = int_bound 4 in
             let size = size + 1 in
             let+ xs =
               list_repeat size @@ pair step (tree (depth - 1, width))
             in
             (Node xs : tree));
            (let+ h = hash in
             Blinded_node h);
            (let+ i = inode width (inode_tree (depth - 1, width / 2)) in
             (Inode i : tree));
            (let+ i = inode_extender (inode_tree (depth - 1, width)) in
             Extender i);
          ]

    let kinded_hash =
      let* h = hash in
      oneofl [`Value h; `Node h]

    let tree_proof =
      let* version = int_bound 3 in
      let* kh1 = kinded_hash in
      let* kh2 = kinded_hash in
      let+ state = tree (5, 64) in
      {version; before = kh1; after = kh2; state}

    module Stream = struct
      open Stream

      let elt =
        oneof
          [
            (let+ v = value in
             Value v);
            (let* size = int_bound 4 in
             let size = size + 1 in
             let+ sks = list_repeat size @@ pair step kinded_hash in
             Node sks);
            (let max_indices = 5 in
             let+ i = inode max_indices hash in
             Inode i);
            (let+ i = inode_extender hash in
             Inode_extender i);
          ]

      let t : Stream.t Gen.t =
        let* size = int_bound 10 in
        let size = size + 1 in
        let+ xs = list_repeat size elt in
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
      (* Format.eprintf "%a@." Data_encoding.Json.pp j ; *)
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
            proof =
              Inode_tree
                {
                  length = 8;
                  proofs = [(0, Blinded_inode ch); (1, Blinded_inode ch)];
                };
          }
      in
      let tree_c =
        Extender
          {
            length = 10;
            segment = [0; 1; 0; 1; 0; 1; 0; 1];
            proof = Inode_values [("z", tree_b)];
          }
      in
      let inode_tree : inode_tree =
        Inode_values [("a", tree_a); ("b", tree_b); ("c", tree_c)]
      in
      let tree =
        Inode {length = 100; proofs = [(0, inode_tree); (1, inode_tree)]}
      in
      {version = 1; before = `Value ch; after = `Node ch; state = tree}
    in
    assert (encoding_test Encoding.tree_proof_encoding ( = ) sample) ;

    let sample_large_inode_tree =
      let bytes s = Bytes.of_string s in
      let tree_a c : tree = Value (bytes @@ "a" ^ c) in
      let ch =
        Context_hash.of_bytes_exn (bytes "01234567890123456789012345678901")
      in
      let inode_tree i : inode_tree =
        let c = string_of_int i in
        Inode_values [(c ^ "a", tree_a c)]
      in
      let proofs = [(0, inode_tree 0); (1, inode_tree 1)] in
      let tree = Inode {length = 100; proofs} in
      {version = 1; before = `Value ch; after = `Node ch; state = tree}
    in
    assert (
      encoding_test Encoding.tree_proof_encoding ( = ) sample_large_inode_tree)

  let test_random_tree_proof =
    QCheck2.Test.make
      ~name:"tree_proof_encoding"
      Gen.tree_proof
      (encoding_test Encoding.tree_proof_encoding ( = ))

  let test_random_stream_proof =
    QCheck2.Test.make
      ~name:"stream_proof_encoding"
      Gen.stream_proof
      (encoding_test
         Encoding.stream_proof_encoding
         (fun
           (* stream proof uses Seq.t *)
             a
           b
         ->
           a.version = b.version && a.before = b.before && a.after = b.after
           && List.of_seq a.state = List.of_seq b.state))

  let tests_random = [test_random_tree_proof; test_random_stream_proof]
end

module Proof32_V1 = struct
  include
    Proof32 (Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V1.Tree32)

  let expected_encoding_size :
      tree Tezos_context_sigs.Context.Proof_types.t -> int =
    let map_sum f l = List.fold_left (fun s e -> s + f e) 0 l in
    let tag_len = 1 in
    let int_len = 4 in
    let int16_len = 2 in
    let index_len = 1 in
    let length_len = 8 in
    let step_len s =
      (* length of string (< 255) + step *)
      1 + Bytes.length (Bytes.unsafe_of_string s)
    in
    let hash_len = 32 in
    let value_len v = int_len + Bytes.length v in
    let kinded_hash_len = tag_len + hash_len in
    let segment_len seg =
      (* length of bytes (< 255) + segment *)
      let len = List.length seg in
      1 + (((len * 5) + 8) / 8)
    in
    let rec tree_len : tree -> int =
     fun tree ->
      tag_len
      +
      match tree with
      | Value v -> value_len v
      | Blinded_value _ -> hash_len
      | Node trees ->
          (* length of trees + (step * tree) list *)
          int_len + map_sum (fun (s, t) -> step_len s + tree_len t) trees
      | Blinded_node _ -> hash_len
      | Inode inode -> inode_len inode
      | Extender ext -> inode_extender_len ext
    and inode_extender_len : inode_tree inode_extender -> int =
     fun {length = _; segment; proof} ->
      length_len + segment_len segment + inode_tree_len proof
    and inode_len : inode_tree inode -> int =
     fun {length = _; proofs} ->
      let nb_entries = Tezos_context_encoding.Context.Conf.nb_entries in
      if List.length proofs < nb_entries / 2 then
        (* length + sparse tag + length of proofs + proofs *)
        length_len + tag_len + int_len
        + map_sum (fun (_, it) -> index_len + inode_tree_len it) proofs
      else
        (* length + dense tag + inode_trees (with none) *)
        length_len + tag_len + nb_entries
        + map_sum
            (fun it ->
              (* `inode_tree` tag is counted in the `entries` above *)
              -1 + inode_tree_len it)
            (List.map snd proofs)
    and inode_tree_len : inode_tree -> int =
     fun inode_tree ->
      tag_len
      +
      match inode_tree with
      | Blinded_inode _ -> hash_len
      | Inode_values trees ->
          (* length of trees + (step * tree) list *)
          int_len + map_sum (fun (s, t) -> step_len s + tree_len t) trees
      | Inode_tree inode -> inode_len inode
      | Inode_extender ext -> inode_extender_len ext
    in
    fun {version = _; before = _; after = _; state} ->
      int16_len + kinded_hash_len + kinded_hash_len + tree_len state

  let encoding_test_with_size enc eq data =
    assert (encoding_test enc eq data) ;
    let size = expected_encoding_size data in
    let l = Bytes.length @@ Data_encoding.Binary.to_bytes_exn enc data in
    Format.eprintf "expected :: %d | actual :: %d@." size l ;
    l = size

  let test_random_tree_proof =
    let encoding_test = encoding_test_with_size in
    QCheck2.Test.make
      ~name:"tree_proof_encoding"
      Gen.tree_proof
      (encoding_test Encoding.tree_proof_encoding ( = ))

  let tests_random = [test_random_tree_proof; test_random_stream_proof]
end

let () =
  let module Proof2_V1 =
    Proof2 (Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V1.Tree2) in
  let module Proof32_V2 =
    Proof32 (Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree32) in
  let module Proof2_V2 =
    Proof2 (Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree2) in
  Alcotest.run
    ~__FILE__
    "test_merkle_proof"
    [
      ("sample32_v1", [("sample", `Quick, Proof32_V1.test_sample)]);
      ("random32_v1", qcheck_wrap Proof32_V1.tests_random);
      ("sample2_v1", [("sample", `Quick, Proof2_V1.test_sample)]);
      ("random2_v1", qcheck_wrap Proof2_V1.tests_random);
      ("sample32_v2", [("sample", `Quick, Proof32_V2.test_sample)]);
      ("random32_v2", qcheck_wrap Proof32_V2.tests_random);
      ("sample2_v2", [("sample", `Quick, Proof2_V2.test_sample)]);
      ("random2_v2", qcheck_wrap Proof2_V2.tests_random);
    ]
