(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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
    Component:  Protocol (skip lists)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/unit/main.exe \
                  -- --file test_skip_list_repr.ml
    Subject:    Test skip list implementation
*)

open Protocol

exception Skip_list_test_error of string

let err x = Exn (Skip_list_test_error x)

(*

   In this test, we check that [best_basis] should be used to optimize
   the size of Merkel proofs based on skip lists. In such a context,
   the skip lists are referenced by Blake2B hashes (32 bytes-long) and
   their contents are also Blake2B hashes. Besides, a Merkel proof is
   a list of cells.

   To that end, we consider a deterministic sample of pairs [(n,
   target)] when [n] is the length of the skip list and [target] is
   the index of a cell in the skip list. Then, for each basis in [2
   .. max_basis] distinct from [best_basis], we check that the largest
   proof is larger than the largest proof of [best_basis].

*)
let test_skip_list_proof_size () =
  let module H = Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash in
  (*

     Basis [4] is very close to [3] as the best basis... therefore, we
     use a fixed seed for the random number generator to avoid any
     flakiness in the test.

  *)
  let () = Random.init 0x0ACAB0 in
  let best_basis = 4 in

  (*

     For the CI, we use relatively small values to avoid slowdowns.

     We choose [max_length] to be of the same order of magnitude as
     the longest lists we can meet in practice in the smart rollups
     inboxes.

     The real [max_length] can be found in [constants_repr.ml]. At the time
     of writing this message, the value is [1_000_000].

  *)
  let max_basis = 7 and nsample = 2048 and max_length = 100_000 in

  (*

     Locally, one can we use large values to get higher confidence:

   *)
  (* let max_basis = 13 and nsample = 4096 and max_length = 200_000 in *)

  (* A sample is a pair [(n, k)] where [k <= n]. *)
  let samples =
    let get () =
      let n = 1 + Random.int (max_length - 1) in
      let target = Random.int (1 + n) in
      (n, Z.of_int target)
    in
    let rec aux r n = if n = 0 then r else aux (get () :: r) (n - 1) in
    aux [] nsample
  in

  (*

     For a given [basis], we compute the largest proof when processing
     [samples]. The considered lists hold the same contents in each cell,
     allowing us to store lists of size [k] in a cache.

  *)
  let largest_proof basis =
    let module M = Skip_list.Make (struct
      let basis = basis
    end) in
    let cell_encoding = M.encoding H.encoding H.encoding in
    let proof_encoding = Data_encoding.list cell_encoding in
    let hash_cell cell =
      let payload_hash = M.content cell in
      let back_pointers_hashes = M.back_pointers cell in
      H.to_bytes payload_hash :: List.map H.to_bytes back_pointers_hashes
      |> H.hash_bytes
    in
    let dummy_content = H.hash_string ["HumptyDumpty"] in
    let cache =
      let cache = Stdlib.Hashtbl.create 13 in
      let rec make_list k n map prev_cell =
        if n = k then
          let prev_cell_ptr = hash_cell prev_cell in
          let map = H.Map.add prev_cell_ptr prev_cell map in
          Stdlib.Hashtbl.add cache k (map, prev_cell_ptr)
        else
          let prev_cell_ptr = hash_cell prev_cell in
          let next_cell = M.next ~prev_cell ~prev_cell_ptr dummy_content in
          let map = H.Map.add prev_cell_ptr prev_cell map in
          Stdlib.Hashtbl.add cache k (map, prev_cell_ptr) ;
          make_list (succ k) n map next_cell
      in
      make_list 0 max_length H.Map.empty (M.genesis dummy_content) ;
      cache
    in
    let proof_of_path deref =
      List.map (fun ptr -> Stdlib.Option.get (deref ptr))
    in
    let proof_size (n, target_index) =
      let make_list n = Stdlib.Hashtbl.find cache n in
      let map, cell_ptr = make_list n in
      let deref ptr = H.Map.find ptr map in
      let path =
        Stdlib.Option.get @@ M.back_path ~deref ~cell_ptr ~target_index
      in
      let proof = proof_of_path deref path in
      let encoded_proof =
        Data_encoding.Binary.to_bytes_exn proof_encoding proof
      in
      Bytes.length encoded_proof
    in
    List.map proof_size samples |> List.fold_left max min_int
  in
  let largest_proofs =
    List.map (fun basis -> (basis, largest_proof basis)) (2 -- max_basis)
  in
  let () =
    List.iter
      (fun (b, p) ->
        Format.eprintf "@[Basis = %d,@, Largest proof = %d@]@;" b p)
      largest_proofs
  in
  let smallest_largest_proofs_basis, _ =
    List.fold_left
      (fun (b1, p1) (b2, p2) -> if p1 < p2 then (b1, p1) else (b2, p2))
      (Stdlib.List.hd largest_proofs)
      (Stdlib.List.tl largest_proofs)
  in
  fail_unless
    (smallest_largest_proofs_basis = best_basis)
    (err
       (Format.asprintf
          "According to the test, %d is the best basis, not %d."
          smallest_largest_proofs_basis
          best_basis))

let tests =
  [
    Tztest.tztest
      "Skip list: check if the best basis for merkelized skip list is indeed \
       the best"
      `Quick
      test_skip_list_proof_size;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("skip list", tests)]
  |> Lwt_main.run
