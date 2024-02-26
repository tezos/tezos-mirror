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
    Component:  Protocol (Merkle list)
    Invocation: dune exec src/proto_019_PtParisA/lib_protocol/test/unit/main.exe \
                  -- --file test_merkle_list.ml
    Subject:    test the ad-hoc merkle tree structure implemented to encode lists
*)

open Merkle_list_helper

let assert_invalid_pos : 'a Environment.Error_monad.tzresult -> _ = function
  | Error err ->
      let expected_error_msg msg = "Error:\n  " ^ msg ^ "\n" in
      let actual_error_msg : string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace err
      in
      Log.debug "%s\n" actual_error_msg ;
      assert (
        expected_error_msg "Merkle_list_invalid_position" = actual_error_msg)
  | _ -> assert false

(* Check that the result of [compute] is the expected hash *)
let test_compute () =
  let open Error_monad.Result_syntax in
  let elements =
    Stdlib.List.init 5 (fun i -> Bytes.of_string (Int.to_string i))
  in
  let element_hashes = List.map (fun e -> Hash.hash_bytes [e]) elements in
  let el_hashes_a = Array.of_list element_hashes in
  let h01 = hash2 el_hashes_a.(0) el_hashes_a.(1) in
  let h23 = hash2 el_hashes_a.(2) el_hashes_a.(3) in
  let h4e = hash2 el_hashes_a.(4) empty in
  let h03 = hash2 h01 h23 in
  let h4ee = hash2 h4e empty in
  let expected_root = hash2 h03 h4ee in
  assert (Hash.equal (compute elements) expected_root) ;
  return_unit

(* Compare the root of a tree constructed by snoc'ing to the value
   given by compute and the values of the leaves *)
let test_snoc () =
  let open Error_monad.Result_syntax in
  let n = 20 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in

  let t = List.fold_left snoc_tr nil elements in
  let element_hashes = List.map (fun e -> Hash.hash_bytes [e]) elements in

  assert (Hash.equal (compute elements) (root t)) ;
  assert (element_hashes = Internal_for_tests.to_list t) ;
  return_unit

(* Compare the result of the two versions of snoc *)
let test_snoc_non_tr () =
  let open Error_monad.Result_syntax in
  let n = 20 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in
  let t1 = List.fold_left snoc_tr nil elements in
  let t2 = List.fold_left snoc nil elements in
  assert (Internal_for_tests.equal t1 t2) ;
  return_unit

(* Check that the path computed is the expected one *)
let test_compute_path () =
  let open Error_monad.Result_syntax in
  let elements =
    Stdlib.List.init 5 (fun i -> Bytes.of_string (Int.to_string i))
  in
  let t = List.fold_left snoc_tr nil elements in

  let element_hashes = List.map (fun e -> Hash.hash_bytes [e]) elements in
  let el_hashes_a = Array.of_list element_hashes in
  let h23 = hash2 el_hashes_a.(2) el_hashes_a.(3) in
  let h4e = hash2 el_hashes_a.(4) empty in
  let h4ee = hash2 h4e empty in
  let expected_path_for_1 = [el_hashes_a.(0); h23; h4ee] in
  let* path = compute_path t 1 in
  assert (Internal_for_tests.path_to_list path = expected_path_for_1) ;
  return_unit

(* Negative test: pos < 0  *)
let test_compute_path_negative_pos () =
  let open Error_monad.Result_syntax in
  let n = 10 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in
  let t = List.fold_left snoc_tr nil elements in
  assert_invalid_pos @@ compute_path t (-1) ;
  return_unit

(* Negative test: pos >= size tree *)
let test_compute_path_out_of_bounds () =
  let open Error_monad.Result_syntax in
  let n = 20 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in
  let t = List.fold_left snoc_tr nil elements in
  assert_invalid_pos @@ compute_path t n ;
  return_unit

(* Negative test: pos = size tree, when tree is full *)
let test_compute_path_out_of_bounds_full () =
  let open Error_monad.Result_syntax in
  let n = 4 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in
  let t = List.fold_left snoc_tr nil elements in
  assert_invalid_pos @@ compute_path t n ;
  return_unit

(* Check that a computed root (from [check_path]) is the actual root *)
let test_check_path () =
  let open Error_monad.Result_syntax in
  let n = 20 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in
  let elements_array = Array.of_list elements in
  let t = List.fold_left snoc_tr nil elements in
  Stdlib.List.init n (fun pos ->
      let* path = compute_path t pos in
      let* b = check_path path pos elements_array.(pos) (ML.root t) in
      assert b ;
      return_unit)
  |> Environment.Error_monad.Result_syntax.tzjoin

(* Check that a path is only valid for the position for which it
   was computed *)
let test_check_path_wrong_pos () =
  let open Error_monad.Result_syntax in
  let n = 20 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in
  let elements_array = Array.of_list elements in
  let t = List.fold_left snoc_tr ML.nil elements in
  let* path = compute_path t (n - 1) in
  Stdlib.List.init (n - 2) (fun pos ->
      let* b = check_path path pos elements_array.(pos) (ML.root t) in
      assert (not b) ;
      return_unit)
  |> Environment.Error_monad.Result_syntax.tzjoin

(* Check that a computed path is invalidated by a tree update  *)
let test_check_invalidated_path () =
  let open Error_monad.Result_syntax in
  let n = 20 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in
  let new_el = Bytes.of_string (Int.to_string n) in
  let t = List.fold_left snoc_tr ML.nil elements in
  let* path = compute_path t 0 in
  let t = snoc_tr t new_el in
  let* b = check_path path 0 (Stdlib.List.hd elements) (ML.root t) in
  assert (not b) ;
  return_unit

(* Negative test: pos < 0 in [check_path] *)
let test_check_path_negative_pos () =
  let open Error_monad.Result_syntax in
  let n = 20 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in
  let elements_array = Array.of_list elements in
  let t = List.fold_left snoc_tr nil elements in
  let pos = Random.int n in
  let* path = compute_path t pos in
  assert_invalid_pos @@ check_path path (-1) elements_array.(pos) (ML.root t) ;
  return_unit

(* Negative test: pos >= 2^depth in [check_path] *)
let test_check_path_out_of_bounds () =
  let open Error_monad.Result_syntax in
  let n = 20 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in
  let elements_array = Array.of_list elements in
  let t = List.fold_left snoc_tr nil elements in
  let pos = Random.int n in
  let* path = compute_path t pos in
  (* NB: for this to be actually invalid, it is not enough to pass
     a position larger than [n]. We need pos >= 2^depth. *)
  assert_invalid_pos @@ check_path path 32 elements_array.(pos) (ML.root t) ;
  return_unit

(* Encoding roundtrip *)
let test_path_encoding () =
  let open Error_monad.Result_syntax in
  let n = 20 in
  let elements =
    Stdlib.List.init n (fun i -> Bytes.of_string (Int.to_string i))
  in
  let t = List.fold_left snoc_tr nil elements in
  let pos = n / 2 in
  let* path = compute_path t pos in
  let b = Data_encoding.Binary.to_bytes_exn path_encoding path in
  let path' = Data_encoding.Binary.of_bytes_exn path_encoding b in
  assert (path' = path) ;
  return_unit

let valid_tests =
  [
    ("compute", test_compute);
    ("snoc", test_snoc);
    ("snoc_non_tr", test_snoc_non_tr);
    ("compute_path", test_compute_path);
    ("check_path", test_check_path);
    ("path_encoding", test_path_encoding);
    ("compute_path_negative_pos", test_compute_path_negative_pos);
    ("compute_path_out_of_bounds", test_compute_path_out_of_bounds);
    ("check_path_negative_pos", test_check_path_negative_pos);
    ("check_path_out_of_bounds", test_check_path_out_of_bounds);
    ("compute_path_out_of_bounds_full", test_compute_path_out_of_bounds_full);
    ("check_path_wrong_pos", test_check_path_wrong_pos);
    ("check_invalidated_path", test_check_invalidated_path);
  ]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      match f () with Ok () -> Lwt.return_unit | Error _ -> assert false)

let tests = List.map wrap valid_tests

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("merkle list", tests)]
  |> Lwt_main.run
