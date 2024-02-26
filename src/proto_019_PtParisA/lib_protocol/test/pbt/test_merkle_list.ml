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
    Component:    Protocol Library
    Invocation:   dune exec src/proto_019_PtParisA/lib_protocol/test/pbt/main.exe \
                  -- --file test_merkle_list.ml
    Subject:      Tx rollup l2 encoding
*)

open Qcheck2_helpers

(* ------ generators -------------------------------------------------------- *)

let input : bytes list QCheck2.Gen.t =
  let open QCheck2.Gen in
  list_size (2 -- 100) bytes_gen

let valid_pos l =
  let open QCheck2.Gen in
  let* pos = 0 -- (List.length l - 1) in
  return pos

let invalid_pos l =
  let open QCheck2.Gen in
  let* choice = bool in
  let* pos =
    if choice then -20 -- -1
    else
      let len = List.length l in
      len -- (2 * len)
  in
  return pos

let input_and_pos : (int * bytes list) QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* l = input in
  let* pos = valid_pos l in
  return (pos, l)

let input_and_pos_and_wrong_pos : (int * int * bytes list) QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* l = input in
  let* pos = valid_pos l in
  let* wrong_pos = invalid_pos l in
  return (pos, wrong_pos, l)

(* ------ tests ------------------------------------------------------------- *)

let test_scons_scons_tr_equiv ~count =
  let open Merkle_list_helper in
  QCheck2.Test.make ~count ~name:"scons_scons_tr_equiv" input (fun input ->
      let snoc = List.fold_left snoc nil input in
      let snoc_tr = List.fold_left snoc_tr nil input in
      Internal_for_tests.equal snoc snoc_tr)

let test_scons_compute_equiv ~count =
  let open Merkle_list_helper in
  QCheck2.Test.make ~count ~name:"scons_compute_equiv" input (fun input ->
      let snoc = List.fold_left snoc nil input |> root in
      let compute = compute input in
      Hash.equal snoc compute)

let test_of_list_compute_equiv ~count =
  let open Merkle_list_helper in
  QCheck2.Test.make ~count ~name:"of_list_compute_equiv" input (fun input ->
      Hash.equal (root (of_list input)) (compute input))

let test_of_list_snoc_tr_equiv ~count =
  let open Merkle_list_helper in
  QCheck2.Test.make ~count ~name:"of_list_snoc_tr_equiv" input (fun input ->
      let of_list = of_list input in
      let snoc_tr = List.fold_left snoc_tr nil input in
      Merkle_list_helper.Internal_for_tests.equal of_list snoc_tr)

let ok_exn = function Ok x -> x | Error _ -> raise (Invalid_argument "ok_exn")

let test_check_path ~count =
  let open Merkle_list_helper in
  QCheck2.Test.make ~count ~name:"check_path" input_and_pos (fun (pos, input) ->
      let tree = List.fold_left snoc nil input in
      let hash = root tree in
      let path = ok_exn @@ compute_path tree pos in
      ok_exn @@ check_path path pos (Stdlib.List.nth input pos) hash)

let test_check_path_wrong ~count =
  let open Merkle_list_helper in
  QCheck2.Test.make
    ~count
    ~name:"check_path_wrong"
    input_and_pos_and_wrong_pos
    (fun (pos, wrong_pos, input) ->
      let tree = List.fold_left snoc nil input in
      let hash = root tree in
      let path = ok_exn @@ compute_path tree pos in
      match check_path path wrong_pos (Stdlib.List.nth input pos) hash with
      | Ok b -> not b
      | Error _ -> true)

let () =
  let qcheck_wrap = qcheck_wrap ~rand:(Random.State.make_self_init ()) in
  Alcotest.run
    ~__FILE__
    Protocol.name
    [
      ( "scons_equiv",
        qcheck_wrap
          [
            test_scons_scons_tr_equiv ~count:1000;
            test_scons_compute_equiv ~count:1000;
          ] );
      ( "of_list_equiv",
        qcheck_wrap
          [
            test_of_list_compute_equiv ~count:1000;
            test_of_list_snoc_tr_equiv ~count:1000;
          ] );
      ( "check_path",
        qcheck_wrap
          [test_check_path ~count:1000; test_check_path_wrong ~count:1000] );
    ]
