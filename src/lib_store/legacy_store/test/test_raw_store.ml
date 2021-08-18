(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
    Component:    Raw Store
    Invocation:   dune build @src/lib_storage/runtest
    Subject:      On (key, value) storage.
*)

open Raw_store

let ( >>= ) = Lwt.bind

let ( >|= ) = Lwt.( >|= )

open Filename.Infix

let wrap_store_init f _ () =
  Lwt_utils_unix.with_tempdir "tezos_test_" (fun base_dir ->
      let root = base_dir // "store" in
      init ~mapsize:4_096_000L root >>= function
      | Error _ -> Assert.fail_msg "wrap_store_init"
      | Ok store -> f store)

let entries s k =
  fold s k ~init:[] ~f:(fun e acc -> Lwt.return (e :: acc)) >|= List.rev

(** Stores various (key, value) associations from type [string list] to
    [bytes]. Use function [entries] to filter out keys by prefix. Then,
    assert that such lists contain the right keys with [`Key] or [`Dir]
    for key prefixes.
*)
let test_fold st =
  store st ["a"; "b"] (Bytes.of_string "Novembre") >>= fun _ ->
  store st ["a"; "c"] (Bytes.of_string "Juin") >>= fun _ ->
  store st ["a"; "d"; "e"] (Bytes.of_string "Septembre") >>= fun _ ->
  store st ["f"] (Bytes.of_string "Avril") >>= fun _ ->
  (* The code of '.' is just below the one of '/' ! *)
  store st ["g"; ".12"; "a"] (Bytes.of_string "Mai") >>= fun _ ->
  store st ["g"; ".12"; "b"] (Bytes.of_string "Février") >>= fun _ ->
  store st ["g"; "123"; "456"] (Bytes.of_string "Mars") >>= fun _ ->
  store st ["g"; "1230"] (Bytes.of_string "Janvier") >>= fun _ ->
  entries st [] >>= fun l ->
  Assert.equal_key_dir_list ~msg:__LOC__ [`Dir ["a"]; `Key ["f"]; `Dir ["g"]] l ;
  entries st ["0"] >>= fun l ->
  Assert.equal_key_dir_list ~msg:__LOC__ [] l ;
  entries st ["0"; "1"] >>= fun l ->
  Assert.equal_key_dir_list ~msg:__LOC__ [] l ;
  entries st ["a"] >>= fun l ->
  Assert.equal_key_dir_list
    ~msg:__LOC__
    [`Key ["a"; "b"]; `Key ["a"; "c"]; `Dir ["a"; "d"]]
    l ;
  entries st ["a"; "d"] >>= fun l ->
  Assert.equal_key_dir_list ~msg:__LOC__ [`Key ["a"; "d"; "e"]] l ;
  entries st ["f"] >>= fun l ->
  Assert.equal_key_dir_list ~msg:__LOC__ [] l ;
  entries st ["f"; "z"] >>= fun l ->
  Assert.equal_key_dir_list ~msg:__LOC__ [] l ;
  entries st ["g"] >>= fun l ->
  Assert.equal_key_dir_list
    ~msg:__LOC__
    [`Dir ["g"; ".12"]; `Dir ["g"; "123"]; `Key ["g"; "1230"]]
    l ;
  entries st ["g"; "123"] >>= fun l ->
  Assert.equal_key_dir_list ~msg:__LOC__ [`Key ["g"; "123"; "456"]] l ;
  entries st ["z"] >>= fun l ->
  Assert.equal_key_dir_list ~msg:__LOC__ [] l ;
  Lwt.return_unit

let tests = [Alcotest_lwt.test_case "fold" `Quick (wrap_store_init test_fold)]
