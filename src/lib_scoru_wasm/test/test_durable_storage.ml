(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech  <contact@trili.tech>                        *)
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
    Component:    Lib_scoru_wasm durable
    Invocation:   dune exec src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "Durable storage"
    Subject:      Durable storage tests for the tezos-scoru-wasm library
*)

open Tztest
open Lazy_containers
open Tezos_scoru_wasm
include Test_encodings_util
module Wasm = Wasm_pvm.Make (Tree)
module Wrapped_tree_runner = Tree_encoding.Runner.Make (Tree_encoding.Wrapped)

let wrap_as_durable tree =
  let open Lwt.Syntax in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.value ["durable"; "_keep_me"] Data_encoding.bool)
      true
      tree
  in
  let+ tree =
    Tree_encoding_runner.decode
      (Tree_encoding.scope ["durable"] Tree_encoding.wrapped_tree)
      tree
  in
  Tree_encoding.Wrapped.wrap tree

let assert_invalid_key run =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let+ _ = run () in
      assert false)
    (fun caught_exn ->
      match caught_exn with
      | Durable.Invalid_key _ -> Lwt.return_ok ()
      | x -> raise x)

(* Test that find_value/find_value_exn correctly decode the
   chunked_byte_vector *)
let test_durable_find_value () =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let value = Chunked_byte_vector.of_string "a very long value" in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ["durable"; "hello"; "value"; "_"]
         Tree_encoding.chunked_byte_vector)
      value
      tree
  in
  let* tree = wrap_as_durable tree in
  let durable = Durable.of_tree tree in
  let* r =
    Durable.find_value durable @@ Durable.key_of_string_exn "/hello/value"
  in
  assert (Option.is_some r) ;
  let* x =
    match r with
    | Some y -> Chunked_byte_vector.to_string y
    | None -> assert false
  in
  assert (x = "a very long value") ;
  let* v =
    Durable.find_value_exn durable @@ Durable.key_of_string_exn "/hello/value"
  in
  let* x = Chunked_byte_vector.to_string v in
  assert (x = "a very long value") ;
  let* r =
    Durable.find_value durable @@ Durable.key_of_string_exn "/hello/other"
  in
  assert (Option.is_none r) ;
  Lwt.return_ok ()

(* Test invalid key encodings are rejected. *)
let test_durable_invalid_keys () =
  let open Lwt.Syntax in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "//hello")
  in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "hello")
  in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "/hello//world")
  in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "/invalid_/key")
  in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "/!\"?I")
  in
  Lwt.return_ok ()

let tests =
  [
    tztest "Durable: find value" `Quick test_durable_find_value;
    tztest "Durable: invalid keys" `Quick test_durable_invalid_keys;
  ]
