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
    Component:    Proxy context (without delegation for now)
    Invocation:   dune exec src/lib_protocol_environment/test/test_proxy_context.exe
    Dependencies: src/lib_protocol_environment/test/assert.ml
    Subject:      Low-level operations on proxy contexts.
*)

(* Generates data inside the context of the block *)
let create_block (ctxt : Context.t) : Context.t Lwt.t =
  let open Lwt_syntax in
  let* ctxt = Context.add ctxt ["a"; "b"] (Bytes.of_string "Novembre") in
  let* ctxt = Context.add ctxt ["a"; "c"] (Bytes.of_string "Juin") in
  let* ctxt = Context.add ctxt ["version"] (Bytes.of_string "0.0") in
  Lwt.return ctxt

let key_to_string : String.t list -> String.t = String.concat ";"

(* Initialize the Context before starting the tests *)
let init_contexts (f : Context.t -> unit Lwt.t) _ () : 'a Lwt.t =
  let open Lwt_syntax in
  let proxy_genesis : Context.t = Proxy_context.empty None in
  let* proxy = create_block proxy_genesis in
  f proxy

let test_context_mem_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_mem key expected =
    let* res = Context.mem proxy key in
    Assert.equal_bool ~msg:("Context.mem " ^ key_to_string key) expected res ;
    Lwt.return_unit
  in
  let* () = assert_mem ["version"] true in
  let* () = assert_mem ["a"; "b"] true in
  let* () = assert_mem ["a"; "c"] true in
  let* () = assert_mem ["a"; "d"] false in
  Lwt.return_unit

let test_context_mem_tree_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_mem_tree key expected =
    let* res = Context.mem_tree proxy key in
    Assert.equal_bool
      ~msg:("Context.mem_tree " ^ key_to_string key)
      expected
      res ;
    Lwt.return_unit
  in
  let* () = assert_mem_tree ["a"] true in
  let* () = assert_mem_tree ["b"] false in
  let* () = assert_mem_tree ["a"; "b"] false in
  Lwt.return_unit

let test_context_find_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_find key expected =
    let* res = Context.find proxy key in
    Assert.equal_bytes_option
      ~msg:("Context.find " ^ key_to_string key)
      expected
      res ;
    Lwt.return_unit
  in
  let* () = assert_find ["version"] (Some (Bytes.of_string "0.0")) in
  let* () = assert_find ["a"; "b"] (Some (Bytes.of_string "Novembre")) in
  let* () = assert_find ["a"] None in
  let* () = assert_find ["a"; "x"] None in
  Lwt.return_unit

let test_context_find_tree_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_find_tree key expected =
    let* res = Context.find_tree proxy key in
    Assert.equal_bool
      ~msg:("Context.find_tree " ^ key_to_string key)
      expected
      (Option.is_some res) ;
    Lwt.return_unit
  in
  let* () = assert_find_tree ["version"] true in
  let* () = assert_find_tree ["a"; "b"] true in
  let* () = assert_find_tree ["a"] true in
  let* () = assert_find_tree ["a"; "x"] false in
  Lwt.return_unit

let test_context_list_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_list key fct =
    let* res = Context.list proxy key in
    Assert.equal_bool ~msg:("Context.list " ^ key_to_string key) true (fct res) ;
    Lwt.return_unit
  in
  let* () = assert_list ["version"] (( = ) []) in
  let* () = assert_list ["a"; "b"] (( = ) []) in
  let* () = assert_list ["a"; "x"] (( = ) []) in
  let* () =
    assert_list ["a"] (fun res -> List.map (fun (i, _) -> i) res = ["b"; "c"])
  in
  Lwt.return_unit

(******************************************************************************)

let tests =
  [
    ("mem", test_context_mem_fct);
    ("memtree", test_context_mem_tree_fct);
    ("find", test_context_find_fct);
    ("find_tree", test_context_find_tree_fct);
    ("list", test_context_list_fct);
  ]

let tests : unit Alcotest_lwt.test_case list =
  List.map
    (fun (n, f) -> Alcotest_lwt.test_case n `Quick (init_contexts f))
    tests

let () =
  Alcotest_lwt.run "tezos-shell-proxy-context" [("proxy_context", tests)]
  |> Lwt_main.run
