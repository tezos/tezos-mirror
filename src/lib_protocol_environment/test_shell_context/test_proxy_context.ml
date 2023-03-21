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
    Invocation:   dune exec src/lib_protocol_environment/test_shell_context/main.exe
    Dependencies: src/lib_protocol_environment/test_shell_context/assert.ml
    Subject:      Low-level operations on proxy contexts.
*)

module Assert = Assert

(* Generates data inside the context of the block *)
let create_block (ctxt : Tezos_context_memory.Context.t) :
    Tezos_context_memory.Context.t Lwt.t =
  let open Lwt_syntax in
  let* ctxt =
    Tezos_context_memory.Context.add
      ctxt
      ["a"; "b"]
      (Bytes.of_string "November")
  in
  let* ctxt =
    Tezos_context_memory.Context.add ctxt ["a"; "c"] (Bytes.of_string "June")
  in
  let* ctxt =
    Tezos_context_memory.Context.add ctxt ["version"] (Bytes.of_string "0.0")
  in
  Lwt.return ctxt

let key_to_string : String.t list -> String.t = String.concat ";"

(* Initialize the Context before starting the tests *)
let init_contexts (f : Context.t -> unit Lwt.t) _ () : 'a Lwt.t =
  let open Lwt_syntax in
  let ctxt = Tezos_context_memory.make_empty_context () in
  let* ctxt = create_block ctxt in
  let proxy : Context.t =
    Tezos_protocol_environment.Proxy_context.empty
      (Some (Tezos_shell_context.Proxy_delegate_maker.of_memory_context ctxt))
  in
  f proxy

let test_context_mem_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_mem key expected =
    let* res = Context.mem proxy key in
    Assert.Bool.equal
      ~loc:__LOC__
      ~msg:(Printf.sprintf "Context.mem [%s], got %b" (key_to_string key) res)
      expected
      res ;
    return_unit
  in
  let* () = assert_mem ["version"] true in
  let* () = assert_mem ["a"; "b"] true in
  let* () = assert_mem ["a"; "c"] true in
  assert_mem ["a"; "d"] false

let test_context_mem_tree_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_mem_tree key expected =
    let* res = Context.mem_tree proxy key in
    Assert.Bool.equal
      ~loc:__LOC__
      ~msg:
        (Printf.sprintf "Context.mem_tree [%s], got %b" (key_to_string key) res)
      expected
      res ;
    return_unit
  in
  let* () = assert_mem_tree ["a"] true in
  let* () = assert_mem_tree ["b"] false in
  let* () = assert_mem_tree ["a"; "b"] true in
  assert_mem_tree ["a"; "x"] false

let test_context_find_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_find key expected =
    let* res = Context.find proxy key in
    Assert.Bytes.Option.equal
      ~loc:__LOC__
      ~msg:
        (Printf.sprintf
           "Context.find [%s], is some: %b"
           (key_to_string key)
           (Option.is_some res))
      expected
      res ;
    return_unit
  in
  let* () = assert_find ["version"] (Some (Bytes.of_string "0.0")) in
  let* () = assert_find ["a"; "b"] (Some (Bytes.of_string "November")) in
  let* () = assert_find ["a"] None in
  assert_find ["a"; "x"] None

let test_context_find_tree_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_find_tree key expected =
    let* res = Context.find_tree proxy key in
    Assert.Bool.equal
      ~loc:__LOC__
      ~msg:
        (Printf.sprintf
           "Context.find_tree [%s], is some: %b"
           (key_to_string key)
           (Option.is_some res))
      expected
      (Option.is_some res) ;
    return_unit
  in
  let* () = assert_find_tree ["version"] true in
  let* () = assert_find_tree ["a"; "b"] true in
  let* () = assert_find_tree ["a"] true in
  assert_find_tree ["a"; "x"] false

let test_context_list_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_list key expected_keys =
    let+ res = Context.list proxy key in
    Assert.String.List.equal
      ~loc:__LOC__
      ~msg:
        (Printf.sprintf
           "Context.list [%s], got [%s]"
           (key_to_string key)
           (String.concat ", " (List.map fst res)))
      (List.map fst res)
      expected_keys
  in
  let* () = assert_list ["version"] [] in
  let* () = assert_list ["a"; "b"] [] in
  let* () = assert_list ["a"; "x"] [] in
  assert_list ["a"] ["b"; "c"]

let test_context_length_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_length key expected =
    let* res = Context.length proxy key in
    Assert.equal
      ~loc:__LOC__
      ~msg:
        (Printf.sprintf "Context.length [%s], got %d" (key_to_string key) res)
      expected
      res ;
    return_unit
  in
  let* () = assert_length ["a"] 2 in
  let* () = assert_length [] 2 in
  let* () = assert_length ["a"; "b"] 0 in
  assert_length ["a"; "x"] 0

let test_context_fold_fct (proxy : Context.t) : unit Lwt.t =
  let open Lwt_syntax in
  let assert_fold key exp =
    let* res =
      Context.fold proxy key ~order:`Undefined ~init:"" ~f:(fun k _ acc ->
          return (acc ^ ":" ^ key_to_string k))
    in
    Assert.equal
      ~loc:__LOC__
      ~msg:(Printf.sprintf "Context.fold [%s], got %S" (key_to_string key) res)
      exp
      res ;
    return_unit
  in
  let* () = assert_fold ["a"; "b"] "" in
  let* () = assert_fold ["a"] "::b:c" in
  assert_fold [] "::a:a;b:a;c:version"

(******************************************************************************)

let tests =
  [
    ("mem", test_context_mem_fct);
    ("memtree", test_context_mem_tree_fct);
    ("find", test_context_find_fct);
    ("find_tree", test_context_find_tree_fct);
    ("list", test_context_list_fct);
    ("length", test_context_length_fct);
    ("fold", test_context_fold_fct);
  ]

let tests : unit Alcotest_lwt.test_case list =
  List.map
    (fun (n, f) -> Alcotest_lwt.test_case n `Quick (init_contexts f))
    tests

let () =
  Alcotest_lwt.run "tezos-shell-proxy-context" [("proxy_context", tests)]
  |> Lwt_main.run
