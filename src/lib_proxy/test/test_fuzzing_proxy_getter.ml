(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Proxy getter
    Invocation:   dune exec src/lib_proxy/test/main.exe
    Subject:      Fuzzing tests of internals of the client's --mode proxy
*)

module Local = Tezos_context_memory.Context
module Proxy_getter = Tezos_proxy.Proxy_getter
module Tree = Proxy_getter.Internal.Tree
open Qcheck2_helpers

open Tezos_proxy_test_helpers_shell_services.Test_helpers_shell_services

let key_gen =
  (* Using small_list, otherwise the test takes considerably longer.
     This test is quite slow already *)
  QCheck2.Gen.(small_list (small_string ?gen:None))

let print_key = QCheck2.Print.(list string)

let tree_gen =
  let open Lwt_syntax in
  let rec mk_tree acc sets =
    match sets with
    | [] -> Lwt.return acc
    | (key, value) :: tl -> (
        let* v = Tree.add_leaf acc key value in
        match v with
        | Tezos_proxy.Proxy.Mutation -> (mk_tree [@ocaml.tailcall]) acc tl
        | Tezos_proxy.Proxy.Value acc' -> (mk_tree [@ocaml.tailcall]) acc' tl)
  in
  let mk_tree acc sets = Lwt_main.run @@ mk_tree acc sets in
  QCheck2.Gen.(
    map (mk_tree Tree.empty) (small_list (pair key_gen raw_context_gen)))

(** [Tree.add_leaf] then [Tree.get] should return the inserted data *)
let test_add_leaf_get =
  QCheck2.Test.make
    ~name:"Tree.get (Tree.add_leaf t k v) k = v"
    ~print:
      (QCheck2.Print.triple (fun _ -> "<tree>") print_key print_raw_context)
    QCheck2.Gen.(triple tree_gen key_gen raw_context_gen)
  @@ fun (tree, key, value) ->
  let expected =
    Lwt_main.run @@ Proxy_getter.Internal.raw_context_to_tree value
  in
  (* We need to make sure that we are actually setting something: *)
  QCheck2.assume @@ Option.is_some expected ;
  let tree' = Lwt_main.run @@ Tree.add_leaf tree key value in
  let tree' =
    match tree' with
    | Tezos_proxy.Proxy.Mutation -> tree
    | Tezos_proxy.Proxy.Value tree' -> tree'
  in
  let actual = Lwt_main.run @@ Tree.get tree' key in
  let pp = Format.pp_print_option Local.Tree.pp in
  let eq = Option.equal Local.Tree.equal in
  qcheck_eq' ~pp ~eq ~expected ~actual ()

let () =
  Alcotest.run
    ~__FILE__
    "Proxy Getter"
    [("Array theory", qcheck_wrap [test_add_leaf_get])]
