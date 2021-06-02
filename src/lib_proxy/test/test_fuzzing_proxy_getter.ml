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
    Invocation:   dune build @src/lib_proxy/runtest
    Subject:      Fuzzing tests of internals of the client's --mode proxy
*)

module Local = Tezos_context_memory.Context
module Proxy_getter = Tezos_proxy.Proxy_getter

let tree_path_gen = Crowbar.list Crowbar.bytes

let leaf_data_gen = Crowbar.map [Crowbar.bytes] Bytes.of_string

(** [Tree.set_leaf] then [Tree.get] should return the inserted data *)
let test_set_leaf_get (tree_path : string list) leaf_data =
  let module Tree = Proxy_getter.Internal.Tree in
  let expected = leaf_data in
  let actual_lwt =
    (Tree.set_leaf
       Tree.empty
       tree_path
       (Tezos_shell_services.Block_services.Key leaf_data)
     >>= function
     | Ok (Value updated_tree) -> Tree.get updated_tree tree_path
     | _ -> assert false)
    >>= function
    | None -> assert false
    | Some result_tree -> (
        Local.Tree.to_value result_tree >|= function
        | None -> assert false
        | Some bytes -> bytes)
  in
  let actual = Lwt_main.run actual_lwt in
  Crowbar.check_eq expected actual

let () =
  Crowbar.add_test
    ~name:"[Tree.set_leaf] then [Tree.get] should return the inserted data"
    [tree_path_gen; leaf_data_gen]
    test_set_leaf_get
