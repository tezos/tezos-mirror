(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Client
    Invocation:   dune exec src/lib_proxy/test/main.exe
    Subject:      --mode proxy of the client
*)

(** Tests [Proxy_getter] by instantiating the [Make] functor with
    a mock of [PROTO_RPC]. It tests the basic behavior of the API. *)

module StringMap = String.Map
module Local = Tezos_context_memory.Context
module Proof = Tezos_context_sigs.Context.Proof_types

(** Alias to make "empty list" intention more explicit *)
let tree_root = []

let tree_testable = Alcotest.testable Local.Tree.pp Local.Tree.equal

let nb_nodes = function
  | None -> Lwt.return 0
  | Some tree ->
      Local.Tree.fold tree tree_root ~order:`Sorted ~init:0 ~f:(fun _ _ acc ->
          Lwt.return (acc + 1))

module type MOCKED_PROTO_RPC = sig
  include Tezos_proxy.Proxy_proto.PROTO_RPC

  val calls : Local.key Stack.t
end

(** Setup mocks *)
let mock_proto_rpc () =
  (module struct
    let calls : Local.key Stack.t = Stack.create ()

    let split_key _ (k : Local.key) =
      match k with
      (* These constants are used in tests below *)
      | "split" :: "key" :: "trigger_now!" :: tail ->
          Some (["split"; "key"; "trigger_now!"], tail)
      | _ -> None

    let failure_is_permanent _ = false

    let do_rpc _chain_n_block (k : Local.key) =
      let rec mock_raw_context = function
        | [] -> Proof.Key Bytes.empty
        | hd :: tail ->
            Proof.Dir (String.Map.singleton hd (mock_raw_context tail))
      in
      (* Remember call *)
      Stack.push k calls ;
      Lwt_result_syntax.return @@ mock_raw_context k
  end : MOCKED_PROTO_RPC)

class mock_rpc_context : Tezos_rpc.Context.simple =
  object
    method call_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
          'p ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
      assert false
  end

let mock_chain = `Main

let mock_block = `Head 0

let mock_input : Tezos_proxy.Proxy.proxy_getter_input =
  {
    rpc_context = new mock_rpc_context;
    mode = Client;
    chain = mock_chain;
    block = mock_block;
  }

open Assert
open Lwt_assert

let test_tree _ () =
  let open Tezos_proxy.Proxy_getter.RequestsTree in
  let is_all = function Some All -> true | _ -> false in
  let is_partial = function Some (Partial _) -> true | _ -> false in
  assert_true
    "empty contains nothing"
    (find_opt empty ["whatever"] |> Option.is_none) ;
  let only_A = add empty ["A"] in
  assert_true "only_A maps A to All" (find_opt only_A ["A"] |> is_all) ;
  assert_true
    "only_A maps A;whatever to All 1/2"
    (find_opt only_A ["A"; "b"] |> is_all) ;
  assert_true
    "only_A maps A;whatever to All 2/2"
    (find_opt only_A ["A"; "b"; "c"] |> is_all) ;
  assert_true "only_A maps B to None" (find_opt only_A ["B"] |> Option.is_none) ;
  let a_b = add empty ["a"; "b"] in
  assert_false "a;b differs from empty" (a_b = empty) ;
  let a_b_and_a_c = add a_b ["a"; "c"] in
  assert_true
    "(a;b ∪ a;c) maps a to Partial"
    (find_opt a_b_and_a_c ["a"] |> is_partial) ;
  assert_false
    "(a;b ∪ a;c) doesn't map a to All"
    (find_opt a_b_and_a_c ["a"] |> is_all) ;
  assert_true
    "(a;b ∪ a;c) maps a;b to All"
    (find_opt a_b_and_a_c ["a"; "b"] |> is_all) ;
  assert_true
    "(a;b ∪ a;c) maps a;b;d to All"
    (find_opt a_b_and_a_c ["a"; "b"; "d"] |> is_all) ;
  assert_true
    "Adding a;b into (a;b ∪ a;c) doesn't cause a change"
    (add a_b_and_a_c ["a"; "b"] = a_b_and_a_c) ;
  assert_true
    "Adding a;b;d into (a;b ∪ a;c) doesn't cause a change"
    (add a_b_and_a_c ["a"; "b"; "d"] = a_b_and_a_c) ;
  Lwt.return_unit

(* Tests that [Proxy_getter]
   uses a cached value when requesting a key longer
   than a key already obtained *)
let test_do_rpc_no_longer_key () =
  let open Lwt_result_syntax in
  let (module MockedProtoRPC) = mock_proto_rpc () in
  let module MockedGetter = Tezos_proxy.Proxy_getter.MakeProxy (MockedProtoRPC) in
  let* a_b_1_tree_opt = MockedGetter.proxy_get mock_input ["A"; "b"; "1"] in
  let*! nb_nodes_a_b_1_tree_opt = nb_nodes a_b_1_tree_opt in
  let*! () =
    lwt_assert_true
      "A;b;1 is mapped to tree of size 4"
      (nb_nodes_a_b_1_tree_opt = 4)
  in
  let a_b_1_tree = WithExceptions.Option.get ~loc:__LOC__ a_b_1_tree_opt in
  let* a_b_1_tree_opt' = MockedGetter.proxy_get mock_input ["A"; "b"; "1"] in
  let a_b_1_tree' = WithExceptions.Option.get ~loc:__LOC__ a_b_1_tree_opt' in
  let*! _ =
    lwt_check tree_testable "Tree is always the same" a_b_1_tree a_b_1_tree'
  in
  let*! _ =
    lwt_assert_true "Done one RPC" (Stack.length MockedProtoRPC.calls = 1)
  in
  let*! _ = MockedGetter.proxy_get mock_input ["A"; "b"; "2"] in
  let*! _ =
    lwt_assert_true "Done two RPCs" (Stack.length MockedProtoRPC.calls = 2)
  in
  (* Let's check that value mapped by A;b;1 was unaffected by getting A;b;2 *)
  let* a_b_1_tree_opt' = MockedGetter.proxy_get mock_input ["A"; "b"; "1"] in
  let a_b_1_tree' = WithExceptions.Option.get ~loc:__LOC__ a_b_1_tree_opt' in
  let*! _ =
    lwt_check
      tree_testable
      "Orthogonal tree stayed the same"
      a_b_1_tree
      a_b_1_tree'
  in
  let*! _ = MockedGetter.proxy_get mock_input ["A"] in
  let*! _ =
    lwt_assert_true "Done three RPCs" (Stack.length MockedProtoRPC.calls = 3)
  in
  let* a_opt = MockedGetter.proxy_get mock_input ["A"] in
  let*! _ =
    lwt_assert_true "Done three RPCs" (Stack.length MockedProtoRPC.calls = 3)
  in
  (* Let's check that value mapped by A;b;1 was changed by getting A.
     This is not needed for correctness but it would be weird if it wasn't
     the case, because getting a parent key erases the previous content of
     longers keys. Because of our mocked implementation of do_rpc, all
     keys are mapped to single values. This means "A" is mapped to Key.
     We can hence check that A;b;1 was affected by witnessing it's None now. *)
  let* a_b_1_tree_opt' = MockedGetter.proxy_get mock_input ["A"; "b"; "1"] in
  let*! nb_nodes_a_b_1_tree_opt' = nb_nodes a_b_1_tree_opt' in
  let*! _ =
    lwt_assert_true "A;b;1 tree is now missing" (nb_nodes_a_b_1_tree_opt' = 0)
  in
  let*! nb_nodes_a_opt = nb_nodes a_opt in
  Stdlib.print_endline @@ string_of_int @@ nb_nodes_a_opt ;
  (* Size is 2 because mock_tree returns a tree rooted with a Dir, it's normal *)
  let*! _ =
    lwt_assert_true "A is mapped to tree of size 2" (nb_nodes_a_opt = 2)
  in
  return_unit

let test_split_key_triggers () =
  let open Lwt_result_syntax in
  let (module MockedProtoRPC) = mock_proto_rpc () in
  let module MockedGetter = Tezos_proxy.Proxy_getter.MakeProxy (MockedProtoRPC) in
  let*! _ =
    MockedGetter.proxy_get
      mock_input
      ["split"; "key"; "trigger_now!"; "whatever"; "more"]
  in
  let*! _ =
    lwt_assert_true "Done one RPC" (Stack.length MockedProtoRPC.calls = 1)
  in
  let*! _ =
    lwt_check
      Alcotest.(list string)
      "Done split;key;trigger_now!, not the longer key"
      (Stack.copy MockedProtoRPC.calls |> Stack.pop)
      ["split"; "key"; "trigger_now!"]
  in
  return_unit

let () =
  Alcotest_lwt.run
    ~__FILE__
    "tezos-proxy"
    [
      ( "all tests",
        [
          Alcotest_lwt.test_case "RequestsTree" `Quick test_tree;
          Tztest.tztest "test do_rpc" `Quick test_do_rpc_no_longer_key;
          Tztest.tztest "test split key triggers" `Quick test_split_key_triggers;
        ] );
    ]
  |> Lwt_main.run
