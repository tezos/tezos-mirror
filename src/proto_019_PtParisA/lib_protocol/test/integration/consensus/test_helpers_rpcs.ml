(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
    Component:  Protocol (Helpers RPCs)
    Invocation: dune exec src/proto_019_PtParisA/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_helpers_rpcs.ml
    Subject:    On RPCs.
*)

(* Test the baking_rights RPC.
   Future levels or cycles are not tested because it's hard in this framework,
   using only RPCs, to fabricate them. *)
let test_baking_rights () =
  let open Lwt_result_syntax in
  let* b, (c1, _c2) = Context.init2 () in
  let open Plugin.RPC.Baking_rights in
  (* default max_round returns 65 results *)
  let* rights = get Block.rpc_ctxt b ~all:true in
  assert (Compare.List_length_with.(rights = 65)) ;
  (* arbitrary max_round *)
  let max_round = 15 in
  let* rights = get Block.rpc_ctxt b ~all:true ~max_round in
  assert (Compare.List_length_with.(rights = max_round + 1)) ;
  (* filtering by delegate *)
  let d = Context.Contract.pkh c1 in
  let* rights = get Block.rpc_ctxt b ~all:true ~delegates:[d] in
  assert (List.for_all (fun {delegate; _} -> delegate = d) rights) ;
  (* filtering by cycle *)
  let* {cycle; _} = Plugin.RPC.current_level Block.rpc_ctxt b in
  let* rights = get Block.rpc_ctxt b ~all:true ~cycle in
  let* first, last = Plugin.RPC.levels_in_current_cycle Block.rpc_ctxt b in
  assert (
    List.for_all (fun {level; _} -> level >= first && level <= last) rights) ;
  (* filtering by level *)
  let* {level; _} = Plugin.RPC.current_level Block.rpc_ctxt b in
  let* rights = get Block.rpc_ctxt b ~all:true ~levels:[level] in
  let expected_level = level in
  assert (List.for_all (fun {level; _} -> level = expected_level) rights) ;
  return_unit

let tests = [Tztest.tztest "baking_rights" `Quick test_baking_rights]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("helpers rpcs", tests)]
  |> Lwt_main.run
