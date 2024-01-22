(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs, <contact@nomadic-labs.com>.                   *)
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
    Component:  Smart rollup node library
    Invocation: dune exec src/lib_smart_rollup_node/test/main.exe \
                -- -f src/lib_smart_rollup_node/test/canary.ml
    Subject:    Canary unit tests to make sure the test helpers work as intended
*)

let build_chain node_ctxt ~genesis ~length =
  let open Lwt_result_syntax in
  let* blocks = Helpers.append_dummy_l2_chain node_ctxt ~length in
  return (genesis :: blocks)

let canary_test node_ctxt ~genesis =
  let open Lwt_result_syntax in
  let length = 100 in
  let* chain = build_chain node_ctxt ~genesis ~length in
  Assert.Int.equal
    ~loc:__LOC__
    ~msg:"chain_length_ok"
    (List.length chain)
    (length + 1) ;
  (* Checking that the chain matches what is stored *)
  let* () =
    List.iter_es
      (fun (block : Sc_rollup_block.t) ->
        let* store_block_by_hash =
          Node_context.get_l2_block node_ctxt block.header.block_hash
        in
        let* store_block_by_level =
          Node_context.get_l2_block_by_level node_ctxt block.header.level
        in
        Helpers.Assert.L2_block.equal
          ~loc:__LOC__
          ~msg:"stored_block_by_hash_ok"
          store_block_by_hash
          block ;
        Helpers.Assert.L2_block.equal
          ~loc:__LOC__
          ~msg:"stored_block_by_level_ok"
          store_block_by_level
          block ;
        return_unit)
      chain
  in
  let* head = Node_context.last_processed_head_opt node_ctxt in
  let last = List.last_opt chain in
  Helpers.Assert.L2_block.Option.equal
    ~loc:__LOC__
    ~msg:"head_is_last_block"
    head
    last ;
  return_unit

let tests =
  List.flatten
  @@ List.map
       (fun proto ->
         [
           Helpers.alcotest
             `Quick
             Example_arith
             proto
             ~boot_sector:""
             canary_test;
           Helpers.alcotest `Quick Wasm_2_0_0 proto ~boot_sector:"" canary_test;
         ])
       (Protocol_plugins.registered_protocols ())

let () =
  Alcotest_lwt.run ~__FILE__ "lib_smart_rollup_node" [("canary", tests)]
  |> Lwt_main.run
