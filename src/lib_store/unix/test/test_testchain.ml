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
    _______

    Component: Store
    Invocation: dune exec src/lib_store/unix/test/main.exe -- --file test_testchain.ml
    Subject: Store tests ( testchain )
*)

open Test_utils

let fork_testchain chain_store (blocks, forked_block) =
  let open Lwt_result_syntax in
  let forked_block_hash = Store.Block.hash forked_block in
  let genesis_hash =
    Block_hash.hash_bytes [Block_hash.to_bytes forked_block_hash]
  in
  let testchain_id = Chain_id.of_block_hash genesis_hash in
  let head_header = Store.Block.header forked_block in
  let test_protocol = Tezos_protocol_alpha.Protocol.hash in
  let expiration = Time.Protocol.epoch in
  let global_store = Store.Chain.global_store chain_store in
  let context_index = Store.context_index global_store in
  (* Call [Context.fork_test_chain] then commit so we are able to gather
     commit info *)
  let* resulting_context_hash =
    Store.Block.resulting_context_hash chain_store forked_block
  in
  let*! context =
    Context_ops.checkout_exn context_index resulting_context_hash
  in
  let*! context =
    Context_ops.fork_test_chain context ~protocol:test_protocol ~expiration
  in
  let*! context_hash =
    Context_ops.commit ~time:head_header.shell.timestamp context
  in
  let genesis_header =
    let shell =
      {
        head_header.Block_header.shell with
        predecessor = genesis_hash;
        context = context_hash;
      }
    in
    {head_header with shell}
  in
  let* testchain =
    Store.Chain.fork_testchain
      chain_store
      ~testchain_id
      ~forked_block
      ~genesis_hash
      ~genesis_header
      ~test_protocol
      ~expiration
  in
  let testchain_store = Store.Chain.testchain_store testchain in
  let* test_blocks, head =
    append_blocks
      ~min_lafl:genesis_header.shell.level
      ~should_set_head:true
      testchain_store
      ~kind:`Full
      10
  in
  let* () = assert_absence_in_store testchain_store blocks in
  let* () = assert_absence_in_store chain_store test_blocks in
  let* () = assert_presence_in_store testchain_store test_blocks in
  return (testchain, test_blocks, head)

let test_simple store =
  let open Lwt_result_syntax in
  let chain_store = Store.main_chain_store store in
  let* blocks, head =
    append_blocks ~should_set_head:true chain_store ~kind:`Full 10
  in
  let* _ = fork_testchain chain_store (blocks, head) in
  return_unit

let test_inner store =
  let open Lwt_result_syntax in
  let chain_store = Store.main_chain_store store in
  let* blocks, head =
    append_blocks ~should_set_head:true chain_store ~kind:`Full 10
  in
  let* testchain, blocks, head = fork_testchain chain_store (blocks, head) in
  let testchain_store = Store.Chain.testchain_store testchain in
  let* _ = fork_testchain testchain_store (blocks, head) in
  return_unit

let test_shutdown store =
  let open Lwt_result_syntax in
  let chain_store = Store.main_chain_store store in
  let* blocks, head =
    append_blocks ~should_set_head:true chain_store ~kind:`Full 10
  in
  let* testchain, blocks, _head = fork_testchain chain_store (blocks, head) in
  let testchain_store = Store.Chain.testchain_store testchain in
  let testchain_id = Store.Chain.chain_id testchain_store in
  let*! o = Store.Chain.testchain chain_store in
  match o with
  | None -> Assert.fail_msg "testchain not found"
  | Some testchain' -> (
      Assert.equal ~eq:( == ) testchain testchain' ;
      let* () = Store.Chain.shutdown_testchain chain_store in
      let*! o = Store.Chain.testchain chain_store in
      match o with
      | Some _ -> Assert.fail_msg "test chain still initialized"
      | None -> (
          let* o =
            Store.Unsafe.load_testchain chain_store ~chain_id:testchain_id
          in
          match o with
          | None -> Assert.fail_msg "failed to load the existing test chain"
          | Some testchain'' ->
              let testchain_store'' = Store.Chain.testchain_store testchain'' in
              let testchain_id'' = Store.Chain.chain_id testchain_store'' in
              Assert.equal ~eq:Chain_id.equal testchain_id testchain_id'' ;
              assert_presence_in_store testchain_store'' blocks))

let tests =
  let wrap_test (s, f) =
    let f _ = f in
    wrap_test (s, f)
  in
  let test_cases =
    List.map
      wrap_test
      [
        ("forking a test chain", test_simple);
        ("spawning a test chain", test_inner);
        ("shutdown test chain then load it", test_shutdown);
      ]
  in
  ("test chain", test_cases)

let () = Lwt_main.run (Alcotest_lwt.run ~__FILE__ "tezos-store" [tests])
