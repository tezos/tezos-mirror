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

module Events = State_events

let genesis chain_state =
  let genesis = Legacy_state.Chain.genesis chain_state in
  Legacy_state.Block.read_opt chain_state genesis.block
  >|= WithExceptions.Option.get ~loc:__LOC__

let known_heads chain_state =
  Legacy_state.read_chain_data chain_state (fun chain_store _data ->
      Legacy_store.Chain_data.Known_heads.elements chain_store)
  >>= fun hashes ->
  List.map_p
    (fun h ->
      Legacy_state.Block.read_opt chain_state h
      >|= WithExceptions.Option.get ~loc:__LOC__)
    hashes

let head chain_state =
  Legacy_state.read_chain_data chain_state (fun _chain_store data ->
      Lwt.return data.current_head)

let mem chain_state hash =
  Legacy_state.read_chain_data chain_state (fun chain_store data ->
      if Block_hash.equal (Legacy_state.Block.hash data.current_head) hash then
        Lwt.return_true
      else Legacy_store.Chain_data.In_main_branch.known (chain_store, hash))

type data = Legacy_state.chain_data = {
  current_head : Legacy_state.Block.t;
  current_mempool : Mempool.t;
  live_blocks : Block_hash.Set.t;
  live_operations : Operation_hash.Set.t;
  test_chain : Chain_id.t option;
  save_point : Int32.t * Block_hash.t;
  caboose : Int32.t * Block_hash.t;
}

let data chain_state =
  Legacy_state.read_chain_data chain_state (fun _chain_store data ->
      Lwt.return data)

let locator chain_state seed =
  data chain_state
  >>= fun data ->
  Legacy_state.compute_locator chain_state data.current_head seed

let locked_set_head chain_store data block =
  let rec pop_blocks ancestor block =
    let hash = Legacy_state.Block.hash block in
    if Block_hash.equal hash ancestor then Lwt.return_unit
    else
      Events.(emit pop_block hash)
      >>= fun () ->
      Legacy_store.Chain_data.In_main_branch.remove (chain_store, hash)
      >>= fun () ->
      Legacy_state.Block.predecessor block
      >>= function
      | Some predecessor ->
          pop_blocks ancestor predecessor
      | None ->
          assert false
    (* Cannot pop the genesis... *)
  in
  let push_block pred_hash block =
    let hash = Legacy_state.Block.hash block in
    Events.(emit push_block hash)
    >>= fun () ->
    Legacy_store.Chain_data.In_main_branch.store (chain_store, pred_hash) hash
    >>= fun () -> Lwt.return hash
  in
  Legacy_chain_traversal.new_blocks
    ~from_block:data.current_head
    ~to_block:block
  >>= fun (ancestor, path) ->
  let ancestor = Legacy_state.Block.hash ancestor in
  pop_blocks ancestor data.current_head
  >>= fun () ->
  List.fold_left_s push_block ancestor path
  >>= fun _ ->
  Legacy_store.Chain_data.Current_head.store
    chain_store
    (Legacy_state.Block.hash block)
  >>= fun () ->
  (* TODO more optimized updated of live_{blocks/operations} when the
     new head is a direct successor of the current head...
     Make sure to do the live blocks computation in `init_head`
     when this TODO is resolved. *)
  Lwt.return
    {
      data with
      current_head = block;
      current_mempool = Mempool.empty;
      live_blocks = Block_hash.Set.empty;
      live_operations = Operation_hash.Set.empty;
    }

let set_head chain_state block =
  (* Legacy_state.Block.max_operations_ttl block
   * >>=? fun max_op_ttl -> *)
  (* Chain_traversal.live_blocks block max_op_ttl
   * >>=? fun (live_blocks, live_operations) -> *)
  Legacy_state.update_chain_data chain_state (fun chain_store data ->
      locked_set_head chain_store data block
      >>= fun new_chain_data ->
      Lwt.return (Some new_chain_data, data.current_head))
  >>= fun chain_state -> return chain_state

let test_and_set_head chain_state ~old block =
  (* Legacy_state.Block.max_operations_ttl block
   * >>=? fun max_op_ttl ->
   * Chain_traversal.live_blocks block max_op_ttl
   * >>=? fun (live_blocks, live_operations) -> *)
  Legacy_state.update_chain_data chain_state (fun chain_store data ->
      if not (Legacy_state.Block.equal data.current_head old) then
        Lwt.return (None, false)
      else
        locked_set_head chain_store data block
        (* live_blocks live_operations *)
        >>= fun new_chain_data -> Lwt.return (Some new_chain_data, true))
  >>= fun chain_state -> return chain_state

let init_head chain_state =
  head chain_state
  >>= fun block ->
  set_head chain_state block >>=? fun (_ : Legacy_state.Block.t) -> return_unit
