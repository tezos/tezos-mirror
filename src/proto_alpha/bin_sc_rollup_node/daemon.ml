(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

type head_state = {head : Layer1.head; finalized : bool; seen_before : bool}

let emit_head_processing_event
    {head = Head {hash; level}; finalized; seen_before} =
  Daemon_event.head_processing hash level finalized seen_before

let emit_heads_not_processed_event head_states =
  Lwt_list.iter_s
    (fun {head = Head {hash; level}; _} ->
      Daemon_event.not_finalized_head hash level)
    head_states

let categorise_heads (node_ctxt : Node_context.t) old_heads new_heads =
  (* For each head, determine if it has already been seen before and if it has
     been finalized, using the block finality time (for Tenderbake, this
     is 2).
  *)

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2868
     Handle protocols with non-deterministic finality. *)
  let all_heads = old_heads @ new_heads in
  let number_of_temporary_heads =
    min node_ctxt.block_finality_time (List.length all_heads)
  in

  let number_of_new_heads = List.length new_heads in

  let (head_states, _, _) =
    List.fold_right
      (fun head (heads, n, m) ->
        ({head; finalized = n <= 0; seen_before = m <= 0} :: heads, n - 1, m - 1))
      all_heads
      ([], number_of_temporary_heads, number_of_new_heads)
  in
  head_states

let process_head node_ctxt store head_state =
  (* Because we keep track of finalized heads using transaction finality time,
     rather than block finality time, it is possible that heads with the same
     level are processed as finalized. Individual modules that process heads
     when finalized, such as Commitment, need to take this into account.
  *)
  let open Lwt_result_syntax in
  let {finalized; seen_before; head} = head_state in
  let* () =
    if seen_before then return_unit
    else
      let*! () = emit_head_processing_event head_state in
      (* Avoid processing inbox again if it has been processed before for this head *)
      let* () = Inbox.process_head node_ctxt store head in
      (* Avoid storing and publishing commitments if the head is not final *)
      (* Avoid triggering the pvm execution if this has been done before for this head *)
      Interpreter.Arith.process_head store head
  in
  let* () =
    if finalized then
      Commitment.process_head (module Arith_pvm) node_ctxt store head
    else return_unit
  in
  (* Publishing a commitment when one is available does not depend on the state of
     the current head, but we still need to ensure that the node only published
     one commitment per block. *)
  Commitment.publish_commitment node_ctxt store

(* [on_layer_1_chain_event node_ctxt store chain_event old_heads] processes a
   list of heads, coming from either a list of [old_heads] or from the current
   [chain_event]. [old_heads] is the list of heads returned by the previous 
   iteration of [on_layer_1_chain_event] in the [daemonize function]. These are
   heads included in the branch currently tracked by the rollup node, and that 
   have only been partially processed, due to the rollup  node not being able 
   to establish their finality. The function returns a list of heads from the 
   current branch tracked by the rollup node, whose finality cannot be 
   established at the time the function is invoked. Those heads will be 
   processed again at the next iteration of [on_layer_1_chain_event] in the 
   [daemonize] function. If [chain_event] is a rollback event, then no head
   needs to be returned to be included as the rollup node started tracking a 
   new branch.
 *)
let on_layer_1_chain_event node_ctxt store chain_event old_heads =
  let open Lwt_result_syntax in
  let open Layer1 in
  let* non_final_heads =
    match chain_event with
    | SameBranch {new_head; intermediate_heads} ->
        let head_states =
          categorise_heads node_ctxt old_heads (intermediate_heads @ [new_head])
        in
        let* () = List.iter_es (process_head node_ctxt store) head_states in
        (* Return new_head to be processed as finalized head if the
           next chain event is of type SameBranch.
        *)
        let non_final_head_states =
          List.filter (fun head_state -> not head_state.finalized) head_states
        in
        let*! () = emit_heads_not_processed_event non_final_head_states in
        let non_final_heads =
          List.map (fun head_state -> head_state.head) non_final_head_states
        in
        return non_final_heads
    | Rollback {new_head = Layer1.Head {level = new_level; _}} ->
        (* The new_head of the rollback event corresponds to a head that
           was previously finalized. Heads in `old_heads` that have a level
           preceding or equal to `new_level` can now be considered final,
           and will be processed as such. `new_level` can now be considered
           as such. Heads in `old_heads` whose level is greater than
           `new_level` can be safely discarded.
        *)
        let (final_heads, _non_final_heads) =
          List.partition
            (fun head ->
              let (Layer1.Head {level; _}) = head in
              level <= new_level)
            old_heads
        in
        let+ () =
          List.iter_es
            (fun head ->
              process_head
                node_ctxt
                store
                {head; finalized = true; seen_before = true})
            final_heads
        in
        []
  in
  let*! () = Layer1.processed chain_event in
  return non_final_heads

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2895
   Use Lwt_stream.fold_es once it is exposed. *)
let iter_stream stream handle =
  let rec go heads =
    Lwt.bind (Lwt_stream.get stream) @@ fun tok ->
    match tok with
    | None -> return_unit
    | Some element -> Lwt_result.bind (handle element heads) go
  in
  go []

let daemonize node_ctxt store layer_1_chain_events =
  Lwt.no_cancel
  @@ iter_stream layer_1_chain_events
  @@ on_layer_1_chain_event node_ctxt store

let install_finalizer store rpc_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = RPC_server.Arith.shutdown rpc_server in
  let* () = Store.close store in
  let* () = Event.shutdown_node exit_status in
  Tezos_base_unix.Internal_event_unix.close ()

let run ~data_dir (cctxt : Protocol_client_context.full) =
  let open Lwt_result_syntax in
  let start () =
    let*! () = Event.starting_node () in
    let* configuration = Configuration.load ~data_dir in
    let open Configuration in
    let {rpc_addr; rpc_port; sc_rollup_address; sc_rollup_node_operator; _} =
      configuration
    in
    let*! store = Store.load configuration in
    let* node_ctxt =
      Node_context.init cctxt sc_rollup_address sc_rollup_node_operator
    in
    let* rpc_server = RPC_server.Arith.start node_ctxt store configuration in
    (* Check that the public key hash is valid *)
    let* (_pkh, _pk, _skh) = Node_context.get_operator_keys node_ctxt in
    let* tezos_heads = Layer1.start configuration node_ctxt.cctxt store in
    let*! () = Inbox.start () in
    let*! () = Commitment.start () in

    let _ = install_finalizer store rpc_server in
    let*! () = Event.node_is_ready ~rpc_addr ~rpc_port in
    daemonize node_ctxt store tezos_heads
  in
  start ()
