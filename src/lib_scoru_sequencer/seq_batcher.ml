(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

open Protocol
open Alpha_context
open Octez_smart_rollup_node_alpha
open Octez_smart_rollup_node_alpha.Batcher_worker_types
module Message_queue = Hash_queue.Make (L2_message.Hash) (L2_message)
module Durable_state = Wasm_2_0_0_pvm.Durable_state

let worker_name = "seq_batcher"

module Batcher_events = Batcher_events.Declare (struct
  let worker_name = worker_name
end)

module L2_batched_message = struct
  type t = {content : string; l1_hash : Injector.Inj_operation.hash}
end

module Batched_messages = Hash_queue.Make (L2_message.Hash) (L2_batched_message)

type state = {
  node_ctxt : Node_context.ro;
  signer : Signature.public_key_hash;
  messages : Message_queue.t;
  batched : Batched_messages.t;
  mutable simulation_ctxt : Simulation.t option;
}

(* Takes sequencer message to inject and L2 messages included into it *)
let inject_sequence state (sequencer_message, l2_messages) =
  let open Lwt_result_syntax in
  let operation = L1_operation.Add_messages {messages = [sequencer_message]} in
  let+ l1_hash =
    Injector.add_pending_operation ~source:state.signer operation
  in
  List.iter
    (fun msg ->
      let content = L2_message.content msg in
      let hash = L2_message.hash msg in
      Batched_messages.replace state.batched hash {content; l1_hash})
    l2_messages

let inject_batches state = List.iter_es (inject_sequence state)

let get_previous_delayed_inbox_size node_ctxt (head : Layer1.head) =
  let open Lwt_result_syntax in
  let*? () =
    error_unless
      (head.level >= node_ctxt.Node_context.genesis_info.level)
      (Exn (Failure "Cannot obtain delayed inbox before origination level"))
  in
  let* previous_head = Node_context.get_predecessor node_ctxt head in
  let first_inbox_level = Int32.succ node_ctxt.genesis_info.level in
  let* ctxt =
    if previous_head.level < first_inbox_level then
      (* This is before we have interpreted the boot sector, so we start
         with an empty context in genesis *)
      return (Context.empty node_ctxt.context)
    else Node_context.checkout_context node_ctxt previous_head.hash
  in
  let* _ctxt, state = Interpreter.state_of_head node_ctxt ctxt previous_head in
  let open Kernel_durable in
  let*! pointer_bytes = Durable_state.lookup state Delayed_inbox_pointer.path in
  match pointer_bytes with
  | None -> return 0
  | Some pointer_bytes ->
      return
      @@ Option.fold ~none:0 ~some:(fun x ->
             Int32.(to_int @@ succ @@ sub x.Delayed_inbox_pointer.tail x.head))
      @@ Data_encoding.Binary.of_bytes_opt
           Delayed_inbox_pointer.encoding
           pointer_bytes

let get_batch_sequences state head =
  let open Lwt_result_syntax in
  let* delayed_inbox_size =
    get_previous_delayed_inbox_size state.node_ctxt head
  in
  (* Assuming at the moment that all the registered messages fit into a single L2 message.
     This logic will be extended later.
  *)
  let l2_messages = Message_queue.elements state.messages in
  let*? l2_messages_serialized =
    List.map_e
      (fun m ->
        Sc_rollup.Inbox_message.(serialize (External (L2_message.content m))))
      l2_messages
    |> Environment.wrap_tzresult
  in
  return
    ( [
        ( Kernel_message.encode_sequence_message
            state.node_ctxt.rollup_address
            ~prefix:(Int32.of_int (delayed_inbox_size - 1))
            ~suffix:1l
            l2_messages_serialized,
          l2_messages );
      ],
      delayed_inbox_size )

let produce_batch_sequences state head =
  let open Lwt_result_syntax in
  let* batches, total_delayed_inbox_sizes = get_batch_sequences state head in
  match batches with
  | [] -> return_unit
  | _ ->
      let* () = inject_batches state batches in
      let*! () =
        Batcher_events.(emit batched)
          (* As we add ALL the messages to the Sequence for now,
             the number of messages is equal to length of state.messages *)
          ( List.length batches,
            total_delayed_inbox_sizes + Message_queue.length state.messages )
      in
      Message_queue.clear state.messages ;
      return_unit

let simulate node_ctxt simulation_ctxt (messages : L2_message.t list) =
  let open Lwt_result_syntax in
  let*? ext_messages =
    List.map_e
      (fun m ->
        Sc_rollup.Inbox_message.(serialize (External (L2_message.content m))))
      messages
    |> Environment.wrap_tzresult
  in
  let+ simulation_ctxt, _ticks =
    Simulation.simulate_messages node_ctxt simulation_ctxt ext_messages
  in
  simulation_ctxt

(* Maximum size of single L2 message.
   If L2 message size exceeds it, it means we won't be able to form a Sequence with solely this message
*)
let max_single_l2_msg_size =
  Protocol.Constants_repr.sc_rollup_message_size_limit
  - Kernel_message.single_l2_message_overhead
  - 4 (* each L2 message prepended with it size *)

let get_simulation_context state =
  let open Lwt_result_syntax in
  match state.simulation_ctxt with
  | None -> failwith "Simulation context of sequencer not initialized"
  | Some simulation_ctxt -> return simulation_ctxt

(*** HANDLERS IMPLEMENTATION ***)
let on_register_messages state (messages : string list) =
  let open Lwt_result_syntax in
  let*? messages =
    List.mapi_e
      (fun i message ->
        if String.length message > max_single_l2_msg_size then
          error_with
            "Message %d is too large (max size is %d)"
            i
            max_single_l2_msg_size
        else Ok (L2_message.make message))
      messages
  in
  let* simulation_ctxt = get_simulation_context state in
  let* advanced_simulation_ctxt =
    simulate state.node_ctxt simulation_ctxt messages
  in
  state.simulation_ctxt <- Some advanced_simulation_ctxt ;
  let*! () = Batcher_events.(emit queue) (List.length messages) in
  let hashes =
    List.map
      (fun message ->
        let msg_hash = L2_message.hash message in
        Message_queue.replace state.messages msg_hash message ;
        msg_hash)
      messages
  in
  return hashes

let on_new_head state head =
  let open Lwt_result_syntax in
  let* () = produce_batch_sequences state head in
  when_ (head.level >= state.node_ctxt.genesis_info.level) @@ fun () ->
  let* simulation_ctxt =
    Simulation.start_simulation ~reveal_map:None state.node_ctxt head
  in
  state.simulation_ctxt <- Some simulation_ctxt ;
  return_unit

let init_batcher_state node_ctxt ~signer =
  Lwt.return
    {
      node_ctxt;
      signer;
      messages = Message_queue.create 100_000 (* ~ 400MB *);
      batched = Batched_messages.create 100_000 (* ~ 400MB *);
      simulation_ctxt = None;
    }

module Types = struct
  type nonrec state = state

  type parameters = {
    node_ctxt : Node_context.ro;
    signer : Signature.public_key_hash;
  }
end

module Name = struct
  (* We only have a single batcher in the node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = [Protocol.name; "sc_sequencer_node"; worker_name; "worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Register messages ->
        protect @@ fun () -> on_register_messages state messages
    | Request.New_head head -> protect @@ fun () -> on_new_head state head

  type launch_error = error trace

  let on_launch _w () Types.{node_ctxt; signer} =
    let open Lwt_result_syntax in
    let*! state = init_batcher_state node_ctxt ~signer in
    return state

  let on_error (type a b) _w st (r : (a, b) Request.t) (errs : b) :
      unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    let request_view = Request.view r in
    let emit_and_return_errors errs =
      let*! () =
        Batcher_events.(emit Worker.request_failed) (request_view, st, errs)
      in
      return_unit
    in
    match r with
    | Request.Register _ -> emit_and_return_errors errs
    | Request.New_head _ -> emit_and_return_errors errs

  let on_completion _w r _ st =
    match Request.view r with
    | Request.View (Register _ | New_head _) ->
        Batcher_events.(emit Worker.request_completed_debug) (Request.view r, st)

  let on_no_request _ = Lwt.return_unit

  let on_close _w = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

let init _conf ~signer node_ctxt =
  let open Lwt_result_syntax in
  let node_ctxt = Node_context.readonly node_ctxt in
  let+ worker = Worker.launch table () {node_ctxt; signer} (module Handlers) in
  Lwt.wakeup worker_waker worker

(* This is a batcher worker for a single scoru *)
let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> ok worker
    | Lwt.Fail _ | Lwt.Sleep -> error Sc_rollup_node_errors.No_batcher)

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Worker_types.Terminated]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let register_messages messages =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  Worker.Queue.push_request_and_wait w (Request.Register messages)
  |> handle_request_error

let new_head b =
  let open Lwt_result_syntax in
  let w = Lazy.force worker in
  match w with
  | Error _ ->
      (* There is no batcher, nothing to do *)
      return_unit
  | Ok w ->
      let*! (_pushed : bool) =
        Worker.Queue.push_request w (Request.New_head b)
      in
      return_unit

let shutdown () =
  let w = Lazy.force worker in
  match w with
  | Error _ ->
      (* There is no batcher, nothing to do *)
      Lwt.return_unit
  | Ok w -> Worker.shutdown w

let get_simulation_state () =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  let state = Worker.state w in
  let+ simulation_ctxt = get_simulation_context state in
  simulation_ctxt.state

let get_simulated_state_value key =
  let open Lwt_result_syntax in
  let* sim_state = get_simulation_state () in
  let*! result = Durable_state.lookup sim_state key in
  return result

let get_simulated_state_subkeys key =
  let open Lwt_result_syntax in
  let* sim_state = get_simulation_state () in
  let*! result = Durable_state.list sim_state key in
  return result
