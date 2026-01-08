(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Batcher_worker_types
module Message_heap = Bounded_min_heap.Make (L2_message.Id) (L2_message)

module Batcher_events = Batcher_events.Declare (struct
  let worker_name = "batcher"
end)

module L2_batched_message = struct
  type t = {content : string; l1_id : Injector.Inj_operation.id}
end

module Batched_messages = Hash_queue.Make (L2_message.Id) (L2_batched_message)

type status = Pending_batch | Batched of Injector.Inj_operation.id

type state = {
  node_ctxt : Node_context.ro;
  messages_heap : Message_heap.t;
  batched : Batched_messages.t;
  mutable plugin : (module Protocol_plugin_sig.S);
}

let message_size s =
  (* Encoded as length of s on 4 bytes + s *)
  4 + String.length s

let link_batch_scope scope batch =
  Opentelemetry.Scope.add_links scope @@ fun () ->
  let scope_link = Opentelemetry.Scope.to_span_link scope in
  List.filter_map
    (fun msg ->
      match L2_message.scope msg with
      | None -> None
      | Some msg_scope ->
          Opentelemetry.Scope.add_links msg_scope (fun () -> [scope_link]) ;
          Some (Opentelemetry.Scope.to_span_link msg_scope))
    batch

let add_etherlink_attrs ?scope state order =
  if state.node_ctxt.config.etherlink then
    match (order, Opentelemetry.Scope.get_ambient_scope ?scope ()) with
    | None, _ | _, None -> ()
    | Some order, Some scope ->
        Opentelemetry.Scope.add_attrs scope @@ fun () ->
        (* The sequencer sets the order as the etherlink block number for
           them to be injected on L1 in order, so we reuse that for the
           trace here. *)
        [("etherlink.block.number", `Int (Z.to_int order))]

let add_etherlink_events name state scope msgs =
  if state.node_ctxt.config.etherlink then
    List.iter
      (fun msg ->
        match L2_message.order msg with
        | Some order ->
            Opentelemetry.Scope.add_event scope @@ fun () ->
            Opentelemetry_lwt.Event.make
              ~attrs:[("etherlink.block.number", `Int (Z.to_int order))]
              name
        | _ -> ())
      msgs

let inject_batch ?order state (l2_messages : L2_message.t list) =
  let open Lwt_result_syntax in
  Octez_telemetry.Trace.with_tzresult ~service_name:"Batcher" "inject_batch"
  @@ fun scope ->
  Opentelemetry.Scope.add_attrs scope (fun () ->
      match order with
      | None -> []
      | Some order ->
          [("rollup_node.batcher.inject.order", `Int (Z.to_int order))]) ;
  add_etherlink_attrs ~scope state order ;
  link_batch_scope scope l2_messages ;
  add_etherlink_events
    "rollup_node.batcher.inject_etherlink_block"
    state
    scope
    l2_messages ;
  let messages = List.map L2_message.content l2_messages in
  let operation = L1_operation.Add_messages {messages} in
  let* l1_id =
    Injector.check_and_add_pending_operation
      ?order
      state.node_ctxt.config.mode
      operation
  in
  let+ l1_id =
    match l1_id with
    | Some l1_id -> return l1_id
    | None ->
        let op = Injector.Inj_operation.make operation in
        return op.id
  in
  List.iter
    (fun msg ->
      let content = L2_message.content msg in
      let id = L2_message.id msg in
      Batched_messages.replace state.batched id {content; l1_id})
    l2_messages

let inject_batches state =
  List.iter_es (fun (min_order, batch) ->
      (* We inject that batch taking the smallest order of that batch,
         i.e. the one of the first element. *)
      inject_batch ?order:min_order state batch)

let max_batch_size {node_ctxt; plugin; _} =
  let module Plugin = (val plugin) in
  Option.value
    node_ctxt.config.batcher.max_batch_size
    ~default:Plugin.Batcher_constants.protocol_max_batch_size

let get_batches state ~only_full =
  let max_batch_size = max_batch_size state in
  let max_batch_elements = state.node_ctxt.config.batcher.max_batch_elements in
  let heap = state.messages_heap in
  let add_batch ~min_order rev_batch rev_batches =
    if List.is_empty rev_batch then rev_batches
    else
      let batch = List.rev rev_batch in
      (min_order, batch) :: rev_batches
  in
  let rec pop_until_enough ~is_first
      (min_order, rev_batch, batch_size, batch_elements) rev_batches =
    let message = Message_heap.peek_min heap in
    match message with
    | None -> add_batch ~min_order rev_batch rev_batches
    | Some message ->
        let min_order =
          (* first element of the batch has the lower order. We only
             care about this one.*)
          if is_first then L2_message.order message else min_order
        in
        let size = message_size (L2_message.content message) in
        let new_batch_size = batch_size + size in
        let new_batch_elements = batch_elements + 1 in
        if
          new_batch_size <= max_batch_size
          && new_batch_elements <= max_batch_elements
        then
          (* pop the message as we only peeked it. *)
          let _message = Message_heap.pop heap in
          (* We can add the message to the current batch because we are still
             within the bounds. *)
          pop_until_enough
            ~is_first:false
            (min_order, message :: rev_batch, new_batch_size, new_batch_elements)
            rev_batches
        else
          (* we haven't pop the message, so we can safely call
             continue_or_stop. *)
          continue_or_stop (add_batch ~min_order rev_batch rev_batches)
  and continue_or_stop rev_batches =
    if
      only_full
      && Message_heap.length heap
         < state.node_ctxt.config.batcher.min_batch_elements
    then rev_batches
    else pop_until_enough ~is_first:true (None, [], 0, 0) rev_batches
  in
  continue_or_stop [] |> List.rev

let produce_batches state ~only_full =
  let open Lwt_result_syntax in
  Octez_telemetry.Trace.with_tzresult ~service_name:"Batcher" "produce_batches"
  @@ fun _ ->
  let start_timestamp = Time.System.now () in
  let batches = get_batches state ~only_full in
  let get_timestamp = Time.System.now () in
  let nb_messages_batched =
    List.fold_left (fun len (_min_order, l) -> len + List.length l) 0 batches
  in
  Metrics.wrap (fun () ->
      Metrics.Batcher.set_messages_queue_size
      @@ Message_heap.length state.messages_heap ;
      Metrics.Batcher.set_messages_size @@ nb_messages_batched ;
      Metrics.Batcher.set_batches_size @@ List.length batches ;
      Metrics.Batcher.set_get_time @@ Ptime.diff get_timestamp start_timestamp) ;
  if List.is_empty batches then return_unit
  else
    let* () =
      Metrics.wrap_lwt (fun () ->
          Metrics.Batcher.set_last_batch_time start_timestamp ;
          let* block = Node_context.last_processed_head_opt state.node_ctxt in
          let () =
            match block with
            | Some block ->
                Metrics.Batcher.set_last_batch_level block.header.level
            | None -> ()
          in
          return_unit)
    in
    let* () = inject_batches state batches in
    let*! () =
      Batcher_events.(emit batched) (List.length batches, nb_messages_batched)
    in
    Metrics.wrap (fun () ->
        let inject_timestamp = Time.System.now () in
        Metrics.Batcher.set_inject_time
        @@ Ptime.diff inject_timestamp get_timestamp) ;
    return_unit

let message_already_batched state msg =
  match Batched_messages.find_opt state.batched (L2_message.id msg) with
  | None -> false (* check is done in insertion in the heap *)
  | Some {l1_id; _} ->
      (* If injector know about the operation then it's either
         pending, injected or included and we don't re-inject the
         operation. If the injector has no status for that operation,
         then it's consider too old (more than the injector's
         `retention_period`) and the user wants to re-inject it. *)
      Option.is_some @@ Injector.operation_status l1_id

let make_l2_messages ?order ~unique state (messages : string list) =
  let module Plugin = (val state.plugin) in
  let max_size_msg =
    min
      (Plugin.Batcher_constants.message_size_limit
     + 4 (* We add 4 because [message_size] adds 4. *))
      (max_batch_size state)
  in
  let scope = Opentelemetry.Scope.get_ambient_scope () in
  List.mapi_e
    (fun i message ->
      if message_size message > max_size_msg then
        error_with "Message %d is too large (max size is %d)" i max_size_msg
      else Ok (L2_message.make ?order ?scope ~unique message))
    messages

type error += Heap_insertion_failed of string

let () =
  register_error_kind
    ~id:"batcher.heap_insertion_failed"
    ~title:"Heap insertion failed"
    ~description:"Heap insertion failed"
    ~pp:(fun ppf err -> Format.fprintf ppf "Heap insertion failed with %s." err)
    `Permanent
    Data_encoding.(obj1 (req "error" string))
    (function Heap_insertion_failed error -> Some error | _ -> None)
    (fun error -> Heap_insertion_failed error)

let add_messages_into_heap ~drop_duplicate state =
  let open Lwt_result_syntax in
  List.map_es @@ fun message ->
  let msg_id = L2_message.id message in
  let* () =
    (* check if already batched *)
    if drop_duplicate && message_already_batched state message then
      let*! () = Batcher_events.(emit dropped_msg) msg_id in
      return_unit
    else
      let*? () =
        Result.map_error (fun err_msg -> [Heap_insertion_failed err_msg])
        @@ Message_heap.insert message state.messages_heap
      in
      return_unit
  in
  return msg_id

let on_register ?order ~drop_duplicate state (messages : string list) =
  let open Lwt_result_syntax in
  let module Plugin = (val state.plugin) in
  add_etherlink_attrs state order ;
  let*? messages =
    make_l2_messages ?order ~unique:(not drop_duplicate) state messages
  in
  let*! () = Batcher_events.(emit queue) (List.length messages) in
  let* ids = add_messages_into_heap ~drop_duplicate state messages in
  let+ () = produce_batches state ~only_full:true in
  ids

let on_new_head state = produce_batches state ~only_full:false

let clear_queues {messages_heap; batched; _} =
  Message_heap.clear messages_heap ;
  Batched_messages.clear batched

let remove_messages
    ({drop_no_order; order_below} : Batcher_worker_types.order_request)
    {messages_heap; _} =
  let predicate_f message =
    Option.map
      (fun op_order -> Z.leq op_order order_below)
      (L2_message.order message)
    |> Option.value ~default:drop_no_order
  in
  let _ids = Message_heap.remove_predicate predicate_f messages_heap in
  ()

let init_batcher_state plugin node_ctxt =
  {
    node_ctxt;
    messages_heap = Message_heap.create 100_000 (* ~ 400MB *);
    batched = Batched_messages.create 100_000 (* ~ 400MB *);
    plugin;
  }

module Types = struct
  type nonrec state = state

  type parameters = {
    node_ctxt : Node_context.ro;
    plugin : (module Protocol_plugin_sig.S);
  }
end

module Name = struct
  (* We only have a single batcher in the node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = Batcher_events.Worker.section @ ["worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Worker = Octez_telemetry.Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  let on_request : type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Register {order; messages; drop_duplicate} ->
        protect @@ fun () -> on_register ?order ~drop_duplicate state messages
    | Request.Produce_batches -> protect @@ fun () -> on_new_head state
    | Request.Clear_queues ->
        protect @@ fun () ->
        clear_queues state ;
        Lwt_result_syntax.return_unit
    | Request.Remove_messages order_request ->
        protect @@ fun () ->
        remove_messages order_request state ;
        Lwt_result_syntax.return_unit

  type launch_error = error trace

  let on_launch _w () Types.{node_ctxt; plugin} =
    let open Lwt_result_syntax in
    let state = init_batcher_state plugin node_ctxt in
    return state

  let on_error (type a b) _w st (r : (a, b) Request.t) (errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    let open Lwt_result_syntax in
    let request_view = Request.view r in
    let emit_and_return_errors errs =
      let*! () =
        Batcher_events.(emit Worker.request_failed) (request_view, st, errs)
      in
      return `Continue
    in
    match r with
    | Request.Register _ -> emit_and_return_errors errs
    | Request.Produce_batches -> emit_and_return_errors errs
    | Request.Clear_queues -> emit_and_return_errors errs
    | Request.Remove_messages _ -> emit_and_return_errors errs

  let on_completion _w r _ st =
    match Request.view r with
    | Request.View
        (Register _ | Produce_batches | Clear_queues | Remove_messages _) ->
        Batcher_events.(emit Worker.request_completed_debug) (Request.view r, st)

  let on_no_request _ = Lwt.return_unit

  let on_close _w = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

let check_batcher_config (module Plugin : Protocol_plugin_sig.S)
    Configuration.{max_batch_size; _} =
  match max_batch_size with
  | Some m when m > Plugin.Batcher_constants.protocol_max_batch_size ->
      error_with
        "batcher.max_batch_size must be smaller than %d"
        Plugin.Batcher_constants.protocol_max_batch_size
  | _ -> Ok ()

let start plugin node_ctxt =
  let open Lwt_result_syntax in
  let*? () =
    check_batcher_config plugin node_ctxt.Node_context.config.batcher
  in
  let node_ctxt = Node_context.readonly node_ctxt in
  let+ worker = Worker.launch table () {node_ctxt; plugin} (module Handlers) in
  Lwt.wakeup worker_waker worker

let start_in_mode mode =
  let open Configuration in
  match mode with
  | Batcher | Operator -> true
  | Observer | Accuser | Bailout | Maintenance -> false
  | Custom ops -> purposes_matches_mode (Custom ops) [Batching]

let init plugin (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  match Lwt.state worker_promise with
  | Lwt.Return _ ->
      (* Worker already started, nothing to do. *)
      return_unit
  | Lwt.Fail exn ->
      (* Worker crashed, not recoverable. *)
      fail [Rollup_node_errors.No_batcher; Exn exn]
  | Lwt.Sleep ->
      (* Never started, start it. *)
      if start_in_mode node_ctxt.config.mode then start plugin node_ctxt
      else return_unit

(* This is a batcher worker for a single scoru *)
let worker () =
  let open Result_syntax in
  match Lwt.state worker_promise with
  | Lwt.Return worker -> return worker
  | Lwt.Fail exn -> tzfail (Error_monad.error_of_exn exn)
  | Lwt.Sleep -> tzfail Rollup_node_errors.No_batcher

let active () =
  match Lwt.state worker_promise with
  | Lwt.Return _ -> true
  | Lwt.Fail _ | Lwt.Sleep -> false

let find_message id =
  let open Result_syntax in
  let+ w = worker () in
  let state = Worker.state w in
  Message_heap.find_opt id state.messages_heap

let get_queue () =
  let open Result_syntax in
  let+ w = worker () in
  let state = Worker.state w in
  Message_heap.elements state.messages_heap

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Worker_types.Terminated]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let register_messages ?order ~drop_duplicate messages =
  let open Lwt_result_syntax in
  let*? w = worker () in
  Octez_telemetry.Trace.with_tzresult
    ~service_name:"Batcher"
    "register_messages"
  @@ fun _ ->
  Worker.Queue.push_request_and_wait
    w
    (Request.Register {order; messages; drop_duplicate})
  |> handle_request_error

let produce_batches () =
  let open Lwt_result_syntax in
  match worker () with
  | Error (Rollup_node_errors.No_batcher :: _) ->
      (* There is no batcher, nothing to do *)
      return_unit
  | Error e -> fail e
  | Ok w ->
      Worker.Queue.push_request_and_wait w Request.Produce_batches
      |> handle_request_error

let clean_queue ?order_request () =
  let open Lwt_result_syntax in
  let*? w = worker () in
  let* () =
    (match order_request with
    | None -> Worker.Queue.push_request_and_wait w Request.Clear_queues
    | Some order_request ->
        Worker.Queue.push_request_and_wait
          w
          (Request.Remove_messages order_request))
    |> handle_request_error
  in
  return_unit

let shutdown () =
  match worker () with
  | Error _ ->
      (* There is no batcher, nothing to do *)
      Lwt.return_unit
  | Ok w -> Worker.shutdown w

let message_status state msg_id =
  match Message_heap.find_opt msg_id state.messages_heap with
  | Some msg -> Some (Pending_batch, L2_message.content msg)
  | None -> (
      match Batched_messages.find_opt state.batched msg_id with
      | Some {content; l1_id} -> Some (Batched l1_id, content)
      | None -> None)

let message_status msg_id =
  let open Result_syntax in
  let+ w = worker () in
  let state = Worker.state w in
  message_status state msg_id

let worker_status () =
  match Lwt.state worker_promise with
  | Lwt.Return _ -> `Running
  | Lwt.Fail exn -> `Crashed exn
  | Lwt.Sleep -> `Not_running
