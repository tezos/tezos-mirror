(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori     <contact@functori.com>          *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)
module Name = struct
  (* We only have a single batcher in the node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["dal-injection"; "worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Dal_worker_types = struct
  module Request = struct
    type ('a, 'b) t =
      | Register : {message : string} -> (unit, error trace) t
      | Produce_dal_slots : int32 -> (unit, error trace) t
      | Set_dal_slot_indices :
          Tezos_dal_node_services.Types.slot_index list
          -> (unit, error trace) t

    type view = View : _ t -> view

    let view req = View req

    let encoding =
      let open Data_encoding in
      union
        [
          case
            (Tag 0)
            ~title:"Register"
            (obj2
               (req "request" (constant "register"))
               (req "slot_content" string))
            (function
              | View (Register {message}) -> Some ((), message) | _ -> None)
            (fun ((), message) -> View (Register {message}));
          case
            (Tag 1)
            ~title:"Produce_dal_slots"
            (obj2
               (req "request" (constant "produce_dal_slots"))
               (req "level" int32))
            (function
              | View (Produce_dal_slots level) -> Some ((), level) | _ -> None)
            (fun ((), level) -> View (Produce_dal_slots level));
          case
            (Tag 2)
            ~title:"Set_dal_slot_indices"
            (obj2
               (req "request" (constant "set_dal_slot_indices"))
               (req "indices" (list uint8)))
            (function
              | View (Set_dal_slot_indices ids) -> Some ((), ids) | _ -> None)
            (fun ((), ids) -> View (Set_dal_slot_indices ids));
        ]

    let pp ppf (View r) =
      match r with
      | Register _ -> Format.fprintf ppf "register slot injection request"
      | Produce_dal_slots level ->
          Format.fprintf ppf "produce DAL slots at level %ld" level
      | Set_dal_slot_indices idx ->
          Format.fprintf
            ppf
            "set slot indices %a"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
               (fun fmt id -> Format.fprintf fmt "%d" id))
            idx

    let name : type a b. (a, b) t -> string = function
      | Register _ -> "Register"
      | Produce_dal_slots _ -> "Produce_dal_slots"
      | Set_dal_slot_indices _ -> "Set_dal_slot_indices"
  end
end

open Dal_worker_types

module Events = struct
  include Internal_event.Simple

  let section = "smart_rollup_node" :: Name.base

  let injected =
    declare_3
      ~section
      ~name:"injected"
      ~msg:
        "Injected a DAL slot {slot_commitment} at index {slot_index} with hash \
         {operation_hash}"
      ~level:Info
      ~pp1:Tezos_crypto_dal.Cryptobox.Commitment.pp
      ~pp3:Injector_sigs.Id.pp
      ("slot_commitment", Tezos_crypto_dal.Cryptobox.Commitment.encoding)
      ("slot_index", Data_encoding.uint8)
      ("operation_hash", Injector_sigs.Id.encoding)

  let request_failed =
    declare_3
      ~section
      ~name:"request_failed"
      ~msg:"Request {request} failed ({worker_status}): {errors}"
      ~level:Warning
      ("request", Request.encoding)
      ~pp1:Request.pp
      ("worker_status", Worker_types.request_status_encoding)
      ~pp2:Worker_types.pp_status
      ("errors", Error_monad.trace_encoding)
      ~pp3:Error_monad.pp_print_trace

  let dal_message_received =
    declare_0
      ~section
      ~name:"dal_message_received"
      ~msg:"A message to inject via DAL has been added to the injection queue"
      ~level:Info
      ()

  let dal_message_size_too_big =
    declare_2
      ~section
      ~name:"dal_message_size_too_big"
      ~msg:
        "The size of a received message to inject via DAL has {message_size} \
         bytes while slot size is {slot_size}"
      ~level:Info
      ("slot_size", Data_encoding.int31)
      ("message_size", Data_encoding.int31)

  let no_dal_slot_indices_set =
    declare_0
      ~section
      ~name:"no_dal_slot_index_set"
      ~msg:"Cannot inject slot as no DAL slot index is set"
      ~level:Error
      ()

  let inject_dal_slot_from_messages =
    declare_5
      ~section
      ~name:"inject_dal_slot_from_messages"
      ~msg:
        "Injecting a DAL slot containing {num_messages} messages at index \
         {slot_index} on top of level {level}, for a total size of {data_size} \
         bytes over {slot_size}"
      ~level:Info
      ("data_size", Data_encoding.int31)
      ("slot_size", Data_encoding.int31)
      ("num_messages", Data_encoding.uint16)
      ("level", Data_encoding.int32)
      ("slot_index", Data_encoding.uint16)

  let request_completed =
    declare_2
      ~section
      ~name:"request_completed"
      ~msg:"{request} {worker_status}"
      ~level:Debug
      ("request", Request.encoding)
      ("worker_status", Worker_types.request_status_encoding)
      ~pp1:Request.pp
      ~pp2:Worker_types.pp_status
end

module Pending_messages =
  Hash_queue.Make
    (struct
      type t = int
      (* injected messages, with the index of its injection in the pool. *)

      let equal = Int.equal

      let hash = Hashtbl.hash
    end)
    (struct
      type t = string
    end)

module Recent_dal_injections =
  Hash_queue.Make
    (Hashed.Injector_operations_hash)
    (struct
      type t = unit
    end)

module Slot_index_queue = struct
  include Queue

  type t = Tezos_dal_node_services.Types.slot_index Queue.t

  let name = "dal_injection_queue_slot_indices"

  (** If [Tezos_dal_node_services.Types.slot_index] changes its
      underlying definition, this code will fail to compile because of
      the [encoding], preventing data corruption in the store. *)
  module Store = Indexed_store.Make_singleton (struct
    type t = Tezos_dal_node_services.Types.slot_index list

    let name = name

    let encoding = Data_encoding.(list uint8)
  end)

  let load_with_mode ~data_dir mode =
    Store.load ~path:(Filename.concat data_dir name) mode

  let load node_ctxt =
    let open Lwt_result_syntax in
    let data_dir = node_ctxt.Node_context.data_dir in
    let* store = load_with_mode ~data_dir Read_only in
    let* dal_slot_indices_opt = Store.read store in
    match dal_slot_indices_opt with
    | None -> return_none
    | Some dal_slot_indices ->
        let queue = Queue.create () in
        List.iter
          (fun slot_index -> Queue.push slot_index queue)
          dal_slot_indices ;
        return_some queue

  let write node_ctxt slot_indices =
    let open Lwt_result_syntax in
    let data_dir = node_ctxt.Node_context.data_dir in
    let* store = load_with_mode ~data_dir Read_write in
    Store.write store slot_indices

  let replace node_ctxt t slot_indices =
    let open Lwt_result_syntax in
    let current_protocol =
      Reference.get node_ctxt.Node_context.current_protocol
    in
    let number_of_slots = current_protocol.constants.dal.number_of_slots in
    let module SI = Set.Make (Int) in
    (* remove duplicates *)
    let slot_indices = SI.of_list slot_indices in
    let* () =
      SI.iter_es
        (fun slot_index ->
          fail_unless (slot_index >= 0 && slot_index < number_of_slots)
          @@ error_of_fmt "Slot index %d out of range" slot_index)
        slot_indices
    in
    Queue.clear t ;
    SI.iter (fun slot_index -> Queue.push slot_index t) slot_indices ;
    let* () = write node_ctxt (SI.elements slot_indices) in
    return_unit

  let next t =
    (* The pop followed by a push is cycle through slot indices. *)
    let slot_index = Queue.pop t in
    Queue.push slot_index t ;
    slot_index
end

type state = {
  node_ctxt : Node_context.ro;
  dal_node_ctxt : Tezos_dal_node_lib.Dal_node_client.cctxt;
  recent_dal_injections : Recent_dal_injections.t;
  mutable count_messages : int;
  pending_messages : Pending_messages.t;
  dal_slot_indices : Slot_index_queue.t;
      (* We use a queue here to easily cycle through
         the slots and use different ones from a
         level to another (in case we have less data
         to publish than available slot indices). *)
}

let inject_slot state ~slot_index ~slot_content =
  let open Lwt_result_syntax in
  let dal_cctxt = state.dal_node_ctxt in
  let* commitment, operation =
    let* commitment, commitment_proof =
      Tezos_dal_node_lib.Dal_node_client.post_slot
        dal_cctxt
        ~slot_index
        slot_content
    in
    return
      ( commitment,
        L1_operation.Publish_dal_commitment
          {slot_index; commitment; commitment_proof} )
  in
  let* l1_hash =
    Injector.check_and_add_pending_operation
      state.node_ctxt.config.mode
      operation
  in
  let* l1_hash =
    match l1_hash with
    | Some l1_hash -> return l1_hash
    | None ->
        let op = Injector.Inj_operation.make operation in
        return op.id
  in
  let*! () = Events.(emit injected) (commitment, slot_index, l1_hash) in
  Recent_dal_injections.replace state.recent_dal_injections l1_hash () ;
  Metrics.wrap (fun () ->
      Metrics.DAL_batcher.set_dal_injections_queue_length
        (Recent_dal_injections.length state.recent_dal_injections)) ;
  return_unit

let on_register state ~message : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* () =
    if Slot_index_queue.is_empty state.dal_slot_indices then
      (* When there is no DAL slot index on which the worker can
         publish, we reject all requests to register new messages. *)
      let*! () = Events.(emit no_dal_slot_indices_set) () in
      tzfail
      @@ error_of_fmt
           "DAL message registration rejected because the rollup cannot \
            publish on the DAL. Please use the POST /local/dal/slot/indices \
            RPC to set the slot indices on which the rollup node should \
            publish."
    else return_unit
  in
  let slot_size =
    (Reference.get state.node_ctxt.current_protocol).constants.dal
      .cryptobox_parameters
      .slot_size
  in
  let message_size = String.length message in
  if message_size > slot_size then
    let*! () =
      Events.(emit dal_message_size_too_big) (slot_size, message_size)
    in
    tzfail @@ Rollup_node_errors.Dal_message_too_big {slot_size; message_size}
  else (
    Pending_messages.replace state.pending_messages state.count_messages message ;
    Metrics.wrap (fun () ->
        Metrics.DAL_batcher.set_dal_batcher_queue_length
          (Pending_messages.length state.pending_messages)) ;
    (* We don't care about overflows here. *)
    state.count_messages <- state.count_messages + 1 ;
    let*! () = Events.(emit dal_message_received) () in
    return_unit)

let on_set_dal_slot_indices {dal_slot_indices; node_ctxt; _} idx =
  Slot_index_queue.replace node_ctxt dal_slot_indices idx

(* This function pops from the injection queue the maximum number
   of messages that fits in a DAL slot. The messages are returned
   in the order they were popped from the queue (which is also the
   order in which they were pushed in the queue). In the particular
   case where there is no more pending message in the queue, the
   returned list is empty.

   Invariants:
    - String.length (String.concat "" accu) + remaining_size = slot_size
    - List.rev accu @ Pending_chunks.elements is invariant. *)
let fill_slot state ~slot_size =
  let rec fill_slot_aux accu ~remaining_size =
    match Pending_messages.peek state.pending_messages with
    | None -> List.rev accu
    | Some (_z, message) ->
        let remaining_size = remaining_size - String.length message in
        if remaining_size < 0 then (* Max slot size reached. *)
          List.rev accu
        else
          (* Pop the element to remove it. *)
          let () = ignore @@ Pending_messages.take state.pending_messages in
          Metrics.wrap (fun () ->
              Metrics.DAL_batcher.set_dal_batcher_queue_length
                (Pending_messages.length state.pending_messages)) ;
          fill_slot_aux (message :: accu) ~remaining_size
  in
  fill_slot_aux [] ~remaining_size:slot_size

(* This function pops from the injection queue the maximum number of
   messages that fits in remaining_num_slots DAL slots (or less if there
   are not enough messages in the queue to fill that many slots). It
   returns an association list mapping slot indices from state.dal_slot_indices
   to lists of messages produced by the fill_slot function. *)
let fill_slots state ~slot_size ~num_slots =
  let rec fill_slots_aux accu ~remaining_num_slots =
    if remaining_num_slots <= 0 then
      (* All available slot indices are used. *)
      accu
    else
      (* There is at least one remaining available slot index. *)
      match fill_slot state ~slot_size with
      | [] -> accu (* But, there is no data to publish. *)
      | slot_messages ->
          (* We still have data to publish. Let's associate it to the next DAL
             slot index. *)
          let slot_index = Slot_index_queue.next state.dal_slot_indices in
          fill_slots_aux
            ((slot_index, slot_messages) :: accu)
            ~remaining_num_slots:(remaining_num_slots - 1)
  in
  fill_slots_aux [] ~remaining_num_slots:num_slots

(* This function is called to produce slots from pending message using the
   function {!fill_slots} above. It then publishes those slots on DAL and posts
   their commitments to L1. *)
let on_produce_dal_slots state ~level =
  let open Lwt_result_syntax in
  if Pending_messages.is_empty state.pending_messages then return_unit
  else if Slot_index_queue.is_empty state.dal_slot_indices then
    (* No provided slot indices, no injection *)
    let*! () = Events.(emit no_dal_slot_indices_set) () in
    return_unit
  else
    let slot_size =
      (Reference.get state.node_ctxt.current_protocol).constants.dal
        .cryptobox_parameters
        .slot_size
    in
    let num_slots = Slot_index_queue.length state.dal_slot_indices in
    (* Inject on all DAL slot indices in parallel. *)
    fill_slots state ~slot_size ~num_slots
    |> List.iter_ep (fun (slot_index, slot_chunks) ->
           let slot_content = String.concat "" slot_chunks in
           let*! () =
             Events.(emit inject_dal_slot_from_messages)
               ( String.length slot_content,
                 slot_size,
                 List.length slot_chunks,
                 level,
                 slot_index )
           in
           inject_slot state ~slot_index ~slot_content)

let init_dal_worker_state node_ctxt dal_node_ctxt =
  let open Lwt_result_syntax in
  let dal_constants =
    let open Node_context in
    (Reference.get node_ctxt.current_protocol).constants.dal
  in
  (* Etherlink serves as primary user for the batcher service, as such its
       constants are targetted for its usage for now. *)
  let etherlink_max_pending_messages_size =
    let maximum_number_of_chunks_per_blueprint = 128 in
    let maximum_number_of_blueprints_per_second = 2 in
    let blueprint_expiration_in_seconds =
      (* We should keep the messages in the injection queue
         for at least one L1 level. There is no upper bound
         to the time between blocks but having blocks more
         than 10 minutes apart is extremely rare *)
      600
    in
    maximum_number_of_chunks_per_blueprint
    * maximum_number_of_blueprints_per_second * blueprint_expiration_in_seconds
  in
  let max_remembered_recent_injections =
    dal_constants.number_of_slots * dal_constants.attestation_lag
    * 5 (* Give more chance for DAL slots to be signaled. *)
  in
  let* dal_slot_indices_opt = Slot_index_queue.load node_ctxt in
  let dal_slot_indices =
    Option.value_f dal_slot_indices_opt ~default:Slot_index_queue.create
  in
  return
    {
      node_ctxt;
      dal_node_ctxt;
      recent_dal_injections =
        Recent_dal_injections.create max_remembered_recent_injections;
      pending_messages =
        Pending_messages.create etherlink_max_pending_messages_size;
      dal_slot_indices;
      count_messages = 0;
    }

module Types = struct
  type nonrec state = state

  type parameters = {
    node_ctxt : Node_context.ro;
    dal_node_ctxt : Tezos_dal_node_lib.Dal_node_client.cctxt;
  }
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
    | Request.Register {message} ->
        protect @@ fun () -> on_register state ~message
    | Request.Produce_dal_slots level ->
        protect @@ fun () -> on_produce_dal_slots state ~level
    | Request.Set_dal_slot_indices idx ->
        protect @@ fun () -> on_set_dal_slot_indices state idx

  type launch_error = error trace

  let on_launch _w () Types.{node_ctxt; dal_node_ctxt} =
    init_dal_worker_state node_ctxt dal_node_ctxt

  let on_error (type a b) _w st (r : (a, b) Request.t) (errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    let open Lwt_result_syntax in
    match r with
    | Request.Register _ ->
        let*! () = Events.(emit request_failed) (Request.view r, st, errs) in
        return `Continue
    | Request.Produce_dal_slots _ ->
        let*! () = Events.(emit request_failed) (Request.view r, st, errs) in
        return `Continue
    | Request.Set_dal_slot_indices _ ->
        let*! () = Events.(emit request_failed) (Request.view r, st, errs) in
        return `Continue

  let on_completion _w r _ st =
    match Request.view r with
    | Request.View (Register _ | Produce_dal_slots _ | Set_dal_slot_indices _)
      ->
        Events.(emit request_completed) (Request.view r, st)

  let on_no_request _ = Lwt.return_unit

  let on_close _w = Lwt.return_unit
end

let table = Worker.create_table Queue

let (worker_promise : Worker.infinite Worker.queue Worker.t Lwt.t), worker_waker
    =
  Lwt.task ()

let start node_ctxt dal_node_ctxt =
  let open Lwt_result_syntax in
  let node_ctxt = Node_context.readonly node_ctxt in
  let+ worker =
    Worker.launch table () {node_ctxt; dal_node_ctxt} (module Handlers)
  in
  Lwt.wakeup worker_waker worker

let start_in_mode mode =
  let open Configuration in
  match mode with
  | Batcher | Operator -> true
  | Observer | Accuser | Bailout | Maintenance -> false
  | Custom ops -> purposes_matches_mode (Custom ops) [Batching]

let init (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  match Lwt.state worker_promise with
  | Lwt.Return _ ->
      (* Worker already started, nothing to do. *)
      return_unit
  | Lwt.Fail exn ->
      (* Worker crashed, not recoverable. *)
      fail [Rollup_node_errors.No_dal_injector; Exn exn]
  | Lwt.Sleep -> (
      (* Never started, start it. *)
      match node_ctxt.Node_context.dal_cctxt with
      | None -> return_unit
      | Some dal_node_ctxt ->
          if start_in_mode node_ctxt.config.mode then
            start node_ctxt dal_node_ctxt
          else return_unit)

(* This is a DAL injection worker for a single scoru *)
let worker () =
  let open Result_syntax in
  match Lwt.state worker_promise with
  | Lwt.Return worker -> return worker
  | Lwt.Fail exn -> tzfail (Error_monad.error_of_exn exn)
  | Lwt.Sleep -> tzfail Rollup_node_errors.No_dal_injector

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Worker_types.Terminated]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let register_dal_message w message =
  Worker.Queue.push_request_and_wait w (Request.Register {message})
  |> handle_request_error

let register_dal_messages ~messages =
  let open Lwt_result_syntax in
  let*? w = worker () in
  List.iter_es (register_dal_message w) messages

let produce_dal_slots ~level =
  let open Lwt_result_syntax in
  match worker () with
  | Error (Rollup_node_errors.No_dal_injector :: _) ->
      (* There is no batcher, nothing to do *)
      return_unit
  | Error e -> fail e
  | Ok w ->
      Worker.Queue.push_request_and_wait w Request.(Produce_dal_slots level)
      |> handle_request_error

let set_dal_slot_indices idx =
  let open Lwt_result_syntax in
  let*? w = worker () in
  Worker.Queue.push_request_and_wait w Request.(Set_dal_slot_indices idx)
  |> handle_request_error

let get_injection_ids () =
  let open Result_syntax in
  let+ w = worker () in
  let state = Worker.state w in
  Recent_dal_injections.keys state.recent_dal_injections

let forget_injection_id id =
  let open Result_syntax in
  let+ w = worker () in
  let state = Worker.state w in
  let () = Recent_dal_injections.remove state.recent_dal_injections id in
  Metrics.wrap (fun () ->
      Metrics.DAL_batcher.set_dal_injections_queue_length
        (Recent_dal_injections.length state.recent_dal_injections))
