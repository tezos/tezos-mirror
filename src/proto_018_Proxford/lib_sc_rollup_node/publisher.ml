(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** The rollup node stores and publishes commitments for the PVM every
    [Constants.sc_rollup_commitment_period_in_blocks] levels.

    Every time a finalized block is processed by the rollup node, the latter
    determines whether the last commitment that the node has produced referred
    to [sc_rollup.commitment_period_in_blocks] blocks earlier. For mainnet,
    [sc_rollup.commitment_period_in_blocks = 30]. In this case, it computes and
    stores a new commitment in a level-indexed map.

    Stored commitments are signed by the rollup node operator
    and published on the layer1 chain. To ensure that commitments
    produced by the rollup node are eventually published,
    storing and publishing commitments are decoupled. Every time
    a new head is processed, the node tries to publish the oldest
    commitment that was not published already.
*)

open Protocol
open Alpha_context
open Publisher_worker_types

module Lwt_result_option_syntax = struct
  let ( let** ) a f =
    let open Lwt_result_syntax in
    let* a in
    match a with None -> return_none | Some a -> f a
end

module Lwt_result_option_list_syntax = struct
  (** A small monadic combinator to return an empty list on None results. *)
  let ( let*& ) x f =
    let open Lwt_result_syntax in
    let* x in
    match x with None -> return_nil | Some x -> f x
end

let add_level level increment =
  (* We only use this function with positive increments so it is safe *)
  if increment < 0 then invalid_arg "Commitment.add_level negative increment" ;
  Int32.add level (Int32.of_int increment)

let sub_level level decrement =
  (* We only use this function with positive increments so it is safe *)
  if decrement < 0 then invalid_arg "Commitment.sub_level negative decrement" ;
  let r = Int32.sub level (Int32.of_int decrement) in
  if r < 0l then None else Some r

let sc_rollup_commitment_period node_ctxt =
  node_ctxt.Node_context.current_protocol.constants.sc_rollup
    .commitment_period_in_blocks

let sc_rollup_challenge_window node_ctxt =
  node_ctxt.Node_context.current_protocol.constants.sc_rollup
    .challenge_window_in_blocks

let next_commitment_level node_ctxt last_commitment_level =
  add_level last_commitment_level (sc_rollup_commitment_period node_ctxt)

type state = Node_context.ro

let tick_of_level (node_ctxt : _ Node_context.t) inbox_level =
  let open Lwt_result_syntax in
  let* block = Node_context.get_l2_block_by_level node_ctxt inbox_level in
  return (Sc_rollup_block.final_tick block)

let build_commitment (node_ctxt : _ Node_context.t)
    (prev_commitment : Octez_smart_rollup.Commitment.Hash.t)
    ~prev_commitment_level ~inbox_level ctxt =
  let open Lwt_result_syntax in
  let module PVM = (val Pvm.of_kind node_ctxt.kind) in
  let*! pvm_state = PVM.State.find ctxt in
  let*? pvm_state =
    match pvm_state with
    | Some pvm_state -> Ok pvm_state
    | None ->
        error_with
          "PVM state for commitment at level %ld is not available"
          inbox_level
  in
  let*! compressed_state = PVM.state_hash pvm_state in
  let*! tick = PVM.get_tick pvm_state in
  let* prev_commitment_tick = tick_of_level node_ctxt prev_commitment_level in
  let distance = Z.sub (Sc_rollup.Tick.to_z tick) prev_commitment_tick in
  let number_of_ticks = Z.to_int64 distance in
  let*? () =
    if number_of_ticks = 0L then error_with "A 0-tick commitment is impossible"
    else if number_of_ticks < 0L then
      error_with "Invalid number of ticks for commitment"
    else Ok ()
  in
  return
    Octez_smart_rollup.Commitment.
      {
        predecessor = prev_commitment;
        inbox_level;
        number_of_ticks;
        compressed_state;
      }

let genesis_commitment (node_ctxt : _ Node_context.t) ctxt =
  let open Lwt_result_syntax in
  let module PVM = (val Pvm.of_kind node_ctxt.kind) in
  let*! pvm_state = PVM.State.find ctxt in
  let*? pvm_state =
    match pvm_state with
    | Some pvm_state -> Ok pvm_state
    | None -> error_with "PVM state for genesis commitment is not available"
  in
  let*! compressed_state = PVM.state_hash pvm_state in
  let commitment =
    Octez_smart_rollup.Commitment.
      {
        predecessor = Hash.zero;
        inbox_level = node_ctxt.genesis_info.level;
        number_of_ticks = 0L;
        compressed_state;
      }
  in
  (* Ensure the initial state corresponds to the one of the rollup's in the
     protocol. A mismatch is possible if a wrong external boot sector was
     provided. *)
  let commitment_hash = Octez_smart_rollup.Commitment.hash commitment in
  let+ () =
    fail_unless
      Octez_smart_rollup.Commitment.Hash.(
        commitment_hash = node_ctxt.genesis_info.commitment_hash)
      (Sc_rollup_node_errors.Invalid_genesis_state
         {
           expected = node_ctxt.genesis_info.commitment_hash;
           actual = commitment_hash;
         })
  in
  commitment

let create_commitment_if_necessary (node_ctxt : _ Node_context.t) ~predecessor
    current_level ctxt =
  let open Lwt_result_syntax in
  if current_level = node_ctxt.genesis_info.level then
    let*! () = Commitment_event.compute_commitment current_level in
    let+ genesis_commitment = genesis_commitment node_ctxt ctxt in
    Some genesis_commitment
  else
    let* last_commitment_hash =
      let+ pred = Node_context.get_l2_block node_ctxt predecessor in
      Sc_rollup_block.most_recent_commitment pred.header
      |> Sc_rollup_proto_types.Commitment_hash.of_octez
    in
    let* last_commitment =
      Node_context.get_commitment node_ctxt last_commitment_hash
    in
    let next_commitment_level =
      next_commitment_level node_ctxt last_commitment.inbox_level
    in
    if current_level = next_commitment_level then
      let*! () = Commitment_event.compute_commitment current_level in
      let+ commitment =
        build_commitment
          node_ctxt
          last_commitment_hash
          ~prev_commitment_level:last_commitment.inbox_level
          ~inbox_level:current_level
          ctxt
      in
      Some commitment
    else return_none

let process_head (node_ctxt : _ Node_context.t) ~predecessor
    Layer1.{level; header = _; _} ctxt =
  let open Lwt_result_syntax in
  let current_level = level in
  let* commitment =
    create_commitment_if_necessary node_ctxt ~predecessor current_level ctxt
  in
  match commitment with
  | None -> return_none
  | Some commitment ->
      let* commitment_hash =
        Node_context.save_commitment node_ctxt commitment
      in
      return_some commitment_hash

let missing_commitments (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  let lpc_level =
    match Reference.get node_ctxt.lpc with
    | None -> node_ctxt.genesis_info.level
    | Some lpc -> lpc.inbox_level
  in
  let* head = Node_context.last_processed_head_opt node_ctxt in
  let next_head_level =
    Option.map (fun (b : Sc_rollup_block.t) -> Int32.succ b.header.level) head
  in
  let sc_rollup_challenge_window_int32 =
    sc_rollup_challenge_window node_ctxt |> Int32.of_int
  in
  let rec gather acc (commitment_hash : Sc_rollup.Commitment.Hash.t) =
    let* commitment = Node_context.find_commitment node_ctxt commitment_hash in
    let lcc = Reference.get node_ctxt.lcc in
    match commitment with
    | None -> return acc
    | Some commitment when commitment.inbox_level <= lcc.level ->
        (* Commitment is before or at the LCC, we have reached the end. *)
        return acc
    | Some commitment when commitment.inbox_level <= lpc_level ->
        (* Commitment is before the last published one, we have also reached
           the end because we only publish commitments that are for the inbox
           of a finalized L1 block. *)
        return acc
    | Some commitment ->
        let* published_info =
          Node_context.commitment_published_at_level node_ctxt commitment_hash
        in
        let past_curfew =
          match (published_info, next_head_level) with
          | None, _ | _, None -> false
          | Some {first_published_at_level; _}, Some next_head_level ->
              Int32.sub next_head_level first_published_at_level
              > sc_rollup_challenge_window_int32
        in
        let acc = if past_curfew then acc else commitment :: acc in
        (* We keep the commitment and go back to the previous one. *)
        gather acc commitment.predecessor
  in
  let* finalized_block = Node_context.get_finalized_head_opt node_ctxt in
  match finalized_block with
  | None -> return_nil
  | Some finalized ->
      (* Start from finalized block's most recent commitment and gather all
         commitments that are missing. *)
      let commitment =
        Sc_rollup_block.most_recent_commitment finalized.header
        |> Sc_rollup_proto_types.Commitment_hash.of_octez
      in
      gather [] commitment

let publish_commitment (node_ctxt : _ Node_context.t) ~source
    (commitment : Octez_smart_rollup.Commitment.t) =
  let open Lwt_result_syntax in
  let publish_operation =
    L1_operation.Publish {rollup = node_ctxt.rollup_address; commitment}
  in
  let*! () =
    Commitment_event.publish_commitment
      (Octez_smart_rollup.Commitment.hash commitment)
      commitment.inbox_level
  in
  let* _hash = Injector.add_pending_operation ~source publish_operation in
  return_unit

let on_publish_commitments (node_ctxt : state) =
  let open Lwt_result_syntax in
  let operator = Node_context.get_operator node_ctxt Publish in
  if Node_context.is_accuser node_ctxt then
    (* Accuser does not publish all commitments *)
    return_unit
  else
    match operator with
    | None ->
        (* Configured to not publish commitments *)
        return_unit
    | Some source ->
        let* commitments = missing_commitments node_ctxt in
        List.iter_es (publish_commitment node_ctxt ~source) commitments

let publish_single_commitment node_ctxt
    (commitment : Octez_smart_rollup.Commitment.t) =
  let open Lwt_result_syntax in
  let operator = Node_context.get_operator node_ctxt Publish in
  let lcc = Reference.get node_ctxt.lcc in
  match operator with
  | None ->
      (* Configured to not publish commitments *)
      return_unit
  | Some source ->
      when_ (commitment.inbox_level > lcc.level) @@ fun () ->
      publish_commitment node_ctxt ~source commitment

(* Commitments can only be cemented after [sc_rollup_challenge_window] has
   passed since they were first published. *)
let earliest_cementing_level node_ctxt commitment_hash =
  let open Lwt_result_option_syntax in
  let** {first_published_at_level; _} =
    Node_context.commitment_published_at_level node_ctxt commitment_hash
  in
  return_some
  @@ Int32.add
       first_published_at_level
       (sc_rollup_challenge_window node_ctxt |> Int32.of_int)

(** [latest_cementable_commitment node_ctxt head] is the most recent commitment
      hash that could be cemented in [head]'s successor if:

      - all its predecessors were cemented
      - it would have been first published at the same level as its inbox

      It does not need to be exact but it must be an upper bound on which we can
      start the search for cementable commitments. *)
let latest_cementable_commitment (node_ctxt : _ Node_context.t)
    (head : Sc_rollup_block.t) =
  let open Lwt_result_option_syntax in
  let commitment_hash =
    Sc_rollup_block.most_recent_commitment head.header
    |> Sc_rollup_proto_types.Commitment_hash.of_octez
  in
  let** commitment = Node_context.find_commitment node_ctxt commitment_hash in
  let** cementable_level_bound =
    return
    @@ sub_level commitment.inbox_level (sc_rollup_challenge_window node_ctxt)
  in
  let lcc = Reference.get node_ctxt.lcc in
  if cementable_level_bound <= lcc.level then return_none
  else
    let** cementable_bound_block =
      Node_context.find_l2_block_by_level node_ctxt cementable_level_bound
    in
    let cementable_commitment =
      Sc_rollup_block.most_recent_commitment cementable_bound_block.header
      |> Sc_rollup_proto_types.Commitment_hash.of_octez
    in
    return_some cementable_commitment

let cementable_commitments (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  let open Lwt_result_option_list_syntax in
  let*& head = Node_context.last_processed_head_opt node_ctxt in
  let head_level = head.header.level in
  let lcc = Reference.get node_ctxt.lcc in
  let rec gather acc (commitment_hash : Sc_rollup.Commitment.Hash.t) =
    let* commitment = Node_context.find_commitment node_ctxt commitment_hash in
    match commitment with
    | None -> return acc
    | Some commitment when commitment.inbox_level <= lcc.level ->
        (* If we have moved backward passed or at the current LCC then we have
           reached the end. *)
        return acc
    | Some commitment ->
        let* earliest_cementing_level =
          earliest_cementing_level node_ctxt commitment_hash
        in
        let acc =
          match earliest_cementing_level with
          | None -> acc
          | Some earliest_cementing_level ->
              if earliest_cementing_level > head_level then
                (* Commitments whose cementing level are after the head's
                   successor won't be cementable in the next block. *)
                acc
              else commitment_hash :: acc
        in
        gather acc commitment.predecessor
  in
  (* We start our search from the last possible cementable commitment. This is
     to avoid iterating over a large number of commitments
     ([challenge_window_in_blocks / commitment_period_in_blocks], in the order
     of 10^3 on mainnet). *)
  let*& latest_cementable_commitment =
    latest_cementable_commitment node_ctxt head
  in
  let* cementable = gather [] latest_cementable_commitment in
  match cementable with
  | [] -> return_nil
  | first_cementable :: _ ->
      (* Make sure that the first commitment can be cemented according to the
         Layer 1 node as a failsafe. *)
      let* green_light =
        Plugin.RPC.Sc_rollup.can_be_cemented
          (new Protocol_client_context.wrap_full node_ctxt.cctxt)
          (node_ctxt.cctxt#chain, `Head 0)
          node_ctxt.rollup_address
          first_cementable
      in
      if green_light then return cementable else return_nil

let cement_commitment (node_ctxt : _ Node_context.t) ~source commitment =
  let open Lwt_result_syntax in
  let commitment = Sc_rollup_proto_types.Commitment_hash.to_octez commitment in
  let cement_operation =
    L1_operation.Cement {rollup = node_ctxt.rollup_address; commitment}
  in
  let* _hash = Injector.add_pending_operation ~source cement_operation in
  return_unit

let on_cement_commitments (node_ctxt : state) =
  let open Lwt_result_syntax in
  let operator = Node_context.get_operator node_ctxt Cement in
  match operator with
  | None ->
      (* Configured to not cement commitments *)
      return_unit
  | Some source ->
      let* cementable_commitments = cementable_commitments node_ctxt in
      List.iter_es (cement_commitment node_ctxt ~source) cementable_commitments

module Types = struct
  type nonrec state = state

  type parameters = {node_ctxt : Node_context.ro}
end

module Name = struct
  (* We only have a single committer in the node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = Commitment_event.section @ ["publisher"]

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
    | Request.Publish -> protect @@ fun () -> on_publish_commitments state
    | Request.Cement -> protect @@ fun () -> on_cement_commitments state

  type launch_error = error trace

  let on_launch _w () Types.{node_ctxt} = return node_ctxt

  let on_error (type a b) _w st (r : (a, b) Request.t) (errs : b) :
      unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    let request_view = Request.view r in
    let emit_and_return_errors errs =
      let*! () =
        Commitment_event.Publisher.request_failed request_view st errs
      in
      return_unit
    in
    match r with
    | Request.Publish -> emit_and_return_errors errs
    | Request.Cement -> emit_and_return_errors errs

  let on_completion _w r _ st =
    Commitment_event.Publisher.request_completed (Request.view r) st

  let on_no_request _ = Lwt.return_unit

  let on_close _w = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

let start node_ctxt =
  let open Lwt_result_syntax in
  let*! () = Commitment_event.starting () in
  let node_ctxt = Node_context.readonly node_ctxt in
  let+ worker = Worker.launch table () {node_ctxt} (module Handlers) in
  Lwt.wakeup worker_waker worker

let init node_ctxt =
  let open Lwt_result_syntax in
  match Lwt.state worker_promise with
  | Lwt.Return _ ->
      (* Worker already started, nothing to do. *)
      return_unit
  | Lwt.Fail exn ->
      (* Worker crashed, not recoverable. *)
      fail [Sc_rollup_node_errors.No_publisher; Exn exn]
  | Lwt.Sleep ->
      (* Never started, start it. *)
      start node_ctxt

(* This is a publisher worker for a single scoru *)
let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> ok worker
    | Lwt.Fail _ | Lwt.Sleep -> error Sc_rollup_node_errors.No_publisher)

let publish_commitments () =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  let*! (_pushed : bool) = Worker.Queue.push_request w Request.Publish in
  return_unit

let cement_commitments () =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  let*! (_pushed : bool) = Worker.Queue.push_request w Request.Cement in
  return_unit

let shutdown () =
  let w = Lazy.force worker in
  match w with
  | Error _ ->
      (* There is no publisher, nothing to do *)
      Lwt.return_unit
  | Ok w -> Worker.shutdown w
