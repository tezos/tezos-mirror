(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

open Publisher_worker_types

module Lwt_result_option_syntax = struct
  let ( let** ) a f =
    let open Lwt_result_syntax in
    let* a in
    match a with None -> return_none | Some a -> f a

  let fail = Lwt_result_syntax.return_none

  let return x = Lwt_result_syntax.return_some x
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
  (* Publishing commitments is done w.r.t. the period in the protocol in which
     the commitment is published, and not the one for the inbox level. *)
  (Reference.get node_ctxt.Node_context.current_protocol).constants.sc_rollup
    .commitment_period_in_blocks

let sc_rollup_challenge_window node_ctxt =
  (Reference.get node_ctxt.Node_context.current_protocol).constants.sc_rollup
    .challenge_window_in_blocks

let next_commitment_level node_ctxt last_commitment_level =
  add_level last_commitment_level (sc_rollup_commitment_period node_ctxt)

type state = {
  node_ctxt : Node_context.rw;
  inbox_checked : unit Commitment.Hash.Table.t;
}

let tick_of_level (node_ctxt : _ Node_context.t) inbox_level =
  let open Lwt_result_syntax in
  let* block = Node_context.get_l2_block_by_level node_ctxt inbox_level in
  return (Sc_rollup_block.final_tick block)

let build_commitment (module Plugin : Protocol_plugin_sig.S)
    (node_ctxt : _ Node_context.t)
    (prev_commitment : Octez_smart_rollup.Commitment.Hash.t)
    ~prev_commitment_level ~inbox_level ctxt =
  let open Lwt_result_syntax in
  let*! pvm_state = Context.PVMState.find ctxt in
  let*? pvm_state =
    match pvm_state with
    | Some pvm_state -> Ok pvm_state
    | None ->
        error_with
          "PVM state for commitment at level %ld is not available"
          inbox_level
  in
  let*! compressed_state = Plugin.Pvm.state_hash node_ctxt.kind pvm_state in
  let*! tick = Plugin.Pvm.get_tick node_ctxt.kind pvm_state in
  let* prev_commitment_tick = tick_of_level node_ctxt prev_commitment_level in
  let distance = Z.sub tick prev_commitment_tick in
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

let genesis_pvm_state (module Plugin : Protocol_plugin_sig.S)
    (node_ctxt : _ Node_context.t) ctxt =
  let open Lwt_result_syntax in
  match
    (node_ctxt.unsafe_patches
      :> (Pvm_patches.unsafe_patch * Pvm_patches.kind) list)
  with
  | [] -> (
      let*! pvm_state = Context.PVMState.find ctxt in
      match pvm_state with
      | Some pvm_state -> return pvm_state
      | None -> failwith "PVM state for genesis commitment is not available")
  | _ ->
      let mode = Context.access_mode_state ctxt in
      (* If there are unsafe patches that were applied to the genesis PVM state,
         we instead recompute the unpatched version to derive the commitment as
         all the following ones will need to be chained to it. *)
      let+ (Unpatched state) =
        Interpreter.genesis_state (Unpatched mode) (module Plugin) node_ctxt
      in
      state

let genesis_commitment (module Plugin : Protocol_plugin_sig.S)
    (node_ctxt : _ Node_context.t) ctxt =
  let open Lwt_result_syntax in
  let* pvm_state = genesis_pvm_state (module Plugin) node_ctxt ctxt in
  let*! compressed_state = Plugin.Pvm.state_hash node_ctxt.kind pvm_state in
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
      (* The protocol part of the RISC-V PVM is not yet synchronised with the
       * node implementation. Re-enable once solved:
       * https://linear.app/tezos/issue/RV-98/re-enable-genesis-commitment-hash-check *)
      (node_ctxt.kind = Kind.Riscv
      || Octez_smart_rollup.Commitment.Hash.(
           commitment_hash = node_ctxt.genesis_info.commitment_hash))
      (Rollup_node_errors.Invalid_genesis_state
         {
           expected = node_ctxt.genesis_info.commitment_hash;
           actual = commitment_hash;
           actual_state_hash = compressed_state;
         })
  in
  commitment

let create_commitment_if_necessary plugin (node_ctxt : _ Node_context.t)
    ~predecessor current_level ctxt =
  let open Lwt_result_syntax in
  if current_level = node_ctxt.genesis_info.level then
    let*! () = Commitment_event.compute_commitment current_level in
    let+ genesis_commitment = genesis_commitment plugin node_ctxt ctxt in
    Some genesis_commitment
  else
    let* last_commitment_hash =
      let+ pred = Node_context.get_l2_block node_ctxt predecessor in
      Sc_rollup_block.most_recent_commitment pred.header
    in
    let* last_commitment =
      Node_context.get_commitment node_ctxt last_commitment_hash
    in
    let next_commitment_level =
      next_commitment_level node_ctxt last_commitment.inbox_level
    in
    if current_level = next_commitment_level then
      let*! () = Commitment_event.compute_commitment current_level in
      let* commitment =
        build_commitment
          plugin
          node_ctxt
          last_commitment_hash
          ~prev_commitment_level:last_commitment.inbox_level
          ~inbox_level:current_level
          ctxt
      in
      return_some commitment
    else return_none

let process_head plugin (node_ctxt : _ Node_context.t) ~predecessor
    Layer1.{level; header = _; _} ctxt =
  let open Lwt_result_syntax in
  let current_level = level in
  let* commitment =
    create_commitment_if_necessary
      plugin
      node_ctxt
      ~predecessor
      current_level
      ctxt
  in
  match commitment with
  | None -> return_none
  | Some commitment ->
      let*! () =
        Commitment_event.new_commitment
          (Octez_smart_rollup.Commitment.hash commitment)
          commitment.inbox_level
      in
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
  let* head = Node_context.last_processed_head_opt node_ctxt
  and* finalized_level = Node_context.get_finalized_level node_ctxt in
  let next_head_level =
    Option.map (fun (b : Sc_rollup_block.t) -> Int32.succ b.header.level) head
  in
  let sc_rollup_challenge_window_int32 =
    sc_rollup_challenge_window node_ctxt |> Int32.of_int
  in
  let rec gather acc (commitment_hash : Commitment.Hash.t) =
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
        let is_finalized = commitment.inbox_level <= finalized_level in
        (* Only publish commitments that are finalized and not past the curfew *)
        let acc =
          if is_finalized && not past_curfew then
            (commitment_hash, commitment) :: acc
          else acc
        in
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
      in
      gather [] commitment

let publish_commitment (node_ctxt : _ Node_context.t)
    (commitment_hash, (commitment : Octez_smart_rollup.Commitment.t)) =
  let open Lwt_result_syntax in
  let publish_operation =
    L1_operation.Publish
      {rollup = node_ctxt.config.sc_rollup_address; commitment}
  in
  let*! () =
    Commitment_event.publish_commitment commitment_hash commitment.inbox_level
  in
  let* _hash =
    Injector.check_and_add_pending_operation
      node_ctxt.config.mode
      publish_operation
  in
  return_unit

let inject_recover_bond (node_ctxt : _ Node_context.t)
    (staker : Signature.Public_key_hash.t) =
  let open Lwt_result_syntax in
  let recover_operation =
    L1_operation.Recover_bond
      {rollup = node_ctxt.config.sc_rollup_address; staker}
  in
  let*! () = Commitment_event.recover_bond staker in
  let* _hash =
    Injector.check_and_add_pending_operation
      node_ctxt.config.mode
      recover_operation
  in
  return_unit

let check_l1_inbox (module Plugin : Protocol_plugin_sig.S) node_ctxt
    (commitment : Commitment.t) =
  let open Lwt_result_syntax in
  let* inbox =
    Node_context.get_inbox_by_level node_ctxt commitment.inbox_level
  in
  Plugin.Inbox.same_as_layer_1 node_ctxt commitment.inbox_level inbox

let run_inbox_checks {node_ctxt; inbox_checked} commitments =
  let open Lwt_result_syntax in
  unless (commitments = []) @@ fun () ->
  let commitments = List.to_seq commitments in
  (* Narrow table to commitments we care about *)
  let com_set = commitments |> Seq.map fst |> Commitment.Hash.Set.of_seq in
  Commitment.Hash.Table.filter_map_inplace
    (fun hash checked ->
      if Commitment.Hash.Set.mem hash com_set then Some checked else None)
    inbox_checked ;
  (* Run inbox checks for new commitments *)
  let*? plugin =
    Protocol_plugins.proto_plugin_for_protocol
      (Reference.get node_ctxt.current_protocol).hash
  in
  Seq.iter_ep
    (fun (hash, (commitment : Commitment.t)) ->
      match Commitment.Hash.Table.find inbox_checked hash with
      | Some () ->
          (* Already checked *)
          return_unit
      | None ->
          let* () = check_l1_inbox plugin node_ctxt commitment in
          Commitment.Hash.Table.replace inbox_checked hash () ;
          return_unit)
    commitments

let on_publish_commitments ({node_ctxt; _} as state) =
  let open Lwt_result_syntax in
  let* commitments = missing_commitments node_ctxt in
  let* () = run_inbox_checks state commitments in
  List.iter_es (publish_commitment node_ctxt) commitments

let publish_single_commitment (node_ctxt : _ Node_context.t)
    (commitment : Octez_smart_rollup.Commitment.t) =
  let open Lwt_result_syntax in
  let lcc = Reference.get node_ctxt.lcc in
  when_ (commitment.inbox_level > lcc.level) @@ fun () ->
  let hash = Commitment.hash commitment in
  let*? plugin =
    Protocol_plugins.proto_plugin_for_protocol
      (Reference.get node_ctxt.current_protocol).hash
  in
  let* () = check_l1_inbox plugin node_ctxt commitment in
  publish_commitment node_ctxt (hash, commitment)

let recover_bond node_ctxt =
  let open Lwt_result_syntax in
  let operator = Node_context.get_operator node_ctxt Operating in
  match operator with
  | None ->
      (* No known operator to recover bond for. *)
      return_unit
  | Some (Single committer) -> inject_recover_bond node_ctxt committer

(* Commitments can only be cemented after [sc_rollup_challenge_window] has
   passed since they were first published. *)
let earliest_cementing_level node_ctxt commitment_hash inbox_level =
  let open Lwt_result_syntax in
  let* publication_info =
    Node_context.commitment_published_at_level node_ctxt commitment_hash
  in
  return
  @@
  match publication_info with
  | Some {first_published_at_level; _} ->
      Int32.add
        first_published_at_level
        (sc_rollup_challenge_window node_ctxt |> Int32.of_int)
  | None ->
      (* NOTE: In case the publication information is missing from the rollup
         node (this is possible if a snapshot produced before
         https://gitlab.com/tezos/tezos/-/merge_requests/13724 was imported), we
         under-approximate the first publication level by the inbox level + 3,
         i.e. inbox level + block finality + injection block. *)
      Int32.add
        (Int32.add inbox_level 3l)
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
  let commitment_hash = Sc_rollup_block.most_recent_commitment head.header in
  let** commitment = Node_context.find_commitment node_ctxt commitment_hash in
  let** cementable_level_bound =
    Lwt_result.return
    @@ sub_level commitment.inbox_level (sc_rollup_challenge_window node_ctxt)
  in
  let lcc = Reference.get node_ctxt.lcc in
  if cementable_level_bound <= lcc.level then fail
  else
    let** cementable_bound_block =
      Node_context.find_l2_block_by_level node_ctxt cementable_level_bound
    in
    let cementable_commitment =
      Sc_rollup_block.most_recent_commitment cementable_bound_block.header
    in
    return cementable_commitment

let cementable_commitments (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  let open Lwt_result_option_list_syntax in
  let*& head = Node_context.last_processed_head_opt node_ctxt in
  let head_level = head.header.level in
  let lcc = Reference.get node_ctxt.lcc in
  let rec gather acc (commitment_hash : Commitment.Hash.t) =
    let* commitment = Node_context.find_commitment node_ctxt commitment_hash in
    match commitment with
    | None -> return acc
    | Some commitment when commitment.inbox_level <= lcc.level ->
        (* If we have moved backward passed or at the current LCC then we have
           reached the end. *)
        return acc
    | Some commitment ->
        let* earliest_cementing_level =
          earliest_cementing_level
            node_ctxt
            commitment_hash
            commitment.inbox_level
        in
        let acc =
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
  gather [] latest_cementable_commitment

module Helpers = struct
  let estimated_time_of_level (head : Sc_rollup_block.t)
      (constants : Rollup_constants.protocol_constants) l =
    let blocks_till = Int32.sub l head.header.level in
    let secs_till =
      Int32.to_int blocks_till * Int64.to_int constants.minimal_block_delay
    in
    let now_s = Time.System.now () |> Ptime.to_float_s |> int_of_float in
    Ptime.of_float_s (now_s + secs_till |> float_of_int)
    |> WithExceptions.Option.get ~loc:__LOC__

  let finalized_level l =
    (* In tenderbake, level l is finalized at l + 2. *)
    Int32.add l 2l

  let next_commitment (node_ctxt : _ Node_context.t) (block : Sc_rollup_block.t)
      =
    let open Lwt_result_syntax in
    let constants = (Reference.get node_ctxt.current_protocol).constants in
    let* prev_commitment =
      Node_context.get_commitment
        node_ctxt
        block.header.previous_commitment_hash
    in
    let next_commitment_level =
      Int32.add
        prev_commitment.inbox_level
        (Int32.of_int constants.sc_rollup.commitment_period_in_blocks)
    in
    let* next_commitment_block =
      Node_context.find_l2_block_by_level node_ctxt next_commitment_level
    in
    match next_commitment_block with
    | Some next ->
        (* Commitment has already been produced *)
        return (next.header.commitment_hash, next.header.level)
    | _ ->
        (* Commitment not yet produced. *)
        return (None, next_commitment_level)

  let committed_status (node_ctxt : _ Node_context.t)
      (block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let lcc = Reference.get node_ctxt.lcc in
    let constants = (Reference.get node_ctxt.current_protocol).constants in
    let* head = Node_context.last_processed_head_opt node_ctxt in
    let head = WithExceptions.Option.get ~loc:__LOC__ head in
    let* next_commitment, next_commitment_level =
      next_commitment node_ctxt block
    in
    let* pub_info =
      Option.map_es
        (fun c ->
          let+ info = Node_context.commitment_published_at_level node_ctxt c in
          (c, info))
        next_commitment
    in
    match pub_info with
    | Some
        ( next_commitment_hash,
          Some {first_published_at_level; published_at_level} ) ->
        (* Commitment already published *)
        if block.header.level <= lcc.level then
          (* Cemented *)
          return
            (Rollup_node_services.Cemented
               {
                 commitment_hash = next_commitment_hash;
                 commitment_level = next_commitment_level;
                 first_published_level = first_published_at_level;
                 published_level = published_at_level;
                 blocks_since_cemented = Int32.sub lcc.level block.header.level;
               })
        else
          (* Commitment published but not cemented *)
          let estimated_cementation_level =
            Int32.add
              first_published_at_level
              (Int32.of_int constants.sc_rollup.challenge_window_in_blocks)
          in
          let estimated_cementation_time =
            estimated_time_of_level head constants estimated_cementation_level
          in
          return
            (Rollup_node_services.Committed_
               {
                 commitment_hash = next_commitment_hash;
                 commitment_level = next_commitment_level;
                 first_published_level = first_published_at_level;
                 published_level = published_at_level;
                 estimated_cementation_time;
               })
    | _ ->
        (* Commitment not yet published *)
        let estimated_commitment_time =
          estimated_time_of_level
            head
            constants
            (finalized_level next_commitment_level)
        in
        let estimated_cementation_level =
          Int32.add
            next_commitment_level
            (Int32.of_int constants.sc_rollup.challenge_window_in_blocks)
        in
        let estimated_cementation_time =
          estimated_time_of_level head constants estimated_cementation_level
        in
        return
          (Rollup_node_services.Uncommitted
             {
               commitment_level = next_commitment_level;
               estimated_commitment_time;
               estimated_cementation_time;
             })
end

let cement_commitment (node_ctxt : _ Node_context.t) commitment =
  let open Lwt_result_syntax in
  let cement_operation =
    L1_operation.Cement
      {rollup = node_ctxt.config.sc_rollup_address; commitment}
  in
  let* _hash =
    Injector.check_and_add_pending_operation
      node_ctxt.config.mode
      cement_operation
  in
  return_unit

let on_cement_commitments ({node_ctxt; _} : state) =
  let open Lwt_result_syntax in
  let* cementable_commitments = cementable_commitments node_ctxt in
  List.iter_es (cement_commitment node_ctxt) cementable_commitments

module Types = struct
  type nonrec state = state

  type parameters = {node_ctxt : Node_context.rw}
end

module Name = struct
  (* We only have a single committer in the node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = Commitment_event.section @ ["publisher"]

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
    | Request.Publish -> protect @@ fun () -> on_publish_commitments state
    | Request.Cement -> protect @@ fun () -> on_cement_commitments state

  type launch_error = error trace

  let on_launch _w () Types.{node_ctxt} =
    Lwt_result.return
      {node_ctxt; inbox_checked = Commitment.Hash.Table.create 31}

  let on_error (type a b) _w st (r : (a, b) Request.t) (errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    let open Lwt_result_syntax in
    let request_view = Request.view r in
    let emit_and_return_errors errs =
      let*! () =
        Commitment_event.Publisher.request_failed request_view st errs
      in
      return `Continue
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
  let+ worker = Worker.launch table () {node_ctxt} (module Handlers) in
  Lwt.wakeup worker_waker worker

let start_in_mode mode =
  let open Configuration in
  match mode with
  | Maintenance | Operator | Bailout -> true
  | Observer | Accuser | Batcher -> false
  | Custom ops -> purposes_matches_mode (Custom ops) [Operating; Cementing]

let init (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  match Lwt.state worker_promise with
  | Lwt.Return _ ->
      (* Worker already started, nothing to do. *)
      return_unit
  | Lwt.Fail exn ->
      (* Worker crashed, not recoverable. *)
      fail [Rollup_node_errors.No_publisher; Exn exn]
  | Lwt.Sleep ->
      (* Never started, start it. *)
      if start_in_mode node_ctxt.config.mode then start node_ctxt
      else return_unit

(* This is a publisher worker for a single scoru *)
let worker () =
  let open Result_syntax in
  match Lwt.state worker_promise with
  | Lwt.Return worker -> return worker
  | Lwt.Fail exn -> tzfail (Error_monad.error_of_exn exn)
  | Lwt.Sleep -> tzfail Rollup_node_errors.No_publisher

let worker_add_request condition ~request =
  let open Lwt_result_syntax in
  match worker () with
  | Ok w ->
      let {node_ctxt; _} = Worker.state w in
      (* Bailout does not publish any commitment it only cement them. *)
      unless
        ((not (condition node_ctxt))
        || (Node_context.is_bailout node_ctxt && request = Request.Publish))
      @@ fun () ->
      let*! (_pushed : bool) = Worker.Queue.push_request w request in
      return_unit
  | Error (Rollup_node_errors.No_publisher :: _) -> return_unit
  | Error e -> fail e

let publish_commitments () =
  worker_add_request ~request:Request.Publish (fun node_ctxt ->
      Configuration.can_inject node_ctxt.config.mode Publish)

let cement_commitments () =
  worker_add_request ~request:Request.Cement (fun node_ctxt ->
      Configuration.can_inject node_ctxt.config.mode Cement)

let shutdown () =
  match worker () with
  | Error _ ->
      (* There is no publisher, nothing to do *)
      Lwt.return_unit
  | Ok w -> Worker.shutdown w

let worker_status () =
  match Lwt.state worker_promise with
  | Lwt.Return _ -> `Running
  | Lwt.Fail exn -> `Crashed exn
  | Lwt.Sleep -> `Not_running
