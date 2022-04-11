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

open Protocol_client_context
open Protocol
open Alpha_context
open Common
open Injector_worker_types

(* TODO/TORU Change me with bound on size and have a configuration option *)
let max_ops_per_l1_batch = 10

let confirmations = 2

module Op_queue = Hash_queue.Make (L1_operation.Hash) (L1_operation)

(** Information stored about an L1 operation that was injected on a Tezos
    node. *)
type injected_info = {
  op : L1_operation.t;  (** The L1 manager operation. *)
  oph : Operation_hash.t;
      (** The hash of the operation which contains [op] (this can be an L1 batch of
          several manager operations). *)
}

(** The part of the state which gathers information about injected
    operations (but not included). *)
type injected_state = {
  injected_operations : injected_info L1_operation.Hash.Table.t;
      (** A table mapping L1 manager operation hashes to the injection info for that
      operation.  *)
  injected_ophs : L1_operation.Hash.t list Operation_hash.Table.t;
      (** A mapping of all L1 manager operations contained in a L1 batch (i.e. an L1
      operation). *)
}

(** Information stored about an L1 operation that was included in a Tezos
    block. *)
type included_info = {
  op : L1_operation.t;  (** The L1 manager operation. *)
  oph : Operation_hash.t;
      (** The hash of the operation which contains [op] (this can be an L1 batch of
          several manager operations). *)
  l1_block : Block_hash.t;
      (** The hash of the L1 block in which the operation was included. *)
  l1_level : int32;  (** The level of [l1_block]. *)
}

(** The part of the state which gathers information about
    operations which are included in the L1 chain (but not confirmed). *)
type included_state = {
  included_operations : included_info L1_operation.Hash.Table.t;
  included_in_blocks : (int32 * L1_operation.Hash.t list) Block_hash.Table.t;
}

(** The type of a queue of pending operation. Each queue has a specific signer
    because each signer can inject at most one L1 batch of operation in each
    block.  *)
type pending_queue = {signer : signer; queue : Op_queue.t}

(* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2755
   Persist injector data on disk *)

(** The internal state of the injector.  *)
type state = {
  cctxt : Protocol_client_context.full;
      (** The client context which is used to perform the injections. *)
  pending_queues : pending_queue Signature.Public_key_hash.Map.t;
      (** The various queues of pending operations. One for each signer.  *)
  mutable injected : injected_state;
      (** The information about injected operations. *)
  mutable included : included_state;
      (** The information about included operations. {b Note}: Operations which
          are confirmed are simply removed from the state and do not appear
          anymore. *)
}

(* Builds an client context from another client context but uses logging instead
   of printing on stdout directly. This client context cannot make the injector
   exit. *)
let injector_context (cctxt : #Client_context.full) =
  let log _channel msg = Logs_lwt.info (fun m -> m "%s" msg) in
  object
    inherit
      Protocol_client_context.wrap_full
        (new Client_context.proxy_context (cctxt :> Client_context.full))

    inherit! Client_context.simple_printer log

    method! exit code =
      Format.ksprintf Stdlib.failwith "Injector client wants to exit %d" code
  end

let init_injector cctxt ~signers =
  let open Lwt_result_syntax in
  let+ pending_queues =
    List.fold_left_es
      (fun acc source ->
        let+ signer = get_signer cctxt source in
        Signature.Public_key_hash.Map.add
          source
          {signer; queue = Op_queue.create 50_000}
          acc)
      Signature.Public_key_hash.Map.empty
      signers
  in
  {
    cctxt = injector_context cctxt;
    pending_queues;
    injected =
      {
        injected_operations = L1_operation.Hash.Table.create n;
        injected_ophs = Operation_hash.Table.create n;
      };
    included =
      {
        included_operations = L1_operation.Hash.Table.create (confirmations * n);
        included_in_blocks = Block_hash.Table.create (confirmations * n);
      };
  }

(** Add an operation to the pending queue corresponding to the signer for this
    operation.  *)
let add_pending_operation state op =
  let open Lwt_result_syntax in
  let*! () = Event.(emit Injector.add_pending) op in
  let*? pending_queue =
    Signature.Public_key_hash.Map.find
      op.L1_operation.source
      state.pending_queues
    |> Result.of_option
         ~error:[Error.No_queue_for_source op.L1_operation.source]
  in
  Op_queue.replace pending_queue.queue op.L1_operation.hash op ;
  return_unit

(** Mark operations as injected (in [oph]). *)
let add_injected_operations state oph operations =
  let infos =
    List.map (fun op -> (op.L1_operation.hash, {op; oph})) operations
  in
  L1_operation.Hash.Table.replace_seq
    state.injected.injected_operations
    (List.to_seq infos) ;
  Operation_hash.Table.replace
    state.injected.injected_ophs
    oph
    (List.map fst infos)

(** [add_included_operations state oph l1_block l1_level operations] marks the
    [operations] as included (in the L1 batch [oph]) in the Tezos block
    [l1_block] of level [l1_level]. *)
let add_included_operations state oph l1_block l1_level operations =
  let open Lwt_syntax in
  let+ () =
    Event.(emit Injector.included)
      (l1_block, l1_level, List.map (fun o -> o.L1_operation.hash) operations)
  in
  let infos =
    List.map
      (fun op -> (op.L1_operation.hash, {op; oph; l1_block; l1_level}))
      operations
  in
  L1_operation.Hash.Table.replace_seq
    state.included.included_operations
    (List.to_seq infos) ;
  Block_hash.Table.replace
    state.included.included_in_blocks
    l1_block
    (l1_level, List.map fst infos)

(** [remove state oph] removes the operations that correspond to the L1 batch
    [oph] from the injected operations in the injector state. This function is
    used to move operations from injected to included. *)
let remove_injected_operation state oph =
  match Operation_hash.Table.find state.injected.injected_ophs oph with
  | None ->
      (* Nothing removed *)
      []
  | Some mophs ->
      Operation_hash.Table.remove state.injected.injected_ophs oph ;
      List.fold_left
        (fun removed moph ->
          match
            L1_operation.Hash.Table.find state.injected.injected_operations moph
          with
          | None -> removed
          | Some info ->
              L1_operation.Hash.Table.remove
                state.injected.injected_operations
                moph ;
              info :: removed)
        []
        mophs

(** [remove state block] removes the included operations that correspond to all
    the L1 batches included in [block]. This function is used when [block] is on
    an alternative chain in the case of a reorganization. *)
let remove_included_operation state block =
  match Block_hash.Table.find state.included.included_in_blocks block with
  | None ->
      (* Nothing removed *)
      []
  | Some (_level, mophs) ->
      Block_hash.Table.remove state.included.included_in_blocks block ;
      List.fold_left
        (fun removed moph ->
          match
            L1_operation.Hash.Table.find state.included.included_operations moph
          with
          | None -> removed
          | Some info ->
              L1_operation.Hash.Table.remove
                state.included.included_operations
                moph ;
              info :: removed)
        []
        mophs

(** Inject the given [operations] (to be signed by [signer]) in an L1 batch. *)
let inject_operations state signer (operations : L1_operation.t list) =
  let open Lwt_result_syntax in
  let open Annotated_manager_operation in
  let*! () = Event.(emit Injector.injecting_operations) operations in
  let operations =
    List.map
      (fun {L1_operation.manager_operation = Manager operation; _} ->
        Annotated_manager_operation
          (Manager_info
             {
               source = None;
               fee = Limit.unknown;
               gas_limit = Limit.unknown;
               storage_limit = Limit.unknown;
               counter = None;
               operation;
             }))
      operations
  in
  let (Manager_list annot_op) =
    Annotated_manager_operation.manager_of_list operations
  in
  (* TODO maybe use something else (e.g. inject directly with correct limits) *)
  let* (oph, _, _) =
    Injection.inject_manager_operation
      state.cctxt
      ~chain:state.cctxt#chain
      ~block:(`Head 0)
      ~source:signer.pkh
      ~src_pk:signer.pk
      ~src_sk:signer.sk
      ~successor_level:
        true (* Needed to simulate tx_rollup operations in the next block *)
      ~fee:Limit.unknown
      ~gas_limit:Limit.unknown
      ~storage_limit:Limit.unknown
      ~fee_parameter:
        {
          minimal_fees = Tez.of_mutez_exn 100L;
          minimal_nanotez_per_byte = Q.of_int 1000;
          minimal_nanotez_per_gas_unit = Q.of_int 100;
          force_low_fee = false;
          fee_cap = Tez.one;
          burn_cap = Tez.one;
        }
      annot_op
  in
  let*! () = Event.(emit Injector.injected) oph in
  return oph

(** [inject_pending_operations_for ~limit state pending] injects up to [limit]
    operation from the pending queue [pending]. Upon successful injection, the
    operations are removed from the queue and marked as injected. *)
let inject_pending_operations_for ~limit state pending =
  let open Lwt_result_syntax in
  (* Retrieve and remove operations from pending *)
  let operations_to_inject = Op_queue.peek_at_most pending.queue limit in
  match operations_to_inject with
  | [] -> return_unit
  | _ ->
      let*! () =
        Event.(emit Injector.injecting_pending)
          (List.length operations_to_inject)
      in
      let+ oph = inject_operations state pending.signer operations_to_inject in
      (* Injection succeeded, remove from pending and add to injected *)
      List.iter
        (fun op -> Op_queue.remove pending.queue op.L1_operation.hash)
        operations_to_inject ;
      add_injected_operations state oph operations_to_inject

(** [inject_pending_operations ~limit pending] injects up to [limit]
    operation from each pending queue [pending]. Upon successful injection, the
    operations are removed from the queue and marked as injected. *)
let inject_pending_operations ?(limit = max_ops_per_l1_batch) state =
  (* TODO: check if iter_ep does not cancel promises *)
  Signature.Public_key_hash.Map.iter_ep
    (fun _source pending -> inject_pending_operations_for ~limit state pending)
    state.pending_queues

(** [register_included_operation state block level oph] marks the manager
    operations contained in the L1 batch [oph] as being included in the [block]
    of level [level], by moving them from the "injected" state to the "included"
    state. *)
let register_included_operation state block level oph =
  match remove_injected_operation state oph with
  | [] -> Lwt.return_unit
  | injected_infos ->
      let included_mops =
        List.map (fun (i : injected_info) -> i.op) injected_infos
      in
      add_included_operations state oph block level included_mops

(** [register_included_operations state block level oph] marks the known (by
    this injector) manager operations contained in [block] as being included. *)
let register_included_operations state (block : Alpha_block_services.block_info)
    =
  List.iter_s
    (List.iter_s (fun (op : Alpha_block_services.operation) ->
         register_included_operation
           state
           block.hash
           block.header.shell.level
           op.hash
         (* TODO/TORU: Handle operations for rollup_id here with
            callback *)))
    block.Alpha_block_services.operations

(** [revert_included_operations state block] marks the known (by this injector)
    manager operations contained in [block] as not being included any more,
    typically in the case of a reorganization where [block] is on an alternative
    chain. The operations are put back in their respective pending queues. *)
let revert_included_operations state block =
  let open Lwt_syntax in
  let included_infos = remove_included_operation state block in
  let* () =
    Event.(emit Injector.revert_operations)
      (List.map (fun o -> o.op.hash) included_infos)
  in
  (* TODO/TORU: maybe put at the front of the queue for re-injection.  *)
  List.iter_es (fun {op; _} -> add_pending_operation state op) included_infos

(** [register_confirmed_level state confirmed_level] is called when the level
    [confirmed_level] is known as confirmed. In this case, the operations of
    block which are below this level are also considered as confirmed and are
    removed from the "included" state. These operations cannot be part of a
    reorganization so there will be no need to re-inject them anymore. *)
let register_confirmed_level state confirmed_level =
  let open Lwt_syntax in
  let* () = Event.(emit Injector.confirmed_level) confirmed_level in
  Block_hash.Map.iter_s
    (fun block (level, _operations) ->
      if level <= confirmed_level then
        let confirmed_ops = remove_included_operation state block in
        Event.(emit Injector.confirmed_operations)
          (level, List.map (fun o -> o.op.hash) confirmed_ops)
      else Lwt.return_unit)
    state.included.included_in_blocks

(** [on_new_tezos_head state head reorg] is called when there is a new Tezos
    head (with a potential reorganization [reorg]). It first reverts any blocks
    that are in the alternative branch of the reorganization and then registers
    the effect of the new branch (the newly included operation and confirmed
    operations).  *)
let on_new_tezos_head state (head : Alpha_block_services.block_info)
    (reorg : Alpha_block_services.block_info reorg) =
  let open Lwt_result_syntax in
  let*! () = Event.(emit Injector.new_tezos_head) head.hash in
  let* () =
    List.iter_es
      (fun removed_block ->
        revert_included_operations state removed_block.Alpha_block_services.hash)
      reorg.old_chain
  in
  let*! () =
    List.iter_s
      (fun added_block -> register_included_operations state added_block)
      reorg.new_chain
  in
  let*! () = register_included_operations state head in
  let confirmed_level =
    Int32.sub
      head.Alpha_block_services.header.shell.level
      (Int32.of_int confirmations)
  in
  let*! () =
    if confirmed_level >= 0l then register_confirmed_level state confirmed_level
    else Lwt.return_unit
  in
  return_unit

(* The request {Request.Inject} triggers an injection of the operations in all
   the pending queues. *)
let on_inject state = inject_pending_operations state

module Types = struct
  type nonrec state = state

  type parameters = {
    cctxt : Client_context.full;
    signers : public_key_hash list;
  }
end

(* The worker for the injector. *)
module Worker = Worker.Make (Name) (Dummy_event) (Request) (Types) (Logger)

(* The queue for the requests to the injector worker is infinite. *)
type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  let on_request : type r. worker -> r Request.t -> r tzresult Lwt.t =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Queue_pending op -> add_pending_operation state op
    | Request.New_tezos_head (head, reorg) -> on_new_tezos_head state head reorg
    | Request.Inject -> on_inject state

  let on_request w r =
    (* The execution of the request handler is protected to avoid stopping the
       worker in case of an exception. *)
    protect @@ fun () -> on_request w r

  let on_launch _w _rollup (* TODO *) Types.{cctxt; signers} =
    init_injector cctxt ~signers

  let on_error _w r st errs =
    let open Lwt_result_syntax in
    (* Errors do not stop the worker but emit an entry in the log. *)
    let*! () = Event.(emit Injector.request_failed) (r, st, errs) in
    return_unit

  let on_completion _w r _ st =
    match Request.view r with
    | Request.View (Queue_pending _ | New_tezos_head _) ->
        Event.(emit Injector.request_completed_debug) (Request.view r, st)
    | View Inject ->
        Event.(emit Injector.request_completed_notice) (Request.view r, st)

  let on_no_request _ = return_unit

  let on_close _w = Lwt.return_unit
end

let table = Worker.create_table Queue

type t = worker

(* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2754
   Injector worker in a separate process *)
let init cctxt ~rollup ~signers =
  Worker.launch
    table
    rollup
    {cctxt = (cctxt :> Client_context.full); signers}
    (module Handlers)

let add_pending_operation w op =
  Worker.Queue.push_request w (Request.Queue_pending op)

let add_pending_operations w ops = List.iter_s (add_pending_operation w) ops

let new_tezos_head w h reorg =
  Worker.Queue.push_request w (Request.New_tezos_head (h, reorg))

let inject w = Worker.Queue.push_request w Request.Inject
