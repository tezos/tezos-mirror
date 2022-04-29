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

(* This is the Tenderbake finality for blocks. *)
(* TODO: https://gitlab.com/tezos/tezos/-/issues/2815
   Centralize this and maybe make it configurable. *)
let confirmations = 2

module Op_queue = Hash_queue.Make (L1_operation.Hash) (L1_operation)

type injection_strategy = Each_block | Delay_block

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

(* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2755
   Persist injector data on disk *)

(** The internal state of each injector worker.  *)
type state = {
  cctxt : Protocol_client_context.full;
      (** The client context which is used to perform the injections. *)
  signer : signer;  (** The signer for this worker. *)
  tags : tags;
      (** The tags of this worker, for both informative and identification
          purposes. *)
  strategy : injection_strategy;
      (** The strategy of this worker for injecting the pending operations. *)
  queue : Op_queue.t;  (** The queue of pending operations for this injector. *)
  injected : injected_state;  (** The information about injected operations. *)
  included : included_state;
      (** The information about included operations. {b Note}: Operations which
          are confirmed are simply removed from the state and do not appear
          anymore. *)
  rollup_node_state : State.t;
      (** The state of the rollup node (essentially to access the stores). *)
}

(** Builds a client context from another client context but uses logging instead
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

let init_injector rollup_node_state ~signer strategy tags =
  let open Lwt_result_syntax in
  let+ signer = get_signer rollup_node_state.State.cctxt signer in
  let queue = Op_queue.create 50_000 in
  (* Very coarse approximation for the number of operation we expect for each
     block *)
  let n =
    Tags.fold
      (fun t acc ->
        let n =
          match t with
          | `Commitment -> 3
          | `Submit_batch -> 509
          | `Finalize_commitment -> 3
          | `Remove_commitment -> 3
          | `Rejection -> 3
          | `Dispatch_withdrawals -> 89
        in
        acc + n)
      tags
      0
  in
  {
    cctxt = injector_context rollup_node_state.State.cctxt;
    signer;
    tags;
    strategy;
    queue;
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
    rollup_node_state;
  }

module Event = struct
  include Injector_events

  let emit1 e state x = Event.emit e (state.signer.pkh, state.tags, x)

  let emit2 e state x y = Event.emit e (state.signer.pkh, state.tags, x, y)

  let emit3 e state x y z = Event.emit e (state.signer.pkh, state.tags, x, y, z)
end

(** Add an operation to the pending queue corresponding to the signer for this
    operation.  *)
let add_pending_operation state op =
  let open Lwt_syntax in
  let+ () = Event.(emit1 add_pending) state op in
  Op_queue.replace state.queue op.L1_operation.hash op

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
    Event.(emit3 included)
      state
      l1_block
      l1_level
      (List.map (fun o -> o.L1_operation.hash) operations)
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

let tag_operation (type kind) (op : kind manager_operation) =
  match op with
  | Tx_rollup_submit_batch _ -> `Submit_batch
  | Tx_rollup_commit _ -> `Commitment
  | Tx_rollup_finalize_commitment _ -> `Finalize_commitment
  | Tx_rollup_remove_commitment _ -> `Remove_commitment
  | Tx_rollup_rejection _ -> `Rejection
  | Tx_rollup_dispatch_tickets _ -> `Dispatch_withdrawals
  | _ -> `Other

let fee_parameter (rollup_node_state : State.t) op =
  let tag = tag_operation op in
  let Node_config.{fee_cap; burn_cap} =
    match tag with
    | `Commitment -> rollup_node_state.caps.operator
    | `Submit_batch -> rollup_node_state.caps.submit_batch
    | `Finalize_commitment -> rollup_node_state.caps.finalize_commitment
    | `Remove_commitment -> rollup_node_state.caps.remove_commitment
    | `Rejection -> rollup_node_state.caps.rejection
    | `Dispatch_withdrawals -> rollup_node_state.caps.dispatch_withdrawals
    | `Other -> Node_config.default_cost_caps
  in
  Injection.
    {
      minimal_fees = Tez.of_mutez_exn 100L;
      minimal_nanotez_per_byte = Q.of_int 1000;
      minimal_nanotez_per_gas_unit = Q.of_int 100;
      force_low_fee = false;
      fee_cap;
      burn_cap;
    }

let fee_parameter_of_operations state ops =
  List.fold_left
    (fun acc {L1_operation.manager_operation = Manager op; _} ->
      let param = fee_parameter state.rollup_node_state op in
      Injection.
        {
          minimal_fees = Tez.max acc.minimal_fees param.minimal_fees;
          minimal_nanotez_per_byte =
            Q.max acc.minimal_nanotez_per_byte param.minimal_nanotez_per_byte;
          minimal_nanotez_per_gas_unit =
            Q.max
              acc.minimal_nanotez_per_gas_unit
              param.minimal_nanotez_per_gas_unit;
          force_low_fee = acc.force_low_fee || param.force_low_fee;
          fee_cap =
            WithExceptions.Result.get_ok
              ~loc:__LOC__
              Tez.(acc.fee_cap +? param.fee_cap);
          burn_cap =
            WithExceptions.Result.get_ok
              ~loc:__LOC__
              Tez.(acc.burn_cap +? param.burn_cap);
        })
    Injection.
      {
        minimal_fees = Tez.zero;
        minimal_nanotez_per_byte = Q.zero;
        minimal_nanotez_per_gas_unit = Q.zero;
        force_low_fee = false;
        fee_cap = Tez.zero;
        burn_cap = Tez.zero;
      }
    ops

(** Simulate the injection of [operations]. See {!inject_operations} for the
    specification of [must_succeed]. *)
let simulate_operations ~must_succeed state signer
    (operations : L1_operation.t list) =
  let open Lwt_result_syntax in
  let open Annotated_manager_operation in
  let force =
    match operations with
    | [] -> assert false
    | [_] ->
        (* If there is only one operation, fail when simulation fails *)
        false
    | _ -> (
        (* We want to see which operation failed in the batch if not all must
           succeed *)
        match must_succeed with `All -> false | `At_least_one -> true)
  in
  let*! () = Event.(emit2 simulating_operations) state operations force in
  let fee_parameter = fee_parameter_of_operations state operations in
  let operations =
    List.map
      (fun {L1_operation.manager_operation = Manager operation; _} ->
        Annotated_manager_operation
          (Injection.prepare_manager_operation
             ~fee:Limit.unknown
             ~gas_limit:Limit.unknown
             ~storage_limit:Limit.unknown
             operation))
      operations
  in
  let (Manager_list annot_op) =
    Annotated_manager_operation.manager_of_list operations
  in
  let* oph, op, result =
    Injection.inject_manager_operation
      state.cctxt
      ~simulation:true (* Only simulation here *)
      ~force
      ~chain:state.cctxt#chain
      ~block:(`Head 0)
      ~source:signer.pkh
      ~src_pk:signer.pk
      ~src_sk:signer.sk
      ~successor_level:true
        (* Needed to simulate tx_rollup operations in the next block *)
      ~fee:Limit.unknown
      ~gas_limit:Limit.unknown
      ~storage_limit:Limit.unknown
      ~fee_parameter
      annot_op
  in
  return (oph, Contents_list op, Apply_results.Contents_result_list result)

let inject_on_node state packed_contents =
  let open Lwt_result_syntax in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2815 *)
  (* Branch to head - 2 for tenderbake *)
  let* branch =
    Tezos_shell_services.Shell_services.Blocks.hash
      state.cctxt
      ~chain:state.cctxt#chain
      ~block:(`Head 2)
      ()
  in
  let unsigned_op_bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({branch}, packed_contents)
  in
  let* signature =
    Client_keys.sign
      state.cctxt
      ~watermark:Signature.Generic_operation
      state.signer.sk
      unsigned_op_bytes
  in
  let (Contents_list contents) = packed_contents in
  let op : _ Operation.t =
    {shell = {branch}; protocol_data = {contents; signature = Some signature}}
  in
  let op_bytes =
    Data_encoding.Binary.to_bytes_exn Operation.encoding (Operation.pack op)
  in
  Tezos_shell_services.Shell_services.Injection.operation
    state.cctxt
    ~chain:state.cctxt#chain
    op_bytes
  >>=? fun oph ->
  let*! () = Event.(emit1 injected) state oph in
  return oph

(** Inject the given [operations] in an L1 batch. If [must_succeed] is [`All]
    then all the operations must succeed in the simulation of injection. If
    [must_succeed] is [`At_least_one] at least one operation in the list
    [operations] must be successful in the simulation. In any case, only
    operations which are known as successful will be included in the injected L1
    batch. {b Note}: [must_succeed = `At_least_one] allows to incrementally build
    "or-batches" by iteratively removing operations that fail from the desired
    batch. *)
let rec inject_operations ~must_succeed state (operations : L1_operation.t list)
    =
  let open Lwt_result_syntax in
  let* _oph, packed_contents, result =
    simulate_operations ~must_succeed state state.signer operations
  in
  let results = Apply_results.to_list result in
  let failure = ref false in
  let* rev_non_failing_operations =
    List.fold_left2_s
      ~when_different_lengths:
        [
          Exn
            (Failure
               "Unexpected error: length of operations and result differ in \
                simulation");
        ]
      (fun acc op (Apply_results.Contents_result result) ->
        match result with
        | Apply_results.Manager_operation_result
            {operation_result = Failed (_, error); _} ->
            let*! () = Event.(emit2 dropping_operation) state op error in
            failure := true ;
            Lwt.return acc
        | Apply_results.Manager_operation_result
            {operation_result = Applied _ | Backtracked _ | Skipped _; _} ->
            (* Not known to be failing *)
            Lwt.return (op :: acc)
        | _ ->
            (* Only manager operations *)
            assert false)
      []
      operations
      results
  in
  if !failure then
    (* Invariant: must_succeed = `At_least_one, otherwise the simulation would have
       returned an error. We try to inject without the failing operation. *)
    let operations = List.rev rev_non_failing_operations in
    inject_operations ~must_succeed state operations
  else
    (* Inject on node for real *)
    let+ oph = inject_on_node state packed_contents in
    (oph, operations)

(** Returns the (upper bound on) the size of an L1 batch of operations composed
    of the manager operations [rev_ops]. *)
let size_l1_batch signer rev_ops =
  let contents_list =
    List.map
      (fun (op : L1_operation.t) ->
        let (Manager operation) = op.manager_operation in
        let contents =
          Manager_operation
            {
              source = signer.pkh;
              operation;
              (* Below are dummy values that are only used to approximate the
                 size. It is thus important that they remain above the real
                 values if we want the computed size to be an over_approximation
                 (without having to do a simulation first). *)
              (* TODO: https://gitlab.com/tezos/tezos/-/issues/2812
                 check the size, or compute them wrt operation kind *)
              fee = Tez.of_mutez_exn 3_000_000L;
              counter = Z.of_int 500_000;
              gas_limit = Gas.Arith.integral_of_int_exn 500_000;
              storage_limit = Z.of_int 500_000;
            }
        in
        Contents contents)
      rev_ops
  in
  let (Contents_list contents) =
    match Operation.of_list contents_list with
    | Error _ ->
        (* Cannot happen: rev_ops is non empty and contains only manager
           operations *)
        assert false
    | Ok packed_contents_list -> packed_contents_list
  in
  let signature =
    match signer.pkh with
    | Signature.Ed25519 _ -> Signature.of_ed25519 Ed25519.zero
    | Secp256k1 _ -> Signature.of_secp256k1 Secp256k1.zero
    | P256 _ -> Signature.of_p256 P256.zero
  in
  let branch = Block_hash.zero in
  let operation =
    {
      shell = {branch};
      protocol_data = Operation_data {contents; signature = Some signature};
    }
  in
  Data_encoding.Binary.length Operation.encoding operation

(** Retrieve as many operations from the queue while remaining below the size
    limit. *)
let get_operations_from_queue ~size_limit state =
  let exception Reached_limit of L1_operation.t list in
  let rev_ops =
    try
      Op_queue.fold
        (fun _oph op ops ->
          let new_ops = op :: ops in
          let new_size = size_l1_batch state.signer new_ops in
          if new_size > size_limit then raise (Reached_limit ops) ;
          new_ops)
        state.queue
        []
    with Reached_limit ops -> ops
  in
  List.rev rev_ops

(* Ignore the failures of finalize and remove commitment operations. These
   operations fail when there are either no commitment to finalize or to remove
   (which can happen when there are no inbox for instance). *)
let ignore_failing_gc_operations operations = function
  | Ok res -> Ok (`Injected res)
  | Error _ as res ->
      let only_gc_operations =
        List.for_all
          (fun op ->
            let (Manager op) = op.L1_operation.manager_operation in
            match op with
            | Tx_rollup_finalize_commitment _ | Tx_rollup_remove_commitment _ ->
                true
            | _ -> false)
          operations
      in
      if only_gc_operations then Ok `Ignored else res

(** [inject_pending_operations_for ~size_limit state pending] injects
    operations from the pending queue [pending], whose total size does
    not exceed [size_limit]. Upon successful injection, the
    operations are removed from the queue and marked as injected. *)
let inject_pending_operations
    ?(size_limit = Constants.max_operation_data_length) state =
  let open Lwt_result_syntax in
  (* Retrieve and remove operations from pending *)
  let operations_to_inject = get_operations_from_queue ~size_limit state in
  match operations_to_inject with
  | [] -> return_unit
  | _ -> (
      let*! () =
        Event.(emit1 injecting_pending) state (List.length operations_to_inject)
      in
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/2813
         Decide if some operations must all succeed *)
      let*! res =
        inject_operations ~must_succeed:`At_least_one state operations_to_inject
      in
      let*? res = ignore_failing_gc_operations operations_to_inject res in
      match res with
      | `Injected (oph, injected_operations) ->
          (* Injection succeeded, remove from pending and add to injected *)
          List.iter
            (fun op -> Op_queue.remove state.queue op.L1_operation.hash)
            injected_operations ;
          add_injected_operations state oph operations_to_inject ;
          return_unit
      | `Ignored ->
          (* Injection failed but we ignore the failure. We can leave the GC
             operations in the queue as their can be only one unique. *)
          return_unit)

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

(** Returns [true] if an included operation should be re-queued for injection
    when the block in which it is included is reverted (due to a
    reorganization). *)
let requeue_reverted_operation state op =
  let open Lwt_syntax in
  let (Manager operation) = op.L1_operation.manager_operation in
  match operation with
  | Tx_rollup_rejection _ ->
      (* TODO: check if rejected commitment in still in main chain *)
      return_true
  | Tx_rollup_commit {commitment; _} -> (
      let level = commitment.level in
      let* l2_block = State.get_level_l2_block state.rollup_node_state level in
      match l2_block with
      | None ->
          (* We don't know this L2 block, should not happen *)
          let+ () = Debug_events.(emit should_not_happen) __LOC__ in
          false
      | Some l2_block ->
          let commit_hash =
            Tx_rollup_commitment.(Compact.hash (Full.compact commitment))
          in
          (* Do not re-queue if commitment for this level has changed *)
          return
            Tx_rollup_commitment_hash.(
              l2_block.L2block.header.commitment = commit_hash))
  | _ -> return_true

(** [revert_included_operations state block] marks the known (by this injector)
    manager operations contained in [block] as not being included any more,
    typically in the case of a reorganization where [block] is on an alternative
    chain. The operations are put back in the pending queue. *)
let revert_included_operations state block =
  let open Lwt_syntax in
  let included_infos = remove_included_operation state block in
  let* () =
    Event.(emit1 revert_operations)
      state
      (List.map (fun o -> o.op.hash) included_infos)
  in
  (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2814
     maybe put at the front of the queue for re-injection. *)
  List.iter_s
    (fun {op; _} ->
      let* requeue = requeue_reverted_operation state op in
      if requeue then add_pending_operation state op else return_unit)
    included_infos

(** [register_confirmed_level state confirmed_level] is called when the level
    [confirmed_level] is known as confirmed. In this case, the operations of
    block which are below this level are also considered as confirmed and are
    removed from the "included" state. These operations cannot be part of a
    reorganization so there will be no need to re-inject them anymore. *)
let register_confirmed_level state confirmed_level =
  let open Lwt_syntax in
  let* () =
    Event.(emit confirmed_level) (state.signer.pkh, state.tags, confirmed_level)
  in
  Block_hash.Table.iter_s
    (fun block (level, _operations) ->
      if level <= confirmed_level then
        let confirmed_ops = remove_included_operation state block in
        Event.(emit2 confirmed_operations)
          state
          level
          (List.map (fun o -> o.op.hash) confirmed_ops)
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
  let*! () = Event.(emit1 new_tezos_head) state head.hash in
  let*! () =
    List.iter_s
      (fun removed_block ->
        revert_included_operations state removed_block.Alpha_block_services.hash)
      (List.rev reorg.old_chain)
  in
  let*! () =
    List.iter_s
      (fun added_block -> register_included_operations state added_block)
      reorg.new_chain
  in
  (* Head is already included in the reorganization, so no need to process it
     separately. *)
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

(* The request {Request.Inject} triggers an injection of the operations
   the pending queue. *)
let on_inject state = inject_pending_operations state

module Types = struct
  type nonrec state = state

  type parameters = {
    rollup_node_state : State.t;
    strategy : injection_strategy;
    tags : Tags.t;
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
    let open Lwt_result_syntax in
    let state = Worker.state w in
    match request with
    | Request.Add_pending op ->
        let*! () = add_pending_operation state op in
        return_unit
    | Request.New_tezos_head (head, reorg) -> on_new_tezos_head state head reorg
    | Request.Inject -> on_inject state

  let on_request w r =
    (* The execution of the request handler is protected to avoid stopping the
       worker in case of an exception. *)
    protect @@ fun () -> on_request w r

  let on_launch _w signer Types.{rollup_node_state; strategy; tags} =
    init_injector rollup_node_state ~signer strategy tags

  let on_error w r st errs =
    let open Lwt_result_syntax in
    let state = Worker.state w in
    (* Errors do not stop the worker but emit an entry in the log. *)
    let*! () = Event.(emit3 request_failed) state r st errs in
    return_unit

  let on_completion w r _ st =
    let state = Worker.state w in
    match Request.view r with
    | Request.View (Add_pending _ | New_tezos_head _) ->
        Event.(emit2 request_completed_debug) state (Request.view r) st
    | View Inject ->
        Event.(emit2 request_completed_notice) state (Request.view r) st

  let on_no_request _ = return_unit

  let on_close _w = Lwt.return_unit
end

let table = Worker.create_table Queue

(* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2754
   Injector worker in a separate process *)
let init rollup_node_state ~signers =
  let open Lwt_result_syntax in
  let signers_map =
    List.fold_left
      (fun acc (signer, strategy, tags) ->
        let tags = Tags.of_list tags in
        let strategy, tags =
          match Signature.Public_key_hash.Map.find_opt signer acc with
          | None -> (strategy, tags)
          | Some (other_strategy, other_tags) ->
              let strategy =
                match (strategy, other_strategy) with
                | Each_block, Each_block -> Each_block
                | Delay_block, _ | _, Delay_block ->
                    (* Delay_block strategy takes over because we can always wait a
                       little bit more to inject operation which are to be injected
                       "each block". *)
                    Delay_block
              in
              (strategy, Tags.union other_tags tags)
        in
        Signature.Public_key_hash.Map.add signer (strategy, tags) acc)
      Signature.Public_key_hash.Map.empty
      signers
  in
  Signature.Public_key_hash.Map.iter_es
    (fun signer (strategy, tags) ->
      let+ worker =
        Worker.launch
          table
          signer
          {rollup_node_state; strategy; tags}
          (module Handlers)
      in
      ignore worker)
    signers_map

let worker_of_signer signer_pkh =
  match Worker.find_opt table signer_pkh with
  | None ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/2818
         maybe lazily start worker here *)
      error (Error.No_worker_for_source signer_pkh)
  | Some worker -> ok worker

let add_pending_operation ~source op =
  let open Lwt_result_syntax in
  let*? w = worker_of_signer source in
  let l1_operation = L1_operation.make op in
  let*! () = Worker.Queue.push_request w (Request.Add_pending l1_operation) in
  return_unit

let new_tezos_head h reorg =
  let workers = Worker.list table in
  List.iter_p
    (fun (_signer, w) ->
      Worker.Queue.push_request w (Request.New_tezos_head (h, reorg)))
    workers

let has_tag_in ~tags state =
  match tags with
  | None ->
      (* Not filtering on tags *)
      true
  | Some tags -> not (Tags.disjoint state.tags tags)

let has_strategy ~strategy state =
  match strategy with
  | None ->
      (* Not filtering on strategy *)
      true
  | Some strategy -> state.strategy = strategy

let inject ?tags ?strategy () =
  let workers = Worker.list table in
  let tags = Option.map Tags.of_list tags in
  List.iter_p
    (fun (_signer, w) ->
      let worker_state = Worker.state w in
      if has_tag_in ~tags worker_state && has_strategy ~strategy worker_state
      then Worker.Queue.push_request w Request.Inject
      else Lwt.return_unit)
    workers
