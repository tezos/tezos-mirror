(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

open Injector_common
open Injector_worker_types
open Injector_sigs
open Injector_errors
module Block_cache =
  Aches_lwt.Lache.Make_result
    (Aches.Rache.Transfer (Aches.Rache.LRU) (Block_hash))

module Int_cache =
  Aches_lwt.Lache.Make_result
    (Aches.Rache.Transfer
       (Aches.Rache.LRU)
       (struct
         include Int

         let hash x = x
       end))

(* This is the Tenderbake finality for blocks. *)
(* TODO: https://gitlab.com/tezos/tezos/-/issues/2815
   Centralize this and maybe make it configurable. *)
let confirmations = 2

(** Builds a client context from another client context but uses logging instead
    of printing on stdout directly. This client context cannot make the injector
    exit. *)
let injector_context (cctxt : #Client_context.full) : Client_context.full =
  let log _channel msg = Logs_lwt.info (fun m -> m "%s" msg) in
  object
    inherit Client_context.proxy_context cctxt

    inherit! Client_context.simple_printer log

    method! exit code =
      Format.ksprintf Stdlib.failwith "Injector client wants to exit %d" code
  end

module Make (Parameters : PARAMETERS) = struct
  module Tags = Injector_tags.Make (Parameters.Tag)
  module Tags_table = Hashtbl.Make (Parameters.Tag)
  module POperation = Parameters.Operation
  module Inj_operation = Injector_operation.Make (POperation)
  module Request = Request (Inj_operation)
  module Inj_proto = Injector_protocol.Make (Parameters)

  module type PROTOCOL_CLIENT =
    PROTOCOL_CLIENT
      with type state = Parameters.state
       and type operation = Parameters.Operation.t

  type proto_client = (module PROTOCOL_CLIENT)

  (** Wrapper for unsigned operation with the protocol code to sign it. *)
  module type Proto_unsigned_op = sig
    module Proto_client : PROTOCOL_CLIENT

    val value : Proto_client.unsigned_operation
  end

  type injected_info = {
    op : Inj_operation.t;
    oph : Operation_hash.t;
    op_index : int;
  }

  type included_info = {
    op : Inj_operation.t;
    oph : Operation_hash.t;
    op_index : int;
    l1_block : Block_hash.t;
    l1_level : int32;
  }

  type injected_l1_op_content = {
    level : int32;
    inj_ops : Inj_operation.Hash.t list;
    signer_pkh : Signature.public_key_hash;
  }

  type included_l1_op_content = {
    level : int32;
    inj_ops : Inj_operation.Hash.t list;
  }

  type status =
    | Pending of POperation.t
    | Injected of injected_info
    | Included of included_info

  module Op_queue =
    Disk_persistence.Make_queue
      (struct
        let name = "operations_queue"
      end)
      (Inj_operation.Hash)
      (struct
        include Inj_operation

        let persist o = Parameters.persist_operation o.operation
      end)

  module Injected_operations = Inj_operation.Hash.Table
  module Injected_ophs = Operation_hash.Table

  (** The part of the state which gathers information about injected
    operations (but not included). *)
  type injected_state = {
    injected_operations : injected_info Injected_operations.t;
        (** A table mapping L1 manager operation hashes to the injection info for that
          operation.  *)
    injected_ophs : injected_l1_op_content Injected_ophs.t;
        (** A mapping of all L1 manager operations contained in a L1 batch (i.e. an L1
          operation). *)
  }

  module Included_operations = Inj_operation.Hash.Table
  module Included_in_blocks = Block_hash.Table

  (** The part of the state which gathers information about
    operations which are included in the L1 chain (but not confirmed). *)
  type included_state = {
    included_operations : included_info Included_operations.t;
    included_in_blocks : included_l1_op_content Included_in_blocks.t;
  }

  type protocols = Tezos_shell_services.Chain_services.Blocks.protocols = {
    current_protocol : Protocol_hash.t;
    next_protocol : Protocol_hash.t;
  }

  type last_seen_head = {block_hash : Block_hash.t; level : int32}

  (** The internal state of each injector worker.  *)
  type state = {
    cctxt : Client_context.full;
        (** The client context which is used to perform the injections. *)
    l1_ctxt : Layer_1.t;  (** Monitoring of L1 heads.  *)
    signers : signer list;  (** The signers for this worker. *)
    tags : Tags.t;
        (** The tags of this worker, for both informative and identification
          purposes. *)
    strategy : injection_strategy;
        (** The strategy of this worker for injecting the pending operations. *)
    save_dir : string;  (** Path to where save persistent state *)
    queue : Op_queue.t;
        (** The queue of pending operations for this injector. *)
    injected : injected_state;
        (** The information about injected operations. *)
    included : included_state;
        (** The information about included operations. {b Note}: Operations which
          are confirmed are simply removed from the state and do not appear
          anymore. *)
    state : Parameters.state;
    retention_period : int;
        (** Number of blocks for which the injector keeps the included
            information. *)
    allowed_attempts : int;
        (** The number of attempts that will be made to inject an operation. *)
    injection_ttl : int;
        (** The number of blocks after which an operation is retried when
            injected but never included. *)
    mutable last_seen_head : last_seen_head option;
        (** Last L1 head that the injector has seen (used to compute
            reorganizations). *)
    mutable protocols : protocols;
        (** The protocols of the head of the L1 chain. This information is used
            to know with which protocol one should perform the
            simulation/injection. NOTE: this value is updated before
            [last_seen_head]. *)
    mutable proto_client : proto_client;
        (** The protocol client that should be used to simulate operations. *)
  }

  module Event = struct
    include
      Injector_events.Make (Parameters) (Tags) (POperation) (Inj_operation)
        (Request)

    let signers_alias signers = List.map (fun s -> s.alias) signers

    let emit1 e state ?(signers = state.signers) x =
      emit e (signers_alias signers, state.tags, x)

    let emit2 e state ?(signers = state.signers) x y =
      emit e (signers_alias signers, state.tags, x, y)

    let emit3 e state ?(signers = state.signers) x y z =
      emit e (signers_alias signers, state.tags, x, y, z)
  end

  let last_head_encoding =
    let open Data_encoding in
    conv
      (fun {block_hash; level} -> (block_hash, level))
      (fun (block_hash, level) -> {block_hash; level})
    @@ obj2 (req "block_hash" Block_hash.encoding) (req "level" int32)

  let read_last_head ~data_dir ~warn =
    Disk_persistence.maybe_read_value
      ~warn
      (Filename.concat data_dir "last_seen_head")
      last_head_encoding

  let set_last_head state head =
    let open Lwt_result_syntax in
    let+ () =
      Disk_persistence.write_value
        (Filename.concat state.save_dir "last_seen_head")
        last_head_encoding
        head
    in
    state.last_seen_head <- Some head

  let init_injector cctxt l1_ctxt ~head_protocols ~data_dir state
      ~retention_period ~allowed_attempts ~injection_ttl ~signers strategy tags
      =
    let open Lwt_result_syntax in
    let* signers = List.map_ep (get_signer cctxt) signers in
    let* () =
      Tezos_signer_backends.Encrypted.decrypt_list
        cctxt
        (List.map (fun k -> k.alias) signers)
    in
    let data_dir = Filename.concat data_dir "injector" in
    let*! () = Lwt_utils_unix.create_dir data_dir in
    let filter op_proj op =
      Tags.mem (Parameters.operation_tag (op_proj op)) tags
    in
    (* Warn of corrupted files but don't fail *)
    let warn file error =
      Event.(emit corrupted_operation_on_disk)
        (List.map (fun s -> s.alias) signers, tags, file, error)
    in
    let warn_unreadable = Some warn in
    let emit_event_loaded kind nb =
      Event.(emit loaded_from_disk)
        (List.map (fun s -> s.alias) signers, tags, nb, kind)
    in
    let* queue =
      Op_queue.load_from_disk
        ~warn_unreadable
        ~capacity:50_000
        ~data_dir
        ~filter:(filter (fun op -> op.Inj_operation.operation))
    in
    let*! () = emit_event_loaded "operations_queue" @@ Op_queue.length queue in
    (* Very coarse approximation for the number of operation we expect for each
       block *)
    let n =
      Tags.fold (fun t acc -> acc + Parameters.table_estimated_size t) tags 0
    in
    let injected_operations = Injected_operations.create n in
    let*! () =
      emit_event_loaded "injected_operations"
      @@ Injected_operations.length injected_operations
    in
    let included_operations =
      Included_operations.create ((confirmations + retention_period) * n)
    in
    let*! () =
      emit_event_loaded "included_operations"
      @@ Included_operations.length included_operations
    in
    let injected_ophs = Injected_ophs.create n in
    let*! () =
      emit_event_loaded "injected_ophs" @@ Injected_ophs.length injected_ophs
    in
    let included_in_blocks =
      Included_in_blocks.create ((confirmations + retention_period) * n)
    in
    let*! () =
      emit_event_loaded "included_in_blocks"
      @@ Included_in_blocks.length included_in_blocks
    in
    let*! last_seen_head = read_last_head ~data_dir ~warn in
    let proto_client =
      Inj_proto.proto_client_for_protocol head_protocols.next_protocol
    in
    return
      {
        cctxt = injector_context (cctxt :> #Client_context.full);
        l1_ctxt;
        signers;
        tags;
        strategy;
        save_dir = data_dir;
        queue;
        injected = {injected_operations; injected_ophs};
        included = {included_operations; included_in_blocks};
        state;
        retention_period;
        allowed_attempts;
        injection_ttl;
        last_seen_head;
        protocols = head_protocols;
        proto_client;
      }

  (** We consider an operation to already exist in the injector if either:
      - It is already in the queue
      - It is injected but not included
      - It is included but not confirmed. *)
  let already_exists state op_hash =
    Op_queue.find_opt state.queue op_hash <> None
    || Injected_operations.mem state.injected.injected_operations op_hash
    ||
    match
      Included_operations.find state.included.included_operations op_hash
    with
    | None -> false
    | Some {l1_level; _} -> (
        match state.last_seen_head with
        | None -> false
        | Some {level = head_level; _} ->
            Int32.sub head_level l1_level < Int32.of_int confirmations)

  (** Add an operation to the pending queue corresponding to the signer for this
    operation.  *)
  let add_pending_operation ?(retry = false) state (op : Inj_operation.t) =
    let open Lwt_result_syntax in
    if already_exists state op.hash then
      (* Ignore operations which already exist in the injector *)
      return_unit
    else
      let*! () =
        Event.(emit1 (if retry then retry_operation else add_pending))
          state
          op.operation
      in
      Op_queue.replace state.queue op.hash op

  (** Mark operations as injected (in [oph]). *)
  let add_injected_operations state {pkh = signer_pkh; _} oph ~injection_level
      operations =
    let infos =
      List.map
        (fun (op_index, op) -> (op.Inj_operation.hash, {op; oph; op_index}))
        operations
    in
    Injected_operations.replace_seq
      state.injected.injected_operations
      (List.to_seq infos) ;
    Injected_ophs.replace
      state.injected.injected_ophs
      oph
      {level = injection_level; inj_ops = List.map fst infos; signer_pkh}

  (** [add_included_operations state oph l1_block l1_level operations] marks the
    [operations] as included (in the L1 batch [oph]) in the Tezos block
    [l1_block] of level [l1_level]. *)
  let add_included_operations state l1_block l1_level
      (operations : injected_info list) =
    let infos =
      List.map
        (fun ({op; oph; op_index} : injected_info) ->
          (op.Inj_operation.hash, {op; oph; op_index; l1_block; l1_level}))
        operations
    in
    Included_operations.replace_seq
      state.included.included_operations
      (List.to_seq infos) ;
    Included_in_blocks.replace
      state.included.included_in_blocks
      l1_block
      {level = l1_level; inj_ops = List.map fst infos}

  (** [remove state oph] removes the operations that correspond to the L1 batch
    [oph] from the injected operations in the injector state. This function is
    used to move operations from injected to included. *)
  let remove_injected_operation state oph =
    match Injected_ophs.find state.injected.injected_ophs oph with
    | None ->
        (* Nothing removed *)
        []
    | Some {inj_ops; _} ->
        Injected_ophs.remove state.injected.injected_ophs oph ;
        let removed =
          List.fold_left
            (fun removed moph ->
              match
                Injected_operations.find state.injected.injected_operations moph
              with
              | None -> removed
              | Some info ->
                  Injected_operations.remove
                    state.injected.injected_operations
                    moph ;
                  info :: removed)
            []
            inj_ops
        in
        List.rev removed

  (** [forget_block state block] removes the included operations that correspond
      to all the L1 batches included in [block]. This function is used,
      e.g. when [block] is on an alternative chain in the case of a
      reorganization. *)
  let forget_block state block =
    match Included_in_blocks.find state.included.included_in_blocks block with
    | None ->
        (* Nothing removed *)
        []
    | Some {inj_ops; _} ->
        let () =
          Included_in_blocks.remove state.included.included_in_blocks block
        in
        List.fold_left
          (fun removed moph ->
            match
              Included_operations.find state.included.included_operations moph
            with
            | None -> removed
            | Some info ->
                Included_operations.remove
                  state.included.included_operations
                  moph ;
                info :: removed)
          []
          inj_ops

  let fee_parameter_of_operations state ops =
    List.fold_left
      (fun acc {Inj_operation.operation; _} ->
        let param = Parameters.fee_parameter state operation in
        {
          minimal_fees =
            {mutez = Int64.max acc.minimal_fees.mutez param.minimal_fees.mutez};
          minimal_nanotez_per_byte =
            Q.max acc.minimal_nanotez_per_byte param.minimal_nanotez_per_byte;
          minimal_nanotez_per_gas_unit =
            Q.max
              acc.minimal_nanotez_per_gas_unit
              param.minimal_nanotez_per_gas_unit;
          force_low_fee = acc.force_low_fee || param.force_low_fee;
          fee_cap = {mutez = Int64.add acc.fee_cap.mutez param.fee_cap.mutez};
          burn_cap = {mutez = Int64.add acc.burn_cap.mutez param.burn_cap.mutez};
        })
      {
        minimal_fees = {mutez = 0L};
        minimal_nanotez_per_byte = Q.zero;
        minimal_nanotez_per_gas_unit = Q.zero;
        force_low_fee = false;
        fee_cap = {mutez = 0L};
        burn_cap = {mutez = 0L};
      }
      ops

  (** The safety guard parameter is global to the simulation function, so in the
      case of a batch of L1 operations, we set this safety guard as the maximum
      of safety guards decided by the injector instance for each operation. *)
  let safety_guard_of_operations ops =
    List.fold_left
      (fun acc {Inj_operation.operation; _} ->
        let op_safety = Parameters.safety_guard operation in
        match (acc, op_safety) with
        | None, _ -> op_safety
        | Some _, None -> acc
        | Some safety, Some op_safety -> Some (max safety op_safety))
      None
      ops

  (** Returns the first half of the list [ops] if there is more than two
      elements, or [None] otherwise.  *)
  let keep_half ops =
    let total = List.length ops in
    if total <= 1 then None else Some (List.take_n (total / 2) ops)

  let available_signers state =
    let used_signers =
      Injected_ophs.fold
        (fun _ {signer_pkh; _} signers_pkh ->
          Signature.Public_key_hash.Set.add signer_pkh signers_pkh)
        state.injected.injected_ophs
        Signature.Public_key_hash.Set.empty
    in
    List.filter
      (fun s -> not @@ Signature.Public_key_hash.Set.mem s.pkh used_signers)
      state.signers

  (** [simulate_operations state operations] simulates the
      injection of [operations] and returns a triple [(op, ops, results)] where
      [op] is the packed operation with the adjusted limits, [ops] is the prefix
      of [operations] which was considered (because it did not exceed the
      quotas) and [results] are the results of the simulation. *)
  let rec simulate_operations state signer (operations : Inj_operation.t list) =
    let open Lwt_result_syntax in
    let force =
      match operations with
      | [] -> assert false
      | [_] ->
          (* If there is only one operation, fail when simulation fails *)
          false
      | _ ->
          (* We want to see which operation failed in the batch *)
          true
    in
    let op_operations =
      List.map (fun o -> o.Inj_operation.operation) operations
    in
    let*! () = Event.(emit2 simulating_operations) state op_operations force in
    let fee_parameter = fee_parameter_of_operations state.state operations in
    let safety_guard = safety_guard_of_operations operations in
    let module Proto_client = (val state.proto_client) in
    let*! simulation_result =
      Proto_client.simulate_operations
        state.cctxt
        ~force
        ~source:signer.pkh
        ~src_pk:signer.pk
        ~successor_level:true
          (* Operations are simulated in the next block, which is important for
             rollups and ok for other applications. *)
        ~fee_parameter
        ?safety_guard
        op_operations
    in
    match simulation_result with
    | Error (`TzError trace) -> fail trace
    | Error (`Exceeds_quotas trace) -> (
        let*! () =
          Event.(emit1 number_of_operations_in_queue)
            state
            (Op_queue.length state.queue)
        in
        (* We perform a dichotomy by injecting the first half of the
           operations (we are not looking to maximize the number of operations
           injected because of the cost of simulation). Only the operations
           which are actually injected will be removed from the queue so the
           other half will be reconsidered later. *)
        match keep_half operations with
        | None ->
            fail
            @@ TzTrace.cons
                 (Exn (Failure "Quotas exceeded when simulating one operation"))
                 trace
        | Some operations -> simulate_operations state signer operations)
    | Ok {operations_statuses; unsigned_operation} ->
        let*? results =
          List.combine
            ~when_different_lengths:
              [
                Exn
                  (Failure
                     "Injector: Not the same number of results as operations \
                      in simulation.");
              ]
            operations
            operations_statuses
        in
        let module Unsigned_op = struct
          module Proto_client = Proto_client

          let value = unsigned_operation
        end in
        return (results, (module Unsigned_op : Proto_unsigned_op))

  let register_error state ?signers (op : Inj_operation.t) error =
    let open Lwt_result_syntax in
    Inj_operation.register_error op error ;
    if op.errors.count > state.allowed_attempts then
      let*! () =
        Event.(emit3 ?signers discard_error_operation)
          state
          op.operation
          op.errors.count
          op.errors.last_error
      in
      Op_queue.remove state.queue op.hash
    else
      let*! () =
        Event.(emit3 ?signers error_simulation_operation)
          state
          op.operation
          op.errors.count
          error
      in
      return_unit

  let inject_on_node state ~nb signer (module Unsigned_op : Proto_unsigned_op) =
    let open Lwt_result_syntax in
    let* signed_op_bytes =
      Unsigned_op.Proto_client.sign_operation
        state.cctxt
        signer.sk
        Unsigned_op.value
    in
    let* oph =
      Tezos_shell_services.Shell_services.Injection.operation
        state.cctxt
        ~chain:state.cctxt#chain
        signed_op_bytes
    in
    let*! () = Event.(emit2 ~signers:[signer] injected) state nb oph in
    return oph

  (** Inject the given [operations] in an L1 batch. Only operations which are
      known as successful will be included in the injected L1 batch. {b Note}:
      This function incrementally builds "or-batches" by iteratively removing
      operations that fail from the desired batch. *)
  let rec inject_operations_for_signer state signer
      (operations : Inj_operation.t list) =
    let open Lwt_result_syntax in
    let*! simulation_result =
      trace (Step_failed "simulation")
      @@ simulate_operations state signer operations
    in
    let* () =
      match simulation_result with
      | Ok _ -> return_unit
      | Error error ->
          List.iter_es
            (fun op -> register_error state ~signers:[signer] op error)
            operations
    in
    let*? operations_results, raw_op = simulation_result in
    let failure = ref false in
    let* rev_non_failing_operations =
      List.fold_left_es
        (fun acc (op, {status; _}) ->
          match status with
          | Unsuccessful (Failed error) ->
              failure := true ;
              let+ () = register_error state ~signers:[signer] op error in
              acc
          | Successful
          | Unsuccessful (Backtracked | Skipped | Other_branch | Never_included)
            ->
              (* Not known to be failing *)
              return (op :: acc))
        []
        operations_results
    in
    if !failure then
      (* We try to inject without the failing operation. *)
      let operations = List.rev rev_non_failing_operations in
      inject_operations_for_signer state signer operations
    else
      (* Inject on node for real *)
      let+ oph =
        trace (Step_failed "injection")
        @@ inject_on_node ~nb:(List.length operations) state signer raw_op
      in
      let operations =
        List.map
          (fun (op, {index_in_batch; _}) -> (index_in_batch, op))
          operations_results
      in
      (oph, operations)

  (** Retrieve as many batch of operations from the queue while batch
      size remains below the size limit. *)
  let get_n_ops_batch_from_queue ~size_limit state n =
    let exception
      Reached_limit of
        (int * Inj_operation.t list * int * Inj_operation.t list list)
    in
    let module Proto_client = (val state.proto_client) in
    let min_size = Block_hash.size + Signature.size Signature.zero in
    let op_size op =
      Proto_client.operation_size op.Inj_operation.operation
      + Proto_client.operation_size_overhead
    in
    let _current_size, rev_current_ops, nb_batch, rev_ops_batch =
      try
        Op_queue.fold
          (fun _oph
               op
               ((current_size, rev_current_ops, nb_batch, rev_ops_batch) as acc) ->
            if nb_batch = n then raise (Reached_limit acc) ;
            let new_size = current_size + op_size op in
            if new_size > size_limit then
              let current_ops = List.rev rev_current_ops in
              ( min_size + op_size op,
                [op],
                nb_batch + 1,
                current_ops :: rev_ops_batch )
            else (new_size, op :: rev_current_ops, nb_batch, rev_ops_batch))
          state.queue
          (min_size, [], 0, [])
      with Reached_limit acc -> acc
    in
    let rev_ops_batch =
      if nb_batch < n then
        (* Add the last batch, even if it's not of full size, to ensure a larger number of batches. *)
        let current_ops = List.rev rev_current_ops in
        current_ops :: rev_ops_batch
      else rev_ops_batch
    in
    List.rev rev_ops_batch

  (* Ignore operations that are allowed to fail. *)
  let ignore_ignorable_failing_operations state operations =
    let open Lwt_result_syntax in
    function
    | Ok res -> return (`Injected res)
    | Error err ->
        let+ operations_to_drop =
          List.fold_left_es
            (fun to_drop op ->
              let*! retry =
                Parameters.retry_unsuccessful_operation
                  state.state
                  op.Inj_operation.operation
                  (Failed err)
              in
              match retry with
              | Abort err -> fail err
              | Retry -> return to_drop
              | Forget -> return (op :: to_drop))
            []
            operations
        in
        `Ignored operations_to_drop

  (** [inject_pending_operations_round state ?size_limit signer]
      injects operations from the pending queue [state.pending], whose
      total size does not exceed [size_limit] using [signer]. Upon
      successful injection, the operations are removed from the queue
      and marked as injected. *)
  let inject_pending_operations_round state signer operations_to_inject =
    let open Lwt_result_syntax in
    (* Retrieve and remove operations from pending *)
    let*! () =
      Event.(emit1 ~signers:[signer] considered_operations_info)
        state
        (List.map (fun o -> o.Inj_operation.operation) operations_to_inject)
    in
    let* injection_level =
      match state.last_seen_head with
      | None ->
          (* Should not happen as head monitoring fills in values *)
          let+ header =
            Tezos_shell_services.Shell_services.Blocks.Header.shell_header
              state.cctxt
              ~chain:`Main
              ~block:(`Head 0)
              ()
          in
          header.level
      | Some {level; _} -> return level
    in
    match operations_to_inject with
    | [] -> return `Stop
    | _ -> (
        let*! () =
          Event.(emit1 ~signers:[signer] injecting_pending)
            state
            (List.length operations_to_inject)
        in
        let*! res =
          inject_operations_for_signer state signer operations_to_inject
        in
        let* res =
          ignore_ignorable_failing_operations state operations_to_inject res
        in
        match res with
        | `Injected (oph, injected_operations) ->
            (* Injection succeeded, remove from pending and add to injected *)
            let* () =
              List.iter_es
                (fun (_index, op) ->
                  Op_queue.remove state.queue op.Inj_operation.hash)
                injected_operations
            in
            let*! () =
              Event.(emit2 injected_ops)
                state
                ~signers:[signer]
                oph
                (List.map
                   (fun (_, o) -> o.Inj_operation.operation)
                   injected_operations)
            in
            let () =
              add_injected_operations
                state
                signer
                oph
                ~injection_level
                injected_operations
            in
            let nb_operations = List.length injected_operations in
            return (`Continue nb_operations)
        | `Ignored operations_to_drop ->
            (* Injection failed but we ignore the failure. *)
            let*! () =
              Event.(emit1 ~signers:[signer] dropped_operations)
                state
                (List.map
                   (fun o -> o.Inj_operation.operation)
                   operations_to_drop)
            in
            let* () =
              List.iter_es
                (fun op -> Op_queue.remove state.queue op.Inj_operation.hash)
                operations_to_drop
            in
            return (`Continue 0))

  let inject_pending_operation_with_all_keys_or_no_op_left state
      ?(size_limit =
        let module Proto_client = (val state.proto_client) in
        Proto_client.max_operation_data_length) () =
    let open Lwt_result_syntax in
    let signers = available_signers state in
    let ops_batch =
      get_n_ops_batch_from_queue ~size_limit state (List.length signers)
    in
    let signers_and_ops = List.combine_drop signers ops_batch in
    let*! res_list =
      List.map_p
        (fun (signer, operations_to_inject) ->
          inject_pending_operations_round state signer operations_to_inject)
        signers_and_ops
    in
    let*? total_np_op =
      List.fold_left
        (fun res injected_res ->
          match (res, injected_res) with
          | Ok n, Ok (`Continue n') -> Ok (n + n')
          | Ok n, Ok `Stop -> Ok n
          | Error err, _ | Ok _, Error err ->
              (* only keep the first found error *) Error err)
        (Ok 0)
        res_list
    in
    return total_np_op

  (** Retrieve protocols of a block with a small cache. *)
  let protocols_of_block =
    let protos_cache = Block_cache.create 32 in
    fun state block_hash ->
      Block_cache.bind_or_put
        protos_cache
        block_hash
        (fun block_hash ->
          Tezos_shell_services.Shell_services.Blocks.protocols
            state.cctxt
            ~chain:state.cctxt#chain
            ~block:(`Hash (block_hash, 0))
            ())
        Lwt.return

  (** [register_included_operation state block level op] marks the manager
      operations contained in the L1 batch [op] as being included in the [block]
      of level [level], by moving the successful ones from the "injected" state
      to the "included" state, and re-queuing the operations that should be
      retried.  *)
  let register_included_operation state block level oph =
    let open Lwt_result_syntax in
    let injected_infos = remove_injected_operation state oph in
    match injected_infos with
    | [] ->
        (* No operations injected by us *)
        return_unit
    | _ ->
        let* block_proto = protocols_of_block state block in
        (* Protocol client for the block in which the operation is included. *)
        let module Proto_client =
          (val Inj_proto.proto_client_for_protocol block_proto.current_protocol)
        in
        let* included, to_retry =
          List.fold_left_es
            (fun (included, to_retry) (info : injected_info) ->
              let* status =
                Proto_client.operation_status
                  state.state
                  block
                  oph
                  ~index:info.op_index
              in
              match status with
              | None ->
                  failwith
                    "Cannot get status for an operation which is not included \
                     in the block"
              | Some Successful -> return (info :: included, to_retry)
              | Some (Unsuccessful status) -> (
                  let*! retry =
                    Parameters.retry_unsuccessful_operation
                      state.state
                      info.op.operation
                      status
                  in
                  let* () =
                    match status with
                    | Failed error -> register_error state info.op error
                    | Other_branch | Backtracked | Skipped | Never_included ->
                        return_unit
                  in
                  match retry with
                  | Retry -> return (included, info.op :: to_retry)
                  | Forget -> return (included, to_retry)
                  | Abort err -> fail err))
            ([], [])
            injected_infos
        in
        let*! () =
          Event.(emit3 included)
            state
            block
            level
            (List.map
               (fun (o : injected_info) -> o.op.Inj_operation.hash)
               included)
        in
        add_included_operations state block level (List.rev included) ;
        List.iter_es
          (add_pending_operation ~retry:true state)
          (List.rev to_retry)

  (** Retrieve operation hashes of a block with a small LRU cache. *)
  let manager_operations_hashes_of_block =
    let blocks_ops_cache = Block_cache.create 32 in
    fun state block_hash ->
      let module Proto_client = (val state.proto_client) in
      Block_cache.bind_or_put
        blocks_ops_cache
        block_hash
        (fun block_hash ->
          Tezos_shell_services.Shell_services.Blocks.Operation_hashes
          .operation_hashes_in_pass
            state.cctxt
            ~chain:state.cctxt#chain
            ~block:(`Hash (block_hash, 0))
            Proto_client.manager_pass)
        Lwt.return

  (** [register_included_operations state (block, level)] marks the known (by
      this injector) manager operations contained in [block] as being included. *)
  let register_included_operations state (block_hash, level) =
    let open Lwt_result_syntax in
    let* operation_hashes =
      manager_operations_hashes_of_block state block_hash
    in
    List.iter_es
      (fun oph -> register_included_operation state block_hash level oph)
      operation_hashes

  (** [revert_included_operations state block] marks the known (by this injector)
    manager operations contained in [block] as not being included any more,
    typically in the case of a reorganization where [block] is on an alternative
    chain. The operations are put back in the pending queue. *)
  let revert_included_operations state block =
    let open Lwt_result_syntax in
    let revert_infos = forget_block state block in
    let*! () =
      Event.(emit1 revert_operations)
        state
        (List.map (fun o -> o.op.hash) revert_infos)
    in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/2814
       maybe put at the front of the queue for re-injection. *)
    List.iter_es
      (fun {op; _} ->
        let*! requeue =
          Parameters.retry_unsuccessful_operation
            state.state
            op.operation
            Other_branch
        in
        match requeue with
        | Retry -> add_pending_operation ~retry:true state op
        | _ -> return_unit)
      revert_infos

  (** [register_confirmed_level state confirmed_level] is called when the level
    [confirmed_level] is known as confirmed. In this case, the operations of
    block which are below this level are also considered as confirmed and are
    removed from the "included" state. These operations cannot be part of a
    reorganization so there will be no need to re-inject them anymore. *)
  let register_confirmed_level state confirmed_level =
    let open Lwt_result_syntax in
    let*! () = Event.(emit1 confirmed_level) state confirmed_level in
    Included_in_blocks.iter_es
      (fun block ({level = inclusion_level; _} : included_l1_op_content) ->
        if
          inclusion_level
          <= Int32.sub confirmed_level (Int32.of_int state.retention_period)
        then
          let _removed_ops = forget_block state block in
          return_unit
        else return_unit)
      state.included.included_in_blocks

  (** Retry operations that are injected but never included after
      [state.injection_ttl].  *)
  let retry_expired_injected_operations state head_level =
    let open Lwt_result_syntax in
    let expired =
      Injected_ophs.fold
        (fun oph ({level = injection_level; _} : injected_l1_op_content) acc ->
          if
            head_level
            > Int32.add injection_level (Int32.of_int state.injection_ttl)
          then oph :: acc
          else acc)
        state.injected.injected_ophs
        []
    in
    let expired_infos =
      List.fold_left
        (fun acc oph ->
          let injected_infos = remove_injected_operation state oph in
          List.rev_append injected_infos acc)
        []
        expired
    in
    List.iter_es
      (fun (info : injected_info) ->
        let*! () =
          Event.(emit2 never_included)
            state
            info.op.operation
            state.injection_ttl
        in
        let*! retry =
          Parameters.retry_unsuccessful_operation
            state.state
            info.op.operation
            Never_included
        in
        match retry with
        | Abort err -> fail err
        | Retry -> add_pending_operation ~retry:true state info.op
        | Forget -> return_unit)
      (List.rev expired_infos)

  (** [on_new_tezos_head state head] is called when there is a new Tezos
      head. It first reverts any blocks that are in the alternative branch of
      the reorganization and then registers the effect of the new branch (the
      newly included operation and confirmed operations).  *)
  let on_new_tezos_head state
      ({block_hash = head_hash; level = head_level} as head) =
    let open Lwt_result_syntax in
    let*! () = Event.(emit1 new_tezos_head) state head_hash in
    let*! reorg =
      match state.last_seen_head with
      | None ->
          return {Reorg.no_reorg with new_chain = [(head_hash, head_level)]}
      | Some last_head ->
          Layer_1.get_tezos_reorg_for_new_head
            state.l1_ctxt
            (`Head (last_head.block_hash, last_head.level))
            (head_hash, head_level)
    in
    let* reorg =
      match reorg with
      | Error trace
        when TzTrace.fold
               (fun yes error ->
                 yes
                 ||
                 match error with
                 | Layer_1.Cannot_find_predecessor _ -> true
                 | _ -> false)
               false
               trace ->
          (* The reorganization could not be computed entirely because of
             missing info on the Layer 1. We may miss on some backtracking but
             it is better than to fail. *)
          let*! () = Event.(emit1 cannot_compute_reorg) state head_hash in
          return {Reorg.no_reorg with new_chain = [(head_hash, head_level)]}
      | _ -> Lwt.return reorg
    in
    let* () =
      List.iter_es
        (fun (removed_block, _) ->
          revert_included_operations state removed_block)
        (List.rev reorg.old_chain)
    in
    let* () =
      List.iter_es
        (fun added_block -> register_included_operations state added_block)
        reorg.new_chain
    in
    let* () = retry_expired_injected_operations state head_level in
    (* Head is already included in the reorganization, so no need to process it
       separately. *)
    let confirmed_level = Int32.sub head_level (Int32.of_int confirmations) in
    let* () =
      if confirmed_level >= 0l then
        register_confirmed_level state confirmed_level
      else return_unit
    in
    set_last_head state head

  (* The request {Request.Inject} triggers an injection of the operations
     the pending queue. *)
  let on_inject state =
    let open Lwt_result_syntax in
    let* total_nb_injected_op =
      inject_pending_operation_with_all_keys_or_no_op_left state ()
    in
    let*! () = Event.(emit1 total_injected_ops) state total_nb_injected_op in
    let*! () =
      Event.(emit1 number_of_operations_in_queue)
        state
        (Op_queue.length state.queue)
    in
    return ()

  module Types = struct
    type nonrec state = state

    type parameters = {
      signers : Signature.public_key_hash list;
      cctxt : Client_context.full;
      l1_ctxt : Layer_1.t;
      head_protocols : protocols;
      data_dir : string;
      state : Parameters.state;
      retention_period : int;
      allowed_attempts : int;
      injection_ttl : int;
      strategy : injection_strategy;
    }
  end

  module Name = struct
    type t = Tags.t

    let encoding = Tags.encoding

    let base = Parameters.events_section @ ["injector"]

    let pp = Tags.pp

    let equal = Tags.equal
  end

  (* The worker for the injector. *)
  module Worker = Worker.MakeSingle (Name) (Request) (Types)

  (* The queue for the requests to the injector worker is infinite. *)
  type worker = Worker.infinite Worker.queue Worker.t

  let table = Worker.create_table Queue

  let tags_table = Tags_table.create 7

  module Handlers = struct
    type self = worker

    let on_request :
        type r request_error.
        worker ->
        (r, request_error) Request.t ->
        (r, request_error) result Lwt.t =
     fun w request ->
      let state = Worker.state w in
      match request with
      | Request.Add_pending op ->
          (* The execution of the request handler is protected to avoid stopping the
             worker in case of an exception. *)
          protect @@ fun () -> add_pending_operation state op
      | Request.New_tezos_head (block_hash, level) ->
          protect @@ fun () -> on_new_tezos_head state {block_hash; level}
      | Request.Inject -> protect @@ fun () -> on_inject state

    type launch_error = error trace

    let on_launch _w tags
        Types.
          {
            signers;
            cctxt;
            l1_ctxt;
            head_protocols;
            data_dir;
            state;
            retention_period;
            allowed_attempts;
            injection_ttl;
            strategy;
          } =
      trace (Step_failed "initialization")
      @@ init_injector
           cctxt
           l1_ctxt
           ~head_protocols
           ~data_dir
           state
           ~retention_period
           ~allowed_attempts
           ~injection_ttl
           ~signers
           strategy
           tags

    let on_error (type a b) w st (r : (a, b) Request.t) (errs : b) :
        unit tzresult Lwt.t =
      let open Lwt_result_syntax in
      let state = Worker.state w in
      let request_view = Request.view r in
      let emit_and_return_errors errs =
        (* Errors do not stop the worker but emit an entry in the log. *)
        let*! () = Event.(emit3 request_failed) state request_view st errs in
        return_unit
      in
      match r with
      | Request.Add_pending _ -> emit_and_return_errors errs
      | Request.New_tezos_head _ -> emit_and_return_errors errs
      | Request.Inject -> emit_and_return_errors errs

    let on_completion w r _ st =
      let state = Worker.state w in
      Event.(emit2 request_completed_debug) state (Request.view r) st

    let on_no_request _ = Lwt.return_unit

    let on_close w =
      let state = Worker.state w in
      Tags.iter (Tags_table.remove tags_table) state.tags ;
      Lwt.return_unit
  end

  let has_tag_in ~tags state =
    match tags with
    | None ->
        (* Not filtering on tags *)
        true
    | Some tags -> not (Tags.disjoint state.tags tags)

  let delay_strategy state header f =
    let open Lwt_syntax in
    match state.strategy with
    | `Each_block -> f ()
    | `Delay_block delay_factor ->
        let module Proto_client = (val state.proto_client) in
        let time_until_next_block =
          Proto_client.time_until_next_block state.state header
          |> Ptime.Span.to_float_s
        in
        let delay = time_until_next_block *. delay_factor in
        if delay <= 0. then f ()
        else
          let promise =
            let* () = Event.(emit1 inject_wait) state delay in
            let* () = Lwt_unix.sleep delay in
            f ()
          in
          ignore promise ;
          return_unit

  let inject ?tags ?header () =
    let workers = Worker.list table in
    let tags = Option.map Tags.of_list tags in
    List.iter_p
      (fun (_signer, w) ->
        let open Lwt_syntax in
        let worker_state = Worker.state w in
        if has_tag_in ~tags worker_state then
          delay_strategy worker_state header @@ fun () ->
          let* _pushed = Worker.Queue.push_request w Request.Inject in
          return_unit
        else Lwt.return_unit)
      workers

  let notify_new_tezos_head h =
    let open Lwt_syntax in
    let workers = Worker.list table in
    List.iter_p
      (fun (_signer, w) ->
        let* (_pushed : bool) =
          Worker.Queue.push_request w (Request.New_tezos_head h)
        in
        return_unit)
      workers

  let protocols_of_head cctxt =
    Tezos_shell_services.Shell_services.Blocks.protocols
      cctxt
      ~chain:cctxt#chain
      ~block:(`Head 0)
      ()

  let update_protocol ~next_protocol =
    let workers = Worker.list table in
    List.iter
      (fun (_signer, w) ->
        let state = Worker.state w in
        let protocols =
          {current_protocol = state.protocols.next_protocol; next_protocol}
        in
        state.proto_client <- Inj_proto.proto_client_for_protocol next_protocol ;
        state.protocols <- protocols)
      workers

  (** Retrieve next protocol of a block with a small cache from protocol levels
      to protocol hashes. *)
  let next_protocol_of_block =
    let proto_levels_cache = Int_cache.create 7 in
    fun (cctxt : Client_context.full)
        (block_hash, (block_header : Block_header.t)) ->
      let open Lwt_result_syntax in
      let proto_level = block_header.shell.proto_level in
      Int_cache.bind_or_put
        proto_levels_cache
        proto_level
        (fun _proto_level ->
          let+ protos =
            Tezos_shell_services.Shell_services.Blocks.protocols
              cctxt
              ~chain:cctxt#chain
              ~block:(`Hash (block_hash, 0))
              ()
          in
          protos.next_protocol)
        Lwt.return

  let rec monitor_l1_chain (cctxt : #Client_context.full) l1_ctxt =
    let open Lwt_result_syntax in
    let*! res =
      Layer_1.iter_heads l1_ctxt @@ fun (head_hash, header) ->
      let head = (head_hash, header.shell.level) in
      let* next_protocol =
        next_protocol_of_block (cctxt :> Client_context.full) (head_hash, header)
      in
      update_protocol ~next_protocol ;
      (* Notify all workers of a new Tezos head *)
      let*! () = notify_new_tezos_head head in
      return_unit
    in
    (* Ignore errors *)
    let*! () =
      match res with
      | Error error -> Internal_event.Simple.emit Event.monitoring_error error
      | Ok () -> Lwt.return_unit
    in
    monitor_l1_chain cctxt l1_ctxt

  let rec async_monitor_l1_chain (cctxt : #Client_context.full) l1_ctxt =
    Lwt.dont_wait
      (fun () -> monitor_l1_chain cctxt l1_ctxt)
      (fun exn ->
        Format.eprintf
          "Warning: Error in async monitoring (%s), restarting"
          (Printexc.to_string exn) ;
        async_monitor_l1_chain cctxt l1_ctxt)

  let check_signer_is_not_already_registered registered_signers signers =
    List.iter_ep
      (fun pkh ->
        fail_when (Signature.Public_key_hash.Set.mem pkh registered_signers)
        @@ error_of_fmt
             "key %a is already registered"
             Signature.Public_key_hash.pp
             pkh)
      signers

  let check_tag_is_not_already_registered registered_tags tags =
    error_unless (Tags.disjoint tags registered_tags)
    @@ error_of_fmt "tags %a is already registered" Tags.pp tags

  let register_worker cctxt l1_ctxt head_protocols ~data_dir state
      retention_period allowed_attempts injection_ttl signers strategy tags =
    let open Lwt_result_syntax in
    let* worker =
      Worker.launch
        table
        tags
        {
          signers;
          cctxt = (cctxt :> Client_context.full);
          l1_ctxt;
          head_protocols;
          data_dir;
          state;
          retention_period;
          allowed_attempts;
          injection_ttl;
          strategy;
        }
        (module Handlers)
    in
    let () = Tags.iter (fun tag -> Tags_table.add tags_table tag worker) tags in
    return_unit

  let register_disjoint_signers (cctxt : #Client_context.full) l1_ctxt ~data_dir
      retention_period ~allowed_attempts ~injection_ttl state
      (signers :
        (Signature.public_key_hash list
        * injection_strategy
        * Parameters.Tag.t list)
        list) =
    let open Lwt_result_syntax in
    let rec aux registered_signers registered_tags = function
      | [] -> return_unit
      | (signers, strategy, tags) :: rest ->
          let* () =
            check_signer_is_not_already_registered registered_signers signers
          in
          let tags = Tags.of_list tags in
          let*? () = check_tag_is_not_already_registered registered_tags tags in
          let*? () = Inj_proto.check_registered_proto_clients state in
          let* head_protocols = protocols_of_head cctxt in
          let* _ =
            register_worker
              cctxt
              l1_ctxt
              head_protocols
              ~data_dir
              state
              retention_period
              allowed_attempts
              injection_ttl
              signers
              strategy
              tags
          in
          let registered_signers =
            Signature.Public_key_hash.Set.add_seq
              (List.to_seq signers)
              registered_signers
          in
          let registered_tags = Tags.union tags registered_tags in
          aux registered_signers registered_tags rest
    in
    aux Signature.Public_key_hash.Set.empty Tags.empty signers

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2754
     Injector worker in a separate process *)
  let init (cctxt : #Client_context.full) ~data_dir ?(retention_period = 0)
      ?(allowed_attempts = 10) ?(injection_ttl = 120)
      ?(reconnection_delay = 2.0) state ~signers =
    let open Lwt_result_syntax in
    assert (retention_period >= 0) ;
    assert (allowed_attempts >= 0) ;
    assert (injection_ttl > 0) ;
    let*! l1_ctxt = Layer_1.start ~name:"injector" ~reconnection_delay cctxt in
    let* () =
      register_disjoint_signers
        (cctxt : #Client_context.full)
        l1_ctxt
        ~data_dir
        retention_period
        ~allowed_attempts
        ~injection_ttl
        state
        signers
    in
    async_monitor_l1_chain cctxt l1_ctxt ;
    return_unit

  let worker_of_tag tag =
    let open Result_syntax in
    match Tags_table.find_opt tags_table tag with
    | None ->
        Format.kasprintf
          (fun s -> tzfail (No_worker_for_tag s))
          "%a"
          Parameters.Tag.pp
          tag
    | Some worker -> return worker

  let add_pending_operation op =
    let open Lwt_result_syntax in
    let operation = Inj_operation.make op in
    let*? w = worker_of_tag (Parameters.operation_tag op) in
    let*! (_pushed : bool) =
      Worker.Queue.push_request w (Request.Add_pending operation)
    in
    return operation.hash

  let shutdown () =
    let workers = Worker.list table in
    (* Don't shutdown L1 monitoring otherwise worker shutdown hangs *)
    List.iter_p (fun (_signer, w) -> Worker.shutdown w) workers

  let op_status_in_worker state l1_hash =
    match Op_queue.find_opt state.queue l1_hash with
    | Some op -> Some (Pending op.operation)
    | None -> (
        match
          Injected_operations.find state.injected.injected_operations l1_hash
        with
        | Some info -> Some (Injected info)
        | None -> (
            match
              Included_operations.find
                state.included.included_operations
                l1_hash
            with
            | Some info -> Some (Included info)
            | None -> None))

  let operation_status l1_hash =
    let workers = Worker.list table in
    List.find_map
      (fun (_signer, w) -> op_status_in_worker (Worker.state w) l1_hash)
      workers

  let register_proto_client = Inj_proto.register
end
