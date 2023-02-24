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
open Injector_common
open Injector_worker_types
open Injector_sigs
open Injector_errors

(* This is the Tenderbake finality for blocks. *)
(* TODO: https://gitlab.com/tezos/tezos/-/issues/2815
   Centralize this and maybe make it configurable. *)
let confirmations = 2

type injection_strategy = [`Each_block | `Delay_block of float]

(** Builds a client context from another client context but uses logging instead
    of printing on stdout directly. This client context cannot make the injector
    exit. *)
let injector_context (cctxt : #Protocol_client_context.full) =
  let log _channel msg = Logs_lwt.info (fun m -> m "%s" msg) in
  object
    inherit
      Protocol_client_context.wrap_full
        (new Client_context.proxy_context (cctxt :> Client_context.full))

    inherit! Client_context.simple_printer log

    method! exit code =
      Format.ksprintf Stdlib.failwith "Injector client wants to exit %d" code
  end

let manager_operation_result_status (type kind)
    (op_result : kind Apply_results.manager_operation_result) : operation_status
    =
  match op_result with
  | Applied _ -> Successful
  | Backtracked (_, None) -> Unsuccessful Backtracked
  | Skipped _ -> Unsuccessful Skipped
  | Backtracked (_, Some err)
  (* Backtracked because internal operation failed *)
  | Failed (_, err) ->
      Unsuccessful (Failed (Environment.wrap_tztrace err))

let operation_result_status (type kind)
    (op_result : kind Apply_results.contents_result) : operation_status =
  match op_result with
  | Preendorsement_result _ -> Successful
  | Endorsement_result _ -> Successful
  | Dal_attestation_result _ -> Successful
  | Seed_nonce_revelation_result _ -> Successful
  | Vdf_revelation_result _ -> Successful
  | Double_endorsement_evidence_result _ -> Successful
  | Double_preendorsement_evidence_result _ -> Successful
  | Double_baking_evidence_result _ -> Successful
  | Activate_account_result _ -> Successful
  | Proposals_result -> Successful
  | Ballot_result -> Successful
  | Drain_delegate_result _ -> Successful
  | Manager_operation_result {operation_result; _} ->
      manager_operation_result_status operation_result

let operation_contents_status (type kind)
    (contents : kind Apply_results.contents_result_list) ~index :
    operation_status tzresult =
  let rec rec_status :
      type kind. int -> kind Apply_results.contents_result_list -> _ =
   fun n -> function
    | Apply_results.Single_result _ when n <> 0 ->
        error_with "No operation with index %d" index
    | Single_result result -> Ok (operation_result_status result)
    | Cons_result (result, _rest) when n = 0 ->
        Ok (operation_result_status result)
    | Cons_result (_result, rest) -> rec_status (n - 1) rest
  in
  rec_status index contents

let operation_status (operation : Protocol.operation_receipt) ~index :
    operation_status tzresult =
  match (operation : _) with
  | No_operation_metadata ->
      error_with "Cannot find operation status because metadata is missing"
  | Operation_metadata {contents} -> operation_contents_status contents ~index

module Make (Parameters : PARAMETERS) = struct
  module Tags = Injector_tags.Make (Parameters.Tag)
  module Tags_table = Hashtbl.Make (Parameters.Tag)
  module POperation = Parameters.Operation
  module Inj_operation = Injector_operation.Make (POperation)
  module Request = Request (Inj_operation)

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

  type status =
    | Pending of POperation.t
    | Injected of injected_info
    | Included of included_info

  let injected_info_encoding =
    let open Data_encoding in
    conv
      (fun ({op; oph; op_index} : injected_info) -> (op, (oph, op_index)))
      (fun (op, (oph, op_index)) -> {op; oph; op_index})
    @@ merge_objs
         Inj_operation.encoding
         (obj1
            (req
               "layer1"
               (obj2
                  (req "operation_hash" Operation_hash.encoding)
                  (req "operation_index" int31))))

  let included_info_encoding =
    let open Data_encoding in
    conv
      (fun {op; oph; op_index; l1_block; l1_level} ->
        (op, (oph, op_index, l1_block, l1_level)))
      (fun (op, (oph, op_index, l1_block, l1_level)) ->
        {op; oph; op_index; l1_block; l1_level})
    @@ merge_objs
         Inj_operation.encoding
         (obj1
            (req
               "layer1"
               (obj4
                  (req "operation_hash" Operation_hash.encoding)
                  (req "operation_index" int31)
                  (req "block_hash" Block_hash.encoding)
                  (req "level" int32))))

  module Op_queue =
    Disk_persistence.Make_queue
      (struct
        let name = "operations_queue"
      end)
      (Inj_operation.Hash)
      (Inj_operation)

  module Injected_operations = Disk_persistence.Make_table (struct
    include Inj_operation.Hash.Table

    type value = injected_info

    let name = "injected_operations"

    let string_of_key = Inj_operation.Hash.to_b58check

    let key_of_string = Inj_operation.Hash.of_b58check_opt

    let value_encoding = injected_info_encoding
  end)

  module Injected_ophs = Disk_persistence.Make_table (struct
    include Operation_hash.Table

    type value = Inj_operation.Hash.t list

    let name = "injected_ophs"

    let string_of_key = Operation_hash.to_b58check

    let key_of_string = Operation_hash.of_b58check_opt

    let value_encoding = Data_encoding.list Inj_operation.Hash.encoding
  end)

  (** The part of the state which gathers information about injected
    operations (but not included). *)
  type injected_state = {
    injected_operations : Injected_operations.t;
        (** A table mapping L1 manager operation hashes to the injection info for that
          operation.  *)
    injected_ophs : Injected_ophs.t;
        (** A mapping of all L1 manager operations contained in a L1 batch (i.e. an L1
          operation). *)
  }

  module Included_operations = Disk_persistence.Make_table (struct
    include Inj_operation.Hash.Table

    type value = included_info

    let name = "included_operations"

    let string_of_key = Inj_operation.Hash.to_b58check

    let key_of_string = Inj_operation.Hash.of_b58check_opt

    let value_encoding = included_info_encoding
  end)

  module Included_in_blocks = Disk_persistence.Make_table (struct
    include Block_hash.Table

    type value = int32 * Inj_operation.Hash.t list

    let name = "included_in_blocks"

    let string_of_key = Block_hash.to_b58check

    let key_of_string = Block_hash.of_b58check_opt

    let value_encoding =
      let open Data_encoding in
      obj2 (req "level" int32) (req "l1_ops" (list Inj_operation.Hash.encoding))
  end)

  (** The part of the state which gathers information about
    operations which are included in the L1 chain (but not confirmed). *)
  type included_state = {
    included_operations : Included_operations.t;
    included_in_blocks : Included_in_blocks.t;
  }

  (** The internal state of each injector worker.  *)
  type state = {
    cctxt : Protocol_client_context.full;
        (** The client context which is used to perform the injections. *)
    constants : Constants.t;  (** The constants of the protocol. *)
    signer : signer;  (** The signer for this worker. *)
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
  }

  module Event = struct
    include
      Injector_events.Make (Parameters) (Tags) (POperation) (Inj_operation)
        (Request)

    let emit1 e state x = emit e (state.signer.pkh, state.tags, x)

    let emit2 e state x y = emit e (state.signer.pkh, state.tags, x, y)

    let emit3 e state x y z = emit e (state.signer.pkh, state.tags, x, y, z)
  end

  let init_injector cctxt constants ~data_dir state ~retention_period ~signer
      strategy tags =
    let open Lwt_result_syntax in
    let* signer = get_signer cctxt signer in
    let data_dir = Filename.concat data_dir "injector" in
    let*! () = Lwt_utils_unix.create_dir data_dir in
    let filter op_proj op =
      Tags.mem (Parameters.operation_tag (op_proj op)) tags
    in
    let warn_unreadable =
      (* Warn of corrupted files but don't fail *)
      Some
        (fun file error ->
          Event.(emit corrupted_operation_on_disk)
            (signer.pkh, tags, file, error))
    in
    let emit_event_loaded kind nb =
      Event.(emit loaded_from_disk) (signer.pkh, tags, nb, kind)
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
    let* injected_operations =
      Injected_operations.load_from_disk
        ~warn_unreadable
        ~initial_size:n
        ~data_dir
        ~filter:(filter (fun (i : injected_info) -> i.op.operation))
    in
    let*! () =
      emit_event_loaded "injected_operations"
      @@ Injected_operations.length injected_operations
    in

    let* included_operations =
      Included_operations.load_from_disk
        ~warn_unreadable
        ~initial_size:((confirmations + retention_period) * n)
        ~data_dir
        ~filter:(filter (fun (i : included_info) -> i.op.operation))
    in
    let*! () =
      emit_event_loaded "included_operations"
      @@ Included_operations.length included_operations
    in
    let* injected_ophs =
      Injected_ophs.load_from_disk
        ~warn_unreadable
        ~initial_size:n
        ~data_dir
        ~filter:(List.exists (Injected_operations.mem injected_operations))
    in
    let*! () =
      emit_event_loaded "injected_ophs" @@ Injected_ophs.length injected_ophs
    in
    let* included_in_blocks =
      Included_in_blocks.load_from_disk
        ~warn_unreadable
        ~initial_size:((confirmations + retention_period) * n)
        ~data_dir
        ~filter:(fun (_, ops) ->
          List.exists (Included_operations.mem included_operations) ops)
    in
    let*! () =
      emit_event_loaded "included_in_blocks"
      @@ Included_in_blocks.length included_in_blocks
    in

    return
      {
        cctxt = injector_context (cctxt :> #Protocol_client_context.full);
        constants;
        signer;
        tags;
        strategy;
        save_dir = data_dir;
        queue;
        injected = {injected_operations; injected_ophs};
        included = {included_operations; included_in_blocks};
        state;
        retention_period;
      }

  (** Add an operation to the pending queue corresponding to the signer for this
    operation.  *)
  let add_pending_operation ?(retry = false) state op =
    let open Lwt_result_syntax in
    let*! () =
      Event.(emit1 (if retry then retry_operation else add_pending))
        state
        op.Inj_operation.operation
    in
    let* () = Op_queue.replace state.queue op.hash op in
    let*! () =
      Event.(emit1 number_of_operations_in_queue)
        state
        (Op_queue.length state.queue)
    in
    return_unit

  (** Mark operations as injected (in [oph]). *)
  let add_injected_operations state oph operations =
    let open Lwt_result_syntax in
    let infos =
      List.map
        (fun (op_index, op) -> (op.Inj_operation.hash, {op; oph; op_index}))
        operations
    in
    let* () =
      Injected_operations.replace_seq
        state.injected.injected_operations
        (List.to_seq infos)
    in
    Injected_ophs.replace state.injected.injected_ophs oph (List.map fst infos)

  (** [add_included_operations state oph l1_block l1_level operations] marks the
    [operations] as included (in the L1 batch [oph]) in the Tezos block
    [l1_block] of level [l1_level]. *)
  let add_included_operations state l1_block l1_level
      (operations : injected_info list) =
    let open Lwt_result_syntax in
    let*! () =
      Event.(emit3 included)
        state
        l1_block
        l1_level
        (List.map
           (fun (o : injected_info) -> o.op.Inj_operation.hash)
           operations)
    in
    let infos =
      List.map
        (fun ({op; oph; op_index} : injected_info) ->
          (op.Inj_operation.hash, {op; oph; op_index; l1_block; l1_level}))
        operations
    in
    let* () =
      Included_operations.replace_seq
        state.included.included_operations
        (List.to_seq infos)
    in
    Included_in_blocks.replace
      state.included.included_in_blocks
      l1_block
      (l1_level, List.map fst infos)

  (** [remove state oph] removes the operations that correspond to the L1 batch
    [oph] from the injected operations in the injector state. This function is
    used to move operations from injected to included. *)
  let remove_injected_operation state oph =
    let open Lwt_result_syntax in
    match Injected_ophs.find state.injected.injected_ophs oph with
    | None ->
        (* Nothing removed *)
        return []
    | Some mophs ->
        let* () = Injected_ophs.remove state.injected.injected_ophs oph in
        let+ removed =
          List.fold_left_es
            (fun removed moph ->
              match
                Injected_operations.find state.injected.injected_operations moph
              with
              | None -> return removed
              | Some info ->
                  let+ () =
                    Injected_operations.remove
                      state.injected.injected_operations
                      moph
                  in
                  info :: removed)
            []
            mophs
        in
        List.rev removed

  (** [forget_block state block] removes the included operations that correspond
      to all the L1 batches included in [block]. This function is used,
      e.g. when [block] is on an alternative chain in the case of a
      reorganization. *)
  let forget_block state block =
    let open Lwt_result_syntax in
    match Included_in_blocks.find state.included.included_in_blocks block with
    | None ->
        (* Nothing removed *)
        return []
    | Some (_level, mophs) ->
        let* () =
          Included_in_blocks.remove state.included.included_in_blocks block
        in
        List.fold_left_es
          (fun removed moph ->
            match
              Included_operations.find state.included.included_operations moph
            with
            | None -> return removed
            | Some info ->
                let+ () =
                  Included_operations.remove
                    state.included.included_operations
                    moph
                in
                info :: removed)
          []
          mophs

  let fee_parameter_of_operations state ops =
    List.fold_left
      (fun acc {Inj_operation.operation; _} ->
        let param = Parameters.fee_parameter state operation in
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

  (** Returns the first half of the list [ops] if there is more than two
      elements, or [None] otherwise.  *)
  let keep_half ops =
    let total = List.length ops in
    if total <= 1 then None else Some (List.take_n (total / 2) ops)

  (** [simulate_operations ~must_succeed state operations] simulates the
      injection of [operations] and returns a triple [(op, ops, results)] where
      [op] is the packed operation with the adjusted limits, [ops] is the prefix
      of [operations] which was considered (because it did not exceed the
      quotas) and [results] are the results of the simulation. See
      {!inject_operations} for the specification of [must_succeed]. *)
  let rec simulate_operations ~must_succeed state
      (operations : Inj_operation.t list) =
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
    let*! () =
      Event.(emit2 simulating_operations)
        state
        (List.map (fun o -> o.Inj_operation.operation) operations)
        force
    in
    let fee_parameter = fee_parameter_of_operations state.state operations in
    let annotated_operations =
      List.map
        (fun {Inj_operation.operation; _} ->
          let (Manager operation) = POperation.to_manager_operation operation in
          Annotated_manager_operation
            (Injection.prepare_manager_operation
               ~fee:Limit.unknown
               ~gas_limit:Limit.unknown
               ~storage_limit:Limit.unknown
               operation))
        operations
    in
    let (Manager_list annot_op) =
      Annotated_manager_operation.manager_of_list annotated_operations
    in
    let*! simulation_result =
      Injection.inject_manager_operation
        state.cctxt
        ~simulation:true (* Only simulation here *)
        ~force
        ~chain:state.cctxt#chain
        ~block:(`Head 0)
        ~source:state.signer.pkh
        ~src_pk:state.signer.pk
        ~src_sk:state.signer.sk
        ~successor_level:true
          (* Operations are simulated in the next block, which is important for rollups
             and ok for other applications. *)
        ~fee:Limit.unknown
        ~gas_limit:Limit.unknown
        ~storage_limit:Limit.unknown
        ~fee_parameter
        annot_op
    in
    match simulation_result with
    | Error trace ->
        let*! () =
          Event.(emit1 number_of_operations_in_queue)
            state
            (Op_queue.length state.queue)
        in
        let exceeds_quota =
          TzTrace.fold
            (fun exceeds -> function
              | Environment.Ecoproto_error
                  (Gas.Block_quota_exceeded | Gas.Operation_quota_exceeded) ->
                  true
              | _ -> exceeds)
            false
            trace
        in
        if exceeds_quota then
          (* We perform a dichotomy by injecting the first half of the
             operations (we are not looking to maximize the number of operations
             injected because of the cost of simulation). Only the operations
             which are actually injected will be removed from the queue so the
             other half will be reconsidered later. *)
          match keep_half operations with
          | None -> fail trace
          | Some operations ->
              simulate_operations ~must_succeed state operations
        else fail trace
    | Ok (_, op, _, result) ->
        let nb_ops = List.length operations in
        let nb_packed_ops =
          let {protocol_data = Operation_data {contents; _}; _} = op in
          Alpha_context.Operation.to_list (Contents_list contents)
          |> List.length
        in
        (* packed_op can have reveal operations added automatically. *)
        let start_index = nb_packed_ops - nb_ops in
        (* Add indexes of operations in the packed, i.e. batched, operation. *)
        let operations =
          List.mapi (fun i op -> (i + start_index, op)) operations
        in
        return (op, operations, Apply_results.Contents_result_list result)

  let inject_on_node state ~nb
      {shell; protocol_data = Operation_data {contents; _}} =
    let open Lwt_result_syntax in
    let unsigned_op = (shell, Contents_list contents) in
    let unsigned_op_bytes =
      Data_encoding.Binary.to_bytes_exn Operation.unsigned_encoding unsigned_op
    in
    let* signature =
      Client_keys.sign
        state.cctxt
        ~watermark:Signature.Generic_operation
        state.signer.sk
        unsigned_op_bytes
    in
    let op : _ Operation.t =
      {shell; protocol_data = {contents; signature = Some signature}}
    in
    let op_bytes =
      Data_encoding.Binary.to_bytes_exn Operation.encoding (Operation.pack op)
    in
    Tezos_shell_services.Shell_services.Injection.operation
      state.cctxt
      ~chain:state.cctxt#chain
      op_bytes
    >>=? fun oph ->
    let*! () = Event.(emit2 injected) state nb oph in
    return oph

  (** Inject the given [operations] in an L1 batch. If [must_succeed] is [`All]
    then all the operations must succeed in the simulation of injection. If
    [must_succeed] is [`At_least_one] at least one operation in the list
    [operations] must be successful in the simulation. In any case, only
    operations which are known as successful will be included in the injected L1
    batch. {b Note}: [must_succeed = `At_least_one] allows to incrementally build
    "or-batches" by iteratively removing operations that fail from the desired
    batch. *)
  let rec inject_operations ~must_succeed state
      (operations : Inj_operation.t list) =
    let open Lwt_result_syntax in
    let* packed_op, operations, result =
      trace (Step_failed "simulation")
      @@ simulate_operations ~must_succeed state operations
    in
    let (Contents_result_list contents_result) = result in
    let failure = ref false in
    let* rev_non_failing_operations =
      List.fold_left_es
        (fun acc (index, op) ->
          let open Lwt_result_syntax in
          let*? status = operation_contents_status contents_result ~index in
          match status with
          | Unsuccessful (Failed error) ->
              let*! () =
                Event.(emit2 dropping_operation)
                  state
                  op.Inj_operation.operation
                  error
              in
              failure := true ;
              return acc
          | Successful | Unsuccessful (Backtracked | Skipped | Other_branch) ->
              (* Not known to be failing *)
              return (op :: acc))
        []
        operations
    in
    if !failure then
      (* Invariant: must_succeed = `At_least_one, otherwise the simulation would
         have returned an error. We try to inject without the failing
         operation. *)
      let operations = List.rev rev_non_failing_operations in
      inject_operations ~must_succeed state operations
    else
      (* Inject on node for real *)
      let+ oph =
        trace (Step_failed "injection")
        @@ inject_on_node ~nb:(List.length operations) state packed_op
      in
      (oph, operations)

  (** Returns the (upper bound on) the size of an L1 batch of operations composed
    of the manager operations [rev_ops]. *)
  let size_l1_batch state rev_ops =
    let contents_list =
      List.map
        (fun (op : Inj_operation.t) ->
          let {fee; counter; gas_limit; storage_limit} =
            Parameters.approximate_fee_bound state.state op.operation
          in
          let (Manager operation) =
            POperation.to_manager_operation op.operation
          in
          let contents =
            Manager_operation
              {
                source = state.signer.pkh;
                operation;
                fee;
                counter;
                gas_limit;
                storage_limit;
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
    let signature = Signature.zero in
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
    let exception Reached_limit of Inj_operation.t list in
    let rev_ops =
      try
        Op_queue.fold
          (fun _oph op ops ->
            let new_ops = op :: ops in
            let new_size = size_l1_batch state new_ops in
            if new_size > size_limit then raise (Reached_limit ops) ;
            new_ops)
          state.queue
          []
      with Reached_limit ops -> ops
    in
    List.rev rev_ops

  (* Ignore operations that are allowed to fail. *)
  let ignore_ignorable_failing_operations state operations = function
    | Ok res -> return (`Injected res)
    | Error err ->
        let open Lwt_result_syntax in
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

  (** [inject_pending_operations_for ~size_limit state pending] injects
    operations from the pending queue [pending], whose total size does
    not exceed [size_limit]. Upon successful injection, the
    operations are removed from the queue and marked as injected. *)
  let inject_pending_operations
      ?(size_limit = Constants.max_operation_data_length) state =
    let open Lwt_result_syntax in
    (* Retrieve and remove operations from pending *)
    let operations_to_inject = get_operations_from_queue ~size_limit state in
    let*! () =
      Event.(emit1 considered_operations_info)
        state
        (List.map (fun o -> o.Inj_operation.operation) operations_to_inject)
    in
    match operations_to_inject with
    | [] -> return_unit
    | _ -> (
        let*! () =
          Event.(emit1 injecting_pending)
            state
            (List.length operations_to_inject)
        in
        let must_succeed =
          Parameters.batch_must_succeed
          @@ List.map
               (fun op -> op.Inj_operation.operation)
               operations_to_inject
        in
        let*! res =
          inject_operations ~must_succeed state operations_to_inject
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
            add_injected_operations state oph injected_operations
        | `Ignored operations_to_drop ->
            (* Injection failed but we ignore the failure. *)
            let*! () =
              Event.(emit1 dropped_operations)
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
            return_unit)

  (** [register_included_operation state block level op] marks the manager
      operations contained in the L1 batch [op] as being included in the [block]
      of level [level], by moving the successful ones from the "injected" state
      to the "included" state, and re-queuing the operations that should be
      retried.  *)
  let register_included_operation state block level
      (operation : Alpha_block_services.operation) =
    let open Lwt_result_syntax in
    let* injected_infos = remove_injected_operation state operation.hash in
    match injected_infos with
    | [] ->
        (* No operations injected by us *)
        return_unit
    | _ ->
        let* included, to_retry =
          List.fold_left_es
            (fun (included, to_retry) (info : injected_info) ->
              let*? receipt =
                match operation.receipt with
                | Empty ->
                    error_with
                      "Empty receipt for %a"
                      Operation_hash.pp
                      operation.hash
                | Too_large ->
                    error_with
                      "Receipt too large for %a"
                      Operation_hash.pp
                      operation.hash
                | Receipt r -> Ok r
              in
              let*? status = operation_status receipt ~index:info.op_index in
              match status with
              | Successful -> return (info :: included, to_retry)
              | Unsuccessful status -> (
                  let*! retry =
                    Parameters.retry_unsuccessful_operation
                      state.state
                      info.op.operation
                      status
                  in
                  match retry with
                  | Retry -> return (included, info.op :: to_retry)
                  | Forget -> return (included, to_retry)
                  | Abort err -> fail err))
            ([], [])
            injected_infos
        in
        let* () =
          add_included_operations state block level (List.rev included)
        in
        List.iter_es
          (add_pending_operation ~retry:true state)
          (List.rev to_retry)

  (** [register_included_operations state block level oph] marks the known (by
    this injector) manager operations contained in [block] as being included. *)
  let register_included_operations state
      (block : Alpha_block_services.block_info) =
    List.iter_es
      (List.iter_es (fun (op : Alpha_block_services.operation) ->
           register_included_operation
             state
             block.hash
             block.header.shell.level
             op))
      block.Alpha_block_services.operations

  (** [revert_included_operations state block] marks the known (by this injector)
    manager operations contained in [block] as not being included any more,
    typically in the case of a reorganization where [block] is on an alternative
    chain. The operations are put back in the pending queue. *)
  let revert_included_operations state block =
    let open Lwt_result_syntax in
    let* revert_infos = forget_block state block in
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
    let*! () =
      Event.(emit confirmed_level)
        (state.signer.pkh, state.tags, confirmed_level)
    in
    Included_in_blocks.iter_es
      (fun block (level, _operations) ->
        if
          level
          <= Int32.sub confirmed_level (Int32.of_int state.retention_period)
        then
          let* _removed_ops = forget_block state block in
          return_unit
        else return_unit)
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
    let* () =
      List.iter_es
        (fun removed_block ->
          revert_included_operations
            state
            removed_block.Alpha_block_services.hash)
        (List.rev reorg.old_chain)
    in
    let* () =
      List.iter_es
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
    if confirmed_level >= 0l then register_confirmed_level state confirmed_level
    else return_unit

  (* The request {Request.Inject} triggers an injection of the operations
     the pending queue. *)
  let on_inject state = inject_pending_operations state

  module Types = struct
    type nonrec state = state

    type parameters = {
      cctxt : Protocol_client_context.full;
      constants : Constants.t;
      data_dir : string;
      state : Parameters.state;
      retention_period : int;
      strategy : injection_strategy;
      tags : Tags.t;
    }
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
      | Request.New_tezos_head (head, reorg) ->
          protect @@ fun () -> on_new_tezos_head state head reorg
      | Request.Inject -> protect @@ fun () -> on_inject state

    type launch_error = error trace

    let on_launch _w signer
        Types.
          {cctxt; constants; data_dir; state; retention_period; strategy; tags}
        =
      trace (Step_failed "initialization")
      @@ init_injector
           cctxt
           constants
           ~data_dir
           state
           ~retention_period
           ~signer
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
      match Request.view r with
      | Request.View (Add_pending _ | New_tezos_head _) ->
          Event.(emit2 request_completed_debug) state (Request.view r) st
      | View Inject ->
          Event.(emit2 request_completed_notice) state (Request.view r) st

    let on_no_request _ = Lwt.return_unit

    let on_close w =
      let state = Worker.state w in
      Tags.iter (Tags_table.remove tags_table) state.tags ;
      Lwt.return_unit
  end

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2754
     Injector worker in a separate process *)
  let init (cctxt : #Protocol_client_context.full) ~data_dir
      ?(retention_period = 0) state ~signers =
    let open Lwt_result_syntax in
    assert (retention_period >= 0) ;
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
                  | `Each_block, `Each_block -> `Each_block
                  | `Delay_block f, _ | _, `Delay_block f ->
                      (* Delay_block strategy takes over because we can always wait a
                         little bit more to inject operation which are to be injected
                         "each block". *)
                      `Delay_block f
                in
                (strategy, Tags.union other_tags tags)
          in
          Signature.Public_key_hash.Map.add signer (strategy, tags) acc)
        Signature.Public_key_hash.Map.empty
        signers
    in
    let* constants =
      Protocol.Constants_services.all cctxt (cctxt#chain, cctxt#block)
    in
    Signature.Public_key_hash.Map.iter_es
      (fun signer (strategy, tags) ->
        let+ worker =
          Worker.launch
            table
            signer
            {
              cctxt = (cctxt :> Protocol_client_context.full);
              constants;
              data_dir;
              state;
              retention_period;
              strategy;
              tags;
            }
            (module Handlers)
        in
        ignore worker)
      signers_map

  let worker_of_signer signer_pkh =
    match Worker.find_opt table signer_pkh with
    | None ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/2818
           maybe lazily start worker here *)
        error (No_worker_for_source signer_pkh)
    | Some worker -> ok worker

  let worker_of_tag tag =
    match Tags_table.find_opt tags_table tag with
    | None ->
        Format.kasprintf
          (fun s -> error (No_worker_for_tag s))
          "%a"
          Parameters.Tag.pp
          tag
    | Some worker -> ok worker

  let add_pending_operation ?source op =
    let open Lwt_result_syntax in
    let operation = Inj_operation.make op in
    let*? w =
      match source with
      | Some source -> worker_of_signer source
      | None -> worker_of_tag (Parameters.operation_tag op)
    in
    let*! (_pushed : bool) =
      Worker.Queue.push_request w (Request.Add_pending operation)
    in
    return operation.hash

  let new_tezos_head h reorg =
    let open Lwt_syntax in
    let workers = Worker.list table in
    List.iter_p
      (fun (_signer, w) ->
        let* (_pushed : bool) =
          Worker.Queue.push_request w (Request.New_tezos_head (h, reorg))
        in
        return_unit)
      workers

  let has_tag_in ~tags state =
    match tags with
    | None ->
        (* Not filtering on tags *)
        true
    | Some tags -> not (Tags.disjoint state.tags tags)

  let time_until_next_block constants (header : Tezos_base.Block_header.t) =
    let open Result_syntax in
    let Constants.Parametric.{minimal_block_delay; delay_increment_per_round; _}
        =
      constants.Constants.parametric
    in
    let next_level_timestamp =
      let* durations =
        Round.Durations.create
          ~first_round_duration:minimal_block_delay
          ~delay_increment_per_round
      in
      let* predecessor_round = Fitness.round_from_raw header.shell.fitness in
      Round.timestamp_of_round
        durations
        ~predecessor_timestamp:header.shell.timestamp
        ~predecessor_round
        ~round:Round.zero
    in
    let next_level_timestamp =
      Result.value
        next_level_timestamp
        ~default:
          (WithExceptions.Result.get_ok
             ~loc:__LOC__
             Timestamp.(header.shell.timestamp +? minimal_block_delay))
    in
    Ptime.diff
      (Time.System.of_protocol_exn next_level_timestamp)
      (Time.System.now ())

  let delay_stategy state header f =
    let open Lwt_syntax in
    match state.strategy with
    | `Each_block -> f ()
    | `Delay_block delay_factor ->
        let time_until_next_block =
          match header with
          | None ->
              state.constants.Constants.parametric.minimal_block_delay
              |> Period.to_seconds |> Int64.to_float
          | Some header ->
              time_until_next_block state.constants header
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
          delay_stategy worker_state header @@ fun () ->
          let* _pushed = Worker.Queue.push_request w Request.Inject in
          return_unit
        else Lwt.return_unit)
      workers

  let shutdown () =
    let workers = Worker.list table in
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
end
