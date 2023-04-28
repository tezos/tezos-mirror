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

open Protocol
open Alpha_context
open Injector_sigs
module Block_cache =
  Aches_lwt.Lache.Make_result
    (Aches.Rache.Transfer (Aches.Rache.LRU) (Block_hash))

module Parameters :
  PARAMETERS
    with type state = Node_context.ro
     and type Tag.t = Configuration.purpose
     and type Operation.t = L1_operation.t = struct
  type state = Node_context.ro

  let events_section = [Protocol.name; "sc_rollup_node"]

  module Tag : TAG with type t = Configuration.purpose = struct
    type t = Configuration.purpose

    let compare = Stdlib.compare

    let equal = Stdlib.( = )

    let hash = Hashtbl.hash

    let string_of_tag = Configuration.string_of_purpose

    let pp ppf t = Format.pp_print_string ppf (string_of_tag t)

    let encoding : t Data_encoding.t =
      let open Data_encoding in
      string_enum
        (List.map (fun t -> (string_of_tag t, t)) Configuration.purposes)
  end

  module Operation = L1_operation

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3459
     Very coarse approximation for the number of operation we
     expect for each block *)
  let table_estimated_size : Tag.t -> int = function
    | Publish -> 1
    | Add_messages -> 100
    | Cement -> 1
    | Timeout -> 1
    | Refute -> 1

  let operation_tag : Operation.t -> Tag.t = function
    | Add_messages _ -> Add_messages
    | Cement _ -> Cement
    | Publish _ -> Publish
    | Timeout _ -> Timeout
    | Refute _ -> Refute

  let fee_parameter node_ctxt operation =
    let {
      Injection.minimal_fees;
      minimal_nanotez_per_byte;
      minimal_nanotez_per_gas_unit;
      force_low_fee;
      fee_cap;
      burn_cap;
    } =
      Node_context.get_fee_parameter node_ctxt (operation_tag operation)
    in
    {
      minimal_fees = {mutez = Tez.to_mutez minimal_fees};
      minimal_nanotez_per_byte;
      minimal_nanotez_per_gas_unit;
      force_low_fee;
      fee_cap = {mutez = Tez.to_mutez fee_cap};
      burn_cap = {mutez = Tez.to_mutez burn_cap};
    }

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3459
     Decide if some batches must have all the operations succeed. See
     {!Injector_sigs.Parameter.batch_must_succeed}. *)
  let batch_must_succeed _ = `At_least_one

  let retry_unsuccessful_operation _node_ctxt (_op : Operation.t) status =
    let open Lwt_syntax in
    match status with
    | Backtracked | Skipped | Other_branch ->
        (* Always retry backtracked or skipped operations, or operations that
           are on another branch because of a reorg:

           - Commitments are always produced on finalized blocks. They don't
             need to be recomputed, and as such are valid in another branch.

           - The cementation operations should be re-injected because the node
             only keeps track of the last cemented level and the last published
             commitment, without rollbacks.

           - Messages posted to an inbox should be re-emitted (i.e. re-queued)
             in case of a fork.

           - Timeout should be re-submitted as the timeout may be reached as well
             on the other branch.

           - Refutation should be re-submitted in case of fork.
             TODO: https://gitlab.com/tezos/tezos/-/issues/3459
             maybe check if game exists on other branch as well.
        *)
        return Retry
    | Failed error -> (
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/4071
           Think about which operations should be retried and when. *)
        match classify_trace error with
        | Permanent | Outdated -> return Forget
        | Branch | Temporary -> return Retry)
    | Never_included ->
        (* Forget operations that are never included *)
        return Forget
end

module Proto_client = struct
  open Protocol_client_context

  type operation = Parameters.Operation.t

  type state = Parameters.state

  type unsigned_operation =
    Tezos_base.Operation.shell_header * packed_contents_list

  let max_operation_data_length = Constants.max_operation_data_length

  let manager_pass = Operation_repr.manager_pass

  let manager_operation_size (Manager operation) =
    let contents =
      Manager_operation
        {
          source = Signature.Public_key_hash.zero;
          operation;
          fee = Tez.zero;
          counter = Manager_counter.Internal_for_tests.of_int 0;
          gas_limit = Gas.Arith.zero;
          storage_limit = Z.zero;
        }
    in
    Data_encoding.Binary.length
      Operation.contents_encoding_with_legacy_attestation_name
      (Contents contents)

  let operation_size op =
    manager_operation_size (L1_operation.to_manager_operation op)

  (* The operation size overhead is an upper bound (in practice) of the overhead
     that will be added to a manager operation. To compute it we can use any
     manager operation (here a revelation), add an overhead with upper bounds as
     values (for the fees, limits, counters, etc.) and compare the encoded
     operations with respect to their size.
     NOTE: This information is only used to pre-select operations from the
     injector queue as a candidate batch. *)
  let operation_size_overhead =
    let dummy_operation =
      Reveal
        (Signature.Public_key.of_b58check_exn
           "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav")
    in
    let dummy_contents =
      Manager_operation
        {
          source = Signature.Public_key_hash.zero;
          operation = dummy_operation;
          fee = Tez.of_mutez_exn 3_000_000L;
          counter = Manager_counter.Internal_for_tests.of_int 500_000;
          gas_limit = Gas.Arith.integral_of_int_exn 500_000;
          storage_limit = Z.of_int 500_000;
        }
    in
    let dummy_size =
      Data_encoding.Binary.length
        Operation.contents_encoding_with_legacy_attestation_name
        (Contents dummy_contents)
    in
    dummy_size - manager_operation_size (Manager dummy_operation)

  let manager_operation_result_status (type kind)
      (op_result : kind Apply_results.manager_operation_result) :
      operation_status =
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

  let operation_status_of_receipt (operation : Protocol.operation_receipt)
      ~index : operation_status tzresult =
    match (operation : _) with
    | No_operation_metadata ->
        error_with "Cannot find operation status because metadata is missing"
    | Operation_metadata {contents} -> operation_contents_status contents ~index

  let get_block_operations =
    let ops_cache = Block_cache.create 32 in
    fun cctxt block_hash ->
      Block_cache.bind_or_put
        ops_cache
        block_hash
        (fun block_hash ->
          let open Lwt_result_syntax in
          let+ operations =
            Alpha_block_services.Operations.operations_in_pass
              cctxt
              ~chain:cctxt#chain
              ~block:(`Hash (block_hash, 0))
              ~metadata:`Always
              manager_pass
          in
          List.fold_left
            (fun acc (op : Alpha_block_services.operation) ->
              Operation_hash.Map.add op.hash op acc)
            Operation_hash.Map.empty
            operations)
        Lwt.return

  let operation_status (node_ctxt : Node_context.ro) block_hash operation_hash
      ~index =
    let open Lwt_result_syntax in
    let* operations = get_block_operations node_ctxt.cctxt block_hash in
    match Operation_hash.Map.find_opt operation_hash operations with
    | None -> return_none
    | Some operation -> (
        match operation.receipt with
        | Empty ->
            failwith "Cannot find operation status because metadata is empty"
        | Too_large ->
            failwith
              "Cannot find operation status because metadata is too large"
        | Receipt receipt ->
            let*? status = operation_status_of_receipt receipt ~index in
            return_some status)

  let dummy_sk_uri =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Tezos_signer_backends.Unencrypted.make_sk
    @@ Signature.Secret_key.of_b58check_exn
         "edsk3UqeiQWXX7NFEY1wUs6J1t2ez5aQ3hEWdqX5Jr5edZiGLW8nZr"

  let simulate_operations cctxt ~force ~source ~src_pk ~successor_level
      ~fee_parameter operations =
    let open Lwt_result_syntax in
    let fee_parameter : Injection.fee_parameter =
      {
        minimal_fees = Tez.of_mutez_exn fee_parameter.minimal_fees.mutez;
        minimal_nanotez_per_byte = fee_parameter.minimal_nanotez_per_byte;
        minimal_nanotez_per_gas_unit =
          fee_parameter.minimal_nanotez_per_gas_unit;
        force_low_fee = fee_parameter.force_low_fee;
        fee_cap = Tez.of_mutez_exn fee_parameter.fee_cap.mutez;
        burn_cap = Tez.of_mutez_exn fee_parameter.burn_cap.mutez;
      }
    in
    let open Annotated_manager_operation in
    let annotated_operations =
      List.map
        (fun operation ->
          let (Manager operation) =
            L1_operation.to_manager_operation operation
          in
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
    let cctxt =
      new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
    in
    let*! simulation_result =
      Injection.inject_manager_operation
        cctxt
        ~simulation:true (* Only simulation here *)
        ~force
        ~chain:cctxt#chain
        ~block:(`Head 0)
        ~source
        ~src_pk
        ~src_sk:dummy_sk_uri
          (* Use dummy secret key as it is not used by simulation *)
        ~successor_level
        ~fee:Limit.unknown
        ~gas_limit:Limit.unknown
        ~storage_limit:Limit.unknown
        ~fee_parameter
        annot_op
    in
    match simulation_result with
    | Error trace ->
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
        fail (if exceeds_quota then `Exceeds_quotas trace else `TzError trace)
    | Ok (_oph, packed_op, _contents, results) ->
        let nb_ops = List.length operations in
        let results = Apply_results.to_list (Contents_result_list results) in
        (* packed_op can have reveal operations added automatically. *)
        let start_index = List.length results - nb_ops in
        (* remove extra reveal operations *)
        let operations_statuses =
          List.fold_left_i
            (fun index_in_batch acc (Apply_results.Contents_result result) ->
              if index_in_batch < start_index then acc
              else
                {index_in_batch; status = operation_result_status result} :: acc)
            []
            results
          |> List.rev
        in
        let unsigned_operation =
          let {shell; protocol_data = Operation_data {contents; signature = _}}
              =
            packed_op
          in
          (shell, Contents_list contents)
        in
        return {operations_statuses; unsigned_operation}

  let sign_operation cctxt src_sk
      ((shell, Contents_list contents) as unsigned_op) =
    let open Lwt_result_syntax in
    let unsigned_bytes =
      Data_encoding.Binary.to_bytes_exn
        Operation.unsigned_encoding_with_legacy_attestation_name
        unsigned_op
    in
    let cctxt =
      new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
    in
    let+ signature =
      Client_keys.sign
        cctxt
        ~watermark:Signature.Generic_operation
        src_sk
        unsigned_bytes
    in
    let op : packed_operation =
      {
        shell;
        protocol_data = Operation_data {contents; signature = Some signature};
      }
    in
    Data_encoding.Binary.to_bytes_exn
      Operation.encoding_with_legacy_attestation_name
      op

  let time_until_next_block (node_ctxt : Node_context.ro)
      (header : Tezos_base.Block_header.shell_header option) =
    let open Result_syntax in
    let Constants.Parametric.{minimal_block_delay; delay_increment_per_round; _}
        =
      node_ctxt.protocol_constants.Constants.parametric
    in
    match header with
    | None ->
        minimal_block_delay |> Period.to_seconds |> Int64.to_int
        |> Ptime.Span.of_int_s
    | Some header ->
        let next_level_timestamp =
          let* durations =
            Round.Durations.create
              ~first_round_duration:minimal_block_delay
              ~delay_increment_per_round
          in
          let* predecessor_round = Fitness.round_from_raw header.fitness in
          Round.timestamp_of_round
            durations
            ~predecessor_timestamp:header.timestamp
            ~predecessor_round
            ~round:Round.zero
        in
        let next_level_timestamp =
          Result.value
            next_level_timestamp
            ~default:
              (WithExceptions.Result.get_ok
                 ~loc:__LOC__
                 Timestamp.(header.timestamp +? minimal_block_delay))
        in
        Ptime.diff
          (Time.System.of_protocol_exn next_level_timestamp)
          (Time.System.now ())
end

include Injector_functor.Make (Parameters)

let () = register_proto_client Protocol.hash (module Proto_client)
