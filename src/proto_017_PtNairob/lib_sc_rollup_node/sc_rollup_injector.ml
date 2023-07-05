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

let injector_operation_to_manager :
    L1_operation.t -> Protocol.Alpha_context.packed_manager_operation = function
  | Add_messages {messages} -> Manager (Sc_rollup_add_messages {messages})
  | Cement {rollup; commitment} ->
      let rollup = Sc_rollup_proto_types.Address.of_octez rollup in
      let commitment =
        Sc_rollup_proto_types.Commitment_hash.of_octez commitment
      in
      Manager (Sc_rollup_cement {rollup; commitment})
  | Publish {rollup; commitment} ->
      let rollup = Sc_rollup_proto_types.Address.of_octez rollup in
      let commitment = Sc_rollup_proto_types.Commitment.of_octez commitment in
      Manager (Sc_rollup_publish {rollup; commitment})
  | Refute {rollup; opponent; refutation} ->
      let rollup = Sc_rollup_proto_types.Address.of_octez rollup in
      let refutation =
        Sc_rollup_proto_types.Game.refutation_of_octez refutation
      in
      Manager (Sc_rollup_refute {rollup; opponent; refutation})
  | Timeout {rollup; stakers} ->
      let rollup = Sc_rollup_proto_types.Address.of_octez rollup in
      let stakers = Sc_rollup_proto_types.Game.index_of_octez stakers in
      Manager (Sc_rollup_timeout {rollup; stakers})

let injector_operation_of_manager :
    type kind.
    kind Protocol.Alpha_context.manager_operation -> L1_operation.t option =
  function
  | Sc_rollup_add_messages {messages} -> Some (Add_messages {messages})
  | Sc_rollup_cement {rollup; commitment} ->
      let rollup = Sc_rollup_proto_types.Address.to_octez rollup in
      let commitment =
        Sc_rollup_proto_types.Commitment_hash.to_octez commitment
      in
      Some (Cement {rollup; commitment})
  | Sc_rollup_publish {rollup; commitment} ->
      let rollup = Sc_rollup_proto_types.Address.to_octez rollup in
      let commitment = Sc_rollup_proto_types.Commitment.to_octez commitment in
      Some (Publish {rollup; commitment})
  | Sc_rollup_refute {rollup; opponent; refutation} ->
      let rollup = Sc_rollup_proto_types.Address.to_octez rollup in
      let refutation =
        Sc_rollup_proto_types.Game.refutation_to_octez refutation
      in
      Some (Refute {rollup; opponent; refutation})
  | Sc_rollup_timeout {rollup; stakers} ->
      let rollup = Sc_rollup_proto_types.Address.to_octez rollup in
      let stakers = Sc_rollup_proto_types.Game.index_to_octez stakers in
      Some (Timeout {rollup; stakers})
  | _ -> None

module Proto_client = struct
  open Protocol_client_context

  type operation = L1_operation.t

  type state = Injector.state

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
    Data_encoding.Binary.length Operation.contents_encoding (Contents contents)

  let operation_size op =
    manager_operation_size (injector_operation_to_manager op)

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
        Operation.contents_encoding
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

  let operation_status {Injector.cctxt; _} block_hash operation_hash ~index =
    let open Lwt_result_syntax in
    let* operations = get_block_operations cctxt block_hash in
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
          let (Manager operation) = injector_operation_to_manager operation in
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
      Data_encoding.Binary.to_bytes_exn Operation.unsigned_encoding unsigned_op
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
    Data_encoding.Binary.to_bytes_exn Operation.encoding op

  let time_until_next_block
      {Injector.minimal_block_delay; delay_increment_per_round; _}
      (header : Tezos_base.Block_header.shell_header option) =
    let open Result_syntax in
    match header with
    | None -> minimal_block_delay |> Int64.to_int |> Ptime.Span.of_int_s
    | Some header ->
        let minimal_block_delay = Period.of_seconds_exn minimal_block_delay in
        let delay_increment_per_round =
          Period.of_seconds_exn delay_increment_per_round
        in
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

  let check_fee_parameters Injector.{fee_parameters; _} =
    let check_value purpose name compare to_string mempool_default value =
      if compare mempool_default value > 0 then
        error_with
          "Bad configuration fee_parameter.%s for %s. It must be at least %s \
           for operations of the injector to be propagated."
          name
          (Configuration.string_of_purpose purpose)
          (to_string mempool_default)
      else Ok ()
    in
    let check purpose
        {
          Injector_sigs.minimal_fees;
          minimal_nanotez_per_byte;
          minimal_nanotez_per_gas_unit;
          force_low_fee = _;
          fee_cap = _;
          burn_cap = _;
        } =
      let open Result_syntax in
      let+ () =
        check_value
          purpose
          "minimal_fees"
          Int64.compare
          Int64.to_string
          (Protocol.Alpha_context.Tez.to_mutez
             Plugin.Mempool.default_minimal_fees)
          minimal_fees.mutez
      and+ () =
        check_value
          purpose
          "minimal_nanotez_per_byte"
          Q.compare
          Q.to_string
          Plugin.Mempool.default_minimal_nanotez_per_byte
          minimal_nanotez_per_byte
      and+ () =
        check_value
          purpose
          "minimal_nanotez_per_gas_unit"
          Q.compare
          Q.to_string
          Plugin.Mempool.default_minimal_nanotez_per_gas_unit
          minimal_nanotez_per_gas_unit
      in
      ()
    in
    Configuration.Operator_purpose_map.iter_e check fee_parameters

  let checks state = check_fee_parameters state
end

let () = Injector.register_proto_client Protocol.hash (module Proto_client)
