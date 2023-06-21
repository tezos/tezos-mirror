(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Block_validator_errors
open Validation_errors

module Event = struct
  include Internal_event.Simple

  let inherited_inconsistent_cache =
    declare_1
      ~section:["block"; "validation"]
      ~name:"block_validation_inconsistent_cache"
      ~msg:"applied block {hash} with an inconsistent cache: reloading cache"
      ~level:Warning
      ~pp1:Block_hash.pp
      ("hash", Block_hash.encoding)
end

type operation = {
  hash : Operation_hash.t;
  operation : Operation.t;
  check_signature : bool;
}

let mk_operation ?known_valid_operation_set operation =
  let hash = Operation.hash operation in
  let check_signature =
    match known_valid_operation_set with
    | None -> true
    | Some known_valid -> not (Operation_hash.Set.mem hash known_valid)
    (* We do not check the signature of the operation
       if the operation is already known valid *)
  in
  {hash; operation; check_signature}

let operation_encoding =
  let open Data_encoding in
  conv
    (fun {hash; operation; check_signature} ->
      (hash, operation, check_signature))
    (fun (hash, operation, check_signature) ->
      {hash; operation; check_signature})
    (obj3
       (req "hash" Operation_hash.encoding)
       (req "operation" Operation.encoding)
       (req "check_signature" bool))

type validation_store = {
  resulting_context_hash : Context_hash.t;
  timestamp : Time.Protocol.t;
  message : string option;
  max_operations_ttl : int;
  last_allowed_fork_level : Int32.t;
}

let validation_store_encoding =
  let open Data_encoding in
  conv
    (fun {
           resulting_context_hash;
           timestamp;
           message;
           max_operations_ttl;
           last_allowed_fork_level;
         } ->
      ( resulting_context_hash,
        timestamp,
        message,
        max_operations_ttl,
        last_allowed_fork_level ))
    (fun ( resulting_context_hash,
           timestamp,
           message,
           max_operations_ttl,
           last_allowed_fork_level ) ->
      {
        resulting_context_hash;
        timestamp;
        message;
        max_operations_ttl;
        last_allowed_fork_level;
      })
    (obj5
       (req "resulting_context_hash" Context_hash.encoding)
       (req "timestamp" Time.Protocol.encoding)
       (req "message" (option string))
       (req "max_operations_ttl" int31)
       (req "last_allowed_fork_level" int32))

type operation_metadata = Metadata of Bytes.t | Too_large_metadata

let operation_metadata_equal m1 m2 =
  match (m1, m2) with
  | Metadata b1, Metadata b2 -> Bytes.equal b1 b2
  | Too_large_metadata, Too_large_metadata -> true
  | Metadata _, Too_large_metadata | Too_large_metadata, Metadata _ -> false

let operation_metadata_encoding =
  let open Data_encoding in
  def
    "operation_metadata"
    ~title:"operation_metadata"
    ~description:"An operation metadata"
    (union
       ~tag_size:`Uint8
       [
         case
           ~title:"metadata"
           ~description:"Content of an operation's metadata."
           (Tag 0)
           bytes
           (function Metadata bytes -> Some bytes | _ -> None)
           (fun bytes -> Metadata bytes);
         case
           ~title:"too_large_metadata"
           ~description:
             "Nothing, as this operation metadata was declared to be too large \
              to be considered."
           (Tag 1)
           unit
           (function Too_large_metadata -> Some () | _ -> None)
           (fun () -> Too_large_metadata);
       ])

type ops_metadata =
  | No_metadata_hash of operation_metadata list list
  | Metadata_hash of (operation_metadata * Operation_metadata_hash.t) list list

module Shell_header_hash =
  Blake2B.Make
    (Base58)
    (struct
      let name = "shell_header_hash"

      let title = "A shell header identifier"

      let b58check_prefix = Base58.Prefix.block_hash

      let size = None
    end)

let hash_shell_header shell =
  Shell_header_hash.hash_bytes
    [Data_encoding.Binary.to_bytes_exn Block_header.shell_header_encoding shell]

type result = {
  shell_header_hash : Shell_header_hash.t;
  validation_store : validation_store;
  block_metadata : Bytes.t * Block_metadata_hash.t option;
  ops_metadata : ops_metadata;
}

type apply_result = {
  result : result;
  cache : Tezos_protocol_environment.Context.cache;
}

let check_proto_environment_version_increasing block_hash before after =
  let open Result_syntax in
  if Protocol.compare_version before after <= 0 then return_unit
  else
    tzfail
      (invalid_block
         block_hash
         (Invalid_protocol_environment_transition (before, after)))

let update_testchain_status ctxt ~predecessor_hash timestamp =
  let open Lwt_syntax in
  let* tc = Context_ops.get_test_chain ctxt in
  match tc with
  | Not_running -> Lwt.return ctxt
  | Running {expiration; _} ->
      if Time.Protocol.(expiration <= timestamp) then
        Context_ops.add_test_chain ctxt Not_running
      else Lwt.return ctxt
  | Forking {protocol; expiration} ->
      let genesis =
        Context_ops.compute_testchain_genesis ctxt predecessor_hash
      in
      let chain_id = Chain_id.of_block_hash genesis in
      (* legacy semantics *)
      Context_ops.add_test_chain
        ctxt
        (Running {chain_id; genesis; protocol; expiration})

let init_test_chain chain_id ctxt forked_header =
  let open Lwt_result_syntax in
  let*! tc = Context_ops.get_test_chain ctxt in
  match tc with
  | Not_running | Running _ -> assert false
  | Forking {protocol; _} ->
      let* (module Proto_test) =
        match Registered_protocol.get protocol with
        | Some proto -> return proto
        | None -> tzfail (Missing_test_protocol protocol)
      in
      let test_ctxt = ctxt in
      let*! () =
        Validation_events.(emit new_protocol_initialisation protocol)
      in
      let* {context = test_ctxt; _} =
        Proto_test.init chain_id test_ctxt forked_header.Block_header.shell
      in
      let*! test_ctxt = Context_ops.add_test_chain test_ctxt Not_running in
      let*! test_ctxt = Context_ops.add_protocol test_ctxt protocol in
      Lwt_result.ok
      @@ Context_ops.commit_test_chain_genesis test_ctxt forked_header

let result_encoding =
  let open Data_encoding in
  let ops_metadata_encoding =
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"no metadata hash"
          (Tag 0)
          (list (list operation_metadata_encoding))
          (function
            | No_metadata_hash ops_metadata -> Some ops_metadata | _ -> None)
          (fun ops_metadata -> No_metadata_hash ops_metadata);
        case
          ~title:"metadata hash"
          (Tag 1)
          (list
             (list
                (tup2
                   operation_metadata_encoding
                   Operation_metadata_hash.encoding)))
          (function
            | Metadata_hash ops_metadata -> Some ops_metadata | _ -> None)
          (fun ops_metadata -> Metadata_hash ops_metadata);
      ]
  in
  conv
    (fun {shell_header_hash; validation_store; block_metadata; ops_metadata} ->
      (shell_header_hash, validation_store, block_metadata, ops_metadata))
    (fun (shell_header_hash, validation_store, block_metadata, ops_metadata) ->
      {shell_header_hash; validation_store; block_metadata; ops_metadata})
    (obj4
       (req "shell_header_hash" Shell_header_hash.encoding)
       (req "validation_store" validation_store_encoding)
       (req "block_metadata" (tup2 bytes (option Block_metadata_hash.encoding)))
       (req "ops_metadata" ops_metadata_encoding))

let preapply_result_encoding :
    (Block_header.shell_header * error Preapply_result.t list) Data_encoding.t =
  let open Data_encoding in
  obj2
    (req "shell_header" Block_header.shell_header_encoding)
    (req
       "preapplied_operations_result"
       (list (Preapply_result.encoding Tezos_rpc.Error.encoding)))

let may_force_protocol_upgrade ~user_activated_upgrades ~level
    (validation_result : Tezos_protocol_environment.validation_result) =
  let open Lwt_syntax in
  match
    Block_header.get_forced_protocol_upgrade ~user_activated_upgrades ~level
  with
  | None -> return validation_result
  | Some hash ->
      let* context =
        Tezos_protocol_environment.Context.set_protocol
          validation_result.context
          hash
      in
      return {validation_result with context}

(** Applies user activated updates based either on block level or on
    voted protocols *)
let may_patch_protocol ~user_activated_upgrades
    ~user_activated_protocol_overrides ~level
    (validation_result : Tezos_protocol_environment.validation_result) =
  let open Lwt_syntax in
  let context = validation_result.context in
  let* protocol = Context_ops.get_protocol context in
  match
    Block_header.get_voted_protocol_overrides
      ~user_activated_protocol_overrides
      protocol
  with
  | None ->
      may_force_protocol_upgrade
        ~user_activated_upgrades
        ~level
        validation_result
  | Some replacement_protocol ->
      let* context =
        Tezos_protocol_environment.Context.set_protocol
          validation_result.context
          replacement_protocol
      in
      return {validation_result with context}

module Make (Proto : Registered_protocol.T) = struct
  type 'operation_data preapplied_operation = {
    hash : Operation_hash.t;
    raw : Operation.t;
    check_signature : bool;
    protocol_data : 'operation_data;
  }

  type preapply_state = {
    validation_state : Proto.validation_state;
    application_state : Proto.application_state;
    applied :
      (Proto.operation_data preapplied_operation * Proto.operation_receipt) list;
    live_blocks : Block_hash.Set.t;
    live_operations : Operation_hash.Set.t;
  }

  type preapply_result =
    | Applied of preapply_state * Proto.operation_receipt
    | Branch_delayed of error list
    | Branch_refused of error list
    | Refused of error list
    | Outdated

  let check_block_header ~(predecessor_block_header : Block_header.t)
      ~predecessor_resulting_context_hash hash (block_header : Block_header.t) =
    let open Lwt_result_syntax in
    let* () =
      fail_unless
        (Int32.succ predecessor_block_header.shell.level
        = block_header.shell.level)
        (invalid_block hash
        @@ Invalid_level
             {
               expected = Int32.succ predecessor_block_header.shell.level;
               found = block_header.shell.level;
             })
    in
    let* () =
      fail_unless
        Time.Protocol.(
          predecessor_block_header.shell.timestamp
          < block_header.shell.timestamp)
        (invalid_block hash Non_increasing_timestamp)
    in
    let* () =
      fail_unless
        Fitness.(
          predecessor_block_header.shell.fitness < block_header.shell.fitness)
        (invalid_block hash Non_increasing_fitness)
    in
    let* () =
      fail_unless
        Compare.List_length_with.(
          Proto.validation_passes = block_header.shell.validation_passes)
        (invalid_block
           hash
           (Unexpected_number_of_validation_passes
              block_header.shell.validation_passes))
    in
    let is_consistent =
      match Proto.expected_context_hash with
      | Predecessor_resulting_context ->
          Context_hash.equal
            block_header.shell.context
            predecessor_resulting_context_hash
      | Resulting_context ->
          (* The check that a header's context is the resulting context
             will be performed post-application (when the resulting
             context is known). *)
          true
    in
    let* () =
      fail_unless
        is_consistent
        (Validation_errors.Inconsistent_hash
           (predecessor_resulting_context_hash, block_header.shell.context))
    in
    return_unit

  let parse_block_header block_hash (block_header : Block_header.t) =
    let open Lwt_result_syntax in
    match
      Data_encoding.Binary.of_bytes_opt
        Proto.block_header_data_encoding
        block_header.protocol_data
    with
    | None -> tzfail (invalid_block block_hash Cannot_parse_block_header)
    | Some protocol_data ->
        return
          ({shell = block_header.shell; protocol_data} : Proto.block_header)

  let check_one_operation_quota block_hash pass ops quota =
    let open Lwt_result_syntax in
    let* () =
      fail_unless
        (match quota.Tezos_protocol_environment.max_op with
        | None -> true
        | Some max -> Compare.List_length_with.(ops <= max))
        (let max = Option.value ~default:~-1 quota.max_op in
         invalid_block
           block_hash
           (Too_many_operations {pass; found = List.length ops; max}))
    in
    List.iter_ep
      (fun {hash; operation; _} ->
        let size = Data_encoding.Binary.length Operation.encoding operation in
        fail_unless
          (size <= Proto.max_operation_data_length)
          (invalid_block
             block_hash
             (Oversized_operation
                {operation = hash; size; max = Proto.max_operation_data_length})))
      ops

  let check_operation_quota block_hash operations =
    let combined =
      match
        List.combine
          ~when_different_lengths:()
          operations
          Proto.validation_passes
      with
      | Ok combined -> combined
      | Error () ->
          raise (Invalid_argument "Block_validation.check_operation_quota")
    in
    List.iteri_ep
      (fun i (ops, quota) ->
        (* passes are 1-based, iteri is 0-based *)
        let pass = i + 1 in
        check_one_operation_quota block_hash pass ops quota)
      combined

  let parse_operations block_hash operations =
    List.mapi_es
      (fun pass ->
        let open Lwt_result_syntax in
        List.map_es (fun {hash; operation; check_signature} ->
            match
              Data_encoding.Binary.of_bytes_opt
                Proto.operation_data_encoding_with_legacy_attestation_name
                operation.Operation.proto
            with
            | None ->
                tzfail (invalid_block block_hash (Cannot_parse_operation hash))
            | Some protocol_data ->
                let op = {Proto.shell = operation.shell; protocol_data} in
                let allowed_pass = Proto.acceptable_pass op in
                let is_pass_consistent =
                  match allowed_pass with
                  | None -> false
                  | Some n -> Int.equal pass n
                in
                let* () =
                  fail_unless
                    is_pass_consistent
                    (invalid_block
                       block_hash
                       (Unallowed_pass {operation = hash; pass; allowed_pass}))
                in
                return (hash, op, check_signature)))
      operations

  (* FIXME: This code is used by preapply but emitting time
     measurement events in prevalidation should not impact current
     benchmarks.
     See https://gitlab.com/tezos/tezos/-/issues/2716 *)
  let compute_metadata ~operation_metadata_size_limit proto_env_version
      block_data ops_metadata =
    let open Lwt_result_syntax in
    (* Block and operation metadata hashes are not required for
       environment V0. *)
    let should_include_metadata_hashes =
      match proto_env_version with
      | Protocol.V0 -> false
      | Protocol.(V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10) -> true
    in
    let block_metadata =
      let metadata =
        Data_encoding.Binary.to_bytes_exn
          Proto.block_header_metadata_encoding
          block_data
      in
      let metadata_hash_opt =
        if should_include_metadata_hashes then
          Some (Block_metadata_hash.hash_bytes [metadata])
        else None
      in
      (metadata, metadata_hash_opt)
    in
    let* ops_metadata =
      try[@time.duration_lwt metadata_serde_check]
        let metadata_list_list =
          List.map
            (List.map (fun receipt ->
                 (* Check that the metadata are
                    serializable/deserializable *)
                 let bytes =
                   Data_encoding.Binary.to_bytes_exn
                     Proto
                     .operation_receipt_encoding_with_legacy_attestation_name
                     receipt
                 in
                 let _ =
                   Data_encoding.Binary.of_bytes_exn
                     Proto
                     .operation_receipt_encoding_with_legacy_attestation_name
                     bytes
                 in
                 let metadata =
                   match operation_metadata_size_limit with
                   | Shell_limits.Unlimited -> Metadata bytes
                   | Limited size_limit ->
                       if Bytes.length bytes < size_limit then Metadata bytes
                       else Too_large_metadata
                 in
                 (bytes, metadata)))
            ops_metadata
        in
        if [@time.duration_lwt metadata_hash] should_include_metadata_hashes
        then
          return
            (Metadata_hash
               (List.map
                  (List.map (fun (bytes, metadata) ->
                       let metadata_hash =
                         Operation_metadata_hash.hash_bytes [bytes]
                       in
                       (metadata, metadata_hash)))
                  metadata_list_list))
        else
          return (No_metadata_hash (List.map (List.map snd) metadata_list_list))
      with exn ->
        trace
          Validation_errors.Cannot_serialize_operation_metadata
          (tzfail (Exn exn))
    in
    return (block_metadata, ops_metadata)

  let prepare_context predecessor_block_metadata_hash
      predecessor_ops_metadata_hash (block_header : Proto.block_header)
      predecessor_context predecessor_hash =
    let open Lwt_result_syntax in
    let*! context =
      update_testchain_status
        predecessor_context
        ~predecessor_hash
        block_header.shell.timestamp
    in
    let*! context =
      match predecessor_block_metadata_hash with
      | None -> Lwt.return context
      | Some hash ->
          Context_ops.add_predecessor_block_metadata_hash context hash
    in
    let*! context =
      match predecessor_ops_metadata_hash with
      | None -> Lwt.return context
      | Some hash -> Context_ops.add_predecessor_ops_metadata_hash context hash
    in
    return context

  (* FIXME: This code is used by recompute_metadata but emitting time
     measurement events in proto_apply_operations should not impact
     current benchmarks.
     See https://gitlab.com/tezos/tezos/-/issues/2716 *)
  let proto_apply_operations chain_id context cache
      (predecessor_block_header : Block_header.t) block_header block_hash
      operations =
    let open Lwt_result_syntax in
    trace
      (invalid_block block_hash Economic_protocol_error)
      (let* state =
         (Proto.begin_application
            context
            chain_id
            (Application block_header)
            ~predecessor:predecessor_block_header.shell
            ~cache [@time.duration_lwt application_beginning])
       in
       let* state, ops_metadata =
         (List.fold_left_es
            (fun (state, acc) ops ->
              let* state, ops_metadata =
                List.fold_left_es
                  (fun (state, acc) (oph, op, _check_signature) ->
                    let* state, op_metadata =
                      Proto.apply_operation state oph op
                    in
                    return (state, op_metadata :: acc))
                  (state, [])
                  ops
              in
              return (state, List.rev ops_metadata :: acc))
            (state, [])
            operations [@time.duration_lwt operations_application])
       in
       let ops_metadata = List.rev ops_metadata in
       let* validation_result, block_data =
         (Proto.finalize_application
            state
            (Some block_header.shell) [@time.duration_lwt block_finalization])
       in
       return (validation_result, block_data, ops_metadata))

  let may_init_new_protocol chain_id new_protocol
      (block_header : Proto.block_header) block_hash
      (validation_result : Tezos_protocol_environment.validation_result) =
    let open Lwt_result_syntax in
    if Protocol_hash.equal new_protocol Proto.hash then
      return
        ( validation_result,
          Proto.environment_version,
          Proto.expected_context_hash )
    else
      match Registered_protocol.get new_protocol with
      | None ->
          tzfail
            (Unavailable_protocol {block = block_hash; protocol = new_protocol})
      | Some (module NewProto) ->
          let*? () =
            check_proto_environment_version_increasing
              block_hash
              Proto.environment_version
              NewProto.environment_version
          in
          let*! () =
            Validation_events.(emit new_protocol_initialisation new_protocol)
          in
          let* validation_result =
            NewProto.init chain_id validation_result.context block_header.shell
          in
          return
            ( validation_result,
              NewProto.environment_version,
              NewProto.expected_context_hash )

  let apply ?(simulate = false) ?cached_result chain_id ~cache
      ~user_activated_upgrades ~user_activated_protocol_overrides
      ~operation_metadata_size_limit ~max_operations_ttl
      ~(predecessor_block_header : Block_header.t)
      ~predecessor_block_metadata_hash ~predecessor_ops_metadata_hash
      ~predecessor_context ~predecessor_resulting_context_hash
      ~(block_header : Block_header.t) operations =
    let open Lwt_result_syntax in
    let block_hash = Block_header.hash block_header in
    let shell_header_hash = hash_shell_header block_header.shell in
    match cached_result with
    | Some (({result; _} as cached_result), context)
      when Shell_header_hash.equal result.shell_header_hash shell_header_hash ->
        (* In order to implement the preapply's cache mechanism, we
           need to differentiate blocks. Their hash cannot be used as
           a resulting preapply block does not contain correct
           protocol data (e.g. signature). Therefore, their hashes
           won't be the same. However, shell headers will remain the
           same thus we use those to discriminate blocks. *)
        let*! () = Validation_events.(emit using_preapply_result block_hash) in
        let*! context_hash =
          if simulate then
            Lwt.return
            @@ Context_ops.hash
                 ~time:block_header.shell.timestamp
                 ?message:result.validation_store.message
                 context
          else
            Context_ops.commit
              ~time:block_header.shell.timestamp
              ?message:result.validation_store.message
              context
        in
        assert (
          Context_hash.equal
            context_hash
            result.validation_store.resulting_context_hash) ;
        return cached_result
    | Some _ | None ->
        let* () =
          check_block_header
            ~predecessor_block_header
            ~predecessor_resulting_context_hash
            block_hash
            block_header
        in
        let* block_header = parse_block_header block_hash block_header in
        let* () = check_operation_quota block_hash operations in
        let predecessor_hash = Block_header.hash predecessor_block_header in
        let* operations =
          (parse_operations
             block_hash
             operations [@time.duration_lwt operations_parsing])
        in
        let* context =
          prepare_context
            predecessor_block_metadata_hash
            predecessor_ops_metadata_hash
            block_header
            predecessor_context
            predecessor_hash
        in
        let* validation_result, block_metadata, ops_metadata =
          proto_apply_operations
            chain_id
            context
            cache
            predecessor_block_header
            block_header
            block_hash
            operations
        in
        let*! validation_result =
          may_patch_protocol
            ~user_activated_upgrades
            ~user_activated_protocol_overrides
            ~level:block_header.shell.level
            validation_result
        in
        let context = validation_result.context in
        let*! new_protocol = Context_ops.get_protocol context in
        let expected_proto_level =
          if Protocol_hash.equal new_protocol Proto.hash then
            predecessor_block_header.shell.proto_level
          else (predecessor_block_header.shell.proto_level + 1) mod 256
        in
        let* () =
          fail_when
            (block_header.shell.proto_level <> expected_proto_level)
            (invalid_block
               block_hash
               (Invalid_proto_level
                  {
                    found = block_header.shell.proto_level;
                    expected = expected_proto_level;
                  }))
        in
        let* () =
          fail_when
            Fitness.(validation_result.fitness <> block_header.shell.fitness)
            (invalid_block
               block_hash
               (Invalid_fitness
                  {
                    expected = block_header.shell.fitness;
                    found = validation_result.fitness;
                  }))
        in
        let* validation_result, new_protocol_env_version, expected_context_hash
            =
          may_init_new_protocol
            chain_id
            new_protocol
            block_header
            block_hash
            validation_result
        in
        let max_operations_ttl =
          max
            0
            (min (max_operations_ttl + 1) validation_result.max_operations_ttl)
        in
        let validation_result = {validation_result with max_operations_ttl} in
        let* block_metadata, ops_metadata =
          compute_metadata
            ~operation_metadata_size_limit
            new_protocol_env_version
            block_metadata
            ops_metadata
        in
        let (Context {cache; _}) = validation_result.context in
        let context = validation_result.context in
        let*! resulting_context_hash =
          if simulate then
            Lwt.return
            @@ Context_ops.hash
                 ~time:block_header.shell.timestamp
                 ?message:validation_result.message
                 context
          else
            Context_ops.commit
              ~time:block_header.shell.timestamp
              ?message:validation_result.message
              context [@time.duration_lwt context_commitment] [@time.flush]
        in
        let* () =
          let is_context_consistent =
            match expected_context_hash with
            | Predecessor_resulting_context ->
                (* The check that the header's context is the
                   predecessor's resulting context has already been
                   performed in the [check_block_header] call above. *)
                true
            | Resulting_context ->
                Context_hash.equal
                  resulting_context_hash
                  block_header.shell.context
          in
          fail_unless
            is_context_consistent
            (Validation_errors.Inconsistent_hash
               (resulting_context_hash, block_header.shell.context))
        in
        let validation_store =
          {
            resulting_context_hash;
            timestamp = block_header.shell.timestamp;
            message = validation_result.message;
            max_operations_ttl = validation_result.max_operations_ttl;
            last_allowed_fork_level = validation_result.last_allowed_fork_level;
          }
        in
        return
          {
            result =
              {
                shell_header_hash = hash_shell_header block_header.shell;
                validation_store;
                block_metadata;
                ops_metadata;
              };
            cache;
          }

  let recompute_metadata chain_id ~cache
      ~(predecessor_block_header : Block_header.t)
      ~predecessor_block_metadata_hash ~predecessor_ops_metadata_hash
      ~predecessor_context ~(block_header : Block_header.t) operations =
    let open Lwt_result_syntax in
    let block_hash = Block_header.hash block_header in
    (* We assume that the block header and its associated operations
       have already been checked as valid. *)
    let* block_header = parse_block_header block_hash block_header in
    let predecessor_hash = Block_header.hash predecessor_block_header in
    let* context =
      prepare_context
        predecessor_block_metadata_hash
        predecessor_ops_metadata_hash
        block_header
        predecessor_context
        predecessor_hash
    in
    let* operations = parse_operations block_hash operations in
    let* validation_result, block_metadata, ops_metadata =
      proto_apply_operations
        chain_id
        context
        cache
        predecessor_block_header
        block_header
        block_hash
        operations
    in
    let context = validation_result.context in
    let*! new_protocol = Context_ops.get_protocol context in
    let* _validation_result, new_protocol_env_version, _expected_context_hash =
      may_init_new_protocol
        chain_id
        new_protocol
        block_header
        block_hash
        validation_result
    in
    compute_metadata
      ~operation_metadata_size_limit:Unlimited
      new_protocol_env_version
      block_metadata
      ops_metadata

  let preapply_operation pv op =
    let open Lwt_syntax in
    if Operation_hash.Set.mem op.hash pv.live_operations then return Outdated
    else
      let+ r =
        protect (fun () ->
            let operation : Proto.operation =
              {shell = op.raw.shell; protocol_data = op.protocol_data}
            in
            let open Lwt_result_syntax in
            let* validation_state =
              Proto.validate_operation
                ~check_signature:op.check_signature
                pv.validation_state
                op.hash
                operation
            in
            let* application_state, receipt =
              Proto.apply_operation pv.application_state op.hash operation
            in
            return (validation_state, application_state, receipt))
      in
      match r with
      | Ok (validation_state, application_state, receipt) -> (
          let pv =
            {
              validation_state;
              application_state;
              applied = (op, receipt) :: pv.applied;
              live_blocks = pv.live_blocks;
              live_operations =
                Operation_hash.Set.add op.hash pv.live_operations;
            }
          in
          match
            Data_encoding.Binary.(
              of_bytes_exn
                Proto.operation_receipt_encoding_with_legacy_attestation_name
                (to_bytes_exn
                   Proto.operation_receipt_encoding_with_legacy_attestation_name
                   receipt))
          with
          | receipt -> Applied (pv, receipt)
          | exception exn ->
              Refused
                [Validation_errors.Cannot_serialize_operation_metadata; Exn exn]
          )
      | Error trace -> (
          match classify_trace trace with
          | Branch -> Branch_refused trace
          | Permanent -> Refused trace
          | Temporary -> Branch_delayed trace
          | Outdated -> Outdated)

  (** Doesn't depend on heavy [Registered_protocol.T] for testability. *)
  let safe_binary_of_bytes (encoding : 'a Data_encoding.t) (bytes : bytes) :
      'a tzresult =
    let open Result_syntax in
    match Data_encoding.Binary.of_bytes_opt encoding bytes with
    | None -> tzfail Parse_error
    | Some protocol_data -> return protocol_data

  let parse_unsafe (proto : bytes) : Proto.operation_data tzresult =
    safe_binary_of_bytes
      Proto.operation_data_encoding_with_legacy_attestation_name
      proto

  let parse {hash; operation = raw; check_signature} =
    let open Result_syntax in
    let size = Data_encoding.Binary.length Operation.encoding raw in
    if size > Proto.max_operation_data_length then
      tzfail (Oversized_operation {size; max = Proto.max_operation_data_length})
    else
      let* protocol_data = parse_unsafe raw.proto in
      return {hash; check_signature; raw; protocol_data}

  let preapply ~chain_id ~cache ~user_activated_upgrades
      ~user_activated_protocol_overrides ~operation_metadata_size_limit
      ~protocol_data ~live_blocks ~live_operations ~timestamp
      ~predecessor_context ~predecessor_resulting_context_hash
      ~(predecessor_shell_header : Block_header.shell_header) ~predecessor_hash
      ~predecessor_max_operations_ttl ~predecessor_block_metadata_hash
      ~predecessor_ops_metadata_hash ~operations =
    let open Lwt_result_syntax in
    let context = predecessor_context in
    let*! context =
      update_testchain_status context ~predecessor_hash timestamp
    in
    let should_metadata_be_present =
      (* Block and operation metadata hashes may not be set on the
         testchain genesis block and activation block, even when they
         are using environment V1, they contain no operations. *)
      let is_from_genesis = predecessor_shell_header.validation_passes = 0 in
      Protocol.(
        match Proto.environment_version with
        | V0 -> false
        | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 -> true)
      && not is_from_genesis
    in
    let* context =
      match predecessor_block_metadata_hash with
      | None ->
          if should_metadata_be_present then
            tzfail (Missing_block_metadata_hash predecessor_hash)
          else return context
      | Some hash ->
          Lwt_result.ok
          @@ Context_ops.add_predecessor_block_metadata_hash context hash
    in
    let* context =
      match predecessor_ops_metadata_hash with
      | None ->
          if should_metadata_be_present then
            tzfail (Missing_operation_metadata_hashes predecessor_hash)
          else return context
      | Some hash ->
          Lwt_result.ok
          @@ Context_ops.add_predecessor_ops_metadata_hash context hash
    in
    let mode =
      Proto.Construction
        {predecessor_hash; timestamp; block_header_data = protocol_data}
    in
    let* validation_state =
      Proto.begin_validation
        context
        chain_id
        mode
        ~predecessor:predecessor_shell_header
        ~cache
    in
    let* application_state =
      Proto.begin_application
        context
        chain_id
        mode
        ~predecessor:predecessor_shell_header
        ~cache
    in
    let preapply_state =
      {
        validation_state;
        application_state;
        applied = [];
        live_blocks;
        live_operations;
      }
    in
    let apply_operation_with_preapply_result preapp t receipts op =
      let open Preapply_result in
      let*! r = preapply_operation t op in
      match r with
      | Applied (t, receipt) ->
          let applied = (op.hash, op.raw) :: preapp.applied in
          Lwt.return ({preapp with applied}, t, receipt :: receipts)
      | Branch_delayed errors ->
          let branch_delayed =
            Operation_hash.Map.add
              op.hash
              (op.raw, errors)
              preapp.branch_delayed
          in
          Lwt.return ({preapp with branch_delayed}, t, receipts)
      | Branch_refused errors ->
          let branch_refused =
            Operation_hash.Map.add
              op.hash
              (op.raw, errors)
              preapp.branch_refused
          in
          Lwt.return ({preapp with branch_refused}, t, receipts)
      | Refused errors ->
          let refused =
            Operation_hash.Map.add op.hash (op.raw, errors) preapp.refused
          in
          Lwt.return ({preapp with refused}, t, receipts)
      | Outdated -> Lwt.return (preapp, t, receipts)
    in
    let*! ( validation_passes,
            validation_result_list_rev,
            receipts_rev,
            validation_state ) =
      List.fold_left_s
        (fun ( acc_validation_passes,
               acc_validation_result_rev,
               receipts,
               acc_validation_state )
             operations ->
          let*! new_validation_result, new_validation_state, rev_receipts =
            List.fold_left_s
              (fun (acc_validation_result, acc_validation_state, receipts)
                   operation ->
                match parse operation with
                | Error _ ->
                    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1721  *)
                    Lwt.return
                      (acc_validation_result, acc_validation_state, receipts)
                | Ok op ->
                    apply_operation_with_preapply_result
                      acc_validation_result
                      acc_validation_state
                      receipts
                      op)
              (Preapply_result.empty, acc_validation_state, [])
              operations
          in
          (* Applied operations are reverted ; revert to the initial ordering *)
          let new_validation_result =
            {
              new_validation_result with
              applied = List.rev new_validation_result.applied;
            }
          in
          Lwt.return
            ( acc_validation_passes + 1,
              new_validation_result :: acc_validation_result_rev,
              List.rev rev_receipts :: receipts,
              new_validation_state ))
        (0, [], [], preapply_state)
        operations
    in
    let validation_result_list = List.rev validation_result_list_rev in
    let applied_ops_metadata = List.rev receipts_rev in
    let preapply_state = validation_state in
    let operations_hash =
      Operation_list_list_hash.compute
        (List.rev_map
           (fun r ->
             Operation_list_hash.compute
               (List.map fst r.Preapply_result.applied))
           validation_result_list_rev)
    in
    let level = Int32.succ predecessor_shell_header.level in
    let shell_header : Block_header.shell_header =
      {
        level;
        proto_level = predecessor_shell_header.proto_level;
        predecessor = predecessor_hash;
        timestamp;
        validation_passes;
        operations_hash;
        context = Context_hash.zero (* place holder *);
        fitness = [];
      }
    in
    let* () = Proto.finalize_validation preapply_state.validation_state in
    let* validation_result, block_header_metadata =
      Proto.finalize_application
        preapply_state.application_state
        (Some shell_header)
    in
    let*! validation_result =
      may_patch_protocol
        ~user_activated_upgrades
        ~user_activated_protocol_overrides
        ~level
        validation_result
    in
    let*! protocol =
      Tezos_protocol_environment.Context.get_protocol validation_result.context
    in
    let proto_level =
      if Protocol_hash.equal protocol Proto.hash then
        predecessor_shell_header.proto_level
      else (predecessor_shell_header.proto_level + 1) mod 256
    in
    let shell_header : Block_header.shell_header =
      {shell_header with proto_level; fitness = validation_result.fitness}
    in
    let* ( validation_result,
           cache,
           new_protocol_env_version,
           proto_expected_context_hash ) =
      if Protocol_hash.equal protocol Proto.hash then
        let (Tezos_protocol_environment.Context.Context {cache; _}) =
          validation_result.context
        in
        return
          ( validation_result,
            cache,
            Proto.environment_version,
            Proto.expected_context_hash )
      else
        match Registered_protocol.get protocol with
        | None ->
            tzfail
              (Block_validator_errors.Unavailable_protocol
                 {block = predecessor_hash; protocol})
        | Some (module NewProto) ->
            let*? () =
              check_proto_environment_version_increasing
                Block_hash.zero
                Proto.environment_version
                NewProto.environment_version
            in
            let* validation_result =
              NewProto.init chain_id validation_result.context shell_header
            in
            let (Tezos_protocol_environment.Context.Context {cache; _}) =
              validation_result.context
            in
            let*! () =
              Validation_events.(emit new_protocol_initialisation NewProto.hash)
            in
            return
              ( validation_result,
                cache,
                NewProto.environment_version,
                NewProto.expected_context_hash )
    in
    let context = validation_result.context in
    let resulting_context_hash =
      Context_ops.hash
        ?message:validation_result.message
        ~time:timestamp
        context
    in
    let header_context_hash =
      match proto_expected_context_hash with
      | Resulting_context -> resulting_context_hash
      | Predecessor_resulting_context -> predecessor_resulting_context_hash
    in
    let preapply_result =
      ({shell_header with context = header_context_hash}, validation_result_list)
    in
    let* block_metadata, ops_metadata =
      compute_metadata
        ~operation_metadata_size_limit
        new_protocol_env_version
        block_header_metadata
        applied_ops_metadata
    in
    let max_operations_ttl =
      max
        0
        (min
           (predecessor_max_operations_ttl + 1)
           validation_result.max_operations_ttl)
    in
    let result =
      let validation_store =
        {
          resulting_context_hash;
          timestamp;
          message = validation_result.message;
          max_operations_ttl;
          last_allowed_fork_level = validation_result.last_allowed_fork_level;
        }
      in
      let result =
        {
          shell_header_hash = hash_shell_header (fst preapply_result);
          validation_store;
          block_metadata;
          ops_metadata;
        }
      in
      {result; cache}
    in
    return (preapply_result, (result, context))

  let precheck block_hash chain_id ~(predecessor_block_header : Block_header.t)
      ~predecessor_block_hash ~predecessor_context
      ~predecessor_resulting_context_hash ~cache
      ~(block_header : Block_header.t) operations =
    let open Lwt_result_syntax in
    let* () =
      check_block_header
        ~predecessor_block_header
        ~predecessor_resulting_context_hash
        block_hash
        block_header
    in
    let* block_header = parse_block_header block_hash block_header in
    let* () = check_operation_quota block_hash operations in
    let*! context =
      update_testchain_status
        predecessor_context
        ~predecessor_hash:predecessor_block_hash
        block_header.shell.timestamp
    in
    let* operations = parse_operations block_hash operations in
    let* state =
      Proto.begin_validation
        context
        chain_id
        (Application block_header)
        ~predecessor:predecessor_block_header.shell
        ~cache
    in
    let* state =
      List.fold_left_es
        (fun state ops ->
          List.fold_left_es
            (fun state (oph, op, check_signature) ->
              let* state =
                Proto.validate_operation ~check_signature state oph op
              in
              return state)
            state
            ops)
        state
        operations
    in
    let* () = Proto.finalize_validation state in
    return_unit

  let precheck chain_id ~(predecessor_block_header : Block_header.t)
      ~predecessor_block_hash ~predecessor_context
      ~predecessor_resulting_context_hash ~cache
      ~(block_header : Block_header.t) operations =
    let block_hash = Block_header.hash block_header in
    trace (invalid_block block_hash Economic_protocol_error)
    @@ precheck
         block_hash
         chain_id
         ~predecessor_block_header
         ~predecessor_block_hash
         ~predecessor_context
         ~predecessor_resulting_context_hash
         ~cache
         ~block_header
         operations
end

let assert_no_duplicate_operations block_hash live_operations operations =
  let open Result_syntax in
  let exception Duplicate of block_error in
  try
    return
      (List.fold_left
         (List.fold_left
            (fun live_operations {hash; operation = _; check_signature = _} ->
              if Operation_hash.Set.mem hash live_operations then
                raise (Duplicate (Replayed_operation hash))
              else Operation_hash.Set.add hash live_operations))
         live_operations
         operations)
  with Duplicate err -> tzfail (invalid_block block_hash err)

let assert_operation_liveness block_hash live_blocks operations =
  let open Result_syntax in
  let exception Outdated of block_error in
  try
    return
      (List.iter
         (List.iter (fun {hash; operation; check_signature = _} ->
              if
                not
                  (Block_hash.Set.mem
                     operation.Operation.shell.branch
                     live_blocks)
              then
                let error =
                  Outdated_operation
                    {
                      operation = hash;
                      originating_block = operation.shell.branch;
                    }
                in
                raise (Outdated error)))
         operations)
  with Outdated err -> tzfail (invalid_block block_hash err)

(* Maybe this function should be moved somewhere else since it used
   once by [Block_validator_process] *)
let check_liveness ~live_blocks ~live_operations block_hash operations =
  let open Result_syntax in
  let* (_ : Operation_hash.Set.t) =
    assert_no_duplicate_operations block_hash live_operations operations
  in
  assert_operation_liveness block_hash live_blocks operations

type apply_environment = {
  max_operations_ttl : int;
  chain_id : Chain_id.t;
  predecessor_block_header : Block_header.t;
  predecessor_context : Tezos_protocol_environment.Context.t;
  predecessor_resulting_context_hash : Context_hash.t;
  predecessor_block_metadata_hash : Block_metadata_hash.t option;
  predecessor_ops_metadata_hash : Operation_metadata_list_list_hash.t option;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
}

let recompute_metadata chain_id ~predecessor_block_header ~predecessor_context
    ~predecessor_block_metadata_hash ~predecessor_ops_metadata_hash ~cache
    block_hash block_header operations =
  let open Lwt_result_syntax in
  let*! pred_protocol_hash = Context_ops.get_protocol predecessor_context in
  let* (module Proto) =
    match Registered_protocol.get pred_protocol_hash with
    | None ->
        tzfail
          (Unavailable_protocol
             {block = block_hash; protocol = pred_protocol_hash})
    | Some p -> return p
  in
  let module Block_validation = Make (Proto) in
  Block_validation.recompute_metadata
    chain_id
    ~predecessor_block_header
    ~predecessor_block_metadata_hash
    ~predecessor_ops_metadata_hash
    ~predecessor_context
    ~cache
    ~block_header
    operations

let recompute_metadata ~chain_id ~predecessor_block_header ~predecessor_context
    ~predecessor_block_metadata_hash ~predecessor_ops_metadata_hash
    ~block_header ~operations ~cache =
  let open Lwt_result_syntax in
  let block_hash = Block_header.hash block_header in
  let*! r =
    recompute_metadata
      chain_id
      ~predecessor_block_header
      ~predecessor_context
      ~predecessor_block_metadata_hash
      ~predecessor_ops_metadata_hash
      ~cache
      block_hash
      block_header
      operations
  in
  match r with
  | Error (Exn (Unix.Unix_error (errno, fn, msg)) :: _) ->
      tzfail (System_error {errno = Unix.error_message errno; fn; msg})
  | (Ok _ | Error _) as res -> Lwt.return res

let precheck ~chain_id ~predecessor_block_header ~predecessor_block_hash
    ~predecessor_context ~predecessor_resulting_context_hash ~cache block_header
    operations =
  let open Lwt_result_syntax in
  let block_hash = Block_header.hash block_header in
  let*! pred_protocol_hash = Context_ops.get_protocol predecessor_context in
  let* (module Proto) =
    match Registered_protocol.get pred_protocol_hash with
    | None ->
        tzfail
          (Unavailable_protocol
             {block = block_hash; protocol = pred_protocol_hash})
    | Some p -> return p
  in
  let module Block_validation = Make (Proto) in
  Block_validation.precheck
    chain_id
    ~predecessor_block_header
    ~predecessor_block_hash
    ~predecessor_context
    ~predecessor_resulting_context_hash
    ~cache
    ~block_header
    operations

let apply ?simulate ?cached_result ?(should_precheck = true)
    {
      chain_id;
      user_activated_upgrades;
      user_activated_protocol_overrides;
      operation_metadata_size_limit;
      max_operations_ttl;
      predecessor_block_header;
      predecessor_block_metadata_hash;
      predecessor_ops_metadata_hash;
      predecessor_context;
      predecessor_resulting_context_hash;
    } ~cache block_hash block_header operations =
  let open Lwt_result_syntax in
  let*! pred_protocol_hash = Context_ops.get_protocol predecessor_context in
  let* (module Proto) =
    match Registered_protocol.get pred_protocol_hash with
    | None ->
        tzfail
          (Unavailable_protocol
             {block = block_hash; protocol = pred_protocol_hash})
    | Some p -> return p
  in
  let* () =
    if should_precheck && Proto.(compare environment_version V7 >= 0) then
      precheck
        ~chain_id
        ~predecessor_block_header
        ~predecessor_block_hash:block_header.Block_header.shell.predecessor
        ~predecessor_context
        ~predecessor_resulting_context_hash
        ~cache
        block_header
        operations
    else return_unit
  in
  let module Block_validation = Make (Proto) in
  Block_validation.apply
    ?simulate
    ?cached_result
    chain_id
    ~user_activated_upgrades
    ~user_activated_protocol_overrides
    ~operation_metadata_size_limit
    ~max_operations_ttl
    ~predecessor_block_header
    ~predecessor_block_metadata_hash
    ~predecessor_ops_metadata_hash
    ~predecessor_context
    ~predecessor_resulting_context_hash
    ~cache
    ~block_header
    operations

let apply ?simulate ?cached_result ?should_precheck apply_environment ~cache
    block_header operations =
  let open Lwt_result_syntax in
  let block_hash = Block_header.hash block_header in
  let*! r =
    (* The cache might be inconsistent with the context. By forcing
       the reloading of the cache, we restore the consistency. *)
    let*! r =
      apply
        ?simulate
        ?cached_result
        ?should_precheck
        apply_environment
        ~cache
        block_hash
        block_header
        operations
    in
    match r with
    | Error (Validation_errors.Inconsistent_hash _ :: _) ->
        (* The shell makes an assumption over the protocol concerning
           the cache which may be broken. In that case, the application
           fails with an [Inconsistency_hash] error. To make the node
           resilient to such problem, when such an error occurs, we retry
           the application using a fresh cache. *)
        let*! () = Event.(emit inherited_inconsistent_cache) block_hash in
        apply
          ?cached_result
          ?should_precheck
          apply_environment
          ~cache:`Force_load
          block_hash
          block_header
          operations
    | (Ok _ | Error _) as res -> Lwt.return res
  in
  match r with
  | Error (Exn (Unix.Unix_error (errno, fn, msg)) :: _) ->
      tzfail (System_error {errno = Unix.error_message errno; fn; msg})
  | (Ok _ | Error _) as res -> Lwt.return res

let preapply ~chain_id ~user_activated_upgrades
    ~user_activated_protocol_overrides ~operation_metadata_size_limit ~timestamp
    ~protocol_data ~live_blocks ~live_operations ~predecessor_context
    ~predecessor_resulting_context_hash ~predecessor_shell_header
    ~predecessor_hash ~predecessor_max_operations_ttl
    ~predecessor_block_metadata_hash ~predecessor_ops_metadata_hash operations =
  let open Lwt_result_syntax in
  let*! protocol = Context_ops.get_protocol predecessor_context in
  let* (module Proto) =
    match Registered_protocol.get protocol with
    | None ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1718 *)
        (* This should not happen: it should be handled in the validator. *)
        failwith
          "Prevalidation: missing protocol '%a' for the current block."
          Protocol_hash.pp_short
          protocol
    | Some protocol -> return protocol
  in
  (* The cache might be inconsistent with the context. By forcing the
     reloading of the cache, we restore the consistency. *)
  let module Block_validation = Make (Proto) in
  let* protocol_data =
    match
      Data_encoding.Binary.of_bytes_opt
        Proto.block_header_data_encoding
        protocol_data
    with
    | None -> failwith "Invalid block header"
    | Some protocol_data -> return protocol_data
  in
  Block_validation.preapply
    ~chain_id
    ~cache:`Force_load
    ~user_activated_upgrades
    ~user_activated_protocol_overrides
    ~operation_metadata_size_limit
    ~protocol_data
    ~live_blocks
    ~live_operations
    ~timestamp
    ~predecessor_context
    ~predecessor_resulting_context_hash
    ~predecessor_shell_header
    ~predecessor_hash
    ~predecessor_max_operations_ttl
    ~predecessor_block_metadata_hash
    ~predecessor_ops_metadata_hash
    ~operations

let preapply ~chain_id ~user_activated_upgrades
    ~user_activated_protocol_overrides ~operation_metadata_size_limit ~timestamp
    ~protocol_data ~live_blocks ~live_operations ~predecessor_context
    ~predecessor_resulting_context_hash ~predecessor_shell_header
    ~predecessor_hash ~predecessor_max_operations_ttl
    ~predecessor_block_metadata_hash ~predecessor_ops_metadata_hash operations =
  let open Lwt_result_syntax in
  let*! r =
    preapply
      ~chain_id
      ~user_activated_upgrades
      ~user_activated_protocol_overrides
      ~operation_metadata_size_limit
      ~timestamp
      ~protocol_data
      ~live_blocks
      ~live_operations
      ~predecessor_context
      ~predecessor_resulting_context_hash
      ~predecessor_shell_header
      ~predecessor_hash
      ~predecessor_max_operations_ttl
      ~predecessor_block_metadata_hash
      ~predecessor_ops_metadata_hash
      operations
  in
  match r with
  | Error (Exn (Unix.Unix_error (errno, fn, msg)) :: _) ->
      tzfail (System_error {errno = Unix.error_message errno; fn; msg})
  | (Ok _ | Error _) as res -> Lwt.return res
