(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

module Proof = Tezos_context_sigs.Context.Proof_types

let read_partial_context =
  let open Lwt_syntax in
  let init = Proof.Dir String.Map.empty in
  fun context path depth ->
    if depth = 0 then Lwt.return Proof.Cut
    else
      (* According to the documentation of Context.fold,
         "[f] is never called with an empty key for values; i.e.,
           folding over a value is a no-op".
         Therefore, we first need to check that whether its a value.
      *)
      let* o = Context_ops.find context path in
      match o with
      | Some v -> Lwt.return (Proof.Key v)
      | None ->
          (* try to read as directory *)
          Context_ops.fold_value
            ~depth:(`Le depth)
            context
            path
            ~order:`Sorted
            ~init
            ~f:(fun k lazy_value acc ->
              let open Block_services in
              if List.compare_length_with k depth >= 0 then
                (* only [=] case is possible because [~depth] is [(`Le depth)] *)
                Lwt.return (raw_context_insert (k, Cut) acc)
              else
                let+ o = lazy_value () in
                match o with
                | None -> acc
                | Some v -> raw_context_insert (k, Key v) acc)

let build_raw_header_rpc_directory (module Proto : Block_services.PROTO) =
  let open Lwt_result_syntax in
  let dir :
      (Store.chain_store * Block_hash.t * Block_header.t) Tezos_rpc.Directory.t
      ref =
    ref Tezos_rpc.Directory.empty
  in
  let register0 s f =
    dir :=
      Tezos_rpc.Directory.register
        !dir
        (Tezos_rpc.Service.subst0 s)
        (fun block p q -> f block p q)
  in
  let module Block_services = Block_services.Make (Proto) (Proto) in
  let module S = Block_services.S in
  register0 S.hash (fun (_, hash, _) () () -> return hash) ;
  (* block header *)
  register0 S.header (fun (chain_store, hash, header) () () ->
      let protocol_data =
        Data_encoding.Binary.of_bytes_exn
          Proto.block_header_data_encoding
          header.protocol_data
      in
      return
        {
          Block_services.hash;
          chain_id = Store.Chain.chain_id chain_store;
          shell = header.shell;
          protocol_data;
        }) ;
  register0 S.raw_header (fun (_, _, header) () () ->
      return (Data_encoding.Binary.to_bytes_exn Block_header.encoding header)) ;
  register0 S.Header.shell_header (fun (_, _, header) () () ->
      return header.shell) ;
  register0 S.Header.protocol_data (fun (_, _, header) () () ->
      return
        (Data_encoding.Binary.of_bytes_exn
           Proto.block_header_data_encoding
           header.protocol_data)) ;
  register0 S.Header.raw_protocol_data (fun (_, _, header) () () ->
      return header.protocol_data) ;
  register0 S.resulting_context_hash (fun (chain_store, hash, _) () () ->
      let*! block_opt = Store.Block.read_block_opt chain_store hash in
      let block = WithExceptions.Option.to_exn ~none:Not_found block_opt in
      Store.Block.resulting_context_hash chain_store block) ;
  (* helpers *)
  register0 S.Helpers.Forge.block_header (fun _block () header ->
      return (Data_encoding.Binary.to_bytes_exn Block_header.encoding header)) ;
  (* protocols *)
  register0 S.protocols (fun (chain_store, _hash, header) () () ->
      let open Lwt_result_syntax in
      let*! next_proto =
        Store.Chain.find_protocol
          chain_store
          ~protocol_level:header.shell.proto_level
      in
      let next_protocol_hash =
        WithExceptions.Option.to_exn ~none:Not_found next_proto
      in
      let*! o =
        Store.Block.read_block_opt chain_store header.shell.predecessor
      in
      match o with
      | None ->
          return
            {
              Tezos_shell_services.Block_services.current_protocol =
                next_protocol_hash;
              next_protocol = next_protocol_hash;
            }
      | Some pred_block ->
          let pred_header = Store.Block.header pred_block in
          let*! current_protocol =
            Store.Chain.find_protocol
              chain_store
              ~protocol_level:pred_header.shell.proto_level
          in
          let protocol_hash =
            WithExceptions.Option.to_exn ~none:Not_found current_protocol
          in
          return
            {
              Tezos_shell_services.Block_services.current_protocol =
                protocol_hash;
              next_protocol = next_protocol_hash;
            }) ;
  !dir

(* This convertor aims to merge the behavior of the force_metadata
   query string into the metadata one. We must remove it as soon as
   the force_metadata query string is removed. *)
let with_metadata ~force_metadata ~metadata =
  match (force_metadata, metadata) with
  | true, _ | _, Some `Always -> Some `Always
  | _, Some `Never -> Some `Never
  | _, None -> None

let build_raw_rpc_directory (module Proto : Block_services.PROTO)
    (module Next_proto : Registered_protocol.T) =
  let open Lwt_result_syntax in
  let dir : (Store.chain_store * Store.Block.block) Tezos_rpc.Directory.t ref =
    ref Tezos_rpc.Directory.empty
  in
  let merge d = dir := Tezos_rpc.Directory.merge d !dir in
  let register0 s f =
    dir :=
      Tezos_rpc.Directory.register
        !dir
        (Tezos_rpc.Service.subst0 s)
        (fun block p q -> f block p q)
  in
  let register1 s f =
    dir :=
      Tezos_rpc.Directory.register
        !dir
        (Tezos_rpc.Service.subst1 s)
        (fun (block, a) p q -> f block a p q)
  in
  let register2 s f =
    dir :=
      Tezos_rpc.Directory.register
        !dir
        (Tezos_rpc.Service.subst2 s)
        (fun ((block, a), b) p q -> f block a b p q)
  in
  let module Block_services = Block_services.Make (Proto) (Next_proto) in
  let module S = Block_services.S in
  register0 S.live_blocks (fun (chain_store, block) () () ->
      let* live_blocks, _ =
        Store.Chain.compute_live_blocks chain_store ~block
      in
      return live_blocks) ;
  (* block metadata *)
  let block_metadata chain_store block =
    let* metadata = Store.Block.get_block_metadata chain_store block in
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.block_header_metadata_encoding
        (Store.Block.block_metadata metadata)
    in
    let* test_chain_status, _ =
      Store.Block.testchain_status chain_store block
    in
    let max_operations_ttl = Store.Block.max_operations_ttl metadata in
    return
      {
        Block_services.protocol_data;
        test_chain_status;
        max_operations_ttl;
        max_operation_data_length = Next_proto.max_operation_data_length;
        max_block_header_length = Next_proto.max_block_length;
        operation_list_quota =
          List.map
            (fun {Tezos_protocol_environment.max_size; max_op} ->
              {Tezos_shell_services.Block_services.max_size; max_op})
            Next_proto.validation_passes;
      }
  in
  register0 S.metadata (fun (chain_store, block) () () ->
      block_metadata chain_store block) ;
  let fail_opt = function None -> Lwt.fail Not_found | Some v -> return v in
  register0 S.metadata_hash (fun (_, block) () () ->
      fail_opt (Store.Block.block_metadata_hash block)) ;
  (* operations *)
  let convert_with_metadata chain_id (op : Operation.t) metadata :
      Block_services.operation =
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.operation_data_encoding_with_legacy_attestation_name
        op.proto
    in
    let receipt =
      match metadata with
      | Block_validation.Metadata bytes ->
          Block_services.Receipt
            (Data_encoding.Binary.of_bytes_exn
               Proto.operation_receipt_encoding_with_legacy_attestation_name
               bytes)
      | Too_large_metadata -> Too_large
    in
    {
      Block_services.chain_id;
      hash = Operation.hash op;
      shell = op.shell;
      protocol_data;
      receipt;
    }
  in
  let convert_without_metadata chain_id (op : Operation.t) =
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.operation_data_encoding_with_legacy_attestation_name
        op.proto
    in
    {
      Block_services.chain_id;
      hash = Operation.hash op;
      shell = op.shell;
      protocol_data;
      receipt = Empty;
    }
  in
  let operations_without_metadata chain_store block =
    let chain_id = Store.Chain.chain_id chain_store in
    let ops = Store.Block.operations block in
    return (List.map (List.map (convert_without_metadata chain_id)) ops)
  in
  let operations chain_store block =
    let chain_id = Store.Chain.chain_id chain_store in
    let ops = Store.Block.operations block in
    let*! o = Store.Block.get_block_metadata_opt chain_store block in
    match o with
    | None ->
        return (List.map (List.map (convert_without_metadata chain_id)) ops)
    | Some metadata ->
        Lwt.catch
          (fun () ->
            let ops_metadata = Store.Block.operations_metadata metadata in
            List.map2_e
              ~when_different_lengths:()
              (List.map2
                 ~when_different_lengths:()
                 (convert_with_metadata chain_id))
              ops
              ops_metadata
            |> function
            | Ok v -> return v
            | Error () -> raise Not_found)
          (fun _ ->
            return (List.map (List.map (convert_without_metadata chain_id)) ops))
  in
  let force_operation_metadata chain_id chain_store block =
    let block_header = Store.Block.header block in
    let operations = Store.Block.operations block in
    let* predecessor_block =
      Store.Block.read_block chain_store (Store.Block.predecessor block)
    in
    let predecessor_header = Store.Block.header predecessor_block in
    let* context = Store.Block.context chain_store predecessor_block in
    let* predecessor_resulting_context =
      Store.Block.resulting_context_hash chain_store predecessor_block
    in
    let* predecessor_context =
      let*! ctxt =
        Context_ops.checkout
          (Context_ops.index context)
          predecessor_resulting_context
      in
      match ctxt with Some c -> return c | None -> fail_with_exn Not_found
    in
    let predecessor_block_metadata_hash =
      Store.Block.block_metadata_hash predecessor_block
    in
    let predecessor_ops_metadata_hash =
      Store.Block.all_operations_metadata_hash predecessor_block
    in
    let operations =
      List.map (List.map Block_validation.mk_operation) operations
    in
    let* _block_metadata, ops_metadata =
      Block_validation.recompute_metadata
        ~chain_id
        ~predecessor_block_header:predecessor_header
        ~predecessor_context
        ~predecessor_block_metadata_hash
        ~predecessor_ops_metadata_hash
        ~block_header
        ~operations
        ~cache:`Lazy
    in
    let ops_metadata =
      match ops_metadata with
      | Block_validation.No_metadata_hash ops_metadata -> ops_metadata
      | Block_validation.Metadata_hash ops_metadata ->
          List.map (List.map fst) ops_metadata
    in
    return ops_metadata
  in
  (*****************************************************************)
  register0 S.Operations.operations (fun (chain_store, block) q () ->
      let with_metadata =
        with_metadata ~force_metadata:q#force_metadata ~metadata:q#metadata
      in
      match with_metadata with
      | Some `Always -> (
          let chain_id = Store.Chain.chain_id chain_store in
          let ops = Store.Block.operations block in
          let* metadata = Store.Block.get_block_metadata chain_store block in
          let ops_metadata = metadata.operations_metadata in
          let* ops_metadata =
            (* Iter through the operations metadata to check if some are
               considered as too large. *)
            if
              List.exists
                (fun v ->
                  List.exists
                    (fun v -> v = Block_validation.Too_large_metadata)
                    v)
                ops_metadata
            then
              (* The metadatas are stored but contains some too large
                 metadata, we need te recompute them *)
              force_operation_metadata chain_id chain_store block
            else return ops_metadata
          in
          List.map2_e
            ~when_different_lengths:()
            (List.map2
               ~when_different_lengths:()
               (convert_with_metadata chain_id))
            ops
            ops_metadata
          |> function
          | Ok v -> return v
          | Error () -> fail_with_exn Not_found)
      | Some `Never -> operations_without_metadata chain_store block
      | None -> operations chain_store block) ;
  register1 S.Operations.operations_in_pass (fun (chain_store, block) i q () ->
      let chain_id = Store.Chain.chain_id chain_store in
      Lwt.catch
        (fun () ->
          let with_metadata =
            with_metadata ~force_metadata:q#force_metadata ~metadata:q#metadata
          in
          match with_metadata with
          | Some `Always -> (
              let*! o = Store.Block.get_block_metadata_opt chain_store block in
              let ops = fst @@ Store.Block.operations_path block i in
              match o with
              | None ->
                  return (List.map (convert_without_metadata chain_id) ops)
              | Some metadata -> (
                  let opss_metadata =
                    Store.Block.operations_metadata metadata
                  in
                  let ops_metadata =
                    List.nth opss_metadata i
                    |> WithExceptions.Option.to_exn ~none:Not_found
                  in
                  let* ops_metadata =
                    (* Iter through the operations metadata of the
                       requested pass to check if some are considered as
                       too large. *)
                    if
                      List.exists
                        (fun v -> v = Block_validation.Too_large_metadata)
                        ops_metadata
                    then
                      let* opss_metadata =
                        force_operation_metadata chain_id chain_store block
                      in
                      let ops_metadata =
                        List.nth_opt opss_metadata i
                        |> WithExceptions.Option.to_exn ~none:Not_found
                      in
                      return ops_metadata
                    else return ops_metadata
                  in
                  List.map2
                    ~when_different_lengths:()
                    (convert_with_metadata chain_id)
                    ops
                    ops_metadata
                  |> function
                  | Ok x -> return x
                  | _ -> fail_with_exn Not_found))
          | Some `Never ->
              let* ops = operations_without_metadata chain_store block in
              return
                (List.nth ops i |> WithExceptions.Option.to_exn ~none:Not_found)
          | None ->
              let* ops = operations chain_store block in
              return
                (List.nth ops i |> WithExceptions.Option.to_exn ~none:Not_found))
        (fun _ -> fail_with_exn Not_found)) ;
  register2 S.Operations.operation (fun (chain_store, block) i j q () ->
      let chain_id = Store.Chain.chain_id chain_store in
      Lwt.catch
        (fun () ->
          let ops = fst @@ Store.Block.operations_path block i in
          let op =
            List.nth ops j |> WithExceptions.Option.to_exn ~none:Not_found
          in
          let with_metadata =
            with_metadata ~force_metadata:q#force_metadata ~metadata:q#metadata
          in
          match with_metadata with
          | Some `Always -> (
              let*! o = Store.Block.get_block_metadata_opt chain_store block in
              match o with
              | None -> return (convert_without_metadata chain_id op)
              | Some metadata -> (
                  let opss_metadata =
                    Store.Block.operations_metadata metadata
                  in
                  let ops_metadata =
                    List.nth opss_metadata i
                    |> WithExceptions.Option.to_exn ~none:Not_found
                  in
                  let op_metadata =
                    List.nth ops_metadata j
                    |> WithExceptions.Option.to_exn ~none:Not_found
                  in
                  match op_metadata with
                  | Block_validation.Too_large_metadata ->
                      let* opss_metadata =
                        force_operation_metadata chain_id chain_store block
                      in
                      let ops_metadata =
                        List.nth_opt opss_metadata i
                        |> WithExceptions.Option.to_exn ~none:Not_found
                      in
                      let op_metadata =
                        List.nth ops_metadata j
                        |> WithExceptions.Option.to_exn ~none:Not_found
                      in
                      return ((convert_with_metadata chain_id) op op_metadata)
                  | Metadata _ ->
                      return (convert_with_metadata chain_id op op_metadata)))
          | Some `Never ->
              let* opss = operations_without_metadata chain_store block in
              let ops =
                List.nth opss i |> WithExceptions.Option.to_exn ~none:Not_found
              in
              let op =
                List.nth ops j |> WithExceptions.Option.to_exn ~none:Not_found
              in
              return op
          | None ->
              let* opss = operations chain_store block in
              let ops =
                List.nth opss i |> WithExceptions.Option.to_exn ~none:Not_found
              in
              let op =
                List.nth ops j |> WithExceptions.Option.to_exn ~none:Not_found
              in
              return op)
        (fun _ -> fail_with_exn Not_found)) ;
  (* operation_hashes *)
  register0 S.Operation_hashes.operation_hashes (fun (_, block) () () ->
      return (Store.Block.all_operation_hashes block)) ;
  register1
    S.Operation_hashes.operation_hashes_in_pass
    (fun (_, block) i () () ->
      return (Store.Block.operations_hashes_path block i |> fst)) ;
  register2 S.Operation_hashes.operation_hash (fun (_, block) i j () () ->
      Lwt.catch
        (fun () ->
          let ops, _ = Store.Block.operations_hashes_path block i in
          return (List.nth ops j |> WithExceptions.Option.to_exn ~none:Not_found))
        (fun _ -> Lwt.fail Not_found)) ;
  (* operation_metadata_hashes *)
  register0 S.Operation_metadata_hashes.root (fun (_, block) () () ->
      fail_opt (Store.Block.all_operations_metadata_hash block)) ;
  register0
    S.Operation_metadata_hashes.operation_metadata_hashes
    (fun (_, block) () () ->
      fail_opt (Store.Block.operations_metadata_hashes block)) ;
  register1
    S.Operation_metadata_hashes.operation_metadata_hashes_in_pass
    (fun (_, block) i () () ->
      let* ops_metadata_hashes =
        fail_opt (Store.Block.operations_metadata_hashes block)
      in
      fail_opt (List.nth_opt ops_metadata_hashes i)) ;
  register2
    S.Operation_metadata_hashes.operation_metadata_hash
    (fun (_, block) i j () () ->
      Lwt.catch
        (fun () ->
          let* hashes =
            fail_opt (Store.Block.operations_metadata_hashes_path block i)
          in
          return
            (List.nth hashes j |> WithExceptions.Option.to_exn ~none:Not_found))
        (fun _ -> Lwt.fail Not_found)) ;
  (* context *)
  register1 S.Context.read (fun (chain_store, block) path q () ->
      let depth = Option.value ~default:max_int q#depth in
      (* [depth] is defined as a [uint] not an [int] *)
      assert (depth >= 0) ;
      let*! _, savepoint_level = Store.Chain.savepoint chain_store in
      if Store.Block.level block >= savepoint_level then
        let* context = Store.Block.context chain_store block in
        let*! mem = Context_ops.mem context path in
        let*! dir_mem = Context_ops.mem_tree context path in
        if not (mem || dir_mem) then Lwt.fail Not_found
        else
          let*! v = read_partial_context context path depth in
          Lwt.return_ok v
      else Lwt.fail Not_found) ;
  register1 S.Context.merkle_tree (fun (chain_store, block) path query () ->
      let*! _, savepoint_level = Store.Chain.savepoint chain_store in
      if Store.Block.level block >= savepoint_level then
        let*! o = Store.Block.context_opt chain_store block in
        match o with
        | None -> return None
        | Some context ->
            let holey = Option.value ~default:false query#holey in
            let leaf_kind =
              let open Proof in
              if holey then Hole else Raw_context
            in
            let*! v = Context_ops.merkle_tree context leaf_kind path in
            return_some v
      else Lwt.fail Not_found) ;
  register1 S.Context.merkle_tree_v2 (fun (chain_store, block) path query () ->
      let*! o = Store.Block.context_opt chain_store block in
      match o with
      | None -> return_none
      | Some context ->
          let holey = Option.value ~default:false query#holey in
          let leaf_kind =
            let open Proof in
            if holey then Hole else Raw_context
          in
          let*! v = Context_ops.merkle_tree_v2 context leaf_kind path in
          return_some v) ;
  (* info *)
  register0 S.info (fun (chain_store, block) q () ->
      let chain_id = Store.Chain.chain_id chain_store in
      let hash = Store.Block.hash block in
      let header = Store.Block.header block in
      let shell = header.shell in
      let protocol_data =
        Data_encoding.Binary.of_bytes_exn
          Proto.block_header_data_encoding
          header.protocol_data
      in
      let* metadata =
        let*! metadata = block_metadata chain_store block in
        return (Option.of_result metadata)
      in
      let* operations =
        let with_metadata =
          with_metadata ~force_metadata:q#force_metadata ~metadata:q#metadata
        in
        match with_metadata with
        | Some `Always -> (
            let ops = Store.Block.operations block in
            let* metadata = Store.Block.get_block_metadata chain_store block in
            let ops_metadata = metadata.operations_metadata in
            let* ops_metadata =
              (* Iter through the operations metadata to check if some are
                 considered as too large. *)
              if
                List.exists
                  (fun v ->
                    List.exists
                      (fun v -> v = Block_validation.Too_large_metadata)
                      v)
                  ops_metadata
              then
                (* The metadatas are stored but contains some too large
                   metadata, we need te recompute them *)
                force_operation_metadata chain_id chain_store block
              else return ops_metadata
            in
            List.map2_e
              ~when_different_lengths:()
              (List.map2
                 ~when_different_lengths:()
                 (convert_with_metadata chain_id))
              ops
              ops_metadata
            |> function
            | Ok v -> return v
            | Error () -> fail_with_exn Not_found)
        | Some `Never -> operations_without_metadata chain_store block
        | None -> operations chain_store block
      in
      return
        {
          Block_services.hash;
          chain_id;
          header = {shell; protocol_data};
          metadata;
          operations;
        }) ;
  (* helpers *)
  register0 S.Helpers.Preapply.block (fun (chain_store, block) q p ->
      let timestamp =
        match q#timestamp with
        | None -> Time.System.to_protocol (Time.System.now ())
        | Some time -> time
      in
      let protocol_data =
        Data_encoding.Binary.to_bytes_exn
          Next_proto.block_header_data_encoding
          p.protocol_data
      in
      let operations =
        List.map
          (fun operations ->
            let operations =
              List.map
                (fun op ->
                  let proto =
                    Data_encoding.Binary.to_bytes_exn
                      Next_proto
                      .operation_data_encoding_with_legacy_attestation_name
                      op.Next_proto.protocol_data
                  in
                  (op, {Operation.shell = op.shell; proto}))
                operations
            in
            let operations =
              if q#sort_operations then
                List.sort
                  (fun (op, ops) (op', ops') ->
                    let oph, oph' = (Operation.hash ops, Operation.hash ops') in
                    Next_proto.compare_operations (oph, op) (oph', op'))
                  operations
              else operations
            in
            List.map snd operations)
          p.operations
      in
      let* bv =
        try return (Block_validator.running_worker ())
        with _ -> failwith "Block validator is not running"
      in
      Block_validator.preapply
        bv
        chain_store
        ~predecessor:block
        ~timestamp
        ~protocol_data
        operations) ;
  register0
    S.Helpers.Preapply.operations
    (fun (chain_store, block) params ops ->
      let* ctxt = Store.Block.context chain_store block in
      let chain_id = Store.Chain.chain_id chain_store in
      let mode =
        let predecessor_hash = Store.Block.hash block in
        let timestamp = Time.System.to_protocol (Time.System.now ()) in
        Next_proto.Partial_construction {predecessor_hash; timestamp}
      in
      let predecessor = Store.Block.shell_header block in
      let* validation_state =
        Next_proto.begin_validation ctxt chain_id mode ~predecessor ~cache:`Lazy
      in
      let* application_state =
        Next_proto.begin_application
          ctxt
          chain_id
          mode
          ~predecessor
          ~cache:`Lazy
      in
      let* hashed_ops =
        List.map_es
          (fun op ->
            match
              Data_encoding.Binary.to_bytes
                Next_proto.operation_data_encoding_with_legacy_attestation_name
                op.Next_proto.protocol_data
            with
            | Error _ ->
                failwith "preapply_operations: cannot deserialize operation"
            | Ok proto ->
                let op_t = {Operation.shell = op.shell; proto} in
                Lwt_result.return (Operation.hash op_t, op))
          ops
      in
      let* _validation_state, _application_state, acc =
        List.fold_left_es
          (fun (validation_state, application_state, acc) (oph, op) ->
            let* validation_state =
              Next_proto.validate_operation validation_state oph op
            in
            let* application_state, result =
              Next_proto.apply_operation application_state oph op
            in
            return
              ( validation_state,
                application_state,
                (op.protocol_data, result) :: acc ))
          (validation_state, application_state, [])
          hashed_ops
      in
      return (params#version, List.rev acc)) ;
  register1 S.Helpers.complete (fun (chain_store, block) prefix () () ->
      let* ctxt = Store.Block.context chain_store block in
      let*! l1 = Tezos_crypto.Base58.complete prefix in
      let*! l2 = Next_proto.complete_b58prefix ctxt prefix in
      return (l1 @ l2)) ;
  (* merge protocol rpcs... *)
  merge
    (Tezos_rpc.Directory.map
       (fun (chain_store, block) ->
         let hash = Store.Block.hash block in
         let header = Store.Block.header block in
         Lwt.return (chain_store, hash, header))
       (build_raw_header_rpc_directory (module Proto))) ;
  let proto_services =
    match Shell_plugin.find_rpc Next_proto.hash with
    | Some (module RPC) -> RPC.rpc_services
    | None -> Next_proto.rpc_services
  in
  merge
    (Tezos_rpc.Directory.map
       (fun (chain_store, block) ->
         let*! r =
           let*! context = Store.Block.context_exn chain_store block in
           let predecessor_context = context in
           let chain_id = Store.Chain.chain_id chain_store in
           let Block_header.
                 {
                   timestamp = predecessor_timestamp;
                   level = predecessor_level;
                   fitness = predecessor_fitness;
                   _;
                 } =
             Store.Block.shell_header block
           in
           (*
             Reactivity is important when executing RPCs and there are
             no constraints to be consistent with other nodes. For this
             reason, the RPC directory loads the cache lazily.
             See {!Environment_context.source_of_cache}.
           *)
           let predecessor = Store.Block.hash block in
           let timestamp = Time.System.to_protocol (Time.System.now ()) in
           let* value_of_key =
             Next_proto.value_of_key
               ~chain_id
               ~predecessor_context
               ~predecessor_timestamp
               ~predecessor_level
               ~predecessor_fitness
               ~predecessor
               ~timestamp
           in
           let* context =
             Tezos_protocol_environment.Context.load_cache
               predecessor
               predecessor_context
               `Lazy
               value_of_key
           in
           return
             {
               Tezos_protocol_environment.block_hash = Store.Block.hash block;
               block_header = Store.Block.shell_header block;
               context;
             }
         in
         match r with
         | Ok result -> Lwt.return result
         | Error _ -> Lwt.fail Not_found)
       proto_services) ;
  !dir

let get_protocol hash =
  match Registered_protocol.get hash with
  | None -> raise Not_found
  | Some protocol -> protocol

let get_directory chain_store block =
  let open Lwt_syntax in
  let* o = Store.Chain.get_rpc_directory chain_store block in
  match o with
  | Some dir -> Lwt.return dir
  | None -> (
      let* next_protocol_hash =
        Store.Block.protocol_hash_exn chain_store block
      in
      let (module Next_proto) = get_protocol next_protocol_hash in
      let build_fake_rpc_directory () =
        build_raw_rpc_directory
          (module Block_services.Fake_protocol)
          (module Next_proto)
      in
      if Store.Block.is_genesis chain_store (Store.Block.hash block) then
        Lwt.return (build_fake_rpc_directory ())
      else
        let* (module Proto) =
          let* o = Store.Block.read_predecessor_opt chain_store block in
          match o with
          | None ->
              (* No predecessors (e.g. pruned caboose), return the
                 current protocol *)
              Lwt.return (module Next_proto : Registered_protocol.T)
          | Some pred ->
              let* _, savepoint_level = Store.Chain.savepoint chain_store in
              let* protocol_hash =
                if Compare.Int32.(Store.Block.level pred < savepoint_level) then
                  let* predecessor_protocol =
                    Store.Chain.find_protocol
                      chain_store
                      ~protocol_level:(Store.Block.proto_level pred)
                  in
                  let protocol_hash =
                    WithExceptions.Option.to_exn
                      ~none:Not_found
                      predecessor_protocol
                  in
                  Lwt.return protocol_hash
                else Store.Block.protocol_hash_exn chain_store pred
              in
              Lwt.return (get_protocol protocol_hash)
        in
        let* o = Store.Chain.get_rpc_directory chain_store block in
        match o with
        | Some dir -> Lwt.return dir
        | None ->
            let dir =
              build_raw_rpc_directory (module Proto) (module Next_proto)
            in
            let* () =
              Store.Chain.set_rpc_directory
                chain_store
                ~protocol_hash:Proto.hash
                ~next_protocol_hash:Next_proto.hash
                dir
            in
            Lwt.return dir)

let build_rpc_directory chain_store block =
  let open Lwt_syntax in
  let* o = Store.Chain.block_of_identifier_opt chain_store block in
  match o with
  | None -> Lwt.fail Not_found
  | Some b ->
      let* dir = get_directory chain_store b in
      Lwt.return
        (Tezos_rpc.Directory.map (fun _ -> Lwt.return (chain_store, b)) dir)
