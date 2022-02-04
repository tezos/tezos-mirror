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

let read_partial_context =
  let init = Block_services.Dir TzString.Map.empty in
  fun context path depth ->
    if depth = 0 then Lwt.return Block_services.Cut
    else
      (* According to the documentation of Context.fold,
         "[f] is never called with an empty key for values; i.e.,
           folding over a value is a no-op".
         Therefore, we first need to check that whether its a value.
      *)
      Context.find context path >>= function
      | Some v -> Lwt.return (Block_services.Key v)
      | None ->
          (* try to read as directory *)
          Context.fold
            ~depth:(`Le depth)
            context
            path
            ~order:`Sorted
            ~init
            ~f:(fun k tree acc ->
              let open Block_services in
              if List.compare_length_with k depth >= 0 then
                (* only [=] case is possible because [~depth] is [(`Le depth)] *)
                Lwt.return (raw_context_insert (k, Cut) acc)
              else
                Context.Tree.to_value tree >|= function
                | None -> acc
                | Some v -> raw_context_insert (k, Key v) acc)

let build_raw_header_rpc_directory (module Proto : Block_services.PROTO) =
  let dir :
      (Store.chain_store * Block_hash.t * Block_header.t) RPC_directory.t ref =
    ref RPC_directory.empty
  in
  let register0 s f =
    dir :=
      RPC_directory.register !dir (RPC_service.subst0 s) (fun block p q ->
          f block p q)
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
  (* helpers *)
  register0 S.Helpers.Forge.block_header (fun _block () header ->
      return (Data_encoding.Binary.to_bytes_exn Block_header.encoding header)) ;
  (* protocols *)
  register0 S.protocols (fun (chain_store, _hash, header) () () ->
      Store.Chain.find_protocol
        chain_store
        ~protocol_level:header.shell.proto_level
      >>= fun next_proto ->
      let next_protocol_hash =
        WithExceptions.Option.to_exn ~none:Not_found next_proto
      in
      Store.Block.read_block_opt chain_store header.shell.predecessor
      >>= function
      | None ->
          return
            {
              Tezos_shell_services.Block_services.current_protocol =
                next_protocol_hash;
              next_protocol = next_protocol_hash;
            }
      | Some pred_block ->
          let pred_header = Store.Block.header pred_block in
          Store.Chain.find_protocol
            chain_store
            ~protocol_level:pred_header.shell.proto_level
          >>= fun current_protocol ->
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

let build_raw_rpc_directory (module Proto : Block_services.PROTO)
    (module Next_proto : Registered_protocol.T) =
  let dir : (Store.chain_store * Store.Block.block) RPC_directory.t ref =
    ref RPC_directory.empty
  in
  let merge d = dir := RPC_directory.merge d !dir in
  let register0 s f =
    dir :=
      RPC_directory.register !dir (RPC_service.subst0 s) (fun block p q ->
          f block p q)
  in
  let register1 s f =
    dir :=
      RPC_directory.register !dir (RPC_service.subst1 s) (fun (block, a) p q ->
          f block a p q)
  in
  let register2 s f =
    dir :=
      RPC_directory.register
        !dir
        (RPC_service.subst2 s)
        (fun ((block, a), b) p q -> f block a b p q)
  in
  let module Block_services = Block_services.Make (Proto) (Next_proto) in
  let module S = Block_services.S in
  register0 S.live_blocks (fun (chain_store, block) () () ->
      Store.Chain.compute_live_blocks chain_store ~block
      >>=? fun (live_blocks, _) -> return live_blocks) ;
  (* block metadata *)
  let block_metadata chain_store block =
    Store.Block.get_block_metadata chain_store block >>=? fun metadata ->
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.block_header_metadata_encoding
        (Store.Block.block_metadata metadata)
    in
    Store.Block.testchain_status chain_store block
    >>=? fun (test_chain_status, _) ->
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
      Data_encoding.Binary.of_bytes_exn Proto.operation_data_encoding op.proto
    in
    let receipt =
      match metadata with
      | Block_validation.Metadata bytes ->
          Block_services.Receipt
            (Data_encoding.Binary.of_bytes_exn
               Proto.operation_receipt_encoding
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
      Data_encoding.Binary.of_bytes_exn Proto.operation_data_encoding op.proto
    in
    {
      Block_services.chain_id;
      hash = Operation.hash op;
      shell = op.shell;
      protocol_data;
      receipt = Empty;
    }
  in
  let operations chain_store block =
    let chain_id = Store.Chain.chain_id chain_store in
    let ops = Store.Block.operations block in
    Store.Block.get_block_metadata_opt chain_store block >>= function
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
  (*****************************************************************)
  register0 S.Operations.operations (fun (chain_store, block) () () ->
      operations chain_store block) ;
  register1 S.Operations.operations_in_pass (fun (chain_store, block) i () () ->
      let chain_id = Store.Chain.chain_id chain_store in
      let (ops, _path) = Store.Block.operations_path block i in
      Lwt.catch
        (fun () ->
          Store.Block.get_block_metadata_opt chain_store block >>= function
          | None -> return (List.map (convert_without_metadata chain_id) ops)
          | Some metadata -> (
              let opss_metadata = Store.Block.operations_metadata metadata in
              let ops_metadata =
                List.nth opss_metadata i
                |> WithExceptions.Option.to_exn ~none:Not_found
              in
              List.map2
                ~when_different_lengths:()
                (convert_with_metadata chain_id)
                ops
                ops_metadata
              |> function
              | Ok x -> return x
              | _ -> raise Not_found))
        (fun _ -> Lwt.fail Not_found)) ;
  register2 S.Operations.operation (fun (chain_store, block) i j () () ->
      let chain_id = Store.Chain.chain_id chain_store in
      Lwt.catch
        (fun () ->
          let (ops, _path) = Store.Block.operations_path block i in
          let op =
            List.nth ops j |> WithExceptions.Option.to_exn ~none:Not_found
          in
          Store.Block.get_block_metadata_opt chain_store block >>= function
          | None -> return (convert_without_metadata chain_id op)
          | Some metadata ->
              let opss_metadata = Store.Block.operations_metadata metadata in
              let ops_metadata =
                List.nth opss_metadata i
                |> WithExceptions.Option.to_exn ~none:Not_found
              in
              let op_metadata =
                List.nth ops_metadata j
                |> WithExceptions.Option.to_exn ~none:Not_found
              in
              return (convert_with_metadata chain_id op op_metadata))
        (fun _ -> Lwt.fail Not_found)) ;
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
          let (ops, _) = Store.Block.operations_hashes_path block i in
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
      fail_opt (Store.Block.operations_metadata_hashes block)
      >>=? fun ops_metadata_hashes ->
      fail_opt (List.nth_opt ops_metadata_hashes i)) ;
  register2
    S.Operation_metadata_hashes.operation_metadata_hash
    (fun (_, block) i j () () ->
      Lwt.catch
        (fun () ->
          fail_opt (Store.Block.operations_metadata_hashes_path block i)
          >>=? fun hashes ->
          return
            (List.nth hashes j |> WithExceptions.Option.to_exn ~none:Not_found))
        (fun _ -> Lwt.fail Not_found)) ;
  (* context *)
  register1 S.Context.read (fun (chain_store, block) path q () ->
      let depth = Option.value ~default:max_int q#depth in
      (* [depth] is defined as a [uint] not an [int] *)
      assert (depth >= 0) ;
      Store.Block.context chain_store block >>=? fun context ->
      Context.mem context path >>= fun mem ->
      Context.mem_tree context path >>= fun dir_mem ->
      if not (mem || dir_mem) then Lwt.fail Not_found
      else read_partial_context context path depth >>= Lwt.return_ok) ;
  register1 S.Context.merkle_tree (fun (chain_store, block) path query () ->
      Store.Block.context_opt chain_store block >>= function
      | None -> return None
      | Some context ->
          let holey = Option.value ~default:false query#holey in
          let leaf_kind =
            let open Tezos_shell_services.Block_services in
            if holey then Hole else Raw_context
          in
          Context.merkle_tree context leaf_kind path >>= return_some) ;
  (* info *)
  register0 S.info (fun (chain_store, block) () () ->
      let chain_id = Store.Chain.chain_id chain_store in
      let hash = Store.Block.hash block in
      let header = Store.Block.header block in
      let shell = header.shell in
      let protocol_data =
        Data_encoding.Binary.of_bytes_exn
          Proto.block_header_data_encoding
          header.protocol_data
      in
      (block_metadata chain_store block >>= function
       | Ok metadata -> return_some metadata
       | Error _ -> return_none)
      >>=? fun metadata ->
      operations chain_store block >>=? fun operations ->
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
        | None -> Time.System.to_protocol (Systime_os.now ())
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
              if q#sort_operations then
                List.sort Next_proto.relative_position_within_block operations
              else operations
            in
            List.map
              (fun op ->
                let proto =
                  Data_encoding.Binary.to_bytes_exn
                    Next_proto.operation_data_encoding
                    op.Next_proto.protocol_data
                in
                {Operation.shell = op.shell; proto})
              operations)
          p.operations
      in
      (try return (Block_validator.running_worker ())
       with _ -> failwith "Block validator is not running")
      >>=? fun bv ->
      Block_validator.preapply
        bv
        chain_store
        ~predecessor:block
        ~timestamp
        ~protocol_data
        operations) ;
  register0 S.Helpers.Preapply.operations (fun (chain_store, block) () ops ->
      Store.Block.context chain_store block >>=? fun ctxt ->
      let predecessor = Store.Block.hash block in
      let header = Store.Block.shell_header block in
      let predecessor_context = Shell_context.wrap_disk_context ctxt in
      Next_proto.begin_construction
        ~chain_id:(Store.Chain.chain_id chain_store)
        ~predecessor_context
        ~predecessor_timestamp:header.timestamp
        ~predecessor_level:header.level
        ~predecessor_fitness:header.fitness
        ~predecessor
        ~timestamp:(Time.System.to_protocol (Systime_os.now ()))
        ~cache:`Lazy
        ()
      >>=? fun state ->
      List.fold_left_es
        (fun (state, acc) op ->
          Next_proto.apply_operation state op >>=? fun (state, result) ->
          return (state, (op.protocol_data, result) :: acc))
        (state, [])
        ops
      >>=? fun (state, acc) ->
      (* A pre application must not commit into the protocol caches.
         Hence, we set [cache_nonce] to None. *)
      Next_proto.finalize_block state None >>=? fun _ -> return (List.rev acc)) ;
  register1 S.Helpers.complete (fun (chain_store, block) prefix () () ->
      Store.Block.context chain_store block >>=? fun ctxt ->
      Base58.complete prefix >>= fun l1 ->
      let ctxt = Shell_context.wrap_disk_context ctxt in
      Next_proto.complete_b58prefix ctxt prefix >>= fun l2 -> return (l1 @ l2)) ;
  (* merge protocol rpcs... *)
  merge
    (RPC_directory.map
       (fun (chain_store, block) ->
         let hash = Store.Block.hash block in
         let header = Store.Block.header block in
         Lwt.return (chain_store, hash, header))
       (build_raw_header_rpc_directory (module Proto))) ;
  let proto_services =
    match Prevalidator_filters.find Next_proto.hash with
    | Some (module Filters) -> Filters.RPC.rpc_services
    | None -> Next_proto.rpc_services
  in
  merge
    (RPC_directory.map
       (fun (chain_store, block) ->
         ( Store.Block.context_exn chain_store block >>= fun context ->
           let predecessor_context = Shell_context.wrap_disk_context context in
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
           let timestamp = Time.System.to_protocol (Systime_os.now ()) in
           Next_proto.value_of_key
             ~chain_id
             ~predecessor_context
             ~predecessor_timestamp
             ~predecessor_level
             ~predecessor_fitness
             ~predecessor
             ~timestamp
           >>=? fun value_of_key ->
           Tezos_protocol_environment.Context.load_cache
             predecessor
             predecessor_context
             `Lazy
             value_of_key
           >>=? fun context ->
           return
             {
               Tezos_protocol_environment.block_hash = Store.Block.hash block;
               block_header = Store.Block.shell_header block;
               context;
             } )
         >>= function
         | Ok result -> Lwt.return result
         | Error _ -> Lwt.fail Not_found)
       proto_services) ;
  !dir

let get_protocol hash =
  match Registered_protocol.get hash with
  | None -> raise Not_found
  | Some protocol -> protocol

let get_directory chain_store block =
  Store.Chain.get_rpc_directory chain_store block >>= function
  | Some dir -> Lwt.return dir
  | None -> (
      Store.Block.protocol_hash_exn chain_store block
      >>= fun next_protocol_hash ->
      let (module Next_proto) = get_protocol next_protocol_hash in
      let build_fake_rpc_directory () =
        build_raw_rpc_directory
          (module Block_services.Fake_protocol)
          (module Next_proto)
      in
      if Store.Block.is_genesis chain_store (Store.Block.hash block) then
        Lwt.return (build_fake_rpc_directory ())
      else
        (Store.Block.read_predecessor_opt chain_store block >>= function
         | None ->
             (* No predecessors (e.g. pruned caboose), return the
                current protocol *)
             Lwt.return (module Next_proto : Registered_protocol.T)
         | Some pred ->
             Store.Chain.savepoint chain_store >>= fun (_, savepoint_level) ->
             (if Compare.Int32.(Store.Block.level pred < savepoint_level) then
              Store.Chain.find_protocol
                chain_store
                ~protocol_level:(Store.Block.proto_level pred)
              >>= fun predecessor_protocol ->
              let protocol_hash =
                WithExceptions.Option.to_exn
                  ~none:Not_found
                  predecessor_protocol
              in
              Lwt.return protocol_hash
             else Store.Block.protocol_hash_exn chain_store pred)
             >>= fun protocol_hash -> Lwt.return (get_protocol protocol_hash))
        >>= fun (module Proto) ->
        Store.Chain.get_rpc_directory chain_store block >>= function
        | Some dir -> Lwt.return dir
        | None ->
            let dir =
              build_raw_rpc_directory (module Proto) (module Next_proto)
            in
            Store.Chain.set_rpc_directory
              chain_store
              ~protocol_hash:Proto.hash
              ~next_protocol_hash:Next_proto.hash
              dir
            >>= fun () -> Lwt.return dir)

let build_rpc_directory chain_store block =
  Store.Chain.block_of_identifier_opt chain_store block >>= function
  | None -> Lwt.fail Not_found
  | Some b ->
      get_directory chain_store b >>= fun dir ->
      Lwt.return (RPC_directory.map (fun _ -> Lwt.return (chain_store, b)) dir)
