(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let rec read_partial_context context path depth =
  (* non tail-recursive *)
  if depth = 0 then Lwt.return Block_services.Cut
  else
    (* try to read as file *)
    Context.find context path
    >>= function
    | Some v ->
        Lwt.return (Block_services.Key v)
    | None ->
        (* try to read as directory *)
        Context.fold
          ~depth:(`Eq 1)
          context
          path
          ~init:TzString.Map.empty
          ~f:(fun k _ acc ->
            match path @ k with
            | [] ->
                (* This is an invariant of {!Context.fold} *)
                assert false
            | khd :: ktl as k ->
                read_partial_context context k (depth - 1)
                >>= fun v ->
                let k = List.last khd ktl in
                Lwt.return (TzString.Map.add k v acc))
        >|= fun map -> Block_services.Dir map

let build_raw_header_rpc_directory (module Proto : Block_services.PROTO) =
  let dir : (State.Chain.t * Block_hash.t * Block_header.t) RPC_directory.t ref
      =
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
  register0 S.header (fun (chain_state, hash, header) () () ->
      let protocol_data =
        Data_encoding.Binary.of_bytes_exn
          Proto.block_header_data_encoding
          header.protocol_data
      in
      return
        {
          Block_services.hash;
          chain_id = State.Chain.id chain_state;
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
  register0 S.protocols (fun (chain_state, _hash, header) () () ->
      State.Chain.get_level_indexed_protocol chain_state header
      >>= fun next_protocol_hash ->
      State.Block.header_of_hash chain_state header.shell.predecessor
      >>= function
      | None ->
          return
            {
              Tezos_shell_services.Block_services.current_protocol =
                next_protocol_hash;
              next_protocol = next_protocol_hash;
            }
      | Some pred_header ->
          State.Chain.get_level_indexed_protocol chain_state pred_header
          >>= fun protocol_hash ->
          return
            {
              Tezos_shell_services.Block_services.current_protocol =
                protocol_hash;
              next_protocol = next_protocol_hash;
            }) ;
  !dir

let build_raw_rpc_directory ~user_activated_upgrades
    ~user_activated_protocol_overrides (module Proto : Block_services.PROTO)
    (module Next_proto : Registered_protocol.T) =
  let dir : State.Block.block RPC_directory.t ref = ref RPC_directory.empty in
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
  register0 S.live_blocks (fun block () () ->
      State.Block.max_operations_ttl block
      >>=? fun max_op_ttl ->
      Chain_traversal.live_blocks block max_op_ttl
      >>=? fun (live_blocks, _) -> return live_blocks) ;
  (* block metadata *)
  let block_metadata block =
    State.Block.metadata block
    >>=? fun metadata ->
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.block_header_metadata_encoding
        metadata
    in
    State.Block.test_chain block
    >>= fun (test_chain_status, _) ->
    State.Block.max_operations_ttl block
    >>=? fun max_operations_ttl ->
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
  register0 S.metadata (fun block () () -> block_metadata block) ;
  let fail_opt = function None -> Lwt.fail Not_found | Some v -> return v in
  register0 S.metadata_hash (fun block () () ->
      State.Block.metadata_hash block >>= fail_opt) ;
  (* operations *)
  let convert_with_metadata chain_id (op : Operation.t) metadata :
      Block_services.operation =
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn Proto.operation_data_encoding op.proto
    in
    let receipt =
      Some
        (Data_encoding.Binary.of_bytes_exn
           Proto.operation_receipt_encoding
           metadata)
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
      receipt = None;
    }
  in
  let operations block =
    let chain_id = State.Block.chain_id block in
    State.Block.all_operations block
    >>= fun ops ->
    Lwt.catch
      (fun () ->
        State.Block.all_operations_metadata block
        >|= fun ops_metadata ->
        List.map2_e
          ~when_different_lengths:()
          (List.map2
             ~when_different_lengths:()
             (convert_with_metadata chain_id))
          ops
          ops_metadata
        |> function Ok v -> Ok v | Error () -> raise Not_found)
      (fun _ ->
        return (List.map (List.map (convert_without_metadata chain_id)) ops))
  in
  register0 S.Operations.operations (fun block () () -> operations block) ;
  register1 S.Operations.operations_in_pass (fun block i () () ->
      let chain_id = State.Block.chain_id block in
      Lwt.try_bind
        (fun () -> State.Block.operations block i)
        (fun (ops, _path) ->
          Lwt.catch
            (fun () ->
              State.Block.operations_metadata block i
              >|= fun ops_metadata ->
              List.map2
                ~when_different_lengths:()
                (convert_with_metadata chain_id)
                ops
                ops_metadata
              |> function Ok v -> Ok v | Error () -> raise Not_found)
            (fun _ ->
              return ((List.map (convert_without_metadata chain_id)) ops)))
        (fun _ -> raise Not_found)) ;
  register2 S.Operations.operation (fun block i j () () ->
      let chain_id = State.Block.chain_id block in
      Lwt.catch
        (fun () ->
          State.Block.operations block i
          >>= fun (ops, _path) ->
          let op = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth ops j in
          Lwt.catch
            (fun () ->
              State.Block.operations_metadata block i
              >>= fun metadata ->
              let op_metadata =
                WithExceptions.Option.get ~loc:__LOC__ @@ List.nth metadata j
              in
              return (convert_with_metadata chain_id op op_metadata))
            (fun _ -> return (convert_without_metadata chain_id op)))
        (fun _ -> raise Not_found)) ;
  (* operation_hashes *)
  register0 S.Operation_hashes.operation_hashes (fun block () () ->
      State.Block.all_operation_hashes block >>= return) ;
  register1 S.Operation_hashes.operation_hashes_in_pass (fun block i () () ->
      State.Block.operation_hashes block i >>= fun (ops, _) -> return ops) ;
  register2 S.Operation_hashes.operation_hash (fun block i j () () ->
      Lwt.catch
        (fun () ->
          State.Block.operation_hashes block i
          >|= fun (ops, _) -> List.nth ops j)
        (fun _ -> raise Not_found)
      >>= fail_opt) ;
  (* operation_metadata_hashes *)
  register0 S.Operation_metadata_hashes.root (fun block () () ->
      State.Block.all_operations_metadata_hash block >>= fail_opt) ;
  register0
    S.Operation_metadata_hashes.operation_metadata_hashes
    (fun block () () ->
      State.Block.all_operations_metadata_hashes block >>= fail_opt) ;
  register1
    S.Operation_metadata_hashes.operation_metadata_hashes_in_pass
    (fun block i () () ->
      State.Block.operations_metadata_hashes block i >>= fail_opt) ;
  register2
    S.Operation_metadata_hashes.operation_metadata_hash
    (fun block i j () () ->
      State.Block.operations_metadata_hashes block i
      >>= fun hashes ->
      Lwt.return @@ Option.bind hashes (fun hashes -> List.nth hashes j)
      >>= fail_opt) ;
  (* context *)
  register1 S.Context.read (fun block path q () ->
      let depth = Option.value ~default:max_int q#depth in
      fail_unless
        (depth >= 0)
        (Tezos_shell_services.Block_services.Invalid_depth_arg depth)
      >>=? fun () ->
      State.Block.context block
      >>=? fun context ->
      Context.mem context path
      >>= fun mem ->
      Context.mem_tree context path
      >>= fun dir_mem ->
      if not (mem || dir_mem) then Lwt.fail Not_found
      else read_partial_context context path depth >>= fun dir -> return dir) ;
  (* info *)
  register0 S.info (fun block () () ->
      let chain_id = State.Block.chain_id block in
      let hash = State.Block.hash block in
      let header = State.Block.header block in
      let shell = header.shell in
      let protocol_data =
        Data_encoding.Binary.of_bytes_exn
          Proto.block_header_data_encoding
          header.protocol_data
      in
      block_metadata block
      >>= (function
            | Ok metadata -> return_some metadata | Error _ -> return_none)
      >>=? fun metadata ->
      operations block
      >>=? fun operations ->
      return
        {
          Block_services.hash;
          chain_id;
          header = {shell; protocol_data};
          metadata;
          operations;
        }) ;
  (* helpers *)
  register0 S.Helpers.Preapply.block (fun block q p ->
      let timestamp =
        match q#timestamp with
        | None ->
            Time.System.to_protocol (Systime_os.now ())
        | Some time ->
            time
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
                List.sort Next_proto.compare_operations operations
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
      Prevalidation.preapply
        ~user_activated_upgrades
        ~user_activated_protocol_overrides
        ~predecessor:block
        ~timestamp
        ~protocol_data
        operations) ;
  register0 S.Helpers.Preapply.operations (fun block () ops ->
      State.Block.context block
      >>=? fun ctxt ->
      let predecessor = State.Block.hash block in
      let header = State.Block.shell_header block in
      let predecessor_context = Shell_context.wrap_disk_context ctxt in
      Next_proto.begin_construction
        ~chain_id:(State.Block.chain_id block)
        ~predecessor_context
        ~predecessor_timestamp:header.timestamp
        ~predecessor_level:header.level
        ~predecessor_fitness:header.fitness
        ~predecessor
        ~timestamp:(Time.System.to_protocol (Systime_os.now ()))
        ()
      >>=? fun state ->
      List.fold_left_es
        (fun (state, acc) op ->
          Next_proto.apply_operation state op
          >>=? fun (state, result) ->
          return (state, (op.protocol_data, result) :: acc))
        (state, [])
        ops
      >>=? fun (state, acc) ->
      Next_proto.finalize_block state >>=? fun _ -> return (List.rev acc)) ;
  register1 S.Helpers.complete (fun block prefix () () ->
      State.Block.context block
      >>=? fun ctxt ->
      Base58.complete prefix
      >>= fun l1 ->
      let ctxt = Shell_context.wrap_disk_context ctxt in
      Next_proto.complete_b58prefix ctxt prefix >>= fun l2 -> return (l1 @ l2)) ;
  (* merge protocol rpcs... *)
  merge
    (RPC_directory.map
       (fun block ->
         let chain_state = State.Block.chain_state block in
         let hash = State.Block.hash block in
         let header = State.Block.header block in
         Lwt.return (chain_state, hash, header))
       (build_raw_header_rpc_directory (module Proto))) ;
  let proto_services =
    match Prevalidator_filters.find Next_proto.hash with
    | Some (module Filters) ->
        Filters.RPC.rpc_services
    | None ->
        Next_proto.rpc_services
  in
  merge
    (RPC_directory.map
       (fun block ->
         State.Block.context_exn block
         >|= fun context ->
         let context = Shell_context.wrap_disk_context context in
         {
           Tezos_protocol_environment.block_hash = State.Block.hash block;
           block_header = State.Block.shell_header block;
           context;
         })
       proto_services) ;
  !dir

let get_protocol hash =
  match Registered_protocol.get hash with
  | None ->
      raise Not_found
  | Some protocol ->
      protocol

let get_directory ~user_activated_upgrades ~user_activated_protocol_overrides
    chain_state block =
  State.Block.get_rpc_directory block
  >>= function
  | Some dir ->
      Lwt.return dir
  | None -> (
      State.Block.protocol_hash_exn block
      >>= fun next_protocol_hash ->
      let next_protocol = get_protocol next_protocol_hash in
      State.Block.predecessor block
      >>= function
      | None ->
          Lwt.return
            (build_raw_rpc_directory
               ~user_activated_upgrades
               ~user_activated_protocol_overrides
               (module Block_services.Fake_protocol)
               next_protocol)
      | Some pred -> (
          State.Chain.save_point chain_state
          >>= fun (save_point_level, _) ->
          ( if Compare.Int32.(State.Block.level pred < save_point_level) then
            State.Chain.get_level_indexed_protocol
              chain_state
              (State.Block.header pred)
          else State.Block.protocol_hash_exn pred )
          >>= fun protocol_hash ->
          let (module Proto) = get_protocol protocol_hash in
          State.Block.get_rpc_directory block
          >>= function
          | Some dir ->
              Lwt.return dir
          | None ->
              let dir =
                build_raw_rpc_directory
                  ~user_activated_upgrades
                  ~user_activated_protocol_overrides
                  (module Proto)
                  next_protocol
              in
              State.Block.set_rpc_directory block dir
              >>= fun () -> Lwt.return dir ) )

let get_block chain_state = function
  | `Genesis ->
      Chain.genesis chain_state >>= fun genesis -> Lwt.return_some genesis
  | `Head n ->
      Chain.head chain_state
      >>= fun head ->
      if n < 0 then Lwt.return_none
      else if n = 0 then Lwt.return_some head
      else
        State.Block.read_predecessor
          chain_state
          ~pred:n
          (State.Block.hash head)
  | (`Alias (_, n) | `Hash (_, n)) as b ->
      ( match b with
      | `Alias (`Checkpoint, _) ->
          State.Chain.checkpoint chain_state
          >>= fun checkpoint -> Lwt.return (Block_header.hash checkpoint)
      | `Alias (`Save_point, _) ->
          State.Chain.save_point chain_state
          >>= fun (_, save_point) -> Lwt.return save_point
      | `Alias (`Caboose, _) ->
          State.Chain.caboose chain_state
          >>= fun (_, caboose) -> Lwt.return caboose
      | `Hash (h, _) ->
          Lwt.return h )
      >>= fun hash ->
      if n < 0 then
        State.Block.read_opt chain_state hash
        >>= function
        | None ->
            Lwt.fail Not_found
        | Some block ->
            Chain.head chain_state
            >>= fun head ->
            let head_level = State.Block.level head in
            let block_level = State.Block.level block in
            let target =
              Int32.(to_int (sub head_level (sub block_level (of_int n))))
            in
            if target < 0 then Lwt.return_none
            else
              State.Block.read_predecessor
                chain_state
                ~pred:target
                (State.Block.hash head)
      else if n = 0 then
        Chain.genesis chain_state
        >>= fun genesis ->
        let genesis_hash = State.Block.hash genesis in
        if Block_hash.equal hash genesis_hash then Lwt.return_some genesis
        else State.Block.read_predecessor chain_state ~pred:0 hash
      else State.Block.read_predecessor chain_state ~pred:n hash
  | `Level i ->
      Chain.head chain_state
      >>= fun head ->
      let target = Int32.(to_int (sub (State.Block.level head) i)) in
      if target < 0 then Lwt.fail Not_found
      else
        State.Block.read_predecessor
          chain_state
          ~pred:target
          (State.Block.hash head)

let build_rpc_directory ~user_activated_upgrades
    ~user_activated_protocol_overrides chain_state block =
  get_block chain_state block
  >>= function
  | None ->
      Lwt.fail Not_found
  | Some b ->
      get_directory
        ~user_activated_upgrades
        ~user_activated_protocol_overrides
        chain_state
        b
      >>= fun dir -> Lwt.return (RPC_directory.map (fun _ -> Lwt.return b) dir)
