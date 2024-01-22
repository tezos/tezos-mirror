(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2023 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type failure_kind =
  | Nothing_to_reconstruct
  | Cannot_read_block_hash of Block_hash.t
  | Cannot_read_block_level of Int32.t
  | Cannot_read_resulting_context_hash of Block_hash.t

let failure_kind_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"nothing_to_reconstruct"
        empty
        (function Nothing_to_reconstruct -> Some () | _ -> None)
        (fun () -> Nothing_to_reconstruct);
      case
        (Tag 1)
        ~title:"cannot_read_block_hash"
        Block_hash.encoding
        (function Cannot_read_block_hash h -> Some h | _ -> None)
        (fun h -> Cannot_read_block_hash h);
      case
        (Tag 2)
        ~title:"cannot_read_block_level"
        int32
        (function Cannot_read_block_level l -> Some l | _ -> None)
        (fun l -> Cannot_read_block_level l);
    ]

let failure_kind_pp ppf = function
  | Nothing_to_reconstruct -> Format.fprintf ppf "nothing to reconstruct"
  | Cannot_read_block_hash h ->
      Format.fprintf ppf "Unexpected missing block in store: %a" Block_hash.pp h
  | Cannot_read_block_level l ->
      Format.fprintf ppf "Unexpected missing block in store at level %ld" l
  | Cannot_read_resulting_context_hash h ->
      Format.fprintf
        ppf
        "Unexpected missing resulting context hash in store for block %a"
        Block_hash.pp
        h

type error += Reconstruction_failure of failure_kind

type error += Cannot_reconstruct of History_mode.t

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"reconstruction.reconstruction_failure"
    ~title:"Reconstruction failure"
    ~description:"Error while performing storage reconstruction."
    ~pp:(fun ppf reason ->
      Format.fprintf
        ppf
        "The data contained in the storage is not valid. The reconstruction \
         procedure failed: %a."
        failure_kind_pp
        reason)
    (obj1 (req "reason" failure_kind_encoding))
    (function Reconstruction_failure r -> Some r | _ -> None)
    (fun r -> Reconstruction_failure r) ;
  register_error_kind
    `Permanent
    ~id:"reconstruction.cannot_failure"
    ~title:"Cannot reconstruct"
    ~description:"Cannot reconstruct"
    ~pp:(fun ppf hm ->
      Format.fprintf
        ppf
        "Cannot reconstruct storage from %a mode."
        History_mode.pp
        hm)
    (obj1 (req "history_mode " History_mode.encoding))
    (function Cannot_reconstruct hm -> Some hm | _ -> None)
    (fun hm -> Cannot_reconstruct hm)

open Reconstruction_events

(* The status of a metadata. It is:
   - Complete: all the metadata of the corresponding cycle are stored
   - Partial level: the metadata before level are missing
   - Not_stored: no metadata are stored *)
type metadata_status = Complete | Partial of Int32.t | Not_stored

(* We assume that :
   - a cemented metadata cycle is partial if, at least, the first
     metadata of the cycle (start_level) is missing.
   - there only exists a contiguous set of empty metadata *)
let cemented_metadata_status cemented_store
    {Cemented_block_store.start_level; end_level; _} =
  let open Lwt_result_syntax in
  let* o = Cemented_block_store.read_block_metadata cemented_store end_level in
  match o with
  | None -> return Not_stored
  | Some _ -> (
      let* o =
        Cemented_block_store.read_block_metadata cemented_store start_level
      in
      match o with
      | Some _ -> return Complete
      | None ->
          let rec search inf sup =
            if inf >= sup then return (Partial inf)
            else
              let level = Int32.(add inf (div (sub sup inf) 2l)) in
              let* o =
                Cemented_block_store.read_block_metadata cemented_store level
              in
              match o with
              | None -> search (Int32.succ level) sup
              | Some _ -> search inf (Int32.pred level)
          in
          search (Int32.succ start_level) (Int32.pred end_level))

(* We assume that the given list is not empty. *)
let compute_block_metadata_hash block_metadata =
  Some (Block_metadata_hash.hash_bytes [block_metadata])

let split_operations_metadata = function
  | Block_validation.No_metadata_hash metadata -> (metadata, None)
  | Metadata_hash l ->
      let metadata, hashes =
        List.fold_left
          (fun (metadata_acc, hashes_acc) l ->
            let metadata, hashes = List.split l in
            (metadata :: metadata_acc, hashes :: hashes_acc))
          ([], [])
          l
      in
      (List.rev metadata, Some (List.rev hashes))

let compute_all_operations_metadata_hash block =
  if Block_repr.validation_passes block = 0 then None
  else
    Option.map
      (fun ll ->
        Operation_metadata_list_list_hash.compute
          (List.map Operation_metadata_list_hash.compute ll))
      (Block_repr.operations_metadata_hashes block)

let apply_context context_index chain_id ~user_activated_upgrades
    ~user_activated_protocol_overrides ~operation_metadata_size_limit
    ~predecessor_block_metadata_hash ~predecessor_ops_metadata_hash
    ~predecessor_block ~expected_context_hash block =
  let open Lwt_result_syntax in
  let block_header = Store.Block.header block in
  let operations = Store.Block.operations block in
  let predecessor_block_header = Store.Block.header predecessor_block in
  let predecessor_resulting_context_hash =
    if
      expected_context_hash
      = Tezos_protocol_environment__Environment_context.Resulting_context
    then Store.Block.context_hash predecessor_block
    else Store.Block.context_hash block
  in
  let*! predecessor_context =
    Context_ops.checkout_exn context_index predecessor_resulting_context_hash
  in
  let apply_environment =
    {
      Block_validation.max_operations_ttl =
        Int32.to_int (Store.Block.level predecessor_block);
      chain_id;
      predecessor_block_header;
      predecessor_context;
      predecessor_block_metadata_hash;
      predecessor_ops_metadata_hash;
      predecessor_resulting_context_hash;
      user_activated_upgrades;
      user_activated_protocol_overrides;
      operation_metadata_size_limit;
    }
  in
  let operations =
    List.map (List.map Block_validation.mk_operation) operations
  in
  let* {
         result =
           {Block_validation.validation_store; block_metadata; ops_metadata; _};
         _;
       } =
    Block_validation.apply
      apply_environment
      block_header
      operations
      ~cache:`Lazy
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/1570
       Reuse in-memory caches along reconstruction
       Since the reconstruction follows the history in a linear way, we
       could do a better usage of in-memory caches by reusing them from one
       block to the next one.
    *)
  in
  return
    ( validation_store.resulting_context_hash,
      validation_store.message,
      validation_store.max_operations_ttl,
      validation_store.last_preserved_block_level,
      fst block_metadata,
      ops_metadata )

(** Returns the protocol environment version of a given protocol level. *)
let protocol_of_protocol_level chain_store protocol_level block_hash =
  let open Lwt_result_syntax in
  let* protocol_hash =
    let*! o = Store.Chain.find_protocol chain_store ~protocol_level in
    match o with
    | Some ph -> return ph
    | None -> tzfail (Store_errors.Cannot_find_protocol protocol_level)
  in
  trace
    (Block_validator_errors.Unavailable_protocol
       {block = block_hash; protocol = protocol_hash})
    (Registered_protocol.get_result protocol_hash)

(* Restores the block and operations metadata hash of a given block,
   if needed. *)
let restore_block_contents chain_store block_protocol_env ~block_metadata
    ~operations_metadata message max_operations_ttl last_preserved_block_level
    block =
  let operations_metadata, operations_metadata_hashes =
    split_operations_metadata operations_metadata
  in
  let contents =
    if
      Store.Block.is_genesis chain_store (Block_repr.hash block)
      || block_protocol_env = Protocol.V0
    then block.contents
    else
      {
        block.contents with
        block_metadata_hash = compute_block_metadata_hash block_metadata;
        operations_metadata_hashes;
      }
  in
  let metadata =
    {
      Block_repr.message;
      max_operations_ttl;
      last_preserved_block_level;
      block_metadata;
      operations_metadata;
    }
  in
  {block with contents; metadata = Some metadata}

let reconstruct_genesis_operations_metadata chain_store =
  let open Lwt_result_syntax in
  let*! genesis_block = Store.Chain.genesis_block chain_store in
  let* {
         message;
         max_operations_ttl;
         last_preserved_block_level;
         block_metadata;
         operations_metadata;
       } =
    Store.Block.get_block_metadata chain_store genesis_block
  in
  let operations_metadata =
    match Store.Block.operations_metadata_hashes genesis_block with
    | Some v ->
        let operations_metadata =
          WithExceptions.List.map2
            ~loc:__LOC__
            (WithExceptions.List.combine ~loc:__LOC__)
            operations_metadata
            v
        in
        Block_validation.Metadata_hash operations_metadata
    | None -> No_metadata_hash operations_metadata
  in
  let* resulting_context_hash =
    Store.Block.resulting_context_hash chain_store genesis_block
  in
  return
    ( resulting_context_hash,
      message,
      max_operations_ttl,
      last_preserved_block_level,
      block_metadata,
      operations_metadata )

let reconstruct_chunk chain_store context_index ~user_activated_upgrades
    ~user_activated_protocol_overrides ~operation_metadata_size_limit
    ~start_level ~end_level =
  let open Lwt_result_syntax in
  let chain_id = Store.Chain.chain_id chain_store in
  let rec loop level acc =
    if level > end_level then return List.(rev acc)
    else
      let* block =
        let*! o = Store.Block.read_block_by_level_opt chain_store level in
        match o with
        | None ->
            failwith
              "Cannot read block in cemented store. The storage is corrupted."
        | Some b -> return b
      in
      let* (module Proto) =
        protocol_of_protocol_level
          chain_store
          (Store.Block.proto_level block)
          (Store.Block.hash block)
      in
      let* ( _resulting_context_hash,
             message,
             max_operations_ttl,
             last_preserved_block_level,
             block_metadata,
             operations_metadata ) =
        if Store.Block.is_genesis chain_store (Store.Block.hash block) then
          reconstruct_genesis_operations_metadata chain_store
        else
          let* ( predecessor_block,
                 predecessor_block_metadata_hash,
                 predecessor_ops_metadata_hash ) =
            match acc with
            | [] ->
                (* As the predecessor of the first block of the chunk was
                   already reconstructed and stored, we can read it as
                   usual. *)
                let* predecessor_block =
                  Store.Block.read_predecessor chain_store block
                in
                return
                  ( predecessor_block,
                    Store.Block.block_metadata_hash predecessor_block,
                    Store.Block.all_operations_metadata_hash predecessor_block
                  )
            | (pred, _) :: _ ->
                (* While the chunk is being recontsructed, we compute the
                   block and operations metadata hash using the predecessor
                   stored in chunk being accumulated instead of reading
                   it. *)
                let predecessor_block = Store.Unsafe.block_of_repr pred in
                return
                  ( predecessor_block,
                    Block_repr.block_metadata_hash pred,
                    compute_all_operations_metadata_hash pred )
          in
          apply_context
            context_index
            chain_id
            ~user_activated_upgrades
            ~user_activated_protocol_overrides
            ~operation_metadata_size_limit
            ~predecessor_block_metadata_hash
            ~predecessor_ops_metadata_hash
            ~predecessor_block
            ~expected_context_hash:Proto.expected_context_hash
            block
      in
      let*! () =
        Event.(emit reconstruct_block_success) (Store.Block.descriptor block)
      in
      let block_protocol_env = Proto.environment_version in
      let reconstructed_block =
        restore_block_contents
          chain_store
          block_protocol_env
          ~block_metadata
          ~operations_metadata
          message
          max_operations_ttl
          last_preserved_block_level
          (Store.Unsafe.repr_of_block block)
      in
      loop (Int32.succ level) ((reconstructed_block, block_protocol_env) :: acc)
  in
  loop start_level []

let store_chunk cemented_store chunk =
  let open Lwt_result_syntax in
  let* lower_block, lower_env_version =
    match List.hd chunk with
    | None -> failwith "Cannot read chunk to cement."
    | Some e -> return e
  in
  let* _, higher_env_version =
    match List.last_opt chunk with
    | None -> failwith "Cannot read chunk to cement."
    | Some e -> return e
  in
  let block_chunk = List.map fst chunk in
  if lower_env_version = Protocol.V0 && higher_env_version = Protocol.V0 then
    (* No need to rewrite the cemented blocks as the block and
       operation metadata hashes are not expected to be stored, only
       store the metadata. *)
    Cemented_block_store.cement_blocks_metadata cemented_store block_chunk
  else
    (* In case of blocks with expected block and operations metadata
       hash, we check if they are missing to, potentially, restore
       them. *)
    let is_valid level =
      let* o =
        Cemented_block_store.get_cemented_block_by_level
          ~read_metadata:false
          cemented_store
          level
      in
      match o with
      | None -> tzfail (Reconstruction_failure (Cannot_read_block_level level))
      | Some b -> (
          match
            ( Block_repr.block_metadata_hash b,
              Block_repr.operations_metadata_hashes b )
          with
          | Some _, Some _ -> return_true
          | _ -> return_false)
    in
    let* valid_lower_block = is_valid (Block_repr.level lower_block) in
    (* If the lower cycle bounds have the block and operations
       metadata hash stored, as expected, we only store the
       metadata. We check only the lower bound as the only case where
       the upper bound may differ is after a snapshot import. In this
       case, the lower bound is enough to determine the validity of
       the cycle as the lower cannot be valid while the upper is
       not. *)
    if valid_lower_block then
      Cemented_block_store.cement_blocks_metadata cemented_store block_chunk
    else
      (* Overwrite the existing cycle to restore the blocks and
         operations metadata hash and store the associated
         metadata. *)
      Cemented_block_store.(
        cement_blocks
          ~check_consistency:false
          cemented_store
          ~write_metadata:true
          (make_chunk_iterator block_chunk))

let gather_available_metadata chain_store ~start_level ~end_level =
  let open Lwt_result_syntax in
  let rec aux level acc =
    if level > end_level then return acc
    else
      let* block = Store.Block.read_block_by_level chain_store level in
      let* metadata = Store.Block.get_block_metadata chain_store block in
      let block_with_metadata =
        {(Store.Unsafe.repr_of_block block) with metadata = Some metadata}
      in
      aux (Int32.succ level) (block_with_metadata :: acc)
  in
  aux start_level []

(* Reconstruct the storage without checking if the context is already
   populated. We assume that committing an existing context is a
   nop. *)
let reconstruct_cemented chain_store context_index ~user_activated_upgrades
    ~user_activated_protocol_overrides ~operation_metadata_size_limit
    ~start_block_level ~progress_display_mode =
  let open Lwt_result_syntax in
  let block_store = Store.Unsafe.get_block_store chain_store in
  let cemented_block_store = Block_store.cemented_block_store block_store in
  let chain_dir = Store.Chain.chain_dir chain_store in
  let cemented_blocks_dir = Naming.cemented_blocks_dir chain_dir in
  let* cemented_cycles, start_cycle_index =
    let* o =
      Cemented_block_store.load_table cemented_blocks_dir
      (* Filter the cemented cycles to get the ones to reconstruct *)
    in
    match o with
    | None -> return ([], 0)
    | Some cycles ->
        let cycles_to_restore =
          List.filter
            (fun {Cemented_block_store.start_level; end_level; _} ->
              start_level >= start_block_level
              || start_block_level >= start_level
                 && start_block_level <= end_level)
            (Array.to_list cycles)
        in
        let first_cycle_index =
          Array.length cycles - List.length cycles_to_restore
        in
        return (cycles_to_restore, first_cycle_index)
  in
  Animation.display_progress
    ~pp_print_step:(fun ppf i ->
      Format.fprintf
        ppf
        "Reconstructing cemented blocks: %i/%d cycles rebuilt"
        (i + start_cycle_index)
        (List.length cemented_cycles + start_cycle_index))
    ~progress_display_mode
    (fun notify ->
      let rec aux = function
        | [] ->
            (* No cemented to reconstruct *)
            return_unit
        | ({Cemented_block_store.start_level; end_level; _} as file) :: tl -> (
            let* s = cemented_metadata_status cemented_block_store file in
            match s with
            | Complete ->
                (* Should not happen: we should have stopped or not started *)
                return_unit
            | Partial limit ->
                (* Reconstruct it partially and then stop *)
                (* As the block at level = limit contains metadata the
                   sub chunk stops before. Then, we gather the stored
                   metadata at limit (incl.). *)
                let* chunk =
                  reconstruct_chunk
                    chain_store
                    context_index
                    ~user_activated_upgrades
                    ~user_activated_protocol_overrides
                    ~operation_metadata_size_limit
                    ~start_level
                    ~end_level:Int32.(pred limit)
                in
                let* brs =
                  gather_available_metadata
                    chain_store
                    ~start_level:limit
                    ~end_level
                in
                let* available_metadata =
                  List.map_es
                    (fun br ->
                      let* (module Proto) =
                        protocol_of_protocol_level
                          chain_store
                          (Block_repr.proto_level br)
                          (Block_repr.hash br)
                      in
                      return (br, Proto.environment_version))
                    brs
                in
                let* () =
                  store_chunk
                    cemented_block_store
                    (List.append chunk available_metadata)
                in
                let*! () = notify () in
                return_unit
            | Not_stored ->
                (* Reconstruct it and continue *)
                let* chunk =
                  reconstruct_chunk
                    chain_store
                    context_index
                    ~user_activated_upgrades
                    ~user_activated_protocol_overrides
                    ~operation_metadata_size_limit
                    ~start_level
                    ~end_level
                in
                let* () = store_chunk cemented_block_store chunk in
                let*! () = notify () in
                aux tl)
      in
      aux cemented_cycles)

(* Tries to read the resulting context hash of a block in the cemented
   block store first, then fallback in the floating.
   This is necessary, while reconstructing the storage, as the blocks
   are reconstructed from the genesis and as the max_op_ttl blocks
   from the floating store are overlapping the cemented store. Thus,
   these blocks will have their metadata freshly reconstructed but
   their resulting context hashes might be wrong (as the context_hash
   was set to Context.zero after a snapshot imported). *)
let local_resulting_context_hash chain_store block =
  let open Lwt_result_syntax in
  let block_store = Store.Unsafe.get_block_store chain_store in
  let cemented_store = Block_store.cemented_block_store block_store in
  let* expect_predecessor =
    Store.Chain.expect_predecessor_context_hash
      chain_store
      ~protocol_level:(Block_repr.proto_level block)
  in
  let get_succ_from_floating block_store block =
    let floating_block_stores = Block_store.floating_block_stores block_store in
    let block_hash = Block_repr.hash block in
    let exception Found of Block_hash.t in
    let* succ_hash =
      List.find_map_es
        (fun fs ->
          protect
            (fun () ->
              let* () =
                Floating_block_store.raw_iterate
                  (fun (bytes, _offset) ->
                    let pred_hash =
                      Block_repr_unix.raw_get_block_predecessor bytes
                    in
                    if Block_hash.equal pred_hash block_hash then
                      raise (Found (Block_repr_unix.raw_get_block_hash bytes))
                    else return_unit)
                  fs
              in
              return_none)
            ~on_error:(function
              | Exn (Found h) :: _ -> return_some h | exn -> fail exn))
        floating_block_stores
    in
    match succ_hash with Some h -> return_some h | None -> return_none
  in
  let ( let*? ) t k =
    let* v_opt = t in
    match v_opt with None -> return_none | Some v -> k v
  in
  let*? adjusted_hash =
    Block_store.get_hash block_store (Block (Block_repr.hash block, 0))
  in
  if expect_predecessor then
    let*? block_level =
      return
        (Cemented_block_store.get_cemented_block_level
           cemented_store
           adjusted_hash)
    in
    let* succ_block =
      Cemented_block_store.get_cemented_block_by_level
        cemented_store
        ~read_metadata:false
        (Int32.succ block_level)
    in
    match succ_block with
    | Some succ_block -> return_some (Block_repr.context succ_block)
    | None ->
        (* We have reached the last cemented block. Falling back to
           the floating store. *)
        let*? succ_block_hash = get_succ_from_floating block_store block in
        let* succ_block = Store.Block.read_block chain_store succ_block_hash in
        return_some (Store.Block.context_hash succ_block)
  else
    let*? block =
      Cemented_block_store.get_cemented_block_by_hash
        cemented_store
        ~read_metadata:false
        adjusted_hash
    in
    return_some (Block_repr.context block)

let reconstruct_floating chain_store context_index ~user_activated_upgrades
    ~user_activated_protocol_overrides ~operation_metadata_size_limit
    ~progress_display_mode =
  let open Lwt_result_syntax in
  let chain_id = Store.Chain.chain_id chain_store in
  let chain_dir = Store.Chain.chain_dir chain_store in
  let block_store = Store.Unsafe.get_block_store chain_store in
  let cemented_block_store = Block_store.cemented_block_store block_store in
  let*! new_ro_store =
    Floating_block_store.init chain_dir ~readonly:false RO_TMP
  in
  let floating_stores = Block_store.floating_block_stores block_store in
  let* () =
    Animation.display_progress
      ~pp_print_step:(fun ppf i ->
        Format.fprintf ppf "Reconstructing floating blocks: %i" i)
      ~progress_display_mode
      (fun notify ->
        List.iter_es
          (fun fs ->
            let* () =
              Floating_block_store.iter_with_info_s
                (fun (block, info) ->
                  let level = Block_repr.level block in
                  let* (module Proto) =
                    protocol_of_protocol_level
                      chain_store
                      (Block_repr.proto_level block)
                      (Block_repr.hash block)
                  in
                  let* ( resulting_context_hash,
                         message,
                         max_operations_ttl,
                         last_preserved_block_level,
                         block_metadata,
                         operations_metadata ) =
                    if
                      (* If the block is genesis then just retrieve
                         its metadata. *)
                      Store.Block.is_genesis chain_store (Block_repr.hash block)
                    then reconstruct_genesis_operations_metadata chain_store
                    else
                      (* It is needed to read the metadata using the
                         cemented_block_store to avoid the cache mechanism which
                         stores blocks without metadata *)
                      let* o =
                        Cemented_block_store.read_block_metadata
                          (Block_store.cemented_block_store block_store)
                          level
                      in
                      match o with
                      | None ->
                          (* When the metadata is not available in the
                             cemented_block_store, it means that the block (in
                             the floating store) was not cemented yet. It is
                             thus needed to recompute its metadata + context
                          *)
                          let block = Store.Unsafe.block_of_repr block in
                          let predecessor_hash =
                            Store.Block.predecessor block
                          in
                          (* We try to read the predecessor in the floating
                             store as a floating store invariant assumes
                             that the predecessor of a block is always
                             stored before. In that case, by the definition
                             of [iter], the predecessor will be available
                             in the [new_ro_store], as already processed. *)
                          let* predecessor_block =
                            let*! o =
                              Floating_block_store.read_block
                                new_ro_store
                                predecessor_hash
                            in
                            match o with
                            | Some pb -> return (Store.Unsafe.block_of_repr pb)
                            | None -> (
                                (* If the predecessor was already cemented,
                                   read it in the cemented store. It is
                                   assumed to be valid as the cemented store
                                   was restored previously.*)
                                let* o =
                                  Cemented_block_store
                                  .get_cemented_block_by_hash
                                    ~read_metadata:true
                                    cemented_block_store
                                    predecessor_hash
                                in
                                match o with
                                | None ->
                                    tzfail
                                      (Reconstruction_failure
                                         (Cannot_read_block_hash
                                            predecessor_hash))
                                | Some b ->
                                    return (Store.Unsafe.block_of_repr b))
                          in
                          let* res =
                            apply_context
                              context_index
                              chain_id
                              ~user_activated_upgrades
                              ~user_activated_protocol_overrides
                              ~operation_metadata_size_limit
                              ~predecessor_block_metadata_hash:
                                (Store.Block.block_metadata_hash
                                   predecessor_block)
                              ~predecessor_ops_metadata_hash:
                                (Store.Block.all_operations_metadata_hash
                                   predecessor_block)
                              ~predecessor_block
                              ~expected_context_hash:Proto.expected_context_hash
                              block
                          in
                          let*! () =
                            Event.(emit reconstruct_block_success)
                              (Store.Block.descriptor block)
                          in
                          return res
                      | Some
                          {
                            message;
                            max_operations_ttl;
                            last_preserved_block_level;
                            block_metadata;
                            operations_metadata;
                          } ->
                          let operations_metadata =
                            match
                              Block_repr.operations_metadata_hashes block
                            with
                            | Some v ->
                                let operations_metadata =
                                  WithExceptions.List.map2
                                    ~loc:__LOC__
                                    (WithExceptions.List.combine ~loc:__LOC__)
                                    operations_metadata
                                    v
                                in
                                Block_validation.Metadata_hash
                                  operations_metadata
                            | None -> No_metadata_hash operations_metadata
                          in
                          let* resulting_context_hash =
                            let* opt =
                              local_resulting_context_hash chain_store block
                            in
                            match opt with
                            | None ->
                                tzfail
                                  (Reconstruction_failure
                                     (Cannot_read_block_level level))
                            | Some v -> return v
                          in
                          return
                            ( resulting_context_hash,
                              message,
                              max_operations_ttl,
                              last_preserved_block_level,
                              block_metadata,
                              operations_metadata )
                  in
                  let reconstructed_block =
                    restore_block_contents
                      chain_store
                      Proto.environment_version
                      ~block_metadata
                      ~operations_metadata
                      message
                      max_operations_ttl
                      last_preserved_block_level
                      block
                  in
                  let* () =
                    Floating_block_store.append_block
                      new_ro_store
                      {info with resulting_context_hash}
                      reconstructed_block
                  in
                  let*! () = notify () in
                  return_unit)
                fs
            in
            return_unit)
          floating_stores)
  in
  let* () =
    Block_store.move_floating_store
      block_store
      ~src:new_ro_store
      ~dst_kind:Floating_block_store.RO
  in
  (* Reset the RW to an empty floating_block_store *)
  let*! empty_rw = Floating_block_store.init chain_dir ~readonly:false RW_TMP in
  Block_store.move_floating_store
    block_store
    ~src:empty_rw
    ~dst_kind:Floating_block_store.RW

(* Only Full modes with any offset can be reconstructed *)
let check_history_mode_compatibility chain_store savepoint genesis_block =
  let open Lwt_result_syntax in
  match Store.Chain.history_mode chain_store with
  | History_mode.(Full _) ->
      fail_when
        (snd savepoint = Store.Block.level genesis_block)
        (Reconstruction_failure Nothing_to_reconstruct)
  | _ as history_mode -> tzfail (Cannot_reconstruct history_mode)

let restore_constants chain_store genesis_block head_lpbl_block
    ~cementing_highwatermark =
  let open Lwt_result_syntax in
  (* The checkpoint is updated to the last preserved block level of
     the current head if higher than the cementing
     highwatermark. Otherwise, the checkpoint is assumed to be the
     cementing highwatermark (this may occur after a snapshot
     import). Thus, we ensure that the store invariant
     `cementing_highwatermark <= checkpoint` is maintained. *)
  let head_lpbl_descr = Store.Block.descriptor head_lpbl_block in
  let checkpoint =
    match cementing_highwatermark with
    | None -> head_lpbl_descr
    | Some chw ->
        if snd chw > Store.Block.level head_lpbl_block then chw
        else head_lpbl_descr
  in
  let* () = Store.Unsafe.set_checkpoint chain_store checkpoint in
  let* () = Store.Unsafe.set_history_mode chain_store History_mode.Archive in
  let genesis = Store.Block.descriptor genesis_block in
  let* () = Store.Unsafe.set_savepoint chain_store genesis in
  Store.Unsafe.set_caboose chain_store genesis

(* Computes at which level the reconstruction should start. If a
   previous reconstruction is left unfinished, the procedure will restart
   at the lowest non cemented cycle. Otherwise, the reconstruction starts
   at the genesis. *)
let compute_start_level chain_store savepoint =
  let open Lwt_result_syntax in
  let chain_dir = Store.Chain.chain_dir chain_store in
  let reconstruct_lockfile = Naming.reconstruction_lock_file chain_dir in
  let reconstruct_lockfile_path = Naming.file_path reconstruct_lockfile in
  if Sys.file_exists reconstruct_lockfile_path then
    let cemented_blocks_dir = Naming.cemented_blocks_dir chain_dir in
    let* o = Cemented_block_store.load_table cemented_blocks_dir in
    match o with
    | None -> return 0l
    | Some l ->
        let rec aux level = function
          | [] -> return level
          | {Cemented_block_store.start_level; file; _} :: tl ->
              let metadata_file =
                Naming.cemented_blocks_metadata_file
                  (Naming.cemented_blocks_metadata_dir cemented_blocks_dir)
                  file
              in
              if Sys.file_exists (Naming.file_path metadata_file) then
                aux start_level tl
              else return start_level
        in
        let* start_block_level = aux 0l (Array.to_list l) in
        let* start_block =
          Store.Block.read_block_by_level chain_store start_block_level
        in
        let*! () =
          Event.(
            emit
              reconstruct_resuming
              (Store.Block.descriptor start_block, savepoint))
        in
        return start_block_level
  else
    let*! () = Event.(emit reconstruct_start_default savepoint) in
    return 0l

(* [locked chain_dir f] locks the [chain_dir] while [f] is
   executing. The aim of this lock is to:
   - avoid the node to be run while the storage reconstruction is
     running,
   - leave the lock file if the reconstruction is interrupted (by any
     exception or if cancelled) acknowledge that a reconstruction must
     be resumed. *)
let locked chain_dir f =
  let open Lwt_result_syntax in
  let reconstruct_lockfile_path =
    Naming.reconstruction_lock_file chain_dir |> Naming.file_path
  in
  let*! file =
    Lwt_unix.openfile
      reconstruct_lockfile_path
      [Unix.O_CREAT; O_RDWR; O_CLOEXEC; O_SYNC]
      0o644
  in
  let*! () = Lwt_unix.close file in
  let* res = f () in
  let*! () = Lwt_unix.unlink reconstruct_lockfile_path in
  return res

(* We must ensure that the context of the genesis is committed in the
   context before starting the reconstruction. It could have been
   removed by a context's GC. *)
let may_commit_genesis chain_store context_index genesis =
  let open Lwt_result_syntax in
  let chain_id = Store.Chain.chain_id chain_store in
  let* genesis_block =
    Store.Block.read_block chain_store genesis.Genesis.block
  in
  let*! genesis_context = Store.Block.context_opt chain_store genesis_block in
  match genesis_context with
  | Some _ctxt -> return_unit
  | None ->
      let* _ctxt_hash =
        Context_ops.commit_genesis
          context_index
          ~chain_id
          ~time:genesis.time
          ~protocol:genesis.protocol
      in
      return_unit

let reconstruct ?patch_context ~store_dir ~context_dir genesis
    ~user_activated_upgrades ~user_activated_protocol_overrides
    ~operation_metadata_size_limit ~progress_display_mode =
  let open Lwt_result_syntax in
  (* We need to inhibit the cache to avoid hitting the cache with
     already loaded blocks with missing metadata. *)
  let* store =
    Store.init
      ~block_cache_limit:1
      ?patch_context
      ~store_dir
      ~context_dir
      ~allow_testchains:false
      genesis
  in
  protect
    ~on_error:(fun err ->
      let*! () = Store.close_store store in
      Lwt.return (Error err))
    (fun () ->
      let context_index = Store.context_index store in
      let chain_store = Store.main_chain_store store in
      let* () = may_commit_genesis chain_store context_index genesis in
      let*! genesis_block = Store.Chain.genesis_block chain_store in
      let*! savepoint = Store.Chain.savepoint chain_store in
      let* () =
        check_history_mode_compatibility chain_store savepoint genesis_block
      in
      let* start_block_level = compute_start_level chain_store savepoint in
      let*! () = Event.(emit reconstruct_enum ()) in
      let*! current_head = Store.Chain.current_head chain_store in
      let* head_metadata =
        Store.Block.get_block_metadata chain_store current_head
      in
      let* head_lpbl_block =
        Store.Block.read_block_by_level
          chain_store
          (Store.Block.last_preserved_block_level head_metadata)
      in
      let* cementing_highwatermark_data =
        Stored_data.load
          (Naming.cementing_highwatermark_file
             (Store.Chain.chain_dir chain_store))
      in
      let* cementing_highwatermark =
        let*! o = Stored_data.get cementing_highwatermark_data in
        match o with
        | None -> return_none
        | Some chw ->
            let* b = Store.Block.read_block_by_level chain_store chw in
            let d = Store.Block.descriptor b in
            return_some d
      in
      let chain_dir = Store.Chain.chain_dir chain_store in
      let* () =
        locked chain_dir (fun () ->
            let* () =
              reconstruct_cemented
                chain_store
                context_index
                ~user_activated_upgrades
                ~user_activated_protocol_overrides
                ~operation_metadata_size_limit
                ~start_block_level
                ~progress_display_mode
            in
            let* () =
              reconstruct_floating
                chain_store
                context_index
                ~user_activated_upgrades
                ~user_activated_protocol_overrides
                ~operation_metadata_size_limit
                ~progress_display_mode
            in
            restore_constants
              chain_store
              genesis_block
              head_lpbl_block
              ~cementing_highwatermark)
      in
      (* TODO? add a global check *)
      let*! () = Event.(emit reconstruct_success ()) in
      let*! () = Store.close_store store in
      return_unit)
