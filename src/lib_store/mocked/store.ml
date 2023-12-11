(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Store_types
open Store_errors

module Shared = struct
  type 'a t = {mutable data : 'a; lock : Lwt_idle_waiter.t}

  let create data = {data; lock = Lwt_idle_waiter.create ()}

  let use shared f = Lwt_idle_waiter.task shared.lock (fun () -> f shared.data)

  (* Causes a deadlock if [use] or [update_with] is called inside [f] *)
  let locked_use shared f =
    Lwt_idle_waiter.force_idle shared.lock (fun () -> f shared.data)

  (* Updates the shared data [v] only when a new value is provided by
     the evaluation of [f]. Causes a deadlock if [use] or [locked_use]
     is called inside [f]. *)
  let update_with v f =
    let open Lwt_result_syntax in
    Lwt_idle_waiter.force_idle v.lock (fun () ->
        let* o_r = f v.data in
        match o_r with
        | Some new_data, res ->
            v.data <- new_data ;
            return res
        | None, res -> return res)
end

type store = {
  store_dir : [`Store_dir] Naming.directory;
  (* Mutability allows a back-reference from chain_store to store: not
     to be modified. *)
  (* Invariant : main_chain_store <> None *)
  mutable main_chain_store : chain_store option;
  context_index : Context_ops.index;
  protocol_store : Protocol_store.t;
  allow_testchains : bool;
  protocol_watcher : Protocol_hash.t Lwt_watcher.input;
  global_block_watcher : (chain_store * block) Lwt_watcher.input;
}

and chain_store = {
  global_store : store;
  chain_id : Chain_id.t;
  chain_dir : [`Chain_dir] Naming.directory;
  chain_config : chain_config;
  block_store : Block_store.t;
  chain_state : chain_state Shared.t;
  (* Genesis is only on-disk: read-only except at creation *)
  genesis_block_data : block Stored_data.t;
  block_watcher : block Lwt_watcher.input;
  validated_block_watcher : block Lwt_watcher.input;
  block_rpc_directories :
    (chain_store * block) Tezos_rpc.Directory.t Protocol_hash.Map.t
    Protocol_hash.Table.t;
}

and chain_state = {
  (* Following fields are not safe to update concurrently and must be
     manipulated carefuly: *)
  current_head_data : block_descriptor Stored_data.t;
  mutable last_finalized_block_level : Int32.t option;
  cementing_highwatermark_data : int32 option Stored_data.t;
  target_data : block_descriptor option Stored_data.t;
  checkpoint_data : block_descriptor Stored_data.t;
  (* Following fields are safe to update directly *)
  protocol_levels_data :
    Protocol_levels.protocol_info Protocol_levels.t Stored_data.t;
  invalid_blocks_data : invalid_block Block_hash.Map.t Stored_data.t;
  forked_chains_data : Block_hash.t Chain_id.Map.t Stored_data.t;
  (* In memory-only: *)
  current_head : Block_repr.t;
  active_testchain : testchain option;
  mempool : Mempool.t;
  live_blocks : Block_hash.Set.t;
  live_operations : Operation_hash.Set.t;
  mutable live_data_cache :
    (Block_hash.t * Operation_hash.Set.t) Ringo.Ring.t option;
  validated_blocks : Block_repr.t Block_lru_cache.t;
}

and testchain = {forked_block : Block_hash.t; testchain_store : chain_store}

and block = Block_repr.t

type t = store

let current_head chain_store =
  Shared.use chain_store.chain_state (fun {current_head; _} ->
      Lwt.return current_head)

let caboose chain_store = Block_store.caboose chain_store.block_store

let checkpoint chain_store =
  Shared.use chain_store.chain_state (fun {checkpoint_data; _} ->
      Stored_data.get checkpoint_data)

let target chain_store =
  Shared.use chain_store.chain_state (fun {target_data; _} ->
      Stored_data.get target_data)

let savepoint chain_store = Block_store.caboose chain_store.block_store

let genesis chain_store = chain_store.chain_config.genesis

let history_mode chain_store = chain_store.chain_config.history_mode

let read_ancestor_hash {block_store; _} ~distance hash =
  Block_store.get_hash block_store (Block (hash, distance))

(* Will that block be compatible with the current checkpoint and
   target. *)
let locked_is_acceptable_block chain_state (hash, level) =
  let open Lwt_syntax in
  let* _checkpoint_hash, checkpoint_level =
    Stored_data.get chain_state.checkpoint_data
  in
  (* The block must be above the checkpoint. *)
  if Compare.Int32.(checkpoint_level >= level) then Lwt.return_false
  else
    (* FIXME? should we read its predecessor at checkpoint level to
       see if it's the same? *)
    let* o = Stored_data.get chain_state.target_data in
    match o with
    | None -> Lwt.return_true
    | Some (target_hash, target_level) ->
        if Compare.Int32.(level = target_level) then
          Lwt.return @@ Block_hash.equal hash target_hash
        else Lwt.return_true

let find_protocol_info chain_store ~protocol_level =
  let open Lwt_syntax in
  Shared.use chain_store.chain_state (fun {protocol_levels_data; _} ->
      let* protocol_levels = Stored_data.get protocol_levels_data in
      return (Protocol_levels.find protocol_level protocol_levels))

let expect_predecessor_context_hash_exn chain_store protocol_level =
  let open Lwt_syntax in
  let* protocol_info = find_protocol_info chain_store ~protocol_level in
  match protocol_info with
  | Some {expect_predecessor_context; _} -> return expect_predecessor_context
  | None ->
      Format.ksprintf
        Stdlib.failwith
        "cannot find protocol info for level: %d"
        protocol_level

let expect_predecessor_context_hash chain_store ~protocol_level =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! b =
        expect_predecessor_context_hash_exn chain_store protocol_level
      in
      return b)
    (fun _ -> tzfail (Protocol_not_found {protocol_level}))

module Block = struct
  type nonrec block = block

  type t = block

  type metadata = Block_repr.metadata = {
    message : string option;
    max_operations_ttl : int;
    last_preserved_block_level : Int32.t;
    block_metadata : Bytes.t;
    operations_metadata : Block_validation.operation_metadata list list;
  }

  let equal b b' = Block_hash.equal (Block_repr.hash b) (Block_repr.hash b')

  let descriptor blk = Block_repr.descriptor blk

  (* I/O operations *)

  let is_known_valid {block_store; _} hash =
    let open Lwt_syntax in
    let* r = Block_store.(mem block_store (Block (hash, 0))) in
    match r with
    | Ok k -> Lwt.return k
    | Error _ ->
        (* should never happen : (0 \in N) *)
        Lwt.return_false

  let locked_is_known_invalid chain_state hash =
    let open Lwt_syntax in
    let* invalid_blocks = Stored_data.get chain_state.invalid_blocks_data in
    Lwt.return (Block_hash.Map.mem hash invalid_blocks)

  let is_known_invalid {chain_state; _} hash =
    Shared.use chain_state (fun chain_state ->
        locked_is_known_invalid chain_state hash)

  let is_known_validated {chain_state; _} hash =
    Shared.use chain_state (fun {validated_blocks; _} ->
        Option.value ~default:Lwt.return_false
        @@ Block_lru_cache.bind validated_blocks hash (function
               | None -> Lwt.return_false
               | Some _ -> Lwt.return_true))

  let is_known chain_store hash =
    let open Lwt_syntax in
    let* is_known = is_known_valid chain_store hash in
    if is_known then Lwt.return_true else is_known_invalid chain_store hash

  let validity chain_store hash =
    let open Lwt_syntax in
    let* b = is_known chain_store hash in
    match b with
    | false -> Lwt.return Block_locator.Unknown
    | true -> (
        let* b = is_known_invalid chain_store hash in
        match b with
        | true -> Lwt.return Block_locator.Known_invalid
        | false -> Lwt.return Block_locator.Known_valid)

  let is_genesis chain_store hash =
    let genesis = genesis chain_store in
    Block_hash.equal hash genesis.Genesis.block

  let read_block {block_store; _} ?(distance = 0) hash =
    let open Lwt_result_syntax in
    let* o =
      Block_store.read_block
        ~read_metadata:false
        block_store
        (Block (hash, distance))
    in
    match o with
    | None -> tzfail @@ Block_not_found {hash; distance}
    | Some block -> return block

  let read_block_metadata ?(distance = 0) chain_store hash =
    Block_store.read_block_metadata
      chain_store.block_store
      (Block (hash, distance))

  let read_block_metadata_opt ?distance chain_store hash =
    let open Lwt_syntax in
    let* r = read_block_metadata ?distance chain_store hash in
    match r with Ok v -> Lwt.return v | Error _ -> Lwt.return_none

  let get_block_metadata_opt chain_store block =
    let open Lwt_syntax in
    match Block_repr.metadata block with
    | Some metadata -> Lwt.return_some metadata
    | None -> (
        let* o = read_block_metadata_opt chain_store block.hash in
        match o with
        | Some metadata ->
            (* Put the metadata in cache *)
            block.metadata <- Some metadata ;
            Lwt.return_some metadata
        | None -> Lwt.return_none)

  let get_block_metadata chain_store block =
    let open Lwt_result_syntax in
    let*! o = get_block_metadata_opt chain_store block in
    match o with
    | Some metadata -> return metadata
    | None -> tzfail (Block_metadata_not_found (Block_repr.hash block))

  let read_block_opt chain_store ?(distance = 0) hash =
    let open Lwt_syntax in
    let* r = read_block chain_store ~distance hash in
    match r with
    | Ok block -> Lwt.return_some block
    | Error _ -> Lwt.return_none

  let read_predecessor chain_store block =
    read_block chain_store (Block_repr.predecessor block)

  let read_predecessor_opt chain_store block =
    let open Lwt_syntax in
    let* r = read_predecessor chain_store block in
    match r with
    | Ok block -> Lwt.return_some block
    | Error _ -> Lwt.return_none

  let read_ancestor_hash chain_store ~distance hash =
    read_ancestor_hash chain_store ~distance hash

  let read_ancestor_hash_opt chain_store ~distance hash =
    let open Lwt_syntax in
    let* r = read_ancestor_hash chain_store ~distance hash in
    match r with Ok v -> Lwt.return v | Error _ -> Lwt.return_none

  let read_predecessor_of_hash_opt chain_store hash =
    let open Lwt_syntax in
    let* o = read_ancestor_hash_opt chain_store ~distance:1 hash in
    match o with
    | Some hash -> read_block_opt chain_store hash
    | None -> Lwt.return_none

  let read_predecessor_of_hash chain_store hash =
    let open Lwt_result_syntax in
    let*! o = read_predecessor_of_hash_opt chain_store hash in
    match o with
    | Some b -> return b
    | None -> tzfail @@ Block_not_found {hash; distance = 0}

  let locked_read_block_by_level chain_store head level =
    let open Lwt_result_syntax in
    let distance = Int32.(to_int (sub (Block_repr.level head) level)) in
    if distance < 0 then
      tzfail
        (Bad_level
           {
             head_level = Block_repr.level head;
             given_level = Int32.of_int distance;
           })
    else read_block chain_store ~distance (Block_repr.hash head)

  let locked_read_block_by_level_opt chain_store head level =
    let open Lwt_syntax in
    let* r = locked_read_block_by_level chain_store head level in
    match r with Error _ -> Lwt.return_none | Ok b -> Lwt.return_some b

  let read_block_by_level chain_store level =
    let open Lwt_syntax in
    let* current_head = current_head chain_store in
    locked_read_block_by_level chain_store current_head level

  let read_block_by_level_opt chain_store level =
    let open Lwt_syntax in
    let* current_head = current_head chain_store in
    locked_read_block_by_level_opt chain_store current_head level

  let read_validated_block_opt {chain_state; _} hash =
    Shared.use chain_state (fun {validated_blocks; _} ->
        Option.value ~default:Lwt.return_none
        @@ Block_lru_cache.bind validated_blocks hash Lwt.return)

  let read_validated_block chain_store hash =
    let open Lwt_result_syntax in
    let*! o = read_validated_block_opt chain_store hash in
    match o with
    | Some b -> return b
    | None -> tzfail (Block_not_found {hash; distance = 0})

  let check_metadata_list ~block_hash ~operations ~ops_metadata =
    fail_unless
      (List.for_all2
         ~when_different_lengths:(`X "unreachable")
         (fun l1 l2 -> Compare.List_lengths.(l1 = l2))
         operations
         ops_metadata
       |> function
       | Ok b -> b
       | _ -> assert false)
      (let to_string l =
         Format.asprintf
           "[%a]"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
              (fun ppf l -> Format.fprintf ppf "[%d]" (List.length l)))
           l
       in
       Cannot_store_block
         ( block_hash,
           Inconsistent_operations_lengths
             {
               operations_lengths = to_string operations;
               operations_data_lengths = to_string ops_metadata;
             } ))

  let store_block chain_store ~block_header ~operations validation_result =
    let open Lwt_result_syntax in
    let {
      Block_validation.validation_store =
        {
          resulting_context_hash;
          timestamp = _;
          message;
          max_operations_ttl;
          last_preserved_block_level;
          last_finalized_block_level;
        };
      block_metadata;
      ops_metadata;
      shell_header_hash = _;
    } =
      validation_result
    in
    let bytes = Block_header.to_bytes block_header in
    let hash = Block_header.hash_raw bytes in
    let operations_length = List.length operations in
    let operation_metadata_length =
      match ops_metadata with
      | Block_validation.No_metadata_hash x -> List.length x
      | Block_validation.Metadata_hash x -> List.length x
    in
    let validation_passes = block_header.shell.validation_passes in
    let* () =
      fail_unless
        (validation_passes = operations_length)
        (Cannot_store_block
           ( hash,
             Invalid_operations_length
               {validation_passes; operations = operations_length} ))
    in
    let* () =
      fail_unless
        (validation_passes = operation_metadata_length)
        (Cannot_store_block
           ( hash,
             Invalid_operations_length
               {validation_passes; operations = operation_metadata_length} ))
    in
    let* () =
      match ops_metadata with
      | No_metadata_hash ops_metadata ->
          check_metadata_list ~block_hash:hash ~operations ~ops_metadata
      | Metadata_hash ops_metadata ->
          check_metadata_list ~block_hash:hash ~operations ~ops_metadata
    in
    let*! genesis_block = Stored_data.get chain_store.genesis_block_data in
    let is_main_chain =
      Chain_id.equal
        chain_store.chain_id
        (WithExceptions.Option.get
           ~loc:__LOC__
           chain_store.global_store.main_chain_store)
          .chain_id
    in
    let genesis_level = Block_repr.level genesis_block in
    let* last_preserved_block_level =
      if is_main_chain then
        let* () =
          fail_unless
            Compare.Int32.(last_preserved_block_level >= genesis_level)
            (Cannot_store_block
               ( hash,
                 Invalid_last_preserved_block_level
                   {last_preserved_block_level; genesis_level} ))
        in
        return last_preserved_block_level
      else if Compare.Int32.(last_preserved_block_level < genesis_level) then
        (* Hack: on the testchain, the block's lpbl depends on the
           lpbl and is not max(genesis_level, expected_lpbl) *)
        return genesis_level
      else return last_preserved_block_level
    in
    let*! b = is_known_valid chain_store hash in
    match b with
    | true -> return_none
    | false ->
        (* Safety check: never ever commit a block that is not
           compatible with the current checkpoint/target. *)
        let*! acceptable_block, known_invalid =
          Shared.use chain_store.chain_state (fun chain_state ->
              let*! acceptable_block =
                locked_is_acceptable_block
                  chain_state
                  (hash, block_header.shell.level)
              in
              let*! known_invalid = locked_is_known_invalid chain_state hash in
              Lwt.return (acceptable_block, known_invalid))
        in
        let* () =
          fail_unless
            acceptable_block
            (Validation_errors.Checkpoint_error (hash, None))
        in
        let* () =
          fail_when
            known_invalid
            Store_errors.(Cannot_store_block (hash, Invalid_block))
        in
        let contents =
          {
            Block_repr.header = block_header;
            operations;
            block_metadata_hash = snd block_metadata;
            operations_metadata_hashes =
              (match ops_metadata with
              | Block_validation.No_metadata_hash _ -> None
              | Block_validation.Metadata_hash ops_metadata ->
                  Some (List.map (List.map snd) ops_metadata));
          }
        in
        let metadata =
          Some
            {
              message;
              max_operations_ttl;
              last_preserved_block_level;
              block_metadata = fst block_metadata;
              operations_metadata =
                (match ops_metadata with
                | Block_validation.No_metadata_hash ops_metadata -> ops_metadata
                | Block_validation.Metadata_hash ops_metadata ->
                    List.map (List.map fst) ops_metadata);
            }
        in
        let block = {Block_repr.hash; contents; metadata} in
        let* () =
          Block_store.store_block
            chain_store.block_store
            block
            resulting_context_hash
        in
        let*! () =
          Store_events.(emit store_block) (hash, block_header.shell.level)
        in
        let* () =
          Shared.update_with chain_store.chain_state (fun chain_state ->
              Block_lru_cache.remove chain_state.validated_blocks hash ;
              let new_last_finalized_block_level =
                match chain_state.last_finalized_block_level with
                | None -> Some last_finalized_block_level
                | Some prev_lfbl ->
                    Some (Int32.max last_finalized_block_level prev_lfbl)
              in
              let new_chain_state =
                {
                  chain_state with
                  last_finalized_block_level = new_last_finalized_block_level;
                }
              in
              return (Some new_chain_state, ()))
        in
        Lwt_watcher.notify chain_store.block_watcher block ;
        Lwt_watcher.notify
          chain_store.global_store.global_block_watcher
          (chain_store, block) ;
        return_some block

  let store_validated_block chain_store ~hash ~block_header ~operations =
    let open Lwt_result_syntax in
    let operations_length = List.length operations in
    let validation_passes = block_header.Block_header.shell.validation_passes in
    let* () =
      fail_unless
        (validation_passes = operations_length)
        (Cannot_store_block
           ( hash,
             Invalid_operations_length
               {validation_passes; operations = operations_length} ))
    in
    let block =
      {
        Block_repr.hash;
        contents =
          {
            header = block_header;
            operations;
            block_metadata_hash = None;
            operations_metadata_hashes = None;
          };
        metadata = None;
      }
    in
    let*! () =
      Shared.use chain_store.chain_state (fun {validated_blocks; _} ->
          Block_lru_cache.put validated_blocks hash (Lwt.return_some block) ;
          Lwt.return_unit)
    in
    let*! () =
      Store_events.(emit store_validated_block) (hash, block_header.shell.level)
    in
    return_unit

  let resulting_context_hash chain_store block =
    let open Lwt_result_syntax in
    let* expect_predecessor_context =
      expect_predecessor_context_hash
        chain_store
        ~protocol_level:(Block_repr.proto_level block)
    in
    let hash = Block_repr.hash block in
    let* resulting_context_hash_opt =
      Block_store.resulting_context_hash
        ~expect_predecessor_context
        chain_store.block_store
        (Block (hash, 0))
    in
    match resulting_context_hash_opt with
    | None ->
        tzfail
          (Resulting_context_hash_not_found
             {hash; level = Block_repr.level block})
    | Some resulting_context_hash -> return resulting_context_hash

  let context_exn chain_store block =
    let context_index = chain_store.global_store.context_index in
    Context_ops.checkout_exn context_index (Block_repr.context block)

  let context_opt chain_store block =
    let context_index = chain_store.global_store.context_index in
    Context_ops.checkout context_index (Block_repr.context block)

  let context chain_store block =
    let open Lwt_result_syntax in
    let*! o = context_opt chain_store block in
    match o with
    | Some context -> return context
    | None ->
        tzfail
          (Cannot_checkout_context
             (Block_repr.hash block, Block_repr.context block))

  let context_exists chain_store block =
    let context_index = chain_store.global_store.context_index in
    Context_ops.exists context_index (Block_repr.context block)

  let testchain_status chain_store block =
    let open Lwt_result_syntax in
    let* context =
      let*! o = context_opt chain_store block in
      match o with
      | Some ctxt -> return ctxt
      | None ->
          tzfail
            (Cannot_checkout_context
               (Block_repr.hash block, Block_repr.context block))
    in
    let*! status = Context_ops.get_test_chain context in
    match status with
    | Running _ ->
        Stdlib.failwith "testchain_status: running testchains not supported"
    | Forking _ -> return (status, Some (Block_repr.hash block))
    | Not_running -> return (status, None)

  let protocol_hash chain_store block =
    let open Lwt_result_syntax in
    Shared.use chain_store.chain_state (fun chain_state ->
        let*! protocol_levels =
          Stored_data.get chain_state.protocol_levels_data
        in
        let open Protocol_levels in
        let proto_level = Block_repr.proto_level block in
        match find proto_level protocol_levels with
        | Some {protocol; _} -> return protocol
        | None -> tzfail (Cannot_find_protocol proto_level))

  let protocol_hash_exn chain_store block =
    let open Lwt_syntax in
    let* r = protocol_hash chain_store block in
    match r with Ok ph -> Lwt.return ph | Error _ -> Lwt.fail Not_found

  (** Operations on invalid blocks *)

  let read_invalid_block_opt {chain_state; _} hash =
    let open Lwt_syntax in
    Shared.use chain_state (fun chain_state ->
        let* invalid_blocks = Stored_data.get chain_state.invalid_blocks_data in
        Lwt.return (Block_hash.Map.find hash invalid_blocks))

  let read_invalid_blocks {chain_state; _} =
    Shared.use chain_state (fun chain_state ->
        Stored_data.get chain_state.invalid_blocks_data)

  let mark_invalid chain_store hash ~level errors =
    let open Lwt_result_syntax in
    if is_genesis chain_store hash then tzfail Invalid_genesis_marking
    else
      let* () =
        Shared.use chain_store.chain_state (fun chain_state ->
            Stored_data.update_with
              chain_state.invalid_blocks_data
              (fun invalid_blocks ->
                Lwt.return
                  (Block_hash.Map.add hash {level; errors} invalid_blocks)))
      in
      return_unit

  let unmark_invalid {chain_state; _} hash =
    Shared.use chain_state (fun chain_state ->
        Stored_data.update_with
          chain_state.invalid_blocks_data
          (fun invalid_blocks ->
            Lwt.return (Block_hash.Map.remove hash invalid_blocks)))

  (** Accessors *)

  let hash blk = Block_repr.hash blk

  let header blk = Block_repr.header blk

  let operations blk = Block_repr.operations blk

  let shell_header blk = Block_repr.shell_header blk

  let level blk = Block_repr.level blk

  let proto_level blk = Block_repr.proto_level blk

  let predecessor blk = Block_repr.predecessor blk

  let timestamp blk = Block_repr.timestamp blk

  let operations_hash blk = Block_repr.operations_hash blk

  let validation_passes blk = Block_repr.validation_passes blk

  let fitness blk = Block_repr.fitness blk

  let context_hash blk = Block_repr.context blk

  let protocol_data blk = Block_repr.protocol_data blk

  let block_metadata_hash blk = Block_repr.block_metadata_hash blk

  let operations_metadata_hashes blk = Block_repr.operations_metadata_hashes blk

  let operations_metadata_hashes_path block i =
    if i < 0 || (header block).shell.validation_passes <= i then
      invalid_arg "operations_metadata_hashes_path" ;
    Option.map
      (fun ll -> List.nth ll i |> WithExceptions.Option.get ~loc:__LOC__)
      (Block_repr.operations_metadata_hashes block)

  let all_operations_metadata_hash blk =
    (* Special case: for genesis, do not commit operation metadatas *)
    if validation_passes blk = 0 then None
    else
      Option.map
        (fun ll ->
          Operation_metadata_list_list_hash.compute
            (List.map Operation_metadata_list_hash.compute ll))
        (Block_repr.operations_metadata_hashes blk)

  (** Metadata accessors *)

  let message metadata = Block_repr.message metadata

  let max_operations_ttl metadata = Block_repr.max_operations_ttl metadata

  let last_preserved_block_level metadata =
    Block_repr.last_preserved_block_level metadata

  let block_metadata metadata = Block_repr.block_metadata metadata

  let operations_metadata metadata = Block_repr.operations_metadata metadata

  let compute_operation_path hashes =
    let list_hashes = List.map Operation_list_hash.compute hashes in
    Operation_list_list_hash.compute_path list_hashes

  let operations_path block i =
    if i < 0 || validation_passes block <= i then invalid_arg "operations_path" ;
    let ops = operations block in
    let hashes = List.(map (map Operation.hash)) ops in
    let path = compute_operation_path hashes in
    (List.nth ops i |> WithExceptions.Option.get ~loc:__LOC__, path i)

  let operations_hashes_path block i =
    if i < 0 || (header block).shell.validation_passes <= i then
      invalid_arg "operations_hashes_path" ;
    let opss = operations block in
    let hashes = List.(map (map Operation.hash)) opss in
    let path = compute_operation_path hashes in
    (List.nth hashes i |> WithExceptions.Option.get ~loc:__LOC__, path i)

  let all_operation_hashes block =
    List.(map (map Operation.hash)) (operations block)
end

module Chain_traversal = struct
  let path chain_store ~from_block ~to_block =
    let open Lwt_syntax in
    if not Compare.Int32.(Block.level from_block <= Block.level to_block) then
      invalid_arg "Chain_traversal.path" ;
    let rec loop acc current =
      if Block.equal from_block current then Lwt.return_some acc
      else
        let* o = Block.read_predecessor_opt chain_store current in
        match o with
        | Some pred -> loop (current :: acc) pred
        | None -> Lwt.return_none
    in
    loop [] to_block

  let common_ancestor chain_store b1 b2 =
    let open Lwt_syntax in
    let rec loop b1 b2 =
      if Block.equal b1 b2 then Lwt.return_some b1
      else if Compare.Int32.(Block.level b1 <= Block.level b2) then
        let* o = Block.read_predecessor_opt chain_store b2 in
        match o with None -> Lwt.return_none | Some b2 -> loop b1 b2
      else
        let* o = Block.read_predecessor_opt chain_store b1 in
        match o with None -> Lwt.return_none | Some b1 -> loop b1 b2
    in
    loop b1 b2

  let new_blocks chain_store ~from_block ~to_block =
    let open Lwt_syntax in
    let* o = common_ancestor chain_store from_block to_block in
    match o with
    | None -> assert false
    | Some ancestor -> (
        let* o = path chain_store ~from_block:ancestor ~to_block in
        match o with
        | None -> Lwt.return (ancestor, [])
        | Some path -> Lwt.return (ancestor, path))

  let folder chain_store block n f init =
    let open Lwt_syntax in
    let rec loop acc block_head n =
      let hashes = Block.all_operation_hashes block_head in
      let acc = f acc (Block.hash block_head, hashes) in
      if n = 0 then Lwt.return acc
      else
        let* o = Block.read_predecessor_opt chain_store block_head in
        match o with
        | None -> Lwt.return acc
        | Some predecessor -> loop acc predecessor (pred n)
    in
    loop init block n

  let live_blocks chain_store block n =
    let fold (bacc, oacc) (head_hash, op_hashes) =
      let bacc = Block_hash.Set.add head_hash bacc in
      let oacc =
        List.fold_left
          (List.fold_left (fun oacc op -> Operation_hash.Set.add op oacc))
          oacc
          op_hashes
      in
      (bacc, oacc)
    in
    let init = (Block_hash.Set.empty, Operation_hash.Set.empty) in
    folder chain_store block n fold init

  let live_blocks_with_ring chain_store block n ring =
    let open Lwt_syntax in
    let fold acc (head_hash, op_hashes) =
      let op_hash_set = Operation_hash.Set.(of_list (List.flatten op_hashes)) in
      (head_hash, op_hash_set) :: acc
    in
    let* l = folder chain_store block n fold [] in
    (* Don't revert the list so we can add them in the correct order. *)
    Ringo.Ring.add_list ring l ;
    Lwt.return_unit
end

module Chain = struct
  type nonrec chain_store = chain_store

  type t = chain_store

  type nonrec testchain = testchain

  type block_identifier = Block_services.block

  let global_store {global_store; _} = global_store

  let chain_id chain_store = chain_store.chain_id

  let chain_dir chain_store = chain_store.chain_dir

  let history_mode chain_store = history_mode chain_store

  let genesis chain_store = genesis chain_store

  let genesis_block chain_store = Stored_data.get chain_store.genesis_block_data

  let expiration chain_store = chain_store.chain_config.expiration

  let checkpoint chain_store = checkpoint chain_store

  let target chain_store = target chain_store

  let savepoint chain_store = savepoint chain_store

  let caboose chain_store = caboose chain_store

  let current_head chain_store = current_head chain_store

  let mempool chain_store =
    Shared.use chain_store.chain_state (fun {mempool; _} -> Lwt.return mempool)

  let block_of_identifier chain_store =
    let open Lwt_result_syntax in
    let not_found () = fail_with_exn Not_found in
    function
    | `Genesis ->
        let*! block = genesis_block chain_store in
        return block
    | `Head n ->
        let*! current_head = current_head chain_store in
        if n < 0 then not_found ()
        else if n = 0 then return current_head
        else Block.read_block chain_store ~distance:n (Block.hash current_head)
    | (`Alias (_, n) | `Hash (_, n)) as b ->
        let*! hash =
          match b with
          | `Alias (`Checkpoint, _) ->
              let*! t = checkpoint chain_store in
              Lwt.return @@ fst t
          | `Alias (`Savepoint, _) ->
              let*! t = savepoint chain_store in
              Lwt.return @@ fst t
          | `Alias (`Caboose, _) ->
              let*! t = caboose chain_store in
              Lwt.return @@ fst t
          | `Hash (h, _) -> Lwt.return h
        in
        if n < 0 then
          let* block = Block.read_block chain_store hash in
          let*! current_head = current_head chain_store in
          let head_level = Block.level current_head in
          let block_level = Block.level block in
          let distance =
            Int32.(to_int (sub head_level (sub block_level (of_int n))))
          in
          if distance < 0 then not_found ()
          else Block.read_block chain_store ~distance (Block.hash current_head)
        else Block.read_block chain_store ~distance:n hash
    | `Level i ->
        if Compare.Int32.(i < 0l) then not_found ()
        else Block.read_block_by_level chain_store i

  let block_of_identifier_opt chain_store identifier =
    let open Lwt_syntax in
    let* r = block_of_identifier chain_store identifier in
    match r with
    | Ok block -> Lwt.return_some block
    | Error _ -> Lwt.return_none

  let set_mempool chain_store ~head mempool =
    let open Lwt_result_syntax in
    Shared.update_with chain_store.chain_state (fun chain_state ->
        let*! current_head_descr =
          Stored_data.get chain_state.current_head_data
        in
        if Block_hash.equal head (fst current_head_descr) then
          return (Some {chain_state with mempool}, ())
        else return (None, ()))

  let live_blocks chain_store =
    Shared.use chain_store.chain_state (fun {live_blocks; live_operations; _} ->
        Lwt.return (live_blocks, live_operations))

  let locked_compute_live_blocks ?(force = false) ?(update_cache = true)
      chain_store chain_state block metadata =
    let open Lwt_syntax in
    let {current_head; live_blocks; live_operations; live_data_cache; _} =
      chain_state
    in
    if Block.equal current_head block && not force then
      Lwt.return (live_blocks, live_operations)
    else
      (* We actually compute max_op_ttl + 1... *)
      let expected_capacity = Block.max_operations_ttl metadata + 1 in
      match live_data_cache with
      | Some live_data_cache
        when update_cache
             && Block_hash.equal
                  (Block.predecessor block)
                  (Block.hash current_head)
             && Ringo.Ring.capacity live_data_cache = expected_capacity -> (
          let most_recent_block = Block.hash block in
          let most_recent_ops =
            Block.all_operation_hashes block
            |> List.flatten |> Operation_hash.Set.of_list
          in
          let new_live_blocks =
            Block_hash.Set.add most_recent_block live_blocks
          in
          let new_live_operations =
            Operation_hash.Set.union most_recent_ops live_operations
          in
          match
            Ringo.Ring.add_and_return_erased
              live_data_cache
              (most_recent_block, most_recent_ops)
          with
          | None -> Lwt.return (new_live_blocks, new_live_operations)
          | Some (last_block, last_ops) ->
              let diffed_new_live_blocks =
                Block_hash.Set.remove last_block new_live_blocks
              in
              let diffed_new_live_operations =
                Operation_hash.Set.diff new_live_operations last_ops
              in
              Lwt.return (diffed_new_live_blocks, diffed_new_live_operations))
      | _ when update_cache ->
          let new_cache = Ringo.Ring.create expected_capacity in
          let* () =
            Chain_traversal.live_blocks_with_ring
              chain_store
              block
              expected_capacity
              new_cache
          in
          chain_state.live_data_cache <- Some new_cache ;
          let live_blocks, live_ops =
            Ringo.Ring.fold
              new_cache
              ~init:(Block_hash.Set.empty, Operation_hash.Set.empty)
              ~f:(fun (bhs, opss) (bh, ops) ->
                (Block_hash.Set.add bh bhs, Operation_hash.Set.union ops opss))
          in
          Lwt.return (live_blocks, live_ops)
      | _ -> Chain_traversal.live_blocks chain_store block expected_capacity

  let compute_live_blocks chain_store ~block =
    let open Lwt_result_syntax in
    Shared.use chain_store.chain_state (fun chain_state ->
        let* metadata = Block.get_block_metadata chain_store block in
        let*! r =
          locked_compute_live_blocks
            ~update_cache:false
            chain_store
            chain_state
            block
            metadata
        in
        return r)

  let is_ancestor chain_store ~head:(hash, lvl) ~ancestor:(hash', lvl') =
    let open Lwt_syntax in
    if Compare.Int32.(lvl' > lvl) then Lwt.return_false
    else if Compare.Int32.(lvl = lvl') then
      Lwt.return (Block_hash.equal hash hash')
    else
      let* o =
        Block.read_ancestor_hash_opt
          chain_store
          hash
          ~distance:Int32.(to_int (sub lvl lvl'))
      in
      match o with
      | None -> Lwt.return_false
      | Some hash_found -> Lwt.return (Block_hash.equal hash' hash_found)

  let is_in_chain chain_store (hash, level) =
    let open Lwt_syntax in
    let* current_head = current_head chain_store in
    is_ancestor
      chain_store
      ~head:Block.(hash current_head, level current_head)
      ~ancestor:(hash, level)

  (* FIXME: this should not be hard-coded *)
  let max_locator_size = 200

  let compute_locator_from_hash chain_store ?(max_size = max_locator_size)
      ?min_level (head_hash, head_header) seed =
    let open Lwt_syntax in
    let* caboose, _ =
      Shared.use chain_store.chain_state (fun chain_state ->
          match min_level with
          | None -> Block_store.caboose chain_store.block_store
          | Some min_level -> (
              let* o =
                Block.locked_read_block_by_level_opt
                  chain_store
                  chain_state.current_head
                  min_level
              in
              match o with
              | None ->
                  (* should not happen *)
                  Block_store.caboose chain_store.block_store
              | Some b -> Lwt.return (Block_repr.descriptor b)))
    in
    let get_predecessor =
      match min_level with
      | None ->
          fun h n -> Block.read_ancestor_hash_opt chain_store h ~distance:n
      | Some min_level -> (
          fun h n ->
            let* o = Block.read_block_opt chain_store h ~distance:n in
            match o with
            | None -> Lwt.return_none
            | Some pred ->
                if Compare.Int32.(Block_repr.level pred < min_level) then
                  Lwt.return_none
                else Lwt.return_some (Block_repr.hash pred))
    in
    Block_locator.compute
      ~get_predecessor
      ~caboose
      ~size:max_size
      head_hash
      head_header
      seed

  let compute_locator chain_store ?(max_size = 200) head seed =
    let open Lwt_syntax in
    let* caboose, _caboose_level = caboose chain_store in
    Block_locator.compute
      ~get_predecessor:(fun h n ->
        Block.read_ancestor_hash_opt chain_store h ~distance:n)
      ~caboose
      ~size:max_size
      head.Block_repr.hash
      head.Block_repr.contents.header
      seed

  let compute_protocol_locator chain_store ?max_size ~proto_level seed =
    let open Lwt_syntax in
    let* o =
      Shared.use chain_store.chain_state (fun chain_state ->
          let* protocol_levels =
            Stored_data.get chain_state.protocol_levels_data
          in
          match Protocol_levels.find proto_level protocol_levels with
          | None -> Lwt.return_none
          | Some {activation_block; _} -> (
              let block_activation_level = snd activation_block in
              (* proto level's lower bound found, now retrieving the upper bound *)
              let head_proto_level =
                Block_repr.proto_level chain_state.current_head
              in
              if Compare.Int.(proto_level = head_proto_level) then
                Lwt.return_some
                  ( block_activation_level,
                    Block_repr.
                      ( hash chain_state.current_head,
                        header chain_state.current_head ) )
              else
                match
                  Protocol_levels.find (succ proto_level) protocol_levels
                with
                | None -> Lwt.return_none
                | Some {activation_block; _} -> (
                    let next_activation_level = snd activation_block in
                    let last_level_in_protocol =
                      Int32.(pred next_activation_level)
                    in
                    let* o =
                      Block.locked_read_block_by_level_opt
                        chain_store
                        chain_state.current_head
                        last_level_in_protocol
                    in
                    match o with
                    | None -> Lwt.return_none
                    | Some pred ->
                        Lwt.return_some
                          ( block_activation_level,
                            Block_repr.(hash pred, header pred) ))))
    in
    match o with
    | None -> Lwt.return_none
    | Some (block_activation_level, upper_block) ->
        let* l =
          compute_locator_from_hash
            chain_store
            ?max_size
            ~min_level:block_activation_level
            upper_block
            seed
        in
        Lwt.return_some l

  let may_update_checkpoint_and_target chain_store ~new_head ~new_head_lfbl
      ~checkpoint ~target =
    let open Lwt_result_syntax in
    let new_checkpoint =
      (* Ensure that the checkpoint is not above the current head *)
      if Compare.Int32.(snd new_head_lfbl > snd checkpoint) then
        if Compare.Int32.(snd new_head_lfbl > snd new_head) then new_head
        else new_head_lfbl
      else checkpoint
    in
    match target with
    | None -> return (new_checkpoint, None)
    | Some target ->
        if Compare.Int32.(snd target < snd new_checkpoint) then assert false
        else if Compare.Int32.(snd target <= snd new_head) then
          let*! b = is_ancestor chain_store ~head:new_head ~ancestor:target in
          match b with
          | true -> return (new_checkpoint, None)
          | false ->
              (* Impossible: a block is not acceptable to be stored if
                 it's not compatible with the target *)
              tzfail Target_mismatch
        else return (new_checkpoint, Some target)

  let write_checkpoint chain_state new_checkpoint =
    let open Lwt_result_syntax in
    let* () = Stored_data.write chain_state.checkpoint_data new_checkpoint in
    let*! () = Store_events.(emit set_checkpoint) new_checkpoint in
    return_unit

  let set_head chain_store new_head =
    let open Lwt_result_syntax in
    Shared.update_with chain_store.chain_state (fun chain_state ->
        (* The merge cannot finish until we release the lock on the
           chain state so its status cannot change while this
           function is executed. *)
        (* Also check the status to be extra-safe *)
        let previous_head = chain_state.current_head in
        let*! checkpoint = Stored_data.get chain_state.checkpoint_data in
        let new_head_descr = Block.descriptor new_head in
        (* Check that the new_head is consistent with the checkpoint *)
        let* () =
          fail_unless
            Compare.Int32.(Block.level new_head >= snd checkpoint)
            (Invalid_head_switch
               {checkpoint_level = snd checkpoint; given_head = new_head_descr})
        in
        (* Check that its predecessor exists and has metadata *)
        let predecessor = Block.predecessor new_head in
        let* new_head_metadata =
          trace
            Bad_head_invariant
            (let* pred_block = Block.read_block chain_store predecessor in
             (* check that prededecessor's block metadata are available *)
             let* _pred_head_metadata =
               Block.get_block_metadata chain_store pred_block
             in
             Block.get_block_metadata chain_store new_head)
        in
        let*! target = Stored_data.get chain_state.target_data in
        (* This write call will initialize the cementing
           highwatermark when it is not yet set or do nothing
           otherwise. *)
        let* lfbl_block_opt =
          match chain_state.last_finalized_block_level with
          | None -> return_none
          | Some lfbl ->
              let distance =
                Int32.(to_int @@ max 0l (sub (Block.level new_head) lfbl))
              in
              Block_store.read_block
                chain_store.block_store
                ~read_metadata:false
                (Block (Block.hash new_head, distance))
        in
        let* new_checkpoint, new_target =
          match lfbl_block_opt with
          | None ->
              (* This case may occur when importing a rolling snapshot
                 where the lfbl block is not known or when a node was
                 just started. We may use the checkpoint instead. *)
              return (checkpoint, target)
          | Some lfbl_block ->
              may_update_checkpoint_and_target
                chain_store
                ~new_head:new_head_descr
                ~new_head_lfbl:(Block.descriptor lfbl_block)
                ~checkpoint
                ~target
        in
        let* () =
          if Compare.Int32.(snd new_checkpoint > snd checkpoint) then
            (* Remove potentially outdated invalid blocks if the
               checkpoint changed *)
            let* () =
              Stored_data.update_with
                chain_state.invalid_blocks_data
                (fun invalid_blocks ->
                  Lwt.return
                    (Block_hash.Map.filter
                       (fun _k {level; _} -> level > snd new_checkpoint)
                       invalid_blocks))
            in
            write_checkpoint chain_state new_checkpoint
          else return_unit
        in
        (* Update values on disk but not the cementing highwatermark
           which will be updated by the merge finalizer. *)
        let* () =
          Stored_data.write chain_state.current_head_data new_head_descr
        in
        let* () = Stored_data.write chain_state.target_data new_target in
        (* Update live_data *)
        let*! live_blocks, live_operations =
          locked_compute_live_blocks
            ~update_cache:true
            chain_store
            chain_state
            new_head
            new_head_metadata
        in
        let new_chain_state =
          {
            chain_state with
            live_blocks;
            live_operations;
            current_head = new_head;
          }
        in
        let*! () = Store_events.(emit set_head) new_head_descr in
        return (Some new_chain_state, previous_head))

  let set_target chain_store new_target =
    let open Lwt_result_syntax in
    Shared.use chain_store.chain_state (fun chain_state ->
        let*! checkpoint = Stored_data.get chain_state.checkpoint_data in
        if Compare.Int32.(snd checkpoint > snd new_target) then
          let*! b =
            is_ancestor chain_store ~head:checkpoint ~ancestor:new_target
          in
          match b with
          | true -> return_unit
          | false -> tzfail (Cannot_set_target new_target)
        else
          (* new_target > checkpoint *)
          let*! b = Block.is_known_valid chain_store (fst new_target) in
          match b with
          | false -> (
              let*! b =
                Block.locked_is_known_invalid chain_state (fst new_target)
              in
              match b with
              | true -> tzfail (Cannot_set_target new_target)
              | false ->
                  (* unknown block => new_target > current_head *)
                  (* Write future-block as target, [set_head] will
                     update it correctly *)
                  let* () =
                    Stored_data.write chain_state.target_data (Some new_target)
                  in
                  let*! () = Store_events.(emit set_target) new_target in
                  return_unit)
          | true ->
              trace
                (Cannot_set_target new_target)
                (* Do not store the target but update the chain data
                   according to the following cases:
                   1. Target is below current_head;
                   2. Target has no head as ancestor:
                      the new_target becomes the head. *)
                (let*! current_head_descr =
                   Stored_data.get chain_state.current_head_data
                 in
                 let*! is_target_an_ancestor_of_current_head =
                   is_ancestor
                     chain_store
                     ~head:current_head_descr
                     ~ancestor:new_target
                 in
                 let* new_current_head, new_checkpoint =
                   if is_target_an_ancestor_of_current_head then
                     return (current_head_descr, new_target)
                   else
                     let* target_block =
                       Block.read_block chain_store (fst new_target)
                     in
                     return (Block.descriptor target_block, new_target)
                 in
                 let* () =
                   Stored_data.write
                     chain_state.current_head_data
                     new_current_head
                 in
                 let* () =
                   Stored_data.write chain_state.checkpoint_data new_checkpoint
                 in
                 Stored_data.write chain_state.target_data None))

  let is_acceptable_block chain_store block_descr =
    Shared.use chain_store.chain_state (fun chain_state ->
        locked_is_acceptable_block chain_state block_descr)

  (* Create / Load / Close *)

  let create_chain_state ?target ~genesis_block ~genesis_protocol chain_dir =
    let open Lwt_result_syntax in
    let genesis_proto_level = Block_repr.proto_level genesis_block in
    let ((_, genesis_level) as genesis_descr) =
      Block_repr.descriptor genesis_block
    in
    let cementing_highwatermark =
      Option.fold
        ~none:0l
        ~some:(fun metadata -> Block.last_preserved_block_level metadata)
        (Block_repr.metadata genesis_block)
    in
    let expect_predecessor_context =
      (* This depends on the genesis protocol. It should be false but
         we cannot retrieved registered protocols due to unix
         dependencies. *)
      false
    in
    let* protocol_levels_data =
      Stored_data.init
        (Naming.protocol_levels_file chain_dir)
        ~initial_data:
          Protocol_levels.(
            add
              genesis_proto_level
              {
                protocol = genesis_protocol;
                activation_block = genesis_descr;
                expect_predecessor_context;
              }
              empty)
    in
    let* current_head_data =
      Stored_data.init
        (Naming.current_head_file chain_dir)
        ~initial_data:genesis_descr
    in
    let* cementing_highwatermark_data =
      Stored_data.init
        (Naming.cementing_highwatermark_file chain_dir)
        ~initial_data:(Some cementing_highwatermark)
    in
    let* checkpoint_data =
      Stored_data.init
        (Naming.checkpoint_file chain_dir)
        ~initial_data:(genesis_block.hash, genesis_level)
    in
    let* target_data =
      Stored_data.init (Naming.target_file chain_dir) ~initial_data:target
    in
    let* invalid_blocks_data =
      Stored_data.init
        (Naming.invalid_blocks_file chain_dir)
        ~initial_data:Block_hash.Map.empty
    in
    let* forked_chains_data =
      Stored_data.init
        (Naming.forked_chains_file chain_dir)
        ~initial_data:Chain_id.Map.empty
    in
    let current_head = genesis_block in
    let last_finalized_block_level = None in
    let active_testchain = None in
    let mempool = Mempool.empty in
    let live_blocks = Block_hash.Set.singleton genesis_block.hash in
    let live_operations = Operation_hash.Set.empty in
    let live_data_cache = None in
    let validated_blocks = Block_lru_cache.create 10 in
    return
      {
        current_head_data;
        last_finalized_block_level;
        cementing_highwatermark_data;
        target_data;
        checkpoint_data;
        protocol_levels_data;
        invalid_blocks_data;
        forked_chains_data;
        active_testchain;
        current_head;
        mempool;
        live_blocks;
        live_operations;
        live_data_cache;
        validated_blocks;
      }

  let create_chain_store ?block_cache_limit global_store chain_dir ?target
      ~chain_id ?(expiration = None) ?genesis_block ~genesis ~genesis_context
      history_mode =
    let open Lwt_result_syntax in
    (* Chain directory *)
    let genesis_block =
      match genesis_block with
      | None -> Block_repr.create_genesis_block ~genesis genesis_context
      | Some genesis_block -> genesis_block
    in
    (* Block_store.create also stores genesis *)
    let* block_store =
      Block_store.create ?block_cache_limit chain_dir ~genesis_block
    in
    let chain_config = {history_mode; genesis; expiration} in
    let* () =
      Stored_data.write_file (Naming.chain_config_file chain_dir) chain_config
    in
    let* chain_state =
      create_chain_state
        chain_dir
        ?target
        ~genesis_block
        ~genesis_protocol:genesis.Genesis.protocol
    in
    let* genesis_block_data =
      Stored_data.init
        (Naming.genesis_block_file chain_dir)
        ~initial_data:genesis_block
    in
    let chain_state = Shared.create chain_state in
    let block_watcher = Lwt_watcher.create_input () in
    let validated_block_watcher = Lwt_watcher.create_input () in
    let block_rpc_directories = Protocol_hash.Table.create 7 in
    let chain_store : chain_store =
      {
        global_store;
        chain_id;
        chain_dir;
        chain_config;
        chain_state;
        genesis_block_data;
        block_store;
        block_watcher;
        validated_block_watcher;
        block_rpc_directories;
      }
    in
    return chain_store

  (* Test chain *)

  let testchain chain_store =
    Shared.use chain_store.chain_state (fun {active_testchain; _} ->
        Lwt.return active_testchain)

  let testchain_forked_block {forked_block; _} = forked_block

  let testchain_store {testchain_store; _} = testchain_store

  let fork_testchain _chain_store ~testchain_id:_ ~forked_block:_
      ~genesis_hash:_ ~genesis_header:_ ~test_protocol:_ ~expiration:_ =
    Stdlib.failwith "fork_testchain: unimplemented"

  (* TODO (later) Also garbage-collect testchains store/context. *)
  let shutdown_testchain chain_store =
    let open Lwt_syntax in
    Shared.update_with
      chain_store.chain_state
      (fun ({active_testchain; _} as chain_state) ->
        match active_testchain with
        | Some _testchain ->
            return_ok (Some {chain_state with active_testchain = None}, ())
        | None -> return_ok (None, ()))

  (* Protocols *)

  let expect_predecessor_context_hash chain_store ~protocol_level =
    expect_predecessor_context_hash chain_store ~protocol_level

  let set_protocol_level chain_store ~protocol_level
      (block, protocol_hash, expect_predecessor_context) =
    let open Lwt_result_syntax in
    Shared.locked_use chain_store.chain_state (fun {protocol_levels_data; _} ->
        let* () =
          Stored_data.update_with protocol_levels_data (fun protocol_levels ->
              Lwt.return
                Protocol_levels.(
                  add
                    protocol_level
                    {
                      protocol = protocol_hash;
                      activation_block = Block.descriptor block;
                      expect_predecessor_context;
                    }
                    protocol_levels))
        in
        let*! () =
          Store_events.(
            emit
              update_protocol_table
              ( protocol_hash,
                protocol_level,
                Block.hash block,
                Block.level block ))
        in
        return_unit)

  let find_protocol_info chain_store ~protocol_level =
    find_protocol_info chain_store ~protocol_level

  let find_activation_block chain_store ~protocol_level =
    let open Lwt_syntax in
    let* protocol_info = find_protocol_info chain_store ~protocol_level in
    match protocol_info with
    | Some {activation_block; _} -> return_some activation_block
    | None -> return_none

  let find_protocol chain_store ~protocol_level =
    let open Lwt_syntax in
    let* protocol_info = find_protocol_info chain_store ~protocol_level in
    match protocol_info with
    | Some {protocol; _} -> return_some protocol
    | None -> return_none

  let may_update_protocol_level chain_store ?pred ?protocol_level
      ~expect_predecessor_context (block, protocol_hash) =
    let open Lwt_result_syntax in
    let* pred =
      match pred with
      | None -> Block.read_predecessor chain_store block
      | Some pred -> return pred
    in
    let prev_proto_level = Block.proto_level pred in
    let protocol_level =
      Option.value ~default:(Block.proto_level block) protocol_level
    in
    if Compare.Int.(prev_proto_level < protocol_level) then
      let*! o = find_activation_block chain_store ~protocol_level in
      match o with
      | Some (bh, _) ->
          if Block_hash.(bh <> Block.hash block) then
            set_protocol_level
              chain_store
              ~protocol_level
              (block, protocol_hash, expect_predecessor_context)
          else return_unit
      | None ->
          set_protocol_level
            chain_store
            ~protocol_level
            (block, protocol_hash, expect_predecessor_context)
    else return_unit

  let may_update_ancestor_protocol_level chain_store ~head =
    let open Lwt_result_syntax in
    let head_proto_level = Block.proto_level head in
    let*! o = find_protocol_info chain_store ~protocol_level:head_proto_level in
    match o with
    | None ->
        (* If no activation block is registered for a given protocol
           level, we search for the activation block. This activation
           block is expected to be found above the savepoint. If not,
           the savepoint will be considered as the activation
           block. *)
        let*! _, savepoint_level = savepoint chain_store in
        let rec find_activation_block lower_bound block =
          let*! pred = Block.read_predecessor_opt chain_store block in
          match pred with
          | None -> return block
          | Some pred ->
              let pred_proto_level = Block.proto_level pred in
              if Compare.Int.(pred_proto_level <= Int.pred head_proto_level)
              then return block
              else if Compare.Int32.(Block.level pred <= lower_bound) then
                return pred
              else find_activation_block lower_bound pred
        in
        let* activation_block = find_activation_block savepoint_level head in
        let protocol_level = Block.proto_level head in
        (* The head must have an associated context stored. *)
        let* context = Block.context chain_store head in
        let*! activated_protocol = Context_ops.get_protocol context in
        (* This depends on the protocol but registered protocols
           cannot be retrieved due to unix dependencies. *)
        let expected_predecessor_context = true in
        set_protocol_level
          chain_store
          ~protocol_level
          (activation_block, activated_protocol, expected_predecessor_context)
    | Some
        {Protocol_levels.protocol; activation_block; expect_predecessor_context}
      -> (
        let*! _, savepoint_level = savepoint chain_store in
        let activation_block_level = snd activation_block in
        if Compare.Int32.(savepoint_level > activation_block_level) then
          (* the block is too far in the past *)
          return_unit
        else
          let*! b =
            is_ancestor
              chain_store
              ~head:(Block.descriptor head)
              ~ancestor:activation_block
          in
          match b with
          | true -> (* nothing to do *) return_unit
          | false -> (
              let distance =
                Int32.(sub (Block.level head) activation_block_level |> to_int)
              in
              let*! o =
                Block.read_block_opt chain_store ~distance (Block.hash head)
              in
              match o with
              | None -> return_unit
              | Some ancestor ->
                  may_update_protocol_level
                    chain_store
                    ~expect_predecessor_context
                    (ancestor, protocol)))

  let all_protocol_levels chain_store =
    Shared.use chain_store.chain_state (fun {protocol_levels_data; _} ->
        Stored_data.get protocol_levels_data)

  let validated_watcher chain_store =
    Lwt_watcher.create_stream chain_store.validated_block_watcher

  let watcher chain_store = Lwt_watcher.create_stream chain_store.block_watcher

  let get_rpc_directory chain_store block =
    let open Lwt_syntax in
    let* o = Block.read_predecessor_opt chain_store block in
    match o with
    | None -> Lwt.return_none (* genesis *)
    | Some pred when Block_hash.equal (Block.hash pred) (Block.hash block) ->
        Lwt.return_none (* genesis *)
    | Some pred -> (
        let* _, save_point_level = savepoint chain_store in
        let* protocol =
          if Compare.Int32.(Block.level pred < save_point_level) then
            let* o =
              find_protocol_info
                chain_store
                ~protocol_level:(Block.proto_level pred)
            in
            match o with
            | Some {Protocol_levels.protocol; _} -> Lwt.return protocol
            | None -> Lwt.fail Not_found
          else Block.protocol_hash_exn chain_store pred
        in
        match
          Protocol_hash.Table.find chain_store.block_rpc_directories protocol
        with
        | None -> Lwt.return_none
        | Some map ->
            let* next_protocol = Block.protocol_hash_exn chain_store block in
            Lwt.return (Protocol_hash.Map.find next_protocol map))

  let set_rpc_directory chain_store ~protocol_hash ~next_protocol_hash dir =
    let map =
      Option.value
        ~default:Protocol_hash.Map.empty
        (Protocol_hash.Table.find
           chain_store.block_rpc_directories
           protocol_hash)
    in
    Protocol_hash.Table.replace
      chain_store.block_rpc_directories
      protocol_hash
      (Protocol_hash.Map.add next_protocol_hash dir map) ;
    Lwt.return_unit

  (* Not implemented as both the store and context garbage collection
     are disabled in the mockup mode.*)
  let register_gc_callback _ _ = ()

  (* Not implemented as both the store and context garbage collection
     are disabled in the mockup mode.*)
  let register_split_callback _ _ = ()
end

module Protocol = struct
  let all {protocol_store; _} = Protocol_store.all protocol_store

  let store {protocol_store; protocol_watcher; _} protocol_hash protocol =
    let open Lwt_syntax in
    let* o = Protocol_store.store protocol_store protocol_hash protocol in
    match o with
    | None -> Lwt.return_none
    | p ->
        Lwt_watcher.notify protocol_watcher protocol_hash ;
        Lwt.return p

  let store_raw {protocol_store; protocol_watcher; _} protocol_hash raw_protocol
      =
    let open Lwt_syntax in
    let* o =
      Protocol_store.raw_store protocol_store protocol_hash raw_protocol
    in
    match o with
    | None -> Lwt.return_none
    | p ->
        Lwt_watcher.notify protocol_watcher protocol_hash ;
        Lwt.return p

  let read {protocol_store; _} protocol_hash =
    Protocol_store.read protocol_store protocol_hash

  let mem {protocol_store; _} protocol_hash =
    Protocol_store.mem protocol_store protocol_hash

  let protocol_watcher {protocol_watcher; _} =
    Lwt_watcher.create_stream protocol_watcher
end

let create_store ?block_cache_limit ~context_index ~chain_id ~genesis
    ~genesis_context ?(history_mode = History_mode.default) ~allow_testchains
    store_dir =
  let open Lwt_result_syntax in
  let*! protocol_store = Protocol_store.init store_dir in
  let protocol_watcher = Lwt_watcher.create_input () in
  let global_block_watcher = Lwt_watcher.create_input () in
  let chain_dir = Naming.chain_dir store_dir chain_id in
  let global_store =
    {
      store_dir;
      context_index;
      main_chain_store = None;
      protocol_store;
      allow_testchains;
      protocol_watcher;
      global_block_watcher;
    }
  in
  let* main_chain_store =
    Chain.create_chain_store
      ?block_cache_limit
      global_store
      chain_dir
      ~chain_id
      ~expiration:None
      ~genesis
      ~genesis_context
      history_mode
  in
  global_store.main_chain_store <- Some main_chain_store ;
  return global_store

let main_chain_store store =
  WithExceptions.Option.get ~loc:__LOC__ store.main_chain_store

let store_dirs = ref []

let context_dirs = ref []

let init ?patch_context ?commit_genesis ?history_mode ?(readonly = false)
    ?block_cache_limit ~store_dir ~context_dir ~allow_testchains genesis =
  let open Lwt_result_syntax in
  if List.mem ~equal:String.equal context_dir !context_dirs then
    Format.kasprintf
      Stdlib.failwith
      "init: already initialized context in %s"
      context_dir ;
  context_dirs := context_dir :: !context_dirs ;
  let patch_context =
    Option.map
      (fun f ctxt ->
        let ctxt =
          Tezos_protocol_environment.Memory_context.wrap_memory_context ctxt
        in
        let+ ctxt = f ctxt in
        Tezos_protocol_environment.Memory_context.unwrap_memory_context ctxt)
      patch_context
  in
  let store_dir = Naming.store_dir ~dir_path:store_dir in
  let chain_id = Chain_id.of_block_hash genesis.Genesis.block in
  let*! context_index, commit_genesis =
    let open Tezos_context_memory in
    match commit_genesis with
    | Some commit_genesis ->
        let*! context_index =
          Context.init ~readonly:true ?patch_context context_dir
        in
        Lwt.return (context_index, commit_genesis)
    | None ->
        let*! context_index =
          Context.init ~readonly ?patch_context context_dir
        in
        let commit_genesis ~chain_id =
          Context.commit_genesis
            context_index
            ~chain_id
            ~time:genesis.time
            ~protocol:genesis.protocol
        in
        Lwt.return (context_index, commit_genesis)
  in
  let chain_dir = Naming.chain_dir store_dir chain_id in
  let chain_dir_path = Naming.dir_path chain_dir in
  if List.mem ~equal:String.equal chain_dir_path !store_dirs then
    Format.kasprintf
      Stdlib.failwith
      "init: already initialized context associated to directory %s@."
      chain_dir_path
  else (
    store_dirs := chain_dir_path :: !store_dirs ;
    (* Fresh store *)
    let* genesis_context = commit_genesis ~chain_id in
    create_store
      ?block_cache_limit
      store_dir
      ~context_index:(Context_ops.Memory_index context_index)
      ~chain_id
      ~genesis
      ~genesis_context
      ?history_mode
      ~allow_testchains)

let close_store global_store =
  Lwt_watcher.shutdown_input global_store.protocol_watcher ;
  Lwt_watcher.shutdown_input global_store.global_block_watcher ;
  Lwt.return_unit

let may_switch_history_mode ~store_dir:_ ~context_dir:_ _genesis
    ~new_history_mode:_ =
  Stdlib.failwith "may_switch_history_mode: unimplemented"

let get_chain_store store chain_id =
  let chain_store = main_chain_store store in
  let rec loop chain_store =
    let open Lwt_result_syntax in
    if Chain_id.equal (Chain.chain_id chain_store) chain_id then
      return chain_store
    else
      Shared.use chain_store.chain_state (fun {active_testchain; _} ->
          match active_testchain with
          | None -> tzfail (Validation_errors.Unknown_chain chain_id)
          | Some {testchain_store; _} -> loop testchain_store)
  in
  loop chain_store

let get_chain_store_opt store chain_id =
  let open Lwt_syntax in
  let* r = get_chain_store store chain_id in
  match r with
  | Ok chain_store -> Lwt.return_some chain_store
  | Error _ -> Lwt.return_none

let all_chain_stores store =
  let chain_store = main_chain_store store in
  let rec loop acc chain_store =
    let acc = chain_store :: acc in
    Shared.use chain_store.chain_state (fun {active_testchain; _} ->
        match active_testchain with
        | None -> Lwt.return acc
        | Some {testchain_store; _} -> loop acc testchain_store)
  in
  loop [] chain_store

let directory store = store.store_dir

let context_index store = store.context_index

let allow_testchains {allow_testchains; _} = allow_testchains

let global_block_watcher {global_block_watcher; _} =
  Lwt_watcher.create_stream global_block_watcher

let option_pp ~default pp fmt = function
  | None -> Format.fprintf fmt "%s" default
  | Some x -> Format.fprintf fmt "%a" pp x

let rec make_pp_chain_store (chain_store : chain_store) =
  let open Lwt_syntax in
  let {chain_id; chain_dir; chain_config; chain_state; block_store; _} =
    chain_store
  in
  let chain_config_json =
    Data_encoding.Json.construct chain_config_encoding chain_config
  in
  let* ( current_head,
         cementing_highwatermark,
         target,
         checkpoint,
         caboose,
         protocol_levels_data,
         invalid_blocks_data,
         forked_chains_data,
         active_test_chain ) =
    Shared.locked_use
      chain_state
      (fun
        {
          current_head;
          cementing_highwatermark_data;
          target_data;
          checkpoint_data;
          protocol_levels_data;
          invalid_blocks_data;
          forked_chains_data;
          active_testchain;
          _;
        }
      ->
        let* cementing_highwatermark =
          Stored_data.get cementing_highwatermark_data
        in
        let* target = Stored_data.get target_data in
        let* checkpoint = Stored_data.get checkpoint_data in
        let* protocol_levels = Stored_data.get protocol_levels_data in
        let* invalid_blocks = Stored_data.get invalid_blocks_data in
        let* forked_chains = Stored_data.get forked_chains_data in
        let* caboose = Block_store.caboose block_store in
        Lwt.return
          ( current_head,
            cementing_highwatermark,
            target,
            checkpoint,
            caboose,
            protocol_levels,
            invalid_blocks,
            forked_chains,
            active_testchain ))
  in
  let pp_proto_info fmt
      (proto_level, {Protocol_levels.protocol; activation_block; _}) =
    Format.fprintf
      fmt
      "proto level: %d, transition block: %a, protocol: %a"
      proto_level
      pp_block_descriptor
      activation_block
      Protocol_hash.pp
      protocol
  in
  let make_pp_test_chain_opt = function
    | None -> Lwt.return (fun fmt () -> Format.fprintf fmt "n/a")
    | Some {testchain_store; _} ->
        let* pp = make_pp_chain_store testchain_store in
        Lwt.return (fun fmt () -> Format.fprintf fmt "@ %a" pp ())
  in
  let* pp_testchain_opt = make_pp_test_chain_opt active_test_chain in
  Lwt.return (fun fmt () ->
      Format.fprintf
        fmt
        "@[<v 2>chain id: %a@ chain directory: %s@ chain config: %a@ current \
         head: %a@ checkpoint: %a@ cementing highwatermark: %a@ caboose: %a@ \
         target: %a@ @[<v 2>protocol levels:@ %a@]@ @[<v 2>invalid blocks:@ \
         %a@]@ @[<v 2>forked chains:@ %a@]@ @[<v 2>active testchain: %a@]@]"
        Chain_id.pp
        chain_id
        (Naming.dir_path chain_dir)
        Data_encoding.Json.pp
        chain_config_json
        (fun fmt block ->
          let metadata =
            WithExceptions.Option.get ~loc:__LOC__ (Block_repr.metadata block)
          in
          Format.fprintf
            fmt
            "%a (lpbl: %ld) (max_op_ttl: %d)"
            pp_block_descriptor
            (Block.descriptor block)
            (Block.last_preserved_block_level metadata)
            (Block.max_operations_ttl metadata))
        current_head
        pp_block_descriptor
        checkpoint
        (fun fmt opt ->
          option_pp
            ~default:"n/a"
            (fun fmt i -> Format.fprintf fmt "%ld" i)
            fmt
            opt)
        cementing_highwatermark
        pp_block_descriptor
        caboose
        (option_pp ~default:"n/a" pp_block_descriptor)
        target
        (Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_proto_info)
        (Protocol_levels.bindings protocol_levels_data)
        (Format.pp_print_list ~pp_sep:Format.pp_print_cut Block_hash.pp)
        (Block_hash.Map.bindings invalid_blocks_data |> List.map fst)
        (Format.pp_print_list
           ~pp_sep:Format.pp_print_cut
           (fun fmt (chain_id, block_hash) ->
             Format.fprintf
               fmt
               "testchain's chain id: %a, forked block: %a"
               Chain_id.pp
               chain_id
               Block_hash.pp
               block_hash))
        (Chain_id.Map.bindings forked_chains_data)
        pp_testchain_opt
        ())

let make_pp_store (store : store) =
  let open Lwt_syntax in
  let {store_dir; allow_testchains; main_chain_store; _} = store in
  let* pp_testchain_store =
    make_pp_chain_store
      (WithExceptions.Option.get ~loc:__LOC__ main_chain_store)
  in
  Lwt.return (fun fmt () ->
      Format.fprintf
        fmt
        "@[<v 2>Store state:@ store directory: %s@ allow testchains: %b@ @[<v \
         2>main chain:@ %a@]@])"
        (Naming.dir_path store_dir)
        allow_testchains
        pp_testchain_store
        ())

module Unsafe = struct
  let repr_of_block = Fun.id

  let block_of_repr = Fun.id
end

let v_3_0_upgrade ~store_dir:_ _genesis = Lwt_result_syntax.return_unit
