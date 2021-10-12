(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

[@@@ocaml.warning "-30"]

module Events = State_events
open Validation_errors

module Shared = struct
  type 'a t = {data : 'a; lock : Lwt_mutex.t}

  let create data = {data; lock = Lwt_mutex.create ()}

  let use {data; lock} f = Lwt_mutex.with_lock lock (fun () -> f data)
end

type global_state = {
  global_data : global_data Shared.t;
  protocol_store : Legacy_store.Protocol.store Shared.t;
  main_chain : Chain_id.t;
  protocol_watcher : Protocol_hash.t Lwt_watcher.input;
  block_watcher : block Lwt_watcher.input;
}

and global_data = {
  chains : chain_state Chain_id.Table.t;
  global_store : Legacy_store.t;
  context_index : Context.index;
}

and chain_state = {
  (* never take the lock on 'block_store' when holding
     the lock on 'chain_data'. *)
  global_state : global_state;
  chain_id : Chain_id.t;
  genesis : Genesis.t;
  faked_genesis_hash : Block_hash.t;
  expiration : Time.Protocol.t option;
  allow_forked_chain : bool;
  block_store : Legacy_store.Block.store Shared.t;
  context_index : Context.index Shared.t;
  block_watcher : block Lwt_watcher.input;
  chain_data : chain_data_state Shared.t;
  block_rpc_directories :
    block RPC_directory.t Protocol_hash.Map.t Protocol_hash.Table.t;
  header_rpc_directories :
    (chain_state * Block_hash.t * Block_header.t) RPC_directory.t
    Protocol_hash.Map.t
    Protocol_hash.Table.t;
}

and chain_data_state = {
  mutable data : chain_data;
  mutable checkpoint : Block_header.t;
  chain_data_store : Legacy_store.Chain_data.store;
}

and chain_data = {
  current_head : block;
  current_mempool : Mempool.t;
  live_blocks : Block_hash.Set.t;
  live_operations : Operation_hash.Set.t;
  test_chain : Chain_id.t option;
  save_point : Int32.t * Block_hash.t;
  caboose : Int32.t * Block_hash.t;
}

and block = {
  chain_state : chain_state;
  hash : Block_hash.t;
  header : Block_header.t;
}

(* Errors *)

type error += Block_not_found of Block_hash.t

type error += Block_contents_not_found of Block_hash.t

let () =
  register_error_kind
    `Permanent
    ~id:"state.block.not_found"
    ~title:"Block_not_found"
    ~description:"Block not found"
    ~pp:(fun ppf block_hash ->
      Format.fprintf ppf "@[Cannot find block %a@]" Block_hash.pp block_hash)
    Data_encoding.(obj1 (req "block_not_found" @@ Block_hash.encoding))
    (function Block_not_found block_hash -> Some block_hash | _ -> None)
    (fun block_hash -> Block_not_found block_hash) ;
  register_error_kind
    `Permanent
    ~id:"state.block.contents_not_found"
    ~title:"Block_contents_not_found"
    ~description:"Block not found"
    ~pp:(fun ppf block_hash ->
      Format.fprintf
        ppf
        "@[Cannot find block contents %a@]"
        Block_hash.pp
        block_hash)
    Data_encoding.(obj1 (req "block_contents_not_found" @@ Block_hash.encoding))
    (function
      | Block_contents_not_found block_hash -> Some block_hash | _ -> None)
    (fun block_hash -> Block_contents_not_found block_hash)

(* Abstract view over block header storage.
   This module aims to abstract over block header's [read], [read_opt] and [known]
   functions by calling the adequate function depending on the block being pruned or not.
*)

module Header = struct
  let read (store, hash) =
    Legacy_store.Block.Contents.read (store, hash) >>= function
    | Ok {header; _} -> return header
    | Error _ ->
        Legacy_store.Block.Pruned_contents.read (store, hash)
        >>=? fun {header} -> return header

  let read_opt (store, hash) =
    read (store, hash) >>= function
    | Ok header -> Lwt.return_some header
    | Error _ -> Lwt.return_none

  let known (store, hash) =
    Legacy_store.Block.Pruned_contents.known (store, hash) >>= function
    | true -> Lwt.return_true
    | false -> Legacy_store.Block.Contents.known (store, hash)
end

let read_chain_data {chain_data; _} f =
  Shared.use chain_data (fun state -> f state.chain_data_store state.data)

let update_chain_data {chain_data; _} f =
  Shared.use chain_data (fun state ->
      f state.chain_data_store state.data >>= fun (data, res) ->
      Option.iter (fun data -> state.data <- data) data ;
      Lwt.return res)

(** The number of predecessors stored per block.
    This value chosen to compute efficiently block locators that
    can cover a chain of 2 months, at 1 block/min, which is ~86K
    blocks at the cost in space of ~72MB.
    |locator| = log2(|chain|/10) -1
*)
let stored_predecessors_size = 12

(**
   Takes a block and populates its predecessors store, under the
   assumption that all its predecessors have their store already
   populated. The precedecessors are distributed along the chain, up
   to the genesis, at a distance from [b] that grows exponentially.
   The store tabulates a function [p] from distances to block_ids such
   that if [p(b,d)=b'] then [b'] is at distance 2^d from [b].
   Example of how previous predecessors are used:
   p(n,0) = n-1
   p(n,1) = n-2  = p(n-1,0)
   p(n,2) = n-4  = p(n-2,1)
   p(n,3) = n-8  = p(n-4,2)
   p(n,4) = n-16 = p(n-8,3)
*)
let store_predecessors (store : Legacy_store.Block.store) (b : Block_hash.t) :
    unit Lwt.t =
  let rec loop pred dist =
    if dist = stored_predecessors_size then Lwt.return_unit
    else
      Legacy_store.Block.Predecessors.read_opt (store, pred) (dist - 1)
      >>= function
      | None -> Lwt.return_unit (* we reached the last known block *)
      | Some p ->
          Legacy_store.Block.Predecessors.store (store, b) dist p >>= fun () ->
          loop p (dist + 1)
  in
  (* the first predecessor is fetched from the header *)
  Header.read_opt (store, b) >|= WithExceptions.Option.get ~loc:__LOC__
  >>= fun header ->
  let pred = header.shell.predecessor in
  if Block_hash.equal b pred then Lwt.return_unit (* genesis *)
  else
    Legacy_store.Block.Predecessors.store (store, b) 0 pred >>= fun () ->
    loop pred 1

(**
   [predecessor_n_raw s b d] returns the hash of the block at distance [d] from [b].
   Returns [None] if [d] is greater than the distance of [b] from genesis or
   if [b] is genesis.
   Works in O(log|chain|) if the chain is shorter than 2^[stored_predecessors_size]
   and in O(|chain|) after that.
   @raise Invalid_argument "State.predecessors: negative distance"
*)
let predecessor_n_raw store block_hash distance =
  (* helper functions *)
  (* computes power of 2 w/o floats *)
  let power_of_2 n =
    if n < 0 then invalid_arg "negative argument"
    else
      let rec loop cnt res =
        if cnt < 1 then res else loop (cnt - 1) (res * 2)
      in
      loop n 1
  in
  (* computes the closest power of two smaller than a given
     a number and the rest w/o floats *)
  let closest_power_two_and_rest n =
    if n < 0 then invalid_arg "negative argument"
    else
      let rec loop cnt n rest =
        if n <= 1 then (cnt, rest)
        else loop (cnt + 1) (n / 2) (rest + (power_of_2 cnt * (n mod 2)))
      in
      loop 0 n 0
  in
  (* actual predecessor function *)
  if distance < 0 then
    invalid_arg ("State.predecessor: distance < 0 " ^ string_of_int distance)
  else if distance = 0 then Lwt.return_some block_hash
  else
    let rec loop block_hash distance =
      if distance = 1 then
        Legacy_store.Block.Predecessors.read_opt (store, block_hash) 0
      else
        let (power, rest) = closest_power_two_and_rest distance in
        let (power, rest) =
          if power < stored_predecessors_size then (power, rest)
          else
            let power = stored_predecessors_size - 1 in
            let rest = distance - power_of_2 power in
            (power, rest)
        in
        Legacy_store.Block.Predecessors.read_opt (store, block_hash) power
        >>= function
        | None -> Lwt.return_none (* reached genesis *)
        | Some pred ->
            if rest = 0 then Lwt.return_some pred
              (* landed on the requested predecessor *)
            else loop pred rest
      (* need to jump further back *)
    in
    loop block_hash distance

let predecessor_n block_store block_hash distance =
  Option.catch_os (fun () ->
      predecessor_n_raw block_store block_hash distance >>= function
      | None -> Lwt.return_none
      | Some predecessor -> (
          Header.known (block_store, predecessor) >>= function
          | false -> Lwt.return_none
          | true -> Lwt.return_some predecessor))

type t = global_state

module Locked_block = struct
  let store_genesis store genesis context =
    let shell : Block_header.shell_header =
      let open Genesis in
      {
        level = 0l;
        proto_level = 0;
        predecessor = genesis.block;
        (* genesis' predecessor is genesis *)
        timestamp = genesis.time;
        fitness = [];
        validation_passes = 0;
        operations_hash = Operation_list_list_hash.empty;
        context;
      }
    in
    let header : Block_header.t = {shell; protocol_data = Bytes.create 0} in
    Legacy_store.Block.Contents.store
      (store, genesis.block)
      {
        header;
        Legacy_store.Block.message = Some "Genesis";
        max_operations_ttl = 0;
        context;
        metadata = Bytes.create 0;
        last_allowed_fork_level = 0l;
      }
    >>= fun () -> Lwt.return header

  (* Will that block is compatible with the current checkpoint. *)
  let acceptable chain_data (header : Block_header.t) =
    let checkpoint_level = chain_data.checkpoint.shell.level in
    if checkpoint_level < header.shell.level then
      (* the predecessor is assumed compatible. *)
      Lwt.return_true
    else if checkpoint_level = header.shell.level then
      Lwt.return (Block_header.equal header chain_data.checkpoint)
    else
      (* header.shell.level < level *)
      (* valid only if the current head is lower than the checkpoint. *)
      let head_level = chain_data.data.current_head.header.shell.level in
      Lwt.return (head_level < checkpoint_level)

  (* Is a block still valid for a given checkpoint ? *)
  let is_valid_for_checkpoint block_store hash (header : Block_header.t)
      (checkpoint : Block_header.t) =
    if Compare.Int32.(header.shell.level < checkpoint.shell.level) then
      Lwt.return_true
    else
      predecessor_n
        block_store
        hash
        (Int32.to_int @@ Int32.sub header.shell.level checkpoint.shell.level)
      >|= WithExceptions.Option.get ~loc:__LOC__
      >>= fun predecessor ->
      if Block_hash.equal predecessor (Block_header.hash checkpoint) then
        Lwt.return_true
      else Lwt.return_false
end

(* Find the branches that are still valid with a given checkpoint, i.e.
   heads with lower level, or branches that goes through the checkpoint. *)
let locked_valid_heads_for_checkpoint block_store data checkpoint =
  Legacy_store.Chain_data.Known_heads.read_all data.chain_data_store
  >>= fun heads ->
  Block_hash.Set.fold_s
    (fun head (valid_heads, invalid_heads) ->
      Header.read_opt (block_store, head)
      >|= WithExceptions.Option.get ~loc:__LOC__
      >>= fun header ->
      Locked_block.is_valid_for_checkpoint block_store head header checkpoint
      >>= fun valid ->
      if valid then Lwt.return ((head, header) :: valid_heads, invalid_heads)
      else Lwt.return (valid_heads, (head, header) :: invalid_heads))
    heads
    ([], [])

(* Tag as invalid all blocks in `heads` and their predecessors whose
   level strictly higher to 'level'. *)
let tag_invalid_heads block_store chain_store heads level =
  let rec tag_invalid_head (hash, header) =
    if header.Block_header.shell.level <= level then
      Legacy_store.Chain_data.Known_heads.store chain_store hash >>= fun () ->
      Lwt.return_some (hash, header)
    else
      let errors = [Validation_errors.Checkpoint_error (hash, None)] in
      Legacy_store.Block.Invalid_block.store
        block_store
        hash
        {level = header.shell.level; errors}
      >>= fun () ->
      Legacy_store.Block.Contents.remove (block_store, hash) >>= fun () ->
      Legacy_store.Block.Operation_hashes.remove_all (block_store, hash)
      >>= fun () ->
      Legacy_store.Block.Operations_metadata.remove_all (block_store, hash)
      >>= fun () ->
      Legacy_store.Block.Operations.remove_all (block_store, hash) >>= fun () ->
      Legacy_store.Block.Predecessors.remove_all (block_store, hash)
      >>= fun () ->
      Header.read_opt (block_store, header.shell.predecessor) >>= function
      | None -> Lwt.return_none
      | Some header -> tag_invalid_head (Block_header.hash header, header)
  in
  List.iter_p
    (fun (hash, _header) ->
      Legacy_store.Chain_data.Known_heads.remove chain_store hash)
    heads
  >>= fun () -> List.filter_map_s tag_invalid_head heads

let prune_block store block_hash =
  let st = (store, block_hash) in
  Legacy_store.Block.Contents.remove st >>= fun () ->
  Legacy_store.Block.Invalid_block.remove store block_hash >>= fun () ->
  Legacy_store.Block.Operations_metadata.remove_all st

let store_header_and_prune_block store block_hash =
  let st = (store, block_hash) in
  (Legacy_store.Block.Contents.read_opt st >>= function
   | Some {header; _} -> Legacy_store.Block.Pruned_contents.store st {header}
   | None -> (
       Legacy_store.Block.Pruned_contents.known st >>= function
       | true -> Lwt.return_unit
       | false -> Events.(emit missing_pruned_contents) block_hash))
  >>= fun () -> prune_block store block_hash

let delete_block store block_hash =
  prune_block store block_hash >>= fun () ->
  let st = (store, block_hash) in
  Legacy_store.Block.Pruned_contents.remove st >>= fun () ->
  Legacy_store.Block.Operations.remove_all st >>= fun () ->
  Legacy_store.Block.Operation_hashes.remove_all st >>= fun () ->
  Legacy_store.Block.Predecessors.remove_all st

(* Remove all blocks that are not in the chain. *)
let cut_alternate_heads block_store chain_store heads =
  let rec cut_alternate_head hash header =
    Legacy_store.Chain_data.In_main_branch.known (chain_store, hash)
    >>= fun in_chain ->
    if in_chain then Lwt.return_unit
    else
      Header.read_opt (block_store, header.Block_header.shell.predecessor)
      >>= function
      | None -> delete_block block_store hash >>= fun () -> Lwt.return_unit
      | Some header ->
          delete_block block_store hash >>= fun () ->
          cut_alternate_head (Block_header.hash header) header
  in
  List.iter_p
    (fun (hash, header) ->
      Legacy_store.Chain_data.Known_heads.remove chain_store hash >>= fun () ->
      cut_alternate_head hash header)
    heads

module Chain = struct
  type t = chain_state

  type chain_state = t

  let main {main_chain; _} = main_chain

  let test chain_state =
    read_chain_data chain_state (fun _ chain_data ->
        Lwt.return chain_data.test_chain)

  let all_indexed_protocols chain_state =
    let chain_id = chain_state.chain_id in
    let global_state = chain_state.global_state in
    Shared.use global_state.global_data (fun global_data ->
        let global_store = global_data.global_store in
        let chain_store = Legacy_store.Chain.get global_store chain_id in
        Legacy_store.Chain.Protocol_info.bindings chain_store)

  let get_level_indexed_protocol chain_state header =
    let chain_id = chain_state.chain_id in
    let protocol_level = header.Block_header.shell.proto_level in
    let global_state = chain_state.global_state in
    Shared.use global_state.global_data (fun global_data ->
        let global_store = global_data.global_store in
        let chain_store = Legacy_store.Chain.get global_store chain_id in
        Legacy_store.Chain.Protocol_info.read_opt chain_store protocol_level
        >>= function
        | None -> Stdlib.failwith "State.Chain.get_level_index_protocol"
        | Some (p, _) -> Lwt.return p)

  let update_level_indexed_protocol_store chain_state chain_id protocol_level
      protocol_hash block_header =
    let global_state = chain_state.global_state in
    Shared.use chain_state.block_store (fun block_store ->
        Header.read_opt
          (block_store, block_header.Block_header.shell.predecessor)
        >>= function
        | None -> Lwt.return_none (* should not happen *)
        | Some header -> Lwt.return_some header)
    >>= function
    | None -> Lwt.return_unit
    | Some pred_header ->
        if pred_header.shell.proto_level <> block_header.shell.proto_level then
          Shared.use global_state.global_data (fun global_data ->
              let global_store = global_data.global_store in
              let chain_store = Legacy_store.Chain.get global_store chain_id in
              Legacy_store.Chain.Protocol_info.store
                chain_store
                protocol_level
                (protocol_hash, block_header.shell.level))
        else Lwt.return_unit

  let allocate ~genesis ~faked_genesis_hash ~save_point ~caboose ~expiration
      ~allow_forked_chain ~current_head ~checkpoint ~chain_id global_state
      context_index chain_data_store block_store =
    Header.read_opt (block_store, current_head)
    >|= WithExceptions.Option.get ~loc:__LOC__
    >>= fun current_block_head ->
    let rec chain_data =
      {
        data =
          {
            save_point;
            caboose;
            current_head =
              {chain_state; hash = current_head; header = current_block_head};
            current_mempool = Mempool.empty;
            live_blocks = Block_hash.Set.singleton genesis.Genesis.block;
            live_operations = Operation_hash.Set.empty;
            test_chain = None;
          };
        checkpoint;
        chain_data_store;
      }
    and chain_state =
      {
        global_state;
        chain_id;
        chain_data = {Shared.data = chain_data; lock = Lwt_mutex.create ()};
        genesis;
        faked_genesis_hash;
        expiration;
        allow_forked_chain;
        block_store = Shared.create block_store;
        context_index = Shared.create context_index;
        block_watcher = Lwt_watcher.create_input ();
        block_rpc_directories = Protocol_hash.Table.create 7;
        header_rpc_directories = Protocol_hash.Table.create 7;
      }
    in
    Lwt.return chain_state

  let locked_create global_state data ?expiration ?(allow_forked_chain = false)
      chain_id genesis (genesis_header : Block_header.t) =
    let open Genesis in
    let chain_store = Legacy_store.Chain.get data.global_store chain_id in
    let block_store = Legacy_store.Block.get chain_store
    and chain_data_store = Legacy_store.Chain_data.get chain_store in
    let save_point = (genesis_header.shell.level, genesis.block) in
    let caboose = (genesis_header.shell.level, genesis.block) in
    let proto_level = genesis_header.shell.proto_level in
    Legacy_store.Chain.Genesis_hash.store chain_store genesis.block
    >>= fun () ->
    Legacy_store.Chain.Genesis_time.store chain_store genesis.time >>= fun () ->
    Legacy_store.Chain.Genesis_protocol.store chain_store genesis.protocol
    >>= fun () ->
    Legacy_store.Chain_data.Current_head.store chain_data_store genesis.block
    >>= fun () ->
    Legacy_store.Chain_data.Known_heads.store chain_data_store genesis.block
    >>= fun () ->
    Legacy_store.Chain_data.Checkpoint.store chain_data_store genesis_header
    >>= fun () ->
    Legacy_store.Chain_data.Save_point.store chain_data_store save_point
    >>= fun () ->
    Legacy_store.Chain_data.Caboose.store chain_data_store caboose >>= fun () ->
    Legacy_store.Chain.Protocol_info.store
      chain_store
      proto_level
      (genesis.protocol, genesis_header.shell.level)
    >>= fun () ->
    (match expiration with
    | None -> Lwt.return_unit
    | Some time -> Legacy_store.Chain.Expiration.store chain_store time)
    >>= fun () ->
    (if allow_forked_chain then
     Legacy_store.Chain.Allow_forked_chain.store data.global_store chain_id
    else Lwt.return_unit)
    >>= fun () ->
    allocate
      ~genesis
      ~faked_genesis_hash:(Block_header.hash genesis_header)
      ~current_head:genesis.block
      ~expiration
      ~allow_forked_chain
      ~checkpoint:genesis_header
      ~chain_id
      ~save_point
      ~caboose
      global_state
      data.context_index
      chain_data_store
      block_store
    >>= fun chain ->
    Chain_id.Table.add data.chains chain_id chain ;
    Lwt.return chain

  let create state ?allow_forked_chain ~commit_genesis genesis chain_id =
    Shared.use state.global_data (fun data ->
        let chain_store = Legacy_store.Chain.get data.global_store chain_id in
        let block_store = Legacy_store.Block.get chain_store in
        if Chain_id.Table.mem data.chains chain_id then
          Stdlib.failwith "State.Chain.create"
        else
          commit_genesis ~chain_id >>=? fun commit ->
          Locked_block.store_genesis block_store genesis commit
          >>= fun genesis_header ->
          locked_create
            state
            data
            ?allow_forked_chain
            chain_id
            genesis
            genesis_header
          >>= fun chain ->
          (* in case this is a forked chain creation,
             delete its header from the temporary table*)
          Legacy_store.Forking_block_hash.remove
            data.global_store
            (Context.compute_testchain_chain_id genesis.block)
          >>= fun () -> return chain)

  let locked_read global_state data chain_id =
    let chain_store = Legacy_store.Chain.get data.global_store chain_id in
    let block_store = Legacy_store.Block.get chain_store
    and chain_data_store = Legacy_store.Chain_data.get chain_store in
    Legacy_store.Chain.Genesis_hash.read chain_store >>=? fun genesis_hash ->
    Legacy_store.Chain.Genesis_time.read chain_store >>=? fun time ->
    Legacy_store.Chain.Genesis_protocol.read chain_store >>=? fun protocol ->
    Legacy_store.Chain.Expiration.read_opt chain_store >>= fun expiration ->
    Legacy_store.Chain.Allow_forked_chain.known data.global_store chain_id
    >>= fun allow_forked_chain ->
    Header.read (block_store, genesis_hash) >>=? fun genesis_header ->
    let genesis = {Genesis.time; protocol; block = genesis_hash} in
    Legacy_store.Chain_data.Current_head.read chain_data_store
    >>=? fun current_head ->
    Legacy_store.Chain_data.Checkpoint.read chain_data_store
    >>=? fun checkpoint ->
    Legacy_store.Chain_data.Save_point.read chain_data_store
    >>=? fun save_point ->
    Legacy_store.Chain_data.Caboose.read chain_data_store >>=? fun caboose ->
    try
      allocate
        ~genesis
        ~faked_genesis_hash:(Block_header.hash genesis_header)
        ~current_head
        ~expiration
        ~allow_forked_chain
        ~checkpoint
        ~chain_id
        ~save_point
        ~caboose
        global_state
        data.context_index
        chain_data_store
        block_store
      >>= return
    with Not_found -> fail Bad_data_dir

  let locked_read_all global_state data =
    Legacy_store.Chain.list data.global_store >>= fun ids ->
    List.iter_ep
      (fun id ->
        locked_read global_state data id >>=? fun chain ->
        Chain_id.Table.add data.chains id chain ;
        return_unit)
      ids

  let read_all state =
    Shared.use state.global_data (fun data -> locked_read_all state data)

  let get_opt state id =
    Shared.use state.global_data (fun data ->
        Lwt.return (Chain_id.Table.find data.chains id))

  let get_exn state id =
    get_opt state id >|= WithExceptions.Option.to_exn ~none:Not_found

  let get state id =
    get_opt state id >|= function
    | Some v -> Ok v
    | None -> error (Unknown_chain id)

  let all state =
    Shared.use state.global_data (fun {chains; _} ->
        Lwt.return @@ Chain_id.Table.to_seq_values chains)

  let id {chain_id; _} = chain_id

  let genesis {genesis; _} = genesis

  let faked_genesis_hash {faked_genesis_hash; _} = faked_genesis_hash

  let expiration {expiration; _} = expiration

  let allow_forked_chain {allow_forked_chain; _} = allow_forked_chain

  let global_state {global_state; _} = global_state

  let checkpoint chain_state =
    Shared.use chain_state.chain_data (fun {checkpoint; _} ->
        Lwt.return checkpoint)

  let save_point chain_state =
    Shared.use chain_state.chain_data (fun state ->
        Lwt.return state.data.save_point)

  let caboose chain_state =
    Shared.use chain_state.chain_data (fun state ->
        Lwt.return state.data.caboose)

  let purge_loop_full ?(chunk_size = 4000) global_store store ~genesis_hash
      block_hash caboose_level =
    let do_prune blocks =
      Legacy_store.with_atomic_rw global_store @@ fun () ->
      List.iter_s (store_header_and_prune_block store) blocks
    in
    let rec loop block_hash (n_blocks, blocks) =
      (if n_blocks >= chunk_size then
       do_prune blocks >>= fun () -> Lwt.return (0, [])
      else Lwt.return (n_blocks, blocks))
      >>= fun (n_blocks, blocks) ->
      Header.read_opt (store, block_hash)
      >|= WithExceptions.Option.get ~loc:__LOC__
      >>= fun header ->
      if Block_hash.equal block_hash genesis_hash then do_prune blocks
      else if header.shell.level = caboose_level then
        do_prune (block_hash :: blocks)
      else loop header.shell.predecessor (n_blocks + 1, block_hash :: blocks)
    in
    Header.read_opt (store, block_hash)
    >|= WithExceptions.Option.get ~loc:__LOC__
    >>= fun header -> loop header.shell.predecessor (0, [])

  let purge_full chain_state (lvl, hash) =
    Shared.use chain_state.global_state.global_data (fun global_data ->
        Shared.use chain_state.block_store (fun store ->
            update_chain_data chain_state (fun _ data ->
                purge_loop_full
                  global_data.global_store
                  store
                  ~genesis_hash:chain_state.genesis.block
                  hash
                  (fst data.save_point)
                >>= fun () ->
                let new_data = {data with save_point = (lvl, hash)} in
                Lwt.return (Some new_data, ()))
            >>= fun () ->
            Shared.use chain_state.chain_data (fun data ->
                Legacy_store.Chain_data.Save_point.store
                  data.chain_data_store
                  (lvl, hash)
                >>= fun () -> return_unit)))

  let purge_loop_rolling global_store store ~genesis_hash block_hash limit =
    let do_delete blocks =
      Legacy_store.with_atomic_rw global_store @@ fun () ->
      List.iter_s (delete_block store) blocks
    in
    let rec prune_loop block_hash limit =
      if Block_hash.equal genesis_hash block_hash then Lwt.return block_hash
      else if limit = 1 then
        Header.read_opt (store, block_hash) >>= function
        | None -> assert false (* Should not happen. *)
        | Some header ->
            store_header_and_prune_block store block_hash >>= fun () ->
            delete_loop header.shell.predecessor (0, []) >>= fun () ->
            Lwt.return block_hash
      else
        Header.read_opt (store, block_hash) >>= function
        | None -> assert false (* Should not happen. *)
        | Some header ->
            store_header_and_prune_block store block_hash >>= fun () ->
            prune_loop header.shell.predecessor (limit - 1)
    and delete_loop block_hash (n_blocks, blocks) =
      (if n_blocks >= 4000 then do_delete blocks >>= fun () -> Lwt.return (0, [])
      else Lwt.return (n_blocks, blocks))
      >>= fun (n_blocks, blocks) ->
      Header.read_opt (store, block_hash) >>= function
      | None -> do_delete blocks
      | Some header ->
          if Block_hash.equal genesis_hash block_hash then do_delete blocks
          else
            delete_loop
              header.shell.predecessor
              (n_blocks + 1, block_hash :: blocks)
    in
    Header.read_opt (store, block_hash)
    >|= WithExceptions.Option.get ~loc:__LOC__
    >>= fun header ->
    if limit = 0 then
      delete_loop header.shell.predecessor (0, []) >>= fun () ->
      Lwt.return block_hash
    else prune_loop header.shell.predecessor limit

  let purge_rolling chain_state ((lvl, hash) as checkpoint) =
    Shared.use chain_state.global_state.global_data (fun global_data ->
        Shared.use chain_state.block_store (fun store ->
            (Legacy_store.Block.Contents.read_opt (store, hash) >>= function
             | None -> fail (Block_contents_not_found hash)
             | Some contents -> return contents)
            >>=? fun contents ->
            let max_op_ttl = contents.max_operations_ttl in
            let limit = max_op_ttl in
            purge_loop_rolling
              ~genesis_hash:chain_state.genesis.block
              global_data.global_store
              store
              hash
              limit
            >>= fun caboose_hash ->
            let caboose_level = Int32.sub lvl (Int32.of_int max_op_ttl) in
            let caboose = (caboose_level, caboose_hash) in
            update_chain_data chain_state (fun _ data ->
                let new_data = {data with save_point = checkpoint; caboose} in
                Lwt.return (Some new_data, ()))
            >>= fun () ->
            Shared.use chain_state.chain_data (fun data ->
                Legacy_store.Chain_data.Save_point.store
                  data.chain_data_store
                  checkpoint
                >>= fun () ->
                Legacy_store.Chain_data.Caboose.store
                  data.chain_data_store
                  caboose
                >>= fun () -> return_unit)))

  let set_checkpoint chain_state checkpoint =
    Shared.use chain_state.block_store (fun store ->
        Shared.use chain_state.chain_data (fun data ->
            let head_header = data.data.current_head.header in
            let head_hash = data.data.current_head.hash in
            Locked_block.is_valid_for_checkpoint
              store
              head_hash
              head_header
              checkpoint
            >>= fun valid ->
            assert valid ;
            (* Remove outdated invalid blocks. *)
            Legacy_store.Block.Invalid_block.iter store ~f:(fun hash iblock ->
                if iblock.level <= checkpoint.shell.level then
                  Legacy_store.Block.Invalid_block.remove store hash
                else Lwt.return_unit)
            >>= fun () ->
            (* Remove outdated heads and tag invalid branches. *)
            ( locked_valid_heads_for_checkpoint store data checkpoint
            >>= fun (valid_heads, invalid_heads) ->
              tag_invalid_heads
                store
                data.chain_data_store
                invalid_heads
                checkpoint.shell.level
              >>= fun outdated_invalid_heads ->
              if head_header.shell.level < checkpoint.shell.level then
                Lwt.return_unit
              else
                let outdated_valid_heads =
                  List.filter
                    (fun (hash, {Block_header.shell; _}) ->
                      shell.level <= checkpoint.shell.level
                      && not (Block_hash.equal hash head_hash))
                    valid_heads
                in
                cut_alternate_heads
                  store
                  data.chain_data_store
                  outdated_valid_heads
                >>= fun () ->
                cut_alternate_heads
                  store
                  data.chain_data_store
                  outdated_invalid_heads )
            >>= fun () ->
            (* Legacy_store the new checkpoint. *)
            Legacy_store.Chain_data.Checkpoint.store
              data.chain_data_store
              checkpoint
            >>= fun () ->
            data.checkpoint <- checkpoint ;
            (* TODO 'git fsck' in the context. *)
            Lwt.return_unit))

  let set_checkpoint_then_purge_full chain_state checkpoint =
    set_checkpoint chain_state checkpoint >>= fun () ->
    let lvl = checkpoint.shell.level in
    let hash = Block_header.hash checkpoint in
    purge_full chain_state (lvl, hash)

  let set_checkpoint_then_purge_rolling chain_state checkpoint =
    set_checkpoint chain_state checkpoint >>= fun () ->
    let lvl = checkpoint.shell.level in
    let hash = Block_header.hash checkpoint in
    purge_rolling chain_state (lvl, hash)

  let acceptable_block chain_state (header : Block_header.t) =
    Shared.use chain_state.chain_data (fun chain_data ->
        Locked_block.acceptable chain_data header)

  let destroy state chain =
    Events.(emit destroy_state (id chain)) >>= fun () ->
    Shared.use state.global_data (fun {global_store; chains; _} ->
        Chain_id.Table.remove chains (id chain) ;
        Legacy_store.Chain.destroy global_store (id chain))

  let store chain_state =
    Shared.use chain_state.global_state.global_data (fun global_data ->
        Lwt.return global_data.global_store)
end

module Block = struct
  type t = block = {
    chain_state : Chain.t;
    hash : Block_hash.t;
    header : Block_header.t;
  }

  type block = t

  module Header = Header

  let compare b1 b2 = Block_hash.compare b1.hash b2.hash

  let equal b1 b2 = Block_hash.equal b1.hash b2.hash

  let hash {hash; _} = hash

  let header {header; _} = header

  let read_contents block =
    Shared.use block.chain_state.block_store (fun store ->
        Legacy_store.Block.Contents.read_opt (store, block.hash) >>= function
        | None -> fail (Block_contents_not_found block.hash)
        | Some contents -> return contents)

  let read_contents_opt block =
    Shared.use block.chain_state.block_store (fun store ->
        Legacy_store.Block.Contents.read_opt (store, block.hash) >>= function
        | None -> Lwt.return_none
        | Some contents -> Lwt.return_some contents)

  let header_of_hash chain_state hash =
    Shared.use chain_state.block_store (fun store ->
        Header.read_opt (store, hash))

  let metadata b = read_contents b >>=? fun {metadata; _} -> return metadata

  let chain_state {chain_state; _} = chain_state

  let chain_id {chain_state = {chain_id; _}; _} = chain_id

  let shell_header {header = {shell; _}; _} = shell

  let timestamp b = (shell_header b).timestamp

  let fitness b = (shell_header b).fitness

  let level b = (shell_header b).level

  let validation_passes b = (shell_header b).validation_passes

  let message b = read_contents b >>=? fun {message; _} -> return message

  let max_operations_ttl b =
    read_contents b >>=? fun {max_operations_ttl; _} ->
    return max_operations_ttl

  let last_allowed_fork_level b =
    read_contents b >>=? fun {last_allowed_fork_level; _} ->
    return last_allowed_fork_level

  let is_genesis b = Block_hash.equal b.hash b.chain_state.genesis.block

  let known_valid chain_state hash =
    Shared.use chain_state.block_store (fun store -> Header.known (store, hash))

  let known_invalid chain_state hash =
    Shared.use chain_state.block_store (fun store ->
        Legacy_store.Block.Invalid_block.known store hash)

  let read_invalid chain_state hash =
    Shared.use chain_state.block_store (fun store ->
        Legacy_store.Block.Invalid_block.read_opt store hash)

  let list_invalid chain_state =
    Shared.use chain_state.block_store (fun store ->
        Legacy_store.Block.Invalid_block.fold
          store
          ~init:[]
          ~f:(fun hash {level; errors} acc ->
            Lwt.return ((hash, level, errors) :: acc)))

  let unmark_invalid chain_state block =
    Shared.use chain_state.block_store (fun store ->
        Legacy_store.Block.Invalid_block.known store block >>= fun mem ->
        if mem then
          Legacy_store.Block.Invalid_block.remove store block >>= return
        else fail (Block_not_invalid block))

  let is_valid_for_checkpoint block checkpoint =
    let chain_state = block.chain_state in
    Shared.use chain_state.block_store (fun store ->
        Locked_block.is_valid_for_checkpoint
          store
          block.hash
          block.header
          checkpoint)

  let read_predecessor chain_state ~pred hash =
    Shared.use chain_state.block_store (fun store ->
        predecessor_n store hash pred >>= fun hash_opt ->
        let new_hash_opt =
          match hash_opt with
          | Some _ as hash_opt -> hash_opt
          | None ->
              if Block_hash.equal hash chain_state.genesis.block then
                Some chain_state.genesis.block
              else None
        in
        match new_hash_opt with
        | None -> Lwt.fail Not_found
        | Some hash -> (
            Header.read_opt (store, hash) >>= fun header ->
            match header with
            | Some header -> Lwt.return_some {chain_state; hash; header}
            | None -> Lwt.return_none))

  let read chain_state hash =
    Shared.use chain_state.block_store (fun store ->
        Header.read (store, hash) >>=? fun header ->
        return {chain_state; hash; header})

  let read_opt chain_state hash =
    read chain_state hash >>= function
    | Error _ -> Lwt.return_none
    | Ok v -> Lwt.return_some v

  let predecessor {chain_state; header; hash; _} =
    if Block_hash.equal hash header.shell.predecessor then Lwt.return_none
      (* we are at genesis *)
    else read_opt chain_state header.shell.predecessor

  let predecessor_n b n =
    Shared.use b.chain_state.block_store (fun block_store ->
        predecessor_n block_store b.hash n)

  (* EDITED : No context checks *)
  let store chain_state block_header block_header_metadata operations
      operations_metadata block_metadata_hash ops_metadata_hashes
      ({
         context_hash;
         timestamp = _;
         message;
         max_operations_ttl;
         last_allowed_fork_level;
       } :
        Block_validation.validation_store) ~forking_testchain =
    let bytes = Block_header.to_bytes block_header in
    let hash = Block_header.hash_raw bytes in
    fail_unless
      Compare.List_length_with.(
        operations = block_header.shell.validation_passes)
      (error_of_fmt "State.Block.store: invalid operations length")
    >>=? fun () ->
    fail_unless
      Compare.List_length_with.(
        operations_metadata = block_header.shell.validation_passes)
      (error_of_fmt "State.Block.store: invalid operations_data length")
    >>=? fun () ->
    let inconsistent_failure =
      error_of_fmt
        "State.Block.store: inconsistent operations and operations_data"
    in
    (List.for_all2
       ~when_different_lengths:inconsistent_failure
       (fun l1 l2 -> Compare.List_lengths.(l1 = l2))
       operations
       operations_metadata
     |> function
     | Ok _ as ok -> ok
     | Error err -> error err)
    >>?= fun all_have_equal_lengths ->
    error_unless all_have_equal_lengths inconsistent_failure >>?= fun () ->
    (* let's the validator check the consistency... of fitness, level, ... *)
    Shared.use chain_state.block_store (fun store ->
        Legacy_store.Block.Invalid_block.known store hash
        >>= fun known_invalid ->
        fail_when known_invalid (error_of_fmt "Known invalid") >>=? fun () ->
        Legacy_store.Block.Contents.known (store, hash) >>= fun known ->
        if known then return_none
        else
          (* safety check: never ever commit a block that is not compatible
             with the current checkpoint. *)
          let predecessor = block_header.shell.predecessor in
          ( Header.known (store, predecessor) >>= fun valid_predecessor ->
            if not valid_predecessor then Lwt.return_false
            else
              Shared.use chain_state.chain_data (fun chain_data ->
                  Locked_block.acceptable chain_data block_header) )
          >>= fun acceptable_block ->
          fail_unless acceptable_block (Checkpoint_error (hash, None))
          >>=? fun () ->
          let commit = context_hash in
          Header.read (store, predecessor) >>=? fun pred_block ->
          Chain.get_level_indexed_protocol chain_state pred_block
          >>= fun protocol ->
          (match Registered_protocol.get protocol with
          | Some (module Proto) -> return Proto.environment_version
          | None ->
              fail
                (Block_validator_errors.Unavailable_protocol
                   {block = predecessor; protocol}))
          >>=? fun env ->
          let contents =
            {
              header = block_header;
              Legacy_store.Block.message;
              max_operations_ttl;
              last_allowed_fork_level;
              context = commit;
              metadata = block_header_metadata;
            }
          in
          Legacy_store.Block.Contents.store (store, hash) contents >>= fun () ->
          List.iteri_p
            (fun i ops ->
              Legacy_store.Block.Operation_hashes.store
                (store, hash)
                i
                (List.map Operation.hash ops))
            operations
          >>= fun () ->
          List.iteri_p
            (fun i ops ->
              Legacy_store.Block.Operations.store (store, hash) i ops)
            operations
          >>= fun () ->
          List.iteri_p
            (fun i ops ->
              Legacy_store.Block.Operations_metadata.store (store, hash) i ops)
            operations_metadata
          >>= fun () ->
          (match block_metadata_hash with
          | Some block_metadata_hash ->
              Legacy_store.Block.Block_metadata_hash.store
                (store, hash)
                block_metadata_hash
              >|= ok
          | None -> (
              match env with
              | V1 | V2 | V3 | V4 ->
                  fail @@ Missing_block_metadata_hash predecessor
              | V0 -> return_unit))
          >>=? fun () ->
          (match ops_metadata_hashes with
          | Some ops_metadata_hashes ->
              Lwt_list.iteri_s
                (fun i hashes ->
                  Legacy_store.Block.Operations_metadata_hashes.store
                    (store, hash)
                    i
                    hashes)
                ops_metadata_hashes
              >|= ok
          | None -> (
              match env with
              | V1 when pred_block.shell.validation_passes > 0 ->
                  fail @@ Missing_operation_metadata_hashes predecessor
              | _ -> return_unit))
          >>=? fun () ->
          (* Legacy_store predecessors *)
          store_predecessors store hash >>= fun () ->
          (* Update the chain state. *)
          Shared.use chain_state.chain_data (fun chain_data ->
              let store = chain_data.chain_data_store in
              let predecessor = block_header.shell.predecessor in
              Legacy_store.Chain_data.Known_heads.remove store predecessor
              >>= fun () -> Legacy_store.Chain_data.Known_heads.store store hash)
          >>= fun () ->
          (if forking_testchain then
           Shared.use chain_state.global_state.global_data (fun global_data ->
               let genesis = Context.compute_testchain_genesis hash in
               Legacy_store.Forking_block_hash.store
                 global_data.global_store
                 (Context.compute_testchain_chain_id genesis)
                 hash)
          else Lwt.return_unit)
          >>= fun () ->
          let block = {chain_state; hash; header = block_header} in
          Lwt_watcher.notify chain_state.block_watcher block ;
          Lwt_watcher.notify chain_state.global_state.block_watcher block ;
          return_some block)

  let remove block =
    let hash = block.hash in
    let header = block.header in
    protect (fun () ->
        Shared.use block.chain_state.block_store (fun store ->
            Legacy_store.Block.Contents.remove (store, hash) >>= fun () ->
            Legacy_store.Block.Operations.remove_all (store, hash) >>= fun () ->
            Legacy_store.Block.Operations_metadata.remove_all (store, hash)
            >>= fun () ->
            Legacy_store.Block.Operation_hashes.remove_all (store, hash)
            >>= fun () ->
            Shared.use block.chain_state.chain_data (fun chain_data ->
                let store = chain_data.chain_data_store in
                let predecessor = header.shell.predecessor in
                Legacy_store.Chain_data.Known_heads.remove store hash
                >>= fun () ->
                Legacy_store.Chain_data.Known_heads.store store predecessor
                >>= fun () ->
                Legacy_store.Chain_data.In_main_branch.remove (store, hash)
                >>= fun () ->
                Legacy_store.Chain_data.Current_head.read_opt store >>= function
                | Some block_hash when block_hash = hash ->
                    Legacy_store.Chain_data.Current_head.store store predecessor
                | Some _ | None -> Lwt.return_unit)
            >>= fun () -> return_unit))

  let store_invalid chain_state block_header errors =
    let bytes = Block_header.to_bytes block_header in
    let hash = Block_header.hash_raw bytes in
    Shared.use chain_state.block_store (fun store ->
        Header.known (store, hash) >>= fun known_valid ->
        fail_when known_valid (error_of_fmt "Known valid") >>=? fun () ->
        Legacy_store.Block.Invalid_block.known store hash
        >>= fun known_invalid ->
        if known_invalid then return_false
        else
          Legacy_store.Block.Invalid_block.store
            store
            hash
            {level = block_header.shell.level; errors}
          >>= fun () -> return_true)

  let watcher (state : chain_state) =
    Lwt_watcher.create_stream state.block_watcher

  let compute_operation_path hashes =
    let list_hashes = List.map Operation_list_hash.compute hashes in
    Operation_list_list_hash.compute_path list_hashes

  let operation_hashes {chain_state; hash; header} i =
    if i < 0 || header.shell.validation_passes <= i then
      invalid_arg "State.Block.operations" ;
    Shared.use chain_state.block_store (fun store ->
        List.map_p
          (fun n ->
            Legacy_store.Block.Operation_hashes.read_opt (store, hash) n
            >|= WithExceptions.Option.get ~loc:__LOC__)
          (0 -- (header.shell.validation_passes - 1))
        >>= fun hashes ->
        let path = compute_operation_path hashes in
        Lwt.return
          ( WithExceptions.Option.to_exn ~none:Not_found @@ List.nth hashes i,
            path i ))

  let all_operation_hashes {chain_state; hash; header; _} =
    Shared.use chain_state.block_store (fun store ->
        List.map_p
          (fun i ->
            Legacy_store.Block.Operation_hashes.read_opt (store, hash) i
            >|= WithExceptions.Option.get ~loc:__LOC__)
          (0 -- (header.shell.validation_passes - 1)))

  let operations {chain_state; hash; header; _} i =
    if i < 0 || header.shell.validation_passes <= i then
      invalid_arg "State.Block.operations" ;
    Shared.use chain_state.block_store (fun store ->
        List.map_p
          (fun n ->
            Legacy_store.Block.Operation_hashes.read_opt (store, hash) n
            >|= WithExceptions.Option.get ~loc:__LOC__)
          (0 -- (header.shell.validation_passes - 1))
        >>= fun hashes ->
        let path = compute_operation_path hashes in
        Legacy_store.Block.Operations.read_opt (store, hash) i
        >|= WithExceptions.Option.get ~loc:__LOC__
        >>= fun ops -> Lwt.return (ops, path i))

  let operations_metadata {chain_state; hash; header; _} i =
    if i < 0 || header.shell.validation_passes <= i then
      invalid_arg "State.Block.operations_metadata" ;
    Shared.use chain_state.block_store (fun store ->
        Legacy_store.Block.Operations_metadata.read_opt (store, hash) i
        >|= WithExceptions.Option.get ~loc:__LOC__)

  let all_operations {chain_state; hash; header; _} =
    Shared.use chain_state.block_store (fun store ->
        List.map_p
          (fun i ->
            Legacy_store.Block.Operations.read_opt (store, hash) i
            >|= WithExceptions.Option.get ~loc:__LOC__)
          (0 -- (header.shell.validation_passes - 1)))

  let all_operations_metadata {chain_state; hash; header; _} =
    Shared.use chain_state.block_store (fun store ->
        List.map_p
          (fun i ->
            Legacy_store.Block.Operations_metadata.read_opt (store, hash) i
            >|= WithExceptions.Option.get ~loc:__LOC__)
          (0 -- (header.shell.validation_passes - 1)))

  let metadata_hash {chain_state; hash; _} =
    Shared.use chain_state.block_store (fun store ->
        Legacy_store.Block.Block_metadata_hash.read_opt (store, hash))

  let operations_metadata_hashes {chain_state; hash; _} i =
    Shared.use chain_state.block_store (fun store ->
        Legacy_store.Block.Operations_metadata_hashes.read_opt (store, hash) i)

  let all_operations_metadata_hashes {chain_state; hash; header; _} =
    Shared.use chain_state.block_store (fun store ->
        if header.shell.validation_passes = 0 then Lwt.return_none
        else
          Legacy_store.Block.Operations_metadata_hashes.known (store, hash) 0
          >>= function
          | false -> Lwt.return_none
          | true ->
              List.map_p
                (fun i ->
                  Legacy_store.Block.Operations_metadata_hashes.read_opt
                    (store, hash)
                    i
                  >|= WithExceptions.Option.get ~loc:__LOC__)
                (0 -- (header.shell.validation_passes - 1))
              >|= fun hashes -> Some hashes)

  let all_operations_metadata_hash block =
    all_operations_metadata_hashes block
    >|= fun predecessor_ops_metadata_hashes ->
    Option.map
      (fun hashes ->
        List.map Operation_metadata_list_hash.compute hashes
        |> Operation_metadata_list_list_hash.compute)
      predecessor_ops_metadata_hashes

  let context_exn {chain_state; hash; _} =
    Lwt.catch
      (fun () ->
        Shared.use chain_state.block_store (fun block_store ->
            Legacy_store.Block.Contents.read_opt (block_store, hash))
        >|= WithExceptions.Option.get ~loc:__LOC__
        >>= fun {context = commit; _} ->
        Shared.use chain_state.context_index (fun context_index ->
            Context.checkout_exn context_index commit))
      (fun _ -> Lwt.fail Not_found)

  let context_opt {chain_state; hash; _} =
    Shared.use chain_state.block_store (fun block_store ->
        Legacy_store.Block.Contents.read_opt (block_store, hash))
    >|= WithExceptions.Option.get ~loc:__LOC__
    >>= fun {context = commit; _} ->
    Shared.use chain_state.context_index (fun context_index ->
        Context.checkout context_index commit)

  let context block =
    context_opt block >>= function
    | Some context -> return context
    | None -> fail (Block_contents_not_found block.hash)

  let context_exists {chain_state; hash; _} =
    Shared.use chain_state.block_store (fun block_store ->
        Legacy_store.Block.Contents.read_opt (block_store, hash))
    >|= WithExceptions.Option.get ~loc:__LOC__
    >>= fun {context = commit; _} ->
    Shared.use chain_state.context_index (fun context_index ->
        Context.exists context_index commit)

  let protocol_hash ({chain_state; _} as block) =
    Chain.save_point chain_state >>= fun (save_point_level, _) ->
    if Compare.Int32.(level block < save_point_level) then
      Chain.get_level_indexed_protocol chain_state block.header >>= return
    else
      context block >>=? fun context ->
      protect (fun () -> Context.get_protocol context >>= return)

  let protocol_hash_exn ({chain_state; _} as block) =
    Chain.save_point chain_state >>= fun (save_point_level, _) ->
    if Compare.Int32.(level block < save_point_level) then
      Chain.get_level_indexed_protocol chain_state block.header
    else context_exn block >>= fun context -> Context.get_protocol context

  let protocol_level block = block.header.shell.proto_level

  let test_chain block =
    context_exn block >>= fun context ->
    Context.get_test_chain context >>= fun status ->
    let lookup_testchain genesis =
      let chain_id = Context.compute_testchain_chain_id genesis in
      (* otherwise, look in the temporary table *)
      Shared.use block.chain_state.global_state.global_data (fun global_data ->
          Legacy_store.Forking_block_hash.read_opt
            global_data.global_store
            chain_id)
      >>= function
      | Some forking_block_hash ->
          read_opt block.chain_state forking_block_hash >>= fun forking_block ->
          Lwt.return (status, forking_block)
      | None -> Lwt.return (status, None)
    in
    match status with
    | Running {genesis; _} -> lookup_testchain genesis
    | Forking _ -> Lwt.return (status, Some block)
    | Not_running -> Lwt.return (status, None)

  let known chain_state hash =
    Shared.use chain_state.block_store (fun store ->
        Header.known (store, hash) >>= fun known ->
        if known then Lwt.return_true
        else Legacy_store.Block.Invalid_block.known store hash)

  let block_validity chain_state block : Block_locator.validity Lwt.t =
    known chain_state block >>= function
    | false ->
        if Block_hash.equal block (Chain.faked_genesis_hash chain_state) then
          Lwt.return Block_locator.Known_valid
        else Lwt.return Block_locator.Unknown
    | true -> (
        known_invalid chain_state block >>= function
        | true -> Lwt.return Block_locator.Known_invalid
        | false -> Lwt.return Block_locator.Known_valid)

  let known_ancestor chain_state locator =
    Shared.use chain_state.global_state.global_data (fun {global_store; _} ->
        Legacy_store.Configuration.History_mode.read_opt global_store
        >|= WithExceptions.Option.get ~loc:__LOC__)
    >>= fun history_mode ->
    Block_locator.unknown_prefix ~is_known:(block_validity chain_state) locator
    >>= function
    | (Known_valid, prefix_locator) -> Lwt.return_some prefix_locator
    | (Known_invalid, _) -> Lwt.return_none
    | (Unknown, _) -> (
        match history_mode with
        | Archive -> Lwt.return_none
        | Rolling | Full -> Lwt.return_some locator)

  (* Hypothesis : genesis' predecessor is itself. *)
  let get_rpc_directory ({chain_state; _} as block) =
    read_opt chain_state block.header.shell.predecessor >>= function
    | None -> Lwt.return_none (* assert false *)
    | Some pred when equal pred block -> Lwt.return_none (* genesis *)
    | Some pred -> (
        protocol_hash_exn pred >>= fun protocol ->
        match
          Protocol_hash.Table.find chain_state.block_rpc_directories protocol
        with
        | None -> Lwt.return_none
        | Some map ->
            protocol_hash_exn block >>= fun next_protocol ->
            Lwt.return (Protocol_hash.Map.find next_protocol map))

  let set_rpc_directory ({chain_state; _} as block) dir =
    read_opt chain_state block.header.shell.predecessor
    >|= WithExceptions.Option.get ~loc:__LOC__
    >>= fun pred ->
    protocol_hash_exn block >>= fun next_protocol ->
    Chain.save_point chain_state >>= fun (save_point_level, _) ->
    (if Compare.Int32.(level pred < save_point_level) then
     Chain.get_level_indexed_protocol chain_state (header pred)
    else protocol_hash_exn pred)
    >>= fun protocol ->
    let map =
      Option.value
        ~default:Protocol_hash.Map.empty
        (Protocol_hash.Table.find chain_state.block_rpc_directories protocol)
    in
    Protocol_hash.Table.replace
      chain_state.block_rpc_directories
      protocol
      (Protocol_hash.Map.add next_protocol dir map) ;
    return_unit

  let get_header_rpc_directory chain_state header =
    Shared.use chain_state.block_store (fun block_store ->
        Header.read_opt (block_store, header.Block_header.shell.predecessor)
        >>= function
        | None -> Lwt.return_none (* caboose *)
        | Some pred when Block_header.equal pred header ->
            Lwt.return_none (* genesis *)
        | Some pred -> (
            Chain.get_level_indexed_protocol chain_state header
            >>= fun protocol ->
            match
              Protocol_hash.Table.find
                chain_state.header_rpc_directories
                protocol
            with
            | None -> Lwt.return_none
            | Some map ->
                Chain.get_level_indexed_protocol chain_state pred
                >>= fun next_protocol ->
                Lwt.return (Protocol_hash.Map.find next_protocol map)))

  let set_header_rpc_directory chain_state header dir =
    Shared.use chain_state.block_store (fun block_store ->
        Header.read_opt (block_store, header.Block_header.shell.predecessor)
        >>= function
        | None -> assert false
        | Some pred ->
            Chain.get_level_indexed_protocol chain_state header
            >>= fun next_protocol ->
            Chain.get_level_indexed_protocol chain_state pred
            >>= fun protocol ->
            let map =
              Option.value
                ~default:Protocol_hash.Map.empty
                (Protocol_hash.Table.find
                   chain_state.header_rpc_directories
                   protocol)
            in
            Protocol_hash.Table.replace
              chain_state.header_rpc_directories
              protocol
              (Protocol_hash.Map.add next_protocol dir map) ;
            Lwt.return_unit)
end

let watcher (state : global_state) =
  Lwt_watcher.create_stream state.block_watcher

let read_block {global_data; _} hash =
  Shared.use global_data (fun {chains; _} ->
      Chain_id.Table.fold_s
        (fun _chain_id chain_state acc ->
          match acc with
          | Some _ -> Lwt.return acc
          | None -> (
              Block.read_opt chain_state hash >>= function
              | None -> Lwt.return acc
              | Some block -> Lwt.return_some block))
        chains
        None)

let read_block_exn t hash =
  read_block t hash >>= function
  | None -> Lwt.fail Not_found
  | Some b -> Lwt.return b

let update_testchain block ~testchain_state =
  update_chain_data block.chain_state (fun _ chain_data ->
      Lwt.return
        (Some {chain_data with test_chain = Some testchain_state.chain_id}, ()))

let fork_testchain block chain_id genesis_hash genesis_header protocol
    expiration =
  Shared.use block.chain_state.global_state.global_data (fun data ->
      let chain_store = Legacy_store.Chain.get data.global_store chain_id in
      let block_store = Legacy_store.Block.get chain_store in
      Legacy_store.Block.Contents.store
        (block_store, genesis_hash)
        {
          header = genesis_header;
          Legacy_store.Block.message = Some "Genesis";
          max_operations_ttl = 0;
          context = genesis_header.shell.context;
          metadata = Bytes.create 0;
          last_allowed_fork_level = 0l;
        }
      >>= fun () ->
      let genesis =
        {
          Genesis.block = genesis_hash;
          time = genesis_header.shell.timestamp;
          protocol;
        }
      in
      Chain.locked_create
        block.chain_state.global_state
        data
        chain_id
        ~expiration
        genesis
        genesis_header
      >>= fun testchain_state ->
      Legacy_store.Chain.Protocol_info.store
        chain_store
        genesis_header.shell.proto_level
        (protocol, genesis_header.shell.level)
      >>= fun () ->
      update_testchain block ~testchain_state >>= fun () ->
      return testchain_state)

let best_known_head_for_checkpoint chain_state checkpoint =
  Shared.use chain_state.block_store (fun store ->
      Shared.use chain_state.chain_data (fun data ->
          let head_hash = data.data.current_head.hash in
          let head_header = data.data.current_head.header in
          Locked_block.is_valid_for_checkpoint
            store
            head_hash
            head_header
            checkpoint
          >>= fun valid ->
          if valid then Lwt.return data.data.current_head
          else
            let find_valid_predecessor hash =
              Header.read_opt (store, hash)
              >|= WithExceptions.Option.get ~loc:__LOC__
              >>= fun header ->
              if Compare.Int32.(header.shell.level < checkpoint.shell.level)
              then Lwt.return {hash; chain_state; header}
              else
                predecessor_n
                  store
                  hash
                  (1
                  + (Int32.to_int
                    @@ Int32.sub header.shell.level checkpoint.shell.level))
                >|= WithExceptions.Option.get ~loc:__LOC__
                >>= fun pred ->
                Header.read_opt (store, pred)
                >|= WithExceptions.Option.get ~loc:__LOC__
                >>= fun pred_header ->
                Lwt.return {hash = pred; chain_state; header = pred_header}
            in
            Legacy_store.Chain_data.Known_heads.read_all data.chain_data_store
            >>= fun heads ->
            Header.read_opt (store, chain_state.genesis.block)
            >|= WithExceptions.Option.get ~loc:__LOC__
            >>= fun genesis_header ->
            let genesis =
              {
                hash = chain_state.genesis.block;
                chain_state;
                header = genesis_header;
              }
            in
            Block_hash.Set.fold_s
              (fun head best ->
                find_valid_predecessor head >|= fun pred ->
                if
                  Fitness.(
                    pred.header.shell.fitness > best.header.shell.fitness)
                then pred
                else best)
              heads
              genesis))

module Protocol = struct
  include Protocol

  let known global_state hash =
    Shared.use global_state.protocol_store (fun store ->
        Legacy_store.Protocol.Contents.known store hash)

  let read global_state hash =
    Shared.use global_state.protocol_store (fun store ->
        Legacy_store.Protocol.Contents.read store hash)

  let read_opt global_state hash =
    Shared.use global_state.protocol_store (fun store ->
        Legacy_store.Protocol.Contents.read_opt store hash)

  let read_raw global_state hash =
    Shared.use global_state.protocol_store (fun store ->
        Legacy_store.Protocol.RawContents.read (store, hash))

  let read_raw_opt global_state hash =
    Shared.use global_state.protocol_store (fun store ->
        Legacy_store.Protocol.RawContents.read_opt (store, hash))

  let store global_state p =
    let bytes = Protocol.to_bytes p in
    let hash = Protocol.hash_raw bytes in
    Shared.use global_state.protocol_store (fun store ->
        Legacy_store.Protocol.Contents.known store hash >>= fun known ->
        if known then Lwt.return_none
        else
          Legacy_store.Protocol.RawContents.store (store, hash) bytes
          >>= fun () ->
          Lwt_watcher.notify global_state.protocol_watcher hash ;
          Lwt.return_some hash)

  let remove global_state hash =
    Shared.use global_state.protocol_store (fun store ->
        Legacy_store.Protocol.Contents.known store hash >>= fun known ->
        if known then Lwt.return_false
        else
          Legacy_store.Protocol.Contents.remove store hash >>= fun () ->
          Lwt.return_true)

  let list global_state =
    Shared.use global_state.protocol_store (fun store ->
        Legacy_store.Protocol.Contents.fold_keys
          store
          ~init:Protocol_hash.Set.empty
          ~f:(fun x acc -> Lwt.return (Protocol_hash.Set.add x acc)))

  let watcher (state : global_state) =
    Lwt_watcher.create_stream state.protocol_watcher
end

module Current_mempool = struct
  let set chain_state ~head mempool =
    update_chain_data chain_state (fun _chain_data_store data ->
        if Block_hash.equal head (Block.hash data.current_head) then
          Lwt.return (Some {data with current_mempool = mempool}, ())
        else Lwt.return (None, ()))

  let get chain_state =
    read_chain_data chain_state (fun _chain_data_store data ->
        Lwt.return (Block.header data.current_head, data.current_mempool))
end

let may_create_chain ~commit_genesis state chain_id genesis =
  Chain.get state chain_id >>= function
  | Ok chain -> return chain
  | Error _ ->
      Chain.create
        ~commit_genesis
        ~allow_forked_chain:true
        state
        genesis
        chain_id

let read global_store context_index main_chain =
  let global_data =
    {chains = Chain_id.Table.create 17; global_store; context_index}
  in
  let state =
    {
      global_data = Shared.create global_data;
      protocol_store = Shared.create @@ Legacy_store.Protocol.get global_store;
      main_chain;
      protocol_watcher = Lwt_watcher.create_input ();
      block_watcher = Lwt_watcher.create_input ();
    }
  in
  Chain.read_all state >>=? fun () -> return state

(* FIXME: this should not be hard-coded *)
let max_locator_size = 200

let compute_locator_from_hash chain_state ?(max_size = max_locator_size)
    ?min_level (head_hash, head_header) seed =
  Shared.use chain_state.block_store (fun block_store ->
      read_chain_data chain_state (fun _ chain_data ->
          match min_level with
          | None -> Lwt.return chain_data.caboose
          | Some level -> (
              let head_level = head_header.Block_header.shell.level in
              let distance = Int32.sub head_level level in
              predecessor_n block_store head_hash (Int32.to_int distance)
              >>= function
              | None -> Lwt.return chain_data.caboose
              | Some hash -> Lwt.return (level, hash)))
      >>= fun (_lvl, caboose) ->
      let get_predecessor =
        match min_level with
        | None -> predecessor_n block_store
        | Some min_level -> (
            fun block_hash distance ->
              predecessor_n block_store block_hash distance >>= function
              | None -> Lwt.return_none
              | Some pred_hash -> (
                  Header.read_opt (block_store, pred_hash) >>= function
                  | None -> Lwt.return_none
                  | Some pred_header
                    when Compare.Int32.(pred_header.shell.level < min_level) ->
                      Lwt.return_none
                  | Some _ -> Lwt.return_some pred_hash))
      in
      Block_locator.compute
        ~get_predecessor
        ~caboose
        ~size:max_size
        head_hash
        head_header
        seed)

let compute_locator chain ?max_size head seed =
  compute_locator_from_hash chain ?max_size (head.hash, Block.header head) seed

let compute_protocol_locator chain_state ?max_size ~proto_level seed =
  Chain.store chain_state >>= fun global_store ->
  let chain_store = Legacy_store.Chain.get global_store chain_state.chain_id in
  read_chain_data chain_state (fun _chain_store chain_data ->
      Legacy_store.Chain.Protocol_info.read_opt chain_store proto_level
      >>= function
      | None -> Lwt.return_none
      | Some (_protocol_hash, block_activation_level) -> (
          (* proto level's lower bound found, now retrieving the upper bound *)
          let head_proto_level = Block.protocol_level chain_data.current_head in
          if Compare.Int.(proto_level = head_proto_level) then
            Lwt.return_some
              ( block_activation_level,
                Block.
                  (hash chain_data.current_head, header chain_data.current_head)
              )
          else
            Legacy_store.Chain.Protocol_info.read_opt
              chain_store
              (succ proto_level)
            >>= function
            | None -> Lwt.return_none
            | Some (_, next_activation_level) -> (
                let last_level_in_protocol =
                  Int32.(pred next_activation_level)
                in
                let delta =
                  Int32.(
                    sub
                      (Block.level chain_data.current_head)
                      last_level_in_protocol)
                in
                Shared.use chain_state.block_store (fun block_store ->
                    predecessor_n
                      block_store
                      (Block.hash chain_data.current_head)
                      (Int32.to_int delta))
                >>= function
                | None -> Lwt.return_none
                | Some pred_hash ->
                    Shared.use chain_state.block_store (fun block_store ->
                        Header.read_opt (block_store, pred_hash) >>= function
                        | None -> Lwt.return_none
                        | Some pred_header ->
                            Lwt.return_some
                              (block_activation_level, (pred_hash, pred_header)))
                )))
  >>= function
  | None -> Lwt.return_none
  | Some (block_activation_level, upper_block) ->
      compute_locator_from_hash
        chain_state
        ?max_size
        ~min_level:block_activation_level
        upper_block
        seed
      >>= Lwt.return_some

type error +=
  | Incorrect_legacy_history_mode_switch of {
      previous_mode : History_mode.Legacy.t;
      next_mode : History_mode.Legacy.t;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"node_config_file.incorrect_legacy_history_mode_switch"
    ~title:"Incorrect legacy history mode switch"
    ~description:"Incorrect legacy history mode switch."
    ~pp:(fun ppf (prev, next) ->
      Format.fprintf
        ppf
        "@[cannot switch from legacy history mode %a mode to %a mode@]"
        History_mode.Legacy.pp
        prev
        History_mode.Legacy.pp
        next)
    (Data_encoding.obj2
       (Data_encoding.req "previous_mode" History_mode.Legacy.encoding)
       (Data_encoding.req "next_mode" History_mode.Legacy.encoding))
    (function
      | Incorrect_legacy_history_mode_switch x ->
          Some (x.previous_mode, x.next_mode)
      | _ -> None)
    (fun (previous_mode, next_mode) ->
      Incorrect_legacy_history_mode_switch {previous_mode; next_mode})

let init ?patch_context ?commit_genesis ?(store_mapsize = 40_960_000_000L)
    ~store_root ~context_root ?history_mode ?(readonly = false)
    (genesis : Genesis.t) =
  Legacy_store.init ~mapsize:store_mapsize store_root >>=? fun global_store ->
  (match commit_genesis with
  | Some commit_genesis ->
      Context.init ~readonly:true ?patch_context context_root
      >>= fun context_index -> Lwt.return (context_index, commit_genesis)
  | None ->
      Context.init ~readonly ?patch_context context_root
      >>= fun context_index ->
      let commit_genesis ~chain_id =
        Context.commit_genesis
          context_index
          ~chain_id
          ~time:genesis.time
          ~protocol:genesis.protocol
      in
      Lwt.return (context_index, commit_genesis))
  >>= fun (context_index, commit_genesis) ->
  let chain_id = Chain_id.of_block_hash genesis.Genesis.block in
  read global_store context_index chain_id >>=? fun state ->
  may_create_chain ~commit_genesis state chain_id genesis
  >>=? fun main_chain_state ->
  (Legacy_store.Configuration.History_mode.read_opt global_store >>= function
   | None ->
       let mode = Option.value ~default:History_mode.Legacy.Full history_mode in
       Legacy_store.Configuration.History_mode.store global_store mode
       >>= fun () -> return mode
   | Some previous_history_mode -> (
       match history_mode with
       | None -> return previous_history_mode
       | Some history_mode ->
           if history_mode <> previous_history_mode then
             fail
               (Incorrect_legacy_history_mode_switch
                  {
                    previous_mode = previous_history_mode;
                    next_mode = history_mode;
                  })
           else return history_mode))
  >>=? fun history_mode ->
  return (state, main_chain_state, context_index, history_mode)

let history_mode {global_data; _} =
  Shared.use global_data (fun {global_store; _} ->
      Legacy_store.Configuration.History_mode.read_opt global_store
      >|= WithExceptions.Option.get ~loc:__LOC__)

let close {global_data; _} =
  Shared.use global_data (fun {global_store; context_index; _} ->
      Legacy_store.close global_store ;
      Context.close context_index)
