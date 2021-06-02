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
open Store_events

(* A non-empty store is considered consistent if the following
   invariants hold:

   - genesis, caboose, savepoint, checkpoint, current_head,
   alternate_heads associated files exists, are decodable and the
   blocks they point to may be read in the block store and are
   consistent with their definition;

   - genesis ≤ caboose ≤ savepoint ≤ [cementing_highwatermark] ≤
   checkpoint ≤ all(alternate_heads ∪ current_head)

   Hypothesis:

   - We suppose that the stores have not been modified outside of the
   store.
 *)

(* [check_cementing_highwatermark ~chain_dir block_store] checks that
   the cementing_highwatermark is consistent with the cemented
   store. *)
let check_cementing_highwatermark ~cementing_highwatermark block_store =
  let cemented_store = Block_store.cemented_block_store block_store in
  let highest_cemented_level =
    Cemented_block_store.get_highest_cemented_level cemented_store
  in
  match (highest_cemented_level, cementing_highwatermark) with
  | (Some highest_cemented_level, Some cementing_highwatermark) ->
      fail_unless
        (Int32.equal highest_cemented_level cementing_highwatermark)
        (Inconsistent_cementing_highwatermark
           {highest_cemented_level; cementing_highwatermark})
  | (Some _, None) ->
      (* Can be the case after a snapshot import *)
      return_unit
  | (None, Some _) ->
      (* Can be the case in Rolling 0 *)
      return_unit
  | (None, None) -> return_unit

let is_block_stored block_store (descriptor, expected_metadata, block_name) =
  Block_store.read_block
    ~read_metadata:expected_metadata
    block_store
    (Block (fst descriptor, 0))
  >>=? function
  | None -> fail (Unexpected_missing_block {block_name})
  | Some b ->
      if expected_metadata then
        match Block_repr.metadata b with
        | None -> fail (Unexpected_missing_block_metadata {block_name})
        | Some _ -> return_unit
      else return_unit

let check_invariant ~genesis ~caboose ~savepoint ~cementing_highwatermark
    ~checkpoint ~current_head ~alternate_heads =
  let ( <= ) descr descr' = Compare.Int32.(snd descr <= snd descr') in
  let invariant_holds =
    genesis <= caboose && caboose <= savepoint && savepoint <= checkpoint
    && checkpoint <= current_head
    && List.for_all
         (fun alternate_head -> checkpoint <= alternate_head)
         alternate_heads
    &&
    match cementing_highwatermark with
    | Some ch -> Compare.Int32.(ch <= snd checkpoint)
    | None -> true
  in
  fail_unless invariant_holds Bad_ordering_invariant

(* [check_consistency ~store_dir genesis] aims to provide a quick
   check (in terms of execution time) which checks that files may be
   read and they are consistent w.r.t to the given invariant.

   Hypothesis: an existing store is provided. *)
let check_consistency chain_dir genesis =
  (* Try loading all the block's data files *)
  Stored_data.load (Naming.genesis_block_file chain_dir)
  >>=? fun genesis_data ->
  Stored_data.get genesis_data >>= fun genesis_block ->
  fail_unless
    (Block_hash.equal (Block_repr.hash genesis_block) genesis.Genesis.block)
    (Inconsistent_genesis
       {expected = genesis.block; got = Block_repr.hash genesis_block})
  >>=? fun () ->
  Stored_data.load (Naming.caboose_file chain_dir) >>=? fun caboose_data ->
  Stored_data.get caboose_data >>= fun caboose ->
  Stored_data.load (Naming.savepoint_file chain_dir) >>=? fun savepoint_data ->
  Stored_data.get savepoint_data >>= fun savepoint ->
  Stored_data.load (Naming.checkpoint_file chain_dir)
  >>=? fun checkpoint_data ->
  Stored_data.get checkpoint_data >>= fun checkpoint ->
  Stored_data.load (Naming.current_head_file chain_dir)
  >>=? fun current_head_data ->
  Stored_data.get current_head_data >>= fun current_head ->
  Stored_data.load (Naming.alternate_heads_file chain_dir)
  >>=? fun alternate_heads_data ->
  Stored_data.get alternate_heads_data >>= fun alternate_heads ->
  Stored_data.load (Naming.protocol_levels_file chain_dir)
  >>=? fun _protocol_levels_data ->
  Stored_data.load (Naming.invalid_blocks_file chain_dir)
  >>=? fun _invalid_blocks_data ->
  Stored_data.load (Naming.forked_chains_file chain_dir)
  >>=? fun _forked_chains_data ->
  Stored_data.load (Naming.target_file chain_dir) >>=? fun _target_data ->
  (* Open the store and try to read the blocks *)
  (* [~readonly:false] to recover from a potential interrupted merge *)
  Block_store.load chain_dir ~genesis_block ~readonly:true
  >>=? fun block_store ->
  Lwt.finalize
    (fun () ->
      (* TODO should we check context as well? *)
      let genesis_descr = Block_repr.descriptor genesis_block in
      let expected_blocks =
        [
          (genesis_descr, false, "genesis");
          (caboose, false, "caboose");
          (savepoint, true, "savepoint");
          (* is this really true? *)
          (checkpoint, true, "checkpoint");
          (current_head, true, "current_head");
        ]
        @ List.map
            (fun descr -> (descr, true, "alternate_head"))
            alternate_heads
      in
      List.iter_es
        (fun block -> is_block_stored block_store block)
        expected_blocks
      >>=? fun () ->
      Stored_data.load (Naming.cementing_highwatermark_file chain_dir)
      >>=? fun cementing_highwatermark_data ->
      Stored_data.get cementing_highwatermark_data
      >>= fun cementing_highwatermark ->
      check_cementing_highwatermark ~cementing_highwatermark block_store
      >>=? fun () ->
      check_invariant
        ~genesis:genesis_descr
        ~caboose
        ~savepoint
        ~cementing_highwatermark
        ~checkpoint
        ~current_head
        ~alternate_heads
      >>=? fun () -> return_unit)
    (fun () -> Block_store.close block_store)

let fix_floating_stores chain_dir =
  let store_kinds = [Floating_block_store.RO; RW; RW_TMP; RO_TMP] in
  Lwt_list.partition_s
    (fun kind -> Floating_block_store.all_files_exists chain_dir kind)
    store_kinds
  >>= fun (existing_floating_stores, incomplete_floating_stores) ->
  (* Remove potentially partial floating stores *)
  Lwt_list.iter_s
    (fun kind ->
      let dir_path =
        Naming.floating_blocks_dir chain_dir kind |> Naming.dir_path
      in
      Lwt_utils_unix.remove_dir dir_path)
    incomplete_floating_stores
  >>= fun () ->
  List.iter_es
    (fun kind -> Floating_block_store.fix_integrity chain_dir kind)
    existing_floating_stores
  >>=? fun () ->
  Event.(emit fix_floating_stores ()) >>= fun () -> return_unit

(* [fix_head ~chain_dir block_store genesis_block] iter through the
   floating blocks and set, as head, the fittest block found. *)
let fix_head block_store genesis_block =
  let floating_stores = Block_store.floating_block_stores block_store in
  List.map_es
    (Floating_block_store.fold_left_s
       (fun last_max block ->
         let block_fitness = Block_repr.fitness block in
         let last_max_fitness = Block_repr.fitness last_max in
         if Fitness.(block_fitness > last_max_fitness) then return block
         else return last_max)
       genesis_block)
    floating_stores
  >|=? List.fold_left
         (fun e1 e2 ->
           if Fitness.(Block_repr.fitness e1 > Block_repr.fitness e2) then e1
           else e2)
         genesis_block
  >>=? fun floating_head ->
  (* Find the highest block from cemented *)
  let cemented_block_store = Block_store.cemented_block_store block_store in
  (match Cemented_block_store.cemented_blocks_files cemented_block_store with
  | None -> return floating_head
  | Some cemented_block_files ->
      let cemented_block_files = Array.to_list cemented_block_files in
      (* If the fittest of the floating blocks is genesis (genesis is the
         result of the unsuccessful search from the floatings) and there
         is at least one cemented file, then it means that the floating
         blocks were truncated. The head is then chosen as the highest
         cemented block known. *)
      if
        List.length cemented_block_files <> 0
        && Block_hash.equal
             (Block_repr.hash genesis_block)
             (Block_repr.hash floating_head)
      then
        let highest_cycle =
          List.last_opt cemented_block_files
          |> WithExceptions.Option.get ~loc:__LOC__
        in
        let highest_cemented_level =
          highest_cycle.Cemented_block_store.end_level
        in
        Cemented_block_store.get_cemented_block_by_level
          cemented_block_store
          ~read_metadata:true
          highest_cemented_level
        >|=? WithExceptions.Option.get ~loc:__LOC__
        (* If the highest of the floating blocks is genesis and there is
           at least one cemented file, then it means that the floating
           blocks were truncated. The head is then chosen as the highest
           cemented block known. *)
      else return floating_head)
  >>=? fun head ->
  (* Make sure that the infered head have metadata *)
  (Block_store.read_block_metadata
     block_store
     (Block_store.Block (Block_repr.hash floating_head, 0))
   >>=? function
   | None -> fail (Corrupted_store "infered head must have metadata")
   | Some _ -> return_unit)
  >>=? fun () ->
  Event.(emit fix_head (Block_repr.descriptor head)) >>= fun () -> return head

(* [fix_savepoint_and_caboose ~chain_dir block_store head]
   Fix the savepoint by setting it to the lowest block with metadata.
   Assumption:
   - block store is valid and available.

   Fix the caboose by setting it to the lowest block.
   Assumption:
   - block store is valid and available. *)
let fix_savepoint_and_caboose chain_dir block_store head =
  (* Search for the lowest block with metadata (for savepoint) and the
     lowest block (for caboose) from the cemented store. *)
  let lowest_cemented_block cemented_block_files =
    match List.hd cemented_block_files with
    | None -> None
    | Some {Cemented_block_store.start_level; _} -> Some start_level
  in
  (* Returns the lowest block level of a cemented metadata file. *)
  let lowest_entry
      (metadata_file : [`Cemented_blocks_metadata] Naming.file option) =
    try
      match metadata_file with
      | None -> return_none
      | Some metadata_file -> (
          let metadata_file_path = Naming.file_path metadata_file in
          let in_file = Zip.open_in metadata_file_path in
          let entries = Zip.entries in_file in
          let asc_entries =
            List.sort
              (fun {Zip.filename = a; _} {filename = b; _} ->
                Int.compare (int_of_string a) (int_of_string b))
              entries
          in
          match List.hd asc_entries with
          | None ->
              (* A metadata file is never empty *)
              assert false
          | Some {Zip.filename; _} -> return_some (Int32.of_string filename))
    with _exn ->
      (* FIXME Is it ok? Or should we take the successor of the
         end_level of the cycle as a savepoint as it is a complete
         metadata file. However, the current metadata file is
         invalid/broken and it should be reported. *)
      trace (Exn _exn)
      @@ fail (Corrupted_store "Failed to find a valid savepoint")
  in
  (* Returns the lowest cemented metadata stored. *)
  let cemented_dir = Naming.cemented_blocks_dir chain_dir in
  let cemented_metadata_dir =
    Naming.cemented_blocks_metadata_dir cemented_dir
  in
  let lowest_cemented_metadata last_cycle =
    let rec aux last_cycle = function
      | [] ->
          let metadata_file =
            Option.map
              (Naming.cemented_blocks_metadata_file cemented_metadata_dir)
              last_cycle
          in
          lowest_entry metadata_file
      | ({file; start_level; _} : Cemented_block_store.cemented_blocks_file)
        :: tl ->
          let metadata_file =
            Naming.cemented_blocks_metadata_file cemented_metadata_dir file
          in
          if Sys.file_exists (Naming.file_path metadata_file) then
            (* If we reach the cycle starting at level 0 and the
               metadata exists, then the savepoint is the genesis. It
               is the case in archive mode and when the offset window
               includes the genesis. *)
            if start_level = 0l then return_some 0l else aux (Some file) tl
          else
            (* As metadata files are ordered and contiguous, we can
               stop and search for the lowest entry in that metadata
               file. Indeed, from an imported snapshot, the metadata
               file could be partially filled. We must seek for the
               first entry which stand for the first block of that
               cycle which contains metadata.*)
            let metadata_file =
              Option.map
                (Naming.cemented_blocks_metadata_file cemented_metadata_dir)
                last_cycle
            in
            lowest_entry metadata_file
    in
    aux last_cycle
  in
  (* Returns both the lowest block and the lowest block with metadata
     from the floating block store.*)
  let lowest_floating_blocks floating_stores =
    List.map_es
      (Floating_block_store.fold_left_s
         (fun (last_min, last_min_with_metadata) block ->
           let lowest_block =
             match last_min with
             | None -> Some (Block_repr.level block)
             | Some last_min -> Some (min last_min (Block_repr.level block))
           in
           let lowest_block_with_metadata =
             match last_min_with_metadata with
             | None -> (
                 match Block_repr.metadata block with
                 | Some _ -> Some (Block_repr.level block)
                 | None -> None)
             | Some last_min_with_metadata -> (
                 match Block_repr.metadata block with
                 | Some _ ->
                     Some (min last_min_with_metadata (Block_repr.level block))
                 | None -> Some last_min_with_metadata)
           in
           return (lowest_block, lowest_block_with_metadata))
         (None, None))
      floating_stores
    >>=? fun l ->
    let min l = List.fold_left (Option.merge min) None l in
    let (lw, lwm) = List.split l in
    (* If we have failed getting a block with metadata from both the
       RO and RW floating stores, then it is not possible to determine
       a savepoint. The store is broken. *)
    let lw = min lw in
    let lwm = min lwm in
    return (lw, lwm)
  in
  let cemented_block_store = Block_store.cemented_block_store block_store in
  let cemented_block_files =
    match Cemented_block_store.cemented_blocks_files cemented_block_store with
    | None -> []
    | Some arr -> Array.to_list arr
  in
  lowest_cemented_metadata None (List.rev cemented_block_files)
  >>=? fun cemented_savepoint_candidate ->
  let cemented_caboose_candidate = lowest_cemented_block cemented_block_files in
  let floating_stores = Block_store.floating_block_stores block_store in
  (match (cemented_savepoint_candidate, cemented_caboose_candidate) with
  | (Some cemented_savepoint, Some caboose) ->
      (* Cemented candidates are available. However, we must check
         that the lowest block with metadata from the floating store
         is not lower than the cemented candidate and thus, a better
         candidate. It can be the case when [checkpoint_level -
         max_op_ttl < lowest_cemented_level_with_metadata]. *)
      lowest_floating_blocks floating_stores
      >>=? fun (_, lowest_floating_with_metadata) ->
      let sp =
        match lowest_floating_with_metadata with
        | Some lowest_floating_with_metadata ->
            if lowest_floating_with_metadata < cemented_savepoint then
              lowest_floating_with_metadata
            else cemented_savepoint
        | None -> cemented_savepoint
      in
      return (sp, caboose)
  | (None, Some caboose_level) ->
      (* No cemented cycle with metadata but some cycles. Search for
         the savepoint in the floating blocks. *)
      lowest_floating_blocks floating_stores
      >>=? fun (_, lowest_floating_with_metadata) ->
      (match lowest_floating_with_metadata with
      | Some lvl -> return lvl
      | None -> fail (Corrupted_store "Failed to find a valid savepoint"))
      >>=? fun savepoint_level -> return (savepoint_level, caboose_level)
  | (None, None) ->
      (* No cycle found. Searching for savepoint and caboose in the
         floating block store.*)
      lowest_floating_blocks floating_stores
      >>=? fun (lowest_floating, lowest_floating_with_metadata) ->
      (match lowest_floating_with_metadata with
      | Some lvl -> return lvl
      | None -> fail (Corrupted_store "Failed to find a valid savepoint"))
      >>=? fun savepoint_level ->
      (match lowest_floating with
      | Some lvl -> return lvl
      | None -> fail (Corrupted_store "Failed to find a valid caboose"))
      >>=? fun caboose_level -> return (savepoint_level, caboose_level)
  | (Some _, None) ->
      (* Inconsistent as a cemented cycle with metadata implies that
         the caboose candidate is known. *)
      assert false)
  >>=? fun (savepoint_level, caboose_level) ->
  (* Setting the savepoint *)
  (Block_store.read_block
     ~read_metadata:false
     block_store
     (Block_store.Block
        ( Block_repr.hash head,
          Int32.(to_int (sub (Block_repr.level head) savepoint_level)) ))
   >>=? function
   | Some b ->
       let savepoint = (Block_repr.hash b, Block_repr.level b) in
       Stored_data.write_file (Naming.savepoint_file chain_dir) savepoint
       >>=? fun () ->
       Event.(emit fix_savepoint savepoint) >>= fun () -> return savepoint
   | None ->
       (* Assumption: the head is valid. Thus, at least the head
          (with metadata) must be a valid candidate for the
          savepoint. *)
       assert false)
  >>=? fun savepoint ->
  (* Setting the caboose *)
  (Block_store.read_block
     ~read_metadata:false
     block_store
     (Block_store.Block
        ( Block_repr.hash head,
          Int32.(to_int (sub (Block_repr.level head) caboose_level)) ))
   >>=? function
   | Some b ->
       let caboose = (Block_repr.hash b, Block_repr.level b) in
       Stored_data.write_file (Naming.caboose_file chain_dir) caboose
       >>=? fun () ->
       Event.(emit fix_caboose caboose) >>= fun () -> return caboose
   | None -> fail (Corrupted_store "Failed to find a valid caboose"))
  >>=? fun caboose -> return (savepoint, caboose)

(* [fix_checkpoint ~chain_dir block_store head] fixes the checkpoint
   by setting it to the lowest block with metadata which is higher
   that the last allowed fork level of the current head (and <=
   head_level).
  Assumptions:
   - head is valid,
   - savepoint is valid,
   - block store is valid and available. *)
let fix_checkpoint chain_dir block_store head =
  let set_checkpoint head =
    (match Block_repr.metadata head with
    | Some m -> return m.last_allowed_fork_level
    | None ->
        (*Assumption: head must have metadata *)
        fail (Corrupted_store "Missing metadata for head: Broken invariant."))
    >>=? fun head_lafl ->
    let head_hash = Block_repr.hash head in
    (* Returns the lowest block with metadata *)
    let rec find_lbwm block_level =
      Block_store.read_block
        ~read_metadata:true
        block_store
        (Block_store.Block
           (head_hash, Int32.(to_int (sub (Block_repr.level head) block_level))))
      >>=? function
      | Some block -> (
          if
            (* The lowest block with metadata is never higher thant current head. *)
            Block_repr.level block = Block_repr.level head
          then return head
          else
            match Block_repr.metadata block with
            | Some _metadata -> return block
            | None -> find_lbwm (Int32.succ block_level))
      | None ->
          fail
            (Corrupted_store
               "No block with metadata found. At least the head must have \
                metadata.")
    in
    find_lbwm head_lafl >>=? fun lbwm ->
    let checkpoint = (Block_repr.hash lbwm, Block_repr.level lbwm) in
    Stored_data.write_file (Naming.checkpoint_file chain_dir) checkpoint
    >>=? fun () -> return checkpoint
  in
  set_checkpoint head >>=? fun checkpoint ->
  Event.(emit fix_checkpoint checkpoint) >>= fun () -> return checkpoint

(* [fix_protocol_levels context_index block_store genesis_header]
   fixes protocol levels table by searching for all the protocol
   levels in the block store (cemented and floating). Fixing this
   table is possible in archive mode only.
   Assumptions:
   - block store is valid and available,
   - current head is valid and available. *)
let fix_protocol_levels context_index block_store genesis genesis_header =
  (* Search in the cemented store*)
  let cemented_block_store = Block_store.cemented_block_store block_store in
  let cemented_block_files =
    match Cemented_block_store.cemented_blocks_files cemented_block_store with
    | None -> []
    | Some arr -> Array.to_list arr
  in
  (* Iters through the blocks of a cemented cycle from [level] to
     [limit] and identify every proto_level changes and its associated
     activation block. *)
  let cycle_search cemented_block_store ~prev_proto_level ~cycle_start
      ~cycle_end:limit =
    let rec aux ~prev_proto_level ~level acc =
      if level > limit then return acc
      else
        Cemented_block_store.get_cemented_block_by_level
          cemented_block_store
          ~read_metadata:false
          level
        >|=? WithExceptions.Option.get ~loc:__LOC__
        >>=? fun block ->
        let block_proto_level = Block_repr.proto_level block in
        match prev_proto_level with
        | None ->
            (* There is no protocol yet known. The genesis protocol
               will be handled later, no need to deal with it here.*)
            aux
              ~prev_proto_level:(Some block_proto_level)
              ~level:(Int32.succ level)
              acc
        | Some ppl ->
            if ppl <> block_proto_level then
              Context.checkout context_index (Block_repr.context block)
              >>= function
              | None ->
                  (* We have an incomplete context (full or rolling)
                     and thus not enough information to get the
                     activation. We ignore this protocol change. *)
                  aux
                    ~prev_proto_level:(Some block_proto_level)
                    ~level:(Int32.succ level)
                    acc
              | Some context ->
                  Context.get_protocol context >>= fun protocol_hash ->
                  (Context.retrieve_commit_info
                     context_index
                     (Block_repr.header block)
                   >>= function
                   | Ok tup ->
                       Lwt.return_some
                         (Protocol_levels.commit_info_of_tuple tup)
                   | Error _ -> Lwt.return_none)
                  >>= fun commit_info ->
                  let activation =
                    ({
                       block = (Block_repr.hash block, Block_repr.level block);
                       protocol = protocol_hash;
                       commit_info;
                     }
                      : Store_types.Protocol_levels.activation_block)
                  in
                  aux
                    ~prev_proto_level:(Some block_proto_level)
                    ~level:(Int32.succ level)
                    (activation :: acc)
            else aux ~prev_proto_level ~level:(Int32.succ level) acc
    in
    aux ~prev_proto_level ~level:cycle_start []
  in
  (* Return the list of protocol activation blocks by iterating
     through the cemented store.*)
  let rec cemented_search prev_proto_level protocols = function
    | [] -> return protocols
    | cycle :: higher_cycles -> (
        let cycle_end = cycle.Cemented_block_store.end_level in
        let cycle_start = cycle.start_level in
        Cemented_block_store.get_cemented_block_by_level
          ~read_metadata:false
          cemented_block_store
          cycle_end
        >|=? WithExceptions.Option.get ~loc:__LOC__
        >>=? fun block ->
        let block_proto_level = Block_repr.proto_level block in
        match prev_proto_level with
        | None ->
            (* Search a protocol upgrade in the cycle as init *)
            cycle_search
              (Block_store.cemented_block_store block_store)
              ~prev_proto_level
              ~cycle_start
              ~cycle_end
            >>=? fun activations ->
            cemented_search
              (Some block_proto_level)
              (activations @ protocols)
              higher_cycles
        | Some ppl when ppl <> block_proto_level ->
            (* At least one protocol transition occurs in this cycle *)
            cycle_search
              (Block_store.cemented_block_store block_store)
              ~prev_proto_level
              ~cycle_start
              ~cycle_end
            >>=? fun activations ->
            cemented_search
              (Some block_proto_level)
              (activations @ protocols)
              higher_cycles
        | Some _ ->
            (* No protocol change in this cycle *)
            cemented_search prev_proto_level protocols higher_cycles)
  in
  cemented_search None [] cemented_block_files
  >>=? fun cemented_protocol_levels ->
  (match cemented_protocol_levels with
  | [] -> return 0
  | {block = (_, block_level); _} :: _ ->
      Cemented_block_store.get_cemented_block_by_level
        ~read_metadata:false
        cemented_block_store
        block_level
      >|=? WithExceptions.Option.get ~loc:__LOC__
      >>=? fun block -> return (Block_repr.proto_level block))
  >>=? fun highest_cemented_proto_level ->
  (* Search protocol activation in the floating stores *)
  let floating_stores = Block_store.floating_block_stores block_store in
  ( List.map_es
      (Floating_block_store.fold_left_s
         (fun (pls, prev_protocol_level) block ->
           match prev_protocol_level with
           | Some l when Block_repr.proto_level block <> l ->
               let new_proto_level = Block_repr.proto_level block in
               (Context.checkout context_index (Block_repr.context block)
                >>= function
                | None ->
                    (* If the context associated to an activation
                       block is not available, then we cannot infer
                       the protocol table. It may be the case after
                       importing a snapshot. *)
                    fail
                      (Corrupted_store
                         "failed to restore the protocol levels: not enough \
                          data.")
                | Some context -> return context)
               >>=? fun context ->
               Context.get_protocol context >>= fun protocol_hash ->
               (Context.retrieve_commit_info
                  context_index
                  (Block_repr.header block)
                >>= function
                | Ok tup ->
                    Lwt.return_some (Protocol_levels.commit_info_of_tuple tup)
                | Error _ -> Lwt.return_none)
               >>= fun commit_info ->
               let activation =
                 ({
                    block = (Block_repr.hash block, Block_repr.level block);
                    protocol = protocol_hash;
                    commit_info;
                  }
                   : Store_types.Protocol_levels.activation_block)
               in
               return (activation :: pls, Some new_proto_level)
           | Some _ -> return (pls, prev_protocol_level)
           | None -> return (pls, Some (Block_repr.proto_level block)))
         ([], Some highest_cemented_proto_level))
      floating_stores
  >|=? fun v -> List.flatten (List.map fst v) )
  (* Assumption: Floating protocol levels is ordered asc. as
     floating_stores = [RO;RW] *)
  >>=?
  fun floating_protocol_levels ->
  (* Add the genesis protocol *)
  let protocol = genesis.Genesis.protocol in
  (Context.retrieve_commit_info context_index genesis_header >>= function
   | Ok tup -> Lwt.return_some (Protocol_levels.commit_info_of_tuple tup)
   | Error _ -> Lwt.return_none)
  >>= fun genesis_commit_info ->
  let genesis_protocol_level =
    ({
       block = (Block_header.hash genesis_header, genesis_header.shell.level);
       protocol;
       commit_info = genesis_commit_info;
     }
      : Store_types.Protocol_levels.activation_block)
  in
  let protocol_levels =
    genesis_protocol_level
    :: (List.rev cemented_protocol_levels @ floating_protocol_levels)
  in
  return protocol_levels

(* [fix_chain_state ~chain_dir ~head ~cementing_highwatermark
   ~checkpoint ~savepoint ~caboose ~alternate_heads ~forked_chains
   ~protocol_levels ~genesis ~genesis_context] writes, as
   [Stored_data.t], the given arguments. *)
let fix_chain_state chain_dir ~head ~cementing_highwatermark ~checkpoint
    ~savepoint ~caboose ~alternate_heads ~forked_chains ~protocol_levels
    ~genesis ~genesis_context =
  (* By setting each stored data, we erase the previous content. *)
  let rec init_protocol_table proto_level protocol_table = function
    | [] -> protocol_table
    | hd :: tl ->
        let new_protocol_table =
          Protocol_levels.add proto_level hd protocol_table
        in
        init_protocol_table (proto_level + 1) new_protocol_table tl
  in
  let protocol_table =
    init_protocol_table 0 Protocol_levels.empty protocol_levels
  in
  Stored_data.write_file (Naming.protocol_levels_file chain_dir) protocol_table
  >>=? fun () ->
  let genesis_block =
    Block_repr.create_genesis_block ~genesis genesis_context
  in
  Stored_data.write_file (Naming.genesis_block_file chain_dir) genesis_block
  >>=? fun () ->
  Stored_data.write_file (Naming.current_head_file chain_dir) head
  >>=? fun () ->
  Stored_data.write_file (Naming.alternate_heads_file chain_dir) alternate_heads
  >>=? fun () ->
  Stored_data.write_file (Naming.checkpoint_file chain_dir) checkpoint
  >>=? fun () ->
  Stored_data.write_file
    (Naming.cementing_highwatermark_file chain_dir)
    cementing_highwatermark
  >>=? fun () ->
  Stored_data.write_file (Naming.savepoint_file chain_dir) savepoint
  >>=? fun () ->
  Stored_data.write_file (Naming.caboose_file chain_dir) caboose >>=? fun () ->
  Stored_data.write_file
    (Naming.invalid_blocks_file chain_dir)
    Block_hash.Map.empty
  >>=? fun () ->
  Stored_data.write_file (Naming.forked_chains_file chain_dir) forked_chains
  >>=? fun () -> return_unit

(* [fix_chain_config ~chain_dir block_store genesis caboose savepoint]
   infers the history mode and update the [chain_config]. *)
let fix_chain_config chain_dir block_store genesis caboose savepoint =
  let cemented_block_store = Block_store.cemented_block_store block_store in
  let cemented_blocks_files =
    match Cemented_block_store.cemented_blocks_files cemented_block_store with
    | None -> []
    | Some arr -> Array.to_list arr
  in
  let cemented_dir = Naming.cemented_blocks_dir chain_dir in
  let cemented_metadata_dir =
    Naming.cemented_blocks_metadata_dir cemented_dir
  in
  let cemented_metadata_dir_path = Naming.dir_path cemented_metadata_dir in
  (if Sys.file_exists cemented_metadata_dir_path then
   Lwt_stream.fold
     (fun e count -> match e with "." | ".." -> count | _ -> count + 1)
     (Lwt_unix.files_of_directory cemented_metadata_dir_path)
     0
  else Lwt.return 0)
  >>= fun nb_cycles_metadata ->
  let nb_cycles = List.length cemented_blocks_files in
  let offset = nb_cycles_metadata in
  let history_mode =
    (* Caboose is not genesis: we sure are in rolling*)
    if fst caboose <> genesis.Genesis.block then History_mode.Rolling {offset}
    else if
      (* Caboose is genesis and savepoint is not genesis: we can be in
         both rolling and full. We choose full as the less destructive. *)
      fst savepoint <> genesis.block
    then Full {offset}
    else if
      (* Caboose is genesis and savepoint is genesis and there are as
         many cycles as metadata: we can be in any modes. We choose
         archive as the less destructive.*)
      nb_cycles_metadata = nb_cycles
    then Archive
    else
      (* Otherwise, the number of cemented data differs. We can be in
         full or rolling. We choose full as the less destructive. *)
      Full {offset}
  in
  let chain_config = {history_mode; genesis; expiration = None} in
  Stored_data.write_file (Naming.chain_config_file chain_dir) chain_config

let fix_cementing_highwatermark block_store =
  let cemented_block_store = Block_store.cemented_block_store block_store in
  let cementing_highwatermark =
    Cemented_block_store.get_highest_cemented_level cemented_block_store
  in
  Event.(emit fix_cementing_highwatermark cementing_highwatermark) >>= fun () ->
  Lwt.return cementing_highwatermark

(* [fix_consistency store_dir context_index]
   aims to fix a store in an inconsistent state. The fixing steps are:
    - the current head is set as the highest block level found in the
      floating stores,
    - the savepoint is set as the lowest block with metadata found in
      both the floating and cemented stores,
    - the caboose is set as the lowest block found in both the
      floating and cemented stores,
    - alternated heads is set as empty,
    - forked chains is set as empty,
    - genesis is set based on the node's run args (network flag),
    - the chain_state is updated accordingly to the inferred values.
   Assumptions:
    - context is valid and available
    - block store is valid and available *)
let fix_consistency chain_dir context_index genesis =
  Event.(emit fix_store ()) >>= fun () ->
  (* We suppose that the genesis block is accessible *)
  trace
    (Corrupted_store "The genesis block is not available in the store.")
    (Stored_data.load (Naming.genesis_block_file chain_dir))
  >>=? fun genesis_data ->
  Stored_data.get genesis_data >>= fun genesis_block ->
  (* Start fixing things *)
  fix_floating_stores chain_dir >>=? fun () ->
  (* May fix an interrupted merging *)
  Block_store.load chain_dir ~genesis_block ~readonly:false
  >>=? fun block_store ->
  fix_head block_store genesis_block >>=? fun head ->
  fix_cementing_highwatermark block_store >>= fun cementing_highwatermark ->
  fix_savepoint_and_caboose chain_dir block_store head
  >>=? fun (savepoint, caboose) ->
  fix_checkpoint chain_dir block_store head >>=? fun checkpoint ->
  fix_chain_config chain_dir block_store genesis caboose savepoint
  >>=? fun () ->
  fix_protocol_levels
    context_index
    block_store
    genesis
    (Block_repr.header genesis_block)
  >>=? fun protocol_levels ->
  fix_chain_state
    chain_dir
    ~head:(Block_repr.descriptor head)
    ~cementing_highwatermark
    ~checkpoint
    ~savepoint
    ~caboose
    ~alternate_heads:[]
    ~forked_chains:Chain_id.Map.empty
    ~protocol_levels
    ~genesis
    ~genesis_context:(Block_repr.context genesis_block)
  >>=? fun () ->
  Block_store.close block_store >>= fun () -> return_unit
