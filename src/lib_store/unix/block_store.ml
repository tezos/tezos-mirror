(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
open Block_repr
open Store_errors

module Profiler = struct
  include (val Profiler.wrap Shell_profiling.merge_profiler)

  let[@warning "-32"] reset_block_section =
    Shell_profiling.create_reset_block_section Shell_profiling.merge_profiler
end

let default_block_cache_limit = 1_000

type merge_status = Not_running | Running | Merge_failed of tztrace

type block_store = {
  chain_dir : [`Chain_dir] Naming.directory;
  readonly : bool;
  genesis_block : Block_repr.t;
  cemented_store : Cemented_block_store.t;
  mutable ro_floating_block_stores : Floating_block_store.t list;
  mutable rw_floating_block_store : Floating_block_store.t;
  caboose : block_descriptor Stored_data.t;
  savepoint : block_descriptor Stored_data.t;
  status_data : Block_store_status.t Stored_data.t;
  block_cache : Block_repr.t Block_lru_cache.t;
  mutable gc_callback : (Block_hash.t -> unit tzresult Lwt.t) option;
  mutable split_callback : (unit -> unit tzresult Lwt.t) option;
  merge_mutex : Lwt_mutex.t;
  merge_scheduler : Lwt_idle_waiter.t;
  (* Target level x Merging thread *)
  mutable merging_thread : (int32 * unit tzresult Lwt.t) option;
  lockfile : Lwt_unix.file_descr;
  stored_data_lockfile : Lwt_unix.file_descr;
}

type t = block_store

type key = Block of (Block_hash.t * int)

let cemented_block_store {cemented_store; _} = cemented_store

let floating_block_stores {ro_floating_block_stores; rw_floating_block_store; _}
    =
  List.rev (rw_floating_block_store :: ro_floating_block_stores)

let savepoint {savepoint; _} = Stored_data.get savepoint

let caboose {caboose; _} = Stored_data.get caboose

let status {status_data; _} = Stored_data.get status_data

let write_savepoint {savepoint; _} v =
  let open Lwt_result_syntax in
  let* () = Stored_data.write savepoint v in
  let*! () = Store_events.(emit set_savepoint v) in
  Prometheus.Gauge.set
    Store_metrics.metrics.savepoint_level
    (Int32.to_float (snd v)) ;
  return_unit

let write_caboose {caboose; _} v =
  let open Lwt_result_syntax in
  let* () = Stored_data.write caboose v in
  let*! () = Store_events.(emit set_caboose v) in
  Prometheus.Gauge.set
    Store_metrics.metrics.caboose_level
    (Int32.to_float (snd v)) ;
  return_unit

let genesis_block {genesis_block; _} = genesis_block

(** [global_predecessor_lookup chain_block_store hash pow_nth] retrieves
    the 2^[pow_nth] predecessor's hash from the block with corresponding
    [hash] by checking all stores iteratively. Returns [None] if the
    predecessor is not found or if it is below genesis. *)
let global_predecessor_lookup block_store hash pow_nth =
  let open Lwt_syntax in
  (* pow_nth = 0 => direct predecessor *)
  (* Look in the RW block_store, then RO stores and finally in the
     cemented store *)
  let* o =
    List.find_map_s
      (fun floating_store ->
        let* o = Floating_block_store.find_predecessors floating_store hash in
        match o with
        | None -> Lwt.return_none
        | Some predecessors -> Lwt.return (List.nth_opt predecessors pow_nth))
      (block_store.rw_floating_block_store
     :: block_store.ro_floating_block_stores)
  in
  match o with
  | Some hash -> Lwt.return_some hash
  | None -> (
      (* It must be cemented *)
      match
        Cemented_block_store.get_cemented_block_level
          block_store.cemented_store
          hash
      with
      | None -> Lwt.return_none
      | Some level ->
          (* level - 2^n *)
          let pred_level =
            max
              (Block_repr.level block_store.genesis_block)
              Int32.(sub level (shift_left 1l pow_nth))
          in
          Lwt.return
            (Cemented_block_store.get_cemented_block_hash
               block_store.cemented_store
               pred_level))

(**
   Takes a block_store and a block and returns the block's known
   predecessors. The predecessors are distributed along the chain,
   up to the genesis, at a distance from [b] that grows exponentially.
   The store tabulates a function [p] from distances to block_ids such
   that if [p(b,d)=b'] then [b'] is at distance 2^d from [b].
   Example of how previous predecessors are used:
   p(n,0) = n-1
   p(n,1) = n-2  = p(n-1,0)
   p(n,2) = n-4  = p(n-2,1)
   p(n,3) = n-8  = p(n-4,2)
   p(n,4) = n-16 = p(n-8,3)
   ...

   The list might be trimmed down if not enough predecessors can be
   found in the block_store.
*)
let compute_predecessors block_store block =
  let open Lwt_syntax in
  let rec loop predecessors_acc pred dist =
    if dist = Floating_block_index.Block_info.max_predecessors then
      Lwt.return predecessors_acc
    else
      let* o = global_predecessor_lookup block_store pred (dist - 1) in
      match o with
      | None -> Lwt.return predecessors_acc
      | Some pred' -> loop (pred' :: predecessors_acc) pred' (dist + 1)
  in
  let predecessor = predecessor block in
  if Block_hash.equal block.hash predecessor then
    (* genesis *)
    Lwt.return [block.hash]
  else
    let* rev_preds = loop [predecessor] predecessor 1 in
    Lwt.return (List.rev rev_preds)

(** [get_hash block_store key] retrieves the block which is at
    [distance] from the block with corresponding [hash] by every store
    iteratively. *)
let get_hash block_store (Block (block_hash, offset)) =
  let open Lwt_result_syntax in
  let closest_power_two n =
    if n < 0 then assert false
    else
      let rec loop cnt n = if n <= 1 then cnt else loop (cnt + 1) (n / 2) in
      loop 0 n
  in
  Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
      if offset = 0 then return_some block_hash
      else if offset < 0 then tzfail (Wrong_predecessor (block_hash, offset))
      else
        match
          Cemented_block_store.get_cemented_block_level
            block_store.cemented_store
            block_hash
        with
        | Some block_level ->
            let target = Int32.(sub block_level (of_int offset)) in
            return
              (Cemented_block_store.get_cemented_block_hash
                 block_store.cemented_store
                 target)
        | None ->
            (* actual predecessor function *)
            let rec loop block_hash offset =
              if offset = 1 then
                let*! pred =
                  global_predecessor_lookup block_store block_hash 0
                in
                return pred
              else
                let power = closest_power_two offset in
                let power =
                  if power < Floating_block_index.Block_info.max_predecessors
                  then power
                  else
                    let power =
                      Floating_block_index.Block_info.max_predecessors - 1
                    in
                    power
                in
                let*! o =
                  global_predecessor_lookup block_store block_hash power
                in
                match o with
                | None -> return_none
                | Some pred ->
                    let rest = offset - (1 lsl power) in
                    if rest = 0 then return_some pred
                      (* landed on the requested predecessor *)
                    else loop pred rest
              (* need to jump further back *)
            in
            loop block_hash offset)

let mem block_store key =
  let open Lwt_result_syntax in
  Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
      let* o = get_hash block_store key in
      match o with
      | None -> return_false
      | Some predecessor_hash
        when Block_hash.equal block_store.genesis_block.hash predecessor_hash ->
          return_true
      | Some predecessor_hash ->
          let*! is_known_in_floating =
            List.exists_s
              (fun store -> Floating_block_store.mem store predecessor_hash)
              (block_store.rw_floating_block_store
             :: block_store.ro_floating_block_stores)
          in
          return
            (is_known_in_floating
            || Cemented_block_store.is_cemented
                 block_store.cemented_store
                 predecessor_hash))

let read_block block_store ~read_metadata key_kind =
  let open Lwt_result_syntax in
  Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
      (* Resolve the hash *)
      let* o = get_hash block_store key_kind in
      match o with
      | None -> return_none
      | Some adjusted_hash ->
          if Block_hash.equal block_store.genesis_block.hash adjusted_hash then
            return_some block_store.genesis_block
          else
            let fetch_block adjusted_hash =
              (* First look in the floating stores *)
              let*! o =
                List.find_map_s
                  (fun store ->
                    Floating_block_store.read_block store adjusted_hash)
                  (block_store.rw_floating_block_store
                 :: block_store.ro_floating_block_stores)
              in
              match o with
              | Some block -> Lwt.return_some block
              | None -> (
                  (* Lastly, look in the cemented blocks *)
                  let*! r =
                    Cemented_block_store.get_cemented_block_by_hash
                      ~read_metadata
                      block_store.cemented_store
                      adjusted_hash
                  in
                  match r with
                  | Ok v -> Lwt.return v
                  | Error _ -> Lwt.return_none)
            in
            let*! block =
              Block_lru_cache.bind_or_put
                block_store.block_cache
                adjusted_hash
                fetch_block
                Lwt.return
            in
            return block)

let read_block_metadata block_store key_kind =
  let open Lwt_result_syntax in
  Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
      (* Resolve the hash *)
      let* o = get_hash block_store key_kind in
      match o with
      | None -> return_none
      | Some adjusted_hash -> (
          if Block_hash.equal block_store.genesis_block.hash adjusted_hash then
            return (Block_repr.metadata block_store.genesis_block)
          else
            (* First look in the floating stores *)
            let*! o =
              List.find_map_s
                (fun store ->
                  Floating_block_store.read_block store adjusted_hash)
                (block_store.rw_floating_block_store
               :: block_store.ro_floating_block_stores)
            in
            match o with
            | Some block -> return block.metadata
            | None -> (
                (* Lastly, look in the cemented blocks *)
                match
                  Cemented_block_store.get_cemented_block_level
                    block_store.cemented_store
                    adjusted_hash
                with
                | None -> return_none
                | Some level ->
                    Cemented_block_store.read_block_metadata
                      block_store.cemented_store
                      level)))

let resulting_context_hash block_store ~fetch_expect_predecessor_context key =
  (* Hypothesis: there is an intersection of at least 1 block with the
     end of the cementing store and the beginning of the floating
     store.
     Indeed, there are [max_op_ttl] blocks below the checkpoint
     kept in the floating store, so that this window of blocks
     overlaps with the content of the cemented store. Thus, looking
     at the successor of the last cemented block should never occur,
     as this case would be tackled by the floating store's looking. *)
  let open Lwt_result_syntax in
  let ( let*? ) t k =
    let* v_opt = t in
    match v_opt with None -> return_none | Some v -> k v
  in
  (* Resolve the hash *)
  let*? adjusted_hash = get_hash block_store key in
  if Block_hash.equal block_store.genesis_block.hash adjusted_hash then
    return_some (Block_repr.context block_store.genesis_block)
  else
    (* First look in the floating stores *)
    let*! resulting_context_opt =
      Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
          List.find_map_s
            (fun store ->
              Floating_block_store.find_resulting_context_hash
                store
                adjusted_hash)
            (block_store.rw_floating_block_store
           :: block_store.ro_floating_block_stores))
    in
    match resulting_context_opt with
    | Some resulting_context_hash -> return_some resulting_context_hash
    | None ->
        (* [fetch_expect_predecessor_context] takes a lock on the
           chain_data: we make sure not to lock the [merge_scheduler]
           to prevent a data-race locking up the store. *)
        let* expect_predecessor = fetch_expect_predecessor_context () in
        (* If not found, look at the context of the direct
           successor of the looked up block in the cemented store. *)
        Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
            let cemented_store = block_store.cemented_store in
            if expect_predecessor then
              let*? block_level =
                return
                  (Cemented_block_store.get_cemented_block_level
                     cemented_store
                     adjusted_hash)
              in
              let*? succ_block =
                Cemented_block_store.get_cemented_block_by_level
                  cemented_store
                  ~read_metadata:false
                  (Int32.succ block_level)
              in
              return_some (Block_repr.context succ_block)
            else
              let*? block =
                Cemented_block_store.get_cemented_block_by_hash
                  cemented_store
                  ~read_metadata:false
                  adjusted_hash
              in
              return_some (Block_repr.context block))

let store_block block_store block resulting_context_hash =
  let open Lwt_result_syntax in
  let* () = fail_when block_store.readonly Cannot_write_in_readonly in
  Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
      protect (fun () ->
          let*! predecessors = compute_predecessors block_store block in
          Block_lru_cache.put
            block_store.block_cache
            block.hash
            (Lwt.return_some block) ;
          Floating_block_store.append_block
            ~log_metrics:true
            block_store.rw_floating_block_store
            {predecessors; resulting_context_hash}
            block))

let cement_blocks ?(check_consistency = true) ~write_metadata block_store
    chunk_iterator ~cycle_range:(cycle_start, cycle_stop) =
  (* No need to lock *)
  let open Lwt_result_syntax in
  let*! () =
    Store_events.(emit start_cementing_blocks) (cycle_start, cycle_stop)
  in
  let {cemented_store; _} = block_store in
  let* () =
    (Cemented_block_store.cement_blocks
       ~check_consistency
       cemented_store
       ~write_metadata
       chunk_iterator
     [@profiler.record_s
       {verbosity = Debug; metadata = [("prometheus", "")]} "cement blocks"])
  in
  let*! () = Store_events.(emit end_cementing_blocks) () in
  return_unit

(* [try_retrieve_n_predecessors stores block_hash n] retrieves, at
   most, the [n] [block_hash]'s predecessors (including [block_hash])
   from the floating stores. The resulting block list may be smaller
   than [n] and contains the oldest blocks first. *)
let try_retrieve_n_predecessors floating_stores block_hash n =
  let open Lwt_syntax in
  let rec loop acc current_hash n =
    if n = 0 then return acc
    else
      let* o =
        List.find_map_s
          (fun floating_store ->
            Floating_block_store.find_predecessors floating_store current_hash)
          floating_stores
      in
      match o with
      | None | Some [] ->
          (* The remaining blocks are not present, skip them. *)
          return acc
      | Some (direct_predecessor_hash :: _ancestors) ->
          loop (current_hash :: acc) direct_predecessor_hash (pred n)
  in
  loop [] block_hash n

let read_predecessor_block_by_level_opt block_store ?(read_metadata = false)
    ~head level =
  read_block
    block_store
    ~read_metadata
    (Block
       (Block_repr.hash head, Int32.(to_int (sub (Block_repr.level head) level))))

let read_predecessor_block_by_level block_store ?(read_metadata = false) ~head
    level =
  let open Lwt_result_syntax in
  let head_level = Block_repr.level head in
  let head_hash = Block_repr.hash head in
  let distance = Int32.(to_int (sub head_level level)) in
  let* o =
    read_block block_store ~read_metadata (Block (head_hash, distance))
  in
  match o with
  | None ->
      if distance < 0 then tzfail (Bad_level {head_level; given_level = level})
      else tzfail (Block_not_found {hash = head_hash; distance})
  | Some b -> return b

let read_iterator_block_range_in_floating_stores block_store ~ro_store ~rw_store
    ~head (low, high) =
  let open Lwt_result_syntax in
  let* high_block = read_predecessor_block_by_level block_store ~head high in
  let nb_blocks =
    Int32.(add one (sub high low) |> to_int)
    (* +1, it's a size *)
  in
  let*! block_hashes =
    try_retrieve_n_predecessors
      [ro_store; rw_store]
      (Block_repr.hash high_block)
      nb_blocks
  in
  let chunk_length = List.length block_hashes (* effective size *) in
  let reading_sequence =
    Floating_block_store.raw_retrieve_blocks_seq
      ~src_floating_stores:[ro_store; rw_store]
      ~block_hashes
  in
  return {Cemented_block_store.chunk_length; reading_sequence}

(* [expected_savepoint block_store target_offset] computes the
   expected savepoint based on the [target_offset]. When the
   [target_offset] cannot be satisfied, the previous savepoint is
   returned.*)
let expected_savepoint block_store ~target_offset =
  let open Lwt_result_syntax in
  let cemented_dir = Naming.cemented_blocks_dir block_store.chain_dir in
  let* metadata_table = Cemented_block_store.load_metadata_table cemented_dir in
  match metadata_table with
  | None ->
      let*! current_savepoint = savepoint block_store in
      return (snd current_savepoint)
  | Some cemented_block_metadata_files ->
      let nb_files = Array.length cemented_block_metadata_files in
      if target_offset >= nb_files || nb_files = 0 then
        (* If cannot provide a savepoint from the cemented block store
            or if the target_offset is equal to the current one then we
            return the current savepoint. *)
        let*! current_savepoint = savepoint block_store in
        return (snd current_savepoint)
      else if target_offset = 0 then
        (* We get the successor of the highest cemented level *)
        let cycle = cemented_block_metadata_files.(nb_files - 1) in
        return (Int32.succ cycle.end_level)
      else
        (* We get the lowest block of the targeted cycle which
           contains metadata *)
        let cycle = cemented_block_metadata_files.(nb_files - target_offset) in
        return cycle.start_level

(* [available_savepoint block_store current_head savepoint_candidate]
   aims to check that the [savepoint_candidate] can be used as a valid
   savepoint (that is to say, contains metadata). It returns the
   [savepoint_candidate] block descriptor if it is valid. Returns the
   current savepoint otherwise. *)
let available_savepoint block_store current_head savepoint_candidate =
  let open Lwt_result_syntax in
  let head_hash = Block_repr.hash current_head in
  let*! current_savepoint = savepoint block_store in
  let new_savepoint_level =
    if savepoint_candidate < snd current_savepoint then snd current_savepoint
    else savepoint_candidate
  in
  let distance =
    Int32.(to_int (sub (Block_repr.level current_head) new_savepoint_level))
  in
  let* block =
    let* o =
      read_block ~read_metadata:false block_store (Block (head_hash, distance))
    in
    match o with
    | Some b -> return b
    | None -> tzfail (Wrong_predecessor (head_hash, distance))
  in
  return (descriptor block)

(* [preserved_block block_store current_head] returns the preserved
   block candidate level. The preserved block aims to be the one
   needed and maintained available to export snapshot. That is to say,
   the block: lpbl(head) - max_op_ttl(lpbl). *)
let preserved_block block_store current_head =
  let open Lwt_result_syntax in
  let head_hash = Block_repr.hash current_head in
  let* current_head_metadata_o =
    read_block_metadata block_store (Block (head_hash, 0))
  in
  let current_head_metadata =
    WithExceptions.Option.get ~loc:__LOC__ current_head_metadata_o
  in
  let head_lpbl = Block_repr.last_preserved_block_level current_head_metadata in
  let head_max_op_ttl =
    Int32.of_int (Block_repr.max_operations_ttl current_head_metadata)
  in
  return Int32.(max 0l (sub head_lpbl head_max_op_ttl))

(* [infer_savepoint block_store current_head ~target_offset] returns
   the savepoint candidate for an history mode switch. *)
let infer_savepoint block_store current_head ~target_offset =
  let open Lwt_result_syntax in
  let* expected_savepoint_level =
    expected_savepoint block_store ~target_offset
  in
  let* preserved_savepoint_level = preserved_block block_store current_head in
  let savepoint_candidate =
    min preserved_savepoint_level expected_savepoint_level
  in
  available_savepoint block_store current_head savepoint_candidate

(* [expected_caboose block_store ~target_offset] computes the
   expected caboose based on the [target_offset]). None is returned if
   the cemented store cannot satisfy the targeted offset. *)
let expected_caboose block_store ~target_offset =
  let cemented_store = cemented_block_store block_store in
  match Cemented_block_store.cemented_blocks_files cemented_store with
  | None -> None
  | Some cemented_block_files ->
      let nb_files = Array.length cemented_block_files in
      if target_offset > nb_files || nb_files = 0 then
        (* The expected caboose cannot be satisfied *)
        None
      else if target_offset = 0 then
        (* We get the successor of the highest cemented level *)
        let cycle = cemented_block_files.(nb_files - 1) in
        Some (Int32.succ cycle.end_level)
      else
        (* We get the lowest block of the targeted cycle *)
        let cycle = cemented_block_files.(nb_files - target_offset) in
        Some cycle.start_level

(* [infer_caboose block_store savepoint current_head ~target_offset
   ~new_history_mode ~previous_history_mode] returns the caboose
   candidate for an history mode switch. *)
let infer_caboose block_store savepoint current_head ~target_offset
    ~new_history_mode ~previous_history_mode =
  let open Lwt_result_syntax in
  match previous_history_mode with
  | History_mode.Archive -> (
      match new_history_mode with
      | History_mode.Archive ->
          tzfail
            (Cannot_switch_history_mode
               {
                 previous_mode = previous_history_mode;
                 next_mode = new_history_mode;
               })
      | Full _ ->
          let*! b = caboose block_store in
          return b
      | Rolling _ -> return savepoint)
  | Full _ -> (
      match expected_caboose block_store ~target_offset with
      | Some expected_caboose ->
          let* preserved_caboose = preserved_block block_store current_head in
          let new_caboose_level = min expected_caboose preserved_caboose in
          let head_hash = Block_repr.hash current_head in
          let distance =
            Int32.(
              to_int (sub (Block_repr.level current_head) new_caboose_level))
          in
          let* block =
            let* o =
              read_block
                ~read_metadata:false
                block_store
                (Block (head_hash, distance))
            in
            match o with
            | Some b -> return b
            | None -> tzfail (Wrong_predecessor (head_hash, distance))
          in
          return (descriptor block)
      | None -> return savepoint)
  | Rolling r ->
      let current_offset =
        Option.value r ~default:History_mode.default_additional_cycles
      in
      if current_offset.offset < target_offset then
        let*! b = caboose block_store in
        return b
      else return savepoint

let switch_history_mode block_store ~current_head ~previous_history_mode
    ~new_history_mode =
  let open Lwt_result_syntax in
  let open History_mode in
  match (previous_history_mode, new_history_mode) with
  | Full _, Rolling m | Rolling _, Rolling m ->
      let m =
        (Option.value m ~default:History_mode.default_additional_cycles).offset
      in
      (* Both the caboose and savepoint can be updated *)
      let* new_savepoint =
        infer_savepoint block_store current_head ~target_offset:m
      in
      let* new_caboose =
        infer_caboose
          block_store
          new_savepoint
          current_head
          ~target_offset:m
          ~new_history_mode
          ~previous_history_mode
      in
      let cemented_block_store = cemented_block_store block_store in
      let*! () =
        Cemented_block_store.trigger_gc cemented_block_store new_history_mode
      in
      let* () = write_savepoint block_store new_savepoint in
      let* () = write_caboose block_store new_caboose in
      return_unit
  | Full _, Full m ->
      let m =
        (Option.value m ~default:History_mode.default_additional_cycles).offset
      in
      (* Only the savepoint can be updated *)
      let* new_savepoint =
        infer_savepoint block_store current_head ~target_offset:m
      in
      let*! () =
        Cemented_block_store.trigger_gc
          (cemented_block_store block_store)
          new_history_mode
      in
      let* () = write_savepoint block_store new_savepoint in
      return_unit
  | Archive, Full m | Archive, Rolling m ->
      let m =
        (Option.value m ~default:History_mode.default_additional_cycles).offset
      in
      (* Both the caboose and savepoint can be updated *)
      let* new_savepoint =
        infer_savepoint block_store current_head ~target_offset:m
      in
      let* new_caboose =
        infer_caboose
          block_store
          new_savepoint
          current_head
          ~target_offset:m
          ~new_history_mode
          ~previous_history_mode
      in
      let*! () =
        Cemented_block_store.trigger_gc
          (cemented_block_store block_store)
          new_history_mode
      in
      let* () = write_savepoint block_store new_savepoint in
      let* () = write_caboose block_store new_caboose in
      return_unit
  | _ ->
      tzfail
        (Cannot_switch_history_mode
           {previous_mode = previous_history_mode; next_mode = new_history_mode})

let compute_new_savepoint block_store history_mode ~new_store
    ~min_level_to_preserve ~new_head ~cycles_to_cement =
  let open Lwt_result_syntax in
  assert (cycles_to_cement <> []) ;
  let*! savepoint = Stored_data.get block_store.savepoint in
  match history_mode with
  | History_mode.Archive ->
      (* new_savepoint = savepoint = genesis *)
      return savepoint
  | Full offset | Rolling offset -> (
      let offset =
        (Option.value offset ~default:History_mode.default_additional_cycles)
          .offset
      in
      let* min_block_to_preserve =
        read_predecessor_block_by_level
          block_store
          ~head:new_head
          min_level_to_preserve
      in
      let ((_min_block_hash, min_block_level) as min_block_descr) =
        Block_repr.descriptor min_block_to_preserve
      in
      (* New savepoint = min min_level_to_preserve (min new lowest cemented block) *)
      let cemented_cycles =
        match
          Cemented_block_store.cemented_blocks_files block_store.cemented_store
        with
        | None -> cycles_to_cement
        | Some table ->
            (Array.to_list table
            |> List.map (fun {Cemented_block_store.start_level; end_level; _} ->
                   (start_level, end_level)))
            @ cycles_to_cement
      in
      let* cemented_metadata_table =
        Cemented_block_store.cemented_metadata_files block_store.cemented_store
      in
      let cemented_metadata_cycles =
        match cemented_metadata_table with
        | None -> []
        | Some table ->
            Array.to_list table
            |> List.map
                 (fun
                   ({Cemented_block_store.start_level; end_level; _} :
                     Cemented_block_store.cemented_metadata_file)
                 -> (start_level, end_level))
      in
      if Compare.Int32.(snd savepoint >= min_block_level) then return savepoint
      else
        let cemented_cycles_len = List.length cemented_cycles in
        (* If the offset is 0, the savepoint will be the minimum block
           to preserve. *)
        if offset = 0 then return min_block_descr
        else if
          (* If the number of cemented cycles is not yet the offset,
             then the savepoint will be unchanged. *)
          cemented_cycles_len < offset
        then
          (* In case of a freshly imported rolling snapshot, we may
             drag the savepoint if it was not set on a cycle
             start. Otherwise, the savepoint would be missing from the
             store. We drag the savepoint only if it is not in the new
             floating store nor in the cycles to cements U cemented
             cycles. *)
          let savepoint_hash, savepoint_level = savepoint in
          let is_savepoint_in_cemented =
            List.exists
              (fun (l, h) -> l <= savepoint_level && savepoint_level <= h)
              (cycles_to_cement @ cemented_metadata_cycles)
          in
          if not is_savepoint_in_cemented then
            let*! is_savepoint_in_new_store =
              Floating_block_store.mem new_store savepoint_hash
            in
            if not is_savepoint_in_new_store then return min_block_descr
            else return savepoint
          else return savepoint
        else
          (* Else we shift the savepoint by [List.length cycles_to_cement]
             cycles *)
          let shifted_savepoint_level =
            (* new lowest cemented block  *)
            fst
              (List.nth cemented_cycles (cemented_cycles_len - offset)
              |> WithExceptions.Option.get ~loc:__LOC__)
          in
          (* If the savepoint is still higher than the shifted
             savepoint, preserve the savepoint *)
          if Compare.Int32.(snd savepoint >= shifted_savepoint_level) then
            return savepoint
          else if
            (* If the new savepoint is still higher than the min block
               to preserve, we choose the min block to preserve. *)
            Compare.Int32.(shifted_savepoint_level >= min_block_level)
          then return min_block_descr
          else
            (* Else the new savepoint is the one-cycle shifted
               savepoint. *)
            let* o =
              read_predecessor_block_by_level_opt
                block_store
                ~head:new_head
                shifted_savepoint_level
            in
            match o with
            | None -> tzfail (Cannot_retrieve_savepoint shifted_savepoint_level)
            | Some savepoint -> return (Block_repr.descriptor savepoint))

let compute_new_caboose block_store history_mode ~new_savepoint
    ~min_level_to_preserve ~new_head =
  let open Lwt_result_syntax in
  let*! caboose = Stored_data.get block_store.caboose in
  match history_mode with
  | History_mode.Archive | Full _ ->
      (* caboose = genesis *)
      return caboose
  | Rolling offset ->
      (* If caboose equals min block to preserve, we leave it
         unchanged. Note: Caboose cannot normally be >
         min_level_to_preserve. *)
      let offset =
        (Option.value offset ~default:History_mode.default_additional_cycles)
          .offset
      in
      if Compare.Int32.(snd caboose >= min_level_to_preserve) then
        return caboose
      else if
        (* If the min level to preserve is lower than the savepoint or
           if we don't keep any extra cycles, the genesis is the min
           block to preserve. *)
        Compare.Int32.(min_level_to_preserve < snd new_savepoint) || offset = 0
      then
        let* min_block_to_preserve =
          read_predecessor_block_by_level
            block_store
            ~head:new_head
            min_level_to_preserve
        in
        return (Block_repr.descriptor min_block_to_preserve)
      else return new_savepoint

module BlocksLPBL = Set.Make (Int32)

(* Limits the maximum number of elements that can be added into a
   cycle.
   This is mandatory when cementing metadata. Indeed, the current
   version of camlzip support only 32bits zip files, that are files
   smaller that ~4GB or containing less that 65_535 entries. When
   cementing cycles, we might reach that limit. We set it to 2^16 - 1. *)
let default_cycle_size_limit = 65_535l

(* May shrink the size of the given cycles to make sure that the size
   of a cycle never exceeds the camlzip 32bits limitation. The shrink
   consist in dividing the cycles in two even parts, recursively,
   until the limit is not exceeded anymore. *)
let may_shrink_cycles cycles ~cycle_size_limit =
  let rec loop acc cycles =
    match cycles with
    | [] -> List.rev acc
    | ((cycle_start, cycle_end) as hd) :: tl ->
        let diff = Int32.(sub cycle_end cycle_start) in
        if diff >= cycle_size_limit then
          let mid = Int32.(div diff 2l) in
          let left_cycle_upper_bound = Int32.(add cycle_start mid) in
          let left_cycle = (cycle_start, left_cycle_upper_bound) in
          let right_cycle =
            (Int32.(add left_cycle_upper_bound 1l), cycle_end)
          in
          loop acc (left_cycle :: right_cycle :: tl)
        else loop (hd :: acc) tl
  in
  loop [] cycles

(* FIXME: update doc *)
(* [update_floating_stores block_store ~history_mode ~ro_store
   ~rw_store ~new_store ~new_head ~new_head_lpbl
   ~lowest_bound_to_preserve_in_floating ~cementing_highwatermark]
   updates the [new_store] by storing the predecessors of the
   [new_head_lpbl] and preserving the
   [lowest_bound_to_preserve_in_floating]. It returns the cycles to
   cement from [new_head] to [cementing_highwatermark] and the
   savepoint and caboose candidates. *)
let update_floating_stores block_store ~history_mode ~ro_store ~rw_store
    ~new_store ~new_head ~new_head_lpbl ~lowest_bound_to_preserve_in_floating
    ~cementing_highwatermark ~cycle_size_limit =
  let open Lwt_result_syntax in
  let*! () = Store_events.(emit start_updating_floating_stores) () in
  let* lpbl_block =
    (read_predecessor_block_by_level
       block_store
       ~head:new_head
       new_head_lpbl
     [@profiler.record_s
       {verbosity = Debug; metadata = [("prometheus", "")]} "read lpbl block"])
  in
  let final_hash, final_level = Block_repr.descriptor lpbl_block in
  (* 1. Append to the new RO [new_store] blocks between
     [lowest_bound_to_preserve_in_floating] and [lpbl_block]. *)
  let max_nb_blocks_to_retrieve =
    Compare.Int.(
      max
        1
        Int32.(
          add one (sub final_level lowest_bound_to_preserve_in_floating)
          |> to_int))
  in
  let*! () = Store_events.(emit start_retreiving_predecessors) () in
  let floating_stores =
    (* Iterate over the store with RO first for the lookup. *)
    [ro_store; rw_store]
  in
  let*! lpbl_predecessors =
    (try_retrieve_n_predecessors
       floating_stores
       final_hash
       max_nb_blocks_to_retrieve
     [@profiler.record_s
       {verbosity = Debug; metadata = [("prometheus", "")]}
         "retrieve N lpbl's predecessors"])
  in
  (* [min_level_to_preserve] is the lowest block that we want to keep
     in the floating stores. *)
  let*! min_level_to_preserve =
    match[@profiler.record_s
           {verbosity = Debug; metadata = [("prometheus", "")]}
             "read min level block to preserve"]
      lpbl_predecessors
    with
    | [] -> Lwt.return new_head_lpbl
    | oldest_predecessor :: _ -> (
        let*! o =
          List.find_map_s
            (fun floating_store ->
              Floating_block_store.read_block floating_store oldest_predecessor)
            floating_stores
        in
        match o with
        | None -> Lwt.return new_head_lpbl
        | Some x -> Lwt.return (Block_repr.level x))
  in
  (* As blocks from [lpbl_predecessors] contains older blocks first,
     the resulting [new_store] will be correct and will contain older
     blocks before more recent ones. *)
  let* () =
    (Floating_block_store.raw_copy_all
       ~src_floating_stores:floating_stores
       ~block_hashes:lpbl_predecessors
       ~dst_floating_store:new_store
     [@profiler.record_s
       {verbosity = Debug; metadata = [("prometheus", "")]}
         "copy all lpbl predecessors"])
  in
  (* 2. Retrieve ALL cycles (potentially more than one) *)
  (* 2.1. We write back to the new store all the blocks from
     [lpbl_block] to the end of the file(s).

     2.2 At the same time, retrieve the list of cycle bounds: i.e. the
     interval of blocks s.t. \forall b \in
     {stores}. cementing_highwatermark < b.lpbl <= new_head_lpbl

     HYPOTHESIS: all blocks at a given level have the same lpbl. *)
  let visited = ref (Block_hash.Set.singleton (Block_repr.hash lpbl_block)) in
  let blocks_lpbl = ref BlocksLPBL.empty in
  let*! () = Store_events.(emit start_retreiving_cycles) () in
  let* () =
    (List.iter_es
       (fun store ->
         let[@warning "-26"] kind =
           match Floating_block_store.kind store with
           | RO -> "RO"
           | RW -> "RW"
           | _ -> assert false
         in
         (Floating_block_store.raw_iterate
            (fun (block_bytes, total_block_length) ->
              let block_level =
                Block_repr_unix.raw_get_block_level block_bytes
              in
              (* Ignore blocks that are below the cementing highwatermark *)
              if Compare.Int32.(block_level <= cementing_highwatermark) then
                return_unit
              else
                let block_lpbl_opt =
                  Block_repr_unix.raw_get_last_preserved_block_level
                    block_bytes
                    total_block_length
                in
                (* Start by updating the set of cycles *)
                Option.iter
                  (fun block_lpbl ->
                    if
                      Compare.Int32.(
                        cementing_highwatermark < block_lpbl
                        && block_lpbl <= new_head_lpbl)
                    then blocks_lpbl := BlocksLPBL.add block_lpbl !blocks_lpbl)
                  block_lpbl_opt ;
                (* Append block if its predecessor was visited and update
                   the visited set. *)
                let block_predecessor =
                  Block_repr_unix.raw_get_block_predecessor block_bytes
                in
                let block_hash =
                  Block_repr_unix.raw_get_block_hash block_bytes
                in
                if Block_hash.Set.mem block_predecessor !visited then (
                  visited := Block_hash.Set.add block_hash !visited ;
                  let*! {predecessors; resulting_context_hash} =
                    let*! pred_opt =
                      (Floating_block_store.find_info
                         store
                         block_hash
                       [@profiler.aggregate_s
                         {verbosity = Debug; metadata = [("prometheus", "")]}
                           "find block index info"])
                    in
                    Lwt.return (WithExceptions.Option.get ~loc:__LOC__ pred_opt)
                  in
                  (Floating_block_store.raw_append
                     new_store
                     ( block_hash,
                       block_bytes,
                       total_block_length,
                       predecessors,
                       resulting_context_hash )
                   [@profiler.aggregate_s
                     {verbosity = Debug; metadata = [("prometheus", "")]}
                       "raw append block"]))
                else return_unit)
            store
          [@profiler.record_s
            {verbosity = Debug; metadata = [("prometheus", "")]}
              (Printf.sprintf "iterate over floating store '%s'" kind)]))
       [ro_store; rw_store]
     [@profiler.record_s
       {verbosity = Debug; metadata = [("prometheus", "")]}
         "iterate and prune floating stores"])
  in
  let is_cementing_highwatermark_genesis =
    Compare.Int32.(
      cementing_highwatermark = Block_repr.level block_store.genesis_block)
  in
  (* Return the range of cycles to cement. *)
  let rec loop acc pred = function
    | [] -> tzfail (Cannot_cement_blocks `Empty)
    | [h] ->
        assert (Compare.Int32.(h = new_head_lpbl)) ;
        return (List.rev ((Int32.succ pred, h) :: acc))
    | h :: (h' :: _ as t) ->
        (* lpbls are monotonous and strictly increasing *)
        assert (Compare.Int32.(h < h')) ;
        loop ((Int32.succ pred, h) :: acc) h t
  in
  let initial_pred =
    (* Hack to include genesis in the first cycle when the initial
       cementing highwatermark is genesis's lpbl *)
    if is_cementing_highwatermark_genesis then
      Int32.pred cementing_highwatermark
    else cementing_highwatermark
  in
  let sorted_lpbl =
    List.sort Compare.Int32.compare (BlocksLPBL.elements !blocks_lpbl)
  in
  let* cycles_to_cement =
    let* cycles =
      (loop
         []
         initial_pred
         sorted_lpbl
       [@profiler.record_s
         {verbosity = Debug; metadata = [("prometheus", "")]}
           "retrieve cycle to cement"])
    in
    return (may_shrink_cycles cycles ~cycle_size_limit)
  in
  let* new_savepoint =
    (compute_new_savepoint
       block_store
       history_mode
       ~new_store
       ~min_level_to_preserve
       ~new_head
       ~cycles_to_cement
     [@profiler.record_s
       {verbosity = Debug; metadata = [("prometheus", "")]}
         "compute new savepoint"])
  in
  let* new_caboose =
    (compute_new_caboose
       block_store
       history_mode
       ~new_savepoint
       ~min_level_to_preserve
       ~new_head
     [@profiler.record_s
       {verbosity = Debug; metadata = [("prometheus", "")]}
         "compute new caboose"])
  in
  return (cycles_to_cement, new_savepoint, new_caboose)

let find_floating_store_by_kind block_store kind =
  List.find_opt
    (fun floating_store -> kind = Floating_block_store.kind floating_store)
    (block_store.rw_floating_block_store :: block_store.ro_floating_block_stores)

let move_floating_store block_store ~src:floating_store ~dst_kind =
  let open Lwt_result_syntax in
  let src_kind = Floating_block_store.kind floating_store in
  let* () = fail_when (src_kind = dst_kind) Wrong_floating_kind_swap in
  (* If the destination floating store exists, try closing it. *)
  let*! () =
    match find_floating_store_by_kind block_store dst_kind with
    | Some old_floating_store ->
        Floating_block_store.swap ~src:floating_store ~dst:old_floating_store
    | None ->
        let src_floating_store_dir_path =
          Naming.(
            floating_blocks_dir block_store.chain_dir src_kind |> dir_path)
        in
        let dst_floating_store_dir_path =
          Naming.(
            floating_blocks_dir block_store.chain_dir dst_kind |> dir_path)
        in
        Lwt_unix.rename src_floating_store_dir_path dst_floating_store_dir_path
  in
  return_unit

(* This function must be called after the former [RO] and [RW] were
   merged together and that the new [RW] is in place. *)
let move_all_floating_stores block_store ~new_ro_store =
  let open Lwt_result_syntax in
  let chain_dir = block_store.chain_dir in
  protect
    ~on_error:(fun err ->
      (* on error: restore all stores *)
      let*! () =
        List.iter_s
          Floating_block_store.close
          (block_store.rw_floating_block_store
         :: block_store.ro_floating_block_stores)
      in
      let*! r =
        protect (fun () ->
            let*! ro = Floating_block_store.init chain_dir ~readonly:false RO in
            block_store.ro_floating_block_stores <- [ro] ;
            let*! rw = Floating_block_store.init chain_dir ~readonly:false RW in
            block_store.rw_floating_block_store <- rw ;
            return_unit)
      in
      match r with
      | Ok () -> Lwt.return (Error err)
      | Error errs' -> Lwt.return_error (TzTrace.conp errs' err))
    (fun () ->
      (* (atomically?) Promote [new_ro] to [ro] *)
      let* () =
        (move_floating_store
           block_store
           ~src:new_ro_store
           ~dst_kind:RO
         [@profiler.record_s
           {verbosity = Debug; metadata = [("prometheus", "")]}
             "promote new ro floating as ro"])
      in
      (* ...and [new_rw] to [rw]  *)
      let* () =
        (move_floating_store
           block_store
           ~src:block_store.rw_floating_block_store
           ~dst_kind:RW
         [@profiler.record_s
           {verbosity = Debug; metadata = [("prometheus", "")]}
             "promote new rw floating as rw"])
      in
      (* Load the swapped stores *)
      (let*! ro = Floating_block_store.init chain_dir ~readonly:false RO in
       block_store.ro_floating_block_stores <- [ro] ;
       let*! rw = Floating_block_store.init chain_dir ~readonly:false RW in
       block_store.rw_floating_block_store <- rw ;
       return_unit)
      [@profiler.record_s
        {verbosity = Debug; metadata = [("prometheus", "")]}
          "open new floating stores"])

let check_store_consistency block_store ~cementing_highwatermark =
  let open Lwt_result_syntax in
  match
    Cemented_block_store.get_highest_cemented_level block_store.cemented_store
  with
  | None ->
      (* First merge or Rolling 0 *)
      return_unit
  | Some highest_cemented_level ->
      fail_unless
        Compare.Int32.(highest_cemented_level = cementing_highwatermark)
        (Store_errors.Inconsistent_cemented_store
           (Inconsistent_highest_cemented_level
              {highest_cemented_level; cementing_highwatermark}))

(* We want to keep in the floating store, at least, the blocks above
   (new_head.lpbl - (new_head.lpbl).max_op_ttl)). Important: we might
   not have this block so it should be treated as a potential lower
   bound. Furethermore, we consider the current caboose as a potential
   lower bound. *)
let compute_lowest_bound_to_preserve_in_floating block_store ~new_head
    ~new_head_metadata =
  let open Lwt_result_syntax in
  (* Safety check: is the highwatermark consistent with our highest
     cemented block *)
  let lpbl = Block_repr.last_preserved_block_level new_head_metadata in
  let* lpbl_block =
    trace
      Missing_last_preserved_block
      (read_predecessor_block_by_level
         block_store
         ~read_metadata:true
         ~head:new_head
         lpbl)
  in
  return
    (Int32.sub
       lpbl
       (Int32.of_int
          (match Block_repr.metadata lpbl_block with
          | None ->
              (* FIXME: this is not valid but it is a good
                 approximation of the max_op_ttl of a block where the
                 metadata is missing. *)
              Block_repr.max_operations_ttl new_head_metadata
          | Some metadata -> Block_repr.max_operations_ttl metadata)))

let instantiate_temporary_floating_store block_store =
  let open Lwt_result_syntax in
  protect
    ~on_error:(fun err ->
      (match block_store.ro_floating_block_stores with
      | [old_rw; old_ro] ->
          block_store.rw_floating_block_store <- old_rw ;
          block_store.ro_floating_block_stores <- [old_ro]
      | [_] -> ()
      | _ -> assert false) ;
      Lwt.return (Error err))
    (fun () ->
      trace
        Cannot_instanciate_temporary_floating_store
        (assert (
           Compare.List_length_with.(block_store.ro_floating_block_stores = 1)) ;
         let ro_store =
           List.hd block_store.ro_floating_block_stores
           |> WithExceptions.Option.get ~loc:__LOC__
         in
         let rw_store = block_store.rw_floating_block_store in
         block_store.ro_floating_block_stores <-
           block_store.rw_floating_block_store
           :: block_store.ro_floating_block_stores ;
         let*! new_rw_store =
           (Floating_block_store.init
              block_store.chain_dir
              ~readonly:false
              RW_TMP
            [@profiler.record_s
              {verbosity = Debug; metadata = [("prometheus", "")]}
                "initializing RW TMP"])
         in
         block_store.rw_floating_block_store <- new_rw_store ;
         return (ro_store, rw_store, new_rw_store)))

let create_lockfile path chain_dir =
  let open Lwt_syntax in
  protect (fun () ->
      let* fd =
        Lwt_unix.openfile
          (path chain_dir |> Naming.file_path)
          [Unix.O_CREAT; O_RDWR; O_CLOEXEC; O_SYNC]
          0o777
      in
      return_ok fd)

let lock lockfile = Lwt_unix.lockf lockfile Unix.F_LOCK 0

let unlock lockfile = Lwt_unix.lockf lockfile Unix.F_ULOCK 0

let create_merging_thread block_store ~history_mode ~old_ro_store ~old_rw_store
    ~new_head ~new_head_lpbl ~lowest_bound_to_preserve_in_floating
    ~cementing_highwatermark ~cycle_size_limit =
  let open Lwt_result_syntax in
  let*! () = Store_events.(emit start_merging_thread) () in
  let*! new_ro_store =
    (Floating_block_store.init
       block_store.chain_dir
       ~readonly:false
       RO_TMP
     [@profiler.record_s
       {verbosity = Debug; metadata = [("prometheus", "")]}
         "initializing RO TMP floating store"])
  in
  let* new_savepoint, new_caboose =
    Lwt.catch
      (fun () ->
        let* cycles_interval_to_cement, new_savepoint, new_caboose =
          (update_floating_stores
             block_store
             ~history_mode
             ~ro_store:old_ro_store
             ~rw_store:old_rw_store
             ~new_store:new_ro_store
             ~new_head
             ~new_head_lpbl
             ~lowest_bound_to_preserve_in_floating
             ~cementing_highwatermark
             ~cycle_size_limit
           [@profiler.record_s
             {verbosity = Debug; metadata = [("prometheus", "")]}
               "update floating stores"])
        in
        let*! () =
          Store_events.(emit cementing_block_ranges) cycles_interval_to_cement
        in
        let cycle_reader =
          read_iterator_block_range_in_floating_stores
            block_store
            ~ro_store:old_ro_store
            ~rw_store:old_rw_store
            ~head:new_head
        in
        let* () =
          match history_mode with
          | History_mode.Archive ->
              List.iter_es
                (fun cycle_range ->
                  let* chunk_iterator = cycle_reader cycle_range in
                  (* In archive, we store the metadatas *)
                  cement_blocks
                    ~write_metadata:true
                    block_store
                    chunk_iterator
                    ~cycle_range)
                cycles_interval_to_cement
          | Rolling offset ->
              let offset =
                (Option.value
                   offset
                   ~default:History_mode.default_additional_cycles)
                  .offset
              in
              if offset > 0 then
                let* () =
                  List.iter_es
                    (fun cycle_range ->
                      let* chunk_iterator = cycle_reader cycle_range in
                      cement_blocks
                        ~write_metadata:true
                        block_store
                        chunk_iterator
                        ~cycle_range)
                    cycles_interval_to_cement
                in
                (* Clean-up the files that are below the offset *)
                let*! () =
                  let*! () = lock block_store.lockfile in
                  Lwt.finalize
                    (fun () ->
                      Cemented_block_store.trigger_gc
                        block_store.cemented_store
                        history_mode)
                    (fun () -> unlock block_store.lockfile)
                in
                return_unit
              else (* Don't cement any cycles! *)
                return_unit
          | Full offset ->
              let offset =
                (Option.value
                   offset
                   ~default:History_mode.default_additional_cycles)
                  .offset
              in
              if offset > 0 then
                (* If the [offset] > 0 then the cemented store's GC should be
                   called to clean-up old cycles. *)
                let* () =
                  List.iter_es
                    (fun cycle_range ->
                      let* chunk_iterator = cycle_reader cycle_range in
                      cement_blocks
                        ~write_metadata:true
                        block_store
                        chunk_iterator
                        ~cycle_range)
                    cycles_interval_to_cement
                in
                (* Clean-up the files that are below the offset *)
                let*! () =
                  let*! () = lock block_store.lockfile in
                  Lwt.finalize
                    (fun () ->
                      Cemented_block_store.trigger_gc
                        block_store.cemented_store
                        history_mode)
                    (fun () -> unlock block_store.lockfile)
                in
                return_unit
              else
                List.iter_es
                  (fun cycle_range ->
                    let* chunk_iterator = cycle_reader cycle_range in
                    (* In full 0, we do not store the metadata *)
                    cement_blocks
                      ~write_metadata:false
                      block_store
                      chunk_iterator
                      ~cycle_range)
                  cycles_interval_to_cement
        in
        return (new_savepoint, new_caboose))
      (fun exn ->
        let*! () = Floating_block_store.close new_ro_store in
        Lwt.fail exn)
  in
  return (new_ro_store, new_savepoint, new_caboose)

let may_trigger_gc ~disable_context_pruning block_store history_mode
    ~previous_savepoint ~new_savepoint =
  let open Lwt_result_syntax in
  if not disable_context_pruning then
    let savepoint_hash = fst new_savepoint in
    if
      History_mode.(equal history_mode Archive)
      || Block_hash.(savepoint_hash = fst previous_savepoint)
    then (* No GC required *) return_unit
    else
      match block_store.gc_callback with
      | None -> return_unit
      | Some gc ->
          let*! () = Store_events.(emit start_context_gc new_savepoint) in
          gc savepoint_hash
  else return_unit

let split_context block_store new_head_lpbl =
  let open Lwt_result_syntax in
  match block_store.split_callback with
  | None -> return_unit
  | Some split ->
      let*! () = Store_events.(emit start_context_split new_head_lpbl) in
      split ()

let merge_stores ?(cycle_size_limit = default_cycle_size_limit) block_store
    ~(on_error : tztrace -> unit tzresult Lwt.t) ~finalizer ~history_mode
    ~new_head ~new_head_metadata ~cementing_highwatermark
    ~disable_context_pruning =
  let open Lwt_result_syntax in
  let* () = fail_when block_store.readonly Cannot_write_in_readonly in
  (* Do not allow multiple merges: force waiting for a potential
     previous merge. *)
  ()
  [@profiler.reset_block_section
    {verbosity = Notice} (Block_repr.hash new_head)] ;
  let*! () = Lwt_mutex.lock block_store.merge_mutex in
  protect
    ~on_error:(fun err ->
      Lwt_mutex.unlock block_store.merge_mutex ;
      Lwt.return (Error err))
    (fun () ->
      let*! store_status = status block_store in
      let* () =
        fail_unless
          (Block_store_status.is_idle store_status)
          (Cannot_merge_store
             {status = Format.asprintf "%a" Block_store_status.pp store_status})
      in
      (* Mark the store's status as Merging *)
      let* () =
        Lwt.finalize
          (fun () ->
            (let*! () = lock block_store.stored_data_lockfile in
             Block_store_status.set_merge_status block_store.status_data)
            [@profiler.record_s
              {verbosity = Notice; metadata = [("prometheus", "")]}
                "write status"])
          (fun () -> unlock block_store.stored_data_lockfile)
      in
      let new_head_lpbl =
        Block_repr.last_preserved_block_level new_head_metadata
      in
      let*! () = Store_events.(emit start_merging_stores) new_head_lpbl in
      let* () = check_store_consistency block_store ~cementing_highwatermark in
      let*! previous_savepoint = Stored_data.get block_store.savepoint in
      let* lowest_bound_to_preserve_in_floating =
        compute_lowest_bound_to_preserve_in_floating
          block_store
          ~new_head
          ~new_head_metadata
      in
      let merge_start = Time.System.now () in
      let* () =
        Lwt_idle_waiter.force_idle block_store.merge_scheduler (fun () ->
            (* Move the rw in the ro stores and create a new tmp *)
            let* old_ro_store, old_rw_store, _new_rw_store =
              Lwt.finalize
                (fun () ->
                  (* Lock the block store to avoid RO instances to open the
                     state while the file descriptors are being updated. *)
                  let*! () = lock block_store.lockfile in

                  (instantiate_temporary_floating_store
                     block_store
                   [@profiler.record_s
                     {
                       verbosity = Notice;
                       metadata =
                         [
                           ("prometheus", "instantiate_temporary_floating_store");
                         ];
                     }
                       "instanciate tmp floating stores"]))
                (fun () -> unlock block_store.lockfile)
            in
            (* Important: do not clean-up the temporary stores on
               failures as they will delete the recently arrived
               blocks. *)
            (* Create the merging thread that we want to run in background *)
            (* Clean-up on cancel/exn *)
            let merging_thread : unit tzresult Lwt.t =
              let* () =
                (Lwt.finalize
                   (fun () ->
                     protect
                       ~on_error:(fun err ->
                         (* Failures should be handled using [get_merge_status] *)
                         let msg = Format.asprintf "%a" pp_print_trace err in
                         let*! () =
                           Store_events.(emit merge_error)
                             (cementing_highwatermark, new_head_lpbl, msg)
                         in
                         on_error (Merge_error :: err))
                       (fun () ->
                         let* new_ro_store, new_savepoint, new_caboose =
                           (create_merging_thread
                              block_store
                              ~cycle_size_limit
                              ~history_mode
                              ~old_ro_store
                              ~old_rw_store
                              ~new_head
                              ~new_head_lpbl
                              ~lowest_bound_to_preserve_in_floating
                              ~cementing_highwatermark
                            [@profiler.record_s
                              {
                                verbosity = Info;
                                metadata = [("prometheus", "")];
                              }
                                "create_merging_thread"])
                         in
                         let* () =
                           Lwt_idle_waiter.force_idle
                             block_store.merge_scheduler
                             (fun () ->
                               Lwt.finalize
                                 (fun () ->
                                   (* Lock the block store to avoid RO
                                      instances to open the state while
                                      the file descriptors are being
                                      updated. *)
                                   let*! () = lock block_store.lockfile in
                                   (* Critical section: update on-disk values *)
                                   let* () =
                                     (move_all_floating_stores
                                        block_store
                                        ~new_ro_store
                                      [@profiler.record_s
                                        {
                                          verbosity = Info;
                                          metadata = [("prometheus", "")];
                                        }
                                          "move_all_floating_stores"])
                                   in
                                   let*! () =
                                     lock block_store.stored_data_lockfile
                                   in
                                   let* () =
                                     (write_caboose
                                        block_store
                                        new_caboose
                                      [@profiler.record_s
                                        {
                                          verbosity = Info;
                                          metadata = [("prometheus", "")];
                                        }
                                          "write_caboose"])
                                   in
                                   let* () =
                                     (write_savepoint
                                        block_store
                                        new_savepoint
                                      [@profiler.record_s
                                        {
                                          verbosity = Info;
                                          metadata = [("prometheus", "")];
                                        }
                                          "write_savepoint"])
                                   in
                                   return_unit)
                                 (fun () ->
                                   let*! () =
                                     unlock block_store.stored_data_lockfile
                                   in
                                   unlock block_store.lockfile))
                         in
                         (* We can now trigger the context GC: if the
                            GC is performed, this call will block until
                            its end. *)
                         let* () =
                           (may_trigger_gc
                              ~disable_context_pruning
                              block_store
                              history_mode
                              ~previous_savepoint
                              ~new_savepoint
                            [@profiler.record_s
                              {
                                verbosity = Info;
                                metadata = [("prometheus", "")];
                              }
                                "performing GC"])
                         in
                         (* Don't call the finalizer in the critical
                            section, in case it needs to access the block
                            store. *)
                         let* () = finalizer new_head_lpbl in
                         (* The merge operation succeeded, the store is
                            now idle. *)
                         block_store.merging_thread <- None ;
                         let* () =
                           (Lwt.finalize
                              (fun () ->
                                let*! () =
                                  lock block_store.stored_data_lockfile
                                in
                                Block_store_status.set_idle_status
                                  block_store.status_data)
                              (fun () ->
                                unlock block_store.stored_data_lockfile)
                            [@profiler.record_s
                              {
                                verbosity = Info;
                                metadata = [("prometheus", "")];
                              }
                                "set idle status"])
                         in
                         return_unit))
                   (fun () ->
                     Lwt_mutex.unlock block_store.merge_mutex ;
                     Lwt.return_unit)
                 [@profiler.record_s
                   {verbosity = Notice; metadata = [("prometheus", "")]}
                     "merging thread"])
              in
              let merge_end = Time.System.now () in
              let merging_time = Ptime.diff merge_end merge_start in
              let*! () = Store_events.(emit end_merging_stores) merging_time in
              Prometheus.Gauge.set
                Store_metrics.metrics.last_store_merge_time
                (Ptime.Span.to_float_s merging_time) ;
              return_unit
            in
            block_store.merging_thread <- Some (new_head_lpbl, merging_thread) ;
            (* Temporary stores in place and the merging thread was
                started: we can now release the hard-lock. *)
            return_unit)
      in
      return_unit)

let get_merge_status block_store =
  match block_store.merging_thread with
  | None -> Not_running
  | Some (_target, th) -> (
      match Lwt.state th with
      | Lwt.Sleep -> Running
      | Lwt.Return (Ok ()) -> Not_running
      | Lwt.Return (Error errs) -> Merge_failed errs
      | Lwt.Fail exn -> Merge_failed [Exn exn])

let merge_temporary_floating block_store =
  let open Lwt_result_syntax in
  let chain_dir = block_store.chain_dir in
  let*! () =
    List.iter_s
      Floating_block_store.close
      (block_store.rw_floating_block_store
     :: block_store.ro_floating_block_stores)
  in
  (* Remove RO_TMP if it still exists *)
  let ro_tmp_floating_store_dir_path =
    Naming.floating_blocks_dir chain_dir RO_TMP |> Naming.dir_path
  in
  let*! () = Lwt_utils_unix.remove_dir ro_tmp_floating_store_dir_path in
  (* If RW_TMP exists, merge RW and RW_TMP into one new
     single floating_store RW_RESTORE then swap it with
     the previous one. *)
  let*! rw_restore =
    Floating_block_store.init chain_dir ~readonly:false (Restore RW)
  in
  let* () =
    Lwt.finalize
      (fun () ->
        let*! rw = Floating_block_store.init chain_dir ~readonly:true RW in
        let*! rw_tmp =
          Floating_block_store.init chain_dir ~readonly:true RW_TMP
        in
        let* () =
          Floating_block_store.append_floating_store ~from:rw ~into:rw_restore
        in
        let* () =
          Floating_block_store.append_floating_store
            ~from:rw_tmp
            ~into:rw_restore
        in
        let*! () = Floating_block_store.swap ~src:rw_restore ~dst:rw in
        let*! () = Floating_block_store.delete_files rw_tmp in
        return_unit)
      (fun () -> Floating_block_store.delete_files rw_restore)
  in
  (* Re-instantiate RO and RW *)
  let*! ro = Floating_block_store.init chain_dir ~readonly:false RO in
  let*! rw = Floating_block_store.init chain_dir ~readonly:false RW in
  block_store.ro_floating_block_stores <- [ro] ;
  block_store.rw_floating_block_store <- rw ;
  Block_store_status.set_idle_status block_store.status_data

(* Removes the potentially leftover temporary files from the cementing
   of cycles. *)
let may_clean_cementing_artifacts block_store =
  let open Lwt_syntax in
  let chain_dir = block_store.chain_dir in
  let cemented_path = Naming.cemented_blocks_dir chain_dir |> Naming.dir_path in
  let rec loop dir =
    let* s = Lwt_unix.readdir dir in
    match s with
    | s when Filename.extension s = ".tmp" ->
        let* () = Lwt_unix.unlink (Filename.concat cemented_path s) in
        loop dir
    | _ -> loop dir
  in
  let* b = Lwt_unix.file_exists cemented_path in
  match b with
  | true ->
      let* dir = Lwt_unix.opendir cemented_path in
      Unit.catch_s
        ~catch_only:(function End_of_file -> true | _ -> false)
        (fun () ->
          Lwt.finalize (fun () -> loop dir) (fun () -> Lwt_unix.closedir dir))
  | false -> Lwt.return_unit

let may_recover_merge block_store =
  let open Lwt_result_syntax in
  let* () = fail_when block_store.readonly Cannot_write_in_readonly in
  let* () =
    Lwt_idle_waiter.force_idle block_store.merge_scheduler (fun () ->
        Lwt_mutex.with_lock block_store.merge_mutex (fun () ->
            let*! status = Stored_data.get block_store.status_data in
            if Block_store_status.is_idle status then return_unit
            else
              let*! () = Store_events.(emit recover_merge ()) in
              merge_temporary_floating block_store))
  in
  (* Try to clean temporary file anyway. *)
  let*! () = may_clean_cementing_artifacts block_store in
  return_unit

let load ?block_cache_limit chain_dir ~genesis_block ~readonly =
  let open Lwt_result_syntax in
  let* lockfile = create_lockfile Naming.block_store_lockfile chain_dir in
  let* block_store =
    Lwt.finalize
      (fun () ->
        let*! () = lock lockfile in
        let* cemented_store = Cemented_block_store.init chain_dir ~readonly in
        let*! ro_floating_block_store =
          Floating_block_store.init chain_dir ~readonly RO
        in
        let ro_floating_block_stores = [ro_floating_block_store] in
        let*! rw_floating_block_store =
          Floating_block_store.init chain_dir ~readonly RW
        in
        let genesis_descr = Block_repr.descriptor genesis_block in
        let* savepoint =
          Stored_data.init
            (Naming.savepoint_file chain_dir)
            ~initial_data:genesis_descr
        in
        let*! _, savepoint_level = Stored_data.get savepoint in
        Prometheus.Gauge.set
          Store_metrics.metrics.savepoint_level
          (Int32.to_float savepoint_level) ;
        let* caboose =
          Stored_data.init
            (Naming.caboose_file chain_dir)
            ~initial_data:genesis_descr
        in
        let*! _, caboose_level = Stored_data.get caboose in
        Prometheus.Gauge.set
          Store_metrics.metrics.caboose_level
          (Int32.to_float caboose_level) ;
        let* status_data =
          Stored_data.init
            (Naming.block_store_status_file chain_dir)
            ~initial_data:Block_store_status.create_idle_status
        in
        let block_cache =
          Block_lru_cache.create
            (Option.value block_cache_limit ~default:default_block_cache_limit)
        in
        let merge_scheduler = Lwt_idle_waiter.create () in
        let merge_mutex = Lwt_mutex.create () in
        let* stored_data_lockfile =
          create_lockfile Naming.stored_data_lockfile chain_dir
        in
        let block_store =
          {
            chain_dir;
            genesis_block;
            readonly;
            cemented_store;
            ro_floating_block_stores;
            rw_floating_block_store;
            caboose;
            savepoint;
            status_data;
            block_cache;
            gc_callback = None;
            split_callback = None;
            merge_mutex;
            merge_scheduler;
            merging_thread = None;
            lockfile;
            stored_data_lockfile;
          }
        in
        let* () =
          if not readonly then may_recover_merge block_store else return_unit
        in
        let*! status = Stored_data.get status_data in
        let* () =
          fail_when
            (Block_store_status.is_merging status && not readonly)
            Cannot_load_degraded_store
        in
        return block_store)
      (fun () -> unlock lockfile)
  in
  return block_store

let lock_block_store {lockfile; _} = lock lockfile

let unlock_block_store {lockfile; _} = unlock lockfile

let sync ~last_status block_store =
  let open Lwt_result_syntax in
  Lwt.finalize
    (fun () ->
      let*! () = lock block_store.lockfile in
      let chain_dir = block_store.chain_dir in
      let readonly = true in
      let* stored_status =
        Stored_data.load (Naming.block_store_status_file chain_dir)
      in
      let*! current_status = Stored_data.get stored_status in
      let fbs =
        block_store.rw_floating_block_store
        :: block_store.ro_floating_block_stores
      in
      (* Prepare resources cleaners for former resources. *)
      let cleanups () = List.iter_s Floating_block_store.close fbs in
      let no_cleanup () = Lwt.return_unit in
      let is_last_status_merging = Block_store_status.is_merging last_status in
      let is_current_status_merging =
        Block_store_status.is_merging current_status
      in
      let last_status_id = Block_store_status.get_status_value last_status in
      let current_status_id =
        Block_store_status.get_status_value current_status
      in
      let* new_block_store, cleanups =
        match (is_last_status_merging, is_current_status_merging) with
        | (false, false | true, true) when last_status_id = current_status_id ->
            (* The status has not changed (Idle to Idle or Merging to
               Merging where status ids are equal)since the last sync:
               we only reload the floating stores. *)
            let () = List.iter Floating_block_store.may_sync fbs in
            return (block_store, no_cleanup)
        | false, false | true, false ->
            (* A complete merge occurred (Idle to Idle where status
               ids are different) since the last sync or a merge (that
               was ongoing during the last sync) finished.
               We need to:
               - open the new floating stores and update some store
               values,
               - synchronize the new cemented store indexes and block
                 files. *)
            let*! ro_floating_block_store =
              Floating_block_store.init chain_dir ~readonly RO
            in
            let ro_floating_block_stores = [ro_floating_block_store] in
            let*! rw_floating_block_store =
              Floating_block_store.init chain_dir ~readonly RW
            in
            let () =
              Cemented_block_store.may_synchronize_indexes
                block_store.cemented_store
            in
            let* () =
              Cemented_block_store.reload_cemented_blocks_files
                block_store.cemented_store
            in
            let* savepoint =
              Stored_data.load (Naming.savepoint_file chain_dir)
            in
            let* caboose = Stored_data.load (Naming.caboose_file chain_dir) in
            let status_data = block_store.status_data in
            return
              ( {
                  block_store with
                  ro_floating_block_stores;
                  rw_floating_block_store;
                  savepoint;
                  caboose;
                  status_data;
                },
                cleanups )
        | false, true | true, true ->
            (* A merge has started (Idle to Merging) since the last
               sync or a merge that was ongoing during the last sync
               finished and a new one is now ongoing (Merging to
               Merging where status ids are different): we need to
               open the temporary floating stores and update some
               store values. *)
            let*! ro_floating_block_store =
              Floating_block_store.init chain_dir ~readonly RO
            in
            let*! rw_floating_block_store =
              Floating_block_store.init chain_dir ~readonly RW
            in
            let ro_floating_block_stores =
              [ro_floating_block_store; rw_floating_block_store]
            in
            let*! rw_floating_block_store =
              Floating_block_store.init chain_dir ~readonly RW_TMP
            in
            let* savepoint =
              Stored_data.load (Naming.savepoint_file chain_dir)
            in
            let* caboose = Stored_data.load (Naming.caboose_file chain_dir) in
            let status_data = block_store.status_data in
            return
              ( {
                  block_store with
                  ro_floating_block_stores;
                  rw_floating_block_store;
                  savepoint;
                  caboose;
                  status_data;
                },
                cleanups )
      in
      return (new_block_store, current_status, cleanups))
    (fun () -> unlock block_store.lockfile)

let create ?block_cache_limit chain_dir ~genesis_block =
  let open Lwt_result_syntax in
  let* block_store =
    load chain_dir ?block_cache_limit ~genesis_block ~readonly:false
  in
  let* () =
    store_block
      block_store
      genesis_block
      genesis_block.contents.header.shell.context
  in
  return block_store

let register_gc_callback block_store gc_callback =
  block_store.gc_callback <- gc_callback

let register_split_callback block_store split_callback =
  block_store.split_callback <- split_callback

let pp_merge_status fmt status =
  match status with
  | Not_running -> Format.fprintf fmt "not running"
  | Running -> Format.fprintf fmt "running"
  | Merge_failed err -> Format.fprintf fmt "merge failed %a" pp_print_trace err

let await_merging block_store =
  let open Lwt_syntax in
  let* () = Lwt_mutex.lock block_store.merge_mutex in
  let thread = block_store.merging_thread in
  Lwt_mutex.unlock block_store.merge_mutex ;
  match thread with
  | None -> Lwt.return_unit
  | Some (_, th) ->
      let* _ = th in
      Lwt.return_unit

let close block_store =
  let open Lwt_syntax in
  (* Wait a bit for the merging to end but hard-stop it if it takes
     too long. *)
  let* () =
    match get_merge_status block_store with
    | Not_running | Merge_failed _ -> Lwt.return_unit
    | Running ->
        let* () = Store_events.(emit try_waiting_for_merge_termination) () in
        Lwt_unix.with_timeout 5. (fun () ->
            let* () = await_merging block_store in
            Lwt.return_unit)
  in
  Cemented_block_store.close block_store.cemented_store ;
  List.iter_s
    Floating_block_store.close
    (block_store.rw_floating_block_store :: block_store.ro_floating_block_stores)

let stat_metadata_cycles block_store =
  Cemented_block_store.stat_metadata_cycles block_store.cemented_store

(***************** Upgrade to V3.1 *****************)

let v_3_1_upgrade chain_dir =
  let open Lwt_result_syntax in
  let legacy_block_store_status_file =
    Naming.legacy_block_store_status_file chain_dir
  in
  let*! exists = Lwt_unix.file_exists legacy_block_store_status_file.path in
  if exists then
    (* Load using the legacy block_store_status encoding *)
    let*! () = Store_events.(emit load_block_store_status ()) in
    let* legacy_status_data =
      Stored_data.init
        legacy_block_store_status_file
        ~initial_data:Block_store_status.Legacy.create_idle_status
    in
    (* Convert to the new encoding *)
    let* status = Block_store_status.of_legacy legacy_status_data in
    (* Overwrite the status file *)
    let* () =
      Stored_data.write_file (Naming.block_store_status_file chain_dir) status
    in
    let*! () = Store_events.(emit fixed_block_store_status ()) in
    return_unit
  else (* The store is not containing any data to upgrade yet.*) return_unit
