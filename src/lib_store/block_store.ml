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
open Block_repr
open Store_errors

let default_block_cache_limit = 100

type merge_status = Not_running | Running | Merge_failed of tztrace

type status = Naming.block_store_status = Idle | Merging

type block_store = {
  chain_dir : [`Chain_dir] Naming.directory;
  readonly : bool;
  genesis_block : Block_repr.t;
  cemented_store : Cemented_block_store.t;
  mutable ro_floating_block_stores : Floating_block_store.t list;
  mutable rw_floating_block_store : Floating_block_store.t;
  caboose : block_descriptor Stored_data.t;
  savepoint : block_descriptor Stored_data.t;
  status_data : status Stored_data.t;
  block_cache : Block_repr.t Block_lru_cache.t;
  merge_mutex : Lwt_mutex.t;
  merge_scheduler : Lwt_idle_waiter.t;
  (* Target level x Merging thread *)
  mutable merging_thread : (int32 * unit tzresult Lwt.t) option;
}

type t = block_store

type key = Block of (Block_hash.t * int)

let status_encoding =
  let open Data_encoding in
  conv
    (function Idle -> false | Merging -> true)
    (function false -> Idle | true -> Merging)
    Data_encoding.bool

let status_to_string = function Idle -> "idle" | Merging -> "merging"

let cemented_block_store {cemented_store; _} = cemented_store

let floating_block_stores {ro_floating_block_stores; rw_floating_block_store; _}
    =
  List.rev (rw_floating_block_store :: ro_floating_block_stores)

let savepoint {savepoint; _} = Stored_data.get savepoint

let caboose {caboose; _} = Stored_data.get caboose

let status {status_data; _} = Stored_data.get status_data

let write_savepoint {savepoint; _} v =
  Stored_data.write savepoint v >>=? fun () ->
  Prometheus.Gauge.set
    Store_metrics.metrics.savepoint_level
    (Int32.to_float (snd v)) ;
  return_unit

let write_caboose {caboose; _} v =
  Stored_data.write caboose v >>=? fun () ->
  Prometheus.Gauge.set
    Store_metrics.metrics.caboose_level
    (Int32.to_float (snd v)) ;
  return_unit

let genesis_block {genesis_block; _} = genesis_block

let write_status {status_data; _} status = Stored_data.write status_data status

(** [global_predecessor_lookup chain_block_store hash pow_nth] retrieves
    the 2^[pow_nth] predecessor's hash from the block with corresponding
    [hash] by checking all stores iteratively. Returns [None] if the
    predecessor is not found or if it is below genesis. *)
let global_predecessor_lookup block_store hash pow_nth =
  (* pow_nth = 0 => direct predecessor *)
  (* Look in the RW block_store, then RO stores and finally in the
     cemented store *)
  Lwt_utils.find_map_s
    (fun floating_store ->
      Floating_block_store.find_predecessors floating_store hash >>= function
      | None -> Lwt.return_none
      | Some predecessors -> Lwt.return (List.nth_opt predecessors pow_nth))
    (block_store.rw_floating_block_store :: block_store.ro_floating_block_stores)
  >>= function
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
  let rec loop predecessors_acc pred dist =
    if dist = Floating_block_index.Block_info.max_predecessors then
      Lwt.return predecessors_acc
    else
      global_predecessor_lookup block_store pred (dist - 1) >>= function
      | None -> Lwt.return predecessors_acc
      | Some pred' -> loop (pred' :: predecessors_acc) pred' (dist + 1)
  in
  let predecessor = predecessor block in
  if Block_hash.equal block.hash predecessor then
    (* genesis *)
    Lwt.return [block.hash]
  else
    loop [predecessor] predecessor 1 >>= fun rev_preds ->
    Lwt.return (List.rev rev_preds)

(** [get_hash block_store key] retrieves the block which is at
    [distance] from the block with corresponding [hash] by every store
    iteratively. *)
let get_hash block_store (Block (block_hash, offset)) =
  Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
      let closest_power_two n =
        if n < 0 then assert false
        else
          let rec loop cnt n = if n <= 1 then cnt else loop (cnt + 1) (n / 2) in
          loop 0 n
      in
      (* actual predecessor function *)
      if offset = 0 then return_some block_hash
      else if offset < 0 then fail (Wrong_predecessor (block_hash, offset))
      else
        let rec loop block_hash offset =
          if offset = 1 then
            global_predecessor_lookup block_store block_hash 0 >>= fun pred ->
            return pred
          else
            let power = closest_power_two offset in
            let power =
              if power < Floating_block_index.Block_info.max_predecessors then
                power
              else
                let power =
                  Floating_block_index.Block_info.max_predecessors - 1
                in
                power
            in
            global_predecessor_lookup block_store block_hash power >>= function
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
  Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
      get_hash block_store key >>=? function
      | None -> return_false
      | Some predecessor_hash
        when Block_hash.equal block_store.genesis_block.hash predecessor_hash ->
          return_true
      | Some predecessor_hash ->
          Lwt_list.exists_s
            (fun store -> Floating_block_store.mem store predecessor_hash)
            (block_store.rw_floating_block_store
             :: block_store.ro_floating_block_stores)
          >>= fun is_known_in_floating ->
          return
            (is_known_in_floating
            || Cemented_block_store.is_cemented
                 block_store.cemented_store
                 predecessor_hash))

let read_block ~read_metadata block_store key_kind =
  Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
      (* Resolve the hash *)
      get_hash block_store key_kind >>=? function
      | None -> return_none
      | Some adjusted_hash ->
          if Block_hash.equal block_store.genesis_block.hash adjusted_hash then
            return_some block_store.genesis_block
          else
            let fetch_block adjusted_hash =
              (* First look in the floating stores *)
              Lwt_utils.find_map_s
                (fun store ->
                  Floating_block_store.read_block store adjusted_hash)
                (block_store.rw_floating_block_store
                 :: block_store.ro_floating_block_stores)
              >>= function
              | Some block -> Lwt.return_some block
              | None -> (
                  (* Lastly, look in the cemented blocks *)
                  Cemented_block_store.get_cemented_block_by_hash
                    ~read_metadata
                    block_store.cemented_store
                    adjusted_hash
                  >>= function
                  | Ok v -> Lwt.return v
                  | Error _ -> Lwt.return_none)
            in
            Block_lru_cache.find_or_replace
              block_store.block_cache
              adjusted_hash
              fetch_block
            >>= fun block -> return block)

let read_block_metadata block_store key_kind =
  Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
      (* Resolve the hash *)
      get_hash block_store key_kind >>=? function
      | None -> return_none
      | Some adjusted_hash -> (
          if Block_hash.equal block_store.genesis_block.hash adjusted_hash then
            return (Block_repr.metadata block_store.genesis_block)
          else
            (* First look in the floating stores *)
            Lwt_utils.find_map_s
              (fun store -> Floating_block_store.read_block store adjusted_hash)
              (block_store.rw_floating_block_store
               :: block_store.ro_floating_block_stores)
            >>= function
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

let store_block block_store block =
  fail_when block_store.readonly Cannot_write_in_readonly >>=? fun () ->
  Lwt_idle_waiter.task block_store.merge_scheduler (fun () ->
      protect (fun () ->
          compute_predecessors block_store block >>= fun predecessors ->
          Block_lru_cache.replace
            block_store.block_cache
            block.hash
            (Lwt.return_some block) ;
          Floating_block_store.append_block
            block_store.rw_floating_block_store
            predecessors
            block
          >>= fun () -> return_unit))

let check_blocks_consistency blocks =
  let rec loop = function
    | [] | [_] -> true
    | pred :: (curr :: _ as r) ->
        let is_consistent =
          Block_hash.equal (predecessor curr) pred.hash
          && Compare.Int32.(level curr = Int32.succ (level pred))
        in
        is_consistent && loop r
  in
  loop blocks

let cement_blocks ?(check_consistency = true) ~write_metadata block_store blocks
    =
  (* No need to lock *)
  let {cemented_store; _} = block_store in
  let are_blocks_consistent = check_blocks_consistency blocks in
  fail_unless are_blocks_consistent Invalid_blocks_to_cement >>=? fun () ->
  Cemented_block_store.cement_blocks
    ~check_consistency
    cemented_store
    ~write_metadata
    blocks

(* [try_retrieve_n_predecessors stores block n] retrieves, at most,
   the [n] [block]'s predecessors (including [block]) from the
   floating stores. The resulting block list may be smaller than
   [n] and contains the oldest blocks first. *)
let try_retrieve_n_predecessors floating_stores block n =
  let rec loop acc predecessor_hash n =
    if n = 0 then Lwt.return acc
    else
      Lwt_utils.find_map_s
        (fun floating_store ->
          Floating_block_store.read_block_and_predecessors
            floating_store
            predecessor_hash)
        floating_stores
      >>= function
      | None ->
          (* The remaining blocks are not present, skip them. *)
          Lwt.return acc
      | Some ((block, _) as elt) ->
          let predecessor_hash = Block_repr.predecessor block in
          loop (elt :: acc) predecessor_hash (pred n)
  in
  loop [] block n

let read_predecessor_block_by_level_opt block_store ?(read_metadata = false)
    ~head level =
  read_block
    block_store
    ~read_metadata
    (Block
       (Block_repr.hash head, Int32.(to_int (sub (Block_repr.level head) level))))

let read_predecessor_block_by_level block_store ?(read_metadata = false) ~head
    level =
  let head_level = Block_repr.level head in
  let head_hash = Block_repr.hash head in
  let distance = Int32.(to_int (sub head_level level)) in
  read_block block_store ~read_metadata (Block (head_hash, distance))
  >>=? function
  | None ->
      if distance < 0 then fail (Bad_level {head_level; given_level = level})
      else fail (Block_not_found {hash = head_hash; distance})
  | Some b -> return b

(* TODO optimize this by reading chunks of contiguous data and
   filtering it afterwards? *)
let read_block_range_in_floating_stores block_store ~ro_store ~rw_store ~head
    (low, high) =
  read_predecessor_block_by_level block_store ~head high >>=? fun high_block ->
  let nb_blocks =
    Int32.(add one (sub high low) |> to_int)
    (* +1, it's a size *)
  in
  try_retrieve_n_predecessors
    [ro_store; rw_store]
    (Block_repr.hash high_block)
    nb_blocks
  >>= fun blocks ->
  let blocks = List.map fst blocks in
  assert (Compare.List_length_with.(blocks = nb_blocks)) ;
  return blocks

(* [expected_savepoint block_store target_offset] computes the
   expected savepoint based on the [target_offset]. When the
   [target_offset] cannot be satisfied, the previous savepoint is
   returned.*)
let expected_savepoint block_store ~target_offset =
  let cemented_store = cemented_block_store block_store in
  match Cemented_block_store.cemented_blocks_files cemented_store with
  | None ->
      savepoint block_store >>= fun current_savepoint ->
      Lwt.return (snd current_savepoint)
  | Some cemented_block_files ->
      let nb_files = Array.length cemented_block_files in
      if target_offset > nb_files || nb_files = 0 then
        (* We cannot provide a savepoint from the cemented block store *)
        savepoint block_store >>= fun current_savepoint ->
        Lwt.return (snd current_savepoint)
      else if target_offset = 0 then
        (* We get the successor of the highest cemented level *)
        let cycle = cemented_block_files.(nb_files - 1) in
        Lwt.return (Int32.succ cycle.end_level)
      else
        (* We get the lowest block of the targeted cycle *)
        let cycle = cemented_block_files.(nb_files - target_offset) in
        Lwt.return cycle.start_level

(* [available_savepoint block_store current_head savepoint_candidate]
   aims to check that the [savepoint_candidate] can be used as a valid
   savepoint (that is to say, contains metadata). It returns the
   [savepoint_candidate] block descriptor if it is valid. Returns the
   current savepoint otherwise. *)
let available_savepoint block_store current_head savepoint_candidate =
  let head_hash = Block_repr.hash current_head in
  savepoint block_store >>= fun current_savepoint ->
  let new_savepoint_level =
    if savepoint_candidate < snd current_savepoint then snd current_savepoint
    else savepoint_candidate
  in
  let distance =
    Int32.(to_int (sub (Block_repr.level current_head) new_savepoint_level))
  in
  (read_block ~read_metadata:false block_store (Block (head_hash, distance))
   >>=? function
   | Some b -> return b
   | None -> fail (Wrong_predecessor (head_hash, distance)))
  >>=? fun block -> return (descriptor block)

(* [preserved_block block_store current_head] returns the
   preserved block candidate level. The preserved block aims to be the
   one needed and maintained available to export snapshot. That is to
   say, the block: lafl(head) - max_op_ttl(lafl). *)
let preserved_block block_store current_head =
  let head_hash = Block_repr.hash current_head in
  read_block_metadata block_store (Block (head_hash, 0))
  >|=? WithExceptions.Option.get ~loc:__LOC__
  >>=? fun current_head_metadata ->
  let head_lafl = Block_repr.last_allowed_fork_level current_head_metadata in
  let head_max_op_ttl =
    Int32.of_int (Block_repr.max_operations_ttl current_head_metadata)
  in
  return Int32.(max 0l (sub head_lafl head_max_op_ttl))

(* [infer_savepoint block_store current_head ~target_offset] returns
   the savepoint candidate for an history mode switch. *)
let infer_savepoint block_store current_head ~target_offset =
  expected_savepoint block_store ~target_offset
  >>= fun expected_savepoint_level ->
  preserved_block block_store current_head >>=? fun preserved_savepoint_level ->
  let savepoint_candidate =
    min preserved_savepoint_level expected_savepoint_level
  in
  available_savepoint block_store current_head savepoint_candidate

(* [expected_caboose block_store ~target_offset] computes the
   expected caboose based on the [target_offset]). None is returned if
   the cemented store cannot satisfy the targeted offset.  *)
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
  match previous_history_mode with
  | History_mode.Archive -> (
      match new_history_mode with
      | History_mode.Archive ->
          fail
            (Cannot_switch_history_mode
               {
                 previous_mode = previous_history_mode;
                 next_mode = new_history_mode;
               })
      | Full _ -> caboose block_store >>= return
      | Rolling _ -> return savepoint)
  | Full _ -> (
      match expected_caboose block_store ~target_offset with
      | Some expected_caboose ->
          preserved_block block_store current_head >>=? fun preserved_caboose ->
          let new_caboose_level = min expected_caboose preserved_caboose in
          let head_hash = Block_repr.hash current_head in
          let distance =
            Int32.(
              to_int (sub (Block_repr.level current_head) new_caboose_level))
          in
          (read_block
             ~read_metadata:false
             block_store
             (Block (head_hash, distance))
           >>=? function
           | Some b -> return b
           | None -> fail (Wrong_predecessor (head_hash, distance)))
          >>=? fun block -> return (descriptor block)
      | None -> return savepoint)
  | Rolling r ->
      let offset =
        Option.value r ~default:History_mode.default_additional_cycles
      in
      if offset.offset < target_offset then caboose block_store >>= return
      else return savepoint

let switch_history_mode block_store ~current_head ~previous_history_mode
    ~new_history_mode =
  let open History_mode in
  match (previous_history_mode, new_history_mode) with
  | (Full _, Rolling m) | (Rolling _, Rolling m) ->
      let m =
        (Option.value m ~default:History_mode.default_additional_cycles).offset
      in
      (* Both the caboose and savepoint can be updated *)
      infer_savepoint block_store current_head ~target_offset:m
      >>=? fun new_savepoint ->
      infer_caboose
        block_store
        new_savepoint
        current_head
        ~target_offset:m
        ~new_history_mode
        ~previous_history_mode
      >>=? fun new_caboose ->
      let cemented_block_store = cemented_block_store block_store in
      Cemented_block_store.trigger_gc cemented_block_store new_history_mode
      >>= fun () ->
      write_savepoint block_store new_savepoint >>=? fun () ->
      write_caboose block_store new_caboose >>=? fun () -> return_unit
  | (Full _, Full m) ->
      let m =
        (Option.value m ~default:History_mode.default_additional_cycles).offset
      in
      (* Only the savepoint can be updated *)
      infer_savepoint block_store current_head ~target_offset:m
      >>=? fun new_savepoint ->
      Cemented_block_store.trigger_gc
        (cemented_block_store block_store)
        new_history_mode
      >>= fun () ->
      write_savepoint block_store new_savepoint >>=? fun () -> return_unit
  | (Archive, Full m) | (Archive, Rolling m) ->
      let m =
        (Option.value m ~default:History_mode.default_additional_cycles).offset
      in
      (* Both the caboose and savepoint can be updated *)
      infer_savepoint block_store current_head ~target_offset:m
      >>=? fun new_savepoint ->
      infer_caboose
        block_store
        new_savepoint
        current_head
        ~target_offset:m
        ~new_history_mode
        ~previous_history_mode
      >>=? fun new_caboose ->
      Cemented_block_store.trigger_gc
        (cemented_block_store block_store)
        new_history_mode
      >>= fun () ->
      write_savepoint block_store new_savepoint >>=? fun () ->
      write_caboose block_store new_caboose >>=? fun () -> return_unit
  | _ ->
      fail
        (Cannot_switch_history_mode
           {previous_mode = previous_history_mode; next_mode = new_history_mode})

let compute_new_savepoint block_store history_mode ~new_store
    ~min_level_to_preserve ~new_head ~cycles_to_cement =
  assert (cycles_to_cement <> []) ;
  Stored_data.get block_store.savepoint >>= fun savepoint ->
  match history_mode with
  | History_mode.Archive ->
      (* new_savepoint = savepoint = genesis *)
      return savepoint
  | Full offset | Rolling offset -> (
      let offset =
        (Option.value offset ~default:History_mode.default_additional_cycles)
          .offset
      in
      read_predecessor_block_by_level
        block_store
        ~head:new_head
        min_level_to_preserve
      >>=? fun min_block_to_preserve ->
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
             floating store nor in the cycles to cements. *)
          let (savepoint_hash, savepoint_level) = savepoint in
          let is_savepoint_in_cemented =
            List.exists
              (fun (l, h) -> l <= savepoint_level && savepoint_level <= h)
              cycles_to_cement
          in
          if not is_savepoint_in_cemented then
            Floating_block_store.mem new_store savepoint_hash
            >>= fun is_savepoint_in_new_store ->
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
            read_predecessor_block_by_level_opt
              block_store
              ~head:new_head
              shifted_savepoint_level
            >>=? function
            | None -> fail (Cannot_retrieve_savepoint shifted_savepoint_level)
            | Some savepoint -> return (Block_repr.descriptor savepoint))

let compute_new_caboose block_store history_mode ~new_savepoint
    ~min_level_to_preserve ~new_head =
  Stored_data.get block_store.caboose >>= fun caboose ->
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
        read_predecessor_block_by_level
          block_store
          ~head:new_head
          min_level_to_preserve
        >>=? fun min_block_to_preserve ->
        return (Block_repr.descriptor min_block_to_preserve)
      else return new_savepoint

module BlocksLAFL = Set.Make (Int32)

(* FIXME: update doc *)
(* [update_floating_stores block_store ~history_mode ~ro_store
   ~rw_store ~new_store ~new_head ~new_head_lafl
   ~lowest_bound_to_preserve_in_floating ~cementing_highwatermark]
   updates the [new_store] by storing the predecessors of the
   [new_head_lafl] and preserving the
   [lowest_bound_to_preserve_in_floating]. It returns the cycles to
   cement from [new_head] to [cementing_highwatermark] and the
   savepoint and caboose candidates. *)
let update_floating_stores block_store ~history_mode ~ro_store ~rw_store
    ~new_store ~new_head ~new_head_lafl ~lowest_bound_to_preserve_in_floating
    ~cementing_highwatermark =
  read_predecessor_block_by_level block_store ~head:new_head new_head_lafl
  >>=? fun lafl_block ->
  let (final_hash, final_level) = Block_repr.descriptor lafl_block in
  (* 1. Append to the new RO [new_store] blocks between
     [lowest_bound_to_preserve_in_floating] and [lafl_block].
     N.B. size in memory proportional to max_op_ttl of the lafl block
  *)
  let max_nb_blocks_to_retrieve =
    Compare.Int.(
      max
        1
        Int32.(
          add one (sub final_level lowest_bound_to_preserve_in_floating)
          |> to_int))
  in
  try_retrieve_n_predecessors
    (* Reverse the stores so that the oldest RO is first in the lookup. *)
    [ro_store; rw_store]
    final_hash
    max_nb_blocks_to_retrieve
  >>= fun lafl_predecessors ->
  (* [min_level_to_preserve] is the lowest block that we want to keep
     in the floating stores. *)
  let min_level_to_preserve =
    if lafl_predecessors <> [] then
      Block_repr.level
        (fst
           (List.hd lafl_predecessors |> WithExceptions.Option.get ~loc:__LOC__))
    else new_head_lafl
  in
  (* As blocks from [lafl_predecessors] contains older blocks first,
     the resulting [new_store] will contains newer blocks first. *)
  Lwt_list.iter_s
    (fun (block, predecessors) ->
      Floating_block_store.append_block new_store predecessors block)
    lafl_predecessors
  >>= fun () ->
  (* 2. Retrieve ALL cycles (potentially more than one) *)
  (* 2.1. We write back to the new store all the blocks from
     [lafl_block] to the end of the file(s).

     2.2 At the same time, retrieve the list of cycle bounds: i.e. the
     interval of blocks s.t. \forall b \in
     {stores}. cementing_highwatermark < b.lafl <= new_head_lafl

     HYPOTHESIS: all blocks at a given level have the same lafl. *)
  let visited = ref (Block_hash.Set.singleton (Block_repr.hash lafl_block)) in
  let blocks_lafl = ref BlocksLAFL.empty in
  List.iter_es
    (fun store ->
      Floating_block_store.iter_with_pred_s
        (fun (block, predecessors) ->
          (* Ignore blocks that are below the cementing highwatermark *)
          if Compare.Int32.(Block_repr.level block <= cementing_highwatermark)
          then return_unit
          else (
            (* Start by updating the set of cycles *)
            Option.iter
              (fun metadata ->
                let block_lafl = Block_repr.last_allowed_fork_level metadata in
                if
                  Compare.Int32.(
                    cementing_highwatermark < block_lafl
                    && block_lafl <= new_head_lafl)
                then blocks_lafl := BlocksLAFL.add block_lafl !blocks_lafl)
              (Block_repr.metadata block) ;
            (* Append block if its predecessor was visited and update
               the visited set. *)
            if Block_hash.Set.mem (Block_repr.predecessor block) !visited then (
              let hash = Block_repr.hash block in
              visited := Block_hash.Set.add hash !visited ;
              Floating_block_store.append_block new_store predecessors block
              >>= return)
            else return_unit))
        store)
    [ro_store; rw_store]
  >>=? fun () ->
  let is_cementing_highwatermark_genesis =
    Compare.Int32.(
      cementing_highwatermark = Block_repr.level block_store.genesis_block)
  in
  (* Return the range of cycles to cement. *)
  let rec loop acc pred = function
    | [] -> fail (Cannot_cement_blocks `Empty)
    | [h] ->
        assert (Compare.Int32.(h = new_head_lafl)) ;
        return (List.rev ((Int32.succ pred, h) :: acc))
    | h :: (h' :: _ as t) ->
        (* lafls are monotonous and strictly increasing *)
        assert (Compare.Int32.(h < h')) ;
        loop ((Int32.succ pred, h) :: acc) h t
  in
  let initial_pred =
    (* Hack to include genesis in the first cycle when the initial
       cementing highwatermark is genesis's lafl *)
    if is_cementing_highwatermark_genesis then
      Int32.pred cementing_highwatermark
    else cementing_highwatermark
  in
  let sorted_lafl =
    List.sort Compare.Int32.compare (BlocksLAFL.elements !blocks_lafl)
  in
  loop [] initial_pred sorted_lafl >>=? fun cycles_to_cement ->
  compute_new_savepoint
    block_store
    history_mode
    ~new_store
    ~min_level_to_preserve
    ~new_head
    ~cycles_to_cement
  >>=? fun new_savepoint ->
  compute_new_caboose
    block_store
    history_mode
    ~new_savepoint
    ~min_level_to_preserve
    ~new_head
  >>=? fun new_caboose -> return (cycles_to_cement, new_savepoint, new_caboose)

let find_floating_store_by_kind block_store kind =
  List.find_opt
    (fun floating_store -> kind = Floating_block_store.kind floating_store)
    (block_store.rw_floating_block_store :: block_store.ro_floating_block_stores)

let move_floating_store block_store ~src:floating_store ~dst_kind =
  let src_kind = Floating_block_store.kind floating_store in
  fail_when (src_kind = dst_kind) Wrong_floating_kind_swap >>=? fun () ->
  (* If the destination floating store exists, try closing it. *)
  (match find_floating_store_by_kind block_store dst_kind with
  | Some old_floating_store ->
      Floating_block_store.swap ~src:floating_store ~dst:old_floating_store
  | None ->
      let src_floating_store_dir_path =
        Naming.(floating_blocks_dir block_store.chain_dir src_kind |> dir_path)
      in
      let dst_floating_store_dir_path =
        Naming.(floating_blocks_dir block_store.chain_dir dst_kind |> dir_path)
      in
      Lwt_unix.rename src_floating_store_dir_path dst_floating_store_dir_path)
  >>= fun () -> return_unit

(* This function must be called after the former [RO] and [RW] were
   merged together and that the new [RW] is in place. *)
let move_all_floating_stores block_store ~new_ro_store =
  let chain_dir = block_store.chain_dir in
  protect
    ~on_error:(fun err ->
      (* on error: restore all stores *)
      Lwt_list.iter_s
        Floating_block_store.close
        (block_store.rw_floating_block_store
         :: block_store.ro_floating_block_stores)
      >>= fun () ->
      protect (fun () ->
          Floating_block_store.init chain_dir ~readonly:false RO >>= fun ro ->
          block_store.ro_floating_block_stores <- [ro] ;
          Floating_block_store.init chain_dir ~readonly:false RW >>= fun rw ->
          block_store.rw_floating_block_store <- rw ;
          return_unit)
      >>= function
      | Ok () -> Lwt.return (Error err)
      | Error errs' -> Lwt.return_error (TzTrace.conp errs' err))
    (fun () ->
      (* (atomically?) Promote [new_ro] to [ro] *)
      move_floating_store block_store ~src:new_ro_store ~dst_kind:RO
      >>=? fun () ->
      (* ...and [new_rw] to [rw]  *)
      move_floating_store
        block_store
        ~src:block_store.rw_floating_block_store
        ~dst_kind:RW
      >>=? fun () ->
      (* Load the swapped stores *)
      Floating_block_store.init chain_dir ~readonly:false RO >>= fun ro ->
      block_store.ro_floating_block_stores <- [ro] ;
      Floating_block_store.init chain_dir ~readonly:false RW >>= fun rw ->
      block_store.rw_floating_block_store <- rw ;
      return_unit)

let check_store_consistency block_store ~cementing_highwatermark =
  Cemented_block_store.get_highest_cemented_level block_store.cemented_store
  |> function
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
   (new_head.lafl - (new_head.lafl).max_op_ttl)). Important: we might
   not have this block so it should be treated as a potential lower
   bound. Furethermore, we consider the current caboose as a potential
   lower bound.*)
let compute_lowest_bound_to_preserve_in_floating block_store ~new_head
    ~new_head_metadata =
  (* Safety check: is the highwatermark consistent with our highest cemented block *)
  let lafl = Block_repr.last_allowed_fork_level new_head_metadata in
  trace
    Missing_last_allowed_fork_level_block
    (read_predecessor_block_by_level
       block_store
       ~read_metadata:true
       ~head:new_head
       lafl)
  >>=? fun lafl_block ->
  return
    (Int32.sub
       lafl
       (Int32.of_int
          (match Block_repr.metadata lafl_block with
          | None ->
              (* FIXME: this is not valid but it is a good
                 approximation of the max_op_ttl of a block where the
                 metadata is missing. *)
              Block_repr.max_operations_ttl new_head_metadata
          | Some metadata -> Block_repr.max_operations_ttl metadata)))

let instanciate_temporary_floating_store block_store =
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
         Floating_block_store.init block_store.chain_dir ~readonly:false RW_TMP
         >>= fun new_rw_store ->
         block_store.rw_floating_block_store <- new_rw_store ;
         return (ro_store, rw_store, new_rw_store)))

let create_merging_thread block_store ~history_mode ~old_ro_store ~old_rw_store
    ~new_head ~new_head_lafl ~lowest_bound_to_preserve_in_floating
    ~cementing_highwatermark =
  Floating_block_store.init block_store.chain_dir ~readonly:false RO_TMP
  >>= fun new_ro_store ->
  Lwt.catch
    (fun () ->
      update_floating_stores
        block_store
        ~history_mode
        ~ro_store:old_ro_store
        ~rw_store:old_rw_store
        ~new_store:new_ro_store
        ~new_head
        ~new_head_lafl
        ~lowest_bound_to_preserve_in_floating
        ~cementing_highwatermark
      >>=? fun (cycles_interval_to_cement, new_savepoint, new_caboose) ->
      let cycle_reader =
        read_block_range_in_floating_stores
          block_store
          ~ro_store:old_ro_store
          ~rw_store:old_rw_store
          ~head:new_head
      in
      (match history_mode with
      | History_mode.Archive ->
          List.iter_es
            (fun cycle_range ->
              cycle_reader cycle_range >>=? fun cycle ->
              (* In archive, we store the metadatas *)
              cement_blocks ~write_metadata:true block_store cycle)
            cycles_interval_to_cement
      | Rolling offset ->
          let offset =
            (Option.value
               offset
               ~default:History_mode.default_additional_cycles)
              .offset
          in
          if offset > 0 then
            (* Only cement <offset> cycles *)
            let cycles_interval_to_cement =
              List.remove
                (List.length cycles_interval_to_cement - offset)
                cycles_interval_to_cement
            in
            List.iter_es
              (fun cycle_range ->
                cycle_reader cycle_range >>=? fun cycle ->
                cement_blocks ~write_metadata:true block_store cycle)
              cycles_interval_to_cement
            >>=? fun () ->
            (* Clean-up the files that are below the offset *)
            Cemented_block_store.trigger_gc
              block_store.cemented_store
              history_mode
            >>= fun () -> return_unit
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
            List.iter_es
              (fun cycle_range ->
                cycle_reader cycle_range >>=? fun cycle ->
                cement_blocks ~write_metadata:true block_store cycle)
              cycles_interval_to_cement
            >>=? fun () ->
            (* Clean-up the files that are below the offset *)
            Cemented_block_store.trigger_gc
              block_store.cemented_store
              history_mode
            >>= fun () -> return_unit
          else
            List.iter_es
              (fun cycle_range ->
                cycle_reader cycle_range >>=? fun cycle ->
                (* In full 0, we do not store the metadata *)
                cement_blocks ~write_metadata:false block_store cycle)
              cycles_interval_to_cement)
      >>=? fun () -> return (new_savepoint, new_caboose))
    (fun exn ->
      Floating_block_store.close new_ro_store >>= fun () -> Lwt.fail exn)
  >>=? fun (new_savepoint, new_caboose) ->
  return (new_ro_store, new_savepoint, new_caboose)

let merge_stores block_store ~(on_error : tztrace -> unit tzresult Lwt.t)
    ~finalizer ~history_mode ~new_head ~new_head_metadata
    ~cementing_highwatermark =
  fail_when block_store.readonly Cannot_write_in_readonly >>=? fun () ->
  (* Do not allow multiple merges: force waiting for a potential
     previous merge. *)
  Lwt_mutex.lock block_store.merge_mutex >>= fun () ->
  protect
    ~on_error:(fun err ->
      Lwt_mutex.unlock block_store.merge_mutex ;
      Lwt.return (Error err))
    (fun () ->
      status block_store >>= fun store_status ->
      fail_unless
        (store_status = Idle)
        (Cannot_merge_store {status = status_to_string store_status})
      >>=? fun () ->
      (* Mark the store's status as Merging *)
      write_status block_store Merging >>=? fun () ->
      let new_head_lafl =
        Block_repr.last_allowed_fork_level new_head_metadata
      in
      Store_events.(emit start_merging_stores) new_head_lafl >>= fun () ->
      check_store_consistency block_store ~cementing_highwatermark
      >>=? fun () ->
      compute_lowest_bound_to_preserve_in_floating
        block_store
        ~new_head
        ~new_head_metadata
      >>=? fun lowest_bound_to_preserve_in_floating ->
      let merge_start = Systime_os.now () in
      Lwt_idle_waiter.force_idle block_store.merge_scheduler (fun () ->
          (* Move the rw in the ro stores and create a new tmp *)
          instanciate_temporary_floating_store block_store
          >>=? fun (old_ro_store, old_rw_store, _new_rw_store) ->
          (* Important: do not clean-up the temporary stores on
             failures as they will delete the recently arrived
             blocks. *)
          (* Create the merging thread that we want to run in background *)
          (* Clean-up on cancel/exn *)
          let merging_thread : unit tzresult Lwt.t =
            Lwt.finalize
              (fun () ->
                protect
                  ~on_error:(fun err ->
                    (* Failures should be handled using [get_merge_status] *)
                    let msg = Format.asprintf "%a" pp_print_trace err in
                    Store_events.(emit merge_error)
                      (cementing_highwatermark, new_head_lafl, msg)
                    >>= fun () -> on_error (Merge_error :: err))
                  (fun () ->
                    create_merging_thread
                      block_store
                      ~history_mode
                      ~old_ro_store
                      ~old_rw_store
                      ~new_head
                      ~new_head_lafl
                      ~lowest_bound_to_preserve_in_floating
                      ~cementing_highwatermark
                    >>=? fun (new_ro_store, new_savepoint, new_caboose) ->
                    Lwt_idle_waiter.force_idle
                      block_store.merge_scheduler
                      (fun () ->
                        (* Critical section: update on-disk values *)
                        move_all_floating_stores block_store ~new_ro_store
                        >>=? fun () ->
                        write_caboose block_store new_caboose >>=? fun () ->
                        write_savepoint block_store new_savepoint >>=? fun () ->
                        return_unit)
                    >>=? fun () ->
                    (* Don't call the finalizer in the critical
                       section, in case it needs to access the block
                       store. *)
                    finalizer new_head_lafl >>=? fun () ->
                    (* The merge operation succeeded, the store is now idle. *)
                    block_store.merging_thread <- None ;
                    write_status block_store Idle >>=? fun () -> return_unit))
              (fun () ->
                Lwt_mutex.unlock block_store.merge_mutex ;
                Lwt.return_unit)
            >>=? fun () ->
            let merge_end = Systime_os.now () in
            let merging_time = Ptime.diff merge_end merge_start in
            Store_events.(emit end_merging_stores) merging_time >>= fun () ->
            return_unit
          in
          block_store.merging_thread <- Some (new_head_lafl, merging_thread) ;
          (* Temporary stores in place and the merging thread was
              started: we can now release the hard-lock. *)
          return_unit)
      >>=? fun () -> return_unit)

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
  let chain_dir = block_store.chain_dir in
  Lwt_list.iter_s
    Floating_block_store.close
    (block_store.rw_floating_block_store :: block_store.ro_floating_block_stores)
  >>= fun () ->
  (* Remove RO_TMP if it still exists *)
  let ro_tmp_floating_store_dir_path =
    Naming.floating_blocks_dir chain_dir RO_TMP |> Naming.dir_path
  in
  Lwt_utils_unix.remove_dir ro_tmp_floating_store_dir_path >>= fun () ->
  (* If RW_TMP exists, merge RW and RW_TMP into one new
     single floating_store RW_RESTORE then swap it with
     the previous one. *)
  Floating_block_store.init chain_dir ~readonly:false (Restore RW)
  >>= fun rw_restore ->
  Lwt.finalize
    (fun () ->
      Floating_block_store.init chain_dir ~readonly:true RW >>= fun rw ->
      Floating_block_store.init chain_dir ~readonly:true RW_TMP
      >>= fun rw_tmp ->
      Floating_block_store.append_floating_store ~from:rw ~into:rw_restore
      >>=? fun () ->
      Floating_block_store.append_floating_store ~from:rw_tmp ~into:rw_restore
      >>=? fun () ->
      Floating_block_store.swap ~src:rw_restore ~dst:rw >>= fun () ->
      Floating_block_store.delete_files rw_tmp >>= fun () -> return_unit)
    (fun () -> Floating_block_store.delete_files rw_restore)
  >>=? fun () ->
  (* Re-instantiate RO and RW *)
  Floating_block_store.init chain_dir ~readonly:false RO >>= fun ro ->
  Floating_block_store.init chain_dir ~readonly:false RW >>= fun rw ->
  block_store.ro_floating_block_stores <- [ro] ;
  block_store.rw_floating_block_store <- rw ;
  write_status block_store Idle

(* Removes the potentially leftover temporary files from the cementing
   of cycles. *)
let may_clean_cementing_artifacts block_store =
  let chain_dir = block_store.chain_dir in
  let cemented_path = Naming.cemented_blocks_dir chain_dir |> Naming.dir_path in
  let rec loop dir =
    Lwt_unix.readdir dir >>= function
    | s when Filename.extension s = ".tmp" ->
        Lwt_unix.unlink (Filename.concat cemented_path s) >>= fun () -> loop dir
    | _ -> loop dir
  in
  Lwt_unix.file_exists cemented_path >>= function
  | true ->
      Lwt_unix.opendir cemented_path >>= fun dir ->
      Lwt.catch
        (fun () ->
          Lwt.finalize (fun () -> loop dir) (fun () -> Lwt_unix.closedir dir))
        (function End_of_file -> Lwt.return_unit | err -> Lwt.fail err)
  | false -> Lwt.return_unit

let may_recover_merge block_store =
  fail_when block_store.readonly Cannot_write_in_readonly >>=? fun () ->
  Lwt_idle_waiter.force_idle block_store.merge_scheduler (fun () ->
      Lwt_mutex.with_lock block_store.merge_mutex (fun () ->
          Stored_data.get block_store.status_data >>= function
          | Idle -> return_unit
          | Merging ->
              Store_events.(emit recover_merge ()) >>= fun () ->
              merge_temporary_floating block_store))
  >>=? fun () ->
  (* Try to clean temporary file anyway. *)
  may_clean_cementing_artifacts block_store >>= return

let load ?block_cache_limit chain_dir ~genesis_block ~readonly =
  Cemented_block_store.init chain_dir ~readonly >>=? fun cemented_store ->
  Floating_block_store.init chain_dir ~readonly RO
  >>= fun ro_floating_block_store ->
  let ro_floating_block_stores = [ro_floating_block_store] in
  Floating_block_store.init chain_dir ~readonly RW
  >>= fun rw_floating_block_store ->
  let genesis_descr = Block_repr.descriptor genesis_block in
  Stored_data.init (Naming.savepoint_file chain_dir) ~initial_data:genesis_descr
  >>=? fun savepoint ->
  Stored_data.get savepoint >>= fun (_, savepoint_level) ->
  Prometheus.Gauge.set
    Store_metrics.metrics.savepoint_level
    (Int32.to_float savepoint_level) ;
  Stored_data.init (Naming.caboose_file chain_dir) ~initial_data:genesis_descr
  >>=? fun caboose ->
  Stored_data.get caboose >>= fun (_, caboose_level) ->
  Prometheus.Gauge.set
    Store_metrics.metrics.caboose_level
    (Int32.to_float caboose_level) ;
  Stored_data.init (Naming.block_store_status_file chain_dir) ~initial_data:Idle
  >>=? fun status_data ->
  let block_cache =
    Block_lru_cache.create
      (Option.value block_cache_limit ~default:default_block_cache_limit)
  in
  let merge_scheduler = Lwt_idle_waiter.create () in
  let merge_mutex = Lwt_mutex.create () in
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
      merge_mutex;
      merge_scheduler;
      merging_thread = None;
    }
  in
  (if not readonly then may_recover_merge block_store else return_unit)
  >>=? fun () ->
  Stored_data.get status_data >>= fun status ->
  fail_unless (status = Idle) Cannot_load_degraded_store >>=? fun () ->
  return block_store

let create ?block_cache_limit chain_dir ~genesis_block =
  load chain_dir ?block_cache_limit ~genesis_block ~readonly:false
  >>=? fun block_store ->
  store_block block_store genesis_block >>=? fun () -> return block_store

let pp_merge_status fmt status =
  match status with
  | Not_running -> Format.fprintf fmt "not running"
  | Running -> Format.fprintf fmt "running"
  | Merge_failed err -> Format.fprintf fmt "merge failed %a" pp_print_trace err

let await_merging block_store =
  Lwt_mutex.lock block_store.merge_mutex >>= fun () ->
  let thread = block_store.merging_thread in
  Lwt_mutex.unlock block_store.merge_mutex ;
  match thread with
  | None -> Lwt.return_unit
  | Some (_, th) -> th >>= fun _ -> Lwt.return_unit

let close block_store =
  (* Wait a bit for the merging to end but hard-stop it if it takes
     too long. *)
  (match get_merge_status block_store with
  | Not_running | Merge_failed _ -> Lwt.return_unit
  | Running ->
      Store_events.(emit try_waiting_for_merge_termination) () >>= fun () ->
      Lwt_unix.with_timeout 5. (fun () ->
          await_merging block_store >>= fun () -> Lwt.return_unit))
  >>= fun () ->
  Cemented_block_store.close block_store.cemented_store ;
  Lwt_list.iter_s
    Floating_block_store.close
    (block_store.rw_floating_block_store :: block_store.ro_floating_block_stores)
