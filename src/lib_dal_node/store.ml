(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Profiler = (val Profiler.wrap Dal_profiler.dal_profiler)

(** FIFO-keyed map with slot_id as keys. *)
module Slot_map =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (Types.Slot_id)

module Version = struct
  type t = int

  let make v = v

  let equal = Int.equal

  let pp = Format.pp_print_int

  let encoding =
    let open Data_encoding in
    obj1 (req "version" int31)

  let version_file_name = "version.json"

  let version_file_path ~base_dir = Filename.concat base_dir version_file_name

  (* Version history:
     - 0: came with octez release v20; used Irmin for storing slots
     - 1: removed Irmin dependency; added slot and status stores; changed layout of shard
       store by indexing on slot ids instead of commitments
     - 2: switch the KVS skip list store for a sqlite3 one.
     - 3: remove status store, keep cache. *)
  let current_version = 3

  type error += Could_not_read_data_dir_version of string

  type error += Could_not_write_version_file of string

  type error += Invalid_data_dir_version of {actual : t; expected : t}

  let () =
    register_error_kind
      `Permanent
      ~id:"dal.store.could_not_read_data_dir_version"
      ~title:"Could not read data directory version file"
      ~description:"Data directory version file is absent or invalid."
      Data_encoding.(obj1 (req "version_path" string))
      ~pp:(fun ppf path ->
        Format.fprintf
          ppf
          "Tried to read version file at '%s', but the file could not be found \
           or parsed."
          path)
      (function Could_not_read_data_dir_version path -> Some path | _ -> None)
      (fun path -> Could_not_read_data_dir_version path) ;
    register_error_kind
      `Permanent
      ~id:"dal.store.could_not_write_version_file"
      ~title:"Could not write version file"
      ~description:"Version file cannot be written."
      Data_encoding.(obj1 (req "file_path" string))
      ~pp:(fun ppf file_path ->
        Format.fprintf
          ppf
          "Tried to write version file at '%s', but the file could not be \
           written."
          file_path)
      (function
        | Could_not_write_version_file file_path -> Some file_path | _ -> None)
      (fun file_path -> Could_not_write_version_file file_path) ;
    register_error_kind
      `Permanent
      ~id:"dal.store.invalid_version"
      ~title:"Invalid store version"
      ~description:"The store's version was not the one that was expected"
      ~pp:(fun ppf (actual, expected) ->
        Format.fprintf
          ppf
          "Invalid store version '%a' (expected '%a'). Your store is %s"
          pp
          actual
          pp
          expected
          (if actual < expected then
             "incompatible and cannot be automatically upgraded."
           else "too recent for this version of the DAL node's store."))
      Data_encoding.(
        obj2 (req "actual_version" encoding) (req "expected_version" encoding))
      (function
        | Invalid_data_dir_version {actual; expected} -> Some (actual, expected)
        | _ -> None)
      (fun (actual, expected) -> Invalid_data_dir_version {actual; expected})

  let read_version_file ~file_path =
    let open Lwt_result_syntax in
    let* json =
      trace
        (Could_not_read_data_dir_version file_path)
        (Lwt_utils_unix.Json.read_file file_path)
    in
    Lwt.catch
      (fun () -> Data_encoding.Json.destruct encoding json |> return)
      (fun _ -> tzfail (Could_not_read_data_dir_version file_path))

  let write_version_file ~version ~base_dir =
    let version_file = version_file_path ~base_dir in
    Lwt_utils_unix.Json.write_file
      version_file
      (Data_encoding.Json.construct encoding version)
    |> trace (Could_not_write_version_file version_file)
end

module KVS = Key_value_store

module Stores_dirs = struct
  let shard = "shard_store"

  let slot = "slot_store"

  let skip_list_cells = "skip_list_store"
end

module Shards_disk = struct
  type error +=
    | Invalid_min_shards_to_reconstruct_slot of {given : int}
    | NoShardLayout of {level : int32}

  let () =
    register_error_kind
      `Permanent
      ~id:"dal.shards.invalid_min_shards_to_reconstruct_slot"
      ~title:"Invalid minimum shards to reconstruct a slot"
      ~description:
        "The minimum number of shards required to reconstruct a slot must be \
         strictly greater than 0."
      ~pp:(fun ppf given ->
        Format.fprintf
          ppf
          "Invalid minimum shards to reconstruct a slot: %d (must be > 0)."
          given)
      Data_encoding.(obj1 (req "given" int31))
      (function
        | Invalid_min_shards_to_reconstruct_slot {given} -> Some given
        | _ -> None)
      (fun given -> Invalid_min_shards_to_reconstruct_slot {given}) ;
    register_error_kind
      `Permanent
      ~id:"dal.shards.NoShardLayout"
      ~title:"No shard layout"
      ~description:"No shard layout found for the given slot level"
      ~pp:(fun ppf level ->
        Format.fprintf ppf "No shard layout found for slot at level %ld" level)
      Data_encoding.(obj1 (req "level" int32))
      (function NoShardLayout {level} -> Some level | _ -> None)
      (fun level -> NoShardLayout {level})

  type t = {
    shards_store : (Types.slot_id, int, Cryptobox.share) KVS.t;
    min_shards_to_reconstruct_slot : int;
        (* Minimum number of distinct shards that must be persisted per slot in
           order to be able to reconstruct the whole slot (k in a k-of-n
           erasure-coding scheme). This value is derived from the DAL cryptobox
           parameters. It must always be > 0. *)
  }

  module ShardsLayouts = Map.Make (struct
    type t = Int32.t

    let compare = compare
  end)

  let shards_layouts = ref ShardsLayouts.empty

  let get_file_layout ~(slot_id : Types.slot_id) =
    let e =
      ShardsLayouts.find_last
        (fun first_level -> slot_id.slot_level >= first_level)
        !shards_layouts
    in
    match e with
    | Some (_, v) -> Lwt_result_syntax.return v
    | None ->
        Lwt_result_syntax.tzfail (NoShardLayout {level = slot_id.slot_level})

  let make_file_layout cryptobox =
    let share_size = Cryptobox.encoded_share_size cryptobox in
    let params = Cryptobox.parameters cryptobox in
    let number_of_shards = params.number_of_shards in
    fun ~root_dir (slot_id : Types.slot_id) ->
      let slot_id_string =
        Format.asprintf "%ld_%d" slot_id.slot_level slot_id.slot_index
      in
      let filepath = Filename.concat root_dir slot_id_string in
      Key_value_store.layout
        ~encoded_value_size:share_size
        ~encoding:Cryptobox.share_encoding
        ~filepath
        ~eq:Stdlib.( = )
        ~index_of:Fun.id
        ~number_of_keys_per_file:number_of_shards
        ()

  let add_file_layout level cryptobox_parameters =
    let open Result_syntax in
    let* cryptobox = Cryptobox.make cryptobox_parameters in
    let layout = make_file_layout cryptobox in
    shards_layouts := ShardsLayouts.add level layout !shards_layouts ;
    return_unit

  let with_metrics store f =
    let open Lwt_result_syntax in
    let* r = f () in
    let opened_files = KVS.View.opened_files store in
    let ongoing_actions = KVS.View.ongoing_actions store in
    Dal_metrics.update_kvs_shards_metrics ~opened_files ~ongoing_actions ;
    return r

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4973
     Make storage more resilient to DAL parameters change. *)
  let number_of_shards_available {shards_store = store; _} slot_id shard_indexes
      =
    let open Lwt_result_syntax in
    let*! fl = get_file_layout ~slot_id in
    match fl with
    | Ok file_layout ->
        List.fold_left_es
          (fun count shard_index ->
            let+ exists =
              KVS.value_exists store file_layout slot_id shard_index
            in
            if exists then count + 1 else count)
          0
          shard_indexes
    | Error [NoShardLayout _] ->
        (* There is no file layout available to read the data, we assume no
           shard are available. *)
        return 0
    | Error _ as e -> Lwt.return e

  (* Persist shards for [slot_id], but never more than
     [min_shards_to_reconstruct_slot]. *)
  let write_all {shards_store; min_shards_to_reconstruct_slot} slot_id shards =
    let open Lwt_result_syntax in
    let* file_layout = Errors.other_lwt_result @@ get_file_layout ~slot_id in
    (* Invariant of init below: [min_shards_to_reconstruct_slot > 0] *)
    (* Get how many shards are already persisted. *)
    let* already_stored =
      KVS.count_values shards_store file_layout slot_id
      |> Errors.other_lwt_result
    in
    (* Compute how many shards need to be persisted to reach threshold. *)
    let remaining = min_shards_to_reconstruct_slot - already_stored in
    let rec loop remaining s =
      if remaining <= 0 then
        (* Target reached. *)
        return_unit
      else
        match s () with
        | Seq.Nil -> return_unit
        | Seq.Cons ({Cryptobox.index; share}, tl) ->
            let* exists =
              (KVS.value_exists
                 shards_store
                 file_layout
                 slot_id
                 index
               [@profiler.aggregate_s {verbosity = Notice} "value_exists"])
            in
            if exists then
              (* Do not decrement remaining on duplicates; continue scanning. *)
              loop remaining tl
            else
              let* () =
                (KVS.write_value
                   shards_store
                   file_layout
                   slot_id
                   index
                   share
                 [@profiler.aggregate_s {verbosity = Notice} "write_value"])
              in
              (* Metrics/events only for actually persisted shards. *)
              let () = Dal_metrics.shard_stored () in
              let*! () =
                Event.emit_stored_slot_shard
                  ~published_level:slot_id.slot_level
                  ~slot_index:slot_id.slot_index
                  ~shard_index:index
              in
              loop (remaining - 1) tl
    in
    if remaining <= 0 then
      (* To avoid triggering metrics in this case. *)
      return_unit
    else
      let* () =
        with_metrics shards_store @@ fun () ->
        loop remaining shards |> Errors.other_lwt_result
      in
      return_unit

  let read_all ?from_bytes {shards_store; _} slot_id ~number_of_shards =
    let open Lwt_result_syntax in
    let* file_layout = get_file_layout ~slot_id in
    let reader =
      match from_bytes with
      | None -> KVS.read_values shards_store file_layout
      | Some bytes -> KVS.read_values_from_bytes file_layout bytes
    in
    let v =
      Seq.ints 0
      |> Seq.take_while (fun x -> x < number_of_shards)
      |> Seq.map (fun shard_index -> (slot_id, shard_index))
      |> reader
    in
    return (fun () -> v ())

  let read {shards_store = store; _} slot_id shard_id =
    let open Lwt_result_syntax in
    let* file_layout = Errors.other_lwt_result @@ get_file_layout ~slot_id in
    let*! res =
      with_metrics store @@ fun () ->
      KVS.read_value store file_layout slot_id shard_id
    in
    match res with
    | Ok share -> return {Cryptobox.share; index = shard_id}
    | Error [KVS.Missing_stored_kvs_data _] | Error [NoShardLayout _] ->
        fail Errors.not_found
    | Error err ->
        let data_kind = Types.Store.Shard in
        fail @@ Errors.decoding_failed data_kind err

  let count_values {shards_store = store; _} slot_id =
    let open Lwt_result_syntax in
    let* file_layout = get_file_layout ~slot_id in
    with_metrics store @@ fun () -> KVS.count_values store file_layout slot_id

  let remove {shards_store = store; _} slot_id =
    let open Lwt_result_syntax in
    let* file_layout = get_file_layout ~slot_id in
    let* () =
      with_metrics store @@ fun () -> KVS.remove_file store file_layout slot_id
    in
    return_unit

  let init node_store_dir shard_store_dir ~min_shards_to_reconstruct_slot
      ~lru_size =
    let open Lwt_result_syntax in
    if min_shards_to_reconstruct_slot <= 0 then
      tzfail
        (Invalid_min_shards_to_reconstruct_slot
           {given = min_shards_to_reconstruct_slot})
    else
      let root_dir = Filename.concat node_store_dir shard_store_dir in
      let* shards_store = KVS.init ~lru_size ~root_dir in
      return {shards_store; min_shards_to_reconstruct_slot}
end

module Shards_cache = struct
  module Int_map = Map.Make (Int)

  type t = Cryptobox.share Int_map.t Slot_map.t

  let init = Slot_map.create

  let has_slot cache slot_id = Option.is_some (Slot_map.find_opt cache slot_id)

  let number_of_shards_available cache slot_id shard_indexes =
    Lwt_result_syntax.return
    @@
    match Slot_map.find_opt cache slot_id with
    | None -> 0
    | Some shards ->
        List.fold_left
          (fun count shard_index ->
            if Int_map.mem shard_index shards then count + 1 else count)
          0
          shard_indexes

  let write_all cache slot_id shards =
    let open Lwt_result_syntax in
    let cached_shards =
      match
        Slot_map.find_opt
          cache
          slot_id [@profiler.aggregate_f {verbosity = Notice} "find_opt"]
      with
      | None -> Int_map.empty
      | Some shards -> shards
    in
    (let* new_shards =
       Seq.ES.fold_left
         (fun shards_map {Cryptobox.index; share} ->
           if Int_map.mem index shards_map then return shards_map
           else
             let shards_map =
               (Int_map.add
                  index
                  share
                  shards_map
                [@profiler.aggregate_f {verbosity = Notice} "add shard"])
             in
             let*! () =
               Event.emit_cached_slot_shard
                 ~published_level:slot_id.slot_level
                 ~slot_index:slot_id.slot_index
                 ~shard_index:index
             in
             return shards_map)
         cached_shards
         shards
     in
     Slot_map.replace cache slot_id new_shards ;
     return_unit)
    |> Errors.other_lwt_result

  let read_all cache slot_id =
    (match Slot_map.find_opt cache slot_id with
    | None -> Seq.empty
    | Some shards ->
        Int_map.bindings shards |> List.to_seq
        |> Seq.map (fun (i, share) -> (slot_id, i, Ok share)))
    |> Seq_s.of_seq

  let read cache slot_id shard_id =
    let open Lwt_result_syntax in
    match Slot_map.find_opt cache slot_id with
    | Some shards -> (
        match Int_map.find_opt shard_id shards with
        | Some share -> return {Cryptobox.share; index = shard_id}
        | None -> fail Errors.not_found)
    | None -> fail Errors.not_found

  let count_values cache slot_id =
    Lwt_result_syntax.return
    @@
    match Slot_map.find_opt cache slot_id with
    | None -> 0
    | Some shards -> Int_map.cardinal shards

  let remove cache slot_id =
    Lwt_result_syntax.return @@ Slot_map.remove cache slot_id
end

(*
  Shards -- cache-first selector via [source_target]

  Policy:
  - Reads/counts: If the slot is present in the memory cache, use the cache,
  otherwise use the disk (if available, otherwise the data is missing).
  - Writes:
      * cache-only (disk=None): write to the memory cache.
      * disk-backed: write to memory cache (allowed to fail), and then, write to
        disk (must succeed).
  - Removal always clears cache first, then disk if present.
*)
module Shards = struct
  module Disk = Shards_disk
  module Cache = Shards_cache

  type t = {disk : Disk.t option; cache : Cache.t}

  let init ~profile_ctxt ~proto_parameters ~lru_size node_store_dir
      shard_store_dir =
    let open Lwt_result_syntax in
    let* disk =
      if Profile_manager.is_attester_only_profile profile_ctxt then return_none
      else
        let min_shards_to_reconstruct_slot =
          let cryptobox = proto_parameters.Types.cryptobox_parameters in
          (* The minimum number of shards required to reconstruct a slot (k in
             k-of-n). *)
          cryptobox.number_of_shards / cryptobox.redundancy_factor
        in
        let* store =
          Disk.init
            node_store_dir
            shard_store_dir
            ~min_shards_to_reconstruct_slot
            ~lru_size
        in
        return_some store
    in
    let cache_size =
      Profile_manager.get_memory_cache_size profile_ctxt proto_parameters
    in
    let cache = Cache.init cache_size in
    return {disk; cache}

  (* Select the backend to use for reads / counts. *)
  let source_target {disk; cache} slot_id =
    if Cache.has_slot cache slot_id then `Cache cache
    else match disk with Some d -> `Disk d | None -> `Cache cache

  let number_of_shards_available st slot_id shard_indexes =
    match source_target st slot_id with
    | `Cache c -> Cache.number_of_shards_available c slot_id shard_indexes
    | `Disk d -> Disk.number_of_shards_available d slot_id shard_indexes

  let write_all {disk; cache} slot_id shards =
    let open Lwt_result_syntax in
    match disk with
    | None -> Cache.write_all cache slot_id shards
    | Some d ->
        (* Write to memory cache, making the value available first, and ignore
           failure. *)
        let*! (_ : (unit, _) result) = Cache.write_all cache slot_id shards in
        (* Disk must succeed for persistence. *)
        let* () = Disk.write_all d slot_id shards in
        return_unit

  let read st slot_id shard_id =
    match source_target st slot_id with
    | `Cache c -> Cache.read c slot_id shard_id
    | `Disk d -> Disk.read d slot_id shard_id

  let read_all ?from_bytes st slot_id ~number_of_shards =
    let open Lwt_result_syntax in
    match source_target st slot_id with
    | `Cache c -> Cache.read_all c slot_id |> return
    | `Disk d ->
        let* v = Disk.read_all ?from_bytes d slot_id ~number_of_shards in
        return (fun () -> v ())

  let count_values st slot_id =
    match source_target st slot_id with
    | `Cache c -> Cache.count_values c slot_id
    | `Disk d -> Disk.count_values d slot_id

  let remove {disk; cache} slot_id =
    let open Lwt_result_syntax in
    let* () = Cache.remove cache slot_id in
    match disk with Some d -> Disk.remove d slot_id | None -> return_unit
end

module Slots = struct
  type error += NoSlotLayout of {level : int32}

  let () =
    register_error_kind
      `Permanent
      ~id:"dal.shards.NoSlotLayout"
      ~title:"No slot layout"
      ~description:"No slot layout found for the given slot level"
      ~pp:(fun ppf level ->
        Format.fprintf ppf "No slot layout found for slot at level %ld" level)
      Data_encoding.(obj1 (req "level" int32))
      (function NoSlotLayout {level} -> Some level | _ -> None)
      (fun level -> NoSlotLayout {level})

  type t = (Types.slot_id, unit, bytes) KVS.t

  module SlotsLayouts = Map.Make (struct
    type t = Int32.t

    let compare = compare
  end)

  let slots_layouts = ref SlotsLayouts.empty

  let get_file_layout ~(slot_id : Types.slot_id) =
    let e =
      SlotsLayouts.find_last
        (fun first_level -> slot_id.slot_level >= first_level)
        !slots_layouts
    in
    match e with
    | Some (_, v) ->
        Lwt_result_syntax.return (fun ~root_dir slot_id -> v ~root_dir slot_id)
    | None ->
        Lwt_result_syntax.tzfail (NoSlotLayout {level = slot_id.slot_level})

  let make_file_layout {Cryptobox.slot_size; _} =
   fun ~root_dir (slot_id : Types.slot_id) ->
    let number_of_slots = 1 in
    let slot_id_string =
      Format.asprintf "%ld_%d" slot_id.slot_level slot_id.slot_index
    in
    let filename = Format.sprintf "%s_%d" slot_id_string slot_size in
    let filepath = Filename.concat root_dir filename in
    Key_value_store.layout
      ~encoding:(Data_encoding.Fixed.bytes slot_size)
      ~filepath
      ~eq:Stdlib.( = )
      ~index_of:(fun () -> 0)
      ~number_of_keys_per_file:number_of_slots
      ()

  let add_file_layout level cryptobox =
    let layout = make_file_layout cryptobox in
    slots_layouts := SlotsLayouts.add level layout !slots_layouts

  let init node_store_dir slot_store_dir ~lru_size =
    let root_dir = Filename.concat node_store_dir slot_store_dir in
    KVS.init ~lru_size ~root_dir

  let add_slot t slot (slot_id : Types.slot_id) =
    let open Lwt_result_syntax in
    let* file_layout = Errors.other_lwt_result @@ get_file_layout ~slot_id in
    let* () =
      KVS.write_value ~override:true t file_layout slot_id () slot
      |> Errors.other_lwt_result
    in
    let*! () =
      Event.emit_stored_slot_content
        ~published_level:slot_id.slot_level
        ~slot_index:slot_id.slot_index
    in
    return_unit

  let find_slot t slot_id =
    let open Lwt_result_syntax in
    let* file_layout = Errors.other_lwt_result @@ get_file_layout ~slot_id in
    let*! res = KVS.read_value t file_layout slot_id () in
    match res with
    | Ok slot -> return slot
    | Error [KVS.Missing_stored_kvs_data _] | Error [NoSlotLayout _] ->
        fail Errors.not_found
    | Error err ->
        let data_kind = Types.Store.Slot in
        fail @@ Errors.decoding_failed data_kind err

  let remove_slot t slot_id =
    let open Lwt_result_syntax in
    let* file_layout = get_file_layout ~slot_id in
    KVS.remove_file t file_layout slot_id
end

module Slot_id_cache = struct
  module Levels =
    Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
      (struct
        type t = Types.level

        let equal = Int32.equal

        let hash = Hashtbl.hash
      end)

  type t = Cryptobox.Commitment.t option array Levels.t

  let create ~capacity = Levels.create capacity

  let add ~number_of_slots t slot_header =
    let Dal_plugin.{slot_index; commitment; published_level} = slot_header in
    match Levels.find_opt t published_level with
    | None ->
        let table = Array.make number_of_slots None in
        Array.set table slot_index (Some commitment) ;
        Levels.replace t published_level table
    | Some table -> Array.set table slot_index (Some commitment)

  let find_opt =
    let get_opt a i =
      let len = Array.length a in
      if i < 0 || i >= len then None else Array.get a i
    in
    fun t Types.Slot_id.{slot_level; slot_index} ->
      Levels.find_opt t slot_level
      |> Option.filter_map (Fun.flip get_opt slot_index)
end

module Traps = struct
  module Level_map =
    Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
      (struct
        type t = Types.level

        let equal = Int32.equal

        let hash = Hashtbl.hash
      end)

  module Slot_index_map = Map.Make (Int)
  module Shard_index_map = Map.Make (Int)

  type payload =
    Signature.Public_key_hash.t * Cryptobox.share * Cryptobox.shard_proof

  type t = payload Shard_index_map.t Slot_index_map.t Level_map.t

  let create ~capacity = Level_map.create capacity

  let add_slot_index t ~slot_index ~shard_index ~delegate ~share ~shard_proof =
    let shard_index_map_opt = Slot_index_map.find_opt slot_index t in
    let shard_index_map =
      Option.value ~default:Shard_index_map.empty shard_index_map_opt
    in
    let new_shard_index_map =
      Shard_index_map.add
        shard_index
        (delegate, share, shard_proof)
        shard_index_map
    in
    Slot_index_map.add slot_index new_shard_index_map t

  let add t ~slot_id ~shard_index ~delegate ~share ~shard_proof =
    let Types.Slot_id.{slot_level; slot_index} = slot_id in
    let slot_index_map_opt = Level_map.find_opt t slot_level in
    let slot_index_map =
      Option.value ~default:Slot_index_map.empty slot_index_map_opt
    in
    let new_slot_index_map =
      add_slot_index
        slot_index_map
        ~slot_index
        ~shard_index
        ~delegate
        ~share
        ~shard_proof
    in
    Level_map.replace t slot_level new_slot_index_map

  let find t ~level =
    match Level_map.find_opt t level with
    | None -> []
    | Some m ->
        Slot_index_map.fold
          (fun slot_index m acc ->
            let res =
              List.map
                (fun (shard_index, (delegate, share, shard_proof)) ->
                  Types.
                    {
                      delegate;
                      slot_index;
                      shard = Cryptobox.{index = shard_index; share};
                      shard_proof;
                    })
                (Shard_index_map.bindings m)
            in
            res @ acc)
          m
          []
end

module Statuses_cache = struct
  type t = Types.header_status Slot_map.t

  let init = Slot_map.create

  let add_status t status (slot_id : Types.slot_id) =
    Slot_map.replace t slot_id status

  let get_slot_status = Slot_map.find_opt

  let update_slot_header_status t slot_id to_status =
    let open Result_syntax in
    let* () =
      match to_status with
      | `Waiting_attestation ->
          tzfail
            (Errors.Unexpected_slot_status_transition
               {
                 slot_id;
                 from_status_opt = None;
                 to_status = `Waiting_attestation;
               })
      | _ -> return_unit
    in
    let* () =
      let from_status_opt = get_slot_status t slot_id in
      match from_status_opt with
      | None ->
          add_status t to_status slot_id ;
          return_unit
      | Some from_status -> (
          let fail_transition () =
            tzfail
              (Errors.Unexpected_slot_status_transition
                 {slot_id; from_status_opt; to_status})
          in
          match from_status with
          | `Unpublished -> (
              match to_status with
              | `Unpublished ->
                  (* No-op: status already Unpublished *)
                  return_unit
              | `Attested _ | `Unattested | `Waiting_attestation ->
                  fail_transition ())
          | `Waiting_attestation -> (
              match to_status with
              | `Attested _lag ->
                  add_status t to_status slot_id ;
                  return_unit
              | `Unattested ->
                  add_status t to_status slot_id ;
                  return_unit
              | `Unpublished -> fail_transition ()
              | `Waiting_attestation -> return_unit (* No-op, already waiting *)
              )
          | `Attested _lag ->
              (* If a status was already inserted, then its value was either
                 [Unpublished] or [Waiting_attestation]. *)
              fail_transition ()
          | `Unattested -> fail_transition ())
    in
    (match to_status with
    | `Attested lag ->
        let slot_index = slot_id.slot_index in
        Dal_metrics.slot_attested ~set:true slot_index ;
        Dal_metrics.slot_attested_with_lag ~lag ~slot_index
    | `Unattested ->
        (* per the invariant stated above, the function can only be called once
           per slot_id with the `Unattested value *)
        Dal_metrics.slot_unattested slot_id.slot_index ;
        (* TODO: is the right way to update the metric here?? *)
        Dal_metrics.slot_attested ~set:false slot_id.slot_index
    | `Unpublished | `Waiting_attestation ->
        Dal_metrics.slot_attested ~set:false slot_id.slot_index) ;
    return_unit
end

module Commitment_indexed_cache =
  (* The commitment-indexed cache is where slots, shards, and
     shard proofs are kept before being associated to some slot id. The
     policy is not LRU to avoid prioritizing slots when they are accessed
     from the cache to be stored and published on the DAL network. *)
    Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
      (struct
        type t = Cryptobox.Commitment.t

        let equal = Cryptobox.Commitment.equal

        let hash = Hashtbl.hash
      end)

module Chain_id = Single_value_store.Make (struct
  type t = Chain_id.t

  let name = "chain_id"

  let encoding = Chain_id.encoding
end)

module Last_processed_level = Single_value_store.Make (struct
  type t = int32

  let name = "last_processed_level"

  let encoding = Data_encoding.int32
end)

module First_seen_level = Single_value_store.Make (struct
  type t = int32

  let name = "first_seen_level"

  let encoding = Data_encoding.int32
end)

module Storage_backend = struct
  (** The type [kind] represents the available storage backend types.
      [SQLite3] corresponds to the current implementation integrating a
      [Sqlite.t] database into the DAL node for storing skip list
      cells. *)
  type kind = SQLite3

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"sqlite3"
          (Tag 1)
          (constant "sqlite3")
          (function SQLite3 -> Some ())
          (fun () -> SQLite3);
      ]

  include Single_value_store.Make (struct
    type t = kind

    let name = "storage_backend"

    let encoding = encoding
  end)

  type error += Storage_backend_mismatch of {current : kind; specified : kind}

  let pp ppf value =
    let json = Data_encoding.Json.construct encoding value in
    Data_encoding.Json.pp ppf json

  let () =
    register_error_kind
      `Permanent
      ~id:"dal.store.storage_backend.backend_mismatch"
      ~title:"Storage backend mismatch"
      ~description:"Cannot use the specified storage backend."
      Data_encoding.(obj2 (req "current" encoding) (req "specified" encoding))
      ~pp:(fun ppf (current, specified) ->
        Format.fprintf
          ppf
          "Cannot use the specified storage backend %a because the store is \
           already configured to use the %a backend. To use the specified \
           backend, you need to start with an empty store."
          pp
          specified
          pp
          current)
      (function
        | Storage_backend_mismatch {current; specified} ->
            Some (current, specified)
        | _ -> None)
      (fun (current, specified) ->
        Storage_backend_mismatch {current; specified})
end

(** Store context *)
type t = {
  statuses_cache : Statuses_cache.t;
  shards : Shards.t;
  slots : Slots.t;
  traps : Traps.t;
  not_yet_published_cache :
    (Cryptobox.slot * Cryptobox.share array * Cryptobox.shard_proof array)
    Commitment_indexed_cache.t;
      (* Cache of not-yet-published slots, shards, and shard proofs. The length
         of the array is the number of shards per slot *)
  chain_id : Chain_id.rw Chain_id.t;
  finalized_commitments : Slot_id_cache.t;
  last_processed_level : Last_processed_level.rw Last_processed_level.t;
  first_seen_level : First_seen_level.rw First_seen_level.t;
  skip_list_cells_store : Dal_store_sqlite3.Skip_list_cells.t;
}

let not_yet_published_cache {not_yet_published_cache; _} =
  not_yet_published_cache

let chain_id {chain_id; _} = chain_id

let first_seen_level {first_seen_level; _} = first_seen_level

let finalized_commitments {finalized_commitments; _} = finalized_commitments

let last_processed_level {last_processed_level; _} = last_processed_level

let shards {shards; _} = shards

let skip_list_cells t = t.skip_list_cells_store

let statuses_cache {statuses_cache; _} = statuses_cache

let slots {slots; _} = slots

let traps {traps; _} = traps

let init_sqlite_skip_list_cells_store ?(perm = Sqlite.Read_write) data_dir =
  let open Lwt_result_syntax in
  let open Filename.Infix in
  let skip_list_cells_data_dir = data_dir // Stores_dirs.skip_list_cells in
  let*! () =
    (* This occurs when running the command:
       ./octez-dal-node debug print store schemas *)
    if not (Sys.file_exists skip_list_cells_data_dir) then
      Lwt_utils_unix.create_dir skip_list_cells_data_dir
    else Lwt.return_unit
  in
  let*! () = Event.emit_dal_node_sqlite3_store_init () in
  Dal_store_sqlite3.Skip_list_cells.init
    ~data_dir:skip_list_cells_data_dir
    ~perm
    ()

module Skip_list_cells = struct
  let find_opt ?conn t skip_list_hash =
    Dal_store_sqlite3.Skip_list_cells.find_opt
      ?conn
      t.skip_list_cells_store
      skip_list_hash

  let find_by_slot_id_opt ?conn t slot_id =
    Dal_store_sqlite3.Skip_list_cells.find_by_slot_id_opt
      ?conn
      t.skip_list_cells_store
      slot_id

  let find_by_level ?conn t ~published_level =
    Dal_store_sqlite3.Skip_list_cells.find_by_level
      ?conn
      t.skip_list_cells_store
      ~published_level

  let insert ?conn t ~attested_level items =
    Dal_store_sqlite3.Skip_list_cells.insert
      ?conn
      t.skip_list_cells_store
      ~attested_level
      items

  let remove ?conn t ~published_level =
    Dal_store_sqlite3.Skip_list_cells.remove
      ?conn
      t.skip_list_cells_store
      ~published_level

  let schemas data_dir =
    let open Lwt_result_syntax in
    let* store = init_sqlite_skip_list_cells_store data_dir in
    Dal_store_sqlite3.Skip_list_cells.schemas store
end

let cache_not_yet_published_entry node_store commitment slot shares shard_proofs
    =
  Commitment_indexed_cache.replace
    node_store.not_yet_published_cache
    commitment
    (slot, shares, shard_proofs)

let upgrade_from_v2_to_v3 ~base_dir =
  let open Lwt_result_syntax in
  let*! () =
    Event.emit_store_upgrade_start
      ~old_version:(Version.make 2)
      ~new_version:(Version.make 3)
  in
  let file_path = Filename.concat base_dir "status_store" in
  let*! exists = Lwt_unix.file_exists file_path in
  let*! () =
    if exists then Lwt_utils_unix.remove_dir file_path else Lwt.return_unit
  in
  Version.write_version_file ~base_dir ~version:3

(* [upgradable old_version new_version] returns an upgrade function if
   the store is upgradable from [old_version] to [new_version]. Otherwise it
   returns [None]. *)
let upgradable old_version new_version :
    (base_dir:string -> unit tzresult Lwt.t) option =
  match (old_version, new_version) with
  | 2, 3 -> Some upgrade_from_v2_to_v3
  | _ -> None

(* Checks the version of the store with the respect to the current
   version. Returns [None] if the store does not need an upgrade and [Some
   upgrade] if the store is upgradable, where [upgrade] is a function that can
   be used to upgrade the store. It returns an error if the version is
   incompatible with the current one. *)
let rec check_version_and_may_upgrade base_dir =
  let open Lwt_result_syntax in
  let file_path = Version.version_file_path ~base_dir in
  let*! exists = Lwt_unix.file_exists file_path in
  let* version =
    if exists then Version.read_version_file ~file_path
    else
      (* In the absence of a version file, we use an heuristic to determine the
         version. *)
      let*! index = Lwt_unix.file_exists (Filename.concat base_dir "index") in
      if index then return (Version.make 0)
      else
        let*! status =
          Lwt_unix.file_exists (Filename.concat base_dir "status_store")
        in
        if status then return (Version.make 2)
        else return (Version.make Version.current_version)
  in
  if Version.(equal version current_version) then return_unit
  else
    match upgradable version Version.current_version with
    | Some upgrade ->
        let* () = upgrade ~base_dir in
        (* Now that we upgraded, check version again *)
        check_version_and_may_upgrade base_dir
    | None ->
        tzfail
          (Version.Invalid_data_dir_version
             {actual = version; expected = Version.current_version})

let init config profile_ctxt proto_parameters =
  let open Lwt_result_syntax in
  let base_dir = Configuration_file.store_path config in
  let* () = check_version_and_may_upgrade base_dir in
  let number_of_slots = proto_parameters.Types.number_of_slots in
  let number_of_shards =
    proto_parameters.cryptobox_parameters.number_of_shards
  in
  let attestation_lag = proto_parameters.attestation_lag in
  let traps_fraction = proto_parameters.traps_fraction in
  let statuses_cache_size =
    Constants.statuses_cache_size ~number_of_slots ~attestation_lag
  in
  let shards_store_lru_size =
    Constants.shards_store_lru_size ~number_of_slots
  in
  let slots_store_lru_size = Constants.slots_store_lru_size in
  let traps_cache_size =
    Constants.traps_cache_size
      ~number_of_slots
      ~number_of_shards
      ~attestation_lag
      ~traps_fraction
  in
  let slot_id_cache_size =
    Constants.slot_id_cache_size ~number_of_slots ~attestation_lag
  in
  let statuses_cache = Statuses_cache.init statuses_cache_size in
  let* shards =
    Shards.init
      ~profile_ctxt
      ~proto_parameters
      ~lru_size:shards_store_lru_size
      base_dir
      Stores_dirs.shard
  in
  let* slots =
    Slots.init base_dir Stores_dirs.slot ~lru_size:slots_store_lru_size
  in
  let* () = Version.(write_version_file ~base_dir ~version:current_version) in
  let traps = Traps.create ~capacity:traps_cache_size in
  let* chain_id = Chain_id.init ~root_dir:base_dir in
  let* last_processed_level = Last_processed_level.init ~root_dir:base_dir in
  let* first_seen_level = First_seen_level.init ~root_dir:base_dir in
  let* skip_list_cells_store = init_sqlite_skip_list_cells_store base_dir in
  let*! () = Event.emit_store_is_ready () in
  return
    {
      shards;
      slots;
      traps;
      statuses_cache;
      not_yet_published_cache =
        Commitment_indexed_cache.create Constants.not_yet_published_cache_size;
      finalized_commitments = Slot_id_cache.create ~capacity:slot_id_cache_size;
      chain_id;
      last_processed_level;
      first_seen_level;
      skip_list_cells_store;
    }

let add_slot_headers ~number_of_slots ~block_level slot_headers t =
  let module SI = Set.Make (Int) in
  let open Lwt_syntax in
  let statuses_cache = t.statuses_cache in
  let* waiting =
    List.fold_left_s
      (fun waiting slot_header ->
        let Dal_plugin.{slot_index; commitment = _; published_level} =
          slot_header
        in
        (* This invariant should hold. *)
        assert (Int32.equal published_level block_level) ;
        let slot_id =
          Types.Slot_id.{slot_level = published_level; slot_index}
        in
        let () =
          Statuses_cache.add_status statuses_cache `Waiting_attestation slot_id
        in
        Slot_id_cache.add ~number_of_slots t.finalized_commitments slot_header ;
        return (SI.add slot_index waiting))
      SI.empty
      slot_headers
  in
  List.iter
    (fun i ->
      let i_is_waiting_for_attestation = SI.mem i waiting in
      Dal_metrics.slot_waiting_for_attestation
        ~set:i_is_waiting_for_attestation
        i ;
      let slot_id = Types.Slot_id.{slot_level = block_level; slot_index = i} in
      if not i_is_waiting_for_attestation then
        Statuses_cache.add_status statuses_cache `Unpublished slot_id)
    (0 -- (number_of_slots - 1)) ;
  return_unit
