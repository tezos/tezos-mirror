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
     - 2: switch the KVS skip list store for a sqlite3 one. *)
  let current_version = 2

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

  let write_version_file ~base_dir =
    let version_file = version_file_path ~base_dir in
    Lwt_utils_unix.Json.write_file
      version_file
      (Data_encoding.Json.construct encoding current_version)
    |> trace (Could_not_write_version_file version_file)
end

module KVS = Key_value_store

module Stores_dirs = struct
  let shard = "shard_store"

  let slot = "slot_store"

  let status = "status_store"

  let skip_list_cells = "skip_list_store"
end

module Shards = struct
  type nonrec t = (Types.slot_id, int, Cryptobox.share) KVS.t

  let file_layout ~root_dir (slot_id : Types.slot_id) =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7045

       Make Key-Value store layout resilient to crypto parameters change.  Also,
       putting a value not far from the real number of shards allows saving disk
       storage. *)
    let number_of_shards = 4096 in
    let slot_id_string =
      Format.asprintf "%ld_%d" slot_id.slot_level slot_id.slot_index
    in
    let filepath = Filename.concat root_dir slot_id_string in
    Key_value_store.layout
      ~encoded_value_size:(Value_size_hooks.share_size ())
      ~encoding:Cryptobox.share_encoding
      ~filepath
      ~eq:Stdlib.( = )
      ~index_of:Fun.id
      ~number_of_keys_per_file:number_of_shards
      ()

  let with_metrics store f =
    let open Lwt_result_syntax in
    let* r = f () in
    let opened_files = KVS.View.opened_files store in
    let ongoing_actions = KVS.View.ongoing_actions store in
    Dal_metrics.update_kvs_shards_metrics ~opened_files ~ongoing_actions ;
    return r

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4973
     Make storage more resilient to DAL parameters change. *)
  let number_of_shards_available store slot_id shard_indexes =
    let open Lwt_result_syntax in
    List.fold_left_es
      (fun count shard_index ->
        let+ exists = KVS.value_exists store file_layout slot_id shard_index in
        if exists then count + 1 else count)
      0
      shard_indexes

  let write_all shards_store slot_id shards =
    let open Lwt_result_syntax in
    let* () =
      with_metrics shards_store @@ fun () ->
      Seq.ES.iter
        (fun {Cryptobox.index; share} ->
          let* exists =
            KVS.value_exists shards_store file_layout slot_id index
          in
          if exists then return_unit
          else
            let* () =
              KVS.write_value shards_store file_layout slot_id index share
            in
            let () = Dal_metrics.shard_stored () in
            let*! () =
              Event.emit_stored_slot_shard
                ~published_level:slot_id.slot_level
                ~slot_index:slot_id.slot_index
                ~shard_index:index
            in
            return_unit)
        shards
      |> Errors.other_lwt_result
    in
    return_unit

  let read_all shards_store slot_id ~number_of_shards =
    Seq.ints 0
    |> Seq.take_while (fun x -> x < number_of_shards)
    |> Seq.map (fun shard_index -> (slot_id, shard_index))
    |> KVS.read_values shards_store file_layout

  let read store slot_id shard_id =
    let open Lwt_result_syntax in
    let*! res =
      with_metrics store @@ fun () ->
      KVS.read_value store file_layout slot_id shard_id
    in
    match res with
    | Ok share -> return {Cryptobox.share; index = shard_id}
    | Error [KVS.Missing_stored_kvs_data _] -> fail Errors.not_found
    | Error err ->
        let data_kind = Types.Store.Shard in
        fail @@ Errors.decoding_failed data_kind err

  let count_values store slot_id =
    with_metrics store @@ fun () -> KVS.count_values store file_layout slot_id

  let remove store slot_id =
    let open Lwt_result_syntax in
    let* () =
      with_metrics store @@ fun () -> KVS.remove_file store file_layout slot_id
    in
    return_unit

  let init node_store_dir shard_store_dir =
    let root_dir = Filename.concat node_store_dir shard_store_dir in
    KVS.init ~lru_size:Constants.shards_store_lru_size ~root_dir
end

module Slots = struct
  type t = (Types.slot_id * int, unit, bytes) KVS.t

  let file_layout ~root_dir ((slot_id : Types.slot_id), slot_size) =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7045

       Make Key-Value store layout resilient to crypto parameters change. *)
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

  let init node_store_dir slot_store_dir =
    let root_dir = Filename.concat node_store_dir slot_store_dir in
    KVS.init ~lru_size:Constants.slots_store_lru_size ~root_dir

  let add_slot t ~slot_size slot (slot_id : Types.slot_id) =
    let open Lwt_result_syntax in
    let* () =
      KVS.write_value ~override:true t file_layout (slot_id, slot_size) () slot
      |> Errors.other_lwt_result
    in
    let*! () =
      Event.emit_stored_slot_content
        ~published_level:slot_id.slot_level
        ~slot_index:slot_id.slot_index
    in
    return_unit

  let find_slot t ~slot_size slot_id =
    let open Lwt_result_syntax in
    let*! res = KVS.read_value t file_layout (slot_id, slot_size) () in
    match res with
    | Ok slot -> return slot
    | Error [KVS.Missing_stored_kvs_data _] -> fail Errors.not_found
    | Error err ->
        let data_kind = Types.Store.Slot in
        fail @@ Errors.decoding_failed data_kind err

  let remove_slot t ~slot_size slot_id =
    KVS.remove_file t file_layout (slot_id, slot_size)
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

module Statuses = struct
  type t = (int32, int, Types.header_status) KVS.t

  let file_layout ~root_dir slot_level =
    (* The number of entries per file is the number of slots. We put
       here the max value (4096) because we don't have a cryptobox
       at hand to get the number_of_slots parameter. *)
    let number_of_keys_per_file = 4096 in
    let level_string = Format.asprintf "%ld" slot_level in
    let filepath = Filename.concat root_dir level_string in
    Key_value_store.layout
      ~encoding:Types.header_status_encoding
      ~filepath
      ~eq:Stdlib.( = )
      ~index_of:Fun.id
      ~number_of_keys_per_file
      ()

  let init node_store_dir status_store_dir =
    let root_dir = Filename.concat node_store_dir status_store_dir in
    KVS.init ~lru_size:Constants.status_store_lru_size ~root_dir

  let add_status t status (slot_id : Types.slot_id) =
    let open Lwt_result_syntax in
    let* () =
      KVS.write_value
        ~override:true
        t
        file_layout
        slot_id.slot_level
        slot_id.slot_index
        status
      |> Errors.other_lwt_result
    in
    let*! () =
      Event.emit_stored_slot_status
        ~published_level:slot_id.slot_level
        ~slot_index:slot_id.slot_index
        ~status
    in
    return_unit

  let find_status t (slot_id : Types.slot_id) =
    let open Lwt_result_syntax in
    let*! res =
      KVS.read_value t file_layout slot_id.slot_level slot_id.slot_index
    in
    match res with
    | Ok status -> return status
    | Error [KVS.Missing_stored_kvs_data _] -> fail Errors.not_found
    | Error err ->
        let data_kind = Types.Store.Header_status in
        fail @@ Errors.decoding_failed data_kind err

  let update_slot_headers_attestation ~published_level ~number_of_slots t
      attested =
    let open Lwt_result_syntax in
    List.iter_es
      (fun slot_index ->
        let index = Types.Slot_id.{slot_level = published_level; slot_index} in
        if attested slot_index then (
          Dal_metrics.slot_attested ~set:true slot_index ;
          add_status t `Attested index |> Errors.to_tzresult)
        else
          let* old_data_opt =
            find_status t index |> Errors.to_option_tzresult
          in
          Dal_metrics.slot_attested ~set:false slot_index ;
          if Option.is_some old_data_opt then
            add_status t `Unattested index |> Errors.to_tzresult
          else
            (* There is no header that has been included in a block
               and selected for this index. So, the slot cannot be
               attested or unattested. *)
            return_unit)
      (0 -- (number_of_slots - 1))

  let update_selected_slot_headers_statuses ~block_level ~attestation_lag
      ~number_of_slots attested t =
    let published_level = Int32.(sub block_level (of_int attestation_lag)) in
    update_slot_headers_attestation ~published_level ~number_of_slots t attested

  let get_slot_status ~slot_id t = find_status t slot_id

  let remove_level_status ~level t = KVS.remove_file t file_layout level
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
      [Legacy] refers to the current implementation using KVS.
      [SQLite3] corresponds to the new implementation integrating a
      [Sqlite.t] database into the DAL node for storing skip list
      cells and whose purpose is to replace the
      [Kvs_skip_list_cells_store] module. *)
  type kind = Legacy | SQLite3

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"legacy"
          (Tag 0)
          (constant "legacy")
          (function Legacy -> Some () | _ -> None)
          (fun () -> Legacy);
        case
          ~title:"sqlite3"
          (Tag 1)
          (constant "sqlite3")
          (function SQLite3 -> Some () | _ -> None)
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

  (** [set ?force store ~sqlite3_backend] configures the storage
      backend based on the value of [sqlite3_backend]. If
      [sqlite3_backend] is [true], it sets the [SQLite3] storage
      backend; otherwise, it sets the [Legacy] backend. This function
      fails with [Storage_backend_mismatch] if the previously used
      storage backend does not match the one specified by
      [sqlite3_backend] and [force] is false. If [force] is true, then
      overwrite the storage backend without taking into account the
      previous one. *)
  let set ?(force = false) store ~sqlite3_backend =
    let open Lwt_result_syntax in
    let* current_opt = load store in
    let specified = if sqlite3_backend then SQLite3 else Legacy in
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7528
       Update the following code according to the migration from KVS
       to SQLite3 once it is done.*)
    if force then
      let+ () = save store specified in
      specified
    else
      match current_opt with
      | Some current ->
          if current = specified then return current
          else tzfail (Storage_backend_mismatch {current; specified})
      | None ->
          let+ () = save store specified in
          specified
end

(** Store context *)
type t = {
  slot_header_statuses : Statuses.t;
  shards : Shards.t;
  slots : Slots.t;
  traps : Traps.t;
  cache :
    (Cryptobox.slot * Cryptobox.share array * Cryptobox.shard_proof array)
    Commitment_indexed_cache.t;
      (* The length of the array is the number of shards per slot *)
  finalized_commitments : Slot_id_cache.t;
  last_processed_level : Last_processed_level.t;
  first_seen_level : First_seen_level.t;
  skip_list_cells_store : Dal_store_sqlite3.Skip_list_cells.t;
}

let cache {cache; _} = cache

let first_seen_level {first_seen_level; _} = first_seen_level

let finalized_commitments {finalized_commitments; _} = finalized_commitments

let last_processed_level {last_processed_level; _} = last_processed_level

let shards {shards; _} = shards

let skip_list_cells t = t.skip_list_cells_store

let slot_header_statuses {slot_header_statuses; _} = slot_header_statuses

let slots {slots; _} = slots

let traps {traps; _} = traps

let init_sqlite_skip_list_cells_store ?(perm = `Read_write) data_dir =
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

  let find_by_slot_id_opt ?conn t ~attested_level ~slot_index =
    Dal_store_sqlite3.Skip_list_cells.find_by_slot_id_opt
      ?conn
      t.skip_list_cells_store
      ~attested_level
      ~slot_index

  let insert ?conn t ~attested_level items =
    Dal_store_sqlite3.Skip_list_cells.insert
      ?conn
      t.skip_list_cells_store
      ~attested_level
      items

  let remove ?conn t ~attested_level =
    Dal_store_sqlite3.Skip_list_cells.remove
      ?conn
      t.skip_list_cells_store
      ~attested_level

  let schemas data_dir =
    let open Lwt_result_syntax in
    let* store = init_sqlite_skip_list_cells_store data_dir in
    Dal_store_sqlite3.Skip_list_cells.schemas store
end

let init_skip_list_cells_store base_dir =
  (* We support at most 64 back-pointers, each of which takes 32 bytes.
     The cells content itself takes less than 64 bytes. *)
  let padded_encoded_cell_size = 64 * (32 + 1) in
  (* A pointer hash is 32 bytes length, but because of the double
     encoding in Dal_proto_types and then in skip_list_cells_store, we
     have an extra 4 bytes for encoding the size. *)
  let encoded_hash_size = 32 + 4 in
  Kvs_skip_list_cells_store.init
    ~node_store_dir:base_dir
    ~skip_list_store_dir:Stores_dirs.skip_list_cells
    ~padded_encoded_cell_size
    ~encoded_hash_size

let cache_entry node_store commitment slot shares shard_proofs =
  Commitment_indexed_cache.replace
    node_store.cache
    commitment
    (slot, shares, shard_proofs)

let upgrade_from_v0_to_v1 ~base_dir =
  let open Lwt_syntax in
  let ( // ) = Filename.Infix.( // ) in
  let* () =
    Event.emit_store_upgrade_start
      ~old_version:(Version.make 0)
      ~new_version:(Version.make 1)
  in
  let rec move_directory_contents src dst =
    let stream = Lwt_unix.files_of_directory src in
    Lwt_stream.iter_s
      (fun name ->
        Lwt.catch
          (fun () ->
            match name with
            | "." | ".." -> Lwt.return_unit
            | file_name -> (
                let src_path = src // file_name in
                let dst_path = dst // file_name in
                let* stats = Lwt_unix.lstat src_path in
                match stats.st_kind with
                | Unix.S_REG | S_LNK -> Lwt_unix.rename src_path dst_path
                | S_DIR ->
                    let* () = Lwt_unix.mkdir dst_path stats.st_perm in
                    let* () = move_directory_contents src_path dst_path in
                    Lwt_utils_unix.remove_dir src_path
                | _ -> Lwt.return_unit))
          (fun exn ->
            let src_path = src // name in
            let dst_path = dst // name in
            let* () =
              Event.emit_store_upgrade_error_moving_directory
                ~src:src_path
                ~dst:dst_path
                ~exn:(Printexc.to_string exn)
            in
            Lwt.return_unit))
      stream
  in
  let move_and_rename old_path new_path =
    let* () =
      Lwt.catch
        (fun () -> Lwt_unix.mkdir new_path 0o700)
        (fun exn ->
          let* () =
            Event.emit_store_upgrade_error_creating_directory
              ~path:new_path
              ~exn:(Printexc.to_string exn)
          in
          Lwt.return ())
    in
    let* () = move_directory_contents old_path new_path in
    Lwt_utils_unix.remove_dir old_path
  in
  (* Remove the Irmin store, that is, delete the "index" directory and all files
     that start with "store". *)
  let stream = Lwt_unix.files_of_directory base_dir in
  let irmin_prefix = "store" in
  let* () =
    Lwt_stream.iter_p
      (fun name ->
        let path = Filename.Infix.(base_dir // name) in
        if String.equal name "index" then
          (* that's Irmin related *)
          Lwt_utils_unix.remove_dir path
        else if String.starts_with ~prefix:irmin_prefix name then
          Lwt_unix.unlink path
        else if String.equal name "shard_store" then
          (* The V0 shard store uses a different layout. We just delete it, for
             simplicity. *)
          Lwt_utils_unix.remove_dir path
        else if String.equal name "skip_list" then
          (* The skip list store is not handled by this module, but we treat this
             case here, for simplicity *)
          let new_path = Filename.Infix.(base_dir // "skip_list_store") in
          move_and_rename path new_path
        else Lwt.return ())
      stream
  in
  Event.emit_store_upgraded
    ~old_version:(Version.make 0)
    ~new_version:(Version.make 1)

let upgrade_from_v1_to_v2 ~base_dir =
  let open Lwt_result_syntax in
  let*! () =
    Event.emit_store_upgrade_start
      ~old_version:(Version.make 1)
      ~new_version:(Version.make 2)
  in
  (* Initialize both stores and migrate. *)
  let* storage_backend_store = Storage_backend.init ~root_dir:base_dir in
  let* storage_backend = Storage_backend.load storage_backend_store in
  let*! res =
    match storage_backend with
    | None | Some Storage_backend.Legacy ->
        let* kvs_store = init_skip_list_cells_store base_dir in
        let* sql_store = init_sqlite_skip_list_cells_store base_dir in
        let* () =
          Store_migrations.migrate_skip_list_store kvs_store sql_store
        in
        let* () = Kvs_skip_list_cells_store.close kvs_store in
        let*! () = Dal_store_sqlite3.Skip_list_cells.close sql_store in
        return_unit
    | Some SQLite3 ->
        (* If a previous sqlite database has been created using the
           `--sqlite3-backend` experimental flag. We simply move it to the
           new destination path. *)
        let open Filename.Infix in
        let mv name =
          let previous_path = base_dir // name in
          let new_path = base_dir // Stores_dirs.skip_list_cells // name in
          if Sys.(file_exists previous_path) then
            let*! () =
              if not (Sys.file_exists new_path) then
                Lwt_utils_unix.copy_file ~src:previous_path ~dst:new_path ()
              else Lwt.return_unit
            in
            let*! () = Lwt_unix.unlink previous_path in
            return_unit
          else return_unit
        in
        let*! () =
          Lwt_utils_unix.create_dir (base_dir // Stores_dirs.skip_list_cells)
        in
        let* () = mv Dal_store_sqlite3.sqlite_file_name in
        let* () = mv (Dal_store_sqlite3.sqlite_file_name ^ "-shm") in
        let* () = mv (Dal_store_sqlite3.sqlite_file_name ^ "-wal") in
        return_unit
  in
  match res with
  | Ok () ->
      (* Set the new storage backend to sqlite3. *)
      let* (_ : Storage_backend.kind) =
        Storage_backend.set
          storage_backend_store
          ~sqlite3_backend:true
          ~force:true
      in
      (* Remove the Stores_dirs.skip_list_cells directory. *)
      let open Filename.Infix in
      let store_dir = base_dir // Stores_dirs.skip_list_cells in
      let*! () = Lwt_utils_unix.remove_dir (store_dir // "hashes") in
      let*! () = Lwt_utils_unix.remove_dir (store_dir // "cells") in
      (* The storage upgrade has been done. *)
      let*! () =
        Event.emit_store_upgraded
          ~old_version:(Version.make 1)
          ~new_version:(Version.make 2)
      in
      return_unit
  | Error err ->
      (* Clean the sqlite store unless the storage backend was already set to sqlite. *)
      let* storage_backend = Storage_backend.load storage_backend_store in
      let*! () =
        match storage_backend with
        | None | Some Legacy ->
            let rm name =
              let open Filename.Infix in
              let path = base_dir // Stores_dirs.skip_list_cells // name in
              Lwt_unix.unlink path
            in
            let*! () = rm Dal_store_sqlite3.sqlite_file_name in
            let*! () = rm (Dal_store_sqlite3.sqlite_file_name ^ "-shm") in
            rm (Dal_store_sqlite3.sqlite_file_name ^ "-wal")
        | Some SQLite3 -> Lwt.return_unit
      in
      (* The store upgrade failed. *)
      let*! () = Event.emit_store_upgrade_error () in
      Format.eprintf "%a" Error_monad.pp_print_trace err ;
      fail err

(* Returns [upgradable old_version new_version] returns an upgrade function if
   the store is upgradable from [old_version] to [new_version]. Otherwise it
   returns [None]. *)
let upgradable old_version new_version :
    (base_dir:string -> unit tzresult Lwt.t) option =
  let open Lwt_result_syntax in
  match (old_version, new_version) with
  | 0, 1 ->
      Some
        (fun ~base_dir ->
          let*! () = upgrade_from_v0_to_v1 ~base_dir in
          return_unit)
  | 0, 2 ->
      Some
        (fun ~base_dir ->
          let*! () = upgrade_from_v0_to_v1 ~base_dir in
          upgrade_from_v1_to_v2 ~base_dir)
  | 1, 2 -> Some (fun ~base_dir -> upgrade_from_v1_to_v2 ~base_dir)
  | _ -> None

(* Checks the version of the store with the respect to the current
   version. Returns [None] if the store does not need an upgrade and [Some
   upgrade] if the store is upgradable, where [upgrade] is a function that can
   be used to upgrade the store. It returns an error if the version is
   incompatible with the current one. *)
let check_version_and_may_upgrade base_dir =
  let open Lwt_result_syntax in
  let file_path = Version.version_file_path ~base_dir in
  let*! exists = Lwt_unix.file_exists file_path in
  let* version =
    if exists then Version.read_version_file ~file_path
    else
      (* In the absence of a version file, we use an heuristic to determine the
         version. *)
      let*! exists = Lwt_unix.file_exists (Filename.concat base_dir "index") in
      return
      @@ if exists then Version.make 0 else Version.make Version.current_version
  in
  if Version.(equal version current_version) then return_unit
  else
    match upgradable version Version.current_version with
    | Some upgrade ->
        let* () = upgrade ~base_dir in
        Version.write_version_file ~base_dir
    | None ->
        tzfail
          (Version.Invalid_data_dir_version
             {actual = version; expected = Version.current_version})

(** [init config] inits the store on the filesystem using the
    given [config]. *)
let init config =
  let open Lwt_result_syntax in
  let base_dir = Configuration_file.store_path config in
  let* () = check_version_and_may_upgrade base_dir in
  let* slot_header_statuses = Statuses.init base_dir Stores_dirs.status in
  let* shards = Shards.init base_dir Stores_dirs.shard in
  let* slots = Slots.init base_dir Stores_dirs.slot in
  let traps = Traps.create ~capacity:Constants.traps_cache_size in
  let* last_processed_level = Last_processed_level.init ~root_dir:base_dir in
  let* first_seen_level = First_seen_level.init ~root_dir:base_dir in
  let* skip_list_cells_store = init_sqlite_skip_list_cells_store base_dir in
  let*! () = Event.emit_store_is_ready () in
  return
    {
      shards;
      slots;
      traps;
      slot_header_statuses;
      cache = Commitment_indexed_cache.create Constants.cache_size;
      finalized_commitments =
        Slot_id_cache.create ~capacity:Constants.slot_id_cache_size;
      last_processed_level;
      first_seen_level;
      skip_list_cells_store;
    }

let add_slot_headers ~number_of_slots ~block_level slot_headers t =
  let module SI = Set.Make (Int) in
  let open Lwt_result_syntax in
  let slot_header_statuses = t.slot_header_statuses in
  let* waiting =
    List.fold_left_es
      (fun waiting slot_header ->
        let Dal_plugin.{slot_index; commitment = _; published_level} =
          slot_header
        in
        (* This invariant should hold. *)
        assert (Int32.equal published_level block_level) ;
        let index = Types.Slot_id.{slot_level = published_level; slot_index} in
        let* () =
          Statuses.add_status slot_header_statuses `Waiting_attestation index
          |> Errors.to_tzresult
        in
        Slot_id_cache.add ~number_of_slots t.finalized_commitments slot_header ;
        return (SI.add slot_index waiting))
      SI.empty
      slot_headers
  in
  List.iter
    (fun i ->
      Dal_metrics.slot_waiting_for_attestation ~set:(SI.mem i waiting) i)
    (0 -- (number_of_slots - 1)) ;
  return_unit
