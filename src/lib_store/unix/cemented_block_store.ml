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

open Store_errors

(* Cemented files overlay:

   | <n> x <offset (4 bytes)> | <n> x <blocks> |

   <offset> is an absolute offset in the file.
   <blocks> are prefixed by 4 bytes of length
*)
(* On-disk index of block's hashes to level *)
module Cemented_block_level_index =
  Index_unix.Make (Block_key) (Block_level) (Index.Cache.Unbounded)

(* On-disk index of block's level to hash *)
module Cemented_block_hash_index =
  Index_unix.Make (Block_level) (Block_key) (Index.Cache.Unbounded)

type cemented_metadata_file = {
  start_level : int32;
  end_level : int32;
  metadata_file : [`Cemented_blocks_metadata] Naming.file;
}

module Metadata_fd_cache =
  Aches.Rache.Borrow
    (Aches.Rache.LRU)
    (struct
      include String

      let hash = Stdlib.Hashtbl.hash
    end)

type cemented_blocks_file = {
  start_level : int32;
  end_level : int32;
  file : [`Cemented_blocks_file] Naming.file;
}

type metadata_handler = {fd : Zip.in_file; waiter : Lwt_idle_waiter.t}

type t = {
  cemented_blocks_dir : [`Cemented_blocks_dir] Naming.directory;
  cemented_block_level_index : Cemented_block_level_index.t;
  cemented_block_hash_index : Cemented_block_hash_index.t;
  mutable cemented_blocks_files : cemented_blocks_file array option;
  metadata_fd_cache : metadata_handler Metadata_fd_cache.t;
}

type chunk_iterator = {
  chunk_length : int;
  reading_sequence : (Block_hash.t * int * bytes) tzresult Lwt.t Seq.t;
}

let make_chunk_iterator bl =
  let chunk_length = List.length bl in
  let reading_sequence =
    List.to_seq bl
    |> Seq.map (fun b ->
           let open Lwt_result_syntax in
           let hash = Block_repr.hash b in
           let b = Data_encoding.Binary.to_bytes_exn Block_repr.encoding b in
           return (hash, Bytes.length b, b))
  in
  {chunk_length; reading_sequence}

let cemented_blocks_files {cemented_blocks_files; _} = cemented_blocks_files

let cemented_blocks_file_length {start_level; end_level; _} =
  (* nb blocks : (end_level - start_level) + 1 *)
  Int32.(succ (sub end_level start_level))

let cemented_block_level_index {cemented_block_level_index; _} =
  cemented_block_level_index

let cemented_block_hash_index {cemented_block_hash_index; _} =
  cemented_block_hash_index

(* The log_size corresponds to the maximum size of the memory zone
   allocated in memory before flushing it onto the disk. It is
   basically a cache which is use for the index. The cache size is
   `log_size * log_entry` where a `log_entry` is roughly 56 bytes. *)
let default_index_log_size = 10_000

let default_compression_level = 9

(* Defines the maximum number of file descriptors that are cached to
   speed up the read of metadata from the cemented store. As these
   file descriptors are kept open, this limit should not be increased
   too much to avoid resources starvation. *)
let default_metadata_fd_cache_size = 5

let init_metadata_fd_cache () =
  let destroyer _key {fd; waiter} =
    Unit.catch (fun () ->
        Lwt.dont_wait
          (fun () ->
            (* [task] is lower priority than [force_idle], which is
               used for reading part, closing fd won't occur until all
               the scheduled reads are fully performed. *)
            Lwt_idle_waiter.task waiter @@ fun () ->
            Zip.close_in fd ;
            Lwt.return_unit)
          (fun _exn -> ()))
  in
  Metadata_fd_cache.create destroyer default_metadata_fd_cache_size

let create ~log_size cemented_blocks_dir =
  let open Lwt_result_syntax in
  protect (fun () ->
      let cemented_blocks_dir_path = Naming.dir_path cemented_blocks_dir in
      let cemented_blocks_metadata_dir =
        cemented_blocks_dir |> Naming.cemented_blocks_metadata_dir
      in
      let cemented_blocks_metadata_dir_path =
        Naming.dir_path cemented_blocks_metadata_dir
      in
      let* () =
        Lwt.catch
          (fun () ->
            let*! () = Lwt_utils_unix.create_dir cemented_blocks_dir_path in
            let*! () =
              Lwt_utils_unix.create_dir cemented_blocks_metadata_dir_path
            in
            return_unit)
          (function
            | Failure s when s = "Not a directory" ->
                tzfail
                  (Store_errors.Failed_to_init_cemented_block_store
                     cemented_blocks_dir_path)
            | e -> Lwt.fail e)
      in
      let cemented_block_level_index =
        Cemented_block_level_index.v
          ~readonly:false
          ~log_size
          (cemented_blocks_dir |> Naming.cemented_blocks_level_index_dir
         |> Naming.dir_path)
      in
      let cemented_block_hash_index =
        Cemented_block_hash_index.v
          ~readonly:false
          ~log_size
          (cemented_blocks_dir |> Naming.cemented_blocks_hash_index_dir
         |> Naming.dir_path)
      in
      (* Empty table at first *)
      let cemented_blocks_files = None in
      let metadata_fd_cache = init_metadata_fd_cache () in
      let cemented_store =
        {
          cemented_blocks_dir;
          cemented_block_level_index;
          cemented_block_hash_index;
          cemented_blocks_files;
          metadata_fd_cache;
        }
      in
      return cemented_store)

let compare_cemented_files {start_level; _} {start_level = start_level'; _} =
  Compare.Int32.compare start_level start_level'

let compare_cemented_metadata ({start_level; _} : cemented_metadata_file)
    ({start_level = start_level'; _} : cemented_metadata_file) =
  Compare.Int32.compare start_level start_level'

let load_table cemented_blocks_dir =
  let open Lwt_result_syntax in
  protect (fun () ->
      let cemented_blocks_dir_path = Naming.dir_path cemented_blocks_dir in
      (* No need to check the existence of the cemented block
         directory as it is always there, even if empty. *)
      let*! dir_handle = Lwt_unix.opendir cemented_blocks_dir_path in
      let rec loop acc =
        let*! filename =
          Option.catch_s
            ~catch_only:(function End_of_file -> true | _ -> false)
            (fun () -> Lwt_unix.readdir dir_handle)
        in
        match filename with
        | Some filename -> (
            let levels = String.split_on_char '_' filename in
            match levels with
            | [start_level; end_level] -> (
                let start_level_opt = Int32.of_string_opt start_level in
                let end_level_opt = Int32.of_string_opt end_level in
                match (start_level_opt, end_level_opt) with
                | Some start_level, Some end_level ->
                    let file =
                      Naming.cemented_blocks_file
                        cemented_blocks_dir
                        ~start_level
                        ~end_level
                    in
                    loop ({start_level; end_level; file} :: acc)
                | _ -> loop acc)
            | _ -> loop acc)
        | None -> Lwt.return acc
      in
      let*! cemented_files_list =
        Lwt.finalize
          (fun () -> loop [])
          (fun () -> Lwt_unix.closedir dir_handle)
      in
      match cemented_files_list with
      | [] -> return_none
      | cemented_files_list ->
          let cemented_files_array = Array.of_list cemented_files_list in
          Array.sort compare_cemented_files cemented_files_array ;
          return_some cemented_files_array)

let load_metadata_table cemented_blocks_dir =
  let open Lwt_result_syntax in
  protect (fun () ->
      let cemented_metadata_dir =
        Naming.cemented_blocks_metadata_dir cemented_blocks_dir
      in
      let metadata_dir_path = Naming.dir_path cemented_metadata_dir in
      (* Make sure that the cemented metadata data directory exists,
         as it may not be the case, depending on the history mode. *)
      let*! exists = Lwt_unix.file_exists metadata_dir_path in
      if exists then (
        let*! dir_handle = Lwt_unix.opendir metadata_dir_path in
        let rec loop acc =
          let*! filename =
            Option.catch_s
              ~catch_only:(function End_of_file -> true | _ -> false)
              (fun () -> Lwt_unix.readdir dir_handle)
          in
          match filename with
          | Some filename -> (
              let levels =
                String.split_on_char '_' (Filename.remove_extension filename)
              in
              match levels with
              | [start_level; end_level] -> (
                  let start_level_opt = Int32.of_string_opt start_level in
                  let end_level_opt = Int32.of_string_opt end_level in
                  match (start_level_opt, end_level_opt) with
                  | Some start_level, Some end_level ->
                      let file =
                        Naming.cemented_blocks_file
                          cemented_blocks_dir
                          ~start_level
                          ~end_level
                      in
                      let metadata_file =
                        Naming.cemented_blocks_metadata_file
                          cemented_metadata_dir
                          file
                      in
                      loop ({start_level; end_level; metadata_file} :: acc)
                  | _ -> loop acc)
              | _ -> loop acc)
          | None -> Lwt.return acc
        in
        let*! cemented_files =
          Lwt.finalize
            (fun () -> loop [])
            (fun () -> Lwt_unix.closedir dir_handle)
        in
        match cemented_files with
        | [] -> return_none
        | cemented_files_list ->
            let cemented_files_array = Array.of_list cemented_files_list in
            Array.sort compare_cemented_metadata cemented_files_array ;
            return_some cemented_files_array)
      else return_none)

let cemented_metadata_files cemented_block_store =
  load_metadata_table cemented_block_store.cemented_blocks_dir

let load ~readonly ~log_size cemented_blocks_dir =
  let open Lwt_result_syntax in
  let cemented_block_level_index =
    Cemented_block_level_index.v
      ~readonly
      ~log_size
      (cemented_blocks_dir |> Naming.cemented_blocks_level_index_dir
     |> Naming.dir_path)
  in
  let cemented_block_hash_index =
    Cemented_block_hash_index.v
      ~readonly
      ~log_size
      (cemented_blocks_dir |> Naming.cemented_blocks_hash_index_dir
     |> Naming.dir_path)
  in
  let* cemented_blocks_files = load_table cemented_blocks_dir in
  let metadata_fd_cache = init_metadata_fd_cache () in
  let cemented_store =
    {
      cemented_blocks_dir;
      cemented_block_level_index;
      cemented_block_hash_index;
      cemented_blocks_files;
      metadata_fd_cache;
    }
  in
  return cemented_store

let init ?(log_size = default_index_log_size) chain_dir ~readonly =
  let open Lwt_result_syntax in
  let cemented_blocks_dir = Naming.cemented_blocks_dir chain_dir in
  let cemented_blocks_dir_path = Naming.dir_path cemented_blocks_dir in
  let*! b = Lwt_unix.file_exists cemented_blocks_dir_path in
  match b with
  | true ->
      let*! is_directory =
        Lwt_utils_unix.is_directory cemented_blocks_dir_path
      in
      let* () =
        fail_unless
          is_directory
          (Failed_to_init_cemented_block_store cemented_blocks_dir_path)
      in
      load ~readonly ~log_size cemented_blocks_dir
  | false -> create ~log_size cemented_blocks_dir

let close cemented_store =
  (try
     Cemented_block_level_index.close cemented_store.cemented_block_level_index
   with Index.Closed -> ()) ;
  (try Cemented_block_hash_index.close cemented_store.cemented_block_hash_index
   with Index.Closed -> ()) ;
  (* The cache element's [destroyer] is asynchronous so this call
     won't hang. In practice, this function is only called when the
     store is closed which is one of the last component to be
     terminated which means potential new reads won't be scheduled. *)
  Metadata_fd_cache.clear cemented_store.metadata_fd_cache

let offset_length = 4 (* file offset *)

let offset_encoding = Data_encoding.int31

let find_block_file cemented_store block_level =
  try
    if Compare.Int32.(block_level < 0l) then None
    else
      match cemented_store.cemented_blocks_files with
      | None -> None
      | Some cemented_blocks_files ->
          let length = Array.length cemented_blocks_files in
          let last_interval =
            cemented_blocks_file_length cemented_blocks_files.(length - 1)
          in
          (* Pivot heuristic: in the main chain, the first cycle is
             [0_1]. Then, the second cycle is [2_4097]. *)
          let heuristic_initial_pivot =
            match block_level with
            | 0l | 1l -> 0
            | _ ->
                Compare.Int.min
                  (length - 1)
                  (1 + Int32.(to_int (div (sub block_level 2l) last_interval)))
          in
          (* Dichotomic search *)
          let rec loop (inf, sup) pivot =
            if pivot < inf || pivot > sup || inf > sup then None
            else
              let ({start_level; end_level; _} as res) =
                cemented_blocks_files.(pivot)
              in
              if
                Compare.Int32.(
                  block_level >= start_level && block_level <= end_level)
              then (* Found *)
                Some res
              else if Compare.Int32.(block_level > end_level) then
                (* Making sure the pivot is strictly increasing *)
                let new_pivot = pivot + max 1 ((sup - pivot) / 2) in
                loop (pivot, sup) new_pivot
              else
                (* Making sure the pivot is strictly decreasing *)
                let new_pivot = pivot - max 1 ((pivot - inf) / 2) in
                loop (inf, pivot) new_pivot
          in
          loop (0, length - 1) heuristic_initial_pivot
  with _ -> None

(* Hypothesis: the table is ordered. *)
let compute_location cemented_store block_level =
  let open Option_syntax in
  let+ {start_level; file; _} = find_block_file cemented_store block_level in
  let level_delta = Int32.(to_int (sub block_level start_level)) in
  (file, level_delta)

let is_cemented cemented_store hash =
  try
    Cemented_block_level_index.mem
      cemented_store.cemented_block_level_index
      hash
  with Not_found -> false

let get_cemented_block_level cemented_store hash =
  try
    Some
      (Cemented_block_level_index.find
         cemented_store.cemented_block_level_index
         hash)
  with Not_found -> None

let get_cemented_block_hash cemented_store level =
  try
    Some
      (Cemented_block_hash_index.find
         cemented_store.cemented_block_hash_index
         level)
  with Not_found -> None

let read_block_metadata cemented_store metadata_file_path block_level =
  let open Lwt_result_syntax in
  let*! b = Lwt_unix.file_exists metadata_file_path in
  match b with
  | false -> return_none
  | true ->
      Lwt.catch
        (fun () ->
          let cache = cemented_store.metadata_fd_cache in
          (* Exceptions will be caught by the enclosing [catch] *)
          let mk_metadata_handler path =
            (* The file descriptor will be closed when the cache
               clears the value and calls its finalizer defined at the
               cache creation which will wait for the potential
               metadata reads to be performed. *)
            let fd = Zip.open_in path in
            let waiter = Lwt_idle_waiter.create () in
            {fd; waiter}
          in
          let {fd; waiter} =
            Metadata_fd_cache.borrow_or_make
              cache
              metadata_file_path
              mk_metadata_handler
              Fun.id
          in
          let*! metadata =
            (* Using [force_idle] prevents the cache's resource
               cleaner to acquire the lock needed (through [task]) to
               close the file descriptor while a read is happening. *)
            Lwt_idle_waiter.force_idle waiter (fun () ->
                Lwt_preemptive.detach
                  (fun () ->
                    let entry =
                      (* Note that the `fd` here is used outside of the
                         borrowed scope. In this specific case it is ok
                         because the destructor for the resource is wrapped
                         inside a `Lwt_ide_waiter.when_idle` which will
                         only run after all the reads are done. *)
                      Zip.find_entry fd (Int32.to_string block_level)
                    in
                    Zip.read_entry fd entry)
                  ())
          in
          Block_repr.decode_metadata metadata |> return)
        (fun exn ->
          let*! () =
            Store_events.(emit metadata_read_error (Printexc.to_string exn))
          in
          return_none)

let read_block_metadata ?location cemented_store block_level =
  let open Lwt_result_syntax in
  let location =
    match location with
    | Some _ -> location
    | None -> compute_location cemented_store block_level
  in
  match location with
  | None -> return_none
  | Some (cemented_file, _block_number) ->
      let metadata_file =
        Naming.(
          cemented_store.cemented_blocks_dir |> cemented_blocks_metadata_dir
          |> fun d -> cemented_blocks_metadata_file d cemented_file |> file_path)
      in
      read_block_metadata cemented_store metadata_file block_level

let cement_blocks_metadata cemented_store blocks =
  let open Lwt_result_syntax in
  let cemented_metadata_dir =
    cemented_store.cemented_blocks_dir |> Naming.cemented_blocks_metadata_dir
  in
  let cemented_metadata_dir_path = cemented_metadata_dir |> Naming.dir_path in
  let*! () =
    let*! b = Lwt_unix.file_exists cemented_metadata_dir_path in
    match b with
    | true -> Lwt.return_unit
    | false -> Lwt_utils_unix.create_dir cemented_metadata_dir_path
  in
  let* () = fail_unless (blocks <> []) (Cannot_cement_blocks_metadata `Empty) in
  match
    find_block_file
      cemented_store
      (Block_repr.level
         (List.hd blocks |> WithExceptions.Option.get ~loc:__LOC__))
  with
  | None -> tzfail (Cannot_cement_blocks_metadata `Not_cemented)
  | Some {file; _} ->
      let tmp_metadata_file_path =
        Naming.cemented_blocks_tmp_metadata_file cemented_metadata_dir file
        |> Naming.file_path
      in
      let*! out_file =
        Lwt_preemptive.detach Zip.open_out tmp_metadata_file_path
      in
      let*! () =
        Lwt.finalize
          (fun () ->
            List.iter_s
              (fun block ->
                let level = Block_repr.level block in
                match Block_repr.metadata block with
                | Some metadata ->
                    let metadata =
                      Data_encoding.Binary.to_string_exn
                        Block_repr.metadata_encoding
                        metadata
                    in
                    Lwt_preemptive.detach
                      (fun () ->
                        Zip.add_entry
                          ~level:default_compression_level
                          metadata
                          out_file
                          (Int32.to_string level))
                      ()
                | None -> Lwt.return_unit)
              blocks)
          (fun () -> Lwt_preemptive.detach Zip.close_out out_file)
      in
      let metadata_file_path =
        Naming.cemented_blocks_metadata_file cemented_metadata_dir file
        |> Naming.file_path
      in
      let*! () = Lwt_unix.rename tmp_metadata_file_path metadata_file_path in
      return_unit

let read_block fd block_number =
  let open Lwt_syntax in
  let* _ofs = Lwt_unix.lseek fd (block_number * offset_length) Unix.SEEK_SET in
  let offset_buffer = Bytes.create offset_length in
  (* We read the (absolute) offset at the position in the offset array *)
  let* () =
    Lwt_utils_unix.read_bytes ~pos:0 ~len:offset_length fd offset_buffer
  in
  let offset =
    Data_encoding.(Binary.of_bytes_exn offset_encoding offset_buffer)
  in
  let* _ofs = Lwt_unix.lseek fd offset Unix.SEEK_SET in
  (* We move the cursor to the element's position *)
  let* block, _len = Block_repr_unix.read_next_block_exn fd in
  Lwt.return block

let get_lowest_cemented_level cemented_store =
  match cemented_store.cemented_blocks_files with
  | None -> None
  | Some cemented_blocks_files ->
      let nb_cemented_blocks = Array.length cemented_blocks_files in
      if nb_cemented_blocks > 0 then Some cemented_blocks_files.(0).start_level
      else None

let get_highest_cemented_level cemented_store =
  match cemented_store.cemented_blocks_files with
  | None -> None
  | Some cemented_blocks_files ->
      let nb_cemented_blocks = Array.length cemented_blocks_files in
      if nb_cemented_blocks > 0 then
        Some cemented_blocks_files.(nb_cemented_blocks - 1).end_level
      else (* No cemented blocks*)
        None

let get_cemented_block_by_level (cemented_store : t) ~read_metadata level =
  let open Lwt_result_syntax in
  match compute_location cemented_store level with
  | None -> return_none
  | Some ((filename, block_number) as location) ->
      let file_path = Naming.file_path filename in
      let*! fd = Lwt_unix.openfile file_path [Unix.O_RDONLY; O_CLOEXEC] 0o444 in
      let*! block =
        Lwt.finalize
          (fun () -> read_block fd block_number)
          (fun () ->
            let*! _ = Lwt_utils_unix.safe_close fd in
            Lwt.return_unit)
      in
      if read_metadata then
        let* metadata = read_block_metadata ~location cemented_store level in
        return_some {block with metadata}
      else return_some block

let read_block_metadata cemented_store block_level =
  read_block_metadata cemented_store block_level

let get_cemented_block_by_hash ~read_metadata (cemented_store : t) hash =
  let open Lwt_result_syntax in
  match get_cemented_block_level cemented_store hash with
  | None -> return_none
  | Some level ->
      get_cemented_block_by_level ~read_metadata cemented_store level

(* Hypothesis:
   - The block list is expected to be ordered by increasing
     level and no blocks are skipped.
   - If the first block has metadata, metadata are written
     and all blocks are expected to have metadata. *)
let cement_blocks ?(check_consistency = true) (cemented_store : t)
    ~write_metadata ({chunk_length; reading_sequence} : chunk_iterator) =
  let open Lwt_result_syntax in
  let nb_blocks = chunk_length in
  let preamble_length = nb_blocks * offset_length in
  let* () = fail_when (nb_blocks = 0) (Cannot_cement_blocks `Empty) in
  let* first_block_level =
    let* _block_hash, _n, block_bytes =
      match reading_sequence () with Cons (x, _) -> x | Nil -> assert false
    in
    return (Block_repr_unix.raw_get_block_level block_bytes)
  in
  let last_block_level =
    Int32.(add first_block_level (of_int (nb_blocks - 1)))
  in
  let* () =
    if check_consistency then
      match get_highest_cemented_level cemented_store with
      | None -> return_unit
      | Some highest_cemented_block ->
          fail_when
            Compare.Int32.(
              first_block_level <> Int32.succ highest_cemented_block)
            (Cannot_cement_blocks `Higher_cemented)
    else return_unit
  in
  let file =
    Naming.cemented_blocks_file
      cemented_store.cemented_blocks_dir
      ~start_level:first_block_level
      ~end_level:last_block_level
  in
  let final_path = Naming.file_path file in
  (* Manipulate temporary files and swap it when everything is written *)
  let tmp_file_path = final_path ^ ".tmp" in
  let*! exists = Lwt_unix.file_exists tmp_file_path in
  let* () = fail_when exists (Temporary_cemented_file_exists tmp_file_path) in
  let*! fd =
    Lwt_unix.openfile
      tmp_file_path
      Unix.[O_CREAT; O_TRUNC; O_RDWR; O_CLOEXEC]
      0o644
  in
  (* Metadata writing hooks *)
  let* metadata_writer, metadata_finalizer =
    if write_metadata then
      let cemented_metadata_dir =
        cemented_store.cemented_blocks_dir
        |> Naming.cemented_blocks_metadata_dir
      in
      let cemented_metadata_dir_path =
        cemented_metadata_dir |> Naming.dir_path
      in
      let*! () =
        let*! b = Lwt_unix.file_exists cemented_metadata_dir_path in
        match b with
        | true -> Lwt.return_unit
        | false -> Lwt_utils_unix.create_dir cemented_metadata_dir_path
      in
      let* () =
        fail_when
          (Seq.is_empty reading_sequence)
          (Cannot_cement_blocks_metadata `Empty)
      in
      let tmp_metadata_file_path =
        Naming.cemented_blocks_tmp_metadata_file cemented_metadata_dir file
        |> Naming.file_path
      in
      let*! out_file =
        Lwt_preemptive.detach Zip.open_out tmp_metadata_file_path
      in
      let metadata_writer
          (block_bytes, total_block_length, block_level, metadata_offset) =
        Lwt_preemptive.detach
          (fun () ->
            let add, finish =
              Zip.add_entry_generator
                out_file
                ~level:default_compression_level
                (Int32.to_string block_level)
            in
            add
              block_bytes
              metadata_offset
              (total_block_length - metadata_offset) ;
            finish ())
          ()
      in
      let metadata_finalizer () =
        let*! () = Lwt_preemptive.detach Zip.close_out out_file in
        let metadata_file_path =
          Naming.cemented_blocks_metadata_file cemented_metadata_dir file
          |> Naming.file_path
        in
        let*! () = Lwt_unix.rename tmp_metadata_file_path metadata_file_path in
        return_unit
      in
      return (metadata_writer, metadata_finalizer)
    else return ((fun _ -> Lwt.return_unit), fun () -> return_unit)
  in
  let*! () =
    Lwt.finalize
      (fun () ->
        (* Blit the offset preamble *)
        let offsets_buffer = Bytes.create preamble_length in
        let*! () =
          Lwt_utils_unix.write_bytes
            ~pos:0
            ~len:preamble_length
            fd
            offsets_buffer
        in
        let first_offset = preamble_length in
        (* Cursor is now at the beginning of the element section *)
        let*! _ =
          Seq.ES.fold_left
            (fun (i, current_offset) block_read ->
              let* block_hash, total_block_length, block_bytes = block_read in
              let pruned_block_length =
                (* This call rewrites [block_bytes] to a pruned block
                   (with its size modified) *)
                Block_repr_unix.prune_raw_block_bytes block_bytes
              in
              (* We start by blitting the corresponding offset in the preamble part *)
              Bytes.set_int32_be
                offsets_buffer
                (i * offset_length)
                (Int32.of_int current_offset) ;
              (* We write the block in the file *)
              let*! () =
                Lwt_utils_unix.write_bytes
                  ~pos:0
                  ~len:pruned_block_length
                  fd
                  block_bytes
              in
              let block_level = Int32.(add first_block_level (of_int i)) in
              let* () =
                protect (fun () ->
                    if total_block_length > pruned_block_length then
                      (* Do not try to write to block's metadata if
                         there are none *)
                      let*! () =
                        metadata_writer
                          ( block_bytes,
                            total_block_length,
                            block_level,
                            pruned_block_length )
                      in
                      return_unit
                    else return_unit)
              in
              (* We also populate the indexes *)
              Cemented_block_level_index.replace
                cemented_store.cemented_block_level_index
                block_hash
                block_level ;
              Cemented_block_hash_index.replace
                cemented_store.cemented_block_hash_index
                block_level
                block_hash ;
              return (succ i, current_offset + pruned_block_length))
            (0, first_offset)
            reading_sequence
        in
        (* We now write the real offsets in the preamble *)
        let*! _ofs = Lwt_unix.lseek fd 0 Unix.SEEK_SET in
        Lwt_utils_unix.write_bytes ~pos:0 ~len:preamble_length fd offsets_buffer)
      (fun () ->
        let*! _ = Lwt_utils_unix.safe_close fd in
        Lwt.return_unit)
  in
  let*! () = Lwt_unix.rename tmp_file_path final_path in
  (* Flush the indexes to make sure that the data is stored on disk *)
  Cemented_block_level_index.flush
    ~with_fsync:true
    cemented_store.cemented_block_level_index ;
  Cemented_block_hash_index.flush
    ~with_fsync:true
    cemented_store.cemented_block_hash_index ;
  (* Update table *)
  let cemented_block_interval =
    {start_level = first_block_level; end_level = last_block_level; file}
  in
  let new_array =
    match cemented_store.cemented_blocks_files with
    | None -> [|cemented_block_interval|]
    | Some arr ->
        if not (Array.mem cemented_block_interval arr) then
          Array.append arr [|cemented_block_interval|]
        else arr
  in
  (* If the cementing is done arbitrarily, we need to make sure the
     files remain sorted. *)
  if not check_consistency then Array.sort compare_cemented_files new_array ;
  cemented_store.cemented_blocks_files <- Some new_array ;
  if write_metadata then metadata_finalizer () else return_unit

let trigger_full_gc cemented_store cemented_blocks_files offset =
  let open Lwt_syntax in
  let nb_files = Array.length cemented_blocks_files in
  if nb_files <= offset then Lwt.return_unit
  else
    let cemented_files = Array.to_list cemented_blocks_files in
    let files_to_remove, _files_to_keep =
      List.split_n (nb_files - offset) cemented_files
    in
    (* Remove the rest of the files to prune *)
    List.iter_s
      (fun {file; _} ->
        let metadata_file_path =
          Naming.(
            cemented_blocks_metadata_file
              (cemented_blocks_metadata_dir cemented_store.cemented_blocks_dir)
              file
            |> file_path)
        in
        let* () = Unit.catch_s (fun () -> Lwt_unix.unlink metadata_file_path) in
        (* Remove the metadata's fd from the cache *)
        Metadata_fd_cache.remove
          cemented_store.metadata_fd_cache
          metadata_file_path ;
        return_unit)
      files_to_remove

let trigger_rolling_gc cemented_store cemented_blocks_files offset =
  let open Lwt_syntax in
  let nb_files = Array.length cemented_blocks_files in
  if nb_files <= offset then Lwt.return_unit
  else
    let {end_level = last_level_to_purge; _} =
      cemented_blocks_files.(nb_files - offset - 1)
    in
    let cemented_files = Array.to_list cemented_blocks_files in
    (* Start by updating the indexes by filtering blocks that are
       below the offset *)
    Cemented_block_hash_index.filter
      cemented_store.cemented_block_hash_index
      (fun (level, _) -> Compare.Int32.(level > last_level_to_purge)) ;
    Cemented_block_level_index.filter
      cemented_store.cemented_block_level_index
      (fun (_, level) -> Compare.Int32.(level > last_level_to_purge)) ;
    let files_to_remove, _files_to_keep =
      List.split_n (nb_files - offset) cemented_files
    in
    (* Remove the rest of the files to prune *)
    List.iter_s
      (fun {file; _} ->
        let metadata_file_path =
          Naming.(
            cemented_blocks_metadata_file
              (cemented_blocks_metadata_dir cemented_store.cemented_blocks_dir)
              file
            |> file_path)
        in
        let* () = Unit.catch_s (fun () -> Lwt_unix.unlink metadata_file_path) in
        (* Remove the metadata's fd from the cache *)
        Metadata_fd_cache.remove
          cemented_store.metadata_fd_cache
          metadata_file_path ;
        let* () =
          Unit.catch_s (fun () -> Lwt_unix.unlink (Naming.file_path file))
        in
        return_unit)
      files_to_remove

let trigger_gc cemented_store history_mode =
  let open Lwt_syntax in
  let* () = Store_events.(emit start_store_garbage_collection) () in
  match cemented_store.cemented_blocks_files with
  | None -> return_unit
  | Some cemented_blocks_files -> (
      match history_mode with
      | History_mode.Archive -> Lwt.return_unit
      | Full offset ->
          let offset =
            (Option.value
               offset
               ~default:History_mode.default_additional_cycles)
              .offset
          in
          trigger_full_gc cemented_store cemented_blocks_files offset
      | Rolling offset ->
          let offset =
            (Option.value
               offset
               ~default:History_mode.default_additional_cycles)
              .offset
          in
          trigger_rolling_gc cemented_store cemented_blocks_files offset)

let raw_iter_cemented_file f ({file; _} as cemented_blocks_file) =
  let open Lwt_syntax in
  let file_path = Naming.file_path file in
  Lwt_io.with_file
    ~flags:[Unix.O_RDONLY; O_CLOEXEC]
    ~mode:Lwt_io.Input
    file_path
    (fun channel ->
      let nb_blocks = cemented_blocks_file_length cemented_blocks_file in
      let* first_block_offset = Lwt_io.BE.read_int channel in
      let* () = Lwt_io.set_position channel (Int64.of_int first_block_offset) in
      let rec loop n =
        if n = 0 then Lwt.return_unit
        else
          (* Read length *)
          let* length = Lwt_io.BE.read_int channel in
          let full_length = 4 (* int32 length *) + length in
          let block_bytes = Bytes.create full_length in
          let* () = Lwt_io.read_into_exactly channel block_bytes 4 length in
          Bytes.set_int32_be block_bytes 0 (Int32.of_int length) ;
          let* () =
            f
              (Data_encoding.Binary.of_bytes_exn
                 Block_repr.encoding
                 block_bytes)
          in
          loop (pred n)
      in
      loop (Int32.to_int nb_blocks))

let iter_cemented_file f ({file; _} as cemented_blocks_file) =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! () = raw_iter_cemented_file f cemented_blocks_file in
      return_unit)
    (fun exn ->
      Format.kasprintf
        (fun trace ->
          tzfail (Inconsistent_cemented_file (Naming.file_path file, trace)))
        "%s"
        (Printexc.to_string exn))

let check_indexes_consistency ?(post_step = fun () -> Lwt.return_unit)
    ?genesis_hash cemented_store =
  let open Lwt_result_syntax in
  match cemented_store.cemented_blocks_files with
  | None -> return_unit
  | Some table ->
      let len = Array.length table in
      let rec check_contiguity i =
        if i = len || i = len - 1 then return_unit
        else
          let* () =
            fail_unless
              Compare.Int32.(
                Int32.succ table.(i).end_level = table.(i + 1).start_level)
              (Inconsistent_cemented_store
                 (Missing_cycle
                    {
                      low_cycle = Naming.file_path table.(i).file;
                      high_cycle = Naming.file_path table.(i + 1).file;
                    }))
          in
          check_contiguity (succ i)
      in
      let* () = check_contiguity 0 in
      let table_list = Array.to_list table in
      let* () =
        List.iter_es
          (fun ({start_level = inf; file; _} as cemented_blocks_file) ->
            let*! fd =
              Lwt_unix.openfile
                (Naming.file_path file)
                [Unix.O_RDONLY; O_CLOEXEC]
                0o444
            in
            Lwt.finalize
              (fun () ->
                let nb_blocks =
                  Int32.to_int
                    (cemented_blocks_file_length cemented_blocks_file)
                in
                (* Load the offset region *)
                let len_offset = nb_blocks * offset_length in
                let bytes = Bytes.create len_offset in
                let*! () = Lwt_utils_unix.read_bytes ~len:len_offset fd bytes in
                let offsets =
                  Data_encoding.Binary.of_bytes_exn
                    Data_encoding.(Variable.array ~max_length:nb_blocks int31)
                    bytes
                in
                (* Cursor is now after the offset region *)
                let rec iter_blocks ?pred_block n =
                  if n = nb_blocks then return_unit
                  else
                    let*! cur_offset = Lwt_unix.lseek fd 0 Unix.SEEK_CUR in
                    let* () =
                      fail_unless
                        Compare.Int.(cur_offset = offsets.(n))
                        (Inconsistent_cemented_store
                           (Bad_offset
                              {level = n; cycle = Naming.file_path file}))
                    in
                    let*! block, _ = Block_repr_unix.read_next_block_exn fd in
                    let* () =
                      fail_unless
                        Compare.Int32.(
                          Block_repr.level block = Int32.(add inf (of_int n)))
                        (Inconsistent_cemented_store
                           (Unexpected_level
                              {
                                block_hash = Block_repr.hash block;
                                expected = Int32.(add inf (of_int n));
                                got = Block_repr.level block;
                              }))
                    in
                    let* () =
                      Block_repr.check_block_consistency
                        ?genesis_hash
                        ?pred_block
                        block
                    in
                    let level = Block_repr.level block in
                    let hash = Block_repr.hash block in
                    let* () =
                      fail_unless
                        (Cemented_block_level_index.mem
                           cemented_store.cemented_block_level_index
                           hash
                        && Cemented_block_hash_index.mem
                             cemented_store.cemented_block_hash_index
                             level)
                        (Inconsistent_cemented_store (Corrupted_index hash))
                    in
                    iter_blocks ~pred_block:block (succ n)
                in
                protect (fun () ->
                    let* () = iter_blocks 0 in
                    let*! () = post_step () in
                    return_unit))
              (fun () ->
                let*! _ = Lwt_utils_unix.safe_close fd in
                Lwt.return_unit))
          table_list
      in
      return_unit
