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
open Naming

type floating_kind = Naming.floating_kind =
  | RO
  | RW
  | RW_TMP
  | RO_TMP
  | Restore of floating_kind

type t = {
  floating_block_index : Floating_block_index.t;
  floating_blocks_dir : [`Floating_dir] directory;
  fd : Lwt_unix.file_descr;
  kind : floating_kind;
  scheduler : Lwt_idle_waiter.t;
}

type info = {
  predecessors : Block_hash.t list;
  resulting_context_hash : Context_hash.t;
}

(* The log_size corresponds to the maximum size of the memory zone
   allocated in memory before flushing it onto the disk. It is
   basically a cache which is use for the index. The cache size is
   `log_size * log_entry` where a `log_entry` is roughly 56 bytes. *)
let default_floating_blocks_log_size = 10_000

let default_block_buffer_size = 500_000

open Floating_block_index.Block_info

let kind {kind; _} = kind

let mem floating_store hash =
  Lwt_idle_waiter.task floating_store.scheduler (fun () ->
      Lwt.return
        (Floating_block_index.mem floating_store.floating_block_index hash))

let find_info floating_store hash =
  Lwt_idle_waiter.task floating_store.scheduler (fun () ->
      try
        let {predecessors; resulting_context_hash; _} =
          Floating_block_index.find floating_store.floating_block_index hash
        in
        Lwt.return_some {predecessors; resulting_context_hash}
      with Not_found -> Lwt.return_none)

let ( let*? ) t k =
  let open Lwt_syntax in
  let* v_opt = t in
  match v_opt with None -> return_none | Some v -> k v

let find_predecessors floating_store hash =
  let*? {predecessors; _} = find_info floating_store hash in
  Lwt.return_some predecessors

let find_resulting_context_hash floating_store hash =
  let*? {resulting_context_hash; _} = find_info floating_store hash in
  Lwt.return_some resulting_context_hash

let read_block_and_info floating_store hash =
  Lwt_idle_waiter.task floating_store.scheduler (fun () ->
      Option.catch_os (fun () ->
          let {offset; predecessors; resulting_context_hash} =
            Floating_block_index.find floating_store.floating_block_index hash
          in
          let*? block, _ =
            Block_repr_unix.pread_block floating_store.fd ~file_offset:offset
          in
          Lwt.return_some (block, {predecessors; resulting_context_hash})))

let read_block floating_store hash =
  let open Lwt_syntax in
  let*? block, _ = read_block_and_info floating_store hash in
  return_some block

let locked_write_block floating_store ~offset ~block ~predecessors
    ~resulting_context_hash =
  let open Lwt_result_syntax in
  let* block_bytes =
    match Data_encoding.Binary.to_bytes_opt Block_repr.encoding block with
    | None -> tzfail (Cannot_encode_block block.Block_repr.hash)
    | Some bytes -> return bytes
  in
  let block_length = Bytes.length block_bytes in
  let*! () =
    Lwt_utils_unix.write_bytes
      ~pos:0
      ~len:block_length
      floating_store.fd
      block_bytes
  in
  Floating_block_index.replace
    floating_store.floating_block_index
    block.Block_repr.hash
    {offset; predecessors; resulting_context_hash} ;
  return block_length

let append_block floating_store ?(flush = true) ?(log_metrics = false)
    ({predecessors; resulting_context_hash} : info) (block : Block_repr.t) =
  let open Lwt_result_syntax in
  Lwt_idle_waiter.force_idle floating_store.scheduler (fun () ->
      let*! offset = Lwt_unix.lseek floating_store.fd 0 Unix.SEEK_END in
      let* written_len =
        locked_write_block
          floating_store
          ~offset
          ~block
          ~predecessors
          ~resulting_context_hash
      in
      if flush then
        Floating_block_index.flush floating_store.floating_block_index ;
      if log_metrics then
        Prometheus.Gauge.set
          Store_metrics.metrics.last_written_block_size
          (Int.to_float written_len) ;
      return_unit)

let append_all floating_store (blocks : (Block_repr.t * info) Seq.t) =
  let open Lwt_result_syntax in
  Lwt_idle_waiter.force_idle floating_store.scheduler (fun () ->
      let*! eof_offset = Lwt_unix.lseek floating_store.fd 0 Unix.SEEK_END in
      let* _last_offset =
        Seq.ES.fold_left
          (fun offset (block, ({predecessors; resulting_context_hash} : info)) ->
            let* written_len =
              locked_write_block
                floating_store
                ~offset
                ~block
                ~predecessors
                ~resulting_context_hash
            in
            return (offset + written_len))
          eof_offset
          blocks
      in
      Floating_block_index.flush floating_store.floating_block_index ;
      return_unit)

let iter_s_raw_fd f fd =
  let open Lwt_result_syntax in
  let*! eof_offset = Lwt_unix.lseek fd 0 Unix.SEEK_END in
  let*! _file_start = Lwt_unix.lseek fd 0 Unix.SEEK_SET in
  let rec loop nb_bytes_left =
    if nb_bytes_left = 0 then return_unit
    else
      let*! o = Block_repr_unix.read_next_block fd in
      match o with
      | None -> return_unit
      | Some (block, length) ->
          let* () = f block in
          loop (nb_bytes_left - length)
  in
  loop eof_offset

(* May raise [Not_found] if index does not contain the block. *)
let iter_with_info_s_raw_fd f fd block_index =
  protect (fun () ->
      iter_s_raw_fd
        (fun block ->
          let {predecessors; resulting_context_hash; _} =
            Floating_block_index.find block_index block.hash
          in
          f (block, {predecessors; resulting_context_hash}))
        fd)

let folder f floating_store =
  let open Lwt_syntax in
  Lwt_idle_waiter.task floating_store.scheduler (fun () ->
      (* We open a new fd *)
      let flags, perms = ([Unix.O_CREAT; O_RDONLY; O_CLOEXEC], 0o444) in
      let path =
        Naming.floating_blocks_file floating_store.floating_blocks_dir
        |> Naming.file_path
      in
      let* fd = Lwt_unix.openfile path flags perms in
      Lwt.finalize
        (fun () -> f fd)
        (fun () ->
          let* _ignore = Lwt_utils_unix.safe_close fd in
          Lwt.return_unit))

let fold_left_s f e floating_store =
  let open Lwt_result_syntax in
  folder
    (fun fd ->
      let acc = ref e in
      let* () =
        iter_s_raw_fd
          (fun block ->
            let* new_acc = f !acc block in
            acc := new_acc ;
            return_unit)
          fd
      in
      return !acc)
    floating_store

let fold_left_with_info_s f e floating_store =
  let open Lwt_result_syntax in
  folder
    (fun fd ->
      let acc = ref e in
      let* () =
        iter_with_info_s_raw_fd
          (fun (b, preds) ->
            let* new_acc = f !acc (b, preds) in
            acc := new_acc ;
            return_unit)
          fd
          floating_store.floating_block_index
      in
      return !acc)
    floating_store

(* Iter sequentially on every blocks in the file *)
let iter_s f floating_store = fold_left_s (fun () e -> f e) () floating_store

let iter_with_info_s f floating_store =
  fold_left_with_info_s (fun () e -> f e) () floating_store

let retrieve_block_from_stores floating_stores block_hash =
  let open Lwt_result_syntax in
  List.find_map
    (fun floating_store ->
      try
        Some
          ( Floating_block_index.find
              floating_store.floating_block_index
              block_hash,
            floating_store )
      with _ -> None)
    floating_stores
  |> function
  | None ->
      tzfail (Store_errors.Block_not_found {hash = block_hash; distance = 0})
  | Some x -> return x

let raw_copy_block ~block_buffer ~src_floating_stores ~block_hash
    ~dst_floating_store =
  let open Lwt_result_syntax in
  protect (fun () ->
      let* {offset; predecessors; resulting_context_hash}, src_floating_store =
        retrieve_block_from_stores src_floating_stores block_hash
      in
      let length_size = 4 in
      let*! () =
        Lwt_utils_unix.read_bytes
          ~file_offset:offset (* = pread *)
          ~pos:0
          ~len:length_size
          src_floating_store.fd
          !block_buffer
      in
      let block_length = Bytes.get_int32_be !block_buffer 0 |> Int32.to_int in
      let buffer_length = Bytes.length !block_buffer in
      let required_length = block_length + length_size in
      (* Resize the buffer if needs be *)
      if buffer_length < required_length then
        block_buffer := Bytes.create required_length ;
      let*! () =
        Lwt_utils_unix.read_bytes
          ~file_offset:offset (* = pread *)
          ~pos:0
          ~len:required_length
          src_floating_store.fd
          !block_buffer
      in
      let*! new_offset = Lwt_unix.lseek dst_floating_store.fd 0 Unix.SEEK_END in
      let*! () =
        Lwt_utils_unix.write_bytes
          ~pos:0
          ~len:required_length
          dst_floating_store.fd
          !block_buffer
      in
      Floating_block_index.replace
        dst_floating_store.floating_block_index
        block_hash
        {offset = new_offset; predecessors; resulting_context_hash} ;
      return_unit)

let raw_copy_all ~src_floating_stores ~block_hashes ~dst_floating_store =
  Lwt_idle_waiter.force_idle dst_floating_store.scheduler (fun () ->
      let block_buffer = ref (Bytes.create default_block_buffer_size) in
      List.iter_es
        (fun block_hash ->
          raw_copy_block
            ~block_buffer
            ~src_floating_stores
            ~block_hash
            ~dst_floating_store)
        block_hashes)

let raw_retrieve_blocks_seq ~src_floating_stores ~block_hashes =
  let open Lwt_result_syntax in
  let block_buffer = ref (Bytes.create default_block_buffer_size) in
  List.to_seq block_hashes
  |> Seq.map (fun block_hash ->
         protect (fun () ->
             let* {offset; _}, src_floating_store =
               retrieve_block_from_stores src_floating_stores block_hash
             in
             let length_size = 4 in
             let*! () =
               Lwt_utils_unix.read_bytes
                 ~file_offset:offset (* = pread *)
                 ~pos:0
                 ~len:length_size
                 src_floating_store.fd
                 !block_buffer
             in
             let block_length =
               Bytes.get_int32_be !block_buffer 0 |> Int32.to_int
             in
             let buffer_length = Bytes.length !block_buffer in
             let required_length = block_length + length_size in
             (* Resize the buffer if needs be *)
             if buffer_length < required_length then
               block_buffer := Bytes.create required_length ;
             let*! () =
               Lwt_utils_unix.read_bytes
                 ~file_offset:offset (* = pread *)
                 ~pos:0
                 ~len:required_length
                 src_floating_store.fd
                 !block_buffer
             in
             return (block_hash, required_length, !block_buffer)))

let raw_iterate f floating_store =
  let open Lwt_result_syntax in
  let block_buffer = ref (Bytes.create default_block_buffer_size) in
  let block_size_buffer = Bytes.create 4 in
  let iterate fd =
    let*! end_of_file_offset = Lwt_unix.lseek fd 0 Unix.SEEK_END in
    let rec loop current_offset =
      if current_offset = end_of_file_offset then return_unit
      else
        let*! () =
          Lwt_utils_unix.read_bytes
            ~file_offset:current_offset
            ~pos:0
            ~len:4
            fd
            block_size_buffer
        in
        let block_length =
          Bytes.get_int32_be block_size_buffer 0 |> Int32.to_int
        in
        let required_length = block_length + 4 in
        let buffer_length = Bytes.length !block_buffer in
        if buffer_length < required_length then
          block_buffer := Bytes.create required_length ;
        let*! () =
          Lwt_utils_unix.read_bytes
            ~file_offset:current_offset
            ~pos:0
            ~len:required_length
            fd
            !block_buffer
        in
        let* () = f (!block_buffer, required_length) in
        loop (current_offset + required_length)
    in
    loop 0
  in
  protect (fun () -> folder iterate floating_store)

let raw_iterate_fd f fd =
  let open Lwt_result_syntax in
  let block_buffer = ref (Bytes.create default_block_buffer_size) in
  let block_size_buffer = Bytes.create 4 in
  let iterate fd =
    let*! end_of_file_offset = Lwt_unix.lseek fd 0 Unix.SEEK_END in
    let rec loop current_offset =
      if current_offset = end_of_file_offset then return_unit
      else
        let*! () =
          Lwt_utils_unix.read_bytes
            ~file_offset:current_offset
            ~pos:0
            ~len:4
            fd
            block_size_buffer
        in
        let block_length =
          Bytes.get_int32_be block_size_buffer 0 |> Int32.to_int
        in
        let required_length = block_length + 4 in
        let buffer_length = Bytes.length !block_buffer in
        if buffer_length < required_length then
          block_buffer := Bytes.create required_length ;
        let*! () =
          Lwt_utils_unix.read_bytes
            ~file_offset:current_offset
            ~pos:0
            ~len:required_length
            fd
            !block_buffer
        in
        let* () = f (!block_buffer, required_length) in
        loop (current_offset + required_length)
    in
    loop 0
  in
  protect (fun () -> iterate fd)

let raw_append floating_store
    ( block_hash,
      block_bytes,
      required_length,
      predecessors,
      resulting_context_hash ) =
  let open Lwt_result_syntax in
  protect @@ fun () ->
  let*! new_offset = Lwt_unix.lseek floating_store.fd 0 Unix.SEEK_END in
  let*! () =
    Lwt_utils_unix.write_bytes
      ~pos:0
      ~len:required_length
      floating_store.fd
      block_bytes
  in
  Floating_block_index.replace
    floating_store.floating_block_index
    block_hash
    {offset = new_offset; predecessors; resulting_context_hash} ;
  return_unit

let init chain_dir ~readonly kind =
  let open Lwt_syntax in
  let flag, perms =
    (* Only RO is readonly: when we open RO_TMP, we actually write in it. *)
    if kind = Naming.RO && readonly then (Unix.O_RDONLY, 0o444)
    else (Unix.O_RDWR, 0o644)
  in
  let floating_blocks_dir = Naming.floating_blocks_dir chain_dir kind in
  let floating_blocks_dir_path = Naming.dir_path floating_blocks_dir in
  let floating_blocks_file = Naming.floating_blocks_file floating_blocks_dir in
  let floating_index_dir =
    Naming.floating_blocks_index_dir floating_blocks_dir
  in
  (* Create the floating store directory if it does not yet exist *)
  let* () =
    let* b = Lwt_unix.file_exists floating_blocks_dir_path in
    match b with
    | false -> Lwt_unix.mkdir floating_blocks_dir_path 0o777
    | true -> Lwt.return_unit
  in
  let* fd =
    Lwt_unix.openfile
      (Naming.file_path floating_blocks_file)
      [Unix.O_CREAT; O_CLOEXEC; flag]
      perms
  in
  let floating_block_index =
    Floating_block_index.v
      ~log_size:default_floating_blocks_log_size
      ~readonly
      (Naming.dir_path floating_index_dir)
  in
  let scheduler = Lwt_idle_waiter.create () in
  return {floating_block_index; fd; floating_blocks_dir; kind; scheduler}

let close {floating_block_index; fd; scheduler; _} =
  let open Lwt_syntax in
  Lwt_idle_waiter.force_idle scheduler (fun () ->
      (try Floating_block_index.close floating_block_index
       with Index.Closed -> ()) ;
      let* _ignore = Lwt_utils_unix.safe_close fd in
      Lwt.return_unit)

let append_floating_store ~from ~into =
  let open Lwt_result_syntax in
  protect (fun () ->
      let* () =
        iter_with_info_s
          (fun (block, info) -> append_block ~flush:false into info block)
          from
      in
      Floating_block_index.flush ~with_fsync:true into.floating_block_index ;
      return_unit)

let all_files_exists chain_dir kind =
  let floating_blocks_dir = Naming.floating_blocks_dir chain_dir kind in
  let floating_blocks_dir_path = Naming.dir_path floating_blocks_dir in
  let floating_blocks_file_path =
    Naming.floating_blocks_file floating_blocks_dir |> Naming.file_path
  in
  let floating_blocks_index_dir_path =
    Naming.floating_blocks_index_dir floating_blocks_dir |> Naming.dir_path
  in
  Lwt_list.for_all_s
    Lwt_unix.file_exists
    [
      floating_blocks_dir_path;
      floating_blocks_file_path;
      floating_blocks_index_dir_path;
    ]

(** [full_integrity_check ~chain_dir kind] performs a full read of the
    floating store [kind] in [chain_dir] and returns [false] if the
    file is inconsistent. *)
let full_integrity_check chain_dir kind =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let* b = all_files_exists chain_dir kind in
      match b with
      | false -> Lwt.return_false
      | true ->
          let rec loop index fd nb_bytes_left count =
            if nb_bytes_left = 0 then Lwt.return_false
            else
              let* o = Block_repr_unix.read_next_block fd in
              match o with
              | None ->
                  (* Returns None if the next block could not be
                     read. Might have some corrupted data. *)
                  Lwt.return_false
              | Some (block, length) ->
                  (* For each block read from the file, we check that it
                     is correctly indexed. *)
                  let h = Block_repr.(hash block) in
                  if Floating_block_index.mem index h then
                    loop index fd (nb_bytes_left - length) (succ count)
                  else Lwt.return_false
          in
          let flag, perms = (Unix.O_RDWR, 0o644) in
          let floating_blocks_dir = Naming.floating_blocks_dir chain_dir kind in
          let floating_blocks_file_path =
            Naming.floating_blocks_file floating_blocks_dir |> Naming.file_path
          in
          let floating_blocks_index_dir_path =
            Naming.floating_blocks_index_dir floating_blocks_dir
            |> Naming.dir_path
          in
          let* fd =
            Lwt_unix.openfile
              floating_blocks_file_path
              [Unix.O_CREAT; O_CLOEXEC; flag]
              perms
          in
          let index =
            Floating_block_index.v
              ~log_size:default_floating_blocks_log_size
              ~readonly:false
              floating_blocks_index_dir_path
          in
          let* eof_offset = Lwt_unix.lseek fd 0 Unix.SEEK_END in
          let* _file_start = Lwt_unix.lseek fd 0 Unix.SEEK_SET in
          Lwt.finalize
            (fun () -> loop index fd eof_offset 0)
            (fun () ->
              let* () = Lwt_unix.close fd in
              Floating_block_index.close index ;
              Lwt.return_unit))
    (function _exn -> Lwt.return_false)

let delete_files floating_store =
  let open Lwt_syntax in
  Unit.catch_s (fun () ->
      let* () = close floating_store in
      let floating_store_dir_path =
        Naming.dir_path floating_store.floating_blocks_dir
      in
      Lwt_utils_unix.remove_dir floating_store_dir_path)

let swap ~src ~dst =
  let open Lwt_syntax in
  let* () = close src in
  let* () = close dst in
  let dst_floating_store_dir_path = Naming.dir_path dst.floating_blocks_dir in
  let src_floating_store_dir_path = Naming.dir_path src.floating_blocks_dir in
  let* () = delete_files dst in
  (* Replace dst's directory by src's directory *)
  let* () =
    Lwt_unix.rename src_floating_store_dir_path dst_floating_store_dir_path
  in
  Lwt.return_unit

(* Call this function when full_integrity_check has failed. *)
let fix_integrity chain_dir kind =
  let open Lwt_result_syntax in
  let*! b = full_integrity_check chain_dir kind in
  match b with
  | true -> (* Nothing to do *) return_unit
  | false ->
      protect (fun () ->
          (* Both errors implies to reconstruct a proper floating store from
             scratch. *)
          let*! inconsistent_floating_store =
            init chain_dir ~readonly:true kind
          in
          let restore_kind = Restore kind in
          let*! () =
            Lwt_utils_unix.remove_dir
              (Naming.floating_blocks_dir chain_dir restore_kind
              |> Naming.dir_path)
          in
          let*! fresh_floating_store =
            init chain_dir ~readonly:false restore_kind
          in
          Lwt.finalize
            (fun () ->
              let* () =
                Lwt.catch
                  (fun () ->
                    (* This [iter_s] stops reading whenever a block
                        cannot be read. *)
                    iter_s
                      (fun block ->
                        let*! o =
                          find_info
                            inconsistent_floating_store
                            (Block_repr.hash block)
                        in
                        match o with
                        | Some preds ->
                            (* TODO: should we retrieve info ? e.g. highest_level, highest_fitness ? *)
                            append_block fresh_floating_store preds block
                        | None -> Lwt.fail Exit)
                      inconsistent_floating_store)
                  (function Exit -> return_unit | exn -> Lwt.fail exn)
              in
              let*! () =
                swap ~src:fresh_floating_store ~dst:inconsistent_floating_store
              in
              return_unit)
            (fun () ->
              let*! () = close inconsistent_floating_store in
              let*! () = close fresh_floating_store in
              delete_files fresh_floating_store))
