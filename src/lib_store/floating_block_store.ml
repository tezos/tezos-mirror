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

(* The log_size corresponds to the maximum size of the memory zone
   allocated in memory before flushing it onto the disk. It is
   basically a cache which is use for the index. The cache size is
   `log_size * log_entry` where a `log_entry` is roughly 56 bytes. *)
let floating_blocks_log_size = 10_000

open Floating_block_index.Block_info

let kind {kind; _} = kind

let mem floating_store hash =
  Lwt_idle_waiter.task floating_store.scheduler (fun () ->
      Lwt.return
        (Floating_block_index.mem floating_store.floating_block_index hash))

let find_predecessors floating_store hash =
  Lwt_idle_waiter.task floating_store.scheduler (fun () ->
      try
        let {predecessors; _} =
          Floating_block_index.find floating_store.floating_block_index hash
        in
        Lwt.return_some predecessors
      with Not_found -> Lwt.return_none)

let read_block_and_predecessors floating_store hash =
  Lwt_idle_waiter.task floating_store.scheduler (fun () ->
      Option.catch_os (fun () ->
          let {offset; predecessors} =
            Floating_block_index.find floating_store.floating_block_index hash
          in
          Block_repr.pread_block floating_store.fd ~file_offset:offset
          >>= function
          | Some (block, _) -> Lwt.return_some (block, predecessors)
          | None ->
              (* May be the case when a stored block is corrupted *)
              Lwt.return_none))

let read_block floating_store hash =
  read_block_and_predecessors floating_store hash >>= function
  | Some (block, _) -> Lwt.return_some block
  | None -> Lwt.return_none

let locked_write_block floating_store ~offset ~block ~predecessors =
  (match Data_encoding.Binary.to_bytes_opt Block_repr.encoding block with
  | None -> fail (Cannot_encode_block block.Block_repr.hash)
  | Some bytes -> return bytes)
  >>=? fun block_bytes ->
  let block_length = Bytes.length block_bytes in
  Lwt_utils_unix.write_bytes
    ~pos:0
    ~len:block_length
    floating_store.fd
    block_bytes
  >>= fun () ->
  Floating_block_index.replace
    floating_store.floating_block_index
    block.Block_repr.hash
    {offset; predecessors} ;
  return block_length

let append_block ?(flush = true) floating_store predecessors
    (block : Block_repr.t) =
  Lwt_idle_waiter.force_idle floating_store.scheduler (fun () ->
      Lwt_unix.lseek floating_store.fd 0 Unix.SEEK_END >>= fun offset ->
      locked_write_block floating_store ~offset ~block ~predecessors
      >>= fun _written_len ->
      if flush then
        Floating_block_index.flush floating_store.floating_block_index ;
      Lwt.return_unit)

let append_all floating_store
    (blocks : (Block_hash.t list * Block_repr.t) Seq.t) =
  Lwt_idle_waiter.force_idle floating_store.scheduler (fun () ->
      Lwt_unix.lseek floating_store.fd 0 Unix.SEEK_END >>= fun eof_offset ->
      Seq.fold_left_es
        (fun offset (predecessors, block) ->
          locked_write_block floating_store ~offset ~block ~predecessors
          >>=? fun written_len -> return (offset + written_len))
        eof_offset
        blocks
      >>=? fun _last_offset ->
      Floating_block_index.flush floating_store.floating_block_index ;
      return_unit)

let iter_s_raw_fd f fd =
  Lwt_unix.lseek fd 0 Unix.SEEK_END >>= fun eof_offset ->
  Lwt_unix.lseek fd 0 Unix.SEEK_SET >>= fun _file_start ->
  let rec loop nb_bytes_left =
    if nb_bytes_left = 0 then return_unit
    else
      Block_repr.read_next_block fd >>= function
      | None -> return_unit
      | Some (block, length) ->
          f block >>=? fun () -> loop (nb_bytes_left - length)
  in
  loop eof_offset

(* May raise [Not_found] if index does not contain the block. *)
let iter_with_pred_s_raw_fd f fd block_index =
  protect (fun () ->
      iter_s_raw_fd
        (fun block ->
          let {predecessors; _} =
            Floating_block_index.find block_index block.hash
          in
          f (block, predecessors))
        fd)

let folder f floating_store =
  Lwt_idle_waiter.task floating_store.scheduler (fun () ->
      (* We open a new fd *)
      let (flags, perms) = ([Unix.O_CREAT; O_RDONLY; O_CLOEXEC], 0o444) in
      let path =
        Naming.floating_blocks_file floating_store.floating_blocks_dir
        |> Naming.file_path
      in
      Lwt_unix.openfile path flags perms >>= fun fd ->
      Lwt.finalize
        (fun () -> f fd)
        (fun () ->
          Lwt_utils_unix.safe_close fd >>= fun _ignore -> Lwt.return_unit))

let fold_left_s f e floating_store =
  folder
    (fun fd ->
      let acc = ref e in
      iter_s_raw_fd
        (fun block ->
          f !acc block >>=? fun new_acc ->
          acc := new_acc ;
          return_unit)
        fd
      >>=? fun () -> return !acc)
    floating_store

let fold_left_with_pred_s f e floating_store =
  folder
    (fun fd ->
      let acc = ref e in
      iter_with_pred_s_raw_fd
        (fun (b, preds) ->
          f !acc (b, preds) >>=? fun new_acc ->
          acc := new_acc ;
          return_unit)
        fd
        floating_store.floating_block_index
      >>=? fun () -> return !acc)
    floating_store

(* Iter sequentially on every blocks in the file *)
let iter_s f floating_store = fold_left_s (fun () e -> f e) () floating_store

let iter_with_pred_s f floating_store =
  fold_left_with_pred_s (fun () e -> f e) () floating_store

let init chain_dir ~readonly kind =
  let (flag, perms) =
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
  (Lwt_unix.file_exists floating_blocks_dir_path >>= function
   | false -> Lwt_unix.mkdir floating_blocks_dir_path 0o777
   | true -> Lwt.return_unit)
  >>= fun () ->
  Lwt_unix.openfile
    (Naming.file_path floating_blocks_file)
    [Unix.O_CREAT; O_CLOEXEC; flag]
    perms
  >>= fun fd ->
  let floating_block_index =
    Floating_block_index.v
      ~log_size:floating_blocks_log_size
      ~readonly
      (Naming.dir_path floating_index_dir)
  in
  let scheduler = Lwt_idle_waiter.create () in
  Lwt.return {floating_block_index; fd; floating_blocks_dir; kind; scheduler}

let close {floating_block_index; fd; scheduler; _} =
  Lwt_idle_waiter.force_idle scheduler (fun () ->
      (try Floating_block_index.close floating_block_index
       with Index.Closed -> ()) ;
      Lwt_utils_unix.safe_close fd >>= fun _ignore -> Lwt.return_unit)

let append_floating_store ~from ~into =
  protect (fun () ->
      iter_with_pred_s
        (fun (block, preds) ->
          append_block ~flush:false into preds block >>= return)
        from
      >>=? fun () ->
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
  Lwt.catch
    (fun () ->
      all_files_exists chain_dir kind >>= function
      | false -> Lwt.return_false
      | true ->
          let rec loop index fd nb_bytes_left count =
            if nb_bytes_left = 0 then Lwt.return_false
            else
              Block_repr.read_next_block fd >>= function
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
          let (flag, perms) = (Unix.O_RDWR, 0o644) in
          let floating_blocks_dir = Naming.floating_blocks_dir chain_dir kind in
          let floating_blocks_file_path =
            Naming.floating_blocks_file floating_blocks_dir |> Naming.file_path
          in
          let floating_blocks_index_dir_path =
            Naming.floating_blocks_index_dir floating_blocks_dir
            |> Naming.dir_path
          in
          Lwt_unix.openfile
            floating_blocks_file_path
            [Unix.O_CREAT; O_CLOEXEC; flag]
            perms
          >>= fun fd ->
          let index =
            Floating_block_index.v
              ~log_size:floating_blocks_log_size
              ~readonly:false
              floating_blocks_index_dir_path
          in
          Lwt_unix.lseek fd 0 Unix.SEEK_END >>= fun eof_offset ->
          Lwt_unix.lseek fd 0 Unix.SEEK_SET >>= fun _file_start ->
          Lwt.finalize
            (fun () -> loop index fd eof_offset 0)
            (fun () ->
              Lwt_unix.close fd >>= fun () ->
              Floating_block_index.close index ;
              Lwt.return_unit))
    (function _exn -> Lwt.return_false)

let delete_files floating_store =
  Unit.catch_s (fun () ->
      close floating_store >>= fun () ->
      let floating_store_dir_path =
        Naming.dir_path floating_store.floating_blocks_dir
      in
      Lwt_utils_unix.remove_dir floating_store_dir_path)

let swap ~src ~dst =
  close src >>= fun () ->
  close dst >>= fun () ->
  let dst_floating_store_dir_path = Naming.dir_path dst.floating_blocks_dir in
  let src_floating_store_dir_path = Naming.dir_path src.floating_blocks_dir in
  delete_files dst >>= fun () ->
  (* Replace dst's directory by src's directory *)
  Lwt_unix.rename src_floating_store_dir_path dst_floating_store_dir_path
  >>= fun () -> Lwt.return_unit

(* Call this function when full_integrity_check has failed. *)
let fix_integrity chain_dir kind =
  full_integrity_check chain_dir kind >>= function
  | true -> (* Nothing to do *) return_unit
  | false ->
      protect (fun () ->
          (* Both errors implies to reconstruct a proper floating store from
             scratch. *)
          init chain_dir ~readonly:true kind
          >>= fun inconsistent_floating_store ->
          let restore_kind = Restore kind in
          Lwt_utils_unix.remove_dir
            (Naming.floating_blocks_dir chain_dir restore_kind
            |> Naming.dir_path)
          >>= fun () ->
          init chain_dir ~readonly:false restore_kind
          >>= fun fresh_floating_store ->
          Lwt.finalize
            (fun () ->
              Lwt.catch
                (fun () ->
                  (* This [iter_s] stops reading whenever a block
                      cannot be read. *)
                  iter_s
                    (fun block ->
                      find_predecessors
                        inconsistent_floating_store
                        (Block_repr.hash block)
                      >>= function
                      | Some preds ->
                          (* TODO: should we retrieve info ? e.g. highest_level, highest_fitness ? *)
                          append_block fresh_floating_store preds block
                          >>= return
                      | None -> Lwt.fail Exit)
                    inconsistent_floating_store)
                (function Exit -> return_unit | exn -> Lwt.fail exn)
              >>=? fun () ->
              swap ~src:fresh_floating_store ~dst:inconsistent_floating_store
              >>= fun () -> return_unit)
            (fun () ->
              close inconsistent_floating_store >>= fun () ->
              close fresh_floating_store >>= fun () ->
              delete_files fresh_floating_store))
