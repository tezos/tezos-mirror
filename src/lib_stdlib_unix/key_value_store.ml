(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Error_monad

type error += Missing_stored_kvs_data of string * int

let () =
  register_error_kind
    `Permanent
    ~id:"stdlib_unix.missing_kvs_data"
    ~title:"Missing stored data from KVS"
    ~description:"Failed to load stored data from KVS"
    ~pp:(fun ppf (path, index) ->
      Format.fprintf
        ppf
        "Failed to load on-disk data: no corresponding data found in file %s \
         at index %d."
        path
        index)
    Data_encoding.(obj2 (req "path" string) (req "index" int31))
    (function
      | Missing_stored_kvs_data (path, index) -> Some (path, index) | _ -> None)
    (fun (path, index) -> Missing_stored_kvs_data (path, index))

type ('file, 'value) directory_spec = {
  encoding : 'value Data_encoding.t;
  eq : 'value -> 'value -> bool;
  index_of : 'file -> int;
  path : string;
  value_size : int;
}

(** [Directories] handle writing and reading virtual files to virtual directories.
    A virtual directory is backed by a physical file and a virtual file is an offset
    in a virtual directory.

    Besides implementing a key-value store, the module [Directories] must properly
    handle resource utilization, especially file descriptors.

    The structure [Directories.t] guarantees that no more than the specified
    [lru_size] file descriptors can be open at the same time.
*)
module Directories : sig
  type 'value t

  val init : lru_size:int -> 'value t

  val close : 'value t -> unit Lwt.t

  val write :
    ?override:bool ->
    'value t ->
    ('b, 'value) directory_spec ->
    'b ->
    'value ->
    unit tzresult Lwt.t

  val read :
    'value t -> ('b, 'value) directory_spec -> 'b -> 'value tzresult Lwt.t

  val value_exists : 'value t -> ('b, 'value) directory_spec -> 'b -> bool Lwt.t
end = struct
  module LRU = Ringo.LRU_Collection

  module Table = Hashtbl.Make (struct
    include String

    let hash = Hashtbl.hash
  end)

  module File_table = Hashtbl.Make (struct
    type t = int

    let equal = Int.equal

    let hash = Hashtbl.hash
  end)

  let max_number_of_files = 4096

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6033
     For now the bitset is a byte set...
     With a true bitset, we'd have [max_number_of_files/8] *)
  let bitset_size = max_number_of_files

  type handle = {fd : Lwt_unix.file_descr; bitset : Lwt_bytes.t}

  let file_exists handle index = handle.bitset.{index} <> '\000'

  let set_file_exists handle index = handle.bitset.{index} <- '\001'

  let initialize_virtual_directory path value_size =
    (* We perform the initialization synchronously to avoid spurious Lwt
       premption slowing down writing shards. The execution time of the
       code below should be on the order of a few tenth of a millisecond
       on a Linux system. *)
    let fd = Unix.openfile path [O_RDWR; O_CREAT; O_EXCL; O_CLOEXEC] 0o660 in
    let total_size = bitset_size + (max_number_of_files * value_size) in
    try
      Unix.ftruncate fd total_size ;
      let bitset = Lwt_bytes.map_file ~fd ~shared:true ~size:bitset_size () in
      let fd = Lwt_unix.of_unix_file_descr ~blocking:true fd in
      Lwt.return {fd; bitset}
    with Unix.Unix_error _ as e ->
      Unix.unlink path ;
      raise e

  let load_virtual_directory path =
    let open Lwt_syntax in
    let* fd = Lwt_unix.openfile path [O_RDWR; O_CLOEXEC] 0o660 in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6033
       Should we check that the file is at least as big as the bitset? *)
    let bitset =
      Lwt_bytes.map_file
        ~fd:(Lwt_unix.unix_file_descr fd)
        ~shared:true
        ~size:bitset_size
        ()
    in
    return {fd; bitset}

  let close_virtual_directory handle = Lwt_unix.close handle.fd

  type 'value handle_and_pending_callbacks =
    | Entry of {
        handle : handle Lwt.t;
        accessed : Lwt_mutex.t File_table.t;
        cached : 'value File_table.t;
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6033
           Should we use a weak table to automatically collect dangling promises?
           Note that we do clear resolved promises each time we grow this list. *)
        mutable pending_callbacks : unit Lwt.t list;
      }
    | Being_evicted of unit Lwt.t

  let keep_pending l =
    List.filter
      (fun p ->
        match Lwt.state p with Return () | Fail _ -> false | Sleep -> true)
      l

  (* The type of directories.
     The domains of [handles] and [lru] should be the same, before and after
     calling the functions [write] and [read] in this module.
  *)
  type 'value t = {
    handles : 'value handle_and_pending_callbacks Table.t;
    lru : string LRU.t;
  }

  let init ~lru_size =
    let handles = Table.create 101 in
    let lru = LRU.create lru_size in
    {handles; lru}

  let close {handles; _} =
    let open Lwt_syntax in
    Table.iter_p
      (fun _ entry ->
        match entry with
        | Being_evicted p -> p
        | Entry {handle; pending_callbacks = _; accessed = _; cached = _} ->
            (* TODO https://gitlab.com/tezos/tezos/-/issues/6033
               Should we lock access to [accessed]; then lock on
               all mutex in [accessed], then close? This would ensure that we wait until
               all pending callbacks terminate. *)
            let* handle in
            let* () = Lwt_unix.fsync handle.fd in
            Lwt_unix.close handle.fd)
      handles

  let resolve_pending_and_close dirs removed =
    let open Lwt_syntax in
    let await_close, resolve_close = Lwt.task () in
    match Table.find dirs.handles removed with
    | None -> assert false
    | Some (Being_evicted _) -> assert false
    | Some (Entry {handle; accessed = _; cached = _; pending_callbacks}) ->
        Table.replace dirs.handles removed (Being_evicted await_close) ;
        let* handle and* () = Lwt.join pending_callbacks in
        let+ () = close_virtual_directory handle in
        Table.remove dirs.handles removed ;
        Lwt.wakeup resolve_close () ;
        ()

  let with_mutex accessed file f =
    match File_table.find accessed file with
    | None ->
        let mutex = Lwt_mutex.create () in
        File_table.add accessed file mutex ;
        Lwt_mutex.with_lock mutex f
    | Some mutex -> Lwt_mutex.with_lock mutex f

  let rec bind_dir_and_lock_file dirs spec index f =
    (* Precondition: the LRU and the table are in sync *)
    let open Lwt_syntax in
    let load_or_initialize () =
      let* b = Lwt_unix.file_exists spec.path in
      if b then load_virtual_directory spec.path
      else initialize_virtual_directory spec.path spec.value_size
    in

    let put_then_bind () =
      (* Precondition: [spec.path] not in [dirs.handle] *)
      let _node, erased_opt = LRU.add_and_return_erased dirs.lru spec.path in
      (* Here, [spec.path] is in the LRU but not in the table yet.
         But:
         - all executions from this point are cooperation-point-free
           until the insertion of [spec.path] in the table
         It follows that this temporary discrepancy is not observable.

         Same observation holds in the other direction if [erased_opt = Some erased].
      *)
      let handle =
        match erased_opt with
        | None -> load_or_initialize ()
        | Some removed ->
            let* () = resolve_pending_and_close dirs removed in
            load_or_initialize ()
      in
      let accessed = File_table.create 3 in
      let cached = File_table.create 3 in
      let callback =
        with_mutex accessed index (fun () -> Lwt.bind handle (f cached))
      in
      Table.replace
        dirs.handles
        spec.path
        (Entry
           {
             handle;
             accessed;
             cached;
             pending_callbacks = [Lwt.map ignore callback];
           }) ;
      callback
    in

    match Table.find dirs.handles spec.path with
    | Some (Entry p) ->
        let promise =
          with_mutex p.accessed index (fun () -> Lwt.bind p.handle (f p.cached))
        in
        p.pending_callbacks <-
          keep_pending (Lwt.map ignore promise :: p.pending_callbacks) ;
        promise
    | Some (Being_evicted await_eviction) ->
        let* () = await_eviction in
        (* We can't directly [put_and_bind] because several threads may be
           waiting here. *)
        bind_dir_and_lock_file dirs spec index f
    | None -> put_then_bind ()

  let write ?(override = false) dirs spec file data =
    let open Lwt_result_syntax in
    let index = spec.index_of file in
    bind_dir_and_lock_file dirs spec index @@ fun cached handle ->
    let perform_write () =
      let pos = Int64.of_int (bitset_size + (index * spec.value_size)) in
      let mmap =
        Lwt_bytes.map_file
          ~fd:(Lwt_unix.unix_file_descr handle.fd)
          ~pos
          ~size:spec.value_size
          ~shared:true
          ()
      in
      let bytes = Data_encoding.Binary.to_bytes_exn spec.encoding data in
      if Bytes.length bytes <> spec.value_size then
        failwith
          "Key_value_store.write: encoded value does not respect specified size"
      else (
        Lwt_bytes.blit_from_bytes bytes 0 mmap 0 (Bytes.length bytes) ;
        set_file_exists handle index ;
        return_unit)
    in
    if not (file_exists handle index) then (
      assert (not (File_table.mem cached index)) ;
      perform_write ())
    else if override then
      match File_table.find cached index with
      | None ->
          File_table.add cached index data ;
          perform_write ()
      | Some cached ->
          if spec.eq cached data then return_unit else perform_write ()
    else return_unit

  let read dirs spec file =
    let open Lwt_result_syntax in
    let index = spec.index_of file in
    bind_dir_and_lock_file dirs spec index @@ fun cached handle ->
    if file_exists handle index then
      match File_table.find cached index with
      | None ->
          (* Note that the following code executes atomically Lwt-wise. *)
          let pos = Int64.of_int (bitset_size + (index * spec.value_size)) in
          let mmap =
            Lwt_bytes.map_file
              ~fd:(Lwt_unix.unix_file_descr handle.fd)
              ~pos
              ~size:spec.value_size
              ~shared:true
              ()
          in
          let bytes = Bytes.make spec.value_size '\000' in
          Lwt_bytes.blit_to_bytes mmap 0 bytes 0 spec.value_size ;
          let data = Data_encoding.Binary.of_bytes_exn spec.encoding bytes in
          File_table.add cached index data ;
          return data
      | Some v -> return v
    else tzfail (Missing_stored_kvs_data (spec.path, index))

  let value_exists dirs spec file =
    let open Lwt_syntax in
    let index = spec.index_of file in
    bind_dir_and_lock_file dirs spec index @@ fun _cached handle ->
    return @@ file_exists handle index
end

type ('dir, 'file, 'value) t =
  | E : {
      directory_of : 'dir -> ('file, 'value) directory_spec;
      directories : 'value Directories.t;
    }
      -> ('dir, 'file, 'value) t

let directory ?encoded_value_size encoding path eq index_of =
  match encoded_value_size with
  | Some value_size -> {path; eq; encoding; index_of; value_size}
  | None -> (
      match Data_encoding.classify encoding with
      | `Fixed value_size -> {path; eq; encoding; index_of; value_size}
      | `Dynamic | `Variable ->
          invalid_arg
            "Key_value_store.directory: encoding does not have fixed size")

(* FIXME https://gitlab.com/tezos/tezos/-/issues/4643

   The reason why there are two LRUs and not one, is that in the case
   of concurrent reads and writes, the LRU cannot prevent the absence
   of race. To prevent that we use two LRUs to be able to discriminate
   between the various concurrent accesses. In particular, while
   reading a value, we want to wait if there is a write in
   progress. Vice versa, if a read fails, we don't want to make the
   next write to fail.

   In practice, there should not be a duplication in memory of the
   values read since values are shared. *)

let init ~lru_size directory_of =
  let directories = Directories.init ~lru_size in
  E {directory_of; directories}

let close (E {directories; _}) = Directories.close directories

let write_value :
    type dir file value.
    ?override:bool ->
    (dir, file, value) t ->
    dir ->
    file ->
    value ->
    unit tzresult Lwt.t =
 fun ?override (E {directories; directory_of}) dir file value ->
  let dir = directory_of dir in
  Directories.write ?override directories dir file value

let read_value :
    type dir file value.
    (dir, file, value) t -> dir -> file -> value tzresult Lwt.t =
 fun (E {directories; directory_of}) dir file ->
  let dir = directory_of dir in
  Directories.read directories dir file

let value_exists :
    type dir file value. (dir, file, value) t -> dir -> file -> bool Lwt.t =
 fun (E {directories; directory_of}) dir file ->
  let dir = directory_of dir in
  Directories.value_exists directories dir file

let write_values ?override t seq =
  Seq.ES.iter
    (fun (dir, file, value) -> write_value ?override t dir file value)
    seq

let read_values t seq =
  let open Lwt_syntax in
  Seq_s.of_seq seq
  |> Seq_s.S.map (fun (dir, file) ->
         let* maybe_value = read_value t dir file in
         return (dir, file, maybe_value))

let values_exist t seq =
  let open Lwt_syntax in
  Seq_s.of_seq seq
  |> Seq_s.S.map (fun (dir, file) ->
         let* maybe_value = value_exists t dir file in
         return (dir, file, maybe_value))
