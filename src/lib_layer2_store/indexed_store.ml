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

open Store_sigs
open Store_errors

(* Helper functions to copy byte sequences or integers in [src] to another byte
   sequence [dst] at offset [offset], with named arguments to avoid
   confusion. These functions return the offset in the destination at which to
   copy the more data. *)

let blit ~src ~dst offset =
  let len = Bytes.length src in
  Bytes.blit src 0 dst offset len ;
  offset + len

let bytes_set_int64 ~src ~dst offset =
  Bytes.set_int64_be dst offset src ;
  offset + 8

let bytes_set_int8 ~src ~dst offset =
  Bytes.set_int8 dst offset src ;
  offset + 1

(* Helper functions to read data (strings with a decoding function, or integers)
   from a binary string. These functions return, as the second component, the
   offset in the string at which to read more data. *)

let read_int64 str offset =
  let i = TzEndian.get_int64_string str offset in
  (i, offset + 8)

let read_int8 str offset =
  let i = TzEndian.get_int8_string str offset in
  (i, offset + 1)

(* Functors to build stores on indexes *)

type ('key, 'value) gc_iterator =
  | Retain of 'key list
  | Iterator of {first : 'key; next : 'key -> 'value -> 'key option Lwt.t}

module type NAME = sig
  val name : string
end

module type SINGLETON_STORE = sig
  type +'a t

  type value

  val load : path:string -> 'a mode -> 'a t tzresult Lwt.t

  val read : [> `Read] t -> value option tzresult Lwt.t

  val write : [> `Write] t -> value -> unit tzresult Lwt.t

  val delete : [> `Write] t -> unit tzresult Lwt.t

  val readonly : [> `Read] t -> [`Read] t
end

module type INDEXABLE_STORE = sig
  type +'a t

  type key

  type value

  val load :
    path:string -> index_buffer_size:int -> 'a mode -> 'a t tzresult Lwt.t

  val mem : [> `Read] t -> key -> bool tzresult Lwt.t

  val find : [> `Read] t -> key -> value option tzresult Lwt.t

  val add : ?flush:bool -> [> `Write] t -> key -> value -> unit tzresult Lwt.t

  val close : _ t -> unit tzresult Lwt.t

  val readonly : [> `Read] t -> [`Read] t

  val gc :
    ?async:bool -> rw t -> (key, value) gc_iterator -> unit tzresult Lwt.t

  val wait_gc_completion : 'a t -> unit Lwt.t

  val is_gc_finished : 'a t -> bool
end

module type INDEXABLE_REMOVABLE_STORE = sig
  include INDEXABLE_STORE

  val remove : ?flush:bool -> [> `Write] t -> key -> unit tzresult Lwt.t
end

module type INDEXED_FILE = sig
  type +'a t

  type key

  type header

  type value

  val mem : [> `Read] t -> key -> bool tzresult Lwt.t

  val header : [> `Read] t -> key -> header option tzresult Lwt.t

  val read : [> `Read] t -> key -> (value * header) option tzresult Lwt.t

  val append :
    ?flush:bool ->
    [> `Write] t ->
    key:key ->
    header:header ->
    value:value ->
    unit tzresult Lwt.t

  val load :
    path:string ->
    index_buffer_size:int ->
    cache_size:int ->
    'a mode ->
    'a t tzresult Lwt.t

  val close : _ t -> unit tzresult Lwt.t

  val readonly : [> `Read] t -> [`Read] t

  val gc :
    ?async:bool ->
    rw t ->
    (key, value * header) gc_iterator ->
    unit tzresult Lwt.t

  val wait_gc_completion : 'a t -> unit Lwt.t

  val is_gc_finished : 'a t -> bool
end

module type SIMPLE_INDEXED_FILE = sig
  include INDEXED_FILE

  val append :
    ?flush:bool -> [> `Write] t -> key:key -> value:value -> unit tzresult Lwt.t
end

module type INDEX_KEY = sig
  include Index.Key.S

  val pp : Format.formatter -> t -> unit
end

module type ENCODABLE_VALUE = sig
  type t

  val name : string

  val encoding : t Data_encoding.t
end

module type FIXED_ENCODABLE_VALUE = sig
  include ENCODABLE_VALUE

  val fixed_size : int
end

module type ENCODABLE_VALUE_HEADER = sig
  include ENCODABLE_VALUE

  module Header : FIXED_ENCODABLE_VALUE
end

module Make_fixed_encodable (V : ENCODABLE_VALUE) :
  FIXED_ENCODABLE_VALUE with type t = V.t = struct
  include V

  let fixed_size =
    match Data_encoding.Binary.fixed_length encoding with
    | None -> Stdlib.invalid_arg (name ^ " encoding is not fixed size")
    | Some size -> size
end

module Make_index_value (E : FIXED_ENCODABLE_VALUE) :
  Index.Value.S with type t = E.t = struct
  type t = E.t

  let encoded_size = E.fixed_size

  let encode v =
    Data_encoding.Binary.to_string_exn ~buffer_size:encoded_size E.encoding v

  let decode buf offset =
    let _read_bytes, v =
      Data_encoding.Binary.read_exn E.encoding buf offset encoded_size
    in
    v

  (* The {!Repr.t} value is only used for pretty printing in {!Index} so this is
     fine. *)
  let t = Repr.map Repr.string (fun s -> decode s 0) encode
end

module Make_index_key (E : sig
  include FIXED_ENCODABLE_VALUE

  val equal : t -> t -> bool
end) : INDEX_KEY with type t = E.t = struct
  include Make_index_value (E)

  let equal = E.equal

  let hash v = Stdlib.Hashtbl.hash (encode v)

  (* {!Stdlib.Hashtbl.hash} is 30 bits *)
  let hash_size = 30 (* in bits *)

  let pp ppf k =
    Format.fprintf
      ppf
      "%a"
      Data_encoding.Json.pp
      (Data_encoding.Json.construct E.encoding k)
end

let gc_reachable_of_iter =
  let open Lwt_syntax in
  function
  | Iterator {first; next} -> (
      (Some first, fun k -> function Some v -> next k v | None -> return_none))
  | Retain keys ->
      let dispenser = Seq.to_dispenser (List.to_seq keys) in
      let first = dispenser () in
      (first, fun _k _v -> return (dispenser ()))

module Make_indexable (N : NAME) (K : INDEX_KEY) (V : Index.Value.S) = struct
  module I = Index_unix.Make (K) (V) (Index.Cache.Unbounded)

  type internal_index = {index : I.t; index_path : string}

  type gc_status =
    | No_gc
    | Ongoing of {tmp_index : internal_index; promise : unit Lwt.t}

  (** In order to periodically clean up the store with the {!gc} function, each
      pure index store is split in multiple indexes: one fresh index and
      multiple stale indexes (zero if no GC has ever occurred, one if the last
      GC was successful, two if the GC is ongoing, and more in case some GC
      failed).

      Adding new information to the store is always done on the fresh index
      whereas queries are done on both the fresh and stale indexes.

      A gc operation starts by moving the fresh index to the stale list and
      creates a new fresh index. The rest of the gc operation consists in,
      asynchronously, merging the stale indexes while removing data. See {!gc}
      for more details.
  *)
  type _ t = {
    mutable fresh : internal_index;
    mutable stales : internal_index list;
    scheduler : Lwt_idle_waiter.t;
    readonly : bool;
    index_buffer_size : int;
    path : string;
    mutable gc_status : gc_status;
  }

  let internal_indexes ?(only_stale = false) store =
    if only_stale then store.stales else store.fresh :: store.stales

  let unsafe_mem store k =
    List.exists (fun i -> I.mem i.index k) (internal_indexes store)

  let mem store k =
    let open Lwt_result_syntax in
    trace (Cannot_read_from_store N.name)
    @@ protect
    @@ fun () ->
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    return (unsafe_mem store k)

  let find_index i k = try Some (I.find i k) with Not_found -> None

  let unsafe_find ?only_stale store k =
    List.find_map
      (fun i ->
        try find_index i.index k
        with e ->
          Format.kasprintf
            Stdlib.failwith
            "cannot access index %s : %s"
            i.index_path
            (Printexc.to_string e))
      (internal_indexes ?only_stale store)

  let find store k =
    let open Lwt_result_syntax in
    trace (Cannot_read_from_store N.name)
    @@ protect
    @@ fun () ->
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    return (unsafe_find store k)

  let add ?(flush = true) store k v =
    let open Lwt_result_syntax in
    trace (Cannot_write_to_store N.name)
    @@ protect
    @@ fun () ->
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    I.replace store.fresh.index k v ;
    if flush then I.flush store.fresh.index ;
    return_unit

  let stale_path path n = String.concat "." [path; string_of_int n]

  let tmp_path path = String.concat "." [path; "tmp"]

  let load_internal_index ~index_buffer_size ~readonly index_path =
    let index = I.v ~log_size:index_buffer_size ~readonly index_path in
    {index; index_path}

  let load (type a) ~path ~index_buffer_size (mode : a mode) :
      a t tzresult Lwt.t =
    let open Lwt_result_syntax in
    trace (Cannot_load_store {name = N.name; path})
    @@ protect
    @@ fun () ->
    let*! () = Lwt_utils_unix.create_dir (Filename.dirname path) in
    let readonly = match mode with Read_only -> true | Read_write -> false in
    let fresh = load_internal_index ~index_buffer_size ~readonly path in
    (* Loading stale indexes if they exist on disk (stale indexes are created by
       GC operations). *)
    let rec load_stales acc n =
      let open Lwt_syntax in
      let stale_path = stale_path path n in
      let*! exists = Lwt_unix.file_exists stale_path in
      if exists then
        let stale =
          load_internal_index ~index_buffer_size ~readonly:true stale_path
        in
        load_stales (stale :: acc) (n + 1)
      else return acc
    in
    let*! stales = load_stales [] 1 in
    let scheduler = Lwt_idle_waiter.create () in
    return
      {
        fresh;
        stales;
        scheduler;
        readonly;
        index_buffer_size;
        path;
        gc_status = No_gc;
      }

  let close_internal_index i = try I.close i.index with Index.Closed -> ()

  let mv_internal_index store index dest =
    let open Lwt_syntax in
    close_internal_index index ;
    let+ () = Lwt_unix.rename index.index_path dest in
    load_internal_index
      ~index_buffer_size:store.index_buffer_size
      ~readonly:store.readonly
      dest

  let rm_internal_index index =
    close_internal_index index ;
    Lwt_utils_unix.remove_dir index.index_path

  let close store =
    protect @@ fun () ->
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    let open Lwt_result_syntax in
    (match store.gc_status with
    | Ongoing {promise; _} -> Lwt.cancel promise
    | No_gc -> ()) ;
    close_internal_index store.fresh ;
    List.iter close_internal_index store.stales ;
    let*! () =
      match store.gc_status with
      | No_gc -> Lwt.return_unit
      | Ongoing {tmp_index; _} -> rm_internal_index tmp_index
    in
    return_unit

  let readonly x = (x :> [`Read] t)

  (** A gc is initiated by moving the fresh index to the stale list and creating
      a new fresh index, as well as creating a temporary index for the result of
      the gc. *)
  let initiate_gc store =
    let open Lwt_syntax in
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    let* () = Store_events.starting_gc N.name in
    let tmp_index_path = tmp_path store.path in
    let new_stale_path = stale_path store.path (List.length store.stales + 1) in
    let* new_index_stale = mv_internal_index store store.fresh new_stale_path in
    let new_index_fresh =
      load_internal_index
        ~index_buffer_size:store.index_buffer_size
        ~readonly:store.readonly
        store.path
    in
    let tmp_index =
      load_internal_index
        ~index_buffer_size:store.index_buffer_size
        ~readonly:false
        tmp_index_path
    in
    (* Clear temporary index in case there are leftovers from a dirtily aborted
       previous gc. *)
    I.clear tmp_index.index ;
    store.fresh <- new_index_fresh ;
    store.stales <- new_index_stale :: store.stales ;
    let promise, resolve = Lwt.task () in
    store.gc_status <- Ongoing {tmp_index; promise} ;
    return (tmp_index, promise, resolve)

  (** If a gc operation fails, reverting simply consists in removing the
      temporary index. We keep the two stale indexes as is, they will be merged
      by the next successful gc.  *)
  let revert_failed_gc store =
    let open Lwt_syntax in
    match store.gc_status with
    | No_gc -> return_unit
    | Ongoing {tmp_index; promise} ->
        Lwt.cancel promise ;
        store.gc_status <- No_gc ;
        rm_internal_index tmp_index

  (** Copy item associated to [k] from the [store] stale indexes to
      [tmp_index]. *)
  let unsafe_retain_one_item store tmp_index filter k =
    let open Lwt_syntax in
    let value = unsafe_find ~only_stale:true store k in
    let* () =
      match value with
      | None ->
          Store_events.missing_value_gc N.name (Format.asprintf "%a" K.pp k)
      | Some v ->
          if filter v then I.replace tmp_index.index k v ;
          return_unit
    in
    return value

  (** When the gc operation finishes, i.e. we have copied all elements to retain
      to the temporary index, we can replace all the stale indexes by the
      temporary one. *)
  let finalize_gc store tmp_index =
    let open Lwt_result_syntax in
    protect @@ fun () ->
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    let*! () = List.iter_s rm_internal_index store.stales in
    let stale_path = stale_path store.path 1 in
    let*! index_stale = mv_internal_index store tmp_index stale_path in
    store.stales <- [index_stale] ;
    store.gc_status <- No_gc ;
    let*! () = Store_events.finished_gc N.name in
    return_unit

  (** The background task for a gc operation consists in copying all items
      reachable by [gc_iter] from the stale indexes to the temporary one. While
      this is happening, the stale indexes can still be queried and new bindings
      can still be added to the store because only the fresh index is
      modified. *)
  let gc_background_task store tmp_index gc_iter filter resolve =
    let open Lwt_syntax in
    Lwt.dont_wait
      (fun () ->
        let* res =
          trace (Gc_failed N.name) @@ protect
          @@ fun () ->
          let first, next = gc_reachable_of_iter gc_iter in
          let rec copy elt =
            let* value = unsafe_retain_one_item store tmp_index filter elt in
            let* next = next elt value in
            match next with None -> return_unit | Some elt -> copy elt
          in
          let* () = Option.iter_s copy first in
          finalize_gc store tmp_index
        in
        let* () =
          match res with
          | Ok () -> return_unit
          | Error error ->
              let* () = Store_events.failed_gc N.name error in
              revert_failed_gc store
        in
        Lwt.wakeup_later resolve () ;
        return_unit)
      (fun exn ->
        Format.eprintf
          "Reverting GC for store %s failed because %s@."
          N.name
          (Printexc.to_string exn))

  (** This function is called every time a gc operation starts. *)
  let gc_internal ~async store gc_iter filter =
    let open Lwt_syntax in
    match store.gc_status with
    | Ongoing _ -> Store_events.ignore_gc N.name
    | No_gc ->
        let* tmp_index, promise, resolve = initiate_gc store in
        gc_background_task store tmp_index gc_iter filter resolve ;
        if async then return_unit
        else
          Lwt.catch
            (fun () -> promise)
            (function Lwt.Canceled -> return_unit | e -> raise e)

  let gc ?(async = true) store gc_iter =
    let open Lwt_result_syntax in
    trace (Gc_failed N.name) @@ protect
    @@ fun () ->
    let*! () = gc_internal ~async store gc_iter (fun _ -> true) in
    return_unit

  let wait_gc_completion store =
    match store.gc_status with
    | No_gc -> Lwt.return_unit
    | Ongoing {promise; _} ->
        Lwt.catch
          (fun () -> promise)
          (function Lwt.Canceled -> Lwt.return_unit | e -> raise e)

  let is_gc_finished store =
    match store.gc_status with No_gc -> true | Ongoing _ -> false
end

module Make_indexable_removable (N : NAME) (K : INDEX_KEY) (V : Index.Value.S) =
struct
  module V_opt = struct
    (* The values stored in the index are optional values.  When we "remove" a
       key from the store, we're not really removing it from the index, but
       simply setting its association to [None] (encoded with zero bytes here).
    *)

    type t = V.t option

    let t = Repr.option V.t

    let encoded_size = 1 + V.encoded_size

    let encode v =
      let dst = Bytes.create encoded_size in
      let tag, value_bytes =
        match v with
        | None -> (0, Bytes.make V.encoded_size '\000')
        | Some v -> (1, V.encode v |> Bytes.unsafe_of_string)
      in
      let offset = bytes_set_int8 ~dst ~src:tag 0 in
      let _ = blit ~src:value_bytes ~dst offset in
      Bytes.unsafe_to_string dst

    let decode str offset =
      let tag, offset = read_int8 str offset in
      match tag with
      | 0 -> None
      | 1 ->
          let value = V.decode str offset in
          Some value
      | _ -> assert false
  end

  include Make_indexable (N) (K) (V_opt)

  let find store k =
    let open Lwt_result_syntax in
    let+ v = find store k in
    match v with None | Some None -> None | Some (Some v) -> Some v

  let mem store hash =
    let open Lwt_result_syntax in
    let+ b = find store hash in
    Option.is_some b

  let add ?flush store k v = add ?flush store k (Some v)

  let remove ?(flush = true) store k =
    let open Lwt_result_syntax in
    trace (Cannot_write_to_store N.name)
    @@ protect
    @@ fun () ->
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    let exists = unsafe_mem store k in
    if not exists then return_unit
    else (
      I.replace store.fresh.index k None ;
      if flush then I.flush store.fresh.index ;
      return_unit)

  let gc ?(async = true) store gc_iter =
    let open Lwt_result_syntax in
    trace (Gc_failed N.name) @@ protect
    @@ fun () ->
    let gc_iter =
      match gc_iter with
      | Retain keys -> Retain keys
      | Iterator {first; next} ->
          let next k = function
            | None -> Lwt.return_none
            | Some v -> next k v
          in
          Iterator {first; next}
    in
    let*! () = gc_internal ~async store gc_iter Option.is_some in
    return_unit
end

module Make_singleton (S : sig
  type t

  val name : string

  val encoding : t Data_encoding.t
end) : SINGLETON_STORE with type value := S.t = struct
  type +'a t = {file : string; mutable cache : S.t option option}

  let read_disk store =
    let open Lwt_result_syntax in
    trace (Cannot_read_from_store S.name)
    @@ protect
    @@ fun () ->
    let*! exists = Lwt_unix.file_exists store.file in
    match exists with
    | false -> return_none
    | true -> (
        Lwt_io.with_file
          ~flags:[Unix.O_RDONLY; O_CLOEXEC]
          ~mode:Input
          store.file
        @@ fun channel ->
        let*! raw_data = Lwt_io.read channel in
        let data = Data_encoding.Binary.of_string S.encoding raw_data in
        match data with
        | Ok data -> return_some data
        | Error err -> tzfail (Decoding_error err))

  let read store =
    let open Lwt_result_syntax in
    match store.cache with Some v -> return v | None -> read_disk store

  let write_disk store x =
    let open Lwt_result_syntax in
    trace (Cannot_write_to_store S.name)
    @@ let*! res =
         Lwt_utils_unix.with_atomic_open_out ~overwrite:true store.file
         @@ fun fd ->
         let block_bytes = Data_encoding.Binary.to_bytes_exn S.encoding x in
         Lwt_utils_unix.write_bytes fd block_bytes
       in
       Result.bind_error res Lwt_utils_unix.tzfail_of_io_error |> Lwt.return

  let write store x =
    let open Lwt_result_syntax in
    let+ () = write_disk store x in
    store.cache <- Some (Some x)

  let delete_disk store =
    let open Lwt_result_syntax in
    trace (Cannot_write_to_store S.name)
    @@ protect
    @@ fun () ->
    let*! exists = Lwt_unix.file_exists store.file in
    match exists with
    | false -> return_unit
    | true ->
        let*! () = Lwt_unix.unlink store.file in
        return_unit

  let delete store =
    let open Lwt_result_syntax in
    let+ () = delete_disk store in
    store.cache <- Some None

  let load ~path _mode =
    let open Lwt_result_syntax in
    trace (Cannot_load_store {name = S.name; path})
    @@ protect
    @@ fun () ->
    let*! () = Lwt_utils_unix.create_dir (Filename.dirname path) in
    return {file = path; cache = None}

  let readonly x = (x :> [`Read] t)
end

module Make_indexed_file (N : NAME) (K : INDEX_KEY) (V : ENCODABLE_VALUE_HEADER) =
struct
  module Cache =
    Aches_lwt.Lache.Make_result (Aches.Rache.Transfer (Aches.Rache.LRU) (K))
  module Raw_header = Make_index_value (V.Header)

  module IHeader = struct
    let name = N.name ^ ".header"

    type t = {offset : int; header : V.Header.t}

    let encoded_size = 8 (* offset *) + Raw_header.encoded_size

    let t =
      let open Repr in
      map
        (pair int Raw_header.t)
        (fun (offset, header) -> {offset; header})
        (fun {offset; header} -> (offset, header))

    let encode v =
      let dst = Bytes.create encoded_size in
      let offset = bytes_set_int64 ~src:(Int64.of_int v.offset) ~dst 0 in
      let _offset =
        blit ~src:(Raw_header.encode v.header |> String.to_bytes) ~dst offset
      in
      Bytes.unsafe_to_string dst

    let decode str offset =
      let file_offset, offset = read_int64 str offset in
      let header = Raw_header.decode str offset in
      {offset = Int64.to_int file_offset; header}
  end

  module Header_index = Index_unix.Make (K) (IHeader) (Index.Cache.Unbounded)

  module Values_file = struct
    let encoding = Data_encoding.dynamic_size ~kind:`Uint30 V.encoding

    let pread_value fd ~file_offset =
      let open Lwt_result_syntax in
      trace (Cannot_read_from_store N.name)
      @@ protect
      @@ fun () ->
      (* Read length *)
      let length_bytes = Bytes.create 4 in
      let*! () =
        Lwt_utils_unix.read_bytes ~file_offset ~pos:0 ~len:4 fd length_bytes
      in
      let value_length_int32 = Bytes.get_int32_be length_bytes 0 in
      let value_length = Int32.to_int value_length_int32 in
      let value_bytes = Bytes.extend length_bytes 0 value_length in
      let*! () =
        Lwt_utils_unix.read_bytes
          ~file_offset:(file_offset + 4)
          ~pos:4
          ~len:value_length
          fd
          value_bytes
      in
      match Data_encoding.Binary.of_bytes encoding value_bytes with
      | Ok value -> return (value, 4 + value_length)
      | Error err -> tzfail (Decoding_error err)
  end

  type internal_store = {
    index : Header_index.t;
    fd : Lwt_unix.file_descr;
    index_path : string;
    data_path : string;
  }

  type gc_status =
    | No_gc
    | Ongoing of {tmp_store : internal_store; promise : unit Lwt.t}

  (** In order to periodically clean up the store with the {!gc} function, each
      store is split in multiple stores: one fresh store and multiple stale
      stores (zero if no GC has ever occurred, one if the last GC was
      successful, two if the GC is ongoing, and more in case some GC failed).

      Adding new information to the store is always done on the fresh store
      whereas queries are done on both the fresh and stale stores.

      A gc operation starts by moving the fresh store to the stale list and
      creates a new fresh store. The rest of the gc operation consists in,
      asynchronously, merging the stale stores while removing data. See {!gc}
      for more details.
  *)
  type _ t = {
    mutable fresh : internal_store;
    mutable stales : internal_store list;
    scheduler : Lwt_idle_waiter.t;
    readonly : bool;
    index_buffer_size : int;
    path : string;
    cache : (V.t * V.Header.t, tztrace) Cache.t;
    mutable gc_status : gc_status;
  }

  let internal_stores ?(only_stale = false) store =
    if only_stale then store.stales else store.fresh :: store.stales

  let unsafe_mem store k =
    List.exists (fun s -> Header_index.mem s.index k) (internal_stores store)

  let mem store key =
    let open Lwt_result_syntax in
    trace (Cannot_read_from_store IHeader.name)
    @@ protect
    @@ fun () ->
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    let cached =
      Cache.bind store.cache key (fun v -> Lwt.return (Result.is_ok v))
    in
    let*! cached = Option.value cached ~default:Lwt.return_false in
    return (cached || unsafe_mem store key)

  let find_index i k = try Some (Header_index.find i k) with Not_found -> None

  let unsafe_find_header ?only_stale store k =
    List.find_map
      (fun s ->
        Option.map
          (fun h -> (h, s))
          (try find_index s.index k
           with e ->
             Format.kasprintf
               Stdlib.failwith
               "cannot access index %s : %s"
               s.index_path
               (Printexc.to_string e)))
      (internal_stores ?only_stale store)

  let header store key =
    let open Lwt_result_syntax in
    trace (Cannot_read_from_store IHeader.name)
    @@ protect
    @@ fun () ->
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    let cached =
      Cache.bind store.cache key @@ function
      | Ok (_value, header) -> return header
      | Error _ as e -> Lwt.return e
    in
    match cached with
    | Some header -> Lwt_result.map Option.some header
    | None -> (
        match unsafe_find_header store key with
        | None -> return_none
        | Some ({header; _}, _store) -> return_some header)

  let unsafe_read_from_disk_opt ?only_stale store key =
    let open Lwt_result_syntax in
    match unsafe_find_header ?only_stale store key with
    | None -> return_none
    | Some ({IHeader.offset; header}, internal_store) ->
        let+ value, _ofs =
          Values_file.pread_value internal_store.fd ~file_offset:offset
        in
        Some (value, header)

  let unsafe_read_from_disk store key =
    let open Lwt_result_syntax in
    let* r = unsafe_read_from_disk_opt store key in
    match r with None -> tzfail (Exn Not_found) | Some r -> return r

  let read store key =
    trace (Cannot_read_from_store IHeader.name)
    @@ protect
    @@ fun () ->
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    let read_from_disk key = unsafe_read_from_disk store key in
    let open Lwt_result_syntax in
    Cache.bind_or_put store.cache key read_from_disk @@ function
    | Ok value -> return_some value
    | Error _ -> return_none

  let locked_write_value store ~offset ~value ~key ~header =
    trace_eval (fun () -> Cannot_write_to_store N.name)
    @@ protect
    @@ fun () ->
    let open Lwt_result_syntax in
    let value_bytes =
      Data_encoding.Binary.to_bytes_exn Values_file.encoding value
    in
    let value_length = Bytes.length value_bytes in
    let*! () =
      Lwt_utils_unix.write_bytes ~pos:0 ~len:value_length store.fd value_bytes
    in
    Header_index.replace store.index key {offset; header} ;
    return value_length

  let unsafe_append_internal ?(flush = true) store ~key ~header ~value =
    let open Lwt_result_syntax in
    let*! offset = Lwt_unix.lseek store.fd 0 Unix.SEEK_END in
    let*! _written_len = locked_write_value store ~offset ~value ~key ~header in
    if flush then Header_index.flush store.index ;
    return_unit

  let append ?(flush = true) store ~key ~header ~value =
    trace (Cannot_write_to_store N.name)
    @@ protect
    @@ fun () ->
    let open Lwt_result_syntax in
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    Cache.put store.cache key (return (value, header)) ;
    unsafe_append_internal ~flush store.fresh ~key ~header ~value

  let stale_path path n = Filename.concat path (string_of_int n)

  let tmp_path path = Filename.concat path "tmp"

  let data_path path = Filename.concat path "data"

  let index_path path = Filename.concat path "index"

  let load_internal_store ~index_buffer_size ~readonly path =
    let open Lwt_syntax in
    let flag = if readonly then Unix.O_RDONLY else Unix.O_RDWR in
    let* () = Lwt_utils_unix.create_dir path in
    let data_path = data_path path in
    let* fd =
      Lwt_unix.openfile data_path [Unix.O_CREAT; O_CLOEXEC; flag] 0o644
    in
    let index_path = index_path path in
    let index =
      Header_index.v ~log_size:index_buffer_size ~readonly index_path
    in
    return {index; fd; index_path; data_path}

  let load (type a) ~path ~index_buffer_size ~cache_size (mode : a mode) :
      a t tzresult Lwt.t =
    let open Lwt_result_syntax in
    trace (Cannot_load_store {name = N.name; path})
    @@ protect
    @@ fun () ->
    let readonly = match mode with Read_only -> true | Read_write -> false in
    let*! fresh = load_internal_store ~index_buffer_size ~readonly path in
    (* Loading stale stores if they exist on disk (stale stores are created by
       GC operations). *)
    let rec load_stales acc n =
      let open Lwt_syntax in
      let stale_path = stale_path path n in
      let*! exists = Lwt_unix.file_exists stale_path in
      if exists then
        let*! stale =
          load_internal_store ~index_buffer_size ~readonly:true stale_path
        in
        load_stales (stale :: acc) (n + 1)
      else return acc
    in
    let*! stales = load_stales [] 1 in
    let scheduler = Lwt_idle_waiter.create () in
    let cache = Cache.create cache_size in
    return
      {
        fresh;
        stales;
        scheduler;
        readonly;
        index_buffer_size;
        path;
        cache;
        gc_status = No_gc;
      }

  let close_internal_store store =
    (try Header_index.close store.index with Index.Closed -> ()) ;
    Lwt_utils_unix.safe_close store.fd

  let mv_internal_store store internal_store dest =
    let open Lwt_result_syntax in
    let* () = close_internal_store internal_store in
    let*! () = Lwt_utils_unix.create_dir dest in
    let*! () = Lwt_unix.rename internal_store.index_path (index_path dest) in
    let*! () = Lwt_unix.rename internal_store.data_path (data_path dest) in
    let*! new_store =
      load_internal_store
        ~index_buffer_size:store.index_buffer_size
        ~readonly:store.readonly
        dest
    in
    return new_store

  let rm_internal_store store =
    let path = Filename.dirname store.index_path in
    let open Lwt_result_syntax in
    let* () = close_internal_store store in
    assert (path = Filename.dirname store.data_path) ;
    let*! () = Lwt_utils_unix.remove_dir path in
    return_unit

  let close store =
    protect @@ fun () ->
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    let open Lwt_result_syntax in
    (match store.gc_status with
    | Ongoing {promise; _} -> Lwt.cancel promise
    | No_gc -> ()) ;
    let* () = close_internal_store store.fresh
    and* () = List.iter_ep close_internal_store store.stales
    and* () =
      match store.gc_status with
      | No_gc -> return_unit
      | Ongoing {tmp_store; _} -> rm_internal_store tmp_store
    in
    return_unit

  let readonly x = (x :> [`Read] t)

  (** A gc is initiated by moving the fresh store to the stale list and creating
      a new fresh store, as well as creating a temporary store for the result of
      the gc. *)
  let initiate_gc store =
    let open Lwt_result_syntax in
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    let*! () = Store_events.starting_gc N.name in
    let tmp_store_path = tmp_path store.path in
    let new_stale_path = stale_path store.path (List.length store.stales + 1) in
    let* new_store_stale = mv_internal_store store store.fresh new_stale_path in
    let*! new_store_fresh =
      load_internal_store
        ~index_buffer_size:store.index_buffer_size
        ~readonly:store.readonly
        store.path
    in
    let*! tmp_store =
      load_internal_store
        ~index_buffer_size:store.index_buffer_size
        ~readonly:false
        tmp_store_path
    in
    (* Clear temporary store in case there are leftovers from a dirtily aborted
       previous gc. *)
    Header_index.clear tmp_store.index ;
    let*! () = Lwt_unix.ftruncate tmp_store.fd 0 in
    store.fresh <- new_store_fresh ;
    store.stales <- new_store_stale :: store.stales ;
    let promise, resolve = Lwt.task () in
    store.gc_status <- Ongoing {tmp_store; promise} ;
    return (tmp_store, promise, resolve)

  (** If a gc operation fails, reverting simply consists in removing the
      temporary store. We keep the two stale stores as is, they will be merged
      by the next successful gc.  *)
  let revert_failed_gc store =
    let open Lwt_syntax in
    match store.gc_status with
    | No_gc -> return_unit
    | Ongoing {tmp_store; promise} -> (
        Lwt.cancel promise ;
        let+ res = rm_internal_store tmp_store in
        match res with
        | Ok () -> ()
        | Error _e -> (* ignore error when reverting *) ())

  (** Copy item associated to [k] from the [store] stale stores to
      [tmp_store]. *)
  let unsafe_retain_one_item store tmp_store key =
    let open Lwt_result_syntax in
    let* v = unsafe_read_from_disk_opt ~only_stale:true store key in
    let* () =
      match v with
      | None ->
          let*! () =
            Store_events.missing_value_gc N.name (Format.asprintf "%a" K.pp key)
          in
          return_unit
      | Some (value, header) ->
          unsafe_append_internal tmp_store ~key ~header ~value
    in
    return v

  (** When the gc operation finishes, i.e. we have copied all elements to retain
      to the temporary store, we can replace all the stale stores by the
      temporary one. *)
  let finalize_gc store tmp_store =
    let open Lwt_result_syntax in
    protect @@ fun () ->
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    let* () = List.iter_es rm_internal_store store.stales in
    let stale_path = stale_path store.path 1 in
    let* store_stale = mv_internal_store store tmp_store stale_path in
    store.stales <- [store_stale] ;
    store.gc_status <- No_gc ;
    Cache.clear store.cache ;
    let*! () = Store_events.finished_gc N.name in
    return_unit

  (** The background task for a gc operation consists in copying all items
      reachable by [gc_iter] from the stale stores to the temporary one. While
      this is happening, the stale stores can still be queried and new bindings
      can still be added to the store because only the fresh store is
      modified. *)
  let gc_background_task store tmp_store gc_iter resolve =
    let open Lwt_result_syntax in
    Lwt.dont_wait
      (fun () ->
        let*! res =
          trace (Gc_failed N.name) @@ protect
          @@ fun () ->
          let first, next = gc_reachable_of_iter gc_iter in
          let rec copy elt =
            let* value = unsafe_retain_one_item store tmp_store elt in
            let*! next = next elt value in
            match next with None -> return_unit | Some elt -> copy elt
          in
          let* () = Option.iter_es copy first in
          finalize_gc store tmp_store
        in
        let*! () =
          let open Lwt_syntax in
          match res with
          | Ok () -> return_unit
          | Error error ->
              let* () = Store_events.failed_gc N.name error in
              revert_failed_gc store
        in
        Lwt.wakeup_later resolve () ;
        Lwt.return_unit)
      (fun exn ->
        Format.eprintf
          "Reverting GC for store %s failed because %s@."
          N.name
          (Printexc.to_string exn))

  let gc_internal ~async store gc_iter =
    let open Lwt_result_syntax in
    match store.gc_status with
    | Ongoing _ ->
        let*! () = Store_events.ignore_gc N.name in
        return_unit
    | No_gc ->
        let* tmp_store, promise, resolve = initiate_gc store in
        gc_background_task store tmp_store gc_iter resolve ;
        if async then return_unit
        else
          Lwt.catch
            (fun () ->
              let*! () = promise in
              return_unit)
            (function Lwt.Canceled -> return_unit | e -> raise e)

  let gc ?(async = true) store gc_iter =
    trace (Gc_failed N.name) @@ gc_internal ~async store gc_iter

  let wait_gc_completion store =
    match store.gc_status with
    | No_gc -> Lwt.return_unit
    | Ongoing {promise; _} ->
        Lwt.catch
          (fun () -> promise)
          (function Lwt.Canceled -> Lwt.return_unit | e -> raise e)

  let is_gc_finished store =
    match store.gc_status with No_gc -> true | Ongoing _ -> false
end

module Make_simple_indexed_file
    (N : NAME)
    (K : INDEX_KEY) (V : sig
      include ENCODABLE_VALUE_HEADER

      val header : t -> Header.t
    end) =
struct
  include Make_indexed_file (N) (K) (V)

  let append ?flush store ~key ~value =
    append ?flush store ~key ~value ~header:(V.header value)
end
