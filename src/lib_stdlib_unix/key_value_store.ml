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

type error +=
  | Missing_stored_kvs_data of {filepath : string; index : int}
  | Wrong_encoded_value_size of {
      file : string;
      index : int;
      expected : int;
      got : int;
    }
  | Closed of {action : string}
  | Corrupted_data of {action : string; filepath : string; index : int}
  | Encoding_failed of {filepath : string; index : int}
  | Decoding_failed of {filepath : string; index : int}

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
    Data_encoding.(obj2 (req "filepath" string) (req "index" int31))
    (function
      | Missing_stored_kvs_data {filepath; index} -> Some (filepath, index)
      | _ -> None)
    (fun (filepath, index) -> Missing_stored_kvs_data {filepath; index}) ;
  register_error_kind
    `Permanent
    ~id:"stdlib_unix.wrong_encoded_value_size"
    ~title:"Wrong encoded value size"
    ~description:"Try to write a value that does not match the expected size"
    ~pp:(fun ppf (file, index, expected, got) ->
      Format.fprintf
        ppf
        "While encoding a value with index '%d' on file '%s', the value size \
         was expected to be '%d'. Got '%d'."
        index
        file
        expected
        got)
    Data_encoding.(
      obj4
        (req "file" string)
        (req "index" int31)
        (req "expected_size " int31)
        (req "got_size" int31))
    (function
      | Wrong_encoded_value_size {file; index; expected; got} ->
          Some (file, index, expected, got)
      | _ -> None)
    (fun (file, index, expected, got) ->
      Wrong_encoded_value_size {file; index; expected; got}) ;
  register_error_kind
    `Permanent
    ~id:"stdlib_unix.closed"
    ~title:"Key value stored was closed"
    ~description:"Action performed while the store is closed"
    ~pp:(fun ppf action ->
      Format.fprintf
        ppf
        "Failed to performa action '%s' because the store was closed"
        action)
    Data_encoding.(obj1 (req "action" string))
    (function Closed {action} -> Some action | _ -> None)
    (fun action -> Closed {action}) ;
  register_error_kind
    `Permanent
    ~id:"stdlib_unix.corrupted_data"
    ~title:"key value store data is corrupted"
    ~description:"A data of the key value store was corrupted"
    ~pp:(fun ppf (action, file, index) ->
      Format.fprintf
        ppf
        "Could not complete action '%s' because the data associated to file \
         '%s' and key index '%d' because the data is corrupted. Likely the \
         store was shutdown abnormally. If you see this message, please report \
         it."
        action
        file
        index)
    Data_encoding.(
      obj3 (req "action" string) (req "filepath" string) (req "index" int31))
    (function
      | Corrupted_data {action; filepath; index} ->
          Some (action, filepath, index)
      | _ -> None)
    (fun (action, filepath, index) -> Corrupted_data {action; filepath; index}) ;
  register_error_kind
    `Permanent
    ~id:"stdlib_unix.encoding_failed"
    ~title:"key value store failed to encode the data"
    ~description:"A failure was triggered while encoding the data"
    ~pp:(fun ppf (file, index) ->
      Format.fprintf
        ppf
        "While writing the data associated to file '%s' and key index '%d', a \
         failure was triggered during the encoding"
        file
        index)
    Data_encoding.(obj2 (req "filepath" string) (req "index" int31))
    (function
      | Encoding_failed {filepath; index} -> Some (filepath, index) | _ -> None)
    (fun (filepath, index) -> Encoding_failed {filepath; index}) ;
  register_error_kind
    `Permanent
    ~id:"stdlib_unix.decoding_failed"
    ~title:"key value store failed to decode the data"
    ~description:"A failure was triggered while decoding the data"
    ~pp:(fun ppf (file, index) ->
      Format.fprintf
        ppf
        "While reading the data associated to file '%s' and key index '%d', a \
         failure was triggered during the decoding"
        file
        index)
    Data_encoding.(obj2 (req "filepath" string) (req "index" int31))
    (function
      | Decoding_failed {filepath; index} -> Some (filepath, index) | _ -> None)
    (fun (filepath, index) -> Decoding_failed {filepath; index})

module Events = struct
  include Internal_event.Simple

  let section = ["key value store"]

  let warn_non_opened_file_descriptor =
    declare_0
      ~section
      ~name:"bad_file_descriptor"
      ~level:Warning
      ~msg:
        "Trying to unlock/close a non-open file descriptor, is the file \
         already closed?\n"
      ()
end

type ('key, 'value) layout = {
  encoding : 'value Data_encoding.t;
  eq : 'value -> 'value -> bool;
  index_of : 'key -> int;
  filepath : string;
  value_size : int;
}

(** The module [Files] handles writing and reading into memory-mapped files. A
    virtual file is backed by a physical file and a key is just an index (from 0
    to [max_number_of_keys - 1]). As values within a file have a fixed size, the
    index encodes the position of the associated value within the physical file.

    This module basically implements the key-value store, by grouping sets of
    key-value pairs in files. Each file comes with its own layout, specifying in
    particular the value size.

    This module must properly handle resource utilization, especially file
    descriptors.

    The structure {!Files.t} guarantees that no more than the specified
    [lru_size] file descriptors can be open at the same time.
*)
module Files : sig
  type 'value t

  val init : lru_size:int -> 'value t

  val close : 'value t -> unit Lwt.t

  val write :
    ?override:bool ->
    'value t ->
    ('key, 'value) layout ->
    'key ->
    'value ->
    unit tzresult Lwt.t

  val read : 'value t -> ('key, 'value) layout -> 'key -> 'value tzresult Lwt.t

  val value_exists :
    'value t -> ('key, 'value) layout -> 'key -> bool tzresult Lwt.t

  val count_values : 'value t -> ('key, 'value) layout -> int tzresult Lwt.t

  val remove : 'value t -> ('key, 'value) layout -> unit tzresult Lwt.t
end = struct
  module LRU = Ringo.LRU_Collection

  module Table = Hashtbl.Make (struct
    include String

    let hash = Hashtbl.hash
  end)

  (* The bitset of each file takes 4096 bytes. Because the bitset is
     actually a byte set, it means that the maximum number of keys is
     bounded by this size. We could optimize this to store `8` times
     more keys in the future or also increase the size of the
     bitset. *)
  let max_number_of_keys = 4096

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6033
     For now the bitset is a byte set...
     With a true bitset, we'd have [max_number_of_keys/8].
     Should be ok in practice since an atomic read/write on Linux is 4KiB.
  *)
  let bitset_size = max_number_of_keys

  (* The following cache allows to cache in memory values that were
     accessed recently. There is one cache per file opened. *)
  module Cache = Hashtbl.Make (struct
    type t = int

    let equal = Int.equal

    (* The index identifies uniquely the content. No need to hash it. *)
    let hash n = n
  end)

  (* This datatype represents an opened virtual file. *)
  type 'value opened_file = {
    fd : Lwt_unix.file_descr;
    (* The file descriptor of the corresponding physical file. The file is
       always prefixed by the bitset indicating the values stored in this file,
       see the {bitset} field. *)
    bitset : Lwt_bytes.t;
    (* This bitset encodes which values are present. *)
    count : int;
    (* The number of values. This is the same as the number of bits which are
       set in the bitset. *)
    cache : 'value Cache.t;
    (* This cache keeps in memory values accessed recently. It is bounded by the
       maximum number of values the file can contain. It is cleaned up only once
       the file is removed from the LRU (see {lru}). *)
    lru_node : string LRU.node; (* LRU node associated with the current file. *)
  }

  let number_of_set_bits (bitset : Lwt_bytes.t) size : int =
    let count = ref 0 in
    for i = 0 to size - 1 do
      (* We don't count the entries being concurrently written (byte
         `\002`) as present because reading them now would fail with a
         Corrupted_data error. *)
      if bitset.{i} = '\001' then count := !count + 1
    done ;
    !count

  (* This datatype represents a virtual file and its current status (opening,
     opened, closing). *)
  type 'value file =
    | Opening of 'value opened_file Lwt.t
    (* The promise is fulfilled only once the file descriptor is opened. *)
    | Closing of unit Lwt.t
  (* The promise is fulfilled only once the file descriptor is closed. *)

  (* This datatype encodes the promise returned by the current action
     performed by the store. *)
  type 'value action_output =
    | Close of unit Lwt.t (* The promise returns by [close] contains no data. *)
    | Read of ('value opened_file option * 'value tzresult) Lwt.t
      (* The promise returned by [read] contains the file read, if it
         exists, as well as the value read if it exists, or an error. *)
    | Write of ('value opened_file * unit tzresult) Lwt.t
      (* The promise returned by [write] contains the file loaded or
         created, and returns nothing (except if an error occured
         during the write). *)
    | Value_exists of ('value opened_file option * bool tzresult) Lwt.t
      (* The promise returned by [value_exists] contains the file read,
         if it exists, as well as the existence of the key. *)
    | Count_values of ('value opened_file option * int tzresult) Lwt.t
      (* The promise returned by [count_values] contains the file read,
         if it exists, as well as the number of keys. *)
    | Remove of unit Lwt.t
  (* The promise returned by [remove] contains nothing. *)

  (* The state of the store. *)
  type 'value t = {
    closed : bool ref;
        (* [true] if the store was closed. Current actions will
           end, and any other actions will fail. *)
    last_actions : 'value action_output Table.t;
        (* [last_actions] contains the last action performed, per file. It must be
           updated atomically when a new action is performed. *)
    files : 'value file Table.t;
        (* [files] is the table of opened, opening, or closing files. It must be
           updated atomically before opening or closing the associated file
           descriptor. *)
    lru : string LRU.t;
        (* [lru] contains the set of names of the files that are opened. It
           ensures there is a limited number of file descriptors opened. *)
  }

  (* The invariant behind this type ensures that

     (A) filename \in lru -> filename \in files /\ filename \in last_actions

     (B) The number of file descriptors opened is bounded by the
     capacity of the LRU

     As a consequence, a read or write in the store can remove another
     file from the LRU. If an action was already performing on such a
     file, the store waits for this action to be terminated and close
     the file before opening a new one.

     Such an eviction explains why the invariant:

     (C) filename \in files -> filename \in lru

     does not hold.

     This store can be shutdown only once. Any other actions performed
     after the store has been closed will fail. The promise returned
     by the close function will be fulfilled only once all the current
     actions will be completed.

     The store ensures that actions performed on a given file are done
     sequentially. *)

  let init ~lru_size =
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/6774

       We may need to support a lockfile to prevent any runtime issues
       if two processes are configured to have R/W access on the same
       storage. We could decide that it is up to the user of this
       module to implement such a mechanism. *)
    let last_actions = Table.create lru_size in
    let files = Table.create lru_size in
    let lru = LRU.create lru_size in
    let closed = ref false in
    {closed; last_actions; files; lru}

  (* The promise returned by this function is fulfilled when the
     current action is completed. The promise returns the opened file
     associated to the action if it exists once the action is
     completed. *)
  let wait_last_action =
    let open Lwt_syntax in
    function
    | Read p ->
        let* file, _ = p in
        return file
    | Close p ->
        let* () = p in
        return_none
    | Write p ->
        let* file, _ = p in
        return_some file
    | Value_exists p ->
        let* file, _ = p in
        return file
    | Count_values p ->
        let* file, _ = p in
        return file
    | Remove p ->
        let* () = p in
        return_none

  (* This function is the only one that calls [Lwt_unix.close]. *)
  let close_opened_file opened_file = Lwt_unix.close opened_file.fd

  (* The 'n'th byte of the bitset indicates whether a value is stored or not. *)
  let key_exists handle index =
    if handle.bitset.{index} = '\000' then `Not_found
    else if handle.bitset.{index} = '\001' then `Found
    else `Corrupted

  (* This computation relies on the fact that the size of all the
     values are fixed, and the values are stored after the bitset. *)
  let position_of layout index =
    bitset_size + (index * layout.value_size) |> Int64.of_int

  let read_with_opened_file layout opened_file key =
    let open Lwt_syntax in
    let index = layout.index_of key in
    let filepath = layout.filepath in
    match key_exists opened_file index with
    | `Not_found ->
        return
          ( Some opened_file,
            Error
              (Error_monad.TzTrace.make
                 (Missing_stored_kvs_data {filepath = layout.filepath; index}))
          )
    | `Corrupted ->
        return
          ( Some opened_file,
            Error
              (Error_monad.TzTrace.make
                 (Corrupted_data {action = "read"; filepath; index})) )
    | `Found -> (
        match Cache.find opened_file.cache index with
        | None -> (
            (* If the value is not in the cache, we do an "I/O" via mmap. *)
            (* Note that the following code executes atomically Lwt-wise. *)
            let pos = position_of layout index in
            let mmap =
              Lwt_bytes.map_file
                ~fd:(Lwt_unix.unix_file_descr opened_file.fd)
                ~pos
                ~size:layout.value_size
                ~shared:true
                ()
            in
            let bytes = Bytes.make layout.value_size '\000' in
            Lwt_bytes.blit_to_bytes mmap 0 bytes 0 layout.value_size ;
            let data =
              catch_f
                (fun () ->
                  Data_encoding.Binary.of_bytes_exn layout.encoding bytes)
                (fun _ -> Encoding_failed {filepath; index})
            in
            match data with
            | Error err -> Lwt.return (Some opened_file, Error err)
            | Ok data ->
                Cache.add opened_file.cache index data ;
                return (Some opened_file, Ok data))
        | Some v -> return (Some opened_file, Ok v))

  let write_with_opened_file ~override layout opened_file key data =
    let open Lwt_syntax in
    let index = layout.index_of key in
    let filepath = layout.filepath in
    let key_already_present = key_exists opened_file index in
    match (key_already_present, override) with
    | `Corrupted, false ->
        Lwt.return
          ( opened_file,
            Error
              (Error_monad.TzTrace.make
                 (Corrupted_data {action = "write"; filepath; index})) )
    | `Found, false -> Lwt.return (opened_file, Ok ())
    | `Corrupted, true | `Found, true | `Not_found, _ -> (
        let pos = position_of layout index in
        let mmap =
          Lwt_bytes.map_file
            ~fd:(Lwt_unix.unix_file_descr opened_file.fd)
            ~pos
            ~size:layout.value_size
            ~shared:true
            ()
        in
        let bytes =
          catch_f
            (fun () -> Data_encoding.Binary.to_bytes_exn layout.encoding data)
            (fun _ -> Encoding_failed {filepath; index})
        in
        match bytes with
        | Error err -> return (opened_file, Error err)
        | Ok bytes ->
            let encoded_size = Bytes.length bytes in
            (* We check that the encoded size is the expected one. *)
            if encoded_size <> layout.value_size then
              Lwt.return
                ( opened_file,
                  Error
                    (Error_monad.TzTrace.make
                       (Wrong_encoded_value_size
                          {
                            file = layout.filepath;
                            index;
                            expected = layout.value_size;
                            got = encoded_size;
                          })) )
            else (
              (* This is necessary only when overriding values. *)
              opened_file.bitset.{index} <- '\002' ;
              Lwt_bytes.blit_from_bytes bytes 0 mmap 0 layout.value_size ;
              Cache.replace opened_file.cache index data ;
              opened_file.bitset.{index} <- '\001' ;
              (* If the key was not yet present, increment the [count] field *)
              let opened_file =
                if key_already_present = `Not_found then
                  {opened_file with count = opened_file.count + 1}
                else opened_file
              in
              return (opened_file, Ok ())))

  let remove_with_opened_file files lru filepath opened_file =
    let open Lwt_syntax in
    let* () = close_opened_file opened_file in
    (* It may happen that the node was already evicted by a concurrent
       action. Hence [LRU.remove] can fail. *)
    (try LRU.remove lru opened_file.lru_node with _ -> ()) ;
    Table.remove files filepath ;
    Lwt_unix.unlink filepath

  module Action = struct
    let get_file_from_last_action files last_actions filepath =
      let open Lwt_syntax in
      let last_or_concurrent_action = Table.find_opt last_actions filepath in
      (* If an action is happening concurrently on the file, we wait
         for it to end.
         The action returns the opened file if any. *)
      match last_or_concurrent_action with
      | None -> (
          let file_cached = Table.find_opt files filepath in
          match file_cached with
          | None -> Lwt.return_none
          | Some (Closing p) ->
              let* () = p in
              Lwt.return_none
          | Some (Opening p) ->
              let* opened_file = p in
              Lwt.return_some opened_file)
      | Some action -> wait_last_action action

    (* Any action on the key value store can be implemented in this way. *)
    let generic_action files last_actions filepath ~on_file_closed
        ~on_file_opened =
      let open Lwt_syntax in
      let* opened_file_opt =
        get_file_from_last_action files last_actions filepath
      in
      match opened_file_opt with
      | None -> on_file_closed ~on_file_opened
      | Some opened_file -> on_file_opened opened_file

    let close_file files last_actions filepath =
      let on_file_closed ~on_file_opened:_ = Lwt.return_unit in
      let on_file_opened opened_file = close_opened_file opened_file in
      generic_action files last_actions filepath ~on_file_closed ~on_file_opened

    let read ~on_file_closed files last_actions layout key =
      let on_file_opened opened_file =
        read_with_opened_file layout opened_file key
      in
      generic_action
        files
        last_actions
        layout.filepath
        ~on_file_closed
        ~on_file_opened

    let value_exists ~on_file_closed files last_actions layout key =
      let on_file_opened opened_file =
        let index = layout.index_of key in
        let filepath = layout.filepath in
        match key_exists opened_file index with
        | `Corrupted ->
            Lwt.return
              ( Some opened_file,
                Error
                  (Error_monad.TzTrace.make
                     (Corrupted_data {action = "value_exists"; filepath; index}))
              )
        | `Found -> Lwt.return (Some opened_file, Ok true)
        | `Not_found -> Lwt.return (Some opened_file, Ok false)
      in
      generic_action
        files
        last_actions
        layout.filepath
        ~on_file_closed
        ~on_file_opened

    let count_values ~on_file_closed files last_actions layout =
      let on_file_opened opened_file =
        Lwt.return (Some opened_file, Ok opened_file.count)
      in
      generic_action
        files
        last_actions
        layout.filepath
        ~on_file_closed
        ~on_file_opened

    let write ~on_file_closed ~override files last_actions layout key data =
      let on_file_opened opened_file =
        write_with_opened_file ~override layout opened_file key data
      in
      generic_action
        files
        last_actions
        layout.filepath
        ~on_file_closed
        ~on_file_opened

    let remove_file ~on_file_closed files last_actions lru filepath =
      let on_file_opened opened_file =
        remove_with_opened_file files lru filepath opened_file
      in
      generic_action files last_actions filepath ~on_file_closed ~on_file_opened
  end

  let close_file files last_actions filepath =
    (* Since this function does not aim to be exposed, we do not check
       whether the store is closed. This would actually be a mistake
       since it is used while the store is closing.

       Moreover, we do not remove the file from the LRU. The reason is
       this function is called twice:

       - when the file is evicted from the LRU (so it was already removed)

       - when closing the store. In that case, after closing the store
       we clean up the LRU.
    *)
    let open Lwt_syntax in
    (* [p] is a promise that triggers the action of closing the
       file. It is important to not wait on it so that we can update
       the store's last_actions atomically to ensure invariant (A). *)
    let p = Action.close_file files last_actions filepath in

    Table.replace files filepath (Closing p) ;
    Table.replace last_actions filepath (Close p) ;
    let* () = p in
    Table.remove files filepath ;
    (* To avoid any memory leaks, we woud like to remove the
       corresponding entry from the [last_actions] table. However, while
       closing the file, another action could have been performed. In
       that case, we don't want to remove the corresponding entry in
       the [last_actions] table.

       Hence, we remove only entries if no other concurrent actions
       happened while closing the file (except closing the very same
       file). *)
    (match Table.find_opt last_actions filepath with
    | Some (Close p) -> (
        match Lwt.state p with
        | Lwt.Return _ -> Table.remove last_actions filepath
        | _ -> ())
    | _ -> ()) ;
    Lwt.return_unit

  (* The promise returned by this function is fullfiled once all the
     current actions are completed and all the opened files are
     closed. This function should be idempotent. *)
  let close {last_actions; files; lru; closed} =
    let open Lwt_syntax in
    if !closed then return_unit
    else (
      closed := true ;
      let* () =
        Table.iter_s
          (fun filename _ -> close_file files last_actions filename)
          files
      in
      LRU.clear lru ;
      return_unit)

  (* This function returns the lru node added and a promise for
     closing the file evicted by the LRU. *)
  let add_lru files last_actions lru filename =
    let open Lwt_syntax in
    let lru_node, remove = LRU.add_and_return_erased lru filename in
    match remove with
    | None -> return lru_node
    | Some filepath ->
        (* We want to ensure that the number of file descriptors opened
           is bounded by the size of the LRU. This is why we wait first
           for the eviction promise to be fulfilled that will close the
           file evicted. *)
        let* () = close_file files last_actions filepath in
        return lru_node

  (* This function aims to be used when the file already exists on the
     file system. *)
  let load_file files last_actions lru filename =
    let open Lwt_syntax in
    let* lru_node = add_lru files last_actions lru filename in
    let* fd = Lwt_unix.openfile filename [O_RDWR; O_CLOEXEC] 0o660 in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6033
       Should we check that the file is at least as big as the bitset? *)
    let bitset =
      Lwt_bytes.map_file
        ~fd:(Lwt_unix.unix_file_descr fd)
        ~shared:true
        ~size:bitset_size
        ()
    in
    return
      {
        fd;
        bitset;
        count = number_of_set_bits bitset bitset_size;
        cache = Cache.create 101;
        lru_node;
      }

  (* This function aims to be used when a write action is performed on
     a file that does not exist yet. *)
  let initialize_file files last_actions lru filename value_size =
    let open Lwt_syntax in
    let* lru_node = add_lru files last_actions lru filename in
    let* fd =
      Lwt_unix.openfile filename [O_RDWR; O_CREAT; O_EXCL; O_CLOEXEC] 0o660
    in
    let total_size = bitset_size + (max_number_of_keys * value_size) in
    let* () = Lwt_unix.ftruncate fd total_size in
    let unix_fd = Lwt_unix.unix_file_descr fd in
    let bitset =
      Lwt_bytes.map_file ~fd:unix_fd ~shared:true ~size:bitset_size ()
    in
    return {fd; bitset; count = 0; cache = Cache.create 101; lru_node}

  (* This function is associated with the [Read] and [Value_exists] actions. *)
  let may_load_file files last_actions lru filepath =
    let open Lwt_syntax in
    let* b = Lwt_unix.file_exists filepath in
    if b then Lwt.return_some (load_file files last_actions lru filepath)
    else Lwt.return_none

  (* This function is associated with the [Remove] action. *)
  let may_remove_file filepath =
    let open Lwt_syntax in
    let* b = Lwt_unix.file_exists filepath in
    if b then Lwt_unix.unlink filepath else Lwt.return_unit

  (* This function is associated with the [Write] action. *)
  let load_or_initialize_file files last_actions lru layout =
    let open Lwt_syntax in
    let* b = Lwt_unix.file_exists layout.filepath in
    if b then load_file files last_actions lru layout.filepath
    else
      initialize_file files last_actions lru layout.filepath layout.value_size

  let read {files; last_actions; lru; closed} layout key =
    let open Lwt_syntax in
    if !closed then
      Lwt.return (Error (Error_monad.TzTrace.make (Closed {action = "read"})))
    else
      let on_file_closed ~on_file_opened =
        let* r = may_load_file files last_actions lru layout.filepath in
        match r with
        | None ->
            let index = layout.index_of key in
            Lwt.return
              ( None,
                Error
                  (Error_monad.TzTrace.make
                     (Missing_stored_kvs_data
                        {filepath = layout.filepath; index})) )
        | Some opened_file_promise ->
            Table.replace files layout.filepath (Opening opened_file_promise) ;
            let* opened_file = opened_file_promise in
            on_file_opened opened_file
      in
      let p = Action.read ~on_file_closed files last_actions layout key in
      Table.replace last_actions layout.filepath (Read p) ;
      let+ _file, value = p in
      value

  (* Very similar to [read] action except we only look at the bitset
     value avoiding one I/O. *)
  let value_exists {files; last_actions; lru; closed} layout key =
    let open Lwt_syntax in
    if !closed then
      Lwt.return
        (Error (Error_monad.TzTrace.make (Closed {action = "value_exists"})))
    else
      let on_file_closed ~on_file_opened =
        let* r = may_load_file files last_actions lru layout.filepath in
        match r with
        | None -> return (None, Ok false)
        | Some opened_file_promise ->
            Table.replace files layout.filepath (Opening opened_file_promise) ;
            let* opened_file = opened_file_promise in
            on_file_opened opened_file
      in
      let p =
        Action.value_exists ~on_file_closed files last_actions layout key
      in
      Table.replace last_actions layout.filepath (Value_exists p) ;
      let+ _, exists = p in
      exists

  (* Very similar to [value_exists] action except we look at the
     [count] counter instead of the bitset. *)
  let count_values {files; last_actions; lru; closed} layout =
    let open Lwt_syntax in
    if !closed then
      Lwt.return
        (Error (Error_monad.TzTrace.make (Closed {action = "count_values"})))
    else
      let on_file_closed ~on_file_opened =
        let* r = may_load_file files last_actions lru layout.filepath in
        match r with
        | None -> return (None, Ok 0)
        | Some opened_file_promise ->
            Table.replace files layout.filepath (Opening opened_file_promise) ;
            let* opened_file = opened_file_promise in
            on_file_opened opened_file
      in
      let p = Action.count_values ~on_file_closed files last_actions layout in
      Table.replace last_actions layout.filepath (Count_values p) ;
      let+ _, count = p in
      count

  let write ?(override = false) {files; last_actions; lru; closed} layout key
      data =
    let open Lwt_syntax in
    if !closed then
      Lwt.return (Error (Error_monad.TzTrace.make (Closed {action = "write"})))
    else
      let on_file_closed ~on_file_opened =
        let opened_file_promise =
          load_or_initialize_file files last_actions lru layout
        in
        Table.replace files layout.filepath (Opening opened_file_promise) ;
        let* opened_file = opened_file_promise in
        on_file_opened opened_file
      in
      let p =
        Action.write
          ~on_file_closed
          ~override
          files
          last_actions
          layout
          key
          data
      in
      Table.replace last_actions layout.filepath (Write p) ;
      let+ _file, result = p in
      result

  let remove {files; last_actions; lru; closed} layout =
    let open Lwt_syntax in
    if !closed then
      Lwt.return (Error (Error_monad.TzTrace.make (Closed {action = "remove"})))
    else
      let on_file_closed ~on_file_opened:_ = may_remove_file layout.filepath in
      let p =
        Action.remove_file
          ~on_file_closed
          files
          last_actions
          lru
          layout.filepath
      in
      Table.replace last_actions layout.filepath (Remove p) ;
      Table.replace files layout.filepath (Closing p) ;
      let* () = p in
      (* See [close_file] for an explanation of the lines below. *)
      (match Table.find_opt last_actions layout.filepath with
      | Some (Close p) -> (
          match Lwt.state p with
          | Lwt.Return _ -> Table.remove last_actions layout.filepath
          | _ -> ())
      | _ -> ()) ;
      return_ok ()
end

let layout ?encoded_value_size ~encoding ~filepath ~eq ~index_of () =
  match encoded_value_size with
  | Some value_size -> {filepath; eq; encoding; index_of; value_size}
  | None -> (
      match Data_encoding.classify encoding with
      | `Fixed value_size -> {filepath; eq; encoding; index_of; value_size}
      | `Dynamic | `Variable ->
          invalid_arg
            "Key_value_store.layout: encoding does not have fixed size")

(* Main data-structure of the store.

   Each physical file may have a different layout.
*)
type ('file, 'key, 'value) t = {
  files : 'value Files.t;
  root_dir : string;
  lockfile : Lwt_unix.file_descr;
}

type ('file, 'key, 'value) file_layout =
  root_dir:string -> 'file -> ('key, 'value) layout

let with_lockfile_lock fn f =
  let open Lwt_result_syntax in
  let* fd =
    Lwt.catch
      (fun () ->
        let*! fd =
          Lwt_unix.openfile fn [Unix.O_CLOEXEC; O_CREAT; O_RDWR] 0o644
        in
        Lwt.return_ok fd)
      (function
        | Unix.Unix_error (unix_code, caller, arg) ->
            tzfail
              (Lwt_utils_unix.Io_error {action = `Open; unix_code; caller; arg})
        | exn -> Lwt.reraise exn)
  in
  Lwt.catch
    (fun () ->
      (* Fails if the lockfile is already taken by another process *)
      let*! () = Lwt_unix.lockf fd F_TLOCK 0 in
      f fd)
    (function
      | Unix.Unix_error (unix_code, caller, arg) ->
          let* () =
            Lwt.catch
              (fun () ->
                let*! () = Lwt_unix.close fd in
                return_unit)
              (function
                | Unix.Unix_error (unix_code, caller, arg) ->
                    tzfail
                      (Lwt_utils_unix.Io_error
                         {action = `Close; unix_code; caller; arg})
                | exn -> Lwt.reraise exn)
          in
          tzfail
            (Lwt_utils_unix.Io_error {action = `Lock; unix_code; caller; arg})
      | exn -> Lwt.reraise exn)

let lockfile_unlock fd =
  let open Lwt_result_syntax in
  let* () =
    Lwt.catch
      (fun () ->
        let*! () = Lwt_unix.lockf fd Unix.F_ULOCK 0 in
        return_unit)
      (function
        | Unix.Unix_error (Unix.EBADF, _, _) ->
            let*! () = Events.(emit warn_non_opened_file_descriptor ()) in
            return_unit
        | Unix.Unix_error (unix_code, caller, arg) ->
            tzfail
              (Lwt_utils_unix.Io_error {action = `Lock; unix_code; caller; arg})
        | exn -> Lwt.reraise exn)
  in
  Lwt.catch
    (fun () ->
      let*! () = Lwt_unix.close fd in
      return_unit)
    (function
      | Unix.Unix_error (Unix.EBADF, _, _) ->
          let*! () = Events.(emit warn_non_opened_file_descriptor ()) in
          return_unit
      | Unix.Unix_error (unix_code, caller, arg) ->
          tzfail
            (Lwt_utils_unix.Io_error {action = `Close; unix_code; caller; arg})
      | exn -> Lwt.reraise exn)

let init ~lru_size ~root_dir =
  let open Lwt_result_syntax in
  let*! () =
    if not (Sys.file_exists root_dir) then Lwt_utils_unix.create_dir root_dir
    else Lwt.return_unit
  in
  with_lockfile_lock (Filename.concat root_dir ".lock") @@ fun fd ->
  return {files = Files.init ~lru_size; root_dir; lockfile = fd}

let close t =
  let open Lwt_result_syntax in
  let*! () = Files.close t.files in
  lockfile_unlock t.lockfile

let write_value :
    type file key value.
    ?override:bool ->
    (file, key, value) t ->
    (file, key, value) file_layout ->
    file ->
    key ->
    value ->
    unit tzresult Lwt.t =
 fun ?override {files; root_dir; _} file_layout file key value ->
  let layout = file_layout ~root_dir file in
  Files.write ?override files layout key value

let read_value :
    type file key value.
    (file, key, value) t ->
    (file, key, value) file_layout ->
    file ->
    key ->
    value tzresult Lwt.t =
 fun {files; root_dir; _} file_layout file key ->
  let layout = file_layout ~root_dir file in
  Files.read files layout key

let value_exists :
    type file key value.
    (file, key, value) t ->
    (file, key, value) file_layout ->
    file ->
    key ->
    bool tzresult Lwt.t =
 fun {files; root_dir; _} file_layout file key ->
  let layout = file_layout ~root_dir file in
  Files.value_exists files layout key

let count_values :
    type file key value.
    (file, key, value) t ->
    (file, key, value) file_layout ->
    file ->
    int tzresult Lwt.t =
 fun {files; root_dir; _} file_layout file ->
  let layout = file_layout ~root_dir file in
  Files.count_values files layout

let write_values ?override t file_layout seq =
  Seq.ES.iter
    (fun (file, key, value) ->
      write_value ?override t file_layout file key value)
    seq

let read_values t file_layout seq =
  let open Lwt_syntax in
  Seq_s.of_seq seq
  |> Seq_s.S.map (fun (file, key) ->
         let* maybe_value = read_value t file_layout file key in
         return (file, key, maybe_value))

let values_exist t file_layout seq =
  let open Lwt_syntax in
  Seq_s.of_seq seq
  |> Seq_s.S.map (fun (file, key) ->
         let* maybe_value = value_exists t file_layout file key in
         return (file, key, maybe_value))

let remove_file {files; root_dir; _} file_layout file =
  let layout = file_layout ~root_dir file in
  Files.remove files layout
