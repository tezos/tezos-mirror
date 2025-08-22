(* The MIT License

   Copyright (c) 2019 Craig Ferguson <craig@tarides.com>
                      Thomas Gazagnaire <thomas@tarides.com>
                      Ioana Cristescu <ioana@tarides.com>
                      Clément Pascutto <clement@tarides.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software. *)

include Index_intf
open! Import

module Key = struct
  module type S = Key

  module String_fixed = Data.String_fixed
end

module Value = struct
  module type S = Value

  module String_fixed = Data.String_fixed
end

exception RO_not_allowed
(** Raised whenever a read-only instance performs a write action. *)

exception RW_not_allowed
(** Raised whenever a read-write instance performs a reserved read-only action. *)

exception Closed
(** Raised whenever a closed instance is used. *)

module Make_private
    (K : Key)
    (V : Value)
    (Platform : Platform.S)
    (Cache : Cache.S) =
struct
  open Platform

  type 'a async = 'a Thread.t

  let await = Thread.await

  type key = K.t
  type value = V.t

  let pp_key = Repr.pp K.t
  let pp_value = Repr.pp V.t

  module Log_file = Log_file.Make (IO) (K) (V)

  module Lru : sig
    type t

    val create : int -> t
    val add : t -> key -> value -> unit
    val find : t -> key -> value option
    val clear : t -> unit
  end = struct
    module Lru =
      Lru.M.Make
        (K)
        (struct
          type t = V.t

          let weight _ = 1
        end)

    type t = Lru.t ref

    let create n = ref (Lru.create n)

    let find t k =
      let result = Lru.find k !t in
      let () =
        match result with
        | None -> Stats.incr_nb_lru_misses ()
        | Some _ -> Stats.incr_nb_lru_hits ()
      in
      result

    (* NOTE: the provided [add] implementation never discards elements from the
       LRU, and the user must manually discard any excess elements using [trim].
       For safety, we shadow [add] with a re-implementation that always trims
       after adding. *)
    let add t k v =
      let t = !t in
      Lru.add k v t;
      Lru.trim t

    let clear t = t := Lru.create (Lru.capacity !t)
  end

  module Entry = struct
    include Log_file.Entry
    module Key = K
    module Value = V

    let to_key { key; _ } = key
    let to_value { value; _ } = value
  end

  module Stats = struct
    include Stats.Make (Clock)
    include Stats
  end

  module IO = struct
    include Io.Extend (IO)

    let iter ?min ?max f =
      let page_size = Int63.(mul Entry.encoded_sizeL (of_int 1_000)) in
      iter ~page_size ?min ?max (fun ~off ~buf ~buf_off ->
          let entry = Entry.decode buf buf_off in
          f off entry;
          Entry.encoded_size)
  end

  type throttle = [ `Overcommit_memory | `Block_writes ]

  type config = {
    log_size : int;  (** The log maximal size before triggering a [merge]. *)
    lru_size : int;
    readonly : bool;
    fresh : bool;
        (** Whether the index was created from scratch, erasing existing data,
            if any. *)
    throttle : throttle;  (** The throttle strategy used when merges stack. *)
    flush_callback : unit -> unit;
        (** A callback called whenever the index is flushed. Useful to sync
            index's flushes with the flushes of larger systems. *)
  }

  type index = {
    io : IO.t;  (** The disk file handler. *)
    fan_out : [ `Read ] Fan.t;
        (** The fan-out, used to map keys to small intervals in the file, in
            constant time. This is an in-memory object, also encoded in the
            header of [io]. *)
  }

  type instance = {
    config : config;
    root : string;  (** The root location of the index *)
    mutable generation : int63;
        (** The generation is a counter of rewriting operations (e.g. [clear]
            and [merge]). It is used to sync RO instances. *)
    mutable index : index option;
        (** The main index file contains old sorted bindings. It is [None] when
            no [merge] occurred yet. On RO instances, this is also [None] when
            the file is empty, e.g. after a [clear]. *)
    mutable log : Log_file.t option;
        (** The log file contains the most recent bindings. It can be [None]
            when a read-only instance is created before its RW counterpart; in
            that case the [log] creation is pending. *)
    mutable log_async : Log_file.t option;
        (** The log_async file contains bindings added concurrently to a [merge]
            operation. It is only present when a merge is ongoing. *)
    mutable open_instances : int;
        (** The number of open instances that are shared through the [Cache.t]. *)
    mutable lru : Lru.t;
    writer_lock : IO.Lock.t option;
        (** A lock that prevents multiple RW instances to be open at the same
            time. *)
    sync_lock : Semaphore.t;
        (** A lock that prevents multiple [sync] to happen at the same time. *)
    merge_lock : Semaphore.t;
        (** A lock that prevents multiple [merges] to happen at the same time. *)
    rename_lock : Semaphore.t;
        (** A lock used to protect a critical bit when finalizing a [merge]
            operation. All operations should be guarded by this lock. *)
    mutable pending_cancel : bool;
        (** A signal for the merging thread to terminate prematurely. *)
  }

  include Private_types

  let check_pending_cancel instance =
    match instance.pending_cancel with true -> `Abort | false -> `Continue

  (* [t] is an [option ref] to handle [close] operations. A closed index is None. *)
  type t = instance option ref

  let check_open t =
    match !t with Some instance -> instance | None -> raise Closed

  (** {1 Clear} *)

  let clear' ~hook t =
    let t = check_open t in
    Log.debug (fun l -> l "clear %S" t.root);
    if t.config.readonly then raise RO_not_allowed;
    t.pending_cancel <- true;
    hook `Abort_signalled;
    Semaphore.with_acquire "clear" t.merge_lock (fun () ->
        t.pending_cancel <- false;
        t.generation <- Int63.succ t.generation;
        let log = Option.get t.log in
        let hook () = hook `IO_clear in
        t.lru <- Lru.create t.config.lru_size;
        Log_file.clear ~generation:t.generation ~reopen:true ~hook log;
        Option.iter
          (fun l -> Log_file.clear ~generation:t.generation ~reopen:false l)
          t.log_async;
        Option.iter
          (fun (i : index) ->
            IO.clear ~generation:t.generation ~reopen:false i.io)
          t.index;
        t.index <- None;
        t.log_async <- None)

  let clear = clear' ~hook:(fun _ -> ())

  (** {1 Flush} *)

  let flush_instance ?no_async ?no_callback ?(with_fsync = false) instance =
    Log.debug (fun l ->
        l "[%s] flushing instance" (Filename.basename instance.root));
    if instance.config.readonly then raise RO_not_allowed;
    instance.log
    |> Option.iter (fun log ->
           Log.debug (fun l ->
               l "[%s] flushing log" (Filename.basename instance.root));
           Log_file.flush ?no_callback ~with_fsync log);

    match (no_async, instance.log_async) with
    | Some (), _ | None, None -> ()
    | None, Some log ->
        Log.debug (fun l ->
            l "[%s] flushing log_async" (Filename.basename instance.root));
        Log_file.flush ?no_callback ~with_fsync log

  let flush ?no_callback ?(with_fsync = false) t =
    let t = check_open t in
    Log.debug (fun l -> l "[%s] flush" (Filename.basename t.root));
    Semaphore.with_acquire "flush" t.rename_lock (fun () ->
        flush_instance ?no_callback ~with_fsync t)

  (** Extract [log] and [log_async] for (private) tests. *)

  let log_file_to_list msg log =
    let x =
      Log_file.to_sorted_seq log
      |> Seq.map (fun (e : Entry.t) -> (e.key, e.value))
      |> List.of_seq
    in
    let y = ref [] in
    IO.iter
      (fun _ (e : Entry.t) -> y := (e.key, e.value) :: !y)
      (Log_file.io log);
    if List.length x <> List.length !y then (
      let pp_entry ppf (k, _) = pp_key ppf k in
      let pp = Fmt.Dump.list pp_entry in
      Fmt.epr "consistency error in %s:\nmem : %a\ndisk: %a\n%!" msg pp x pp !y;
      assert false);
    x

  let log t =
    flush t;
    let t = check_open t in
    Option.map (log_file_to_list "log") t.log

  let log_async t =
    flush t;
    let t = check_open t in
    Option.map (log_file_to_list "log_async") t.log_async

  (** {1 RO instances syncing} *)

  (** Loads the log file at [path], if it exists. Used by RO instances to load
      the temporary [log_async], or to fill the [log] field when they have been
      created before their RW counterpart. *)
  let try_load_log t path =
    match IO.v_readonly path with
    | Error `No_file_on_disk -> None
    | Ok io ->
        let log = Log_file.create io in
        Log.debug (fun l ->
            l "[%s] loaded %d entries from %s" (Filename.basename t.root)
              (Log_file.cardinal log) (Filename.basename path));
        Some log

  (** Syncs the [log_async] of the instance by checking on-disk changes. *)
  let sync_log_async ~hook t =
    match t.log_async with
    | None ->
        hook `Reload_log_async;
        t.log_async <- try_load_log t (Layout.log_async ~root:t.root)
    | Some log ->
        let old_generation = t.generation in
        let old_offset = IO.offset (Log_file.io log) in
        let h = IO.Header.get (Log_file.io log) in
        if
          (* the generation has changed *)
          h.generation > Int63.succ old_generation
          ||
          (* the last sync was done between clear(log) and clear(log_async) *)
          (h.generation = Int63.succ old_generation && h.offset = Int63.zero)
        then (
          (* close the file .*)
          Log_file.close log;
          (* check that file is on disk, reopen and reload everything. *)
          hook `Reload_log_async;
          t.log_async <- try_load_log t (Layout.log_async ~root:t.root)
          (* else if the disk offset is greater, reload the newest data. *))
        else if old_offset < h.offset then
          Log_file.sync_entries ~min:old_offset log
          (* else if the offset is lesser, that means the [log_async] was
             cleared, and the generation should have changed. *)
        else if old_offset > h.offset then (
          (* Should never occur, but we can recover by reloading the log from
             scratch rather than just hard failing. *)
          Log.err (fun l ->
              l
                "[%s] log_async IO header monotonicity violated during sync:@,\
                \  offset: %a -> %a@,\
                \  generation: %a -> %a@,\
                 Reloading the log to compensate."
                (Filename.basename t.root) Int63.pp old_offset Int63.pp h.offset
                Int63.pp old_generation Int63.pp h.generation);
          Log_file.reload log)

  (** Syncs the [index] of the instance by checking on-disk changes. *)
  let sync_index t =
    (* Close the file handler to be able to reload it, as the file may have
       changed after a merge. *)
    Option.iter (fun (i : index) -> IO.close i.io) t.index;
    let index_path = Layout.data ~root:t.root in
    match IO.v_readonly index_path with
    | Error `No_file_on_disk -> t.index <- None
    | Ok io ->
        let fan_out = Fan.import ~hash_size:K.hash_size (IO.get_fanout io) in
        (* We maintain that [index] is [None] if the file is empty. *)
        if IO.offset io = Int63.zero then t.index <- None
        else t.index <- Some { fan_out; io }

  (** Syncs an instance entirely, by checking on-disk changes for [log], [sync],
      and [log_async]. *)
  let sync_instance ?(hook = fun _ -> ()) t =
    Semaphore.with_acquire "sync" t.sync_lock @@ fun () ->
    Log.debug (fun l ->
        l "[%s] checking for changes on disk (generation=%a)"
          (Filename.basename t.root) Int63.pp t.generation);
    (* the first sync needs to load the log file. *)
    let () =
      match t.log with
      | None -> t.log <- try_load_log t (Layout.log ~root:t.root)
      | Some _ -> ()
    in
    (* There is a race between sync and merge:

       - At the end of the merge, the entries in log_async are copied
       into log. [merge] starts by calling IO.Header.set(log) with a
       new generation number, copies all the entries and then clear
       log_async.

       - so here we need to make sure we do the same thing in reverse:
       start by syncing [log_async], then read [log]'s headers. At
       worse, [log_async] and [log] might contain duplicated entries,
       but we won't miss any. These entries will be added to [log.mem]
       using Tbl.replace where they will be deduplicated. *)
    sync_log_async ~hook t;
    match t.log with
    | None -> ()
    | Some log ->
        (* This one is the cached offset, from the previous sync. *)
        let log_offset = IO.offset (Log_file.io log) in
        hook `Before_offset_read;
        let h = IO.Header.get (Log_file.io log) in
        hook `After_offset_read;
        if t.generation <> h.generation then (
          (* If the generation has changed, then we need to reload both the
             [index] and the [log]. The new generation is the one on disk. *)
          Log.debug (fun l ->
              l "[%s] generation has changed: %a -> %a"
                (Filename.basename t.root) Int63.pp t.generation Int63.pp
                h.generation);
          hook `Reload_log;
          t.generation <- h.generation;
          Log_file.close log;
          Lru.clear t.lru;
          t.log <- try_load_log t (Layout.log ~root:t.root);
          (* The log file is never removed (even by clear). *)
          assert (t.log <> None);
          sync_index t)
        else if log_offset < h.offset then (
          (* else if the disk offset is greater, we read the newest bindings. *)
          Log.debug (fun l ->
              l "[%s] new entries detected, reading log from disk"
                (Filename.basename t.root));
          Log_file.sync_entries ~min:log_offset log)
        else
          (* Here the disk offset should be equal to the known one. A smaller
             offset should not be possible, because that would mean a [clear] or
             [merge] occurred, which should have changed the generation. *)
          (* TODO: Handle the "impossible" case differently? *)
          Log.debug (fun l ->
              l "[%s] no changes detected" (Filename.basename t.root))

  (** {1 Find and Mem}*)

  module IOArray = Io_array.Make (IO) (Entry)

  module Search =
    Search.Make (Entry) (IOArray)
      (struct
        type t = int

        module Entry = Entry

        let compare : int -> int -> int = compare
        let of_entry e = e.Entry.key_hash
        let of_key = K.hash

        let linear_interpolate ~low:(low_index, low_metric)
            ~high:(high_index, high_metric) key_metric =
          let low_in = float_of_int low_metric in
          let high_in = float_of_int high_metric in
          let target_in = float_of_int key_metric in
          let low_out = Int63.to_float low_index in
          let high_out = Int63.to_float high_index in
          (* Fractional position of [target_in] along the line from [low_in] to [high_in] *)
          let proportion = (target_in -. low_in) /. (high_in -. low_in) in
          (* Convert fractional position to position in output space *)
          let position = low_out +. (proportion *. (high_out -. low_out)) in
          let rounded = ceil (position -. 0.5) +. 0.5 in
          Int63.of_float rounded
      end)

  let interpolation_search index key =
    let hashed_key = K.hash key in
    let low_bytes, high_bytes = Fan.search index.fan_out hashed_key in
    let low, high =
      Int63.
        (div low_bytes Entry.encoded_sizeL, div high_bytes Entry.encoded_sizeL)
    in
    Search.interpolation_search (IOArray.v index.io) key ~low ~high

  (** Finds the value associated to [key] in [t]. In order, checks in
      [log_async] (in memory), then [log] (in memory), then [index] (on disk). *)
  let find_instance t key =
    let find_if_exists ~name ~find db =
      match db with
      | None -> raise Not_found
      | Some e ->
          let ans = find e key in
          Log.debug (fun l ->
              l "[%s] found in %s" (Filename.basename t.root) name);
          ans
    in
    let find_log_async () =
      find_if_exists ~name:"log_async" ~find:Log_file.find t.log_async
    in
    let find_log () = find_if_exists ~name:"log" ~find:Log_file.find t.log in
    let find_index () =
      find_if_exists ~name:"index" ~find:interpolation_search t.index
    in
    Semaphore.with_acquire "find_instance" t.rename_lock @@ fun () ->
    match Lru.find t.lru key with
    | Some e -> e
    | None ->
        let e =
          match find_log_async () with
          | e -> e
          | exception Not_found -> (
              match find_log () with
              | e -> e
              | exception Not_found -> find_index ())
        in
        Lru.add t.lru key e;
        e

  let find t key =
    let t = check_open t in
    Log.debug (fun l -> l "[%s] find %a" (Filename.basename t.root) pp_key key);
    find_instance t key

  let mem t key =
    let t = check_open t in
    Log.debug (fun l -> l "[%s] mem %a" (Filename.basename t.root) pp_key key);
    match find_instance t key with _ -> true | exception Not_found -> false

  let sync' ?hook t =
    let f t =
      Stats.incr_nb_sync ();
      let t = check_open t in
      Log.info (fun l -> l "[%s] sync" (Filename.basename t.root));
      if t.config.readonly then sync_instance ?hook t else raise RW_not_allowed
    in
    Stats.sync_with_timer (fun () -> f t)

  let sync = sync' ?hook:None

  (** {1 Index creation} *)

  let transfer_log_async_to_log ~root ~generation ~log ~log_async =
    let entries = Int63.div (IO.offset log_async) Entry.encoded_sizeL in
    Log.debug (fun l ->
        l "[%s] log_async file detected. Loading %a entries"
          (Filename.basename root) Int63.pp entries);
    (* Can only happen in RW mode where t.log is always [Some _] *)
    match log with
    | None -> assert false
    | Some log ->
        IO.iter (fun _ e -> Log_file.replace log e.key e.value) log_async;
        (* Force fsync here so that persisted entries in log_async
           continue to persist in log. *)
        Log_file.flush ~with_fsync:true log;
        IO.clear ~generation ~reopen:false log_async

  let v_no_cache ?(flush_callback = fun () -> ()) ~throttle ~fresh ~readonly
      ~lru_size ~log_size root =
    Log.debug (fun l ->
        l "[%s] not found in cache, creating a new instance"
          (Filename.basename root));
    let writer_lock =
      if not readonly then Some (IO.Lock.lock (Layout.lock ~root)) else None
    in
    let config =
      {
        log_size = log_size * Entry.encoded_size;
        lru_size;
        readonly;
        fresh;
        throttle;
        flush_callback;
      }
    in
    (* load the [log] file *)
    let log =
      let log_path = Layout.log ~root in
      if readonly then if fresh then raise RO_not_allowed else None
      else
        let io =
          IO.v ~flush_callback ~fresh ~generation:Int63.zero
            ~fan_size:Int63.zero log_path
        in
        let entries = Int63.(to_int_exn (IO.offset io / Entry.encoded_sizeL)) in
        Log.debug (fun l ->
            l "[%s] log file detected. Loading %d entries"
              (Filename.basename root) entries);
        Some (Log_file.create io)
    in
    let generation =
      match log with
      | None -> Int63.zero
      | Some log -> IO.get_generation (Log_file.io log)
    in
    (* load the [log_async] file *)
    let () =
      let log_async_path = Layout.log_async ~root in
      (* - If we are in readonly mode, the log_async will be read
           during sync_log so there is no need to do it here. *)
      if (not readonly) && IO.exists log_async_path then
        let io =
          IO.v ~flush_callback ~fresh ~generation ~fan_size:Int63.zero
            log_async_path
        in
        (* in fresh mode, we need to wipe the existing [log_async] file. *)
        if fresh then IO.clear ~generation ~reopen:false io
        else
          (* If we are not in fresh mode, we move the contents
             of log_async to log. *)
          transfer_log_async_to_log ~root ~generation ~log ~log_async:io
    in
    (* load the [data] file *)
    let index =
      if readonly then None
      else
        let index_path = Layout.data ~root in
        if IO.exists index_path then
          let io =
            (* NOTE: No [flush_callback] on the Index IO as we maintain the
               invariant that any bindings it contains were previously persisted
               in either [log] or [log_async]. *)
            IO.v ?flush_callback:None ~fresh ~generation ~fan_size:Int63.zero
              index_path
          in
          let entries = Int63.div (IO.offset io) Entry.encoded_sizeL in
          if entries = Int63.zero then None
          else (
            Log.debug (fun l ->
                l "[%s] index file detected. Loading %a entries"
                  (Filename.basename root) Int63.pp entries);
            let fan_out =
              Fan.import ~hash_size:K.hash_size (IO.get_fanout io)
            in
            Some { fan_out; io })
        else (
          Log.debug (fun l ->
              l "[%s] no index file detected." (Filename.basename root));
          None)
    in
    {
      config;
      generation;
      log;
      log_async = None;
      root;
      index;
      lru = Lru.create lru_size;
      open_instances = 1;
      merge_lock = Semaphore.make true;
      rename_lock = Semaphore.make true;
      sync_lock = Semaphore.make true;
      writer_lock;
      pending_cancel = false;
    }

  type cache = (string * bool, instance) Cache.t

  let empty_cache = Cache.create

  let v ?(flush_callback = fun () -> ()) ?(cache = empty_cache ())
      ?(fresh = false) ?(readonly = false) ?(throttle = `Block_writes)
      ?(lru_size = 30_000) ~log_size root =
    let new_instance () =
      let instance =
        v_no_cache ~flush_callback ~fresh ~readonly ~log_size ~lru_size
          ~throttle root
      in
      if readonly then sync_instance instance;
      Cache.add cache (root, readonly) instance;
      ref (Some instance)
    in
    Log.info (fun l ->
        l "[%s] v fresh=%b readonly=%b log_size=%d" (Filename.basename root)
          fresh readonly log_size);
    match (Cache.find cache (root, readonly), IO.exists (Layout.log ~root)) with
    | None, _ -> new_instance ()
    | Some _, false ->
        Log.debug (fun l ->
            l "[%s] does not exist anymore, cleaning up the fd cache"
              (Filename.basename root));
        Cache.remove cache (root, true);
        Cache.remove cache (root, false);
        new_instance ()
    | Some t, true -> (
        match t.open_instances with
        | 0 ->
            Cache.remove cache (root, readonly);
            new_instance ()
        | _ ->
            Log.debug (fun l ->
                l "[%s] found in cache" (Filename.basename root));
            t.open_instances <- t.open_instances + 1;
            if readonly then sync_instance t;
            let t = ref (Some t) in
            if fresh then clear t;
            t)

  (** {1 Merges} *)

  (** Appends the entry encoded in [buf] into [dst_io] and registers it in
      [fan_out] with hash [hash]. *)
  let append_substring_fanout fan_out hash dst_io buf ~off ~len =
    Fan.update fan_out hash (IO.offset dst_io);
    IO.append_substring dst_io buf ~off ~len

  (** Appends [entry] into [dst_io] and registers it in [fan_out]. *)
  let append_entry_fanout fan_out entry dst_io =
    Fan.update fan_out entry.Entry.key_hash (IO.offset dst_io);
    Entry.encode entry (IO.append dst_io)

  (** Appends the [log] values into [dst_io], from [log_i] to the first value
      which hash is higher than or equal to [hash_e] (the current value in
      [data]), excluded, and returns its index. Also registers the appended
      values to [fan_out]. *)
  let rec merge_from_log fan_out log hash_e dst_io =
    match log with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (v, _) when v.Entry.key_hash >= hash_e -> log
    | Seq.Cons (v, log) ->
        append_entry_fanout fan_out v dst_io;
        (merge_from_log [@tailcall]) fan_out (log ()) hash_e dst_io

  (** Appends the [log] values into [dst_io], from [log_i] to the end, and
      registers them in [fan_out]. *)
  let append_remaining_log fan_out log dst_io =
    Seq.iter (fun entry -> append_entry_fanout fan_out entry dst_io) log

  (** Merges [log] with [index] into [dst_io], ignoring bindings that do not
      satisfy [filter (k, v)]. [log] must be sorted by key hashes. *)
  let merge_with ~hook ~yield ~filter log index_io fan_out dst_io =
    (* We read the [index] by page; the [refill] function is in charge of
       refilling the page buffer when empty. *)
    let len = 10_000 * Entry.encoded_size in
    let buf = Bytes.create len in
    let refill off = ignore (IO.read index_io ~off ~len buf) in
    let index_end = IO.offset index_io in
    refill Int63.zero;
    let filter =
      Option.fold
        ~none:(fun _ _ -> true)
        ~some:(fun f key entry_off ->
          (* When the filter is not provided, we don't need to decode the value. *)
          let value =
            Entry.decode_value (Bytes.unsafe_to_string buf) entry_off
          in
          f (key, value))
        filter
    in
    (* This performs the merge. [index_offset] is the offset of the next entry
       to process in [index], [buf_offset] is its counterpart in the page
       buffer. [log_i] is the index of the next entry to process in [log]. *)
    let rec go first_entry index_offset buf_offset log =
      (* If the index is fully read, we append the rest of the [log]. *)
      if index_offset >= index_end then (
        append_remaining_log fan_out (fun () -> log) dst_io;
        `Completed)
      else
        let index_offset = Int63.add index_offset Entry.encoded_sizeL in
        let index_key, index_key_hash =
          Entry.decode_key (Bytes.unsafe_to_string buf) buf_offset
        in
        let log = merge_from_log fan_out log index_key_hash dst_io in
        match yield () with
        | `Abort -> `Aborted
        | `Continue ->
            (* This yield is used to balance the resources between the merging
               thread (here) and the main thread. *)
            Thread.yield ();
            (* If the log entry has the same key as the index entry, we do not
               add the index one, respecting the [replace] semantics. *)
            let log_overwrites_index_entry =
              match log with
              | Seq.Nil -> false
              | Seq.Cons (entry, _) -> K.(equal entry.key index_key)
            in
            if (not log_overwrites_index_entry) && filter index_key buf_offset
            then
              append_substring_fanout fan_out index_key_hash dst_io
                (Bytes.unsafe_to_string buf)
                ~off:buf_offset ~len:Entry.encoded_size;
            if first_entry then hook `After_first_entry;
            let buf_offset =
              let n = buf_offset + Entry.encoded_size in
              (* If the buffer is entirely consumed, refill it. *)
              if n >= Bytes.length buf then (
                refill index_offset;
                0)
              else n
            in
            (go [@tailcall]) false index_offset buf_offset log
    in
    (go [@tailcall]) true Int63.zero 0 (log ())

  (** Increases and returns the merge counter. *)
  let merge_counter =
    let n = ref 0 in
    fun () ->
      incr n;
      !n

  (** Merges the entries in [t.log] into the data file, ensuring that concurrent
      writes are not lost.

      The caller must ensure the following:

      - [t.log] has been loaded;
      - [t.log_async] has been created;
      - [t.merge_lock] is acquired before entry, and released immediately after
        this function returns or raises an exception. *)
  let unsafe_perform_merge ~filter ~hook t =
    hook `Before;
    let log = Option.get t.log in
    let generation = Int63.succ t.generation in
    let sorted_log_bindings =
      Log_file.to_sorted_seq log
      |> Option.fold filter ~none:Fun.id ~some:(fun f ->
             Seq.filter (fun (e : Entry.t) -> f (e.key, e.value)))
    in
    let fan_size, old_fan_nb =
      match t.index with
      | None -> (Log_file.cardinal log, None)
      | Some index ->
          ( Int63.(to_int_exn (IO.offset index.io / Entry.encoded_sizeL))
            + Log_file.cardinal log,
            Some (Fan.nb_fans index.fan_out) )
    in
    let fan_out =
      Fan.v ~hash_size:K.hash_size ~entry_size:Entry.encoded_size fan_size
    in
    let () =
      match old_fan_nb with
      | Some on ->
          let new_fan_nb = Fan.nb_fans fan_out in
          if new_fan_nb <> on then
            Log.info (fun m ->
                m "the number of fan-out entries has changed: %d to %d" on
                  new_fan_nb)
      | _ -> ()
    in
    let merge =
      let merge_path = Layout.merge ~root:t.root in
      IO.v ~fresh:true ~generation
        ~fan_size:(Int63.of_int (Fan.exported_size fan_out))
        merge_path
    in
    let merge_result : [ `Index_io of IO.t | `Aborted ] =
      match t.index with
      | None -> (
          match check_pending_cancel t with
          | `Abort -> `Aborted
          | `Continue ->
              let io =
                IO.v ~fresh:true ~generation ~fan_size:Int63.zero
                  (Layout.data ~root:t.root)
              in
              append_remaining_log fan_out sorted_log_bindings merge;
              `Index_io io)
      | Some index -> (
          match
            merge_with ~hook
              ~yield:(fun () -> check_pending_cancel t)
              ~filter sorted_log_bindings index.io fan_out merge
          with
          | `Completed -> `Index_io index.io
          | `Aborted -> `Aborted)
    in
    match merge_result with
    | `Aborted ->
        IO.clear ~generation ~reopen:false merge;
        (`Aborted, Mtime.Span.zero)
    | `Index_io io ->
        let fan_out = Fan.finalize fan_out in
        let index = { io; fan_out } in
        IO.set_fanout merge (Fan.export index.fan_out);
        let before_rename_lock = Clock.counter () in
        let rename_lock_duration =
          Semaphore.with_acquire "merge-rename" t.rename_lock (fun () ->
              let rename_lock_duration = Clock.count before_rename_lock in
              IO.rename ~src:merge ~dst:index.io;
              t.index <- Some index;
              t.generation <- generation;
              (* The filter may have removed some of the bindings that exist in
                 the LRU. We over-approximate by clearing the entire thing. *)
              if Option.is_some filter then Lru.clear t.lru;
              Log_file.clear ~generation ~reopen:true log;
              hook `After_clear;
              let log_async = Option.get t.log_async in
              Log_file.iter log_async ~f:(fun entry ->
                  Log_file.replace log entry.key entry.value);
              (* NOTE: It {i may} not be necessary to trigger the
                 [flush_callback] here. If the instance has been recently
                 flushed (or [log_async] just reached the [auto_flush_limit]),
                 we're just moving already-persisted values around. However, we
                 trigger the callback anyway for simplicity. *)
              (* `fsync` is necessary, since bindings in `log_async` may have
                 been explicitely `fsync`ed during the merge, so we need to
                 maintain their durability. *)
              Log_file.flush ~with_fsync:true log;
              Log_file.clear ~generation:(Int63.succ generation) ~reopen:false
                log_async;
              (* log_async.mem does not need to be cleared as we are discarding
                 it. *)
              t.log_async <- None;
              rename_lock_duration)
        in
        hook `After;
        (`Completed, rename_lock_duration)

  let reset_log_async t =
    let io =
      let log_async_path = Layout.log_async ~root:t.root in
      IO.v ~flush_callback:t.config.flush_callback ~fresh:true
        ~generation:(Int63.succ t.generation) ~fan_size:Int63.zero
        log_async_path
    in
    t.log_async <- Some (Log_file.create io)

  let merge' ?(blocking = false) ?filter ?(hook = fun _ -> ()) ?(force = false)
      t =
    let merge_started = Clock.counter () in
    let merge_id = merge_counter () in
    let msg = Fmt.str "merge { id=%d }" merge_id in
    Semaphore.acquire msg t.merge_lock;
    let merge_lock_wait = Clock.count merge_started in
    Log.info (fun l ->
        let pp_forced ppf () = if force then Fmt.string ppf "; force=true" in
        l "[%s] merge started { id=%d%a }" (Filename.basename t.root) merge_id
          pp_forced ());
    Stats.incr_nb_merge ();

    (* Cleanup previous crashes of the merge thread. *)
    Option.iter
      (fun l ->
        transfer_log_async_to_log ~root:t.root ~generation:t.generation
          ~log:t.log ~log_async:(Log_file.io l))
      t.log_async;
    reset_log_async t;

    (* NOTE: We flush [log] {i after} enabling [log_async] to ensure that no
       unflushed bindings make it into [log] before being merged into the index.
       This satisfies the invariant that all bindings are {i first} persisted in
       a log, so that the [index] IO doesn't need to trigger the
       [flush_callback]. *)
    flush_instance ~no_async:() ~with_fsync:true t;
    let go () =
      let merge_result, rename_lock_wait =
        Fun.protect
          (fun () -> unsafe_perform_merge ~filter ~hook t)
          ~finally:(fun () -> Semaphore.release t.merge_lock)
      in
      let total_duration = Clock.count merge_started in
      let merge_duration = Mtime.Span.abs_diff total_duration merge_lock_wait in
      Stats.add_merge_duration merge_duration;
      Log.info (fun l ->
          let action =
            match merge_result with
            | `Aborted -> "aborted"
            | `Completed -> "completed"
          in
          l
            "[%s] merge %s { id=%d; total-duration=%a; merge-duration=%a; \
             merge-lock=%a; rename-lock=%a }"
            (Filename.basename t.root) action merge_id Mtime.Span.pp
            total_duration Mtime.Span.pp merge_duration Mtime.Span.pp
            merge_lock_wait Mtime.Span.pp rename_lock_wait);
      merge_result
    in
    if blocking then go () |> Thread.return else Thread.async go

  let is_empty t =
    (* A read-only instance may have not yet loaded the [log], if no explicit
       [sync] has yet occurred, leaving some ambiguity as to whether the index
       is strictly "empty". For now, we only need this internal function for
       read-write instances, so we dodge the question .*)
    assert (not t.config.readonly);
    match t.log with
    | None -> true
    | Some log -> Option.is_none t.index && Log_file.cardinal log = 0

  (** This triggers a merge if the [log] exceeds [log_size], or if the [log]
      contains entries and [force] is true *)
  let try_merge_aux ?hook ?(force = false) t =
    let t = check_open t in
    match is_empty t with
    | true ->
        Log.debug (fun l -> l "[%s] index is empty" (Filename.basename t.root));
        Thread.return `Completed
    | false -> (
        match t.log with
        | None ->
            Log.debug (fun l ->
                l "[%s] log is empty" (Filename.basename t.root));
            Thread.return `Completed
        | Some log ->
            if
              force
              || Int63.compare
                   (IO.offset (Log_file.io log))
                   (Int63.of_int t.config.log_size)
                 > 0
            then merge' ~force ?hook t
            else Thread.return `Completed)

  let merge t = ignore (try_merge_aux ?hook:None ~force:true t : _ async)
  let try_merge t = ignore (try_merge_aux ?hook:None ~force:false t : _ async)

  let instance_is_merging t =
    (* [merge_lock] is used to detect an ongoing merge. Other operations can
       take this lock, but as they are not async, we consider this to be a good
       enough approximation. *)
    Semaphore.is_held t.merge_lock

  let is_merging t =
    let t = check_open t in
    if t.config.readonly then raise RO_not_allowed;
    instance_is_merging t

  (** {1 Replace} *)

  let replace' ?hook ?(overcommit = false) t key value =
    let t = check_open t in
    Stats.incr_nb_replace ();
    Log.debug (fun l ->
        l "[%s] replace %a %a" (Filename.basename t.root) pp_key key pp_value
          value);
    if t.config.readonly then raise RO_not_allowed;
    let log_limit_reached =
      Semaphore.with_acquire "replace" t.rename_lock (fun () ->
          let log =
            match t.log_async with Some log -> log | None -> Option.get t.log
          in
          Log_file.replace log key value;
          Lru.add t.lru key value;
          let offset = IO.offset (Log_file.io log) in
          Int63.compare offset (Int63.of_int t.config.log_size) > 0)
    in
    if log_limit_reached && not overcommit then
      let is_merging = instance_is_merging t in
      match (t.config.throttle, is_merging) with
      | `Overcommit_memory, true ->
          (* Do not start a merge, overcommit the memory instead. *)
          None
      | `Overcommit_memory, false | `Block_writes, _ ->
          (* Start a merge, blocking if one is already running. *)
          let hook = hook |> Option.map (fun f stage -> f (`Merge stage)) in
          Some (merge' ?hook t)
    else None

  let replace ?overcommit t key value =
    ignore (replace' ?hook:None ?overcommit t key value : _ async option)

  let replace_with_timer ?sampling_interval t key value =
    match sampling_interval with
    | None -> replace t key value
    | Some sampling_interval ->
        Stats.start_replace ();
        replace t key value;
        Stats.end_replace ~sampling_interval

  (** {1 Filter} *)

  (** [filter] is implemented with a [merge], during which bindings that do not
      satisfy the predicate are not merged. *)
  let filter t f =
    let t = check_open t in
    Log.debug (fun l -> l "[%s] filter" (Filename.basename t.root));
    if t.config.readonly then raise RO_not_allowed;
    match is_empty t with
    | true ->
        Log.debug (fun l -> l "[%s] index is empty" (Filename.basename t.root))
    | false -> (
        match Thread.await (merge' ~blocking:true ~filter:f t) with
        | Ok (`Aborted | `Completed) -> ()
        | Error (`Async_exn exn) ->
            Fmt.failwith "filter: asynchronous exception during merge (%s)"
              (Printexc.to_string exn))

  (** {1 Iter} *)

  let iter f t =
    let t = check_open t in
    Log.debug (fun l -> l "[%s] iter" (Filename.basename t.root));
    match t.log with
    | None -> ()
    | Some log ->
        Log_file.iter log ~f:(fun entry -> f entry.key entry.value);
        Option.iter
          (fun (i : index) -> IO.iter (fun _ e -> f e.key e.value) i.io)
          t.index;
        Semaphore.with_acquire "iter" t.rename_lock (fun () ->
            match t.log_async with
            | None -> ()
            | Some log ->
                Log_file.iter log ~f:(fun entry -> f entry.key entry.value))

  (** {1 Close} *)

  let close' ~hook ?immediately it =
    let abort_merge = Option.is_some immediately in
    match !it with
    | None -> Log.debug (fun l -> l "close: instance already closed")
    | Some t ->
        Log.debug (fun l -> l "[%s] close" (Filename.basename t.root));
        if abort_merge then (
          t.pending_cancel <- true;
          hook `Abort_signalled);
        Semaphore.with_acquire "close" t.merge_lock (fun () ->
            (* The instance is set to [None] prior to closing the resources to
               ensure other clients see it as atomic. *)
            it := None;
            t.open_instances <- t.open_instances - 1;
            (* Resources are closed only if this is the last open instance. *)
            if t.open_instances = 0 then (
              Log.debug (fun l ->
                  l "[%s] last open instance: closing the file descriptor"
                    (Filename.basename t.root));
              if not t.config.readonly then flush_instance ~with_fsync:true t;
              Option.iter Log_file.close t.log;
              Option.iter (fun (i : index) -> IO.close i.io) t.index;
              Option.iter (fun lock -> IO.Lock.unlock lock) t.writer_lock))

  let close = close' ~hook:(fun _ -> ())

  module Checks = Checks.Make (K) (V) (Platform)
end

module Cache = Cache
module Checks = Checks
module Make = Make_private
module Platform = Platform
module Stats = Stats

module Private = struct
  module Fan = Fan
  module Io = Io
  module Io_array = Io_array
  module Search = Search
  module Data = Data
  module Layout = Layout
  module Logs = Log

  module Hook = struct
    type 'a t = 'a -> unit

    let v f = f
  end

  module type S = Private with type 'a hook := 'a Hook.t

  module Make = Make_private
end
