(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** The store module is a pointer to the latest version. However, in order to
    run long migrations in the background, this module allows to have a store
    which is a hybrid version of the last two versions. *)

include Store_v5

(* TODO: When the V5 store has been in production for long enough, all the below
   code can be removed. *)

(** The hybrid store. *)
type 'a hybrid = {
  v4 : Store_v4.ro;  (** A read-only old V4 store. *)
  v5 : 'a Store_v5.t;
      (** A read-write V5 store to which we migrate data and which we use to
          store new data. *)
  migration_done : bool ref;
      (** A flag to indicate when the migration has terminated. When this is the
          case we only query the v5 store.  *)
}

(** The type of stores. *)
type nonrec 'a t =
  | Hybrid of 'a hybrid
      (** Either a hybrid store when a bacground migration is running.  *)
  | Normal of 'a t  (** Or a "normal" store, for the nominal case.  *)

type rw = Store_sigs.rw t

type ro = Store_sigs.ro t

let with_rw (type a) (mode : a Store_sigs.mode) (f : unit -> rw tzresult Lwt.t)
    : a t tzresult Lwt.t =
  match mode with
  | Read_only ->
      failwith
        "The rollup node store needs a migration but was opened in read-only \
         mode."
  | Read_write -> f ()

(** Initialize a store and run the corresponding migrations if needed. For the
    v4 -> v5 transition, the migration is run in the background. *)
let init_with_migration (type a) (mode : a Store_sigs.mode) metadata ~data_dir :
    a t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let storage_dir = Filename.concat data_dir "storage" in
  let* current_version = Store_version.read_version_file ~dir:storage_dir in
  match (current_version, version) with
  | Some V4, V5_sqlite ->
      with_rw mode @@ fun () ->
      Format.eprintf
        "Hybrid V4/V5 store while migration runs in the background@." ;
      let migration_done = ref false in
      let* v4 =
        Store_v4.load
          Read_only
          ~index_buffer_size:1000
          ~l2_blocks_cache_size:64
          storage_dir
      and* v5 = Store_v5.init Read_write ~data_dir in
      Lwt.dont_wait
        (fun () ->
          let*! r =
            protect @@ fun () ->
            (* Migrate in place *)
            Store_migration.V5_sqlite_migrations.From_v4.migrate_between_stores
              metadata
              ~data_dir
              ~dest_data_dir:data_dir
              v4
              v5
          in
          Format.eprintf "Migration thread terminated@." ;
          let*! (Ok () | Error _) = Store_v4.close v4 in
          (match r with
          | Ok () -> migration_done := true
          | Error e -> Format.eprintf "Migration error: %a@." pp_print_trace e) ;
          Lwt.return_unit)
        (fun exn ->
          Format.eprintf "Migration exception: %s@." (Printexc.to_string exn)) ;
      let store = {migration_done; v4; v5} in
      return (Hybrid store)
  | _ ->
      let* () =
        Store_migration.maybe_run_migration metadata version ~data_dir
      in
      let+ store = init mode ~data_dir in
      Normal store

let close = function
  | Normal s -> close s
  | Hybrid {v4; v5; _} ->
      let open Lwt_syntax in
      let* () = close v5 and* _ = Store_v4.close v4 in
      return_unit

let readonly = function
  | Normal s -> Normal (readonly s)
  | Hybrid {v4; v5; migration_done} ->
      Hybrid {v4 = Store_v4.readonly v4; v5 = readonly v5; migration_done}

let gc = function
  | Normal s -> gc s
  | Hybrid {v5; _} ->
      (* Only run GC on new store, the old one will be removed *)
      gc v5

let read_from = function
  | Hybrid {v5; migration_done = {contents = true}; _} ->
      (* If the migration is done we only read in the V5 store. *)
      Normal v5
  | store -> store

let write_to = function
  | Normal v5 | Hybrid {v5; _} ->
      (* Always write to the V5 store. *)
      v5

(** [x @?~~> y] executes [x] first and then [y] if [x] resolves to [None] (where
    [x] is an access to the V5 store and [y] to the V4 store). *)
let ( @?~~> ) t1 t2 =
  let open Lwt_result_syntax in
  let*! r = t1 in
  (match r with
  | Ok _ -> ()
  | Error e -> Format.eprintf "Store v5 error: %a@." pp_print_trace e) ;
  let*? r in
  match r with
  | Some r -> return_some r
  | None ->
      let*! r = t2 in
      (match r with
      | Ok _ -> ()
      | Error e -> Format.eprintf "Store v4 error: %a@." pp_print_trace e) ;
      Lwt.return r

module Commitments = struct
  let store s c = Commitments.store (write_to s) c

  let find s h =
    match read_from s with
    | Normal s -> Commitments.find s h
    | Hybrid {v4; v5; _} ->
        Commitments.find v5 h
        @?~~> (Store_v4.Commitments.read v4.commitments h
              |> Lwt_result.map @@ Option.map fst)

  let find_lcc s =
    match read_from s with
    | Normal s -> Commitments.find_lcc s ()
    | Hybrid {v4; v5; _} -> (
        Commitments.find_lcc v5 ()
        @?~~>
        let open Lwt_result_syntax in
        let* r = Store_v4.Lcc.read v4.lcc in
        match r with
        | None -> return_none
        | Some {commitment; _} ->
            Store_v4.Commitments.read v4.commitments commitment
            |> Lwt_result.map @@ Option.map fst)

  let find_lpc s =
    match read_from s with
    | Normal s -> Commitments.find_lpc s
    | Hybrid {v4; v5; _} ->
        Commitments.find_lpc v5 @?~~> Store_v4.Lpc.read v4.lpc
end

module Commitments_published_at_levels = struct
  include Commitments_published_at_levels

  let register s h levels = register (write_to s) h levels

  let get s h =
    match read_from s with
    | Normal s -> get s h
    | Hybrid {v4; v5; _} -> (
        get v5 h
        @?~~>
        let open Lwt_result_syntax in
        let+ r =
          Store_v4.Commitments_published_at_level.find
            v4.commitments_published_at_level
            h
        in
        match r with
        | None -> None
        | Some {first_published_at_level; published_at_level} ->
            Some {first_published_at_level; published_at_level})

  let get_first_published_level s h =
    let open Lwt_result_syntax in
    let+ r = get s h in
    match r with
    | None -> None
    | Some {first_published_at_level; _} -> Some first_published_at_level
end

module Inboxes = struct
  let store s i = Inboxes.store (write_to s) i

  let find s h =
    match read_from s with
    | Normal s -> Inboxes.find s h
    | Hybrid {v4; v5; _} ->
        Inboxes.find v5 h
        @?~~> (Store_v4.Inboxes.read v4.inboxes h
              |> Lwt_result.map @@ Option.map fst)

  let find_by_block_hash s h =
    match read_from s with
    | Normal s -> Inboxes.find_by_block_hash s h
    | Hybrid {v4; v5; _} -> (
        Inboxes.find_by_block_hash v5 h
        @?~~>
        let open Lwt_result_syntax in
        let* b = Store_v4.L2_blocks.header v4.l2_blocks h in
        match b with
        | None -> return_none
        | Some {inbox_hash; _} ->
            Store_v4.Inboxes.read v4.inboxes inbox_hash
            |> Lwt_result.map @@ Option.map fst)
end

module Messages = struct
  let store s i = Messages.store (write_to s) i

  let find s h =
    match read_from s with
    | Normal s -> Messages.find s h
    | Hybrid {v4; v5; _} ->
        Messages.find v5 h
        @?~~> (Store_v4.Messages.read v4.messages h
              |> Lwt_result.map @@ Option.map fst)
end

module Outbox_messages = struct
  let pending s ~min_level ~max_level =
    match read_from s with
    | Normal s -> Outbox_messages.pending s ~min_level ~max_level
    | Hybrid {v4; v5; _} ->
        let open Lwt_result_syntax in
        let* p1 = Outbox_messages.pending v5 ~min_level ~max_level
        and* p2 =
          Store_v4.Outbox_messages.pending
            v4.outbox_messages
            ~min_level
            ~max_level
        in
        let rec merge acc p1 p2 =
          match (p1, p2) with
          | (l1, m1) :: r1, (l2, m2) :: r2 when l1 = l2 ->
              (* Pending lists (for both stores) are obtained by taking whole
                 messages for this level and removing the executed ones. The
                 pending messages are the ones that are still pending both in v4
                 and v5. *)
              let m =
                List.filter (fun x -> List.mem ~equal:Int.equal x m2) m1
              in
              merge ((l1, m) :: acc) r1 r2
          | (l1, m1) :: r1, (l2, _) :: _ when l1 < l2 ->
              merge ((l1, m1) :: acc) r1 p2
          | (_, _) :: _, (l2, m2) :: r2 -> merge ((l2, m2) :: acc) p1 r2
          | [], _ -> List.rev_append p2 acc |> List.rev
          | _, [] -> List.rev_append p1 acc |> List.rev
        in
        merge [] p1 p2 |> return

  let register_outbox_messages s ~outbox_level ~indexes =
    Outbox_messages.register_outbox_messages (write_to s) ~outbox_level ~indexes

  let set_outbox_message_executed s ~outbox_level ~index =
    let open Lwt_result_syntax in
    match read_from s with
    | Normal s ->
        Outbox_messages.set_outbox_message_executed s ~outbox_level ~index
    | Hybrid {v4; v5; _} ->
        (* copy over pending messages to v5 *)
        let* pending =
          Store_v4.Outbox_messages.pending
            v4.outbox_messages
            ~min_level:outbox_level
            ~max_level:outbox_level
        in
        let* () =
          match pending with
          | [] -> return_unit
          | [(outbox_level, indexes)] ->
              let*? indexes = Bitset.from_list indexes in
              Outbox_messages.register_outbox_messages v5 ~outbox_level ~indexes
          | _ ->
              (* Only 1 level requested, we can't have more than one element. *)
              assert false
        in
        Outbox_messages.set_outbox_message_executed v5 ~outbox_level ~index
end

module Protocols = struct
  include Protocols

  let store s info = store (write_to s) info

  let find s h =
    match read_from s with
    | Normal s -> find s h
    | Hybrid {v4; v5; _} -> (
        find v5 h
        @?~~>
        let open Lwt_result_syntax in
        let* protos = Store_v4.Protocols.read v4.protocols in
        match protos with
        | None -> return_none
        | Some protos ->
            List.find_map
              (fun Store_v4.Protocols.{level; proto_level; protocol} ->
                if Protocol_hash.equal protocol h then
                  let level =
                    match level with
                    | First_known l -> First_known l
                    | Activation_level l -> Activation_level l
                  in
                  Some {level; proto_level; protocol}
                else None)
              protos
            |> return)

  let proto_of_level s level =
    match read_from s with
    | Normal s -> proto_of_level s level
    | Hybrid {v4; v5; _} -> (
        proto_of_level v5 level
        @?~~>
        let open Lwt_result_syntax in
        let* protos = Store_v4.Protocols.read v4.protocols in
        match protos with
        | None -> return_none
        | Some protos ->
            let rec find = function
              | [] -> None
              | Store_v4.Protocols.
                  {
                    level = (First_known l | Activation_level l) as xl;
                    proto_level;
                    protocol;
                  }
                :: _
                when level > l ->
                  let level =
                    match xl with
                    | First_known l -> First_known l
                    | Activation_level l -> Activation_level l
                  in
                  Some {level; proto_level; protocol}
              | _ :: rest -> find rest
            in
            find protos |> return)

  let last s =
    match read_from s with
    | Normal s -> last s
    | Hybrid {v4; v5; _} -> (
        last v5
        @?~~>
        let open Lwt_result_syntax in
        let* protos = Store_v4.Protocols.read v4.protocols in
        match protos with
        | None -> return_none
        | Some [] -> return_none
        | Some (Store_v4.Protocols.{level; proto_level; protocol} :: _) ->
            let level =
              match level with
              | First_known l -> First_known l
              | Activation_level l -> Activation_level l
            in
            return_some {level; proto_level; protocol})
end

module Dal_slots_headers = struct
  let store s block header = Dal_slots_headers.store (write_to s) block header

  let find_slot_header s block ~slot_index =
    match read_from s with
    | Normal s -> Dal_slots_headers.find_slot_header s block ~slot_index
    | Hybrid {v4; v5; _} ->
        Dal_slots_headers.find_slot_header v5 block ~slot_index
        @?~~> Store_v4.Dal_slots_headers.find
                v4.irmin_store
                ~primary_key:block
                ~secondary_key:slot_index

  let list_slot_headers s block =
    match read_from s with
    | Normal s -> Dal_slots_headers.list_slot_headers s block
    | Hybrid {v4; v5; _} -> (
        let open Lwt_result_syntax in
        let* v5_headers = Dal_slots_headers.list_slot_headers v5 block in
        match v5_headers with
        | _ :: _ -> return v5_headers
        | [] ->
            Store_v4.Dal_slots_headers.list_values
              v4.irmin_store
              ~primary_key:block)

  let list_slot_indexes s block =
    match read_from s with
    | Normal s -> Dal_slots_headers.list_slot_indexes s block
    | Hybrid {v4; v5; _} -> (
        let open Lwt_result_syntax in
        let* v5_indexes = Dal_slots_headers.list_slot_indexes v5 block in
        match v5_indexes with
        | _ :: _ -> return v5_indexes
        | [] ->
            Store_v4.Dal_slots_headers.list_secondary_keys
              v4.irmin_store
              ~primary_key:block)
end

module Dal_slots_statuses = struct
  let store s block index status =
    Dal_slots_statuses.store (write_to s) block index status

  let find_slot_status s block ~slot_index =
    match read_from s with
    | Normal s -> Dal_slots_statuses.find_slot_status s block ~slot_index
    | Hybrid {v4; v5; _} ->
        Dal_slots_statuses.find_slot_status v5 block ~slot_index
        @?~~> Store_v4.Dal_slots_statuses.find
                v4.irmin_store
                ~primary_key:block
                ~secondary_key:slot_index

  let list_slot_statuses s block =
    match read_from s with
    | Normal s -> Dal_slots_statuses.list_slot_statuses s block
    | Hybrid {v4; v5; _} -> (
        let open Lwt_result_syntax in
        let* v5_statuses = Dal_slots_statuses.list_slot_statuses v5 block in
        match v5_statuses with
        | _ :: _ -> return v5_statuses
        | [] ->
            Store_v4.Dal_slots_statuses.list_secondary_keys_with_values
              v4.irmin_store
              ~primary_key:block)
end

module L2_levels = struct
  let store s level hash = L2_levels.store (write_to s) level hash

  let find s level =
    match read_from s with
    | Normal s -> L2_levels.find s level
    | Hybrid {v4; v5; _} ->
        L2_levels.find v5 level
        @?~~> Store_v4.Levels_to_hashes.find v4.levels_to_hashes level
end

module L2_blocks = struct
  open L2_blocks

  let store s block = store (write_to s) block

  let find s hash =
    match read_from s with
    | Normal s -> find s hash
    | Hybrid {v4; v5; _} ->
        find v5 hash
        @?~~>
        let open Lwt_result_syntax in
        let+ res = Store_v4.L2_blocks.read v4.l2_blocks hash in
        Option.map
          (fun (block, header) -> Sc_rollup_block.{block with header})
          res

  let find_by_level s level =
    match read_from s with
    | Normal s -> find_by_level s level
    | Hybrid {v4; v5; _} -> (
        find_by_level v5 level
        @?~~>
        let open Lwt_result_syntax in
        let* hash = Store_v4.Levels_to_hashes.find v4.levels_to_hashes level in
        match hash with
        | None -> return_none
        | Some hash ->
            let+ res = Store_v4.L2_blocks.read v4.l2_blocks hash in
            Option.map
              (fun (block, header) -> Sc_rollup_block.{block with header})
              res)

  let find_level s hash =
    match read_from s with
    | Normal s -> find_level s hash
    | Hybrid {v4; v5; _} ->
        find_level v5 hash
        @?~~>
        let open Lwt_result_syntax in
        let+ res = Store_v4.L2_blocks.header v4.l2_blocks hash in
        Option.map (fun header -> header.Sc_rollup_block.level) res

  let find_context s hash =
    match read_from s with
    | Normal s -> find_context s hash
    | Hybrid {v4; v5; _} ->
        find_context v5 hash
        @?~~>
        let open Lwt_result_syntax in
        let+ res = Store_v4.L2_blocks.header v4.l2_blocks hash in
        Option.map (fun header -> header.Sc_rollup_block.context) res

  let find_head s =
    match read_from s with
    | Normal s -> find_head s
    | Hybrid {v4; v5; _} -> find_head v5 @?~~> Store_v4.L2_head.read v4.l2_head

  let find_finalized s =
    match read_from s with
    | Normal s -> find_finalized s
    | Hybrid {v4; v5; _} -> (
        find_finalized v5
        @?~~>
        let open Lwt_result_syntax in
        let* level =
          Store_v4.Last_finalized_level.read v4.last_finalized_level
        in
        match level with
        | None -> return_none
        | Some level -> (
            let* hash =
              Store_v4.Levels_to_hashes.find v4.levels_to_hashes level
            in
            match hash with
            | None -> return_none
            | Some hash ->
                let+ res = Store_v4.L2_blocks.read v4.l2_blocks hash in
                Option.map
                  (fun (block, header) -> Sc_rollup_block.{block with header})
                  res))

  let find_predecessor s hash =
    match read_from s with
    | Normal s -> find_predecessor s hash
    | Hybrid {v4; v5; _} ->
        find_predecessor v5 hash
        @?~~>
        let open Lwt_result_syntax in
        let+ res = Store_v4.L2_blocks.header v4.l2_blocks hash in
        Option.map
          (fun header ->
            (header.Sc_rollup_block.predecessor, Int32.pred header.level))
          res

  let find_full s hash =
    match read_from s with
    | Normal s -> find_full s hash
    | Hybrid {v4; v5; _} -> (
        find_full v5 hash
        @?~~>
        let open Lwt_result_syntax in
        let* res = Store_v4.L2_blocks.read v4.l2_blocks hash in
        match res with
        | None -> return_none
        | Some (block, header) -> (
            let* inbox = Store_v4.Inboxes.read v4.inboxes header.inbox_hash
            and* messages =
              Store_v4.Messages.read v4.messages header.inbox_witness
            and* commitment =
              match header.commitment_hash with
              | None -> return_none
              | Some c ->
                  Store_v4.Commitments.read v4.commitments c
                  |> Lwt_result.map @@ Option.map fst
            in
            match (inbox, messages) with
            | Some (inbox, ()), Some (messages, _) ->
                return_some
                  {
                    block with
                    header;
                    content =
                      Sc_rollup_block.
                        {inbox; messages; commitment; outbox = None};
                  }
            | _ -> return_none))
end

module State = struct
  include State

  module Finalized_level = struct
    open Finalized_level

    let set s v = set (write_to s) v

    let get s =
      match read_from s with
      | Normal s -> get s
      | Hybrid {v4; v5; _} -> (
          get v5
          @?~~>
          let open Lwt_result_syntax in
          let* level =
            Store_v4.Last_finalized_level.read v4.last_finalized_level
          in
          match level with
          | None -> return_none
          | Some level -> (
              let+ hash =
                Store_v4.Levels_to_hashes.find v4.levels_to_hashes level
              in
              match hash with None -> None | Some hash -> Some (hash, level)))
  end

  module LCC = struct
    open LCC

    let set s v = set (write_to s) v

    let get s =
      match read_from s with
      | Normal s -> get s
      | Hybrid {v4; v5; _} -> (
          get v5
          @?~~>
          let open Lwt_result_syntax in
          let+ lcc = Store_v4.Lcc.read v4.lcc in
          match lcc with
          | None -> None
          | Some {commitment; level} -> Some (commitment, level))
  end

  module LPC = struct
    open LPC

    let set s v = set (write_to s) v

    let get s =
      match read_from s with
      | Normal s -> get s
      | Hybrid {v4; v5; _} -> (
          get v5
          @?~~>
          let open Lwt_result_syntax in
          let+ lpc = Store_v4.Lpc.read v4.lpc in
          match lpc with
          | None -> None
          | Some commitment ->
              let hash = Commitment.hash commitment in
              Some (hash, commitment.inbox_level))
  end

  module Last_gc_target = struct
    open Last_gc_target

    let set s v = set (write_to s) v

    let get s =
      match read_from s with
      | Normal s -> get s
      | Hybrid {v4; v5; _} -> (
          get v5
          @?~~>
          let open Lwt_result_syntax in
          let+ gc_levels = Store_v4.Gc_levels.read v4.gc_levels in
          match gc_levels with
          | None -> None
          | Some {first_available_level; _} -> Some first_available_level)
  end

  module Last_gc_triggered_at = struct
    open Last_gc_triggered_at

    let set s v = set (write_to s) v

    let get s =
      match read_from s with
      | Normal s -> get s
      | Hybrid {v4; v5; _} -> (
          get v5
          @?~~>
          let open Lwt_result_syntax in
          let+ gc_levels = Store_v4.Gc_levels.read v4.gc_levels in
          match gc_levels with
          | None -> None
          | Some {last_gc_level; _} -> Some last_gc_level)
  end

  module Last_successful_gc_target = struct
    open Last_successful_gc_target

    let set s v = set (write_to s) v

    let get s =
      match read_from s with
      | Normal s -> get s
      | Hybrid {v4; v5; _} -> (
          get v5
          @?~~>
          let open Lwt_result_syntax in
          let+ gc_levels = Store_v4.Gc_levels.read v4.successful_gc_levels in
          match gc_levels with
          | None -> None
          | Some {first_available_level; _} -> Some first_available_level)
  end

  module Last_successful_gc_triggered_at = struct
    open Last_successful_gc_triggered_at

    let set s v = set (write_to s) v

    let get s =
      match read_from s with
      | Normal s -> get s
      | Hybrid {v4; v5; _} -> (
          get v5
          @?~~>
          let open Lwt_result_syntax in
          let+ gc_levels = Store_v4.Gc_levels.read v4.successful_gc_levels in
          match gc_levels with
          | None -> None
          | Some {last_gc_level; _} -> Some last_gc_level)
  end

  module Last_context_split = struct
    open Last_context_split

    let set s v = set (write_to s) v

    let get s =
      match read_from s with
      | Normal s -> get s
      | Hybrid {v4; v5; _} ->
          get v5
          @?~~> Store_v4.Last_context_split.read v4.last_context_split_level
  end

  module History_mode = struct
    open History_mode

    let set s v = set (write_to s) v

    let get s =
      match read_from s with
      | Normal s -> get s
      | Hybrid {v4; v5; _} -> (
          get v5
          @?~~>
          let open Lwt_result_syntax in
          let+ m = Store_v4.History_mode.read v4.history_mode in
          match m with
          | None -> None
          | Some Full -> Some Full
          | Some Archive -> Some Archive)
  end

  module L2_head = struct
    open L2_head

    let set s v = set (write_to s) v

    let get s =
      match read_from s with
      | Normal s -> get s
      | Hybrid {v4; v5; _} -> (
          get v5
          @?~~>
          let open Lwt_result_syntax in
          let+ block = Store_v4.L2_head.read v4.l2_head in
          match block with
          | None -> None
          | Some block -> Some (block.header.block_hash, block.header.level))
  end
end
