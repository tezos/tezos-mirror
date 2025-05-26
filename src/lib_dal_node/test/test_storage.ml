(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Dal node storage
   Invocation:   dune exec src/lib_dal_node/test/main.exe \
                  -- --file test_storage.ml
   Subject:      Testsuite related with the DAL node storage backend
*)

(* [number_of_slots] indicates the number of available slots per
   block.
   TODO: Update this value to match the current protocol constant when
   needed. *)
let number_of_slots = 32

module Helpers = struct
  open Ppx_hash_lib.Std.Hash.Builtin
  open Bam.Std
  open Bam.Std.Syntax
  open Tezos_dal_node_lib.Dal_proto_types
  open Tezos_dal_node_services.Types

  (* Generate levels. *)
  module Level = struct
    type t = level

    let hash_fold_t = hash_fold_int32

    let min = 0l

    (* Arbitrary value. *)
    let max = 100_000l

    let gen = int32 ~min ~max ()
  end

  (* In the following we approximate the size of the data used by the store. *)

  (* Helper to hash bytes on which relies ppx_hash. *)
  let hash_fold_bytes state b = hash_fold_string state (Bytes.to_string b)

  module Skip_list_hash = struct
    include Skip_list_hash

    let hash_fold_t state t =
      let bytes = Data_encoding.Binary.to_bytes_exn Skip_list_hash.encoding t in
      hash_fold_bytes state bytes

    (* A pointer hash is 32 bytes length. *)
    let gen =
      let size = return 32 in
      let* data = bytes ~size ~char:(char ~printable:true ()) () in
      (* To write the data size we perform {to/of}_bytes_exn. *)
      let bytes = Data_encoding.(Binary.to_bytes_exn bytes) data in
      let t = Data_encoding.Binary.of_bytes_exn Skip_list_hash.encoding bytes in
      return t

    let pp fmt t =
      Format.fprintf
        fmt
        "\"%s\""
        (Data_encoding.Binary.to_string_exn Skip_list_hash.encoding t)
  end

  module Skip_list_cell = struct
    include Skip_list_cell

    let hash_fold_t state t =
      let bytes = Data_encoding.Binary.to_bytes_exn Skip_list_cell.encoding t in
      hash_fold_bytes state bytes

    (* We support at most 64 back-pointers, each of which takes 32
       bytes. *)
    let gen =
      let sizes = Stdlib.List.init 65 (fun i -> (1, return @@ (32 * i))) in
      let size = oneof sizes in
      let* data = bytes ~size ~char:(char ~printable:true ()) () in
      (* To write the data size we perform {to/of}_bytes_exn. *)
      let bytes = Data_encoding.(Binary.to_bytes_exn bytes) data in
      let t = Data_encoding.Binary.of_bytes_exn Skip_list_cell.encoding bytes in
      return t

    (* This can be pretty large, the data is then truncated. *)
    let pp fmt t =
      let s = Data_encoding.Binary.to_string_exn Skip_list_cell.encoding t in
      let l = String.length s in
      let s' = String.sub s 0 (min 10 l) ^ if l < 10 then "" else ".." in
      Format.fprintf fmt "\"%s\"" s'
  end

  module HashSet = Set.Make (struct
    type t = Skip_list_hash.t

    let compare = compare
  end)

  module LevelMap = Map.Make (Int32)

  module State = struct
    (* The state keeps traces of:
       - each registered cell hash in [registered_hashes], together
       with their attested level.
       - each removed cell hash in [removed_hashes]. *)
    type t = {
      mutable removed_hashes : HashSet.t;
      mutable registered_hashes : HashSet.t LevelMap.t;
    }

    let zero =
      {removed_hashes = HashSet.empty; registered_hashes = LevelMap.empty}

    let pp_removed_hashes fmt removed_hashes =
      let elements = HashSet.elements removed_hashes in
      match elements with
      | [] -> Format.fprintf fmt "[]"
      | _ ->
          Format.pp_print_list
            Skip_list_hash.pp
            fmt
            (HashSet.elements removed_hashes)

    let pp_registered_hashes_item fmt (level, removed_hashes) =
      Format.fprintf
        fmt
        "registered_hashes.level %ld = %a"
        level
        pp_removed_hashes
        removed_hashes

    let pp_registered_hashes fmt registered_hashes =
      let bindings = LevelMap.bindings registered_hashes in
      match bindings with
      | [] -> Format.fprintf fmt "[]"
      | _ -> Format.pp_print_list pp_registered_hashes_item fmt bindings

    let pp fmt state =
      Format.fprintf
        fmt
        "removed_hashes = %a@. registered_hashes = %a"
        pp_removed_hashes
        state.removed_hashes
        pp_registered_hashes
        state.registered_hashes
  end

  module Action = struct
    type t =
      | Insert of {
          attested_level : Level.t;
          payload : (Skip_list_hash.t * Skip_list_cell.t) list;
        }
      | Find of {skip_list_hash : Skip_list_hash.t}
      | Remove of {attested_level : Level.t}
    [@@deriving hash]

    let gen_Insert ?level () =
      return @@ fun state ->
      let* attested_level = Option.fold ~none:Level.gen ~some:return level in
      let* payload =
        list
          ~size:(return number_of_slots)
          (pair Skip_list_hash.gen Skip_list_cell.gen)
      in
      let hashes =
        List.fold_left
          (fun acc (hash, _) -> HashSet.add hash acc)
          HashSet.empty
          payload
      in
      let new_registered_hashes =
        LevelMap.add attested_level hashes state.State.registered_hashes
      in
      let new_removed_hashes = HashSet.diff state.State.removed_hashes hashes in
      let new_state =
        {
          State.registered_hashes = new_registered_hashes;
          removed_hashes = new_removed_hashes;
        }
      in
      return (Some (new_state, Insert {attested_level; payload}))

    let gen_Find =
      return @@ fun state ->
      match LevelMap.choose_opt state.State.registered_hashes with
      | None -> return None
      | Some (_attested_level, removed_hashes) -> (
          match HashSet.choose_opt removed_hashes with
          | None -> return None
          | Some skip_list_hash -> return (Some (state, Find {skip_list_hash})))

    let gen_Remove =
      return @@ fun state ->
      match LevelMap.choose_opt state.State.registered_hashes with
      | None -> return None
      | Some (attested_level, removed_hashes) ->
          let new_registered_hashes =
            LevelMap.remove attested_level state.registered_hashes
          in
          let new_removed_hashes =
            HashSet.union removed_hashes state.State.removed_hashes
          in
          let new_state =
            {
              State.registered_hashes = new_registered_hashes;
              removed_hashes = new_removed_hashes;
            }
          in
          return (Some (new_state, Remove {attested_level}))

    let pp_action_insert_payload_item fmt (hash, cell) =
      Format.fprintf
        fmt
        "(%a, %a)"
        Skip_list_hash.pp
        hash
        Skip_list_cell.pp
        cell

    let pp_action_insert_payload fmt payload =
      Format.pp_print_list pp_action_insert_payload_item fmt payload

    let pp fmt action =
      match action with
      | Insert {attested_level; payload} ->
          Format.fprintf
            fmt
            "insert at attested level %ld the following payload:@, %a"
            attested_level
            pp_action_insert_payload
            payload
      | Find {skip_list_hash} ->
          Format.fprintf
            fmt
            "find skip list hash %a"
            Skip_list_hash.pp
            skip_list_hash
      | Remove {attested_level} ->
          Format.fprintf fmt "remove attested level %ld" attested_level
  end

  type scenario = {state : State.t; [@hash.ignore] actions : Action.t list}
  [@@deriving hash]

  let pp fmt {state; actions} =
    Format.fprintf
      fmt
      "scenario.state: %a@. scenario.actions:%a"
      State.pp
      state
      (Format.pp_print_list Action.pp)
      actions

  (** [gen_with_state ?insert_dist ?find_dist ?remove_dist state n]
      generates a [scenario] containing a list of [n] actions and a
      [state] resulting from their interpretations. Actions are
      selected according to the probability distribution defined by
      [insert_dist], [find_dist], and [remove_dist] (each defaulting
      to 1).

      The sum of distribution weights must be non-zero. Additionally,
      [insert_dist] must be non-zero since [find] and [remove] actions
      operate on payloads introduced by prior [insert] actions. *)
  let gen_with_state ?(insert_dist = 1) ?(find_dist = 1) ?(remove_dist = 1)
      state n =
    let open Bam.Std.Syntax in
    assert (n >= 0) ;
    let rec loop l acc state n =
      if n = 0 then return {state; actions = List.rev acc}
      else
        let choices =
          (insert_dist, Action.gen_Insert ~level:l ())
          :: (find_dist, Action.gen_Find)
          :: [(remove_dist, Action.gen_Remove)]
        in
        let* choice = Bam.Std.oneof choices in
        let* res = choice state in
        match res with
        | None -> loop l acc state n
        | Some (new_state, (Action.Insert _ as action)) ->
            loop (Int32.succ l) (action :: acc) new_state (pred n)
        | Some (new_state, action) -> loop l (action :: acc) new_state (pred n)
    in
    loop 0l [] state n

  let gen ?insert_dist ?find_dist ?remove_dist n =
    gen_with_state ?insert_dist ?find_dist ?remove_dist State.zero n
end

open Helpers

let perform_kvs store action =
  let open Lwt_result_syntax in
  let open Kvs_skip_list_cells_store in
  match action with
  | Action.Insert {attested_level; payload} ->
      insert store ~attested_level payload
  | Find {skip_list_hash} ->
      let* _cell = find store skip_list_hash in
      return_unit
  | Remove {attested_level} -> remove store ~attested_level

let perform_sqlite store action =
  let open Lwt_result_syntax in
  let open Dal_store_sqlite3.Skip_list_cells in
  match action with
  | Action.Insert {attested_level; payload} ->
      insert store ~attested_level payload
  | Find {skip_list_hash} ->
      let* _cell = find_opt store skip_list_hash in
      return_unit
  | Remove {attested_level} -> remove store ~attested_level

let run_kvs store actions = List.iter_es (perform_kvs store) actions

let run_sqlite3 store actions = List.iter_es (perform_sqlite store) actions

(** [handshake state kvs_store sql_store] ensures that each hash
    generated during the run is either available or not found in each
    store. *)
let handshake state kvs_store sql_store =
  let open Lwt_result_syntax in
  let must_exist_hashes =
    LevelMap.fold
      (fun _level -> HashSet.union)
      state.State.registered_hashes
      HashSet.empty
  in
  let must_not_exist_hashes = state.State.removed_hashes in
  let* () =
    HashSet.iter_es
      (fun hash ->
        (* The hash must exist in the stores and the associated cell must be the same. *)
        let* kvs_cell = Kvs_skip_list_cells_store.find kvs_store hash in
        let* sql_cell =
          Dal_store_sqlite3.Skip_list_cells.find_opt sql_store hash
        in
        assert (Option.equal Skip_list_cell.equal (Some kvs_cell) sql_cell) ;
        return_unit)
      must_exist_hashes
  in
  let* () =
    HashSet.iter_es
      (fun hash ->
        (* The hash must not exist in the stores, the lookup must return false. *)
        let* kvs_lookup =
          Kvs_skip_list_cells_store.Internal_for_tests.skip_list_hash_exists
            kvs_store
            hash
        in
        let* sql_lookup =
          Dal_store_sqlite3.Skip_list_cells.Internal_for_tests
          .skip_list_hash_exists
            sql_store
            hash
        in
        assert (not kvs_lookup) ;
        assert (not sql_lookup) ;
        return_unit)
      must_not_exist_hashes
  in
  return_unit

(* As found in lib_dal_node/store.ml *)
let init_skip_list_cells_store node_store_dir =
  let padded_encoded_cell_size = 64 * (32 + 1) in
  let encoded_hash_size = 32 + 4 in
  Kvs_skip_list_cells_store.init
    ~node_store_dir
    ~skip_list_store_dir:"skip_list_store"
    ~padded_encoded_cell_size
    ~encoded_hash_size

let init_sqlite_skip_list_cells_store ?(perm = `Read_write) data_dir =
  Dal_store_sqlite3.Skip_list_cells.init
    ~data_dir:(Filename.concat data_dir "skip_list_store")
    ~perm
    ()

(* Required by the KVS store. *)
let () = Value_size_hooks.set_number_of_slots number_of_slots

(* This test checks that the sqlite and the kvs backends exhibit the same behaviours.
   To do that, it generates lists of actions to be performed. After performing the
   list of actions, the test runs a handshake to verify that the stores are equivalent
   w.r.t to the cell hashes generated during the run. *)
let consistency_test =
  let open Lwt_result_syntax in
  let property {state; actions} =
    let run =
      (* Initialize the KVS and the SQLite stores. *)
      let data_dir = Tezt.Temp.dir "data-dir" in
      let* kvs_store = init_skip_list_cells_store data_dir in
      let* sql_store = init_sqlite_skip_list_cells_store data_dir in
      (* Run the actions both for the KVS and the SQL backends. *)
      let* () = run_kvs kvs_store actions in
      let* () = run_sqlite3 sql_store actions in
      (* Perform the handshake. *)
      let* () = handshake state kvs_store sql_store in
      (* We are done with this run. *)
      let* () = Kvs_skip_list_cells_store.close kvs_store in
      return_unit
    in
    match Lwt_main.run run with
    | Ok () -> Ok ()
    | Error err ->
        Error (`Fail (Format.asprintf "%a" Error_monad.pp_print_trace err))
  in
  Tezt_bam.Pbt.register
    ~pp
    ~hash:[%hash: scenario]
    ~minimum_number_of_samples:10
    ~title:"KVS and SQLite3 stores consistency"
    ~stop_after:(`Timeout 30.)
    ~__FILE__
    ~tags:["kvs"; "sqlite"; Tag.ci_disabled]
    ~gen:(Helpers.gen 20)
    ~property
    ()

(** Test skip list storage migrations.

    This test validates the migration from the key-value store to
    sqlite. More precisely, it
    - initializes a kvs store,
    - executes a sequence of insertions,
    - initializes a sqlite store and migrates the data from the kvs one,
    - performs a handshake to verify data consistency between the two stores. *)
let migration_skip_list_test {state; actions} =
  let open Lwt_result_syntax in
  let data_dir = Tezt.Temp.dir "data-dir" in
  let* kvs_store = init_skip_list_cells_store data_dir in
  let* () = run_kvs kvs_store actions in
  let* sql_store = init_sqlite_skip_list_cells_store data_dir in
  let* () = Store_migrations.migrate_skip_list_store kvs_store sql_store in
  let* () = handshake state kvs_store sql_store in
  Kvs_skip_list_cells_store.close kvs_store

let () =
  let open Tezt_bam in
  let open Lwt_result_syntax in
  let title = "skip list storage migration test" in
  Test.register ~seed:Random ~__FILE__ ~title ~tags:["kvs"; "sqlite"]
  @@ fun () ->
  (* [seed] and [get_state] as found in [Tezt_bam.Runner.register] *)
  let seed = Random.full_int Int.max_int in
  let state = Bam.Gen.Random.make [|seed|] in
  let state = snd (Bam.Gen.Random.split state) in
  let tree = Gen.run (gen 10 ~find_dist:0 ~remove_dist:0) state in
  let*! res = migration_skip_list_test (Tree.root tree) in
  match res with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Test.fail
        "Test fails with the following errors: %a"
        Error_monad.pp_print_trace
        err

(** Test skip list storage migrations.

    This test validates the migration from the key-value store to
    sqlite with a 10 000 levels. *)

let populate n kvs_store =
  let open Lwt_result_syntax in
  let seed = Tezt.Test.current_test_seed () in
  let state = Bam.Gen.Random.make [|seed|] in
  let tree = Tezt_bam.Gen.run Skip_list_cell.gen state in
  let cells = Bam.Std.return @@ Tezt_bam.Tree.root tree in
  let gen =
    let open Bam.Std in
    let open Bam.Std.Syntax in
    let* payload =
      list ~size:(return number_of_slots) (pair Skip_list_hash.gen cells)
    in
    return payload
  in
  let rec loop state n =
    if n = 0 then return_unit
    else
      let state = snd (Bam.Gen.Random.split state) in
      let tree = Tezt_bam.Gen.run gen state in
      let payload = Tezt_bam.Tree.root tree in
      let open Kvs_skip_list_cells_store in
      let* () = insert kvs_store ~attested_level:(Int32.of_int n) payload in
      loop state (pred n)
  in
  loop state n

let migration_skip_list_test () =
  let open Lwt_result_syntax in
  let data_dir = Tezt.Temp.dir "data-dir" in
  let* kvs_store = init_skip_list_cells_store data_dir in
  let n = 10_000 in
  let t1 = Unix.gettimeofday () in
  let* () = populate n kvs_store in
  let t2 = Unix.gettimeofday () in
  Log.info "Execution time for generating %d elements: %f seconds" n (t2 -. t1) ;
  let* sql_store =
    Dal_store_sqlite3.Skip_list_cells.init ~data_dir ~perm:`Read_write ()
  in
  let t1 = Unix.gettimeofday () in
  let* () = Store_migrations.migrate_skip_list_store kvs_store sql_store in
  let t2 = Unix.gettimeofday () in
  Log.info "Execution time for migrating %d elements: %f seconds" n (t2 -. t1) ;
  let* () = Kvs_skip_list_cells_store.close kvs_store in
  let*! () = Dal_store_sqlite3.Skip_list_cells.close sql_store in
  return_unit

let () =
  let open Tezt_bam in
  let open Lwt_result_syntax in
  let title = "skip list storage migration with large data" in
  Test.register
    ~seed:Random
    ~__FILE__
    ~title
    ~tags:["kvs"; "sqlite"; Tag.ci_disabled]
  @@ fun () ->
  let*! res = migration_skip_list_test () in
  match res with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Test.fail
        "Test fails with the following errors: %a"
        Error_monad.pp_print_trace
        err
