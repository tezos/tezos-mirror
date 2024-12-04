(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error += Skip_list_cells_insert_failed of {attested_level : int32}

let () =
  register_error_kind
    `Permanent
    ~id:"dal.lib.store_migrations.skip_list_cells_insert_fail"
    ~title:"Skip list cell insertion failed."
    ~description:
      "Failed to insert the skip list cell during the migration to sqlite3."
    Data_encoding.(obj1 (req "attested_level" int32))
    ~pp:(fun ppf attested_level ->
      Format.fprintf
        ppf
        "Failed to insert the skip list cell at the given attested level %ld \
         when migrating the store."
        attested_level)
    (function
      | Skip_list_cells_insert_failed {attested_level} -> Some attested_level
      | _ -> None)
    (fun attested_level -> Skip_list_cells_insert_failed {attested_level})

(** [fetch_bounds attested_levels] computes the minimum and maximum levels from a stream
    [attested_levels]. It returns
    - [Some (min, max)] where [min] (resp. [max]) is the minimum (resp. maximum) level,
    - [None] if the stream is empty, or
    - fails if an exception occurs during stream processing.

    The function processes the stream sequentially, maintaining minimum and
    maximum values in the accumulator. *)
let fetch_bounds attested_levels =
  let open Lwt_result_syntax in
  let rec loop (vmin, vmax) =
    let* new_level =
      Lwt.catch
        (fun () ->
          let*! v = Lwt_stream.next attested_levels in
          return_some v)
        (function
          | Lwt_stream.Empty -> return_none | exn -> fail [error_of_exn exn])
    in
    match new_level with
    | None -> return (vmin, vmax)
    | Some v -> loop Int32.(min vmin v, max vmax v)
  in
  (* We need to catch a first level to initialize the first values. *)
  let* level =
    Lwt.catch
      (fun () ->
        let*! v = Lwt_stream.next attested_levels in
        return_some v)
      (function
        | Lwt_stream.Empty -> return_none | exn -> fail [error_of_exn exn])
  in
  Option.map_es (fun v -> loop (v, v)) level

let fetch_attested_level kvs_store slots attested_level =
  let open Lwt_result_syntax in
  let fetch_slot_index ~slot_index =
    let open Lwt_result_syntax in
    let* hash =
      Kvs_skip_list_cells_store.Internal_for_migrations.find_hash
        kvs_store
        ~attested_level
        ~slot_index
    in
    let* cell = Kvs_skip_list_cells_store.find kvs_store hash in
    return (hash, cell)
  in
  let+ payload =
    List.map_es (fun slot_index -> fetch_slot_index ~slot_index) slots
  in
  (attested_level, payload)

let fetch_batch kvs_store ~pos ~vmax =
  let open Lwt_result_syntax in
  (* Size of the batch. *)
  let batch_size = 500l in
  let number_of_slots = Value_size_hooks.number_of_slots () in
  let slots = 0 -- (number_of_slots - 1) in
  let range = Int32.(min (sub (succ vmax) pos) batch_size) in
  let levels =
    let open Int32 in
    Stdlib.List.init (to_int range) (fun l -> add (of_int l) pos)
  in
  (* Surprisingly, using List.map_ep here is very slower. *)
  let fetch level = fetch_attested_level kvs_store slots level in
  let+ attested_levels = List.map_es fetch levels in
  let new_pos = Int32.(add pos range) in
  (new_pos, attested_levels)

let write_batch conn sql_store data =
  let open Lwt_result_syntax in
  List.iter_es
    (fun (attested_level, payload) ->
      let*! res =
        Dal_store_sqlite3.Skip_list_cells.insert
          ~conn
          sql_store
          ~attested_level
          payload
      in
      match res with
      | Ok () -> return_unit
      | Error err -> fail (Skip_list_cells_insert_failed {attested_level} :: err))
    data

let migrate_skip_list_store kvs_store sql_store =
  let open Lwt_result_syntax in
  let attested_levels =
    Kvs_skip_list_cells_store.Internal_for_migrations.get_attested_levels
      kvs_store
  in
  let* bounds = fetch_bounds attested_levels in
  match bounds with
  | None -> return_unit
  | Some (vmin, vmax) ->
      Dal_store_sqlite3.Skip_list_cells.use sql_store @@ fun conn ->
      let rec loop promise =
        let* new_pos, attested_levels = promise in
        if List.is_empty attested_levels then return_unit
        else
          let promise = fetch_batch kvs_store ~pos:new_pos ~vmax in
          let* () =
            Sqlite.with_transaction conn @@ fun conn ->
            write_batch conn sql_store attested_levels
          in
          loop promise
      in
      let promise = fetch_batch kvs_store ~pos:vmin ~vmax in
      loop promise
