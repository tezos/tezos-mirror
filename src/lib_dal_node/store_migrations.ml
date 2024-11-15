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

let migrate_attested_level_for_slot_index kvs_store attested_level ~slot_index
    ~payload =
  let open Lwt_result_syntax in
  let* hash =
    Kvs_skip_list_cells_store.Internal_for_migrations.find_hash
      kvs_store
      ~attested_level
      ~slot_index
  in
  let* cell = Kvs_skip_list_cells_store.find kvs_store hash in
  return ((hash, cell) :: payload)

let migrate_attested_level kvs_store sql_store attested_level =
  let open Lwt_result_syntax in
  let number_of_slots = Value_size_hooks.number_of_slots () in
  let* payload =
    List.fold_left_es
      (fun payload slot_index ->
        migrate_attested_level_for_slot_index
          kvs_store
          attested_level
          ~slot_index
          ~payload)
      []
      (0 -- (number_of_slots - 1))
  in
  let*! res =
    Dal_store_sqlite3.Skip_list_cells.insert sql_store ~attested_level payload
  in
  match res with
  | Ok () -> return_unit
  | Error err -> fail (Skip_list_cells_insert_failed {attested_level} :: err)

let migrate_skip_list_store kvs_store sql_store =
  let open Lwt_result_syntax in
  let attested_levels =
    Kvs_skip_list_cells_store.Internal_for_migrations.get_attested_levels
      kvs_store
  in
  Lwt.catch
    (fun () ->
      let rec loop () =
        let*! attested_level = Lwt_stream.next attested_levels in
        let* () = migrate_attested_level kvs_store sql_store attested_level in
        loop ()
      in
      loop ())
    (function
      | Lwt_stream.Empty -> return_unit | exn -> fail [error_of_exn exn])
