(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Trilitech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

open Nds_errors

module Api =
  Octez_riscv_nds_memory_api.Octez_riscv_durable_storage_in_memory_api

let convert_invalid_argument = function
  | Api.Key_not_found -> Key_not_found
  | Api.Key_too_long -> Key_too_long
  | Api.Offset_too_large -> Offset_too_large
  | Api.Database_index_out_of_bounds -> Database_index_out_of_bounds
  | Api.Registry_resize_too_large -> Registry_resize_too_large

let wrap_error = function
  | Ok x -> Ok x
  | Error e ->
      Result_syntax.tzfail (Invalid_argument (convert_invalid_argument e))

module Normal = struct
  module Registry = struct
    type t = Api.registry

    let hash registry =
      Ok (Api.octez_riscv_durable_in_memory_registry_hash registry)

    let size registry =
      Ok (Api.octez_riscv_durable_in_memory_registry_size registry)

    let resize registry n =
      Api.octez_riscv_durable_in_memory_registry_resize registry n |> wrap_error

    let copy_database registry ~src ~dst =
      Api.octez_riscv_durable_in_memory_registry_copy registry src dst
      |> wrap_error

    let move_database registry ~src ~dst =
      Api.octez_riscv_durable_in_memory_registry_move registry src dst
      |> wrap_error

    let clear registry db_index =
      Api.octez_riscv_durable_in_memory_registry_clear registry db_index
      |> wrap_error

    let create () = Api.octez_riscv_durable_in_memory_registry_new ()
  end

  module Database = struct
    let exists registry ~db_index ~key =
      Api.octez_riscv_durable_in_memory_database_exists registry db_index key
      |> wrap_error

    let set registry ~db_index ~key ~value =
      Api.octez_riscv_durable_in_memory_database_set registry db_index key value
      |> wrap_error

    let write registry ~db_index ~key ~offset ~value =
      Api.octez_riscv_durable_in_memory_database_write
        registry
        db_index
        key
        offset
        value
      |> wrap_error

    let read registry ~db_index ~key ~offset ~len =
      Api.octez_riscv_durable_in_memory_database_read
        registry
        db_index
        key
        offset
        len
      |> wrap_error

    let value_length registry ~db_index ~key =
      Api.octez_riscv_durable_in_memory_database_value_length
        registry
        db_index
        key
      |> wrap_error

    let delete registry ~db_index ~key =
      Api.octez_riscv_durable_in_memory_database_delete registry db_index key
      |> wrap_error

    let hash registry ~db_index =
      Api.octez_riscv_durable_in_memory_database_hash registry db_index
      |> wrap_error
  end
end
