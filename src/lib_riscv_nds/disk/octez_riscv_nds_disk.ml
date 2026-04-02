(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Nds_errors

module Api = Octez_riscv_nds_disk_api.Octez_riscv_durable_storage_on_disk_api

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

module Repo = struct
  type t = Api.repo

  let create path =
    Api.octez_riscv_durable_on_disk_repo_new (Bytes.of_string path)
end

module Proof = struct
  type t = Api.proof

  let start_state proof =
    Api.octez_riscv_durable_on_disk_proof_start_state proof

  let stop_state proof = Api.octez_riscv_durable_on_disk_proof_stop_state proof

  let serialise proof = Api.octez_riscv_durable_on_disk_serialise_proof proof

  let deserialise bytes =
    match Api.octez_riscv_durable_on_disk_deserialise_proof bytes with
    | Ok proof -> Ok proof
    | Error msg -> Result_syntax.tzfail (Proof_deserialisation_error msg)
end

module Normal = struct
  module Registry = struct
    type t = Api.registry

    let hash registry =
      Ok (Api.octez_riscv_durable_on_disk_registry_hash registry)

    let size registry =
      Ok (Api.octez_riscv_durable_on_disk_registry_size registry)

    let resize registry n =
      Api.octez_riscv_durable_on_disk_registry_resize registry n |> wrap_error

    let copy_database registry ~src ~dst =
      Api.octez_riscv_durable_on_disk_registry_copy registry src dst
      |> wrap_error

    let move_database registry ~src ~dst =
      Api.octez_riscv_durable_on_disk_registry_move registry src dst
      |> wrap_error

    let clear registry db_index =
      Api.octez_riscv_durable_on_disk_registry_clear registry db_index
      |> wrap_error

    let create repo = Api.octez_riscv_durable_on_disk_registry_new repo

    let commit registry =
      Api.octez_riscv_durable_on_disk_registry_commit registry

    let checkout repo commit_id =
      Api.octez_riscv_durable_on_disk_registry_checkout repo commit_id
  end

  module Database = struct
    let exists registry ~db_index ~key =
      Api.octez_riscv_durable_on_disk_database_exists registry db_index key
      |> wrap_error

    let set registry ~db_index ~key ~value =
      Api.octez_riscv_durable_on_disk_database_set registry db_index key value
      |> wrap_error

    let write registry ~db_index ~key ~offset ~value =
      Api.octez_riscv_durable_on_disk_database_write
        registry
        db_index
        key
        offset
        value
      |> wrap_error

    let read registry ~db_index ~key ~offset ~len =
      Api.octez_riscv_durable_on_disk_database_read
        registry
        db_index
        key
        offset
        len
      |> wrap_error

    let value_length registry ~db_index ~key =
      Api.octez_riscv_durable_on_disk_database_value_length
        registry
        db_index
        key
      |> wrap_error

    let delete registry ~db_index ~key =
      Api.octez_riscv_durable_on_disk_database_delete registry db_index key
      |> wrap_error

    let hash registry ~db_index =
      Api.octez_riscv_durable_on_disk_database_hash registry db_index
      |> wrap_error
  end
end

module Prove = struct
  module Registry = struct
    type t = Api.registry_prove

    let hash t = Ok (Api.octez_riscv_durable_on_disk_prove_registry_hash t)

    let size t = Ok (Api.octez_riscv_durable_on_disk_prove_registry_size t)

    let resize t n =
      Api.octez_riscv_durable_on_disk_prove_registry_resize t n |> wrap_error

    let copy_database t ~src ~dst =
      Api.octez_riscv_durable_on_disk_prove_registry_copy t src dst
      |> wrap_error

    let move_database t ~src ~dst =
      Api.octez_riscv_durable_on_disk_prove_registry_move t src dst
      |> wrap_error

    let clear t db_index =
      Api.octez_riscv_durable_on_disk_prove_registry_clear t db_index
      |> wrap_error
  end

  module Database = struct
    let exists t ~db_index ~key =
      Api.octez_riscv_durable_on_disk_prove_database_exists t db_index key
      |> wrap_error

    let set t ~db_index ~key ~value =
      Api.octez_riscv_durable_on_disk_prove_database_set t db_index key value
      |> wrap_error

    let write t ~db_index ~key ~offset ~value =
      Api.octez_riscv_durable_on_disk_prove_database_write
        t
        db_index
        key
        offset
        value
      |> wrap_error

    let read t ~db_index ~key ~offset ~len =
      Api.octez_riscv_durable_on_disk_prove_database_read
        t
        db_index
        key
        offset
        len
      |> wrap_error

    let value_length t ~db_index ~key =
      Api.octez_riscv_durable_on_disk_prove_database_value_length t db_index key
      |> wrap_error

    let delete t ~db_index ~key =
      Api.octez_riscv_durable_on_disk_prove_database_delete t db_index key
      |> wrap_error

    let hash t ~db_index =
      Api.octez_riscv_durable_on_disk_prove_database_hash t db_index
      |> wrap_error
  end

  module Proof = Proof

  let start_proof registry =
    Api.octez_riscv_durable_on_disk_start_proof registry

  let produce_proof registry_prove =
    Api.octez_riscv_durable_on_disk_produce_proof registry_prove
end
