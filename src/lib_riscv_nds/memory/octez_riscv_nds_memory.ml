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

module Proof = struct
  type t = Api.proof

  let start_state proof =
    Api.octez_riscv_durable_in_memory_proof_start_state proof

  let stop_state proof =
    Api.octez_riscv_durable_in_memory_proof_stop_state proof

  let serialise proof = Api.octez_riscv_durable_in_memory_serialise_proof proof

  let deserialise bytes =
    match Api.octez_riscv_durable_in_memory_deserialise_proof bytes with
    | Ok proof -> Ok proof
    | Error msg -> Result_syntax.tzfail (Proof_deserialisation_error msg)
end

module Normal = struct
  module Registry = struct
    type t = Api.registry

    type invalid_argument_error = Api.invalid_argument_error

    let hash registry =
      Ok (Api.octez_riscv_durable_in_memory_registry_hash registry)

    let size registry =
      Ok (Api.octez_riscv_durable_in_memory_registry_size registry)

    let resize registry n =
      Api.octez_riscv_durable_in_memory_registry_resize registry n

    let copy_database registry ~src ~dst =
      Api.octez_riscv_durable_in_memory_registry_copy registry src dst

    let move_database registry ~src ~dst =
      Api.octez_riscv_durable_in_memory_registry_move registry src dst

    let clear registry db_index =
      Api.octez_riscv_durable_in_memory_registry_clear registry db_index

    let create () = Api.octez_riscv_durable_in_memory_registry_new ()
  end

  module Database = struct
    let exists registry ~db_index ~key =
      Api.octez_riscv_durable_in_memory_database_exists registry db_index key

    let set registry ~db_index ~key ~value =
      Api.octez_riscv_durable_in_memory_database_set registry db_index key value

    let write registry ~db_index ~key ~offset ~value =
      Api.octez_riscv_durable_in_memory_database_write
        registry
        db_index
        key
        offset
        value

    let read registry ~db_index ~key ~offset ~len =
      Api.octez_riscv_durable_in_memory_database_read
        registry
        db_index
        key
        offset
        len

    let value_length registry ~db_index ~key =
      Api.octez_riscv_durable_in_memory_database_value_length
        registry
        db_index
        key

    let delete registry ~db_index ~key =
      Api.octez_riscv_durable_in_memory_database_delete registry db_index key

    let hash registry ~db_index =
      Api.octez_riscv_durable_in_memory_database_hash registry db_index
  end
end

module Prove = struct
  module Registry = struct
    type t = Api.registry_prove

    type invalid_argument_error = Api.invalid_argument_error

    let hash t = Ok (Api.octez_riscv_durable_in_memory_prove_registry_hash t)

    let size t = Ok (Api.octez_riscv_durable_in_memory_prove_registry_size t)

    let resize t n = Api.octez_riscv_durable_in_memory_prove_registry_resize t n

    let copy_database t ~src ~dst =
      Api.octez_riscv_durable_in_memory_prove_registry_copy t src dst

    let move_database t ~src ~dst =
      Api.octez_riscv_durable_in_memory_prove_registry_move t src dst

    let clear t db_index =
      Api.octez_riscv_durable_in_memory_prove_registry_clear t db_index
  end

  module Database = struct
    let exists t ~db_index ~key =
      Api.octez_riscv_durable_in_memory_prove_database_exists t db_index key

    let set t ~db_index ~key ~value =
      Api.octez_riscv_durable_in_memory_prove_database_set t db_index key value

    let write t ~db_index ~key ~offset ~value =
      Api.octez_riscv_durable_in_memory_prove_database_write
        t
        db_index
        key
        offset
        value

    let read t ~db_index ~key ~offset ~len =
      Api.octez_riscv_durable_in_memory_prove_database_read
        t
        db_index
        key
        offset
        len

    let value_length t ~db_index ~key =
      Api.octez_riscv_durable_in_memory_prove_database_value_length
        t
        db_index
        key

    let delete t ~db_index ~key =
      Api.octez_riscv_durable_in_memory_prove_database_delete t db_index key

    let hash t ~db_index =
      Api.octez_riscv_durable_in_memory_prove_database_hash t db_index
  end

  module Proof = Proof

  let start_proof registry =
    Api.octez_riscv_durable_in_memory_start_proof registry

  let produce_proof registry_prove =
    Api.octez_riscv_durable_in_memory_produce_proof registry_prove
end

type verify_error =
  | Verify_invalid_argument of Api.invalid_argument_error
  | Verify_not_found of Api.verification_error

let wrap_verification_error = function
  | Ok x -> Ok x
  | Error e -> Error (Verify_not_found e)

let wrap_verification_argument_error = function
  | Ok x -> Ok x
  | Error (Api.Invalid_argument e) -> Error (Verify_invalid_argument e)
  | Error (Api.Verification e) -> Error (Verify_not_found e)

module Verify = struct
  module Registry = struct
    type t = Api.registry_verify

    type invalid_argument_error = verify_error

    let hash t =
      Api.octez_riscv_durable_in_memory_verify_registry_hash t
      |> wrap_verification_error

    let size t =
      Api.octez_riscv_durable_in_memory_verify_registry_size t
      |> wrap_verification_error

    let resize t n =
      Api.octez_riscv_durable_in_memory_verify_registry_resize t n
      |> wrap_verification_argument_error

    let copy_database t ~src ~dst =
      Api.octez_riscv_durable_in_memory_verify_registry_copy t src dst
      |> wrap_verification_argument_error

    let move_database t ~src ~dst =
      Api.octez_riscv_durable_in_memory_verify_registry_move t src dst
      |> wrap_verification_argument_error

    let clear t db_index =
      Api.octez_riscv_durable_in_memory_verify_registry_clear t db_index
      |> wrap_verification_argument_error
  end

  module Database = struct
    let exists t ~db_index ~key =
      Api.octez_riscv_durable_in_memory_verify_database_exists t db_index key
      |> wrap_verification_argument_error

    let set t ~db_index ~key ~value =
      Api.octez_riscv_durable_in_memory_verify_database_set t db_index key value
      |> wrap_verification_argument_error

    let write t ~db_index ~key ~offset ~value =
      Api.octez_riscv_durable_in_memory_verify_database_write
        t
        db_index
        key
        offset
        value
      |> wrap_verification_argument_error

    let read t ~db_index ~key ~offset ~len =
      Api.octez_riscv_durable_in_memory_verify_database_read
        t
        db_index
        key
        offset
        len
      |> wrap_verification_argument_error

    let value_length t ~db_index ~key =
      Api.octez_riscv_durable_in_memory_verify_database_value_length
        t
        db_index
        key
      |> wrap_verification_argument_error

    let delete t ~db_index ~key =
      Api.octez_riscv_durable_in_memory_verify_database_delete t db_index key
      |> wrap_verification_argument_error

    let hash t ~db_index =
      Api.octez_riscv_durable_in_memory_verify_database_hash t db_index
      |> wrap_verification_argument_error
  end

  let start_verify proof = Api.octez_riscv_durable_in_memory_start_verify proof
end
