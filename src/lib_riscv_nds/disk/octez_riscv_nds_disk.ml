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
  | Api.Io_request_too_large -> Io_request_too_large
  | Api.Offset_too_large -> Offset_too_large
  | Api.Value_size_too_large -> Value_size_too_large
  | Api.Database_index_out_of_bounds -> Database_index_out_of_bounds
  | Api.Registry_resize_too_large -> Registry_resize_too_large

let wrap_error r = Result.map_error convert_invalid_argument r

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

    let hash registry = Api.octez_riscv_durable_on_disk_registry_hash registry

    let size registry = Api.octez_riscv_durable_on_disk_registry_size registry

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

    type imm = Api.imm_registry

    let to_imm registry =
      Api.octez_riscv_durable_on_disk_registry_to_imm registry

    let from_imm imm = Api.octez_riscv_durable_on_disk_registry_from_imm imm

    (* [to_imm] then [from_imm] copies unshared state up to twice (once
       now, once on the duplicate's first mutation); fine off the hot
       path. *)
    let duplicate registry = from_imm (to_imm registry)
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

type _ Nds.tag += Normal_tag : Normal.Registry.t Nds.tag

let unwrap_normal nds : Normal.Registry.t option =
  match Nds.unpack nds with
  | Nds.Packed {tag = Normal_tag; value; _} -> Some value
  | _ -> None

module Prove = struct
  module Registry = struct
    type t = Api.registry_prove

    let hash t = Api.octez_riscv_durable_on_disk_prove_registry_hash t

    let size t = Api.octez_riscv_durable_on_disk_prove_registry_size t

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

type _ Nds.tag += Prove_tag : Prove.Registry.t Nds.tag

let unwrap_prove nds : Prove.Registry.t option =
  match Nds.unpack nds with
  | Nds.Packed {tag = Prove_tag; value; _} -> Some value
  | _ -> None

let convert_verification = function Api.Not_found -> Not_found

(** Mirror of [octez_riscv_nds_memory]'s exception-based escape:
    [Verification _] errors raise {!Nds_errors.Verification_failed}
    out-of-band so Verify-mode operations expose the same
    [invalid_argument_error] return type as [NORMAL]. *)
let raise_on_verification_argument r =
  Result.map_error
    (function
      | Api.Invalid_argument e -> convert_invalid_argument e
      | Api.Verification v ->
          raise (Verification_failed (convert_verification v)))
    r

(** The [registry_hash] / [registry_size] FFI primitives return only
    a [verification_error].  Translate the Error path into
    {!Verification_failed} and unwrap the Ok payload so callers see
    plain [bytes] / [int64] matching the infallible {!REGISTRY}
    signature. *)
let raise_and_extract_verification = function
  | Ok v -> v
  | Error v -> raise (Verification_failed (convert_verification v))

module Verify = struct
  module Registry = struct
    type t = Api.registry_verify

    let hash t =
      Api.octez_riscv_durable_on_disk_verify_registry_hash t
      |> raise_and_extract_verification

    let size t =
      Api.octez_riscv_durable_on_disk_verify_registry_size t
      |> raise_and_extract_verification

    let resize t n =
      Api.octez_riscv_durable_on_disk_verify_registry_resize t n
      |> raise_on_verification_argument

    let copy_database t ~src ~dst =
      Api.octez_riscv_durable_on_disk_verify_registry_copy t src dst
      |> raise_on_verification_argument

    let move_database t ~src ~dst =
      Api.octez_riscv_durable_on_disk_verify_registry_move t src dst
      |> raise_on_verification_argument

    let clear t db_index =
      Api.octez_riscv_durable_on_disk_verify_registry_clear t db_index
      |> raise_on_verification_argument
  end

  module Database = struct
    let exists t ~db_index ~key =
      Api.octez_riscv_durable_on_disk_verify_database_exists t db_index key
      |> raise_on_verification_argument

    let set t ~db_index ~key ~value =
      Api.octez_riscv_durable_on_disk_verify_database_set t db_index key value
      |> raise_on_verification_argument

    let write t ~db_index ~key ~offset ~value =
      Api.octez_riscv_durable_on_disk_verify_database_write
        t
        db_index
        key
        offset
        value
      |> raise_on_verification_argument

    let read t ~db_index ~key ~offset ~len =
      Api.octez_riscv_durable_on_disk_verify_database_read
        t
        db_index
        key
        offset
        len
      |> raise_on_verification_argument

    let value_length t ~db_index ~key =
      Api.octez_riscv_durable_on_disk_verify_database_value_length
        t
        db_index
        key
      |> raise_on_verification_argument

    let delete t ~db_index ~key =
      Api.octez_riscv_durable_on_disk_verify_database_delete t db_index key
      |> raise_on_verification_argument

    let hash t ~db_index =
      Api.octez_riscv_durable_on_disk_verify_database_hash t db_index
      |> raise_on_verification_argument
  end

  let start_verify proof = Api.octez_riscv_durable_on_disk_start_verify proof
end

type _ Nds.tag += Verify_tag : Verify.Registry.t Nds.tag

let unwrap_verify nds : Verify.Registry.t option =
  match Nds.unpack nds with
  | Nds.Packed {tag = Verify_tag; value; _} -> Some value
  | _ -> None
