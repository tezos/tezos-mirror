(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Api = Octez_riscv_api

module Repo = struct
  type t = Api.repo
end

module State = struct
  type t = Api.state

  let equal state1 state2 = Api.octez_riscv_storage_state_equal state1 state2
end

module Mutable_state = struct
  type t = Api.mut_state

  let equal state1 state2 =
    Api.octez_riscv_storage_mut_state_equal state1 state2
end

module Id = struct
  type t = Api.id

  let unsafe_of_raw_string hash =
    Api.octez_riscv_id_unsafe_of_raw_bytes (Bytes.of_string hash)

  let to_raw_string id =
    String.of_bytes (Api.octez_riscv_storage_id_to_raw_bytes id)

  let equal id1 id2 = Api.octez_riscv_storage_id_equal id1 id2
end

let load ~cache_size:_ ~readonly:_ path =
  Lwt.return (Api.octez_riscv_storage_load path)

let close repo =
  Api.octez_riscv_storage_close repo ;
  Lwt.return_unit

let checkout repo id = Lwt.return (Api.octez_riscv_storage_checkout repo id)

let empty () = Api.octez_riscv_storage_mut_state_empty ()

let commit ?message:_ repo state =
  Lwt.return (Api.octez_riscv_storage_commit repo state)

let is_gc_finished _repo = true

let cancel_gc _repo = false

let split _repo = ()

let gc _repo ?callback:_ _key = Lwt.return_unit

let wait_gc_completion _repo = Lwt.return_unit

let export_snapshot repo hash path =
  let open Lwt_result_syntax in
  match Api.octez_riscv_storage_export_snapshot repo hash path with
  | Ok () -> return_unit
  | Error (`Msg e) -> tzfail (Exn (Failure e))

let pvm_state_key = ["pvm_state"]

let find state key =
  (* The entire context is the PVM state, no other keys are supported *)
  if key == pvm_state_key then Lwt.return_some state else Lwt.return_none

(* Only used to inspect the durable storage, currently not supported *)
let lookup _state _key = raise (Invalid_argument "lookup not implemented")

let set state key substate =
  (* The entire context is the PVM state, no other keys are supported *)
  if key != pvm_state_key then raise (Invalid_argument "key not supported") ;
  (* substate should have been obtained by modifying state *)
  if state != substate then
    raise
      (Invalid_argument
         "state update not supported, modify mutable state instead") ;
  Lwt.return_unit

(* Only used for internal testing of the rollup node, not supported *)
let add _state _key _bytes = raise (Invalid_argument "add not implemented")
