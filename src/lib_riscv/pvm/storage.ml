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

module Id = struct
  type t = Api.id

  let unsafe_of_raw_string hash = Api.octez_riscv_id_unsafe_of_raw_string hash

  let to_raw_string id = Api.octez_riscv_storage_id_to_raw_string id

  let equal id1 id2 = Api.octez_riscv_storage_id_equal id1 id2
end

let load ~cache_size:_ ~readonly:_ path =
  Lwt.return (Api.octez_riscv_storage_load path)

let close repo =
  Api.octez_riscv_storage_close repo ;
  Lwt.return_unit

let checkout repo id = Lwt.return (Api.octez_riscv_storage_checkout repo id)

let empty () = Api.octez_riscv_storage_state_empty ()

let commit ?message:_ repo state =
  Lwt.return (Api.octez_riscv_storage_commit repo state)

let is_gc_finished _repo = true

let split _repo = raise (Invalid_argument "split not implemented")

let gc _repo ?callback:_ _key = Lwt.return_unit

let wait_gc_completion _repo = Lwt.return_unit

let export_snapshot _repo _key _path =
  raise (Invalid_argument "export_snapshot not implemented")

let find _state _key = raise (Invalid_argument "find not implemented")

let lookup _state _key = raise (Invalid_argument "find not implemented")

let set _state _key _substate = raise (Invalid_argument "set not implemented")

let add _state _key _bytes = raise (Invalid_argument "add not implemented")
