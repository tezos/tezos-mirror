(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type repo = unit

type tree = unit

module Id = struct
  type t = string

  let unsafe_of_raw_string hash = hash

  let to_raw_string id = id
end

let load ~cache_size:_ ~readonly:_ _path =
  raise (Invalid_argument "load not implemented")

let close _repo = raise (Invalid_argument "close not implemented")

let checkout _repo _hash = raise (Invalid_argument "checkout not implemented")

let empty () = raise (Invalid_argument "empty not implemented")

let commit ?message:_ _repo _tree =
  raise (Invalid_argument "commit not implemented")

let is_gc_finished _repo = true

let split _repo = raise (Invalid_argument "split not implemented")

let gc _repo ?callback:_ _key = Lwt.return_unit

let wait_gc_completion _repo = Lwt.return_unit

let export_snapshot _repo _key _path =
  raise (Invalid_argument "export_snapshot not implemented")

let find _tree _key = raise (Invalid_argument "find not implemented")

let lookup _tree _key = raise (Invalid_argument "find not implemented")

let set _tree _key _subtree = raise (Invalid_argument "set not implemented")

let add _tree _key _bytes = raise (Invalid_argument "add not implemented")
