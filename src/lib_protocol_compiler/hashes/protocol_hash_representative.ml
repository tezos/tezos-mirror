(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Maps protocol hashes to their representative. *)
module Representatives = Map.Make (Protocol_hash)

let representatives = Representatives.empty

(* Resolve is not transitive on purpose: there is no reason a protocol hash is
   represented by another protocol hash that has itself a representative. *)
let resolve_repr hash =
  Representatives.find_opt hash representatives |> Option.value ~default:hash

let equivalent protocol_hash protocol_hash' =
  let protocol_hash = resolve_repr protocol_hash in
  let protocol_hash' = resolve_repr protocol_hash' in
  Protocol_hash.equal protocol_hash protocol_hash'
