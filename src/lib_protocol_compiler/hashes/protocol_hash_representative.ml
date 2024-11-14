(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Maps protocol hashes to their representative. *)
module Representatives = Map.Make (Protocol_hash)

let register_protocol_hash computed_hash declared_hash map =
  Representatives.add
    (Protocol_hash.of_b58check_exn computed_hash)
    (Protocol_hash.of_b58check_exn declared_hash)
    map

let representatives =
  Representatives.empty
  (* Protocol used in tests only, see `tezt/test/injection.ml`. *)
  |> register_protocol_hash
       "Pry4stD6qN1ZagUX6YCHvMxA1xvSjARkdt8bhs86j74JGLoLDKN"
       "Ps8MVx2JuQaFrXpbuwSeBvXmi1xraGmXuJZHULEbPBY7mzFchxz"

(* Resolve is not transitive on purpose: there is no reason a protocol hash is
   represented by another protocol hash that has itself a representative. *)
let resolve_repr hash =
  Representatives.find_opt hash representatives |> Option.value ~default:hash

let equivalent protocol_hash protocol_hash' =
  let protocol_hash = resolve_repr protocol_hash in
  let protocol_hash' = resolve_repr protocol_hash' in
  Protocol_hash.equal protocol_hash protocol_hash'
