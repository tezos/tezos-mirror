(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>          *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

include
  Irmin_context.Proof
    (struct
      include Octez_smart_rollup.State_hash

      let of_context_hash =
        Octez_smart_rollup.State_hash.context_hash_to_state_hash
    end)
    (struct
      let proof_encoding =
        Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree2
        .tree_proof_encoding
    end)

type context = Irmin_context.rw_index

let proof_start_state = proof_before

let proof_stop_state = proof_after

module Wrapped_tree :
  Tezos_tree_encoding.TREE with type tree = Irmin_context.tree = struct
  type Tezos_tree_encoding.tree_instance += PVM_tree of Irmin_context.tree

  include Irmin_context.Tree

  let select = function
    | PVM_tree t -> t
    | _ -> raise Tezos_tree_encoding.Incorrect_tree_type

  let wrap t = PVM_tree t
end

include Tezos_scoru_wasm.Tree_state.Make (Wrapped_tree)

let empty_state () = Irmin_context.Tree.empty ()

let state_hash tree = hash_tree tree |> Lwt.return
