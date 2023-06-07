(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

type rollup_entity = {rollup : Tx_rollup.t; origination_level : int32 option}

module TxRollupEntity = struct
  type t = rollup_entity

  include Compare.Make (struct
    type t = rollup_entity

    let compare r1 r2 = Tx_rollup.compare r1.rollup r2.rollup
  end)

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"rollup without origination level"
          (Tag 0)
          Tx_rollup.encoding
          (function
            | {rollup; origination_level = None} -> Some rollup | _ -> None)
          (fun rollup -> {rollup; origination_level = None});
        case
          ~title:"rollup with origination level"
          (Tag 1)
          (obj2
             (req "rollup" Tx_rollup.encoding)
             (req "origination_level" int32))
          (function
            | {rollup; origination_level = Some level} -> Some (rollup, level)
            | _ -> None)
          (fun (rollup, level) -> {rollup; origination_level = Some level});
      ]

  let of_source s =
    let open Lwt_result_syntax in
    let*? rollup =
      Tx_rollup.of_b58check s |> Environment.wrap_tzresult
      |> record_trace_eval (fun () ->
             error_of_fmt "bad transaction rollup notation")
    in
    return {rollup; origination_level = None}

  let to_source {rollup; _} = return (Tx_rollup.to_b58check rollup)

  let name = "tx_rollup"
end

module TxRollupAlias = Client_aliases.Alias (TxRollupEntity)

module ScRollup = struct
  module In_memory_context = struct
    open Tezos_context_memory

    module Tree = struct
      include Context.Tree

      type tree = Context.tree

      type t = Context.t

      type key = string list

      type value = bytes
    end

    type tree = Tree.tree

    type proof = Context.Proof.tree Context.Proof.t

    let hash_tree _ = assert false

    let verify_proof p f =
      Lwt.map Result.to_option (Context.verify_tree_proof p f)

    let produce_proof context state step =
      let open Lwt_syntax in
      let* context = Context.add_tree context [] state in
      let* h = Context.commit ~time:Time.Protocol.epoch context in
      let index = Context.index context in
      let* context = Context.checkout_exn index h in
      match Tree.kinded_key state with
      | Some k ->
          let index = Context.index context in
          let* p = Context.produce_tree_proof index k step in
          return (Some p)
      | None -> return None

    let kinded_hash_to_state_hash = function
      | `Value hash | `Node hash ->
          Sc_rollup.State_hash.context_hash_to_state_hash hash

    let proof_before proof =
      kinded_hash_to_state_hash proof.Context.Proof.before

    let proof_after proof = kinded_hash_to_state_hash proof.Context.Proof.after

    let proof_encoding =
      Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree32
      .tree_proof_encoding
  end

  module Arith_pvm :
    Sc_rollup.PVM.S
      with type context = In_memory_context.Tree.t
       and type state = In_memory_context.tree
       and type proof =
        Tezos_context_memory.Context.Proof.tree
        Tezos_context_memory.Context.Proof.t =
    Sc_rollup.ArithPVM.Make (In_memory_context)

  module Wasm_pvm :
    Sc_rollup.PVM.S
      with type context = In_memory_context.Tree.t
       and type state = In_memory_context.tree
       and type proof =
        Tezos_context_memory.Context.Proof.tree
        Tezos_context_memory.Context.Proof.t =
    Sc_rollup.Wasm_2_0_0PVM.Make
      (Environment.Wasm_2_0_0.Make)
      (In_memory_context)

  let origination_proof_exn ~boot_sector kind =
    let aux = function
      | Sc_rollup.Kind.Example_arith ->
          let open Lwt_result_syntax in
          let context = Tezos_context_memory.Context.make_empty_context () in
          let* proof =
            Arith_pvm.produce_origination_proof context boot_sector
          in
          return
            (Sc_rollup.Arith_pvm_with_proof
               (module struct
                 include Arith_pvm

                 let proof = proof
               end))
      | Sc_rollup.Kind.Wasm_2_0_0 ->
          let open Lwt_result_syntax in
          let context = Tezos_context_memory.Context.make_empty_context () in
          let* proof = Wasm_pvm.produce_origination_proof context boot_sector in
          return
            (Sc_rollup.Wasm_2_0_0_pvm_with_proof
               (module struct
                 include Wasm_pvm

                 let proof = proof
               end))
    in
    let open Lwt_syntax in
    let* res = aux kind in
    match res with
    | Ok res -> Lwt.return res
    | Error _ ->
        raise
          (Invalid_argument
             "origination_proof_exn: could not produce an origination proof")
end
