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
      include Context_binary.Tree

      type tree = Context_binary.tree

      type t = Context_binary.t

      type key = string list

      type value = bytes
    end

    type tree = Tree.tree

    type proof = Context.Proof.tree Context.Proof.t

    let hash_tree _ = assert false

    let verify_proof p f =
      Lwt.map Result.to_option (Context_binary.verify_tree_proof p f)

    let produce_proof context state step =
      let open Lwt_syntax in
      let* context = Context_binary.add_tree context [] state in
      let* h = Context_binary.commit ~time:Time.Protocol.epoch context in
      let index = Context_binary.index context in
      let* context = Context_binary.checkout_exn index h in
      match Tree.kinded_key state with
      | Some k ->
          let index = Context_binary.index context in
          let* p = Context_binary.produce_tree_proof index k step in
          return (Some p)
      | None -> return None

    let kinded_hash_to_state_hash = function
      | `Value hash | `Node hash ->
          Sc_rollup.State_hash.context_hash_to_state_hash hash

    let proof_before proof =
      kinded_hash_to_state_hash proof.Context.Proof.before

    let proof_after proof = kinded_hash_to_state_hash proof.Context.Proof.after

    let proof_encoding =
      Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree2
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

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4386
     Extracted and adapted from {!Tezos_context_memory}. *)
  let make_empty_context ?(root = "/tmp") () =
    let open Lwt_syntax in
    let context_promise =
      let+ index = Tezos_context_memory.Context_binary.init root in
      Tezos_context_memory.Context_binary.empty index
    in
    match Lwt.state context_promise with
    | Lwt.Return result -> result
    | Lwt.Fail exn -> raise exn
    | Lwt.Sleep ->
        (* The in-memory context should never block *)
        assert false

  let origination_proof_exn ~boot_sector kind =
    let aux = function
      | Sc_rollup.Kind.Example_arith ->
          let open Lwt_result_syntax in
          let context = make_empty_context () in
          let* proof =
            Arith_pvm.produce_origination_proof context boot_sector
          in
          let*? proof =
            Sc_rollup.Proof.serialize_pvm_step ~pvm:(module Wasm_pvm) proof
          in
          return proof
      | Sc_rollup.Kind.Wasm_2_0_0 ->
          let open Lwt_result_syntax in
          let context = make_empty_context () in
          let* proof = Wasm_pvm.produce_origination_proof context boot_sector in
          let*? proof =
            Sc_rollup.Proof.serialize_pvm_step ~pvm:(module Wasm_pvm) proof
          in
          return proof
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

module EpoxyEntity = struct
  include Zk_rollup.Address

  let of_source s =
    let open Lwt_result_syntax in
    let* rollup =
      match Zk_rollup.Address.of_b58check_opt s with
      | None -> tzfail @@ error_of_fmt "bad epoxy notation"
      | Some rollup -> return rollup
    in
    return rollup

  let to_source rollup = return (Zk_rollup.Address.to_b58check rollup)

  let name = "epoxy"
end

module EpoxyAlias = Client_aliases.Alias (EpoxyEntity)
