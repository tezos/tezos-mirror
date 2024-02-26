(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

(** This module manifests the proof format used by the Arith PVM as defined by
    the Layer 1 implementation for it.

    It is imperative that this is aligned with the protocol's implementation.
*)
module Arith_proof_format =
  Irmin_context.Proof
    (struct
      include Sc_rollup.State_hash

      let of_context_hash = Sc_rollup.State_hash.context_hash_to_state_hash
    end)
    (struct
      let proof_encoding =
        Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree2
        .tree_proof_encoding
    end)

module Impl : Pvm_sig.S = struct
  module PVM = Sc_rollup.ArithPVM.Make (Arith_proof_format)
  include PVM

  let kind = Sc_rollup.Kind.Example_arith

  module State = Irmin_context.PVMState

  module Inspect_durable_state = struct
    let lookup _state _keys =
      raise (Invalid_argument "No durable storage for arith PVM")
  end

  let new_dissection = Game_helpers.default_new_dissection

  let string_of_status status =
    match status with
    | Halted -> "Halted"
    | Waiting_for_input_message -> "Waiting for input message"
    | Waiting_for_reveal -> "Waiting for reveal"
    | Waiting_for_metadata -> "Waiting for metadata"
    | Parsing -> "Parsing"
    | Evaluating -> "Evaluating"

  let eval_many ~reveal_builtins:_ ~write_debug:_ ?stop_at_snapshot ~max_steps
      initial_state =
    ignore stop_at_snapshot ;
    let rec go state step =
      let open Lwt.Syntax in
      let* is_input_required = is_input_state state in

      if is_input_required = No_input_required && step < max_steps then
        let open Lwt.Syntax in
        (* Note: This is not an efficient implementation because the state is
           decoded/encoded to/from the tree at each step but for Arith PVM
           it doesn't matter
        *)
        let* next_state = eval state in
        go next_state (Int64.succ step)
      else Lwt.return (state, step)
    in
    go initial_state 0L
end

include Impl
