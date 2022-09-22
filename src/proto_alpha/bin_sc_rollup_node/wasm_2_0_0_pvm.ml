(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** This module manifests the proof format used by the Wasm PVM as defined by
    the Layer 1 implementation for it.

    It is imperative that this is aligned with the protocol's implementation.
*)
module Wasm_2_0_0_proof_format =
  Context.Proof
    (struct
      include Sc_rollup.State_hash

      let of_context_hash = Sc_rollup.State_hash.context_hash_to_state_hash
    end)
    (struct
      let proof_encoding =
        Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree32
        .tree_proof_encoding
    end)

module Conversions = struct
  (* BEWARE OF UGLY HACK !!!
     Environment.Wasm_2_0_0.info/input/output are not compatible with
      Tezos_scoru_wasm.Wasm_pvm_sig.info/input_info/output_info/
      although they should be, by their definitions:

      ```
      type input = Tezos_scoru_wasm.Wasm_pvm_sig.input_info = ...
      type output = Tezos_scoru_wasm.Wasm_pvm_sig.input_info = ...
      ```

      Somehow the type equality is not preserved through the protocol environment.

      This is why we need to do the de/reconstruction manualy.
  *)
  module EInt32 = Environment.Bounded.Non_negative_int32
  module SInt32 = Bounded.Non_negative_int32
  module Opt = Stdlib.Option
  module Scoru = Tezos_scoru_wasm.Wasm_pvm_sig
  module Env = Environment.Wasm_2_0_0

  let from_env_int32 (i : EInt32.t) : SInt32.t =
    EInt32.to_value i |> SInt32.of_value |> Opt.get

  let to_env_int32 (i : SInt32.t) : EInt32.t =
    SInt32.to_value i |> EInt32.of_value |> Opt.get

  let to_env_input (input : Scoru.input_info) : Env.input =
    {
      inbox_level = to_env_int32 input.inbox_level;
      message_counter = input.message_counter;
    }

  let to_env_input_request : Scoru.input_request -> Env.input_request = function
    | Scoru.No_input_required -> Env.No_input_required
    | Input_required -> Input_required

  let of_env_input (input : Env.input) : Scoru.input_info =
    {
      inbox_level = from_env_int32 input.inbox_level;
      message_counter = input.message_counter;
    }

  let of_env_output (output : Env.output) : Scoru.output_info =
    {
      outbox_level = from_env_int32 output.outbox_level;
      message_index = output.message_index;
    }

  let to_env_info (s_info : Scoru.info) : Env.info =
    {
      current_tick = s_info.current_tick;
      last_input_read = Opt.map to_env_input s_info.last_input_read;
      input_request = to_env_input_request s_info.input_request;
    }
end

module type TreeS =
  Tezos_context_sigs.Context.TREE
    with type key = string list
     and type value = bytes

module Make_backend (Tree : TreeS) = struct
  type Lazy_containers.Lazy_map.tree += PVM_tree of Tree.tree

  module Wasm_pvm = Tezos_scoru_wasm.Wasm_pvm.Make (struct
    include Tree

    let select = function
      | PVM_tree t -> t
      | _ -> raise Tree_encoding.Incorrect_tree_type

    let wrap t = PVM_tree t
  end)

  let compute_step (tree : Tree.tree) = Wasm_pvm.compute_step tree

  include Conversions

  let set_input_step (input : Environment.Wasm_2_0_0.input) payload
      (tree : Tree.tree) =
    Wasm_pvm.set_input_step (of_env_input input) payload tree

  let get_output (output : Environment.Wasm_2_0_0.output) (tree : Tree.tree) =
    Wasm_pvm.get_output (of_env_output output) tree

  let get_info (tree : Tree.tree) =
    Lwt.map to_env_info @@ Wasm_pvm.get_info tree
end

module Impl : Pvm.S = struct
  include Sc_rollup.Wasm_2_0_0PVM.Make (Make_backend) (Wasm_2_0_0_proof_format)
  module State = Context.PVMState

  let string_of_status : status -> string = function
    | Waiting_for_input_message -> "Waiting for input message"
    | Computing -> "Computing"
end

include Impl
