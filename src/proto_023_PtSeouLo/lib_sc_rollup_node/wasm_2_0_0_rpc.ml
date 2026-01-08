(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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

open Rpc_directory_helpers
open Context_wrapper.Irmin

module Make_RPC
    (Durable_state :
      Wasm_2_0_0_pvm.Durable_state with type state = Irmin_context.tree) =
struct
  module Block_directory = Make_sub_directory (struct
    include Sc_rollup_services.Block

    type context = Node_context.rw

    type subcontext = Node_context.ro * Block_hash.t

    let context_of_prefix node_ctxt (((), block) : prefix) =
      let open Lwt_result_syntax in
      let+ block = Block_directory_helpers.block_of_prefix node_ctxt block in
      (Node_context.readonly node_ctxt, block)
  end)

  let get_state (node_ctxt : _ Node_context.t) block_hash =
    let open Lwt_result_syntax in
    let* ctxt = Node_context.checkout_context node_ctxt block_hash in
    let*! state = Context.PVMState.find ctxt in
    match state with None -> failwith "No state" | Some state -> return state

  let register () =
    let open Protocol.Alpha_context.Sc_rollup in
    ( Block_directory.register0
        (Sc_rollup_services.Block.durable_state_value Kind.Wasm_2_0_0)
    @@ fun (node_ctxt, block) {key} () ->
      let open Lwt_result_syntax in
      let* state = get_state node_ctxt block in
      let*! value = Durable_state.lookup (of_node_pvmstate state) key in
      return value ) ;

    ( Block_directory.register0
        (Sc_rollup_services.Block.durable_state_length Kind.Wasm_2_0_0)
    @@ fun (node_ctxt, block) {key} () ->
      let open Lwt_result_syntax in
      let* state = get_state node_ctxt block in
      let*! leng = Durable_state.value_length (of_node_pvmstate state) key in
      return leng ) ;

    ( Block_directory.register0
        (Sc_rollup_services.Block.durable_state_subkeys Kind.Wasm_2_0_0)
    @@ fun (node_ctxt, block) {key} () ->
      let open Lwt_result_syntax in
      let* state = get_state node_ctxt block in
      let*! subkeys = Durable_state.list (of_node_pvmstate state) key in
      return subkeys ) ;

    Block_directory.register0
      (Sc_rollup_services.Block.durable_state_values Kind.Wasm_2_0_0)
    @@ fun (node_ctxt, block) {key} () ->
    let open Lwt_result_syntax in
    let* state = get_state node_ctxt block in
    let tree = of_node_pvmstate state in
    let*! subkeys = Durable_state.list tree key in
    let*! bindings =
      List.filter_map_s
        (fun subkey ->
          let open Lwt_syntax in
          let+ value =
            Durable_state.lookup tree (String.concat "/" [key; subkey])
          in
          match value with None -> None | Some value -> Some (subkey, value))
        subkeys
    in
    return bindings

  let build_sub_directory node_ctxt =
    register () ;
    Block_directory.build_sub_directory node_ctxt
end
