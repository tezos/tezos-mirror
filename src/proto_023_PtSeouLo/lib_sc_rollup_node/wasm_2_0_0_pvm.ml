(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2024 TriliTech <contact@trili.tech>                    *)
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

module type TreeS =
  Tezos_context_sigs.Context.TREE
    with type key = string list
     and type value = bytes

module Make_wrapped_tree (Tree : TreeS) :
  Tezos_tree_encoding.TREE with type tree = Tree.tree = struct
  type Tezos_tree_encoding.tree_instance += PVM_tree of Tree.tree

  include Tree

  let select = function
    | PVM_tree t -> t
    | _ -> raise Tezos_tree_encoding.Incorrect_tree_type

  let wrap t = PVM_tree t
end

module Make_backend (Tree : TreeS) = struct
  include Tezos_scoru_wasm_fast.Pvm.Make (Make_wrapped_tree (Tree))

  let compute_step =
    compute_step ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
end

(** Durable part of the storage of this PVM. *)
module type Durable_state = sig
  type state

  (** [value_length state key] returns the length of data stored
        for the [key] in the durable storage of the PVM state [state], if any. *)
  val value_length : state -> string -> int64 option Lwt.t

  (** [lookup state key] returns the data stored
        for the [key] in the durable storage of the PVM state [state], if any. *)
  val lookup : state -> string -> bytes option Lwt.t

  (** [subtrees state key] returns subtrees
        for the [key] in the durable storage of the PVM state [state].
        Empty list in case if path doesn't exist. *)
  val list : state -> string -> string list Lwt.t

  module Tree_encoding_runner :
    Tezos_tree_encoding.Runner.S with type tree = state
end

module Make_durable_state
    (T : Tezos_tree_encoding.TREE with type tree = Irmin_context.tree) :
  Durable_state with type state = T.tree = struct
  module Tree_encoding_runner = Tezos_tree_encoding.Runner.Make (T)

  type state = T.tree

  let decode_durable tree =
    Tree_encoding_runner.decode
      Tezos_scoru_wasm.Wasm_pvm.durable_storage_encoding
      tree

  let value_length tree key_str =
    let open Lwt_syntax in
    let key = Tezos_scoru_wasm.Durable.key_of_string_exn key_str in
    let* durable = decode_durable tree in
    let+ res_opt = Tezos_scoru_wasm.Durable.find_value durable key in
    Option.map Tezos_lazy_containers.Chunked_byte_vector.length res_opt

  let lookup tree key_str =
    let open Lwt_syntax in
    let key = Tezos_scoru_wasm.Durable.key_of_string_exn key_str in
    let* durable = decode_durable tree in
    let* res_opt = Tezos_scoru_wasm.Durable.find_value durable key in
    match res_opt with
    | None -> return_none
    | Some v ->
        let+ bts = Tezos_lazy_containers.Chunked_byte_vector.to_bytes v in
        Some bts

  let list tree key_str =
    let open Lwt_syntax in
    let key = Tezos_scoru_wasm.Durable.key_of_string_exn key_str in
    let* durable = decode_durable tree in
    Tezos_scoru_wasm.Durable.list durable key
end

module Durable_state =
  Make_durable_state (Make_wrapped_tree (Wasm_2_0_0_proof_format.Tree))

type unsafe_patch =
  | Increase_max_nb_ticks of int64
  | Patch_durable_storage of {key : string; value : string}
  | Patch_PVM_version of {version : string}

module Impl : Pvm_sig.S with type Unsafe_patches.t = unsafe_patch = struct
  module PVM =
    Sc_rollup.Wasm_2_0_0PVM.Make (Make_backend) (Wasm_2_0_0_proof_format)
  include PVM

  type repo = Irmin_context.repo

  type tree = Irmin_context.tree

  module Ctxt_wrapper = Context_wrapper.Irmin

  let kind = Sc_rollup.Kind.Wasm_2_0_0

  let new_dissection = Game_helpers.Wasm.new_dissection

  module State = Irmin_context.PVMState

  module Inspect_durable_state = struct
    let lookup state keys =
      let key = "/" ^ String.concat "/" keys in
      Durable_state.lookup state key
  end

  module Backend = Make_backend (Wasm_2_0_0_proof_format.Tree)

  module Unsafe_patches = struct
    type t = unsafe_patch

    let of_patch (p : Pvm_patches.unsafe_patch) =
      match p with
      | Increase_max_nb_ticks max_nb_ticks ->
          Ok (Increase_max_nb_ticks max_nb_ticks)
      | Patch_durable_storage {key; value} ->
          Ok (Patch_durable_storage {key; value})
      | Patch_PVM_version {version} -> Ok (Patch_PVM_version {version})

    let apply state unsafe_patch =
      let open Lwt_syntax in
      match unsafe_patch with
      | Increase_max_nb_ticks max_nb_ticks ->
          let* registered_max_nb_ticks =
            Backend.Unsafe.get_max_nb_ticks state
          in
          let max_nb_ticks = Z.of_int64 max_nb_ticks in
          if Z.Compare.(max_nb_ticks < registered_max_nb_ticks) then
            Format.ksprintf
              invalid_arg
              "Decreasing tick limit of WASM PVM from %s to %s is not allowed"
              (Z.to_string registered_max_nb_ticks)
              (Z.to_string max_nb_ticks) ;
          Backend.Unsafe.set_max_nb_ticks max_nb_ticks state
      | Patch_durable_storage {key; value} ->
          Backend.Unsafe.durable_set ~key ~value state
      | Patch_PVM_version {version = version_str} -> (
          let version_opt =
            Tezos_scoru_wasm.Wasm_pvm_state.version_of_string version_str
          in
          match version_opt with
          | Some version -> Backend.Unsafe.set_pvm_version ~version state
          | None ->
              invalid_arg
                (Format.sprintf
                   "Unsafe patch: unknown PVM version %s"
                   version_str))
  end

  let string_of_status : status -> string = function
    | Waiting_for_input_message -> "Waiting for input message"
    | Waiting_for_reveal (Sc_rollup.Reveal_raw_data hash) ->
        Format.asprintf
          "Waiting for preimage reveal %a"
          Sc_rollup_reveal_hash.pp
          hash
    | Waiting_for_reveal Sc_rollup.Reveal_metadata -> "Waiting for metadata"
    | Waiting_for_reveal (Sc_rollup.Request_dal_page page_id) ->
        Format.asprintf "Waiting for page data %a" Dal.Page.pp page_id
    | Waiting_for_reveal Sc_rollup.Reveal_dal_parameters ->
        "Waiting for DAL parameters"
    | Computing -> "Computing"
    | Waiting_for_reveal (Request_adal_page _) ->
        (* ADAL/FIXME: https://gitlab.com/tezos/tezos/-/milestones/410

           To be implemented. *)
        assert false

  let eval_many ?(check_invalid_kernel = true) ~reveal_builtins ~write_debug
      ~is_reveal_enabled:_ =
    Backend.compute_step_many
      ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
      ~reveal_builtins
      ~write_debug
      ~hooks:(Wasm_2_0_0_utilities.hooks ~check_invalid_kernel)

  (** WASM PVM Mutable API works by holding a reference to an immutable state
      and wrapping all immutable functionality around the reference *)
  module Mutable_state :
    Pvm_sig.MUTABLE_STATE_S
      with type hash = hash
       and type t = Ctxt_wrapper.mut_state = struct
    type t = tree ref

    type hash = Sc_rollup.State_hash.t

    let get_tick state = get_tick !state

    let state_hash state = state_hash !state

    let is_input_state ~is_reveal_enabled state =
      is_input_state ~is_reveal_enabled !state

    let set_input input state =
      let open Lwt_syntax in
      let* imm_state = set_input input !state in
      state := imm_state ;
      return_unit

    let eval_many ?check_invalid_kernel ~reveal_builtins ~write_debug
        ~is_reveal_enabled ?stop_at_snapshot ~max_steps mut_state =
      let open Lwt_syntax in
      let* imm_state, steps =
        eval_many
          ?check_invalid_kernel
          ~reveal_builtins
          ~write_debug
          ~is_reveal_enabled
          ?stop_at_snapshot
          ~max_steps
          !mut_state
      in
      mut_state := imm_state ;
      return steps

    module Internal_for_tests = struct
      let insert_failure state =
        let open Lwt_syntax in
        let* imm_state = Internal_for_tests.insert_failure !state in
        state := imm_state ;
        return_unit
    end
  end
end

include Impl
