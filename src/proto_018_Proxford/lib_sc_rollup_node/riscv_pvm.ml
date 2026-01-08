(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
module Context = Irmin_context

module type Serializable_state_S = sig
  type context

  type state

  val empty : unit -> state

  val of_index : Context.rw_index -> context

  val state_encoding : state Data_encoding.t

  val directory : string
end

module Embed_into_Irmin
    (P : Sc_rollup.PVM.S)
    (S :
      Serializable_state_S
        with type context = P.context
         and type state = P.state) : sig
  include
    Sc_rollup.PVM.S
      with type context = Context.rw_index
       and type state = Context.tree
       and type hash = Sc_rollup.State_hash.t

  val decode : Context.tree -> P.state Lwt.t
end = struct
  (* We need to instantiate this functor to access the underlying Tree module. *)
  module Irmin_proof_format =
    Context.Proof
      (struct
        include Sc_rollup.State_hash

        let of_context_hash = Sc_rollup.State_hash.context_hash_to_state_hash
      end)
      (struct
        let proof_encoding =
          Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree2
          .tree_proof_encoding
      end)

  module Tree = Irmin_proof_format.Tree

  let decode state =
    let open Lwt_syntax in
    let* bytes_opt = Tree.find state [S.directory] in
    match bytes_opt with
    | None ->
        Format.kasprintf
          Lwt.fail_with
          "Riscv_pvm: could not find state in /%s"
          S.directory
    | Some bytes ->
        Data_encoding.Binary.of_bytes_exn S.state_encoding bytes |> Lwt.return

  let apply f state =
    let open Lwt_syntax in
    let* internal_state = decode state in
    f internal_state

  let lift f state =
    let open Lwt_syntax in
    let* internal_state = decode state in
    let* internal_state = f internal_state in
    let bytes =
      Data_encoding.Binary.to_bytes_exn S.state_encoding internal_state
    in
    Tree.add state [S.directory] bytes

  type context = Context.rw_index

  let parse_boot_sector = P.parse_boot_sector

  let pp_boot_sector = P.pp_boot_sector

  type state = Context.tree

  type hash = Sc_rollup.State_hash.t

  type proof = P.proof

  let pp = apply P.pp

  let proof_encoding = P.proof_encoding

  let proof_start_state = P.proof_start_state

  let proof_stop_state = P.proof_stop_state

  let state_hash = apply P.state_hash

  let initial_state ~empty =
    let empty_state = S.empty () in
    let bytes =
      Data_encoding.Binary.to_bytes_exn S.state_encoding empty_state
    in
    Tree.add empty [S.directory] bytes

  let install_boot_sector state boot_sector =
    lift (fun state -> P.install_boot_sector state boot_sector) state

  let is_input_state ~is_reveal_enabled state =
    apply (P.is_input_state ~is_reveal_enabled) state

  let set_input input state = lift (P.set_input input) state

  let eval = lift P.eval

  let verify_proof = P.verify_proof

  let produce_proof context ~is_reveal_enabled input_opt state =
    apply
      (P.produce_proof (S.of_index context) ~is_reveal_enabled input_opt)
      state

  type output_proof = P.output_proof

  let output_proof_encoding = P.output_proof_encoding

  let output_of_output_proof = P.output_of_output_proof

  let state_of_output_proof = P.state_of_output_proof

  let verify_output_proof = P.verify_output_proof

  let produce_output_proof context state output =
    apply
      (fun state -> P.produce_output_proof (S.of_index context) state output)
      state

  let check_dissection = P.check_dissection

  let get_current_level = apply P.get_current_level

  module Internal_for_tests = struct
    let insert_failure = lift P.Internal_for_tests.insert_failure
  end
end

module Serializable_riscv_state = struct
  type context = Sc_rollup.Riscv_PVM.Protocol_implementation.context

  type state = Sc_rollup.Riscv_PVM.Protocol_implementation.state

  let empty = Sc_rollup_riscv.make_empty_state

  let directory = "riscv_pvm"

  let of_index _index = ()

  let state_encoding = Sc_rollup_riscv.minimal_state_encoding
end

include
  Embed_into_Irmin
    (Sc_rollup.Riscv_PVM.Protocol_implementation)
    (Serializable_riscv_state)

let kind = Sc_rollup.Kind.Riscv

let get_tick state =
  let open Lwt_syntax in
  let* state = decode state in
  Lwt.return (Sc_rollup.Tick.of_z state.Sc_rollup_riscv.tick)

type status = Riscv_dummy_status

let get_status ~is_reveal_enabled:_ _state = Lwt.return Riscv_dummy_status

let string_of_status Riscv_dummy_status = "riscv_dummy_status"

let get_outbox _level _state = Lwt.return []

(* It is safe to pass the [is_reveal_enabled_predicate]:
   [eval_many] always stops at the beginning of a new Tezos block,
   so no execution of several Tezos block inboxes is possible. *)
(* Copied from [arith_pvm.ml]. *)
let eval_many ?check_invalid_kernel:_ ~reveal_builtins:_ ~write_debug:_
    ~is_reveal_enabled ?stop_at_snapshot ~max_steps initial_state =
  ignore stop_at_snapshot ;
  ignore max_steps ;
  ignore is_reveal_enabled ;
  Lwt.return (initial_state, 0L)

let new_dissection = Game_helpers.default_new_dissection

module State = Context.PVMState

module Inspect_durable_state = struct
  let lookup _state _keys =
    raise (Invalid_argument "No durable storage for riscv PVM")
end

module Unsafe_patches = struct
  (** No unsafe patches for the riscv PVM. *)
  type t = |

  let of_patch (p : Pvm_patches.unsafe_patch) =
    match p with
    | Increase_max_nb_ticks _ -> assert false
    | Patch_durable_storage _ -> assert false

  let apply _state (x : t) = match x with _ -> .
end
