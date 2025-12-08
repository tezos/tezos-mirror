(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2024-2025 Trilitech <contact@trili.tech>                    *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
module Context = Riscv_context
module Storage = Octez_riscv_pvm.Storage

type repo = Context.repo

type tree = Context.tree

module State = Riscv_context.PVMState
module Backend = Octez_riscv_pvm.Backend
module Ctxt_wrapper = Context_wrapper.Riscv

let to_pvm_input (input : Sc_rollup.input) : Backend.input =
  match input with
  | Sc_rollup.Inbox_message {inbox_level; message_counter; payload} ->
      Inbox_message
        ( Raw_level.to_int32 inbox_level,
          Z.to_int64 message_counter,
          Sc_rollup.Inbox_message.unsafe_to_string payload )
  | Sc_rollup.(Reveal _reveal_data) -> assert false

let of_pvm_input_request (_input_request : Backend.input_request) :
    Sc_rollup.input_request =
  raise (Invalid_argument "input_request not implemented")

let make_is_input_state (get_status : 'a -> Backend.status Lwt.t)
    (get_current_level : 'a -> int32 option Lwt.t)
    (get_message_counter : 'a -> int64 Lwt.t) ~is_reveal_enabled:_ state =
  let open Lwt_syntax in
  let* status = get_status state in
  match status with
  | Evaluating -> return Sc_rollup.No_input_required
  | Waiting_for_input -> (
      let* level = get_current_level state in
      match level with
      | None -> return Sc_rollup.Initial
      | Some level ->
          let* message_counter = get_message_counter state in
          return
            (Sc_rollup.First_after
               (Raw_level.of_int32_exn level, Z.of_int64 message_counter)))
  | Waiting_for_reveal ->
      (* TODO: RV-407: Rollup node handles reveal request from riscv pvm *)
      assert false

module Insert_failure_impl = struct
  let insert_failure _state =
    raise (Invalid_argument "insert_failure not implemented")
end

module PVM :
  Sc_rollup.PVM.S
    with type state = tree
     and type context = Riscv_context.rw_index = struct
  let parse_boot_sector s = Some s

  let pp_boot_sector fmt s = Format.fprintf fmt "%s" s

  type void = |

  let void =
    Data_encoding.(
      conv_with_guard
        (function (_ : void) -> .)
        (fun _ -> Error "void has no inhabitant")
        unit)

  type state = tree

  let pp _ = raise (Invalid_argument "pp not implemented")

  type context = Context.rw_index

  type hash = Sc_rollup.State_hash.t

  type proof = Backend.proof

  let proof_encoding =
    Data_encoding.(
      conv_with_guard
        (function (_ : proof) -> ())
        (fun _ -> Error "proofs not implemented")
        unit)

  let proof_start_state proof = Backend.proof_start_state proof

  let proof_stop_state proof = Backend.proof_stop_state proof

  let state_hash state = Lwt.return @@ Backend.state_hash state

  let initial_state ~empty:_ = Lwt.return (Storage.empty ())

  let install_boot_sector state boot_sector =
    Backend.install_boot_sector state boot_sector

  let is_input_state =
    make_is_input_state
      Backend.get_status
      Backend.get_current_level
      Backend.get_message_counter

  let set_input input state = Backend.set_input state (to_pvm_input input)

  let eval state = Backend.compute_step state

  let verify_proof ~is_reveal_enabled:_ input_given proof =
    let open Environment.Error_monad.Lwt_result_syntax in
    match Backend.verify_proof (Option.map to_pvm_input input_given) proof with
    | None -> tzfail Sc_rollup_riscv.RISCV_proof_verification_failed
    | Some request -> return (of_pvm_input_request request)

  let produce_proof _context ~is_reveal_enabled:_ input_given state =
    let open Environment.Error_monad.Lwt_result_syntax in
    match Backend.produce_proof (Option.map to_pvm_input input_given) state with
    | None -> tzfail Sc_rollup_riscv.RISCV_proof_production_failed
    | Some proof -> return proof

  type output_proof = void

  let output_proof_encoding = void

  let output_of_output_proof = function (_ : output_proof) -> .

  let state_of_output_proof = function (_ : output_proof) -> .

  let verify_output_proof = function (_ : output_proof) -> .

  let produce_output_proof _context _state _output = assert false

  let check_dissection ~default_number_of_sections:_ ~start_chunk:_
      ~stop_chunk:_ =
    assert false

  let get_current_level state =
    let open Lwt_syntax in
    let* level = Backend.get_current_level state in
    return (Option.map Raw_level.of_int32_exn level)

  module Internal_for_tests = Insert_failure_impl
end

include PVM

let kind = Sc_rollup.Kind.Riscv

let get_tick state =
  let open Lwt_syntax in
  let* tick = Backend.get_tick state in
  return (Sc_rollup.Tick.of_z tick)

type status = Backend.status

let get_status ~is_reveal_enabled:_ state = Backend.get_status state

let string_of_status status = Backend.string_of_status status

let get_outbox _level _state = Lwt.return []

let eval_many ?check_invalid_kernel:_ ~reveal_builtins:_ ~write_debug
    ~is_reveal_enabled:_ ?stop_at_snapshot ~max_steps initial_state =
  let debug_printer =
    match write_debug with
    | Tezos_scoru_wasm.Builtins.Noop -> None
    | Tezos_scoru_wasm.Builtins.Printer p -> Some p
  in
  Backend.compute_step_many
    ?stop_at_snapshot
    ?write_debug:debug_printer
    ~max_steps
    initial_state

module Mutable_state :
  Pvm_sig.MUTABLE_STATE_S
    with type hash = PVM.hash
     and type t = Ctxt_wrapper.mut_state = struct
  type t = Backend.Mutable_state.t

  type hash = PVM.hash

  let get_tick state =
    let open Lwt_syntax in
    let* tick = Backend.Mutable_state.get_tick state in
    return (Sc_rollup.Tick.of_z tick)

  let state_hash state = Lwt.return @@ Backend.Mutable_state.state_hash state

  let is_input_state =
    make_is_input_state
      Backend.Mutable_state.get_status
      Backend.Mutable_state.get_current_level
      Backend.Mutable_state.get_message_counter

  let set_input input state =
    Backend.Mutable_state.set_input state @@ to_pvm_input input

  let eval_many ?check_invalid_kernel:_ ~reveal_builtins:_ ~write_debug
      ~is_reveal_enabled:_ ?stop_at_snapshot ~max_steps initial_state =
    let debug_printer =
      match write_debug with
      | Tezos_scoru_wasm.Builtins.Noop -> None
      | Tezos_scoru_wasm.Builtins.Printer p -> Some p
    in
    Backend.Mutable_state.compute_step_many
      ?stop_at_snapshot
      ?write_debug:debug_printer
      ~max_steps
      initial_state

  module Internal_for_tests = Insert_failure_impl
end

let new_dissection = Game_helpers.default_new_dissection

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
    | Patch_PVM_version _ -> assert false

  let apply _state (x : t) = match x with _ -> .
end
