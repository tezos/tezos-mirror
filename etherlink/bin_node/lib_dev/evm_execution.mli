(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Signature for a module allowing to execute the kernel on top of existing
    states. *)
module type S = sig
  (** See {!Evm_context.replay}. *)
  val replay :
    ?log_file:string ->
    ?profile:Configuration.profile_mode ->
    ?alter_evm_state:(Pvm.State.t -> Pvm.State.t tzresult Lwt.t) ->
    Ethereum_types.quantity ->
    Evm_state.apply_result tzresult Lwt.t

  (** See {!Evm_context.execute}. *)
  val execute :
    ?alter_evm_state:(Pvm.State.t -> Pvm.State.t tzresult Lwt.t) ->
    Simulation.Encodings.simulate_input ->
    Ethereum_types.Block_parameter.extended ->
    Pvm.State.t tzresult Lwt.t
end

(** A placeholder module for when executing EVM code is not available. *)
module No_execution : S
