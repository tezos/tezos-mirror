(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  val replay :
    ?log_file:string ->
    ?profile:Configuration.profile_mode ->
    ?alter_evm_state:(Pvm.State.t -> Pvm.State.t tzresult Lwt.t) ->
    Ethereum_types.quantity ->
    Evm_state.apply_result tzresult Lwt.t

  val execute :
    ?alter_evm_state:(Pvm.State.t -> Pvm.State.t tzresult Lwt.t) ->
    Simulation.Encodings.simulate_input ->
    Ethereum_types.Block_parameter.extended ->
    Pvm.State.t tzresult Lwt.t
end

module No_execution : S = struct
  let replay ?log_file:_ ?profile:_ ?alter_evm_state:_ _ =
    Lwt_result_syntax.tzfail Tracer_types.Not_supported

  let execute ?alter_evm_state:_ _ _ =
    Lwt_result_syntax.tzfail Tracer_types.Not_supported
end
