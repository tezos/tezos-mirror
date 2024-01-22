(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol.Alpha_context

(** {1 API} *)

val bake :
  Protocol_client_context.full ->
  ?minimal_fees:Tez.t ->
  ?minimal_nanotez_per_gas_unit:Q.t ->
  ?minimal_nanotez_per_byte:Q.t ->
  ?force_apply:bool ->
  ?force:bool ->
  ?minimal_timestamp:bool ->
  ?extra_operations:Baking_configuration.Operations_source.t ->
  ?monitor_node_mempool:bool ->
  ?context_path:string ->
  ?dal_node_endpoint:Uri.t ->
  (* Number of baked blocks. Defaults to 1. *)
  ?count:int ->
  ?state_recorder:Baking_configuration.state_recorder_config ->
  Baking_state.consensus_key list ->
  unit tzresult Lwt.t

val preendorse :
  Protocol_client_context.full ->
  ?force:bool ->
  Baking_state.consensus_key list ->
  unit tzresult Lwt.t

val endorse :
  Protocol_client_context.full ->
  ?force:bool ->
  Baking_state.consensus_key list ->
  unit tzresult Lwt.t

val propose :
  Protocol_client_context.full ->
  ?minimal_fees:Tez.t ->
  ?minimal_nanotez_per_gas_unit:Q.t ->
  ?minimal_nanotez_per_byte:Q.t ->
  ?force_apply:bool ->
  ?force:bool ->
  ?minimal_timestamp:bool ->
  ?extra_operations:Baking_configuration.Operations_source.t ->
  ?context_path:string ->
  ?state_recorder:Baking_configuration.state_recorder_config ->
  Baking_state.consensus_key list ->
  unit tzresult Lwt.t

(** [repropose] tries to bake a new block proposal on the same level
    as the current head. If provided, the proposal will use the
    [force_round] argument as its reproposal round, otherwise the
    current tenderbake round will be used. *)
val repropose :
  Protocol_client_context.full ->
  ?force:bool ->
  ?force_round:Round.t ->
  Baking_state.consensus_key list ->
  unit tzresult Lwt.t
