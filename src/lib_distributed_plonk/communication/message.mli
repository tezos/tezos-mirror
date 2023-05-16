(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module type S = sig
  (** Plonk Main Protocol *)
  module MP : Distribution.Main_protocol.S

  (** Type witness for a protocol step. *)
  type 'a step =
    | S_ctw : [`Commit_to_wires] step
    | S_ctp : [`Commit_to_plook] step
    | S_pppi : [`PP_prepare_ids] step
    | S_ppctt : [`PP_commit_to_t] step
    | S_ppkeax : [`PP_kzg_eval_at_x] step
    | S_pcd : [`PC_distribution] step

  (* All message payloads need to have an index, so that
     they can be sorted back by the master. *)
  type ctw_payload = {index : int; content : MP.worker_inputs Plonk.SMap.t}

  type ctpap_payload = {index : int; content : MP.PP.transcript}

  type pppi_payload = {index : int; content : MP.PP.transcript}

  type ppctt_payload = {index : int; content : string list * Plonk.Bls.Scalar.t}

  type ppkeax_payload = {index : int; content : MP.PP.transcript}

  type pcd_payload = {index : int; content : MP.PP.PC.worker_msg}

  type 'step request =
    | Commit_to_wires : ctw_payload -> [`Commit_to_wires] request
    | Commit_to_plook : ctpap_payload -> [`Commit_to_plook] request
    | PP_prepare_ids : pppi_payload -> [`PP_prepare_ids] request
    | PP_commit_to_t : ppctt_payload -> [`PP_commit_to_t] request
    | PP_KZG_eval_at_x : ppkeax_payload -> [`PP_kzg_eval_at_x] request
    | PC_Distribution : pcd_payload -> [`PC_distribution] request

  type ctw_res_payload = {index : int; content : MP.commit_to_wires_reply}

  type ctpap_res_payload = {index : int; content : MP.commit_to_plook_rc_reply}

  type pppi_res_payload = {index : int; content : string list}

  type ppctt_res_payload = {index : int; content : Plonk.Bls.Evaluations.t}

  type ppkeax_res_payload = {index : int; content : MP.PP.PC.answer list}

  type pcd_res_payload = {index : int; content : MP.PP.PC.main_prover_msg}

  type 'step reply =
    | Commit_to_wires_res : ctw_res_payload -> [`Commit_to_wires] reply
    | Commit_to_plook_res : ctpap_res_payload -> [`Commit_to_plook] reply
    | PP_prepare_ids_res : pppi_res_payload -> [`PP_prepare_ids] reply
    | PP_commit_to_t_res : ppctt_res_payload -> [`PP_commit_to_t] reply
    | PP_KZG_eval_at_x_res : ppkeax_res_payload -> [`PP_kzg_eval_at_x] reply
    | PC_Distribution_res : pcd_res_payload -> [`PC_distribution] reply

  type t = bytes

  val request_step : 'step request -> 'step step

  val of_request : 'a request -> t

  val of_reply : 'a reply -> t

  val to_request : 'step step -> t -> 'step request option

  val to_reply : 'step step -> t -> 'step reply option

  val index : t -> int

  val string_of_message : t -> string
end

module Make : functor (MP : Distribution.Main_protocol.S) ->
  S with module MP = MP
