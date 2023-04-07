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

module Make =
functor
  (MP : Distribution.Main_protocol.S)
  ->
  struct
    module MP = MP

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
    [@@deriving repr]

    type ctpap_payload = {index : int; content : MP.PP.transcript}
    [@@deriving repr]

    type pppi_payload = {index : int; content : MP.PP.transcript}
    [@@deriving repr]

    type ppctt_payload = {
      index : int;
      content : string list * Plonk.Bls.Scalar.t;
    }
    [@@deriving repr]

    type ppkeax_payload = {index : int; content : MP.PP.transcript}
    [@@deriving repr]

    type pcd_payload = {index : int; content : MP.PP.PC.worker_msg}
    [@@deriving repr]

    type 'step request =
      | Commit_to_wires : ctw_payload -> [`Commit_to_wires] request
      | Commit_to_plook : ctpap_payload -> [`Commit_to_plook] request
      | PP_prepare_ids : pppi_payload -> [`PP_prepare_ids] request
      | PP_commit_to_t : ppctt_payload -> [`PP_commit_to_t] request
      | PP_KZG_eval_at_x : ppkeax_payload -> [`PP_kzg_eval_at_x] request
      | PC_Distribution : pcd_payload -> [`PC_distribution] request

    type ctw_res_payload = {index : int; content : MP.commit_to_wires_reply}
    [@@deriving repr]

    type ctpap_res_payload = {
      index : int;
      content : MP.commit_to_plook_rc_reply;
    }
    [@@deriving repr]

    type pppi_res_payload = {index : int; content : string list}
    [@@deriving repr]

    type ppctt_res_payload = {index : int; content : Plonk.Bls.Evaluations.t}
    [@@deriving repr]

    type ppkeax_res_payload = {index : int; content : MP.PP.PC.answer list}
    [@@deriving repr]

    type pcd_res_payload = {index : int; content : MP.PP.PC.main_prover_msg}
    [@@deriving repr]

    type 'step reply =
      | Commit_to_wires_res : ctw_res_payload -> [`Commit_to_wires] reply
      | Commit_to_plook_res : ctpap_res_payload -> [`Commit_to_plook] reply
      | PP_prepare_ids_res : pppi_res_payload -> [`PP_prepare_ids] reply
      | PP_commit_to_t_res : ppctt_res_payload -> [`PP_commit_to_t] reply
      | PP_KZG_eval_at_x_res : ppkeax_res_payload -> [`PP_kzg_eval_at_x] reply
      | PC_Distribution_res : pcd_res_payload -> [`PC_distribution] reply

    (* Non-GADT versions of [request] and [reply], used to derive a [Repr.t] *)
    type f_request =
      | FCommit_to_wires : ctw_payload -> f_request
      | FCommit_to_plook : ctpap_payload -> f_request
      | FPP_prepare_ids : pppi_payload -> f_request
      | FPP_commit_to_t : ppctt_payload -> f_request
      | FPP_KZG_eval_at_x : ppkeax_payload -> f_request
      | FPC_Distribution : pcd_payload -> f_request
    [@@deriving repr]

    let forget_request : type a. a request -> f_request = function
      | Commit_to_wires p -> FCommit_to_wires p
      | Commit_to_plook p -> FCommit_to_plook p
      | PP_prepare_ids p -> FPP_prepare_ids p
      | PP_commit_to_t p -> FPP_commit_to_t p
      | PP_KZG_eval_at_x p -> FPP_KZG_eval_at_x p
      | PC_Distribution p -> FPC_Distribution p

    type f_reply =
      | FCommit_to_wires_res : ctw_res_payload -> f_reply
      | FCommit_to_plook_res : ctpap_res_payload -> f_reply
      | FPP_prepare_ids_res : pppi_res_payload -> f_reply
      | FPP_commit_to_t_res : ppctt_res_payload -> f_reply
      | FPP_KZG_eval_at_x_res : ppkeax_res_payload -> f_reply
      | FPC_Distribution_res : pcd_res_payload -> f_reply
    [@@deriving repr]

    let forget_reply : type a. a reply -> f_reply = function
      | Commit_to_wires_res p -> FCommit_to_wires_res p
      | Commit_to_plook_res p -> FCommit_to_plook_res p
      | PP_prepare_ids_res p -> FPP_prepare_ids_res p
      | PP_commit_to_t_res p -> FPP_commit_to_t_res p
      | PP_KZG_eval_at_x_res p -> FPP_KZG_eval_at_x_res p
      | PC_Distribution_res p -> FPC_Distribution_res p

    type msg = Request : f_request -> msg | Reply : f_reply -> msg
    [@@deriving repr]

    type t = bytes

    let of_bytes_exn r b =
      Result.get_ok
      @@ Repr.(unstage @@ Repr.of_bin_string r) (Bytes.to_string b)

    let to_bytes r v = Bytes.of_string Repr.((unstage @@ to_bin_string r) v)

    let request_step : type s. s request -> s step = function
      | Commit_to_wires _p -> S_ctw
      | Commit_to_plook _p -> S_ctp
      | PP_prepare_ids _p -> S_pppi
      | PP_commit_to_t _p -> S_ppctt
      | PP_KZG_eval_at_x _p -> S_ppkeax
      | PC_Distribution _p -> S_pcd

    let of_request : 'a request -> t =
     fun r -> to_bytes msg_t @@ Request (forget_request r)

    let of_reply : 'a reply -> t =
     fun r -> to_bytes msg_t @@ Reply (forget_reply r)

    let to_reply : type s. s step -> t -> s reply option =
     fun step t ->
      let m = of_bytes_exn msg_t t in
      match (step, m) with
      | S_ctw, Reply (FCommit_to_wires_res p) -> Some (Commit_to_wires_res p)
      | S_ctp, Reply (FCommit_to_plook_res p) -> Some (Commit_to_plook_res p)
      | S_pppi, Reply (FPP_prepare_ids_res p) -> Some (PP_prepare_ids_res p)
      | S_ppctt, Reply (FPP_commit_to_t_res p) -> Some (PP_commit_to_t_res p)
      | S_ppkeax, Reply (FPP_KZG_eval_at_x_res p) ->
          Some (PP_KZG_eval_at_x_res p)
      | S_pcd, Reply (FPC_Distribution_res p) -> Some (PC_Distribution_res p)
      | _ -> None

    let to_request : type s. s step -> t -> s request option =
     fun step t ->
      let m = of_bytes_exn msg_t t in
      match (step, m) with
      | S_ctw, Request (FCommit_to_wires p) -> Some (Commit_to_wires p)
      | S_ctp, Request (FCommit_to_plook p) -> Some (Commit_to_plook p)
      | S_pppi, Request (FPP_prepare_ids p) -> Some (PP_prepare_ids p)
      | S_ppctt, Request (FPP_commit_to_t p) -> Some (PP_commit_to_t p)
      | S_ppkeax, Request (FPP_KZG_eval_at_x p) -> Some (PP_KZG_eval_at_x p)
      | S_pcd, Request (FPC_Distribution p) -> Some (PC_Distribution p)
      | _ -> None

    let index : t -> int =
     fun t ->
      let m = of_bytes_exn msg_t t in
      match m with
      | Request (FCommit_to_wires {index; _})
      | Reply (FCommit_to_wires_res {index; _})
      | Request (FCommit_to_plook {index; _})
      | Reply (FCommit_to_plook_res {index; _})
      | Request (FPP_prepare_ids {index; _})
      | Reply (FPP_prepare_ids_res {index; _})
      | Request (FPP_commit_to_t {index; _})
      | Reply (FPP_commit_to_t_res {index; _})
      | Request (FPP_KZG_eval_at_x {index; _})
      | Reply (FPP_KZG_eval_at_x_res {index; _})
      | Request (FPC_Distribution {index; _})
      | Reply (FPC_Distribution_res {index; _}) ->
          index

    let string_of_bytes bytes =
      let bytes = float_of_int bytes in
      if bytes <= 1024. then Printf.sprintf "%3.2f B " bytes
      else
        let kilobytes = bytes /. 1024. in
        if kilobytes <= 1024. then Printf.sprintf "%3.2f KB" kilobytes
        else
          let megabytes = kilobytes /. 1024. in
          if megabytes <= 1024. then Printf.sprintf "%3.2f MB" megabytes
          else
            let gigabytes = megabytes /. 1024. in
            Printf.sprintf "%.2f GB" gigabytes

    let string_of_message : t -> string =
     fun t ->
      let m = of_bytes_exn msg_t t in
      match m with
      | Request (FCommit_to_wires {index; content = _}) ->
          Format.sprintf
            "Commit_to_wires: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Reply (FCommit_to_wires_res {index; content = _}) ->
          Format.sprintf
            "Commit_to_wires_res: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Request (FCommit_to_plook {index; content = _}) ->
          Format.sprintf
            "Commit_to_plook: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Reply (FCommit_to_plook_res {index; content = _}) ->
          Format.sprintf
            "Commit_to_plook_res: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Request (FPP_prepare_ids {index; content = _}) ->
          Format.sprintf
            "PP_prepare_ids: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Reply (FPP_prepare_ids_res {index; content = _}) ->
          Format.sprintf
            "PP_prepare_ids_res: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Request (FPP_commit_to_t {index; content = _}) ->
          Format.sprintf
            "PP_commit_to_t: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Reply (FPP_commit_to_t_res {index; content = _}) ->
          Format.sprintf
            "PP_commit_to_t_res: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Request (FPP_KZG_eval_at_x {index; content = _}) ->
          Format.sprintf
            "PP_KZG_eval_at_x: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Reply (FPP_KZG_eval_at_x_res {index; content = _}) ->
          Format.sprintf
            "PP_KZG_eval_at_x_res: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Request (FPC_Distribution {index; content = _}) ->
          Format.sprintf
            "PC_Distribution: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
      | Reply (FPC_Distribution_res {index; content = _}) ->
          Format.sprintf
            "PC_Distribution_res: %d (%s)"
            index
            (string_of_bytes @@ Bytes.length t)
  end
