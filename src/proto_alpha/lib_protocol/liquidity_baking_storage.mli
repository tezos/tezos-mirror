(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <contact@tezos.com>            *)
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

(** Get the address of the Constant-Product Market Maker receiving the
    Liquidity Baking subsidy *)
val get_cpmm_address : Raw_context.t -> Contract_hash.t tzresult Lwt.t

(** [on_subsidy_allowed ctxt ~toggle_vote f] updates the toggle EMA according to
    [toggle_vote]. Then the callback function [f] is called if the following
    conditions are met:
    - the updated EMA is below the threshold,
    - the CPMM contract exists.

    The role of the callback function [f] is to send the subsidy to the CPMM,
    see [apply_liquidity_baking_subsidy] in [apply.ml]. *)
val on_subsidy_allowed :
  Raw_context.t ->
  toggle_vote:Toggle_votes_repr.toggle_vote ->
  (Raw_context.t -> Contract_hash.t -> (Raw_context.t * 'a list) tzresult Lwt.t) ->
  (Raw_context.t * 'a list * Toggle_EMA.t) tzresult Lwt.t
