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

val get_cpmm_address : Raw_context.t -> Contract_repr.t tzresult Lwt.t

module Escape_EMA : sig
  type t

  val of_int32 : Int32.t -> t tzresult Lwt.t

  val zero : t

  val to_int32 : t -> Int32.t

  val encoding : t Data_encoding.t
end

(** [compute_new_ema ~escape_vote old_ema] returns the value of the exponential
    moving average [old_ema] updated by the vote [escape_vote].

    This function is only exported to ease unit testing.
    *)
val compute_new_ema :
  escape_vote:Block_header_repr.liquidity_baking_escape_vote ->
  Escape_EMA.t ->
  Escape_EMA.t

(** Checks if below EMA threshold (after updating), sunset level, and if CPMM
    contract exists. *)
val on_subsidy_allowed :
  Raw_context.t ->
  escape_vote:Block_header_repr.liquidity_baking_escape_vote ->
  (Raw_context.t -> Contract_repr.t -> (Raw_context.t * 'a list) tzresult Lwt.t) ->
  (Raw_context.t * 'a list * Escape_EMA.t) tzresult Lwt.t
