(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

module Tx_rollup : sig
  type state = {
    oldest_inbox_level : int option;
    head_level : (int * int) option;
    commitment_head_level : (int * string) option;
    burn_per_byte : int;
    inbox_ema : int;
  }

  type inbox = {cumulated_size : int; contents : string list; hash : string}

  val get_state :
    ?hooks:Process.hooks -> rollup:string -> Client.t -> state Process.runnable

  val get_inbox :
    ?hooks:Process.hooks ->
    rollup:string ->
    level:int ->
    Client.t ->
    inbox Process.runnable

  val get_commitment :
    ?hooks:Process.hooks ->
    ?block:string ->
    rollup:string ->
    level:int ->
    Client.t ->
    JSON.t Process.runnable

  val get_pending_bonded_commitments :
    ?hooks:Process.hooks ->
    ?block:string ->
    rollup:string ->
    pkh:string ->
    Client.t ->
    JSON.t Process.runnable

  module Check : sig
    val state : state Check.typ

    val inbox : inbox Check.typ
  end

  module Parameters : sig
    type t = {finality_period : int; withdraw_period : int}

    val default : t

    val parameter_file : ?parameters:t -> Protocol.t -> string Lwt.t
  end
end
