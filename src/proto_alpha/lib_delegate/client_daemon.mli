(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Daemons directly supported by lib_delegate *)

(** {1 Baker daemon} *)
module Baker : sig
  val run :
    Protocol_client_context.full ->
    ?minimal_fees:Protocol.Alpha_context.Tez.t ->
    ?minimal_nanotez_per_gas_unit:Q.t ->
    ?minimal_nanotez_per_byte:Q.t ->
    ?votes:Baking_configuration.toggle_votes_config ->
    ?extra_operations:Baking_configuration.Operations_source.t ->
    ?dal_node_endpoint:Uri.t ->
    ?force_apply:bool ->
    ?context_path:string ->
    chain:Shell_services.chain ->
    keep_alive:bool ->
    Baking_state.consensus_key list ->
    unit tzresult Lwt.t
end

(** {1 Accuser daemon} *)

module Accuser : sig
  val run :
    #Protocol_client_context.full ->
    chain:Chain_services.chain ->
    preserved_levels:int ->
    keep_alive:bool ->
    unit tzresult Lwt.t
end

(** {1 VDF computation daemon} *)

module VDF : sig
  val run :
    Protocol_client_context.full ->
    chain:Chain_services.chain ->
    keep_alive:bool ->
    unit tzresult Lwt.t
end
