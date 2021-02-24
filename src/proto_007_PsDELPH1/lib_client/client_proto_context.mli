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

open Protocol
open Alpha_context

val list_contract_labels :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  (string * string * string) list tzresult Lwt.t

val get_storage :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Script.expr option tzresult Lwt.t

val get_contract_big_map_value :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Script.expr * Script.expr ->
  Script.expr option tzresult Lwt.t

val get_big_map_value :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Z.t ->
  Script_expr_hash.t ->
  Script.expr tzresult Lwt.t

val get_script :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Script.t option tzresult Lwt.t

val get_balance :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Tez.t tzresult Lwt.t

val parse_arg_transfer : string option -> Script.lazy_expr tzresult Lwt.t

type period_info = {
  current_period_kind : Voting_period.kind;
  position : Int32.t;
  remaining : Int32.t;
  current_proposal : Protocol_hash.t option;
}

type ballots_info = {
  current_quorum : Int32.t;
  participation : Int32.t;
  supermajority : Int32.t;
  ballots : Vote.ballots;
}

val get_period_info :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  period_info tzresult Lwt.t

val get_ballots_info :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ballots_info tzresult Lwt.t

val get_proposals :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Int32.t Environment.Protocol_hash.Map.t tzresult Lwt.t

(** lookup an operation in [predecessors] previous blocks, and print the
    receipt if found *)
val display_receipt_for_operation :
  #Protocol_client_context.full ->
  chain:Block_services.chain ->
  ?predecessors:int ->
  Operation_list_hash.elt ->
  unit tzresult Lwt.t
