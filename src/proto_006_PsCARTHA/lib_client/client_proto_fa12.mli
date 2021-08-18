(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** [Client_proto_fa12] implements built-in support for the
   {{:https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-7/tzip-7.md}
   FA1.2} standard. This module features functions to check whether a
   contract implements the standard interface and to interact with
   such contracts using high-level [actions] that model the entrypoint
   calls.

    This module also provides functions to unwrap [Micheline] values
   into [actions], which can be useful for indexers or applications
   using this module to interpret transactions on FA1.2 contracts as
   FA1.2 operations. *)

open Protocol
open Alpha_context
open Protocol_client_context

(** A callback contract is represented by an address and a possible
   entrypoint on which the transaction is done. *)
type callback_contract = Contract.t * string option

type action =
  | Transfer of Contract.t * Contract.t * Z.t
  | Approve of Contract.t * Z.t
  | Get_allowance of Contract.t * Contract.t * callback_contract
  | Get_balance of Contract.t * callback_contract
  | Get_total_supply of callback_contract

val print_action : Format.formatter -> action -> unit

val action_encoding : action Data_encoding.encoding

val action_to_expr : ?loc:Script.location -> action -> Script.node

val action_of_expr : entrypoint:string -> Script.node -> action tzresult

(** [convert_wrapped_parameter_into_action ccctx ~chain ~block
   ~contract parameter] converts a wrapped FA1.2 contract [parameter]
   into the corresponding FA1.2 [action].

    That is, it takes a contract parameter on the form [C_1 .. (C_n
   ... <entrypoint argument> ))] where [C_1 ... C_n] is a sequence of
   [Left]/[Right] constructors. It finds the entrypoint corresponding
   to that path in [contract]'s interface. The result of the function
   is the [<entrypoint arguments>] applied to the [action]
   corresponding to that entrypoint. *)
val convert_wrapped_parameter_into_action :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Script.node ->
  action tzresult Lwt.t

(** Check whether a contract has an FA1.2 interface. *)
val contract_has_fa12_interface :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  contract:Alpha_context.Contract.t ->
  unit ->
  unit tzresult Lwt.t
