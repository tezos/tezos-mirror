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

open Protocol
open Alpha_context

(** The information that the user has to provide for every smart contract
   they want to call during the stress test. *)
type contract_parameters = {
  probability : float;  (** The probability of calling this smart contract *)
  invocation_fee : Tez.t;
      (** Fee to use for invocations during the stress test *)
  invocation_gas_limit : Gas.Arith.integral;
      (** Gas limit to use for invocations during the stress test *)
}

(** Encoding of [contract_parameters]. *)
val contract_parameters_encoding : contract_parameters Data_encoding.t

(** Contract parameters collection encoding. *)
val contract_parameters_collection_encoding :
  (string * contract_parameters) list Data_encoding.t

(** An opaque type that stores all the information that is necessary for
    efficient sampling of smart contract calls. *)
type t

(** A value with no smart contracts. *)
val no_contracts : t

(** Convert a map of probabilities per smart contract alias to a [t] value.
   *)
val init :
  Protocol_client_context.full ->
  (string * contract_parameters) list ->
  t tzresult Lwt.t

(** All information that is necessary for performing a smart contract call
    during the stress test. *)
type invocation_parameters = {
  destination : Contract.t;
  entrypoint : Entrypoint_repr.t;
  arg : Script.expr;
  fee : Tez.t;
  gas_limit : Gas.Arith.integral;
}

(** Given [t] initialized earlier and a [float] in [0;1], decide
   which smart contract is going to be chosen for the next call, if any. *)
val select : t -> float -> invocation_parameters option

(** A ready-to-use command that originates all supported smart contracts. *)
val originate_command : Protocol_client_context.full Tezos_clic.command

(** Call the callback function once per supported smart contract by passing
    it [t] that only selects that smart contract. Collect the
    results and pair them with the respective smart contract aliases. This
    is used for e.g. gas estimations. *)
val with_every_known_smart_contract :
  Protocol_client_context.full ->
  (t -> 'a tzresult Lwt.t) ->
  (string * 'a) list tzresult Lwt.t

(** Return the alias that corresponds to the smart contract address on the
   Mainnet. Return [None] if the given address is not supported. *)
val mainnet_address_to_alias : string -> string option
