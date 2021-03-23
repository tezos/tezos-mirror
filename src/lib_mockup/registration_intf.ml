(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module T = struct
  (** Type of a mockup environment *)
  type mockup_context = Chain_id.t * Tezos_protocol_environment.rpc_context
end

module type PROTOCOL = sig
  val hash : Protocol_hash.t

  include Tezos_protocol_environment.PROTOCOL
end

(** The module type of a mockup environment. Modules of this type should be
    prepared protocol-side and registered in {!Registration} to become available to the
    mockup facility. *)
module type MOCKUP = sig
  type parameters

  type protocol_constants

  val parameters_encoding : parameters Data_encoding.t

  val default_parameters : parameters

  val protocol_constants_encoding : protocol_constants Data_encoding.t

  (** The content equivalent to the default value of {[--protocol-constants]} *)
  val default_protocol_constants :
    Tezos_client_base.Client_context.full -> protocol_constants tzresult Lwt.t

  (** The content equivalent to the default value of {[--bootstrap-acounts]} *)
  val default_bootstrap_accounts :
    Tezos_client_base.Client_context.full -> string tzresult Lwt.t

  val protocol_hash : Protocol_hash.t

  module Protocol : PROTOCOL

  module Block_services :
      module type of
        Tezos_shell_services.Block_services.Make (Protocol) (Protocol)

  val directory : Tezos_protocol_environment.rpc_context RPC_directory.t

  val init :
    cctxt:Tezos_client_base.Client_context.full ->
    parameters:parameters ->
    constants_overrides_json:Data_encoding.json option ->
    bootstrap_accounts_json:Data_encoding.json option ->
    T.mockup_context tzresult Lwt.t

  val migrate : T.mockup_context -> T.mockup_context tzresult Lwt.t
end

module type S = sig
  include module type of T

  type mockup_environment = (module MOCKUP)

  val register_mockup_environment : mockup_environment -> unit

  val get_registered_environments : unit -> mockup_environment list
end
