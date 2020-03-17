(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type Mockup_sig = sig
  type parameters

  val parameters_encoding : parameters Data_encoding.t

  val default_parameters : parameters

  val protocol_hash : Protocol_hash.t

  module Protocol : sig
    val hash : Protocol_hash.t

    include Tezos_protocol_environment.PROTOCOL
  end

  module Block_services :
      module type of
        Tezos_shell_services.Block_services.Make (Protocol) (Protocol)

  val directory : Tezos_protocol_environment.rpc_context RPC_directory.t

  val init :
    parameters -> Tezos_protocol_environment.rpc_context tzresult Lwt.t
end

type mockup_environment = (module Mockup_sig)

let registered : mockup_environment list ref = ref []

let register_mockup_context m = registered := m :: !registered

let get_registered_contexts () = !registered
