(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

(** Type of a protocol-specific mempool filter plug-in. *)
module type FILTER = sig
  module Proto : Registered_protocol.T

  module Mempool : sig
    type config

    val config_encoding : config Data_encoding.t

    val default_config : config

    val pre_filter :
      config ->
      ?validation_state_before:Proto.validation_state ->
      Proto.operation_data ->
      [ `Undecided
      | `Branch_delayed of tztrace
      | `Branch_refused of Error_monad.tztrace
      | `Refused of Error_monad.tztrace
      | `Outdated of tztrace ]

    val post_filter :
      config ->
      validation_state_before:Proto.validation_state ->
      validation_state_after:Proto.validation_state ->
      Proto.operation_data * Proto.operation_receipt ->
      bool Lwt.t
  end

  module RPC : sig
    val rpc_services : Environment_context.rpc_context RPC_directory.directory
  end
end

(** Dummy filter that does nothing *)
module No_filter (Proto : Registered_protocol.T) :
  FILTER with module Proto = Proto

(** Registers a mempool plug-in for a specific protocol (according to its [Proto.hash]). *)
val register : (module FILTER) -> unit

(** Looks for a mempool plug-in for a specific protocol. *)
val find : Protocol_hash.t -> (module FILTER) option
