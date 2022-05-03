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

module type T = sig
  module P : sig
    val hash : Protocol_hash.t

    include Tezos_protocol_environment.PROTOCOL
  end

  include module type of struct
    include P
  end

  val complete_b58prefix :
    Tezos_protocol_environment.Context.t -> string -> string list Lwt.t
end

type t = (module T)

val mem : Protocol_hash.t -> bool

val seq : unit -> t Seq.t

val get : Protocol_hash.t -> t option

val get_result : Protocol_hash.t -> t tzresult Lwt.t

val seq_embedded : unit -> Protocol_hash.t Seq.t

val get_embedded_sources : Protocol_hash.t -> Protocol.t option

module Register_embedded_V0
    (Env : Tezos_protocol_environment.V0)
    (Proto : Env.Updater.PROTOCOL) (Source : sig
      val hash : Protocol_hash.t option

      val sources : Protocol.t
    end) :
  T
    with type P.block_header_data = Proto.block_header_data
     and type P.operation_data = Proto.operation_data
     and type P.operation_receipt = Proto.operation_receipt
     and type P.validation_state = Proto.validation_state

module Register_embedded_V1
    (Env : Tezos_protocol_environment.V1)
    (Proto : Env.Updater.PROTOCOL) (Source : sig
      val hash : Protocol_hash.t option

      val sources : Protocol.t
    end) :
  T
    with type P.block_header_data = Proto.block_header_data
     and type P.operation_data = Proto.operation_data
     and type P.operation_receipt = Proto.operation_receipt
     and type P.validation_state = Proto.validation_state

module Register_embedded_V2
    (Env : Tezos_protocol_environment.V2)
    (Proto : Env.Updater.PROTOCOL) (Source : sig
      val hash : Protocol_hash.t option

      val sources : Protocol.t
    end) :
  T
    with type P.block_header_data = Proto.block_header_data
     and type P.operation_data = Proto.operation_data
     and type P.operation = Proto.operation
     and type P.operation_receipt = Proto.operation_receipt
     and type P.validation_state = Proto.validation_state

module Register_embedded_V3
    (Env : Tezos_protocol_environment.V3)
    (Proto : Env.Updater.PROTOCOL) (Source : sig
      val hash : Protocol_hash.t option

      val sources : Protocol.t
    end) :
  T
    with type P.block_header_data = Proto.block_header_data
     and type P.operation_data = Proto.operation_data
     and type P.operation = Proto.operation
     and type P.operation_receipt = Proto.operation_receipt
     and type P.validation_state = Proto.validation_state

module Register_embedded_V4
    (Env : Tezos_protocol_environment.V4)
    (Proto : Env.Updater.PROTOCOL) (Source : sig
      val hash : Protocol_hash.t option

      val sources : Protocol.t
    end) :
  T
    with type P.block_header_data = Proto.block_header_data
     and type P.operation_data = Proto.operation_data
     and type P.operation = Proto.operation
     and type P.operation_receipt = Proto.operation_receipt
     and type P.validation_state = Proto.validation_state

module Register_embedded_V5
    (Env : Tezos_protocol_environment.V5)
    (Proto : Env.Updater.PROTOCOL) (Source : sig
      val hash : Protocol_hash.t option

      val sources : Protocol.t
    end) :
  T
    with type P.block_header_data = Proto.block_header_data
     and type P.operation_data = Proto.operation_data
     and type P.operation = Proto.operation
     and type P.operation_receipt = Proto.operation_receipt
     and type P.validation_state = Proto.validation_state

module Register_embedded_V6
    (Env : Tezos_protocol_environment.V6)
    (Proto : Env.Updater.PROTOCOL) (Source : sig
      val hash : Protocol_hash.t option

      val sources : Protocol.t
    end) :
  T
    with type P.block_header_data = Proto.block_header_data
     and type P.operation_data = Proto.operation_data
     and type P.operation = Proto.operation
     and type P.operation_receipt = Proto.operation_receipt
     and type P.validation_state = Proto.validation_state
