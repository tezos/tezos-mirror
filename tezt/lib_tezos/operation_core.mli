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

(** This module can be used to craft an operation without using client
   dedicated commands.

    {1 Overview}

    This module aims to replace the module {!module:Operation_legacy}
   to provide an interface which is more extensible. In other words,
   supporting a new operation should be easier using this interface.

    An unsigned operation is represented by the datatype
   {!type:t}. {!type:t} is a wrapper around a JSON representation of
   an operation. Some meta information needs to be provided to sign
   this operation. {!type:t} is not signed a priori to ease writing
   tests with bad signatures.

    This module also provides two functions to ease the injection of
   an operation: {!val:inject} which should be called when the
   injection is expected to succeed, and {!val:inject_with_error} when
   the injection is expected to fail.

    Anyone is free to add support for new operations.

        
    {2 Manager operations}

    Manager operations represent most of the operations used by the
   tests. Those operations contain several parameters (see
   {!val:Manager.make}) and can be batched. Wrapper like
   {!val:Manager.inject} and {!val:Manager.inject_with_error} are
   provided to ease the writing of tests.

*)

(** The abstract representation of an unsigned operation. *)
type t

type operation := t

(** The kind is necessary because it determines the watermark of an
   operation which is necessary for signing an operation. This type
   aims to be extended when other kinds of operations are added into
   this module. *)
type kind = Manager

(** [make ~branch ~signer ~kind json client] builds the representation
   of an unsigned operation. *)
val make : branch:string -> signer:Account.key -> kind:kind -> JSON.u -> t

(** [json t] gives the json representation of an unsigned operation. *)
val json : t -> JSON.u

(** [hex ?(signature=None) t client] computes the binary
   representation of an operation as an hexadecimal string. If
   [signature] is given, the hexadecimal represents the signed
   version of the operation. [client] is used to construct the binary
   representation of [t].

   @param signature controls whether a signature should be attached
   to the operation. *)
val hex : ?signature:Tezos_crypto.Signature.t -> t -> Client.t -> Hex.t Lwt.t

(** [sign t client] signs the raw representation of operation [t] by
   its signer (see {!val:make}). [client] is used to construct the
   binary representation of [t]. *)
val sign : t -> Client.t -> Tezos_crypto.Signature.t Lwt.t

(** [inject ?(request=`Inject) ?(force=false) ?(signature=None)
   ?(error=None) t] injects an operation into the node. The node is
   extracted from the [Client]. If a node cannot be extracted, the
   injection fails. If the injection succeeds, the hash of the
   operation is returned.

   @param request If [`Inject], we do not wait the [prevalidator] to
   classify the operation. This can create some flakyness in the test
   but is needed to test corner cases. If [`Notify], the function
   waits for the prevalidator to classify the operation. However, the
   nodes need to activate the debug events for the prevalidator.

   @param force If [true], the function succeeds even though the
   operation was classified with an error and was not propagated by
   the prevalidator. If [false], the call fails if the prevalidator
   classified the operation with an error.

   @param signature Allows to give manually the signature of the
   operation. The operation is signed when the signature is omitted.

   @param error If the injection is expecting to fail, allows to
   specify the expected error.  *)
val inject :
  ?request:[`Inject | `Notify] ->
  ?force:bool ->
  ?signature:Tezos_crypto.Signature.t ->
  ?error:rex ->
  t ->
  Client.t ->
  [`OpHash of string] Lwt.t

module Manager : sig
  (** Payload of a manager operation. This excludes generic parameters
     common to all manager operations. See {!type:t}. *)
  type payload

  (** [transfer ?(dest=Constant.bootstrap2) ~amount:1_000_000 ()]
     builds a transfer operation. Note that the amount is expressed in
     mutez. *)
  val transfer : ?dest:Account.key -> ?amount:int -> unit -> payload

  (** [dal_publish_slot_header ~level ~index ~header] builds an
     operation for the data-availability layer that publishes a
     slot. *)
  val dal_publish_slot_header : level:int -> index:int -> header:int -> payload

  (** A representation of a manager operation. This includes generic
     parameters common to all manager operations. See {!val:make}. *)
  type t

  (** [make] builds a manager operation from a payload and generic
     parameters common to all the manager operations. The default
     values of the generic parameters are set depending on the
     payload. Those default values ensure that the operation can be
     executed correctly. Have a look at the definition of this
     function to know the default value for each operation payload. *)
  val make :
    ?source:Account.key ->
    ?counter:int ->
    ?fee:int ->
    ?gas_limit:int ->
    ?storage_limit:int ->
    payload ->
    t

  (** [operation ?branch t client] constructs an operation from a
     manager operation. [branch] can be used to set manually the
     branch. [client] can be used to get some meta information such as
     the [counter] for the operation. *)
  val operation :
    ?branch:string ->
    ?signer:Account.key ->
    t list ->
    Client.t ->
    operation Lwt.t

  (** A wrapper for {!val:inject} with manager operations. The client
     is used to get all the data that was not provided if it can be
     recovered via RPCs. Mainly those are the [branch] and the
     [counter]. *)
  val inject :
    ?request:[`Inject | `Notify] ->
    ?force:bool ->
    ?branch:string ->
    ?signer:Account.key ->
    ?error:rex ->
    t list ->
    Client.t ->
    [`OpHash of string] Lwt.t
end
