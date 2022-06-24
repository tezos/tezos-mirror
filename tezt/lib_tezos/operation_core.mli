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
type kind = Consensus of {chain_id : string} | Voting | Manager

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

(** [inject_operations] is similar as [inject] for a list of
   operations. This function calls the RPC
   {!val:RPC.post_private_injection_operations} which is faster than
   calling the RPC used by {!val:inject} several times. Note that this
   function should be used mainly when the time for injecting
   operations matters. *)
val inject_operations :
  ?request:[`Inject | `Notify] ->
  ?force:bool ->
  ?error:rex ->
  t list ->
  Client.t ->
  [`OpHash of string] list Lwt.t

(** Craft a json representing the full operation, in a format that is
   compatible with the [run_operation] RPC
   ({!RPC.post_chain_block_helpers_scripts_run_operation}).

   This json contains many more fields than the one produced by the
   {!json} function above.

   The operation is signed with {!Tezos_crypto.Signature.zero},
   because the [run_operation] RPC skips signature checks anyway.

   @param sign_correctly If [true], use the correct signature of the
   operation instead of [Signature.zero]. Defaults to [false]. This
   parameter is temporary until the [run_operation] RPC is fixed to
   actually skip signature checks for non-manager operations (see
   https://gitlab.com/tezos/tezos/-/issues/3401)

   @param chain_id Allows to manually provide the [chain_id]. If
   omitted, the [chain_id] is retrieved via RPC using the provided
   [client].

   @param client The {!Client.t} argument is used to retrieve the
   [chain_id] when it is not provided. *)
val make_run_operation_input :
  ?chain_id:string ->
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3401

     Remove the [sign_correctly] argument once the [run_operation] RPC
     is fixed to actually skip signature checks for non-manager
     operations. *)
  ?sign_correctly:bool ->
  t ->
  Client.t ->
  JSON.u Lwt.t

module Consensus : sig
  (** A representation of a consensus operation. *)
  type t

  (** [slot_availability ~endorsement] crafts a data-availability
     consensus to endorse slot headers. For each slot, the value of
     the booleans indicates whether the data is available. *)
  val slot_availability : endorsement:bool array -> t

  (** [operation] constructs an operation from a consensus
     operation. the [client] is used to fetch the branch and the
     [chain_id]. *)
  val operation :
    ?branch:string ->
    ?chain_id:string ->
    signer:Account.key ->
    t ->
    Client.t ->
    operation Lwt.t

  (** A wrapper for {!val:inject} with consensus operations. The client
     is used to get all the data that was not provided if it can be
     recovered via RPCs. Mainly those are the [branch] and the
     [chain_id]. *)
  val inject :
    ?request:[`Inject | `Notify] ->
    ?force:bool ->
    ?branch:string ->
    ?chain_id:string ->
    ?error:rex ->
    signer:Account.key ->
    t ->
    Client.t ->
    [`OpHash of string] Lwt.t
end

(** Voting operations (validation pass [1]): [proposals] and [ballot].

   Only the [proposals] operation is currently supported. Feel free to
   add support for [ballot] as needed. *)
module Voting : sig
  (** A representation of a voting operation. *)
  type t

  (** [proposals source period protocol_hashes] crafts a [proposals]
     operation, that is, an operation that submits candidate protocol
     hashes for voting.

     @param source The account that submits the proposals.

     @param period An index that identifies the targeted voting
     period.

     @params protocol_hashes A list of candidate protocol hashes. *)
  val proposals : Account.key -> int -> string list -> t

  (** Contruct a voting operation from its representation.

     @param branch Allows to manually provide the branch. If omitted,
     the branch it retrieved via RPC using the given client.

     @param client Used to retrieve the branch when it is not
     provided.

     @param signer Allows to manually set the signer of the operation,
     e.g. to craft an operation with a wrong signature. If omitted,
     the signer is the operation's source.

     @raise Invalid_argument if neither the [branch] argument nor the
     [client] one is provided. *)
  val operation :
    ?branch:string ->
    ?client:Client.t ->
    ?signer:Account.key ->
    t ->
    operation Lwt.t

  (** A wrapper for {!inject}ing a voting operation.

     See {!inject} for a description of arguments [request], [force],
     [signature], and [error].

     See [Voting.operation] right above for a description of arguments
     [branch] and [signer]. *)
  val inject :
    ?request:[`Inject | `Notify] ->
    ?force:bool ->
    ?signature:Tezos_crypto.Signature.t ->
    ?error:rex ->
    ?branch:string ->
    ?signer:Account.key ->
    t ->
    Client.t ->
    [`OpHash of string] Lwt.t
end

module Manager : sig
  (** Payload of a manager operation. This excludes generic parameters
     common to all manager operations. See {!type:t}. *)
  type payload

  (** Build a public key revelation.

     The [Account.key] argument has no default value because it will
     typically be a fresh account. *)
  val reveal : Account.key -> payload

  (** [transfer ?(dest=Constant.bootstrap2) ~amount:1_000_000 ()]
     builds a transfer operation. Note that the amount is expressed in
     mutez. *)
  val transfer : ?dest:Account.key -> ?amount:int -> unit -> payload

  (** [dal_publish_slot_header ~level ~index ~header] builds an
     operation for the data-availability layer that publishes a
     slot. *)
  val dal_publish_slot_header : level:int -> index:int -> header:int -> payload

  (** [sc_rollup_dal_slot_subscribe ~rollup ~slot_index] builds an
     operation for the sc rollup [rollup] to subscribe to the data
     availability layer slot with index [slot_index]. *)
  val sc_rollup_dal_slot_subscribe : rollup:string -> slot_index:int -> payload

  (** [delegation ?(delegate=Constant.bootstrap2) ()] builds a
     delegation operation. *)
  val delegation : ?delegate:Account.key -> unit -> payload

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

  (** [make_batch] builds a batch of manager operations from a list of
     payloads and an initial counter. This function calls {!val:make}
     on all the payloads incrementing the initial [counter] for each
     operation except for the first one. The function does not fail if
     the list of [payload] is empty. *)
  val make_batch :
    ?source:Account.key ->
    ?fee:int ->
    ?gas_limit:int ->
    ?storage_limit:int ->
    counter:int ->
    payload list ->
    t list

  (** [get_next_counter ~source client] returns the next valid counter
     value for [source] expected by the protocol for a manager
     operation where the source is [source]. If the [source] is not
     provided, the same one as {!val:make} is used. *)
  val get_next_counter : ?source:Account.key -> Client.t -> int Lwt.t

  (** [json t] gives the json representation of a manager operation. *)
  val json : Client.t -> t -> JSON.u Lwt.t

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
