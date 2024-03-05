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

type consensus_kind = Attestation of {with_dal : bool} | Preattestation

(** The kind is necessary because it determines the watermark of an
   operation which is necessary for signing an operation. This type
   aims to be extended when other kinds of operations are added into
   this module. *)
type kind =
  | Consensus of {kind : consensus_kind; chain_id : string}
  | Anonymous
  | Voting
  | Manager

(** [make ~branch ?signer ~kind json client] builds the representation
   of an unsigned operation. *)
val make : branch:string -> ?signer:Account.key -> kind:kind -> JSON.u -> t

(** [json t] gives the json representation of an unsigned operation. *)
val json : t -> JSON.u

(** [hex ?(protocol=None) ?(signature=None) t client] computes the binary
    representation of an operation as a hexadecimal string. If [protocol] is
    given, the binary representation is computed using the encoding of operation
    from the [protocol]. Otherwise, a call to the [forge_operations] RPC is done
    to compute the binary representation. If [signature] is given, the
    hexadecimal represents the signed version of the operation. [client] is used
    to construct the binary representation of [t].

    @param protocol controls whether the encoding of a protocol should be used
    to compute the binary representation of the operation rather than calling
    the [forge_operations] RPC to compute it.

    @param signature controls whether a signature should be attached
    to the operation. *)
val hex :
  ?protocol:Protocol.t ->
  ?signature:Tezos_crypto.Signature.t ->
  t ->
  Client.t ->
  Hex.t Lwt.t

(** [sign t client] signs the raw representation of operation [t] by its signer
    (see {!val:make}). [client] is used to construct the binary representation
    of [t]. Note that if no signer have been given to [t] the function returns
    [Tezos_crypto.Signature.zero]. *)
val sign :
  ?protocol:Protocol.t -> t -> Client.t -> Tezos_crypto.Signature.t Lwt.t

(** [hash t client] returns the hash of the operation  *)
val hash : t -> Client.t -> [`OpHash of string] Lwt.t

(** Returns the size (in bytes) of the operation.

    @param protocol Allows using the operation encoding rather than
    using the [forge_operations] RPC to compute the hexadecimal
    representation of the operation.

    @param signature Allows to manually set the signature of the
    operation. When omitted, the operation is correctly signed using {!sign}. *)
val byte_size :
  ?protocol:Protocol.t ->
  ?signature:Tezos_crypto.Signature.t ->
  t ->
  Client.t ->
  int Lwt.t

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

   @param protocol Allow using the operation encoding rather than using the
   [forge_operations] RPC to compute the hexadecimal representation of the
   operations.

   @param signature Allows to give manually the signature of the
   operation. The operation is signed when the signature is omitted.

   @param error If the injection is expecting to fail, allows to
   specify the expected error.  *)
val inject :
  ?request:[`Inject | `Notify] ->
  ?force:bool ->
  ?protocol:Protocol.t ->
  ?signature:Tezos_crypto.Signature.t ->
  ?error:rex ->
  t ->
  Client.t ->
  [`OpHash of string] Lwt.t

(** Same as [inject], but do not wait for the process to exit. *)
val spawn_inject :
  ?force:bool ->
  ?protocol:Protocol.t ->
  ?signature:Tezos_crypto.Signature.t ->
  t ->
  Client.t ->
  JSON.t Runnable.process Lwt.t

(** Run [spawn_inject] then capture two groups on stderr with [rex]. *)
val inject_and_capture2_stderr :
  rex:rex ->
  ?force:bool ->
  ?protocol:Protocol.t ->
  ?signature:Tezos_crypto.Signature.t ->
  t ->
  Client.t ->
  (string * string) Lwt.t

(** [inject_operations ?protocol ?request ?force ?error ?use_tmp_file ops
    client] is similar as [inject] for a list of operations. This function calls
    the RPC {!val:RPC.post_private_injection_operations} which is faster than
    calling the RPC used by {!val:inject} several times. Note that this function
    should be used mainly when the time for injecting operations matters.

    @param use_tmp_file see {!val:RPC.post_private_injection_operations} for
    more information. *)
val inject_operations :
  ?protocol:Protocol.t ->
  ?request:[`Inject | `Notify] ->
  ?force:bool ->
  ?error:rex ->
  ?use_tmp_file:bool ->
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

   @param chain_id Allows to manually provide the [chain_id]. If
   omitted, the [chain_id] is retrieved via RPC using the provided
   [client].

   @param client The {!Client.t} argument is used to retrieve the
   [chain_id] when it is not provided. *)
val make_run_operation_input : ?chain_id:string -> t -> Client.t -> JSON.u Lwt.t

(** Craft a json representing the full operation, in a format that is
   compatible with the [preapply/operations] RPC
   ({!RPC.post_chain_block_helpers_preapply_operations}).

   This json contains many more fields than the one produced by the
   {!json} function above. *)
val make_preapply_operation_input :
  protocol:Protocol.t -> signature:Tezos_crypto.Signature.t -> t -> JSON.u

module Consensus : sig
  (** A representation of a consensus operation. *)
  type t

  (** [consensus ~kind ~use_legacy_name ~level ~round ~slot ~block_payload_hash]
      crafts a consensus operation with the [kind] at [level] on the [round]
      with the [slot] and [block_payload_hash]. If [use_legacy_name] is set, the
      [kind] field in the crafted JSON will be "(pre)endorsement" instead of
      "(pre)attestation". *)
  val consensus :
    use_legacy_name:bool ->
    kind:consensus_kind ->
    slot:int ->
    level:int ->
    round:int ->
    block_payload_hash:string ->
    t

  (** [preattestation ?use_legacy_name ~level ~round ~slot ~block_payload_hash]
      crafts a preattestation operation at [level] on the [round] with the
      [slot] and [block_payload_hash]. If [use_legacy_name] is set, the [kind]
      field in the crafted JSON will be "preendorsement" instead of
      "preattestation". *)
  val preattestation :
    use_legacy_name:bool ->
    slot:int ->
    level:int ->
    round:int ->
    block_payload_hash:string ->
    t

  (** [attestation ?use_legacy_name ~level ~round ~slot ~block_payload_hash
      ?dal_attestation ()] crafts an attestation operation at the given [level]
      on the given [round] with the given [slot] and [block_payload_hash] and
      optionally the given [dal_attestation]. If [use_legacy_name] is set, the
      [kind] field in the crafted JSON will be "endorsement" instead of
      "attestation". *)
  val attestation :
    use_legacy_name:bool ->
    slot:int ->
    level:int ->
    round:int ->
    block_payload_hash:string ->
    ?dal_attestation:bool array ->
    unit ->
    t

  (** [kind_to_string kind ~use_legacy_name] returns the name of the
      [kind]. If [use_legacy_name] is set, the name corresponding to the [kind]
      will be "(pre)endorsement" instead of "(pre)attestation". *)
  val kind_to_string : consensus_kind -> use_legacy_name:bool -> string

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

  (** Retrieves the attestation slots at [level] by calling the [GET
      /chains/<chain>/blocks/<block>/helpers/validators] RPC. *)
  val get_slots : level:int -> Client.t -> JSON.t Lwt.t

  (** Returns the first slot of the provided delegate in the
      [slots_json] that describes all attestation rights at some
      level.

      Causes the test to fail if the delegate is not found. *)
  val first_slot : slots_json:JSON.t -> Account.key -> int

  (** Calls the [GET /chains/<chain>/blocks/<block>/header] RPC and
      extracts the head block's payload hash from the result. *)
  val get_block_payload_hash : ?block:string -> Client.t -> string Lwt.t
end

module Anonymous : sig
  (** A representation of an anonymous operation. *)
  type t

  type double_consensus_evidence_kind =
    | Double_attestation_evidence
    | Double_preattestation_evidence

  (** [double_consensus_evidence ~kind ~use_legacy_name op1 op2] crafts a double
      consensus evidence operation with the [kind], [op1] and [op2]. Both
      operations should be of the same kind and the same as the one expected by
      [kind]. If [use_legacy_name] is set, the [kind] field in the crafted JSON
      will be "(pre)endorsement" instead of "(pre)attestation". *)
  val double_consensus_evidence :
    kind:double_consensus_evidence_kind ->
    use_legacy_name:bool ->
    operation * Tezos_crypto.Signature.t ->
    operation * Tezos_crypto.Signature.t ->
    t

  (** [double_attestation_evidence ~use_legacy_name op1 op2] crafts a double
      attestation evidence operation with op1 and op2. Both operations should be
      attestations. If [use_legacy_name] is set, the [kind] field in the crafted
      JSON will be "endorsement" instead of "attestation". *)
  val double_attestation_evidence :
    use_legacy_name:bool ->
    operation * Tezos_crypto.Signature.t ->
    operation * Tezos_crypto.Signature.t ->
    t

  (** [double_preattestation_evidence ~use_legacy_name op1 op2] crafts a double
      attestation evidence operation with op1 and op2. Both operations should be
      preattestations. If [use_legacy_name] is set, the [kind] field in the
      crafted JSON will be "preendorsement" instead of "preattestation". *)
  val double_preattestation_evidence :
    use_legacy_name:bool ->
    operation * Tezos_crypto.Signature.t ->
    operation * Tezos_crypto.Signature.t ->
    t

  (** [kind_to_string kind ~use_legacy_name] return the name of the [kind]. If
      [use_legacy_name] is set, the name corresponding to the [kind] will be
      "double_(pre)endorsement_evidence" instead of
      "double_(pre)attestation_evidence". *)
  val kind_to_string :
    double_consensus_evidence_kind -> use_legacy_name:bool -> string

  (** [operation] constructs an operation from an anonymous operation. the
      [client] is used to fetch the branch and the [chain_id]. *)
  val operation : ?branch:string -> t -> Client.t -> operation Lwt.t

  (** A wrapper for {!val:inject} with anonymous operations. The client is used
      to get all the data that was not provided if it can be recovered via RPCs.
      Mainly those are the [branch] and the [chain_id]. *)
  val inject :
    ?request:[`Inject | `Notify] ->
    ?force:bool ->
    ?branch:string ->
    ?error:rex ->
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

     @param protocol_hashes A list of candidate protocol hashes. *)
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

  (** [origination ?(init_balance=0) ~code ~init_storage ()]
     builds an origination operation. *)
  val origination :
    ?init_balance:int -> code:JSON.u -> init_storage:JSON.u -> unit -> payload

  (** [call ~dest ~amount:0 ~entrypoint ~arg ()] builds a smart contract call
      operation to the [entrypoint] with the provided Michelson argument
      [arg]. Note that the amount is expressed in mutez. *)
  val call :
    ?dest:string ->
    ?amount:int ->
    ?entrypoint:string ->
    ?arg:JSON.u ->
    unit ->
    payload

  (** [dal_publish_commitment ~level ~index ~header] builds an
     operation for the data-availability layer that publishes a
     slot. *)
  val dal_publish_commitment :
    index:int ->
    commitment:Tezos_crypto_dal.Cryptobox.commitment ->
    proof:Tezos_crypto_dal.Cryptobox.commitment_proof ->
    payload

  (** [delegation ?(delegate=Constant.bootstrap2) ()] builds a
     delegation operation. *)
  val delegation : ?delegate:Account.key -> unit -> payload

  (** The sc_rollup_proof structure is complex to be written by hand during tests
     and should likely be generated by a PVM. We expose it as a JSON value. *)
  type sc_rollup_proof = JSON.u

  (** A section of a proof is make of a state hash, if any, and of a tick. *)
  type sc_rollup_dissection_chunk = {state_hash : string option; tick : int}

  (** A refutation step is either a dissection or a proof. *)
  type sc_rollup_game_refutation_step =
    | Proof of sc_rollup_proof
    | Dissection of sc_rollup_dissection_chunk list

  (** An sc_rollup_refutation is the information submitted by players during a
     (refutation) game. *)
  type sc_rollup_refutation =
    | Start of {
        player_commitment_hash : string;
        opponent_commitment_hash : string;
      }
    | Move of {
        choice_tick : int;
        refutation_step : sc_rollup_game_refutation_step;
      }

  (** [sc_rollup_refute ?refutation ~rollup ~oppenent] builds an Sc rollup
      refutation manager operation payload. The refutation is [None] in case
      of initialization or some game refutation step (either a dissection or
      a proof) otherwise. [sc_rollup] is the Sc rollup's address and [opponent] the
      public key hash of the staker who published the commitment we are about
      to refute. *)
  val sc_rollup_refute :
    refutation:sc_rollup_refutation ->
    sc_rollup:string ->
    opponent:string ->
    unit ->
    payload

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

  (** A wrapper for {!RPC.get_chain_block_hash} with an offset for the block.

      [offset] defaults to [2], to pick the latested finalized branch with
      Tenderbake.
  *)
  val get_branch : ?chain:string -> ?offset:int -> Client.t -> string Lwt.t

  (** A wrapper for {!val-operation} on a list consisting in a single
      {!val-transfer}. See both functions for details and default
      values of arguments. *)
  val mk_single_transfer :
    ?source:Account.key ->
    ?counter:int ->
    ?fee:int ->
    ?gas_limit:int ->
    ?storage_limit:int ->
    ?dest:Account.key ->
    ?amount:int ->
    ?branch:string ->
    ?signer:Account.key ->
    Client.t ->
    operation Lwt.t

  (** A wrapper for {!inject}ing a batch consisting in a single
      transfer. See {!transfer}, {!make_batch}, and {!inject} for the
      descriptions and default values of the arguments. *)
  val inject_single_transfer :
    ?source:Account.key ->
    ?counter:int ->
    ?fee:int ->
    ?gas_limit:int ->
    ?storage_limit:int ->
    ?dest:Account.key ->
    ?amount:int ->
    ?request:[< `Arrived | `Flush | `Inject | `Notify > `Inject] ->
    ?force:bool ->
    ?branch:string ->
    ?signer:Account.key ->
    ?error:rex ->
    Client.t ->
    [`OpHash of string] Lwt.t
end

(** Regular expressions for specific error messages.

    Can be used as e.g.
    - the [error] argument of {!val:inject}
    - the [rex] argument of {!val:inject_and_capture2_stderr}
    - the [msg] argument of {!val:Process.check_error}. *)

(** Matches the client message for the [Operation_quota_exceeded]
    protocol error. *)
val gas_limit_exceeded : rex

(** Matches the message produced by
    [Operation_conflict {new_hash; needed_fee_in_mutez = Some fee}]
    from [src/lib_shell_services/validation_errors].

    Captures [new_hash] and [fee]. *)
val conflict_error_with_needed_fee : rex

(** Matches the message produced by
    [Operation_conflict {new_hash; needed_fee_in_mutez = None}]
    from [src/lib_shell_services/validation_errors].

    Captures [new_hash]. *)
val conflict_error_no_possible_fee : rex

(** Matches the message produced by
    [Rejected_by_full_mempool {hash; needed_fee_in_mutez = Some fee}]
    from [src/lib_shell_services/validation_errors].

    Captures [hash] and [fee]. *)
val rejected_by_full_mempool_with_needed_fee : rex

(** Matches the message produced by
    [Rejected_by_full_mempool {hash; needed_fee_in_mutez = None}]
    from [src/lib_shell_services/validation_errors].

    Captures [hash]. *)
val rejected_by_full_mempool_no_possible_fee : rex

(** Matches the message produced by
    [Dal_data_availibility_attester_not_in_committee {attester; level; slot}]
    from [src/proto_alpha/lib_protocol/dal_errors_repr].

    Captures [attester], [level], and [slot]. *)
val dal_data_availibility_attester_not_in_committee : rex

(** Calls {!inject_and_capture2_stderr} and checks that the second
    captured group is [expected_fee].

    Intended to be used with {!conflict_error_with_needed_fee} or
    {!rejected_by_full_mempool_with_needed_fee} as [rex]. *)
val inject_error_check_recommended_fee :
  loc:string -> rex:rex -> expected_fee:int -> t -> Client.t -> unit Lwt.t
