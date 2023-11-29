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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3181
   Improve documentation of the operation helpers *)

(** Assemble the given signature and [contents_list] into a
    [packed_operation].

    The context argument is used to retrieve the branch.

    If the [signature option] argument is [None], then the resulting
    operation is unsigned.

    This function is mainly useful to craft an operation with a
    missing or invalid signatue. Otherwise, it is often better to use
    one of the helpers below: they handle the signature internally to
    directly return well-signed operations. *)
val pack_operation :
  Context.t -> signature option -> 'a contents_list -> packed_operation

val sign :
  ?watermark:Signature.watermark ->
  Signature.secret_key ->
  Block_hash.t ->
  packed_contents_list ->
  packed_operation

(** Create an unpacked attestation that is expected for given [Block.t].

    Optional parameters allow to specify the attested values: [level],
    [round] and/or [block_payload_hash].

    They also allow to specify the attester ([delegate]), and/or the
    [slot]. These default to the first slot and its delegate.

    Finally, the operation [branch] can be specified. It defaults to the
    predecessor of the attested block. *)
val raw_attestation :
  ?delegate:public_key_hash ->
  ?slot:Slot.t ->
  ?level:Raw_level.t ->
  ?round:Round.t ->
  ?block_payload_hash:Block_payload_hash.t ->
  ?branch:Block_hash.t ->
  Block.t ->
  Kind.attestation Operation.t tzresult Lwt.t

(** Create an unpacked preattestation that is expected for a given
    [Block.t].

    Optional parameters are the same than {!raw_attestation}. *)
val raw_preattestation :
  ?delegate:public_key_hash ->
  ?slot:Slot.t ->
  ?level:Raw_level.t ->
  ?round:Round.t ->
  ?block_payload_hash:Block_payload_hash.t ->
  ?branch:Block_hash.t ->
  Block.t ->
  Kind.preattestation Operation.t tzresult Lwt.t

(** Create a packed attestation that is expected for a given
    [Block.t] by packing the result of {!raw_attestation}. *)
val attestation :
  ?delegate:public_key_hash ->
  ?slot:Slot.t ->
  ?level:Raw_level.t ->
  ?round:Round.t ->
  ?block_payload_hash:Block_payload_hash.t ->
  ?branch:Block_hash.t ->
  Block.t ->
  Operation.packed tzresult Lwt.t

(** Create a packed preattestation that is expected for a given
    [Block.t] by packing the result of {!raw_preattestation}. *)
val preattestation :
  ?delegate:public_key_hash ->
  ?slot:Slot.t ->
  ?level:Raw_level.t ->
  ?round:Round.t ->
  ?block_payload_hash:Block_payload_hash.t ->
  ?branch:Block_hash.t ->
  Block.t ->
  Operation.packed tzresult Lwt.t

type gas_limit =
  | Max  (** Max corresponds to the [max_gas_limit_per_operation] constant. *)
  | High
      (** High corresponds to [50_000] gas unit which should cover a
      majority of use-cases. This is the default used when forging
      manager operations. *)
  | Low  (** Low corresponds to the gas entry cost of a manager operation *)
  | Zero
  | Custom_gas of Gas.Arith.integral

(** Pretty printer for gas_limit type. *)
val pp_gas_limit : Format.formatter -> gas_limit -> unit

val transaction :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  ?parameters:Script.lazy_expr ->
  ?entrypoint:Entrypoint.t ->
  Context.t ->
  Contract.t ->
  Contract.t ->
  Tez.t ->
  Operation.packed tzresult Lwt.t

(** Same as [transaction], but with a more generic destination
    parameter. It is said unsafe because it can construct transactions
    that will always fail, such as

    {ul {li Transaction to the deposit entrypoint of a transaction
    rollup, as these transactions are necessarily internals.}}

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val unsafe_transaction :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  ?parameters:
    Michelson_v1_primitives.prim Micheline.canonical Data_encoding.lazy_t ->
  ?entrypoint:Entrypoint_repr.t ->
  Context.t ->
  Contract.t ->
  Contract.t ->
  Tez.t ->
  packed_operation tzresult Lwt.t

val delegation :
  ?force_reveal:bool ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?counter:Manager_counter.t ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  public_key_hash option ->
  Operation.packed tzresult Lwt.t

val set_deposits_limit :
  ?force_reveal:bool ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  Context.t ->
  Contract.t ->
  Tez.tez option ->
  Operation.packed tzresult Lwt.t

val increase_paid_storage :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  source:Contract.t ->
  destination:Contract_hash.t ->
  Z.t ->
  Operation.packed tzresult Lwt.t

(** [revelation ?fee ?gas_limit ?forge_pkh ctxt pkh] Creates a new
    [Reveal] {!manager_operation} to reveal a public key [pkh]
    applying to current context [ctxt].

    Optional arguments allow to override defaults:

    {ul {li [?fee:Tez.tez]: specify a fee, otherwise set to
    [Tez.zero].}

    {li [?gas_limit:Gas.Arith.integral]: force a gas limit, otherwise
    set to 10000 gas units.}

    {li [?forge_pkh]: use a provided [pkh] as source, instead of
    hashing [pkh]. Useful for forging non-honest reveal operations}

    {li [?storage_limit:Z.t]: forces a storage limit, otherwise
    set to [Z.zero]}}
*)
val revelation :
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  ?forge_pkh:public_key_hash option ->
  Context.t ->
  public_key ->
  (packed_operation, tztrace) result Lwt.t

val failing_noop :
  Context.t -> public_key_hash -> string -> Operation.packed tzresult Lwt.t

(** [contract_origination ctxt source] Create a new contract origination
    operation, sign it with [source] and returns it alongside the contract
    address. The contract address is using the initial origination nonce with the
    hash of the operation. If this operation is combined with [combine_operations]
    then the contract address is false as the nonce is not based on the correct
    operation hash.

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val contract_origination :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?delegate:public_key_hash ->
  script:Script.t ->
  ?public_key:public_key ->
  ?credit:Tez.tez ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  (Operation.packed * Contract.t) tzresult Lwt.t

val contract_origination_hash :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?delegate:public_key_hash ->
  script:Script.t ->
  ?public_key:public_key ->
  ?credit:Tez.tez ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  (Operation.packed * Contract_hash.t) tzresult Lwt.t

val originated_contract : Operation.packed -> Contract.t

val register_global_constant :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?public_key:Signature.public_key ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  (* Account doing the registration *)
  source:Contract.t ->
  (* Micheline value to be registered *)
  value:Protocol.Alpha_context.Script.lazy_expr ->
  (Protocol.operation, tztrace) result Lwt.t

val double_attestation :
  Context.t ->
  Kind.attestation Operation.t ->
  Kind.attestation Operation.t ->
  Operation.packed

val double_preattestation :
  Context.t ->
  Kind.preattestation Operation.t ->
  Kind.preattestation Operation.t ->
  Operation.packed

val double_baking :
  Context.t ->
  Block_header.block_header ->
  Block_header.block_header ->
  Operation.packed

val activation :
  Context.t ->
  Signature.Public_key_hash.t ->
  Blinded_public_key_hash.activation_code ->
  Operation.packed tzresult Lwt.t

val combine_operations :
  ?public_key:public_key ->
  ?counter:Manager_counter.t ->
  ?spurious_operation:packed_operation ->
  source:Contract.t ->
  Context.t ->
  packed_operation list ->
  packed_operation tzresult Lwt.t

(** Batch a list of (already signed) operations and (re-)sign with the
    [source]. No revelation is inserted and the counters are kept as
    they are unless [recompute_counters] is set to [true] (defaults false). *)
val batch_operations :
  ?recompute_counters:bool ->
  source:Contract.t ->
  Context.t ->
  packed_operation list ->
  packed_operation tzresult Lwt.t

(** Reveals a seed_nonce that was previously committed at a certain level *)
val seed_nonce_revelation :
  Context.t -> Raw_level.t -> Nonce.t -> Operation.packed

(** Reveals a VDF with a proof of correctness *)
val vdf_revelation : Context.t -> Seed.vdf_solution -> Operation.packed

(** Craft the [contents_list] for a Proposals operation.

    Invocation: [proposals_contents ctxt source ?period proposals].

    @param period defaults to the index of the current voting period
    in [ctxt]. *)
val proposals_contents :
  Context.t ->
  Contract.t ->
  ?period:int32 ->
  Protocol_hash.t list ->
  Kind.proposals contents_list tzresult Lwt.t

(** Craft a Proposals operation.

    Invocation: [proposals ctxt source ?period proposals].

    @param period defaults to the index of the current voting period
    in [ctxt]. *)
val proposals :
  Context.t ->
  Contract.t ->
  ?period:int32 ->
  Protocol_hash.t list ->
  Operation.packed tzresult Lwt.t

(** Craft the [contents_list] for a Ballot operation.

    Invocation: [ballot_contents ctxt source ?period proposal ballot].

    @param period defaults to the index of the current voting period
    in [ctxt]. *)
val ballot_contents :
  Context.t ->
  Contract.t ->
  ?period:int32 ->
  Protocol_hash.t ->
  Vote.ballot ->
  Kind.ballot contents_list tzresult Lwt.t

(** Craft a Ballot operation.

    Invocation: [ballot ctxt source ?period proposal ballot].

    @param period defaults to the index of the current voting period
    in [ctxt]. *)
val ballot :
  Context.t ->
  Contract.t ->
  ?period:int32 ->
  Protocol_hash.t ->
  Vote.ballot ->
  Operation.packed tzresult Lwt.t

val dummy_script : Script.t

val dummy_script_cost : Tez.t

(** [transfer_ticket] allows an implicit account to transfer tickets they owned.

    The arguments are:

    {ul
      {li [Context.t]: the context on which to apply the operation}
      {li [source:Contract.t]: the source contract of the operation}
      {li [Tx_rollup.t]: the rollup to which the withdrawal pertains}
      {li [Tx_rollup_level.t]: the level on which the withdrawal was commited}
      {li [contents:Script.lazy_expr]: the contents of the ticket of
          the withdrawal}
      {li [ty:Script.lazy_expr]: the type of the ticket of the withdrawal}
      {li [ticketer:Contract.t]: the ticketer of the ticket of the withdrawal}
      {li [Z.t]: the quantity of the ticket of the withdrawal}
      {li [destination:Contract.t]: the destination contract that
          should receive the ticket of the withdrawal}
      {li [Entrypoint_repr.t]: the entrypoint of the destination
          contract to which the ticket should be sent}}

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val transfer_ticket :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  source:Contract.t ->
  contents:Script.lazy_expr ->
  ty:Script.lazy_expr ->
  ticketer:Contract.t ->
  amount:Ticket_amount.t ->
  destination:Contract.t ->
  entrypoint:Entrypoint_repr.t ->
  (packed_operation, tztrace) result Lwt.t

(** [sc_rollup_origination ctxt source kind boot_sector] originates a
    new smart contract rollup of some given [kind] booting using
    [boot_sector].

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val sc_rollup_origination :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  ?whitelist:Sc_rollup.Whitelist.t ->
  Context.t ->
  Contract.t ->
  Sc_rollup.Kind.t ->
  boot_sector:string ->
  parameters_ty:Script.lazy_expr ->
  (packed_operation * Sc_rollup.t) tzresult Lwt.t

(** [sc_rollup_publish ctxt source rollup commitment] tries to publish
    a commitment to the SCORU.  Optional arguments allow to override
    defaults:

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val sc_rollup_publish :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  Sc_rollup.Commitment.t ->
  Operation.packed tzresult Lwt.t

(** [sc_rollup_cement ctxt source rollup] tries to cement a commitment.

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val sc_rollup_cement :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  Operation.packed tzresult Lwt.t

val sc_rollup_execute_outbox_message :
  ?counter:Manager_counter.t ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  ?force_reveal:bool ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  Sc_rollup.Commitment.Hash.t ->
  output_proof:string ->
  (packed_operation, tztrace) result Lwt.t

(** [sc_rollup_recover_bond ctxt source sc_rollup staker] recovers the
    commitment bond of [staker]. *)
val sc_rollup_recover_bond :
  ?counter:Manager_counter.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  ?force_reveal:bool ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  public_key_hash ->
  Operation.packed tzresult Lwt.t

val sc_rollup_add_messages :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  string list ->
  Operation.packed tzresult Lwt.t

val sc_rollup_refute :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  public_key_hash ->
  Sc_rollup.Game.refutation ->
  Operation.packed tzresult Lwt.t

val sc_rollup_timeout :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  Sc_rollup.Game.Index.t ->
  Operation.packed tzresult Lwt.t

val dal_publish_slot_header :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Dal.Operations.Publish_slot_header.t ->
  (packed_operation, tztrace) result Lwt.t

(** [zk_rollup_origination ctxt source ~public_parameters ~circuits_info
    ~init_state ~nb_ops] tries to originate a ZK Rollup. *)
val zk_rollup_origination :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  public_parameters:Plonk.Main_protocol.verifier_public_parameters ->
  circuits_info:[`Public | `Private | `Fee] Zk_rollup.Account.SMap.t ->
  init_state:Zk_rollup.State.t ->
  nb_ops:int ->
  (Operation.packed * Zk_rollup.t) tzresult Lwt.t

val update_consensus_key :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  public_key ->
  (packed_operation, tztrace) result Lwt.t

val drain_delegate :
  Context.t ->
  consensus_key:Signature.Public_key_hash.t ->
  delegate:Signature.Public_key_hash.t ->
  destination:Signature.Public_key_hash.t ->
  packed_operation tzresult Lwt.t

(** [zk_rollup_publish ctxt source ~zk_rollup ~op] tries to add an operation
    to the pending list of a ZK Rollup. *)
val zk_rollup_publish :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  zk_rollup:Zk_rollup.t ->
  ops:(Zk_rollup.Operation.t * Zk_rollup.Ticket.t option) list ->
  Operation.packed tzresult Lwt.t

(** [zk_rollup_update ctxt source ~zk_rollup ~update] tries to apply an update
    to a ZK Rollup. *)
val zk_rollup_update :
  ?force_reveal:bool ->
  ?counter:Manager_counter.t ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  zk_rollup:Zk_rollup.t ->
  update:Zk_rollup.Update.t ->
  Operation.packed tzresult Lwt.t
