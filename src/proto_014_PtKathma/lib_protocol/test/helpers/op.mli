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

val sign :
  ?watermark:Signature.watermark ->
  Signature.secret_key ->
  Context.t ->
  packed_contents_list ->
  packed_operation

val endorsement :
  ?delegate:public_key_hash * Slot.t list ->
  ?slot:Slot.t ->
  ?level:Raw_level.t ->
  ?round:Round.t ->
  ?block_payload_hash:Block_payload_hash.t ->
  endorsed_block:Block.t ->
  Context.t ->
  ?signing_context:Context.t ->
  unit ->
  Kind.endorsement Operation.t tzresult Lwt.t

val preendorsement :
  ?delegate:public_key_hash * Slot.t list ->
  ?slot:Slot.t ->
  ?level:Raw_level.t ->
  ?round:Round.t ->
  ?block_payload_hash:Block_payload_hash.t ->
  endorsed_block:Block.t ->
  Context.t ->
  ?signing_context:Context.t ->
  unit ->
  Kind.preendorsement Operation.t tzresult Lwt.t

val miss_signed_endorsement :
  ?level:Raw_level.t ->
  endorsed_block:Block.t ->
  Context.t ->
  Kind.endorsement Operation.t tzresult Lwt.t

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
  ?counter:Z.t ->
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
  ?counter:counter ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:counter ->
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
  ?counter:Z.t ->
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
  ?counter:Z.t ->
  Context.t ->
  Contract.t ->
  Tez.tez option ->
  Operation.packed tzresult Lwt.t

val increase_paid_storage :
  ?force_reveal:bool ->
  ?counter:Z.t ->
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

    {li [?storage_limit:counter]: forces a storage limit, otherwise
    set to [Z.zero]}
*)
val revelation :
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:counter ->
  ?counter:counter ->
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
  ?counter:Z.t ->
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
  ?counter:Z.t ->
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
  ?counter:Z.t ->
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

val double_endorsement :
  Context.t ->
  Kind.endorsement Operation.t ->
  Kind.endorsement Operation.t ->
  Operation.packed

val double_preendorsement :
  Context.t ->
  Kind.preendorsement Operation.t ->
  Kind.preendorsement Operation.t ->
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
  ?counter:counter ->
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

(** Propose a list of protocol hashes during the approval voting *)
val proposals :
  Context.t ->
  Contract.t ->
  Protocol_hash.t list ->
  Operation.packed tzresult Lwt.t

(** Cast a vote yay, nay or pass *)
val ballot :
  Context.t ->
  Contract.t ->
  Protocol_hash.t ->
  Vote.ballot ->
  Operation.packed tzresult Lwt.t

val dummy_script : Script.t

val dummy_script_cost : Tez.t

(** [tx_rollup_origination ctxt source] Originate a new tx rollup operation,
    sign it with [source] and returns it alongside the tx rollup address. The
    tx_rollup address is using the initial origination nonce with the hash of the
    operation. If this operation is combined with [combine_operations] then the
    tx rollup address is false as the nonce is not based on the correct operation
    hash. *)
val tx_rollup_origination :
  ?force_reveal:bool ->
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  (Operation.packed * Tx_rollup.t) tzresult Lwt.t

(** [tx_rollup_submit_batch ctxt source tx_rollup batch] submits
    [batch], an array of bytes that is expected to be a batch of L2
    transactions, to be appended in the inbox of [tx_rollup].

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val tx_rollup_submit_batch :
  ?force_reveal:bool ->
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?burn_limit:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  string ->
  Operation.packed tzresult Lwt.t

(** [tx_rollup_commit ctxt source tx_rollup commitment] Commits to a
    tx.

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val tx_rollup_commit :
  ?force_reveal:bool ->
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  Tx_rollup_commitment.Full.t ->
  Operation.packed tzresult Lwt.t

(** [tx_rollup_return_bond ctxt source tx_rollup] returns a commitment
    bond.

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default..}} *)
val tx_rollup_return_bond :
  ?force_reveal:bool ->
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  Operation.packed tzresult Lwt.t

(** [tx_rollup_finalize ctxt source tx_rollup] finalizes the most
    recent final level of a rollup.

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val tx_rollup_finalize :
  ?force_reveal:bool ->
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  Operation.packed tzresult Lwt.t

(** [tx_rollup_remove_commitment ctxt source tx_rollup] tries to
    remove a commitment from the rollup context. *)
val tx_rollup_remove_commitment :
  ?force_reveal:bool ->
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  Operation.packed tzresult Lwt.t

(** [tx_rollup_dispatch_tickets ctxt ~source ~message_index tx_rollup
    level context_hash tickets_info] sends all tickets from
    [tickets_info] to the appropriate implicit accounts, as authorized
    by the [message_index]th hash of the commitment of [tx_rollup]
    posted for [level].

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val tx_rollup_dispatch_tickets :
  ?force_reveal:bool ->
  ?counter:counter ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:counter ->
  Context.t ->
  source:Contract.t ->
  message_index:int ->
  message_result_path:Tx_rollup_commitment.Merkle.path ->
  Tx_rollup.t ->
  Tx_rollup_level.t ->
  Context_hash.t ->
  Tx_rollup_reveal.t list ->
  (packed_operation, tztrace) result Lwt.t

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
  ?counter:counter ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:counter ->
  Context.t ->
  source:Contract.t ->
  contents:Script.lazy_expr ->
  ty:Script.lazy_expr ->
  ticketer:Contract.t ->
  Z.t ->
  destination:Contract.t ->
  Entrypoint_repr.t ->
  (packed_operation, tztrace) result Lwt.t

(** [tx_rollup_reject ctxt source tx_rollup tx_rollup level message
    index proof] Rejects a tx rollup commitment.

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val tx_rollup_reject :
  ?force_reveal:bool ->
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  Tx_rollup_level.t ->
  Tx_rollup_message.t ->
  message_position:int ->
  message_path:Tx_rollup_inbox.Merkle.path ->
  message_result_hash:Tx_rollup_message_result_hash.t ->
  message_result_path:Tx_rollup_commitment.Merkle.path ->
  proof:Tx_rollup_l2_proof.t ->
  previous_message_result:Tx_rollup_message_result.t ->
  previous_message_result_path:Tx_rollup_commitment.Merkle.path ->
  Operation.packed tzresult Lwt.t

(** [sc_rollup_origination ctxt source kind boot_sector] originates a
    new smart contract rollup of some given [kind] booting using
    [boot_sector].  The process is the same as in
    [tx_rollup_origination].

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val sc_rollup_origination :
  ?force_reveal:bool ->
  ?counter:counter ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:counter ->
  Context.t ->
  Contract.t ->
  Sc_rollup.Kind.t ->
  string ->
  Script.lazy_expr ->
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
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  Sc_rollup.Commitment.t ->
  Operation.packed tzresult Lwt.t

(** [sc_rollup_cement ctxt source rollup commitment] tries to cement
    the specified commitment.

    Optional arguments allow to override defaults:

    {ul {li [?force_reveal:bool]: prepend the operation to reveal
    [source]'s public key if the latter has not been revealed
    yet. Disabled (set to [false]) by default.}} *)
val sc_rollup_cement :
  ?force_reveal:bool ->
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  Sc_rollup.Commitment.Hash.t ->
  Operation.packed tzresult Lwt.t

val sc_rollup_execute_outbox_message :
  ?counter:counter ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:counter ->
  ?force_reveal:bool ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  Sc_rollup.Commitment.Hash.t ->
  outbox_level:Raw_level.t ->
  message_index:int ->
  inclusion_proof:string ->
  message:string ->
  (packed_operation, tztrace) result Lwt.t

(** [sc_rollup_recover_bond ctxt source sc_rollup] returns a commitment bond. *)
val sc_rollup_recover_bond :
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  ?force_reveal:bool ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  Operation.packed tzresult Lwt.t

val sc_rollup_add_messages :
  ?force_reveal:bool ->
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  string list ->
  Operation.packed tzresult Lwt.t

val sc_rollup_refute :
  ?force_reveal:bool ->
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:gas_limit ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Sc_rollup.t ->
  public_key_hash ->
  Sc_rollup.Game.refutation ->
  bool ->
  Operation.packed tzresult Lwt.t

val sc_rollup_timeout :
  ?force_reveal:bool ->
  ?counter:Z.t ->
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
  ?counter:counter ->
  ?fee:Tez.t ->
  ?gas_limit:gas_limit ->
  ?storage_limit:counter ->
  Context.t ->
  Contract.t ->
  Dal.Slot.t ->
  (packed_operation, tztrace) result Lwt.t
