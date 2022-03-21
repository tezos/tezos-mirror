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

val transaction :
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
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

    {ul {li Transaction to the deposit entrypoint of a transaction rollup,
            as these transactions are necessarily internals.}}
 *)
val unsafe_transaction :
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?parameters:Script.lazy_expr ->
  ?entrypoint:Entrypoint.t ->
  Context.t ->
  Contract.t ->
  Destination.t ->
  Tez.t ->
  Operation.packed tzresult Lwt.t

val delegation :
  ?fee:Tez.tez ->
  Context.t ->
  Contract.t ->
  public_key_hash option ->
  Operation.packed tzresult Lwt.t

val set_deposits_limit :
  ?fee:Tez.tez ->
  Context.t ->
  Contract.t ->
  Tez.tez option ->
  Operation.packed tzresult Lwt.t

val revelation :
  ?fee:Tez.tez -> Context.t -> public_key -> Operation.packed tzresult Lwt.t

val failing_noop :
  Context.t -> public_key_hash -> string -> Operation.packed tzresult Lwt.t

(** [contract_origination ctxt source] Create a new contract origination
    operation, sign it with [source] and returns it alongside the contract
    address. The contract address is using the initial origination nonce with the
    hash of the operation. If this operation is combine with [combine_operations]
    then the contract address is false as the nonce is not based on the correct
    operation hash. *)
val contract_origination :
  ?counter:Z.t ->
  ?delegate:public_key_hash ->
  script:Script.t ->
  ?public_key:public_key ->
  ?credit:Tez.tez ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.contract ->
  (Operation.packed * Contract.contract) tzresult Lwt.t

val originated_contract : Operation.packed -> Contract.contract

val register_global_constant :
  ?counter:Z.t ->
  ?public_key:Signature.public_key ->
  ?fee:Tez.tez ->
  ?gas_limit:Alpha_context.Gas.Arith.integral ->
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

(** Batch a list of (already signed) operations and (re-)sign with the [source].
    No revelation is inserted and the counters are kept as they are. *)
val batch_operations :
  source:Contract.t ->
  Context.t ->
  packed_operation list ->
  packed_operation tzresult Lwt.t

(** Reveals a seed_nonce that was previously committed at a certain level *)
val seed_nonce_revelation :
  Context.t -> Raw_level.t -> Nonce.t -> Operation.packed

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
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  (Operation.packed * Tx_rollup.t) tzresult Lwt.t

(** [tx_rollup_submit_batch ctxt source tx_rollup batch] submits
    [batch], an array of bytes that is expected to be a batch of L2
    transactions, to be appended in the inbox of [tx_rollup].  *)
val tx_rollup_submit_batch :
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?burn_limit:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  string ->
  Operation.packed tzresult Lwt.t

(** [sc_rollup_origination ctxt source kind boot_sector] originates a new
    smart contract rollup of some given [kind] booting using [boot_sector].
    The process is the same as in [tx_rollup_origination]. *)
val sc_rollup_origination :
  ?counter:counter ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:counter ->
  Context.t ->
  Contract.t ->
  Sc_rollup.Kind.t ->
  string ->
  packed_operation tzresult Lwt.t

(** [tx_rollup_commit ctxt source tx_rollup commitment] Commits to a tx
    rollup state. *)
val tx_rollup_commit :
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  Tx_rollup_commitment.t ->
  Operation.packed tzresult Lwt.t

(** [tx_rollup_return_bond ctxt source tx_rollup] returns a commitment bond. *)
val tx_rollup_return_bond :
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  Operation.packed tzresult Lwt.t

(** [tx_rollup_finalize ctxt source tx_rollup] finalizes the most recent
    final level of a rollup. *)
val tx_rollup_finalize :
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  Operation.packed tzresult Lwt.t

(** [tx_rollup_remove_commitment ctxt source tx_rollup] tries to
    remove a commitment from the rollup context. *)
val tx_rollup_remove_commitment :
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  Operation.packed tzresult Lwt.t

(** [tx_rollup_withdraw] executes a tx rollup withdrawal.

    The arguments are:

    - [Context.t]: the context on which to apply the operation
    - [source:Contract.t]: the source contract of the operation
    - [Tx_rollup.t]: the rollup to which the withdrawal pertains
    - [Tx_rollup_level.t]: the level on which the withdrawal was commited
    - [context_hash:bytes]: the hash of the context of the rollup when the withdrawal was commited
    - [message_index:int]: the inbox index of the message that that emitted this withdrawal
    - [contents:Script.lazy_expr]: the contents of the ticket of the withdrawal
    - [ty:Script.lazy_expr]: the type of the ticket of the withdrawal
    - [ticketer:Contract.t]: the ticketer of the ticket of the withdrawal
    - [Tx_rollup_l2_qty.t]: the qty of the ticket of the withdrawal
    - [destination:Contract.t]: the destination contract that should received the ticket of the withdrawal
    - [Tx_rollup_withdraw.merkle_tree_path]: the proof that this withdrawal was contained in the commitment
    - [Entrypoint_repr.t]: the entrypoint of the destination contract to which the ticket should be sent

 *)
val tx_rollup_withdraw :
  ?counter:counter ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:counter ->
  Context.t ->
  source:Contract.t ->
  Tx_rollup.t ->
  Tx_rollup_level.t ->
  context_hash:Context_hash.t ->
  message_index:int ->
  contents:Script.lazy_expr ->
  ty:Script.lazy_expr ->
  ticketer:Contract.t ->
  Tx_rollup_l2_qty.t ->
  destination:Contract.t ->
  withdraw_position:int ->
  Tx_rollup_withdraw.Merkle.root ->
  Tx_rollup_withdraw.Merkle.path ->
  Entrypoint_repr.t ->
  (packed_operation, tztrace) result Lwt.t

(** [tx_rollup_reject ctxt source tx_rollup tx_rollup level message
    index proof] Rejects a tx rollup commitment. *)
val tx_rollup_reject :
  ?counter:Z.t ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.t ->
  Tx_rollup.t ->
  Tx_rollup_level.t ->
  Tx_rollup_message.t ->
  message_position:int ->
  message_path:Tx_rollup_inbox.Merkle.path ->
  proof:Tx_rollup_l2_proof.t ->
  previous_message_result:Tx_rollup_commitment.message_result ->
  Operation.packed tzresult Lwt.t
