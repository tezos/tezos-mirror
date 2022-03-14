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

val list_contract_labels :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  (string * string * string) list tzresult Lwt.t

val get_storage :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  unparsing_mode:Script_ir_translator.unparsing_mode ->
  Contract.t ->
  Script.expr option tzresult Lwt.t

val get_contract_big_map_value :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Script.expr * Script.expr ->
  Script.expr option tzresult Lwt.t

val register_global_constant :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  source:Signature.public_key_hash ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  constant:string ->
  unit ->
  (Kind.register_global_constant Kind.manager Injection.result, tztrace) result
  Lwt.t

val get_big_map_value :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  unparsing_mode:Script_ir_translator.unparsing_mode ->
  Big_map.Id.t ->
  Script_expr_hash.t ->
  Script.expr tzresult Lwt.t

val get_script :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  unparsing_mode:Script_ir_translator.unparsing_mode ->
  Contract.t ->
  Script.t option tzresult Lwt.t

val get_script_hash :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Script_expr_hash.t option tzresult Lwt.t

val get_balance :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Tez.t tzresult Lwt.t

val get_frozen_deposits_limit :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Signature.Public_key_hash.t ->
  Tez.t option tzresult Lwt.t

val build_delegate_operation :
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  public_key_hash option ->
  Kind.delegation Annotated_manager_operation.t

val set_delegate :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  public_key_hash ->
  src_pk:public_key ->
  manager_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  public_key_hash option ->
  Kind.delegation Kind.manager Injection.result tzresult Lwt.t

val set_deposits_limit :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  public_key_hash ->
  src_pk:public_key ->
  manager_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  Tez.t option ->
  Kind.set_deposits_limit Kind.manager Injection.result tzresult Lwt.t

val register_as_delegate :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?fee:Tez.tez ->
  manager_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  public_key ->
  Kind.delegation Kind.manager Injection.result tzresult Lwt.t

val save_contract :
  force:bool ->
  #Protocol_client_context.full ->
  string ->
  Contract.t ->
  unit tzresult Lwt.t

val originate_contract :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?branch:int ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  delegate:public_key_hash option ->
  initial_storage:string ->
  balance:Tez.t ->
  source:public_key_hash ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  code:Script.expr ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Kind.origination Kind.manager Injection.result * Contract.t) tzresult Lwt.t

val parse_arg_transfer : string option -> Script.lazy_expr tzresult Lwt.t

val build_transaction_operation :
  amount:Tez.t ->
  parameters:Script.lazy_expr ->
  ?entrypoint:Entrypoint.t ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  Destination.t ->
  Kind.transaction Annotated_manager_operation.t

(** Same as {!transfer}, but takes parameters as {!Script.lazy_expr} instead of
    a raw string. This can be useful for commands that elaborate Micheline parameters,
    such as Multisigs or FA1.2 operations. *)
val transfer_with_script :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?force:bool ->
  ?branch:int ->
  ?successor_level:bool ->
  source:public_key_hash ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  destination:Destination.t ->
  ?entrypoint:Entrypoint.t ->
  parameters:Script.lazy_expr ->
  amount:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  fee_parameter:Injection.fee_parameter ->
  ?replace_by_fees:bool ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract.t list) tzresult
  Lwt.t

val transfer :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?force:bool ->
  ?branch:int ->
  ?successor_level:bool ->
  source:public_key_hash ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  destination:Destination.t ->
  ?entrypoint:Entrypoint.t ->
  ?arg:string ->
  amount:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  fee_parameter:Injection.fee_parameter ->
  ?replace_by_fees:bool ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract.t list) tzresult
  Lwt.t

val build_reveal_operation :
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  public_key ->
  Kind.reveal Annotated_manager_operation.t

val reveal :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?branch:int ->
  source:public_key_hash ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  ?fee:Tez.t ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  Kind.reveal Kind.manager Injection.result tzresult Lwt.t

type activation_key = {
  pkh : Ed25519.Public_key_hash.t;
  amount : Tez.t;
  activation_code : Blinded_public_key_hash.activation_code;
  mnemonic : string list;
  password : string;
  email : string;
}

val activation_key_encoding : activation_key Data_encoding.t

type batch_transfer_operation = {
  destination : string;
  fee : string option;
  gas_limit : Gas.Arith.integral option;
  storage_limit : Z.t option;
  amount : string;
  arg : string option;
  entrypoint : Entrypoint.t option;
}

val batch_transfer_operation_encoding : batch_transfer_operation Data_encoding.t

val activate_account :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?encrypted:bool ->
  ?force:bool ->
  activation_key ->
  string ->
  Kind.activate_account Injection.result tzresult Lwt.t

val activate_existing_account :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  string ->
  Blinded_public_key_hash.activation_code ->
  Kind.activate_account Injection.result tzresult Lwt.t

type period_info = {
  current_period_kind : Voting_period.kind;
  position : Int32.t;
  remaining : Int32.t;
  current_proposal : Protocol_hash.t option;
}

type ballots_info = {
  current_quorum : Int32.t;
  participation : Int32.t;
  supermajority : Int64.t;
  ballots : Vote.ballots;
}

val get_period_info :
  ?successor:bool ->
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  period_info tzresult Lwt.t

val get_ballots_info :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ballots_info tzresult Lwt.t

val get_proposals :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Int64.t Environment.Protocol_hash.Map.t tzresult Lwt.t

val submit_proposals :
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  src_sk:Client_keys.sk_uri ->
  public_key_hash ->
  Protocol_hash.t list ->
  Kind.proposals Injection.result_list tzresult Lwt.t

val submit_ballot :
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  src_sk:Client_keys.sk_uri ->
  public_key_hash ->
  Protocol_hash.t ->
  Vote.ballot ->
  Kind.ballot Injection.result_list tzresult Lwt.t

(** lookup an operation in [predecessors] previous blocks, and print the
    receipt if found *)
val display_receipt_for_operation :
  #Protocol_client_context.full ->
  chain:Block_services.chain ->
  ?predecessors:int ->
  Operation_list_hash.elt ->
  unit tzresult Lwt.t

val cached_contracts :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  (Contract.t * int) list tzresult Lwt.t

val contract_rank :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  int option tzresult Lwt.t

val contract_cache_size :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  int tzresult Lwt.t

val contract_cache_size_limit :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  int tzresult Lwt.t

val originate_tx_rollup :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  source:Signature.public_key_hash ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Operation_hash.t
  * Kind.tx_rollup_origination Kind.manager contents
  * Kind.tx_rollup_origination Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

val submit_tx_rollup_batch :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  ?burn_limit:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  source:Signature.public_key_hash ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  content:string ->
  tx_rollup:Tx_rollup.t ->
  unit ->
  (Operation_hash.t
  * Kind.tx_rollup_submit_batch Kind.manager contents
  * Kind.tx_rollup_submit_batch Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

val submit_tx_rollup_commitment :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  source:Signature.public_key_hash ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  level:int32 ->
  inbox_merkle_root:string ->
  batches:string ->
  predecessor:string option ->
  tx_rollup:Tx_rollup.t ->
  unit ->
  (Operation_hash.t
  * Kind.tx_rollup_commit Kind.manager contents
  * Kind.tx_rollup_commit Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

val submit_tx_rollup_finalize_commitment :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  source:Signature.public_key_hash ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  tx_rollup:Tx_rollup.t ->
  unit ->
  (Operation_hash.t
  * Kind.tx_rollup_finalize_commitment Kind.manager contents
  * Kind.tx_rollup_finalize_commitment Kind.manager
    Apply_results.contents_result)
  tzresult
  Lwt.t

val submit_tx_rollup_remove_commitment :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  source:Signature.public_key_hash ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  tx_rollup:Tx_rollup.t ->
  unit ->
  (Operation_hash.t
  * Kind.tx_rollup_remove_commitment Kind.manager contents
  * Kind.tx_rollup_remove_commitment Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

val submit_tx_rollup_rejection :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  source:Signature.public_key_hash ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  level:int32 ->
  tx_rollup:Tx_rollup.t ->
  message:string ->
  message_position:int ->
  message_path:string ->
  context_hash:string ->
  withdrawals_merkle_root:string ->
  proof:bool ->
  unit ->
  (Operation_hash.t
  * Kind.tx_rollup_rejection Kind.manager contents
  * Kind.tx_rollup_rejection Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

val sc_rollup_originate :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:counter ->
  ?counter:counter ->
  source:public_key_hash ->
  kind:Sc_rollup.Kind.t ->
  boot_sector:string ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  ( Operation_hash.t
    * Kind.sc_rollup_originate Kind.manager contents
    * Kind.sc_rollup_originate Kind.manager Apply_results.contents_result,
    tztrace )
  result
  Lwt.t

val sc_rollup_add_messages :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:counter ->
  ?counter:counter ->
  source:public_key_hash ->
  rollup:Alpha_context.Sc_rollup.t ->
  messages:string list ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Operation_hash.t
  * Kind.sc_rollup_add_messages Kind.manager contents
  * Kind.sc_rollup_add_messages Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

val sc_rollup_cement :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:counter ->
  ?counter:counter ->
  source:public_key_hash ->
  rollup:Alpha_context.Sc_rollup.t ->
  commitment:Alpha_context.Sc_rollup.Commitment_hash.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Operation_hash.t
  * Kind.sc_rollup_cement Kind.manager contents
  * Kind.sc_rollup_cement Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t
