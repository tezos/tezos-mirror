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

(** Calls {!Tezos_protocol_alpha.Protocol.Contract_services.list}. *)
val list_contract_labels :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  (string * string * string) list tzresult Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Contract.get_storage_normalized}. *)
val get_storage :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  unparsing_mode:Script_ir_unparser.unparsing_mode ->
  Contract_hash.t ->
  Script.expr option tzresult Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Contract.get_used_storage_space}. *)
val get_used_storage_space :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract_hash.t ->
  Z.t option tzresult Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Contract.get_paid_storage_space}. *)
val get_paid_storage_space :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract_hash.t ->
  Z.t option tzresult Lwt.t

(** Calls {!Tezos_protocol_alpha.Protocol.Contract_services.contract_big_map_get_opt}. *)
val get_contract_big_map_value :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract_hash.t ->
  Script.expr * Script.expr ->
  Script.expr option tzresult Lwt.t

(** Calls {!Injection.prepare_manager_operation}
    with [Register_global_constant constant] as operation. *)
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
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:Signature.public_key_hash ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  constant:string ->
  unit ->
  (Kind.register_global_constant Kind.manager Injection.result, tztrace) result
  Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Big_map.big_map_get_normalized}. *)
val get_big_map_value :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  unparsing_mode:Script_ir_unparser.unparsing_mode ->
  Big_map.Id.t ->
  Script_expr_hash.t ->
  Script.expr tzresult Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Contract.get_script_normalized}. *)
val get_script :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  unparsing_mode:Script_ir_unparser.unparsing_mode ->
  normalize_types:bool ->
  Contract_hash.t ->
  Script.t option tzresult Lwt.t

(** Calls {!Tezos_protocol_alpha.Protocol.Contract_services.script_opt}. *)
val get_script_hash :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract_hash.t ->
  Script_expr_hash.t option tzresult Lwt.t

(** Calls {!Tezos_protocol_alpha.Protocol.Contract_services.val-balance}. *)
val get_balance :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Tez.t tzresult Lwt.t

(** Calls {!Tezos_protocol_alpha.Protocol.Contract_services.val-staked_balance}. *)
val get_staked_balance :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Tez.t option tzresult Lwt.t

(** Calls {!Tezos_protocol_alpha.Protocol.Contract_services.val-full_balance}. *)
val get_full_balance :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Tez.t tzresult Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Contract.get_ticket_balance}. *)
val get_contract_ticket_balance :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Ticket_token.unparsed_token ->
  Z.t tzresult Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Contract.get_all_ticket_balances}. *)
val get_contract_all_ticket_balances :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract_hash.t ->
  (Ticket_token.unparsed_token * Z.t) list tzresult Lwt.t

val ticket_balances_encoding :
  (Ticket_token.unparsed_token * Z.t) list Data_encoding.t

(** Calls {!Tezos_protocol_alpha.Protocol.Delegate_services.val-frozen_deposits_limit}. *)
val get_frozen_deposits_limit :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Signature.Public_key_hash.t ->
  Tez.t option tzresult Lwt.t

(** Calls {!Injection.prepare_manager_operation}
    with {!Alpha_context.Delegation} [delegate_opt] as operation. *)
val build_delegate_operation :
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  public_key_hash option ->
  Kind.delegation Annotated_manager_operation.t

(** Calls {!Injection.inject_manager_operation}
    with {!Annotated_manager_operation.Single_manager} [build_delegate_operation ?fee opt_delegate]
    as operation. *)
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

val update_consensus_key :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  consensus_pk:Signature.public_key ->
  manager_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  Signature.public_key ->
  Kind.update_consensus_key Kind.manager Injection.result tzresult Lwt.t

val drain_delegate :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  consensus_sk:Client_keys.sk_uri ->
  consensus_pkh:Signature.public_key_hash ->
  ?destination:Signature.public_key_hash ->
  delegate:Signature.public_key_hash ->
  unit ->
  Kind.drain_delegate Injection.result tzresult Lwt.t

(** Calls {!Injection.inject_manager_operation}
    with {!Annotated_manager_operation.Single_manager} {!Alpha_context.Set_deposits_limit} [limit_opt]
    as operation. *)
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

(** Calls {!Injection.inject_manager_operation}
    with {!Annotated_manager_operation.Single_manager} {!Alpha_context.Increase_paid_storage}
    [{amount_in_bytes; destination}] as operation. *)
val increase_paid_storage :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?force:bool ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?fee:Tez.tez ->
  ?confirmations:int ->
  ?simulation:bool ->
  source:public_key_hash ->
  destination:Contract_hash.t ->
  src_pk:public_key ->
  manager_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  amount_in_bytes:Z.t ->
  unit ->
  Kind.increase_paid_storage Kind.manager Injection.result tzresult Lwt.t

(** Same as {!set_delegate} but the [~source] argument of {!Injection.inject_manager_operation}
    is {!Signature.Public_key.hash} [src_pk]. *)
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
  ?consensus_pk:public_key ->
  public_key ->
  (Kind.delegation Kind.manager Injection.result
  * (Kind.update_consensus_key Kind.manager contents
    * Kind.update_consensus_key Kind.manager Apply_results.contents_result)
    option)
  tzresult
  Lwt.t

(** Calls {!Raw_contract_alias.add}. *)
val save_contract :
  force:bool ->
  #Protocol_client_context.full ->
  string ->
  Contract.t ->
  unit tzresult Lwt.t

(** Injects the origination of a script into the context.
    See {!Injection.inject_manager_operation} for the injection, and
    {!Protocol.Alpha_context.Origination} for the origination parameters. *)
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
  ?safety_guard:Gas.Arith.integral ->
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

(** Calls {!Michelson_v1_parser.parse_expression arg}. *)
val parse_arg_transfer : string option -> Script.lazy_expr tzresult Lwt.t

(** Calls {!Injection.prepare_manager_operation}
    with {!Alpha_context.Transaction} [{amount;parameters;destinations;entrypoint}]
    as operation. *)
val build_transaction_operation :
  amount:Tez.t ->
  parameters:Script.lazy_expr ->
  ?entrypoint:Entrypoint.t ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  Contract.t ->
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
  destination:Contract.t ->
  ?entrypoint:Entrypoint.t ->
  parameters:Script.lazy_expr ->
  amount:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  fee_parameter:Injection.fee_parameter ->
  ?replace_by_fees:bool ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract_hash.t list)
  tzresult
  Lwt.t

(** Calls {!Injection.inject_manager_operation}
    with {!Annotated_manager_operation.Single_manager}
    [build_transaction_operation
    ~amount ~parameters ~entrypoint ?fee ?gas_limit ?storage_limit destination]
    as contents. *)
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
  destination:Contract.t ->
  ?entrypoint:Entrypoint.t ->
  ?arg:string ->
  amount:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  fee_parameter:Injection.fee_parameter ->
  ?replace_by_fees:bool ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract_hash.t list)
  tzresult
  Lwt.t

(** Calls {!Injection.prepare_manager_operation} with [Reveal pk] as [operation] *)
val build_reveal_operation :
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  public_key ->
  Kind.reveal Annotated_manager_operation.t

(** Calls {!Injection.inject_manager_operation}
    with {!Annotated_manager_operation.Single_manager}
    [build_reveal_operation ?fee ~storage_limit:Z.zero src_pk]
    as contents. *)
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
  pkh : Signature.Ed25519.Public_key_hash.t;
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

(** Activate an account, by calling {!Injection.inject_operation}. *)
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

(** Activate an existing account,
    by calling {!Injection.inject_operation} with [activation code].
    It fails if the account is unknown or if the account is not [Signature.Ed25519]. *)
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

(** [get_period_info ~successor cctx ~chain ~block] returns the successor [period_info] if [successor],
    it returns the current [period_info] otherwise.
    This function uses {!Tezos_protocol_alpha.Protocol.Voting_services.successor_period} if [successor],
    otherwise it calls {!Tezos_protocol_alpha.Protocol.Voting_services.current_period}.
    In any case, it also uses {!Tezos_protocol_alpha.Protocol.Voting_services.current_proposal}
*)
val get_period_info :
  ?successor:bool ->
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  period_info tzresult Lwt.t

(**  [get_ballots_info cctx ~chain ~block] returns the [ballots_info].
     It calls {!Tezos_protocol_alpha.Protocol.Voting_services.ballots},
     {!Tezos_protocol_alpha.Protocol.Voting_services.current_quorum}
     and {!Tezos_protocol_alpha.Protocol.Voting_services.total_voting_power}.
*)
val get_ballots_info :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ballots_info tzresult Lwt.t

(** Calls {!Tezos_protocol_alpha.Protocol.Voting_services.proposals} *)
val get_proposals :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Int64.t Environment.Protocol_hash.Map.t tzresult Lwt.t

(** Calls {!Injection.inject_operation}
    where [contents] is {!Alpha_context.Single} [(Proposals {source; period = index; proposals})]
    with [index] the result of {!Alpha_services.Voting.successor_period} *)
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

(** Calls {!Injection.inject_operation}
    where [contents] is {!Alpha_context.Single} [(Ballot {source; period = index; proposals})]
    with [index] the result of {!Alpha_services.Voting.successor_period} *)
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

(** Calls {!Tezos_protocol_alpha.Protocol.Alpha_services.Cache.cached_contracts} *)
val cached_contracts :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  (Contract_hash.t * int) list tzresult Lwt.t

(** Calls {!Tezos_protocol_alpha.Protocol.Alpha_services.Cache.contract_rank} *)
val contract_rank :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract_hash.t ->
  int option tzresult Lwt.t

(** Calls {!Tezos_protocol_alpha.Protocol.Alpha_services.Cache.contract_cache_size} *)
val contract_cache_size :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  int tzresult Lwt.t

(** Calls {!Tezos_protocol_alpha.Protocol.Alpha_services.Cache.contract_cache_size_limit} *)
val contract_cache_size_limit :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  int tzresult Lwt.t

val transfer_ticket :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:Signature.public_key_hash ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  contents:string ->
  ty:string ->
  ticketer:Contract.t ->
  amount:Ticket_amount.t ->
  destination:Contract.t ->
  entrypoint:Entrypoint.t ->
  unit ->
  (Operation_hash.t
  * Kind.transfer_ticket Kind.manager contents
  * Kind.transfer_ticket Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

(** Injects a smart-contract rollup origination operation using
    {!Injection.inject_operation}.

    Originates a smart rollup of [kind] with the [boot_sector] using
    [paramaters_ty]. Generates a unique smart rollup address returned
    in the operation's receipt.

    This is the only entry-point to create a smart rollup.
*)
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
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  ?whitelist:Sc_rollup.Whitelist.t ->
  source:public_key_hash ->
  kind:Sc_rollup.Kind.t ->
  boot_sector:string ->
  parameters_ty:Script.lazy_expr ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Operation_hash.t
  * Kind.sc_rollup_originate Kind.manager contents
  * Kind.sc_rollup_originate Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

(** Injects a smart-contract rollup add messages operation using
    {!Injection.inject_operation}.

    Adds external messages to the smart rollup inbox shared with all
    smart rollups.
*)
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
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:public_key_hash ->
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

(** Injects a smart-contract rollup cement commitment operation using
    {!Injection.inject_operation}.

    Cements a [commitment] for the smart rollup [rollup]. The commitment
    now becomes unrefutable and we can execute outbox messages for the
    committed PVM state (see {!sc_rollup_execute_outbox_message}).
*)
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
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:public_key_hash ->
  rollup:Alpha_context.Sc_rollup.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Operation_hash.t
  * Kind.sc_rollup_cement Kind.manager contents
  * Kind.sc_rollup_cement Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

(** Injects a smart-contract rollup publish commitment operation using
    {!Injection.inject_operation}.

    Publishes a [commitment] to announces the PVM state at the end of
    a commitment period and the number of ticks executed.
    If it is the first time [src_pk] publishes a commitment, a bond
    is frozen.
*)
val sc_rollup_publish :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:public_key_hash ->
  rollup:Alpha_context.Sc_rollup.t ->
  commitment:Alpha_context.Sc_rollup.Commitment.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Operation_hash.t
  * Kind.sc_rollup_publish Kind.manager contents
  * Kind.sc_rollup_publish Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

(** Injects a smart-contract rollup execute outbox message operation using
    {!Injection.inject_operation}.

    Executes [output_proof] on the PVM commited state from the
    [cemented_commitment]. Allows to perform L2->L1 communication.
*)
val sc_rollup_execute_outbox_message :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:public_key_hash ->
  rollup:Sc_rollup.t ->
  cemented_commitment:Sc_rollup.Commitment.Hash.t ->
  output_proof:string ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Operation_hash.t
  * Kind.sc_rollup_execute_outbox_message Kind.manager contents
  * Kind.sc_rollup_execute_outbox_message Kind.manager
    Apply_results.contents_result)
  tzresult
  Lwt.t

(** Injects a smart-contract rollup recover bond operation using
    {!Injection.inject_operation}.

    Allows to recover the bond frozen by the operation {!sc_rollup_publish}
    of [src_pk], if the commitment is no longer subject to refutations.
*)
val sc_rollup_recover_bond :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.tez ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:Signature.public_key_hash ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  sc_rollup:Sc_rollup.t ->
  staker:public_key_hash ->
  unit ->
  (Operation_hash.t
  * Kind.sc_rollup_recover_bond Kind.manager contents
  * Kind.sc_rollup_recover_bond Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

(** Injects a smart-contract rollup refutation operation using
    {!Injection.inject_operation}.

    Either start a refutation game between [src_pk] and [oppononent] or
    plays a move in an existing refutation game.
*)
val sc_rollup_refute :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:public_key_hash ->
  rollup:Alpha_context.Sc_rollup.t ->
  refutation:Alpha_context.Sc_rollup.Game.refutation ->
  opponent:Alpha_context.Sc_rollup.Staker.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Operation_hash.t
  * Kind.sc_rollup_refute Kind.manager contents
  * Kind.sc_rollup_refute Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

(** Injects a smart-contract rollup timeout operation using
    {!Injection.inject_operation}.

    Timeouts the absent player from the refutation game between [alice]
    and [bob].
*)
val sc_rollup_timeout :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:public_key_hash ->
  rollup:Alpha_context.Sc_rollup.t ->
  alice:Alpha_context.Sc_rollup.Staker.t ->
  bob:Alpha_context.Sc_rollup.Staker.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Operation_hash.t
  * Kind.sc_rollup_timeout Kind.manager contents
  * Kind.sc_rollup_timeout Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t

val zk_rollup_originate :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:public_key_hash ->
  public_parameters:Environment.Plonk.public_parameters ->
  circuits_info:[`Fee | `Private | `Public] Zk_rollup.Account.SMap.t ->
  init_state:Bls12_381.Fr.t array ->
  nb_ops:int ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  ( Operation_hash.t
    * Kind.zk_rollup_origination Kind.manager contents
    * Kind.zk_rollup_origination Kind.manager Apply_results.contents_result,
    tztrace )
  result
  Lwt.t

val zk_rollup_publish :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:public_key_hash ->
  zk_rollup:Zk_rollup.t ->
  ops:(Zk_rollup.Operation.t * Zk_rollup.Ticket.t option) list ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  ( Operation_hash.t
    * Kind.zk_rollup_publish Kind.manager contents
    * Kind.zk_rollup_publish Kind.manager Apply_results.contents_result,
    tztrace )
  result
  Lwt.t

val zk_rollup_update :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:public_key_hash ->
  zk_rollup:Zk_rollup.t ->
  update:Zk_rollup.Update.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  ( Operation_hash.t
    * Kind.zk_rollup_update Kind.manager contents
    * Kind.zk_rollup_update Kind.manager Apply_results.contents_result,
    tztrace )
  result
  Lwt.t

(** Injects a DAL publish commitment operation using
    {!Injection.inject_operation}.
*)
val dal_publish :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  source:public_key_hash ->
  slot_header:Alpha_context.Dal.Operations.Publish_slot_header.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Operation_hash.t
  * Kind.dal_publish_slot_header Kind.manager contents
  * Kind.dal_publish_slot_header Kind.manager Apply_results.contents_result)
  tzresult
  Lwt.t
