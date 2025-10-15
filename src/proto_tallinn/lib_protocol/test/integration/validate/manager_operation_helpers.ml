(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
open Tez_helpers

(** {2 Constants} *)

(** Hard gas limit *)

let gb_limit = Gas.Arith.(integral_of_int_exn 100_000)

let half_gb_limit = Gas.Arith.(integral_of_int_exn 50_000)

let default_fund = Tez.of_mutez_exn 400_000_000_000L

(** {2 Datatypes} *)

(** Context abstraction in a test. *)
type ctxt = {
  block : Block.t;
  bootstraps : public_key_hash list;
  originated_contract : Contract_hash.t option;
  sc_rollup : Sc_rollup.t option;
  zk_rollup : Zk_rollup.t option;
}

(** Accounts manipulated in the tests. By convention, each field name
   specifies the role of the account in a test. It is the case in most
   of the tests. In operations smart contructors, it happens that in
   impossible case, [source] is used as a dummy value. In some test that
   requires a second source, [del] will be used as the second source. *)
type accounts = {
  sources : Account.t list;
  dest : Account.t option;
  del : Account.t option;
  sc : Account.t option;
  zk : Account.t option;
}

(** Feature flags requirements for a context setting for a test. *)
type feature_flags = {dal : bool; scoru_arith : bool; zkru : bool}

(** Infos describes the information of the setting for a test: the
   context and used accounts. *)
type infos = {ctxt : ctxt; accounts : accounts; flags : feature_flags}

(** This type should be extended for each new manager_operation kind
    added in the protocol. See
    [test_manager_operation_validation.ensure_kind] for more
    information on how we ensure that this type is extended for each
    new manager_operation kind. *)
type manager_operation_kind =
  | K_Transaction
  | K_Origination
  | K_Register_global_constant
  | K_Delegation
  | K_Undelegation
  | K_Self_delegation
  | K_Set_deposits_limit
  | K_Update_consensus_key
  | K_Increase_paid_storage
  | K_Reveal
  | K_Transfer_ticket
  | K_Sc_rollup_origination
  | K_Sc_rollup_publish
  | K_Sc_rollup_cement
  | K_Sc_rollup_add_messages
  | K_Sc_rollup_refute
  | K_Sc_rollup_timeout
  | K_Sc_rollup_execute_outbox_message
  | K_Sc_rollup_recover_bond
  | K_Dal_publish_commitment
  | K_Zk_rollup_origination
  | K_Zk_rollup_publish
  | K_Zk_rollup_update

(** The requirements for a tested manager operation. *)
type operation_req = {
  kind : manager_operation_kind;
  counter : Manager_counter.t option;
  fee : Tez.t option;
  gas_limit : Op.gas_limit option;
  storage_limit : Z.t option;
  force_reveal : bool option;
  amount : Tez.t option;
}

(** The requirements for a context setting for a test. *)
type ctxt_req = {
  hard_gas_limit_per_block : Gas.Arith.integral option;
  fund_src : Tez.t option;
  fund_dest : Tez.t option;
  fund_del : Tez.t option;
  reveal_accounts : bool;
  fund_sc : Tez.t option;
  fund_zk : Tez.t option;
  flags : feature_flags;
}

(** Validation mode.

   FIXME: https://gitlab.com/tezos/tezos/-/issues/3365
   This type should be replaced by the one defined
   in validation, type mode in `validate_operation`, when it would
   include the distinction between Contruction and Application. *)
type mode = Construction | Mempool | Application

(** {2 Default values} *)
let all_enabled = {dal = true; scoru_arith = true; zkru = true}

let disabled_dal = {all_enabled with dal = false}

let disabled_scoru_arith = {all_enabled with scoru_arith = false}

let disabled_zkru = {all_enabled with zkru = false}

let ctxt_req_default_to_flag flags =
  {
    hard_gas_limit_per_block = None;
    fund_src = Some default_fund;
    fund_dest = Some Tez.one;
    fund_del = Some default_fund;
    reveal_accounts = true;
    fund_sc = Some Tez.one;
    fund_zk = Some Tez.one;
    flags;
  }

let ctxt_req_default = ctxt_req_default_to_flag all_enabled

let operation_req_default kind =
  {
    kind;
    counter = None;
    fee = None;
    gas_limit = None;
    storage_limit = None;
    force_reveal = None;
    amount = None;
  }

(** {2 String_of data} *)
let kind_to_string = function
  | K_Transaction -> "Transaction"
  | K_Delegation -> "Delegation"
  | K_Undelegation -> "Undelegation"
  | K_Self_delegation -> "Self-delegation"
  | K_Set_deposits_limit -> "Set deposits limit"
  | K_Update_consensus_key -> "Update consensus key"
  | K_Origination -> "Origination"
  | K_Register_global_constant -> "Register global constant"
  | K_Increase_paid_storage -> "Increase paid storage"
  | K_Reveal -> "Revelation"
  | K_Transfer_ticket -> "Transfer_ticket"
  | K_Sc_rollup_origination -> "Sc_rollup_origination"
  | K_Sc_rollup_publish -> "Sc_rollup_publish"
  | K_Sc_rollup_cement -> "Sc_rollup_cement"
  | K_Sc_rollup_timeout -> "Sc_rollup_timeout"
  | K_Sc_rollup_refute -> "Sc_rollup_refute"
  | K_Sc_rollup_add_messages -> "Sc_rollup_add_messages"
  | K_Sc_rollup_execute_outbox_message -> "Sc_rollup_execute_outbox_message"
  | K_Sc_rollup_recover_bond -> "Sc_rollup_recover_bond"
  | K_Dal_publish_commitment -> "Dal_publish_commitment"
  | K_Zk_rollup_origination -> "Zk_rollup_origination"
  | K_Zk_rollup_publish -> "Zk_rollup_publish"
  | K_Zk_rollup_update -> "Zk_rollup_update"

(** {2 Pretty-printers} *)
let pp_opt pp v =
  let open Format in
  pp_print_option ~none:(fun fmt () -> fprintf fmt "None") pp v

let pp_operation_req pp
    {kind; counter; fee; gas_limit; storage_limit; force_reveal; amount} =
  Format.fprintf
    pp
    "@[<v 4>Operation_req:@,\
     kind: %s@,\
     counter: %a@,\
     fee: %a@,\
     gas_limit: %a@,\
     storage_limit: %a@,\
     force_reveal: %a@,\
     amount: %a@,\
     @]"
    (kind_to_string kind)
    (pp_opt Manager_counter.pp)
    counter
    (pp_opt Tez.pp)
    fee
    (pp_opt Op.pp_gas_limit)
    gas_limit
    (pp_opt Z.pp_print)
    storage_limit
    (pp_opt (fun fmt -> Format.fprintf fmt "%b"))
    force_reveal
    (pp_opt Tez.pp)
    amount

let pp_2_operation_req pp (op_req1, op_req2) =
  Format.fprintf
    pp
    "[<v 4> %a,@ and %a,@ @]"
    pp_operation_req
    op_req1
    pp_operation_req
    op_req2

let pp_ctxt_req pp
    {
      hard_gas_limit_per_block;
      fund_src;
      fund_dest;
      fund_del;
      reveal_accounts;
      fund_sc;
      fund_zk;
      flags;
    } =
  Format.fprintf
    pp
    "@[<v 4>Ctxt_req:@,\
     hard_gas_limit_per_block:%a@,\
     fund_src: %a tz@,\
     fund_dest: %a tz@,\
     fund_del: %a tz@,\
     reveal_accounts: %b tz@,\
     fund_sc: %a tz@,\
     fund_zk: %a tz@,\
     dal_flag: %a@,\
     scoru_arith_flag: %a@,\
     zkru_flag: %a@,\
     @]"
    (pp_opt Gas.Arith.pp_integral)
    hard_gas_limit_per_block
    (pp_opt Tez.pp)
    fund_src
    (pp_opt Tez.pp)
    fund_dest
    (pp_opt Tez.pp)
    fund_del
    reveal_accounts
    (pp_opt Tez.pp)
    fund_sc
    (pp_opt Tez.pp)
    fund_zk
    Format.pp_print_bool
    flags.dal
    Format.pp_print_bool
    flags.scoru_arith
    Format.pp_print_bool
    flags.zkru

let pp_mode pp = function
  | Construction -> Format.fprintf pp "Construction"
  | Mempool -> Format.fprintf pp "Mempool"
  | Application -> Format.fprintf pp "Block"

(** {2 Short-cuts} *)
let contract_of (account : Account.t) = Contract.Implicit account.pkh

(** Make a [mempool_mode], aka a boolean, as used in incremental from
   a [mode]. *)
let mempool_mode_of = function Mempool -> true | _ -> false

let get_pk infos source =
  let open Lwt_result_syntax in
  let+ account = Context.Contract.manager infos source in
  account.pk

(** Operation for specific context.  *)
let self_delegate block pkh =
  let open Lwt_result_syntax in
  let contract = Contract.Implicit pkh in
  let* operation =
    Op.delegation ~force_reveal:true (B block) contract (Some pkh)
  in
  let* block = Block.bake block ~operation in
  let* del_opt_new = Context.Contract.delegate_opt (B block) contract in
  let* del = Assert.get_some ~loc:__LOC__ del_opt_new in
  let+ () = Assert.equal_pkh ~loc:__LOC__ del pkh in
  block

let delegation block source delegate =
  let open Lwt_result_syntax in
  let delegate_pkh = delegate.Account.pkh in
  let contract_source = contract_of source in
  let* operation =
    Op.delegation
      ~force_reveal:true
      (B block)
      contract_source
      (Some delegate_pkh)
  in
  let* block = Block.bake block ~operation in
  let* del_opt_new = Context.Contract.delegate_opt (B block) contract_source in
  let* del = Assert.get_some ~loc:__LOC__ del_opt_new in
  let* () = Assert.equal_pkh ~loc:__LOC__ del delegate_pkh in
  return block

let originate_sc_rollup block rollup_account =
  let open Lwt_result_syntax in
  let rollup_contract = contract_of rollup_account in
  let kind = Sc_rollup.Kind.Example_arith in
  let* rollup_origination, sc_rollup =
    Sc_rollup_helpers.origination_op
      ~force_reveal:true
      (B block)
      rollup_contract
      kind
  in
  let+ block =
    Block.bake ~allow_manager_failures:true ~operation:rollup_origination block
  in
  (block, sc_rollup)

module ZKOperator = Dummy_zk_rollup.Operator (struct
  let batch_size = 10
end)

let originate_zk_rollup block rollup_account =
  let open Lwt_result_syntax in
  let rollup_contract = contract_of rollup_account in
  let _prover_pp, public_parameters = Lazy.force ZKOperator.lazy_pp in
  let* rollup_origination, zk_rollup =
    Op.zk_rollup_origination
      ~force_reveal:true
      (B block)
      rollup_contract
      ~public_parameters
      ~circuits_info:
        (Zk_rollup.Account.SMap.of_seq @@ Kzg.SMap.to_seq ZKOperator.circuits)
      ~init_state:ZKOperator.init_state
      ~nb_ops:1
  in
  let+ block =
    Block.bake ~allow_manager_failures:true ~operation:rollup_origination block
  in
  (block, zk_rollup)

(** {2 Setting's context construction} *)

let fund_account_op block bootstrap account fund counter =
  let open Lwt_result_wrap_syntax in
  let* fund =
    match fund with
    | None -> return Tez.one
    | Some fund ->
        let* source_balance = Context.Contract.balance (B block) bootstrap in
        if Tez.(fund > source_balance) then
          let*?@ result = Tez.(source_balance -? one) in
          return result
        else return fund
  in
  let+ op =
    Op.transaction
      ~counter
      ~gas_limit:Op.High
      (B block)
      bootstrap
      (Contract.Implicit account)
      fund
  in
  (op, Manager_counter.succ counter)

let fund_account block bootstrap account fund =
  let open Lwt_result_syntax in
  let* counter = Context.Contract.counter (B block) bootstrap in
  let* operation, (_counter : Manager_counter.t) =
    fund_account_op block bootstrap account fund counter
  in
  let*! b = Block.bake ~operation block in
  match b with Error _ -> failwith "Funding account error" | Ok b -> return b

(** Set the constants according to a [ctxt_req] in an existing parameters. *)
let manager_parameters : Parameters.t -> ctxt_req -> Parameters.t =
 fun params {hard_gas_limit_per_block; flags; _} ->
  let hard_gas_limit_per_block =
    match hard_gas_limit_per_block with
    | Some gb -> gb
    | None -> Default_parameters.constants_mainnet.hard_gas_limit_per_block
  in
  let dal = {params.constants.dal with feature_enable = flags.dal} in
  let sc_rollup =
    {params.constants.sc_rollup with arith_pvm_enable = flags.scoru_arith}
  in
  let zk_rollup = {params.constants.zk_rollup with enable = flags.zkru} in
  let constants =
    {params.constants with hard_gas_limit_per_block; dal; zk_rollup; sc_rollup}
  in
  {params with constants}

(** Initialize a context with the constants extracted from a context requirements
    and 7 bootstrap accounts. *)
let init_ctxt_only ctxtreq =
  let open Lwt_result_syntax in
  let initial_params =
    Tezos_protocol_alpha_parameters.Default_parameters.parameters_of_constants
      {Context.default_test_constants with consensus_threshold_size = 0}
  in
  let* _cryptobox =
    Dal_helpers.mk_cryptobox initial_params.constants.dal.cryptobox_parameters
  in
  let* block, contracts =
    Context.init_with_parameters_n (manager_parameters initial_params ctxtreq) 7
  in
  return
    ( block,
      List.map
        (function Contract.Implicit pkh -> pkh | Originated _ -> assert false)
        contracts )

(** Build a generic setting for a test according to a context requirement
    on an existing context with 7 bootstraps accounts. *)
let init_infos :
    ctxt_req -> Block.t -> public_key_hash list -> infos tzresult Lwt.t =
 fun ctxtreq block bootstraps ->
  let {
    fund_src;
    fund_dest;
    fund_del;
    fund_sc;
    fund_zk;
    flags;
    reveal_accounts;
    _;
  } =
    ctxtreq
  in
  let open Lwt_result_syntax in
  let create_and_fund ?originate_rollup block bootstrap fund =
    match fund with
    | None -> return (block, None, None)
    | Some _ ->
        let account = Account.new_account () in
        let* block = fund_account block bootstrap account.pkh fund in
        let* block, rollup =
          match originate_rollup with
          | None -> return (block, None)
          | Some f ->
              let+ block, rollup = f block account in
              (block, Some rollup)
        in
        return (block, Some account, rollup)
  in
  let reveal_accounts_operations b l =
    List.filter_map_es
      (function
        | None -> return_none
        | Some account ->
            let* op = Op.revelation ~gas_limit:Low (B b) account.Account.pk in
            return_some op)
      l
  in
  let get_bootstrap bootstraps n =
    Contract.Implicit (Stdlib.List.nth bootstraps n)
  in
  let source = Account.new_account () in
  let* block =
    fund_account block (get_bootstrap bootstraps 0) source.pkh fund_src
  in
  let* block, dest, _ =
    create_and_fund block (get_bootstrap bootstraps 1) fund_dest
  in
  let* block, del, _ =
    create_and_fund block (get_bootstrap bootstraps 2) fund_del
  in
  let* block, sc, sc_rollup =
    if flags.scoru_arith then
      create_and_fund
        ~originate_rollup:originate_sc_rollup
        block
        (get_bootstrap bootstraps 4)
        fund_sc
    else return (block, None, None)
  in
  let* block, zk, zk_rollup =
    if flags.zkru then
      create_and_fund
        ~originate_rollup:originate_zk_rollup
        block
        (get_bootstrap bootstraps 5)
        fund_zk
    else return (block, None, None)
  in
  let* create_contract_hash, originated_contract =
    Op.contract_origination_hash
      (B block)
      (get_bootstrap bootstraps 6)
      ~fee:Tez.zero
      ~script:Op.dummy_script
  in
  let* reveal_operations =
    if reveal_accounts then
      reveal_accounts_operations block [Some source; dest; del]
    else return_nil
  in
  let operations = create_contract_hash :: reveal_operations in
  let+ block = Block.bake ~operations block in
  let ctxt =
    {
      block;
      bootstraps;
      originated_contract = Some originated_contract;
      sc_rollup;
      zk_rollup;
    }
  in
  {ctxt; accounts = {sources = [source]; dest; del; sc; zk}; flags}

(** The generic setting for a test is built up according to a context
   requirement. It provides a context and accounts where the accounts
   have been created and funded according to the context
   requirements.*)
let init_ctxt : ctxt_req -> infos tzresult Lwt.t =
 fun ctxtreq ->
  let open Lwt_result_syntax in
  let* block, bootstraps = init_ctxt_only ctxtreq in
  init_infos ctxtreq block bootstraps

(** return the first source from the list of sources in [infos] accounts. *)
let get_source infos =
  match infos.accounts.sources with source :: _ -> source | [] -> assert false

(** In addition of building up a context according to a context
    requirement, source is self-delegated.

   see [init_ctxt] description. *)
let ctxt_with_self_delegation : ctxt_req -> infos tzresult Lwt.t =
 fun ctxt_req ->
  let open Lwt_result_syntax in
  let* infos = init_ctxt ctxt_req in
  let+ block = self_delegate infos.ctxt.block (get_source infos).pkh in
  let ctxt = {infos.ctxt with block} in
  {infos with ctxt}

(** In addition of building up a context accordning to a context
    requirement, source delegates to del.

   See [init_ctxt] description. *)
let ctxt_with_delegation : ctxt_req -> infos tzresult Lwt.t =
 fun ctxt_req ->
  let open Lwt_result_syntax in
  let* infos = init_ctxt ctxt_req in
  let* delegate =
    match infos.accounts.del with
    | None -> failwith "Delegate account should be funded"
    | Some a -> return a
  in
  let* block = delegation infos.ctxt.block delegate delegate in
  let+ block = delegation block (get_source infos) delegate in
  let ctxt = {infos.ctxt with block} in
  {infos with ctxt}

let default_init_ctxt () = init_ctxt ctxt_req_default

let default_init_with_flags flags = init_ctxt (ctxt_req_default_to_flag flags)

let default_ctxt_with_self_delegation () =
  ctxt_with_self_delegation ctxt_req_default

let default_ctxt_with_delegation () = ctxt_with_delegation ctxt_req_default

(** {2 Smart constructors} *)

(** Smart constructors to forge manager operations according to
   operation requirements in a test setting. *)

let mk_transaction (oinfos : operation_req) (infos : infos) =
  Op.transaction
    ?force_reveal:oinfos.force_reveal
    ?counter:oinfos.counter
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?storage_limit:oinfos.storage_limit
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    (contract_of
       (match infos.accounts.dest with
       | None -> get_source infos
       | Some dest -> dest))
    (match oinfos.amount with None -> Tez.zero | Some amount -> amount)

let mk_delegation (oinfos : operation_req) (infos : infos) =
  Op.delegation
    ?force_reveal:oinfos.force_reveal
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    (Some
       (match infos.accounts.del with
       | None -> (get_source infos).pkh
       | Some delegate -> delegate.pkh))

let mk_undelegation (oinfos : operation_req) (infos : infos) =
  Op.delegation
    ?force_reveal:oinfos.force_reveal
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    None

let mk_self_delegation (oinfos : operation_req) (infos : infos) =
  Op.delegation
    ?force_reveal:oinfos.force_reveal
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    (Some (get_source infos).pkh)

let mk_origination (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let+ op, _ =
    Op.contract_origination
      ?force_reveal:oinfos.force_reveal
      ?counter:oinfos.counter
      ?fee:oinfos.fee
      ?gas_limit:oinfos.gas_limit
      ?storage_limit:oinfos.storage_limit
      ~script:Op.dummy_script
      (B infos.ctxt.block)
      (contract_of (get_source infos))
  in
  op

let mk_register_global_constant (oinfos : operation_req) (infos : infos) =
  Op.register_global_constant
    ?force_reveal:oinfos.force_reveal
    ?counter:oinfos.counter
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?storage_limit:oinfos.storage_limit
    (B infos.ctxt.block)
    ~source:(contract_of (get_source infos))
    ~value:(Script_repr.lazy_expr (Expr.from_string "Pair 1 2"))

let mk_set_deposits_limit (oinfos : operation_req) (infos : infos) =
  Op.set_deposits_limit
    ?force_reveal:oinfos.force_reveal
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?storage_limit:oinfos.storage_limit
    ?counter:oinfos.counter
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    None

let mk_update_consensus_key (oinfos : operation_req) (infos : infos) =
  Op.update_consensus_key
    ?force_reveal:oinfos.force_reveal
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?storage_limit:oinfos.storage_limit
    ?counter:oinfos.counter
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    (match infos.accounts.dest with
    | None -> (get_source infos).pk
    | Some dest -> dest.pk)

let mk_increase_paid_storage (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let* destination =
    match infos.ctxt.originated_contract with
    | None ->
        failwith
          "infos should be initialized with an origniated contract to be able \
           to add an increase_paid_storage operation."
    | Some c -> return c
  in
  Op.increase_paid_storage
    ?force_reveal:oinfos.force_reveal
    ?counter:oinfos.counter
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?storage_limit:oinfos.storage_limit
    (B infos.ctxt.block)
    ~source:(contract_of (get_source infos))
    ~destination
    Z.one

let mk_reveal (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let* pk = get_pk (B infos.ctxt.block) (contract_of (get_source infos)) in
  Op.revelation
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    (B infos.ctxt.block)
    pk

let sc_rollup_of =
  let open Lwt_result_syntax in
  function
  | Some sc_rollup -> return sc_rollup
  | None -> failwith "Sc_rollup not created in this context"

let zk_rollup_of =
  let open Lwt_result_syntax in
  function
  | Some zk_rollup -> return zk_rollup
  | None -> failwith "Zk_rollup not created in this context"

let mk_transfer_ticket (oinfos : operation_req) (infos : infos) =
  Op.transfer_ticket
    ?fee:oinfos.fee
    ?force_reveal:oinfos.force_reveal
    ?counter:oinfos.counter
    ?gas_limit:oinfos.gas_limit
    ?storage_limit:oinfos.storage_limit
    (B infos.ctxt.block)
    ~source:(contract_of (get_source infos))
    ~contents:(Script.lazy_expr (Expr.from_string "1"))
    ~ty:(Script.lazy_expr (Expr.from_string "nat"))
    ~ticketer:
      (contract_of
         (match infos.accounts.sc with
         | None -> get_source infos
         | Some tx -> tx))
    ~amount:Ticket_amount.one
    ~destination:
      (contract_of
         (match infos.accounts.dest with
         | None -> get_source infos
         | Some dest -> dest))
    ~entrypoint:Entrypoint.default

let mk_sc_rollup_origination (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let+ op, _ =
    let kind = Sc_rollup.Kind.Example_arith in
    Sc_rollup_helpers.origination_op
      ?fee:oinfos.fee
      ?gas_limit:oinfos.gas_limit
      ?counter:oinfos.counter
      ?storage_limit:oinfos.storage_limit
      ?force_reveal:oinfos.force_reveal
      (B infos.ctxt.block)
      (contract_of (get_source infos))
      kind
  in
  op

let sc_dummy_commitment =
  let number_of_ticks =
    match Sc_rollup.Number_of_ticks.of_value 3000L with
    | None -> assert false
    | Some x -> x
  in
  Sc_rollup.Commitment.
    {
      predecessor = Sc_rollup.Commitment.Hash.zero;
      inbox_level = Raw_level.of_int32_exn Int32.zero;
      number_of_ticks;
      compressed_state = Sc_rollup.State_hash.zero;
    }

let mk_sc_rollup_publish (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let* sc_rollup = sc_rollup_of infos.ctxt.sc_rollup in
  Op.sc_rollup_publish
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    ?force_reveal:oinfos.force_reveal
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    sc_rollup
    sc_dummy_commitment

let mk_sc_rollup_cement (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let* sc_rollup = sc_rollup_of infos.ctxt.sc_rollup in
  Op.sc_rollup_cement
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    ?force_reveal:oinfos.force_reveal
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    sc_rollup

let mk_sc_rollup_refute (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let* sc_rollup = sc_rollup_of infos.ctxt.sc_rollup in
  let refutation : Sc_rollup.Game.refutation =
    Move {choice = Sc_rollup.Tick.initial; step = Dissection []}
  in
  Op.sc_rollup_refute
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    ?force_reveal:oinfos.force_reveal
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    sc_rollup
    (match infos.accounts.dest with
    | None -> (get_source infos).pkh
    | Some dest -> dest.pkh)
    refutation

let mk_sc_rollup_add_messages (oinfos : operation_req) (infos : infos) =
  Op.sc_rollup_add_messages
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    ?force_reveal:oinfos.force_reveal
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    [""]

let mk_sc_rollup_timeout (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let* sc_rollup = sc_rollup_of infos.ctxt.sc_rollup in
  Op.sc_rollup_timeout
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    ?force_reveal:oinfos.force_reveal
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    sc_rollup
    (Sc_rollup.Game.Index.make
       (get_source infos).pkh
       (match infos.accounts.dest with
       | None -> (get_source infos).pkh
       | Some dest -> dest.pkh))

let mk_sc_rollup_execute_outbox_message (oinfos : operation_req) (infos : infos)
    =
  let open Lwt_result_syntax in
  let* sc_rollup = sc_rollup_of infos.ctxt.sc_rollup in
  Op.sc_rollup_execute_outbox_message
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    ?force_reveal:oinfos.force_reveal
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    sc_rollup
    (Sc_rollup.Commitment.hash_uncarbonated sc_dummy_commitment)
    ~output_proof:""

let mk_sc_rollup_return_bond (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let* sc_rollup = sc_rollup_of infos.ctxt.sc_rollup in
  let source, staker =
    match contract_of (get_source infos) with
    | Implicit staker as source -> (source, staker)
    | _ -> assert false
  in
  Op.sc_rollup_recover_bond
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    ?force_reveal:oinfos.force_reveal
    (B infos.ctxt.block)
    source
    sc_rollup
    staker

let mk_dal_publish_commitment (oinfos : operation_req) (infos : infos) =
  let slot_index = Alpha_context.Dal.Slot_index.zero in
  let commitment = Alpha_context.Dal.Slot.Commitment.zero in
  let commitment_proof = Alpha_context.Dal.Slot.Commitment_proof.zero in
  let slot =
    Dal.Operations.Publish_commitment.{slot_index; commitment; commitment_proof}
  in
  Op.dal_publish_commitment
    ?fee:oinfos.fee
    ?gas_limit:oinfos.gas_limit
    ?counter:oinfos.counter
    ?storage_limit:oinfos.storage_limit
    ?force_reveal:oinfos.force_reveal
    (B infos.ctxt.block)
    (contract_of (get_source infos))
    slot

let mk_zk_rollup_origination (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let _prover_pp, public_parameters = Lazy.force ZKOperator.lazy_pp in
  let* op, _ =
    Op.zk_rollup_origination
      ?fee:oinfos.fee
      ?gas_limit:oinfos.gas_limit
      ?counter:oinfos.counter
      ?storage_limit:oinfos.storage_limit
      ?force_reveal:oinfos.force_reveal
      (B infos.ctxt.block)
      (contract_of (get_source infos))
      ~public_parameters
      ~circuits_info:
        (Zk_rollup.Account.SMap.of_seq @@ Kzg.SMap.to_seq ZKOperator.circuits)
      ~init_state:ZKOperator.init_state
      ~nb_ops:1
  in
  return op

let mk_zk_rollup_publish (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let open Zk_rollup.Operation in
  let* zk_rollup = zk_rollup_of infos.ctxt.zk_rollup in
  let l2_op =
    {ZKOperator.Internal_for_tests.false_op with rollup_id = zk_rollup}
  in
  let* op =
    Op.zk_rollup_publish
      ?fee:oinfos.fee
      ?gas_limit:oinfos.gas_limit
      ?counter:oinfos.counter
      ?storage_limit:oinfos.storage_limit
      ?force_reveal:oinfos.force_reveal
      (B infos.ctxt.block)
      (contract_of (get_source infos))
      ~zk_rollup
      ~ops:[(l2_op, None)]
  in
  return op

let mk_zk_rollup_update (oinfos : operation_req) (infos : infos) =
  let open Lwt_result_syntax in
  let* zk_rollup = zk_rollup_of infos.ctxt.zk_rollup in
  let update = Lazy.force ZKOperator.Internal_for_tests.lazy_update_data in
  let* op =
    Op.zk_rollup_update
      ?fee:oinfos.fee
      ?gas_limit:oinfos.gas_limit
      ?counter:oinfos.counter
      ?storage_limit:oinfos.storage_limit
      ?force_reveal:oinfos.force_reveal
      (B infos.ctxt.block)
      (contract_of (get_source infos))
      ~zk_rollup
      ~update
  in
  return op

(** {2 Helpers for generation of generic check tests by manager operation} *)

(** Generic forge for any kind of manager operation according to
   operation requirements in a specific test setting. *)
let select_op (op_req : operation_req) (infos : infos) =
  let mk_op =
    match op_req.kind with
    | K_Transaction -> mk_transaction
    | K_Origination -> mk_origination
    | K_Register_global_constant -> mk_register_global_constant
    | K_Delegation -> mk_delegation
    | K_Undelegation -> mk_undelegation
    | K_Self_delegation -> mk_self_delegation
    | K_Set_deposits_limit -> mk_set_deposits_limit
    | K_Update_consensus_key -> mk_update_consensus_key
    | K_Increase_paid_storage -> mk_increase_paid_storage
    | K_Reveal -> mk_reveal
    | K_Transfer_ticket -> mk_transfer_ticket
    | K_Sc_rollup_origination -> mk_sc_rollup_origination
    | K_Sc_rollup_publish -> mk_sc_rollup_publish
    | K_Sc_rollup_cement -> mk_sc_rollup_cement
    | K_Sc_rollup_refute -> mk_sc_rollup_refute
    | K_Sc_rollup_add_messages -> mk_sc_rollup_add_messages
    | K_Sc_rollup_timeout -> mk_sc_rollup_timeout
    | K_Sc_rollup_execute_outbox_message -> mk_sc_rollup_execute_outbox_message
    | K_Sc_rollup_recover_bond -> mk_sc_rollup_return_bond
    | K_Dal_publish_commitment -> mk_dal_publish_commitment
    | K_Zk_rollup_origination -> mk_zk_rollup_origination
    | K_Zk_rollup_publish -> mk_zk_rollup_publish
    | K_Zk_rollup_update -> mk_zk_rollup_update
  in
  mk_op op_req infos

let make_test
    ~(register_test :
       title:string ->
       ?additional_tags:string trace ->
       ?slow:bool ->
       (unit -> (unit, tztrace) result Lwt.t) ->
       unit) name test (subjects : manager_operation_kind list) info_builder =
  List.iter
    (fun kind ->
      let title = sf "%s: %s" name (kind_to_string kind) in
      register_test ~title @@ fun () ->
      let open Lwt_syntax in
      let* infos = info_builder () in
      let infos =
        match infos with
        | Error errs ->
            Tezt.Test.fail "Error: %a" Error_monad.pp_print_trace errs
        | Ok infos -> infos
      in
      test infos kind)
    subjects

let make_test_batched
    ~(register_test :
       title:string ->
       ?additional_tags:string trace ->
       ?slow:bool ->
       (unit -> (unit, tztrace) result Lwt.t) ->
       unit) name test subjects info_builder =
  List.iter
    (fun kind1 ->
      let k1s = kind_to_string kind1 in
      List.iter
        (fun kind2 ->
          let title = sf "%s: [%s ; %s]" name k1s (kind_to_string kind2) in
          register_test ~title @@ fun () ->
          let* infos = info_builder () in
          let infos =
            match infos with
            | Error errs ->
                Tezt.Test.fail "Error: %a" Error_monad.pp_print_trace errs
            | Ok infos -> infos
          in
          test infos kind1 kind2)
        subjects)
    subjects

(** {2 Diagnostic helpers.} *)

(** The purpose of diagnostic helpers is to state the correct
   observation according to the validate result of a test. *)

(** For a manager operation a [probes] contains the values required
   for observing its validate success. Its source, fees (sum for a
   batch), gas_limit (sum of gas_limit of the batch), and the
   increment of the counters aka 1 for a single operation, n for a
   batch of n manager operations. *)
type probes = {
  source : Signature.Public_key_hash.t;
  fee : Tez.t;
  gas_limit : Gas.Arith.integral;
  nb_counter : int;
}

let rec contents_infos : type kind.
    kind Kind.manager contents_list -> probes tzresult Lwt.t =
 fun op ->
  let open Lwt_result_syntax in
  match op with
  | Single (Manager_operation {source; fee; gas_limit; _}) ->
      return {source; fee; gas_limit; nb_counter = 1}
  | Cons (Manager_operation manop, manops) ->
      let* probes = contents_infos manops in
      let*? fee = manop.fee +? probes.fee in
      let gas_limit = Gas.Arith.add probes.gas_limit manop.gas_limit in
      let nb_counter = succ probes.nb_counter in
      let* () = Assert.equal_pkh ~loc:__LOC__ manop.source probes.source in
      return {fee; source = probes.source; gas_limit; nb_counter}

(** Computes a [probes] from a list of manager contents. *)
let manager_content_infos op =
  let (Operation_data {contents; _}) = op.protocol_data in
  match contents with
  | Single (Manager_operation _) as op -> contents_infos op
  | Cons (Manager_operation _, _) as op -> contents_infos op
  | _ -> failwith "Should only handle manager operation"

(** We need a way to get the available gas in a context of type
   block. *)
let available_gas = function
  | Context.I inc -> Some (Gas.block_level (Incremental.alpha_ctxt inc))
  | B _ -> None

(** Computes the witness value in a state. The witness values are the
    the initial balance of source, its initial counter and the
    available gas in the state. The available gas is computed only
    when the context is an incremental one. *)
let witness ctxt source =
  let open Lwt_result_syntax in
  let* b_in = Context.Contract.balance ctxt source in
  let* c_in = Context.Contract.counter ctxt source in
  let g_in = available_gas ctxt in
  return (b_in, c_in, g_in)

(** According to the witness in pre-state and the probes, computes the
    expected outputs. In any mode, when the source is not deallocated,
    the expected witness:
    - the balance of source should be the one in the pre-state minus
    the fee of probes,
    - the counter of source should be the one in the pre-state plus
    the number of counter in probes.

    Concerning the expected available gas in the block:
    - In [Application] mode, it cannot be computed, so we do not expect
    any,
    - In [Mempool] mode, it is the remaining gas after removing the gas
    of probes gas from an empty block,
    - In the [Construction] mode, it is the remaining gas after removing
    the gas of probes from the available gas in the pre-state.*)
let expected_witness witness probes ~mode ctxt =
  let open Lwt_result_syntax in
  let b_in, c_in, g_in = witness in
  let*? b_expected = b_in -? probes.fee in
  let c_expected =
    Manager_counter.Internal_for_tests.add c_in probes.nb_counter
  in
  let* g_expected =
    match (g_in, mode) with
    | Some g_in, Construction ->
        return_some (Gas.Arith.sub g_in (Gas.Arith.fp probes.gas_limit))
    | _, Mempool ->
        let* c = Context.get_constants ctxt in
        return_some
          (Gas.Arith.sub
             (Gas.Arith.fp c.parametric.hard_gas_limit_per_block)
             (Gas.Arith.fp probes.gas_limit))
    | None, Application -> return_none
    | Some _, Application ->
        failwith "In application mode witness should not care about gas level"
    | None, Construction ->
        failwith "In Construction mode the witness should return a gas level"
  in
  return (b_expected, c_expected, g_expected)

(** The validity of a test in positve case, observes that validation
    of a manager operation implies the fee payment. This observation
    differs according to the validation calling [mode] (see type mode
    for more details) and that the [source] has been [deallocated].
    Given the values of witness in the pre-state, the probes of the
    operation probes and the values of witness in the post-state, if
    the validation succeeds while deallocating the [source], [source]
    must be unallocated in the post-state.

    In case of successful validation Without deallocation, then we
    observe in the post-state:

    The balance of source decreases at least by fee of probes when the
    application has succeeded,

    Its counter in the pre-state increases by the number of counter of
    probes.

    The remaining gas in the pre-state decreases at least by the gas
    of probes, in [Construction] and [Mempool] mode.

    In [Mempool] mode, the remaining gas in the pre-state is always
    the available gas in an empty block.

    In the [Application] mode, we do not perform any check on the
    available gas. *)
let observe ~mode ~deallocated ctxt_pre ctxt_post op =
  let open Lwt_result_wrap_syntax in
  let check_deallocated ctxt contract =
    let* actxt =
      let+ i =
        match ctxt with
        | Context.B b -> Incremental.begin_construction b
        | I i -> return i
      in
      Incremental.alpha_ctxt i
    in
    let*!@ res = Contract.must_be_allocated actxt contract in
    match res with
    | Ok () ->
        failwith
          "%a should have been deallocated@."
          Signature.Public_key_hash.pp
          (Context.Contract.pkh contract)
    | Error
        [
          Environment.Ecoproto_error (Contract_storage.Empty_implicit_contract _);
        ] ->
        return_unit
    | Error errs ->
        failwith "unexpected error, got %a@." Error_monad.pp_print_trace errs
  in
  let check_still_allocated ctxt_pre ctxt_post probes contract =
    let* witness_in = witness ctxt_pre contract in
    let* b_out, c_out, g_out = witness ctxt_post contract in
    let* b_expected, c_expected, g_expected =
      expected_witness witness_in probes ~mode ctxt_post
    in
    let b_cmp =
      Assert.equal
        ~loc:__LOC__
        Tez.( <= )
        "Balance decreases at least by fees"
        Tez.pp
    in
    let* () = b_cmp b_out b_expected in
    let* () =
      Assert.equal
        Manager_counter.equal
        ~loc:__LOC__
        "Counter incrementation"
        Manager_counter.pp
        c_out
        c_expected
    in
    let g_msg =
      match mode with
      | Application -> "Gas consumption (application)"
      | Mempool -> "Gas consumption (mempool)"
      | Construction -> "Gas consumption (construction)"
    in
    match g_expected with
    | None -> Assert.is_none ~loc:__LOC__ ~pp:Gas.Arith.pp g_out
    | Some g_expected ->
        let* g_out = Assert.get_some ~loc:__LOC__ g_out in
        Assert.equal
          ~loc:__LOC__
          Gas.Arith.( <= )
          g_msg
          Gas.Arith.pp
          g_out
          g_expected
  in
  let* probes = manager_content_infos op in
  let contract = Contract.Implicit probes.source in
  if deallocated then check_deallocated ctxt_post contract
  else check_still_allocated ctxt_pre ctxt_post probes contract

let observe_list ~mode ~deallocated ctxt_pre ctxt_post ops =
  List.iter_es (fun op -> observe ~mode ~deallocated ctxt_pre ctxt_post op) ops

let validate_operations_effects inc_in ops =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun inc op ->
      let* inc_out =
        Incremental.add_operation ~allow_manager_failure:true inc op
      in
      return inc_out)
    inc_in
    ops

(** In [Construction] and [Mempool] mode, the pre-state provide an
   incremental, whereas in the [Application] mode, it is the block in
   the setting context of the test. *)
let pre_state_of_mode ~mode infos =
  let open Lwt_result_syntax in
  match mode with
  | Construction | Mempool ->
      let* inc = Incremental.begin_construction infos.ctxt.block in
      return (Context.I inc)
  | Application -> return (Context.B infos.ctxt.block)

(** In [Construction] and [Mempool] mode, the post-state is
   incrementally built upon a pre-state, whereas in the [Application]
   mode it is obtained by baking. *)
let post_state_of_mode ?(_only_validate = false) ~mode ctxt ops infos =
  let open Lwt_result_syntax in
  match (mode, ctxt) with
  | (Construction | Mempool), Context.I inc_pre ->
      let* inc_post = validate_operations_effects inc_pre ops in
      let* block = Incremental.finalize_block inc_post in
      return (Context.I inc_post, {infos with ctxt = {infos.ctxt with block}})
  | Application, Context.B b ->
      let+ block =
        Block.bake
          ~allow_manager_failures:true
          ~baking_mode:Application
          ~operations:ops
          b
      in
      (Context.B block, {infos with ctxt = {infos.ctxt with block}})
  | Application, Context.I _ ->
      failwith "In Application mode, context should not be an Incremental"
  | (Construction | Mempool), Context.B _ ->
      failwith "In (Partial) Contruction mode, context should not be a Block"

(** A positive test builds a pre-state from a mode, and a setting
   context, then it computes a post-state from the mode, the setting
   context and the operations. Finally, it observes the result
   according to the [emptying] status for each operation.

   See [observe] for more details on the observational validation.
   If the operation validation succeeds but should be deallocated,
   then [deallocated ] must be set.

    Default mode is [Construction]. *)
let validate_diagnostic ?(deallocated = false) ?(mode = Construction)
    (infos : infos) ops =
  let open Lwt_result_syntax in
  let* ctxt_pre = pre_state_of_mode ~mode infos in
  let* ctxt_post, infos = post_state_of_mode ~mode ctxt_pre ops infos in
  let* () = observe_list ~mode ~deallocated ctxt_pre ctxt_post ops in
  return infos

let add_operations ~expect_failure inc_in ops =
  let open Lwt_result_syntax in
  let* last, ops =
    match List.rev ops with
    | op :: rev_ops -> return (op, List.rev rev_ops)
    | [] -> failwith "Empty list of operations given to add_operations"
  in
  let* inc =
    List.fold_left_es
      (fun inc op ->
        let* inc = Incremental.validate_operation inc op in
        return inc)
      inc_in
      ops
  in
  Incremental.validate_operation inc last ~expect_failure

(** [validate_ko_diagnostic] wraps the [expect_failure] when [op]
   validate failed. It is used in test that expects validate of the
   last operation of a list of operations to fail. *)
let validate_ko_diagnostic ?(mode = Construction) (infos : infos) ops
    expect_failure =
  let open Lwt_result_syntax in
  match mode with
  | Construction | Mempool ->
      let* i =
        Incremental.begin_construction
          infos.ctxt.block
          ~mempool_mode:(mempool_mode_of mode)
      in
      let* (_ : Incremental.t) = add_operations ~expect_failure i ops in
      return_unit
  | Application -> (
      let*! res =
        Block.bake
          ~allow_manager_failures:true
          ~baking_mode:Application
          ~operations:ops
          infos.ctxt.block
      in
      match res with
      | Error tr -> expect_failure tr
      | _ -> failwith "Block application was expected to fail")

(** List of operation kinds that must run on generic tests. This list
   should be extended for each new manager_operation kind. *)
let subjects =
  [
    K_Transaction;
    K_Origination;
    K_Register_global_constant;
    K_Delegation;
    K_Undelegation;
    K_Self_delegation;
    K_Set_deposits_limit;
    K_Update_consensus_key;
    K_Increase_paid_storage;
    K_Reveal;
    K_Transfer_ticket;
    K_Sc_rollup_origination;
    K_Sc_rollup_publish;
    K_Sc_rollup_cement;
    K_Sc_rollup_add_messages;
    K_Sc_rollup_refute;
    K_Sc_rollup_timeout;
    K_Sc_rollup_execute_outbox_message;
    K_Sc_rollup_recover_bond;
    K_Dal_publish_commitment;
    K_Zk_rollup_origination;
    K_Zk_rollup_publish;
    K_Zk_rollup_update;
  ]

let is_consumer = function
  | K_Set_deposits_limit | K_Update_consensus_key | K_Increase_paid_storage
  | K_Reveal | K_Self_delegation | K_Delegation | K_Undelegation
  | K_Sc_rollup_add_messages | K_Sc_rollup_origination | K_Sc_rollup_refute
  | K_Sc_rollup_timeout | K_Sc_rollup_cement | K_Sc_rollup_publish
  | K_Sc_rollup_execute_outbox_message | K_Sc_rollup_recover_bond
  | K_Dal_publish_commitment | K_Zk_rollup_origination | K_Zk_rollup_publish
  | K_Zk_rollup_update ->
      false
  | K_Transaction | K_Origination | K_Register_global_constant
  | K_Transfer_ticket ->
      true

let gas_consumer_in_validate_subjects, not_gas_consumer_in_validate_subjects =
  List.partition is_consumer subjects

let revealed_subjects =
  List.filter (function K_Reveal -> false | _ -> true) subjects

let is_disabled flags = function
  | K_Transaction | K_Origination | K_Register_global_constant | K_Delegation
  | K_Undelegation | K_Self_delegation | K_Set_deposits_limit
  | K_Update_consensus_key | K_Increase_paid_storage | K_Reveal
  | K_Transfer_ticket ->
      false
  | K_Sc_rollup_origination | K_Sc_rollup_publish | K_Sc_rollup_cement
  | K_Sc_rollup_add_messages | K_Sc_rollup_refute | K_Sc_rollup_timeout
  | K_Sc_rollup_execute_outbox_message | K_Sc_rollup_recover_bond ->
      flags.scoru_arith = false
  | K_Dal_publish_commitment -> flags.dal = false
  | K_Zk_rollup_origination | K_Zk_rollup_publish | K_Zk_rollup_update ->
      flags.zkru = false
