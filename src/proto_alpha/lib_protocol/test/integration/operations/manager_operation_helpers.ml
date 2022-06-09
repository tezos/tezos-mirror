(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic-Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission  is hereby granted, free of charge, to any person obtaining a  *)
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
open Test_tez

(* Hard gas limit *)
let gb_limit = Gas.Arith.(integral_of_int_exn 100_000)

let half_gb_limit = Gas.Arith.(integral_of_int_exn 50_000)

type infos = {
  block : Block.t;
  account1 : Account.t;
  contract1 : Contract.t;
  account2 : Account.t;
  contract2 : Contract.t;
  account3 : Account.t;
  contract3 : Contract.t;
  tx_rollup : Tx_rollup.t;
  sc_rollup : Sc_rollup.t;
}

(* Initialize an [infos] record with a context enabling tx and sc
   rollup, funded accounts, tx_rollup, sc_rollup *)
let init_context ?hard_gas_limit_per_block () =
  let open Lwt_result_syntax in
  let* b, bootstrap_contract =
    Context.init1
      ~consensus_threshold:0
      ?hard_gas_limit_per_block
      ~tx_rollup_enable:true
      ~tx_rollup_sunset_level:Int32.max_int
      ~sc_rollup_enable:true
      ~dal_enable:true
      ()
  in
  (* Set a gas_limit to avoid the default gas_limit of the helpers
     ([hard_gas_limit_per_operation]) *)
  let gas_limit = Op.Custom_gas (Gas.Arith.integral_of_int_exn 10_000) in
  (* Create and fund an account use for originate a Tx and a Sc
     rollup *)
  let rollup_account = Account.new_account () in
  let rollup_contract = Contract.Implicit rollup_account.pkh in
  let counter = Z.zero in
  let* fund_rollup_account =
    Op.transaction
      ~counter
      ~gas_limit
      (B b)
      bootstrap_contract
      rollup_contract
      Tez.one
  in
  let* b = Block.bake ~operation:fund_rollup_account b in
  let counter2 = Z.succ counter in
  let* rollup_origination, tx_rollup =
    Op.tx_rollup_origination ~counter:counter2 ~gas_limit (B b) rollup_contract
  in
  let* _, sc_rollup =
    Op.sc_rollup_origination
      ~counter:counter2
      ~gas_limit
      (B b)
      rollup_contract
      Sc_rollup.Kind.Example_arith
      ""
      (Script.lazy_expr (Expr.from_string "1"))
  in
  let* b = Block.bake ~operation:rollup_origination b in
  (* Create and fund three accounts *)
  let account1 = Account.new_account () in
  let contract1 = Contract.Implicit account1.pkh in
  let counter = Z.succ counter in
  let* fund_account1 =
    Op.transaction
      ~counter
      ~gas_limit
      (B b)
      bootstrap_contract
      contract1
      Tez.one
  in
  let account2 = Account.new_account () in
  let contract2 = Contract.Implicit account2.pkh in
  let counter = Z.succ counter in
  let* fund_account2 =
    Op.transaction
      ~counter
      ~gas_limit
      (B b)
      bootstrap_contract
      contract2
      Tez.one
  in
  let account3 = Account.new_account () in
  let contract3 = Contract.Implicit account3.pkh in
  let counter = Z.succ counter in
  let* fund_account3 =
    Op.transaction
      ~counter
      ~gas_limit
      (B b)
      bootstrap_contract
      contract3
      Tez.one
  in
  let* operation =
    Op.batch_operations
      ~source:bootstrap_contract
      (B b)
      [fund_account1; fund_account2; fund_account3]
  in
  let+ block = Block.bake ~operation b in
  {
    block;
    account1;
    contract1;
    account2;
    contract2;
    account3;
    contract3;
    tx_rollup;
    sc_rollup;
  }

(* Same as [init_context] but [contract1] delegate to [contract2] *)
let init_delegated_implicit () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let* del_opt =
    Context.Contract.delegate_opt (B infos.block) infos.contract1
  in
  let* _ =
    Assert.is_none
      ~loc:__LOC__
      ~pp:(fun fmt _ -> Format.fprintf fmt "should not be delegated")
      del_opt
  in
  let* operation =
    Op.delegation
      (B infos.block)
      infos.contract2
      (Some (Context.Contract.pkh infos.contract2))
  in
  let* block = Block.bake infos.block ~operation in
  let* operation =
    Op.delegation (B block) infos.contract1 (Some infos.account2.pkh)
  in
  let* block = Block.bake block ~operation in
  let* del_opt_new = Context.Contract.delegate_opt (B block) infos.contract1 in
  let* del = Assert.get_some ~loc:__LOC__ del_opt_new in
  let+ _ = Assert.equal_pkh ~loc:__LOC__ del infos.account2.pkh in
  {infos with block}

(* Same as [init_context] but [contract1] self delegate. *)
let init_self_delegated_implicit () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let* del_opt =
    Context.Contract.delegate_opt (B infos.block) infos.contract1
  in
  let* _ =
    Assert.is_none
      ~loc:__LOC__
      ~pp:(fun fmt _ -> Format.fprintf fmt "should not be delegated")
      del_opt
  in
  let* operation =
    Op.delegation (B infos.block) infos.contract1 (Some infos.account1.pkh)
  in
  let* block = Block.bake infos.block ~operation in
  let* del_opt_new = Context.Contract.delegate_opt (B block) infos.contract1 in
  let* del = Assert.get_some ~loc:__LOC__ del_opt_new in
  let+ _ = Assert.equal_pkh ~loc:__LOC__ del infos.account1.pkh in
  {infos with block}

(* Local helpers for generating all kind of manager operations. *)

(* Create a fresh account used for empty implicit account tests. *)
let mk_fresh_contract () = Contract.Implicit Account.(new_account ()).pkh

let get_pkh source = Context.Contract.pkh source

let get_pk infos source =
  let open Lwt_result_syntax in
  let+ account = Context.Contract.manager infos source in
  account.pk

let mk_transaction ?counter ?fee ?gas_limit ?storage_limit ?force_reveal ~source
    (infos : infos) =
  Op.transaction
    ?force_reveal
    ?counter
    ?fee
    ?gas_limit
    ?storage_limit
    (B infos.block)
    source
    infos.contract2
    Tez.one

let mk_delegation ?counter ?fee ?gas_limit ?storage_limit ?force_reveal ~source
    (infos : infos) =
  Op.delegation
    ?force_reveal
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    (B infos.block)
    source
    (Some infos.account2.pkh)

let mk_undelegation ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  Op.delegation
    ?force_reveal
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    (B infos.block)
    source
    None

let mk_self_delegation ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  Op.delegation
    ?force_reveal
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    (B infos.block)
    source
    (Some (get_pkh source))

let mk_origination ?counter ?fee ?gas_limit ?storage_limit ?force_reveal ~source
    (infos : infos) =
  let open Lwt_result_syntax in
  let+ op, _ =
    Op.contract_origination
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~script:Op.dummy_script
      (B infos.block)
      source
  in
  op

let mk_register_global_constant ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  Op.register_global_constant
    ?force_reveal
    ?counter
    ?fee
    ?gas_limit
    ?storage_limit
    (B infos.block)
    ~source
    ~value:(Script_repr.lazy_expr (Expr.from_string "Pair 1 2"))

let mk_set_deposits_limit ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  Op.set_deposits_limit
    ?force_reveal
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    (B infos.block)
    source
    None

let mk_reveal ?counter ?fee ?gas_limit ?storage_limit ?force_reveal:_ ~source
    (infos : infos) =
  let open Lwt_result_syntax in
  let* pk = get_pk (B infos.block) source in
  Op.revelation ?fee ?gas_limit ?counter ?storage_limit (B infos.block) pk

let mk_tx_rollup_origination ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  let open Lwt_result_syntax in
  let+ op, _rollup =
    Op.tx_rollup_origination
      ?fee
      ?gas_limit
      ?counter
      ?storage_limit
      ?force_reveal
      (B infos.block)
      source
  in
  op

let mk_tx_rollup_submit_batch ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  Op.tx_rollup_submit_batch
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.tx_rollup
    "batch"

let mk_tx_rollup_commit ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  let commitement : Tx_rollup_commitment.Full.t =
    {
      level = Tx_rollup_level.root;
      messages = [];
      predecessor = None;
      inbox_merkle_root = Tx_rollup_inbox.Merkle.merklize_list [];
    }
  in
  Op.tx_rollup_commit
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.tx_rollup
    commitement

let mk_tx_rollup_return_bond ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  Op.tx_rollup_return_bond
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.tx_rollup

let mk_tx_rollup_finalize ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  Op.tx_rollup_finalize
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.tx_rollup

let mk_tx_rollup_remove_commitment ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  Op.tx_rollup_remove_commitment
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.tx_rollup

let mk_tx_rollup_reject ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  let message, _ = Tx_rollup_message.make_batch "" in
  let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
  let message_path =
    match Tx_rollup_inbox.Merkle.compute_path [message_hash] 0 with
    | Ok message_path -> message_path
    | _ -> raise (Invalid_argument "Single_message_inbox.message_path")
  in
  let proof : Tx_rollup_l2_proof.t =
    {
      version = 1;
      before = `Value Tx_rollup_message_result.empty_l2_context_hash;
      after = `Value Context_hash.zero;
      state = Seq.empty;
    }
  in
  let previous_message_result : Tx_rollup_message_result.t =
    {
      context_hash = Tx_rollup_message_result.empty_l2_context_hash;
      withdraw_list_hash = Tx_rollup_withdraw_list_hash.empty;
    }
  in
  Op.tx_rollup_reject
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.tx_rollup
    Tx_rollup_level.root
    message
    ~message_position:0
    ~message_path
    ~message_result_hash:Tx_rollup_message_result_hash.zero
    ~message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    ~proof
    ~previous_message_result
    ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path

let mk_transfer_ticket ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  Op.transfer_ticket
    ?fee
    ?force_reveal
    ?counter
    ?gas_limit
    ?storage_limit
    (B infos.block)
    ~source
    ~contents:(Script.lazy_expr (Expr.from_string "1"))
    ~ty:(Script.lazy_expr (Expr.from_string "nat"))
    ~ticketer:infos.contract3
    Z.zero
    ~destination:infos.contract2
    Entrypoint.default

let mk_tx_rollup_dispacth_ticket ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  let reveal =
    Tx_rollup_reveal.
      {
        contents = Script.lazy_expr (Expr.from_string "1");
        ty = Script.lazy_expr (Expr.from_string "nat");
        ticketer = infos.contract2;
        amount = Tx_rollup_l2_qty.of_int64_exn 10L;
        claimer = infos.account3.pkh;
      }
  in
  Op.tx_rollup_dispatch_tickets
    ?fee
    ?force_reveal
    ?counter
    ?gas_limit
    ?storage_limit
    (B infos.block)
    ~source
    ~message_index:0
    ~message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    infos.tx_rollup
    Tx_rollup_level.root
    Context_hash.zero
    [reveal]

let mk_sc_rollup_origination ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  let open Lwt_result_syntax in
  let+ op, _ =
    Op.sc_rollup_origination
      ?fee
      ?gas_limit
      ?counter
      ?storage_limit
      ?force_reveal
      (B infos.block)
      source
      Sc_rollup.Kind.Example_arith
      ""
      (Script.lazy_expr (Expr.from_string "1"))
  in
  op

let sc_dummy_commitment =
  let number_of_messages =
    match Sc_rollup.Number_of_messages.of_int32 3l with
    | None -> assert false
    | Some x -> x
  in
  let number_of_ticks =
    match Sc_rollup.Number_of_ticks.of_int32 3000l with
    | None -> assert false
    | Some x -> x
  in
  Sc_rollup.Commitment.
    {
      predecessor = Sc_rollup.Commitment.Hash.zero;
      inbox_level = Raw_level.of_int32_exn Int32.zero;
      number_of_messages;
      number_of_ticks;
      compressed_state = Sc_rollup.State_hash.zero;
    }

let mk_sc_rollup_publish ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  Op.sc_rollup_publish
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.sc_rollup
    sc_dummy_commitment

let mk_sc_rollup_cement ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  Op.sc_rollup_cement
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.sc_rollup
    (Sc_rollup.Commitment.hash sc_dummy_commitment)

let mk_sc_rollup_refute ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  let refutation : Sc_rollup.Game.refutation =
    {choice = Sc_rollup.Tick.initial; step = Dissection []}
  in
  Op.sc_rollup_refute
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.sc_rollup
    infos.account2.pkh
    refutation
    false

let mk_sc_rollup_add_messages ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  Op.sc_rollup_add_messages
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.sc_rollup
    []

let mk_sc_rollup_timeout ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ~source (infos : infos) =
  Op.sc_rollup_timeout
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.sc_rollup
    (Sc_rollup.Game.Index.make infos.account2.pkh infos.account3.pkh)

let mk_sc_rollup_execute_outbox_message ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  Op.sc_rollup_execute_outbox_message
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.sc_rollup
    (Sc_rollup.Commitment.hash sc_dummy_commitment)
    ~outbox_level:Raw_level.root
    ~message_index:0
    ~inclusion_proof:""
    ~message:""

let mk_sc_rollup_return_bond ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  Op.sc_rollup_recover_bond
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    infos.sc_rollup

let mk_dal_publish_slot_header ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ~source (infos : infos) =
  let open Lwt_result_syntax in
  let level = 0 in
  let index = 0 in
  let header = 0 in
  let json_slot =
    Data_encoding.Json.from_string
      (Format.asprintf
         {|{"level":%d,"index":%d,"header":%d}|}
         level
         index
         header)
  in
  let* json_slot =
    match json_slot with Error s -> failwith "%s" s | Ok slot -> return slot
  in
  let slot = Data_encoding.Json.destruct Dal.Slot.encoding json_slot in
  Op.dal_publish_slot_header
    ?fee
    ?gas_limit
    ?counter
    ?storage_limit
    ?force_reveal
    (B infos.block)
    source
    slot

(* Helpers for generation of generic check tests by manager operation. *)
(* This type should be extended for each new manager_operation kind
   added in the protocol. *)
type manager_operation_kind =
  | K_Transaction
  | K_Origination
  | K_Register_global_constant
  | K_Delegation
  | K_Undelegation
  | K_Self_delegation
  | K_Set_deposits_limit
  | K_Reveal
  | K_Tx_rollup_origination
  | K_Tx_rollup_submit_batch
  | K_Tx_rollup_commit
  | K_Tx_rollup_return_bond
  | K_Tx_rollup_finalize
  | K_Tx_rollup_remove_commitment
  | K_Tx_rollup_dispatch_tickets
  | K_Transfer_ticket
  | K_Tx_rollup_reject
  | K_Sc_rollup_origination
  | K_Sc_rollup_publish
  | K_Sc_rollup_cement
  | K_Sc_rollup_add_messages
  | K_Sc_rollup_refute
  | K_Sc_rollup_timeout
  | K_Sc_rollup_execute_outbox_message
  | K_Sc_rollup_recover_bond
  | K_Dal_publish_slot_header

let select_op = function
  | K_Transaction -> mk_transaction
  | K_Origination -> mk_origination
  | K_Register_global_constant -> mk_register_global_constant
  | K_Delegation -> mk_delegation
  | K_Undelegation -> mk_undelegation
  | K_Self_delegation -> mk_self_delegation
  | K_Set_deposits_limit -> mk_set_deposits_limit
  | K_Reveal -> mk_reveal
  | K_Tx_rollup_origination -> mk_tx_rollup_origination
  | K_Tx_rollup_submit_batch -> mk_tx_rollup_submit_batch
  | K_Tx_rollup_commit -> mk_tx_rollup_commit
  | K_Tx_rollup_return_bond -> mk_tx_rollup_return_bond
  | K_Tx_rollup_finalize -> mk_tx_rollup_finalize
  | K_Tx_rollup_remove_commitment -> mk_tx_rollup_remove_commitment
  | K_Tx_rollup_reject -> mk_tx_rollup_reject
  | K_Transfer_ticket -> mk_transfer_ticket
  | K_Tx_rollup_dispatch_tickets -> mk_tx_rollup_dispacth_ticket
  | K_Sc_rollup_origination -> mk_sc_rollup_origination
  | K_Sc_rollup_publish -> mk_sc_rollup_publish
  | K_Sc_rollup_cement -> mk_sc_rollup_cement
  | K_Sc_rollup_refute -> mk_sc_rollup_refute
  | K_Sc_rollup_add_messages -> mk_sc_rollup_add_messages
  | K_Sc_rollup_timeout -> mk_sc_rollup_timeout
  | K_Sc_rollup_execute_outbox_message -> mk_sc_rollup_execute_outbox_message
  | K_Sc_rollup_recover_bond -> mk_sc_rollup_return_bond
  | K_Dal_publish_slot_header -> mk_dal_publish_slot_header

let string_of_kind = function
  | K_Transaction -> "Transaction"
  | K_Delegation -> "Delegation"
  | K_Undelegation -> "Undelegation"
  | K_Self_delegation -> "Self-delegation"
  | K_Set_deposits_limit -> "Set deposits limit"
  | K_Origination -> "Origination"
  | K_Register_global_constant -> "Register global constant"
  | K_Reveal -> "Revelation"
  | K_Tx_rollup_origination -> "Tx_rollup_origination"
  | K_Tx_rollup_submit_batch -> "Tx_rollup_submit_batch"
  | K_Tx_rollup_commit -> "Tx_rollup_commit"
  | K_Tx_rollup_return_bond -> "Tx_rollup_return_bond"
  | K_Tx_rollup_finalize -> "Tx_rollup_finalize"
  | K_Tx_rollup_remove_commitment -> "Tx_rollup_remove_commitment"
  | K_Tx_rollup_dispatch_tickets -> "Tx_rollup_dispatch_tickets"
  | K_Tx_rollup_reject -> "Tx_rollup_reject"
  | K_Transfer_ticket -> "Transfer_ticket"
  | K_Sc_rollup_origination -> "Sc_rollup_origination"
  | K_Sc_rollup_publish -> "Sc_rollup_publish"
  | K_Sc_rollup_cement -> "Sc_rollup_cement"
  | K_Sc_rollup_timeout -> "Sc_rollup_timeout"
  | K_Sc_rollup_refute -> "Sc_rollup_refute"
  | K_Sc_rollup_add_messages -> "Sc_rollup_add_messages"
  | K_Sc_rollup_execute_outbox_message -> "Sc_rollup_execute_outbox_message"
  | K_Sc_rollup_recover_bond -> "Sc_rollup_return_bond"
  | K_Dal_publish_slot_header -> "Dal_publish_slot_header"

let create_Tztest ?hd_msg test tests_msg operations =
  let hd_msg k =
    let sk = string_of_kind k in
    match hd_msg with
    | None -> sk
    | Some hd -> Format.sprintf "Batch: %s, %s" hd sk
  in
  List.map
    (fun kind ->
      Tztest.tztest
        (Format.sprintf "%s with %s" (hd_msg kind) tests_msg)
        `Quick
        (fun () -> test kind ()))
    operations

let rec create_Tztest_batches test tests_msg operations =
  let hdmsg k = Format.sprintf "%s" (string_of_kind k) in
  let aux hd_msg test operations =
    create_Tztest ~hd_msg test tests_msg operations
  in
  match operations with
  | [] -> []
  | kop :: kops as ops ->
      aux (hdmsg kop) (test kop) ops @ create_Tztest_batches test tests_msg kops

(* Diagnostic helpers. *)

type probes = {
  source : Signature.Public_key_hash.t;
  fee : Tez.tez;
  gas_limit : Gas.Arith.integral;
  nb_counter : Z.t;
}

let rec contents_infos :
    type kind. kind Kind.manager contents_list -> probes tzresult Lwt.t =
 fun op ->
  let open Lwt_result_syntax in
  match op with
  | Single (Manager_operation {source; fee; gas_limit; _}) ->
      return {source; fee; gas_limit; nb_counter = Z.one}
  | Cons (Manager_operation manop, manops) ->
      let* probes = contents_infos manops in
      let*? fee = manop.fee +? probes.fee in
      let gas_limit = Gas.Arith.add probes.gas_limit manop.gas_limit in
      let nb_counter = Z.succ probes.nb_counter in
      let _ = Assert.equal_pkh ~loc:__LOC__ manop.source probes.source in
      return {fee; source = probes.source; gas_limit; nb_counter}

let manager_content_infos op =
  let (Operation_data {contents; _}) = op.protocol_data in
  match contents with
  | Single (Manager_operation _) as op -> contents_infos op
  | Cons (Manager_operation _, _) as op -> contents_infos op
  | _ -> assert false

let observe ?g_in contract b_in c_in probes i =
  let open Lwt_result_syntax in
  let* b_out = Context.Contract.balance (I i) contract in
  let g_out = Gas.block_level (Incremental.alpha_ctxt i) in
  let* c_out = Context.Contract.counter (I i) contract in
  let*? b_expected = b_in -? probes.fee in
  let* _ = Assert.equal_tez ~loc:__LOC__ b_out b_expected in
  let c_expected = Z.add c_in probes.nb_counter in
  let _ =
    Assert.equal
      Z.equal
      ~loc:__LOC__
      "Counter incrementation"
      Z.pp_print
      c_out
      c_expected
  in
  match g_in with
  | Some g_in ->
      let g_expected = Gas.Arith.sub g_in (Gas.Arith.fp probes.gas_limit) in
      Assert.equal
        ~loc:__LOC__
        Gas.Arith.equal
        "Gas consumption"
        Gas.Arith.pp
        g_out
        g_expected
  | None -> return_unit

let precheck_ko_diagnostic ?(mempool_mode = false) (infos : infos) op
    expect_failure =
  let open Lwt_result_syntax in
  let* i = Incremental.begin_construction infos.block ~mempool_mode in
  let* _ = Incremental.add_operation ~expect_failure i op in
  return_unit

let apply_with_diagnostic ?expect_apply_failure (infos : infos) op =
  let open Lwt_result_syntax in
  let* i = Incremental.begin_construction infos.block in
  let* prbs = manager_content_infos op in
  let contract = Contract.Implicit prbs.source in
  let* b_in = Context.Contract.balance (I i) contract in
  let* c_in = Context.Contract.counter (I i) contract in
  let g_in = Gas.block_level (Incremental.alpha_ctxt i) in
  let* i = Incremental.add_operation ?expect_apply_failure i op in
  observe ~g_in contract b_in c_in prbs i

(* If the precheck of an operation succeed, whether the application
   fail or not, the fees must be paid, the block gas consumption
   should be decreased and the counter of operation should be
   incremented. *)
let apply_ko_diagnostic (infos : infos) op expect_apply_failure =
  apply_with_diagnostic ~expect_apply_failure (infos : infos) op

let apply_ok_diagnostic (infos : infos) op =
  apply_with_diagnostic (infos : infos) op

(* List of operation kind that must run on generic tests. This list
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
    K_Reveal;
    K_Tx_rollup_origination;
    K_Tx_rollup_submit_batch;
    K_Tx_rollup_commit;
    K_Tx_rollup_return_bond;
    K_Tx_rollup_finalize;
    K_Tx_rollup_remove_commitment;
    K_Tx_rollup_dispatch_tickets;
    K_Transfer_ticket;
    K_Tx_rollup_reject;
    K_Sc_rollup_origination;
    K_Sc_rollup_publish;
    K_Sc_rollup_cement;
    K_Sc_rollup_add_messages;
    K_Sc_rollup_refute;
    K_Sc_rollup_timeout;
    K_Sc_rollup_execute_outbox_message;
    K_Sc_rollup_recover_bond;
    K_Dal_publish_slot_header;
  ]

let except_not_consumer_in_precheck_subjects =
  List.filter
    (function
      | K_Set_deposits_limit | K_Reveal | K_Self_delegation | K_Delegation
      | K_Undelegation | K_Tx_rollup_origination | K_Tx_rollup_submit_batch
      | K_Tx_rollup_finalize | K_Tx_rollup_commit | K_Tx_rollup_return_bond
      | K_Tx_rollup_remove_commitment | K_Tx_rollup_reject
      | K_Sc_rollup_add_messages | K_Sc_rollup_origination | K_Sc_rollup_refute
      | K_Sc_rollup_timeout | K_Sc_rollup_cement | K_Sc_rollup_publish
      | K_Sc_rollup_execute_outbox_message | K_Sc_rollup_recover_bond
      | K_Dal_publish_slot_header ->
          false
      | _ -> true)
    subjects

let except_self_delegated_and_revelation_subjects =
  List.filter
    (function K_Self_delegation | K_Reveal -> false | _ -> true)
    subjects

let revealed_except_set_deposits_limit_and_submit_batch_subjects =
  List.filter
    (function
      | K_Set_deposits_limit | K_Tx_rollup_submit_batch | K_Reveal -> false
      | _ -> true)
    subjects

let revealed_only_set_deposits_limit_and_submit_batch_subjects =
  List.filter
    (function
      | K_Set_deposits_limit | K_Tx_rollup_submit_batch -> true | _ -> false)
    subjects

let revealed_subjects =
  List.filter (function K_Reveal -> false | _ -> true) subjects
