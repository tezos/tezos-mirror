(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

(** Testing
    -------
    Component:  Protocol (tx rollup l2)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "tx rollup l2 apply"
    Subject:    test the layer-2 apply implementation of transaction rollup
*)

open Tztest
open Tx_rollup_l2_helpers
open Protocol
open Alpha_context
open Apply_l2
open Context_l2
open Tx_rollup_message
open Tx_rollup_l2_apply
open Tx_rollup_l2_batch.V1
open Indexable

(** {2. Utils. } *)

(** {3. Various helpers to facilitate the tests. } *)

let pkh = Signature.Public_key_hash.zero

let ((_, pk1, addr1) as l2_addr1) = gen_l2_address ()

let ((_, pk2, addr2) as l2_addr2) = gen_l2_address ()

let wrap_test t () =
  t () >|= function
  | Ok x -> Ok x
  | Error err -> Error [Environment.Ecoproto_error err]

let wrap_tztest_tests =
  List.map (fun (name, test) -> tztest name `Quick @@ wrap_test test)

let get_opt = function
  | Some x -> return x
  | None -> fail_msg "Expected a Some value"

let expect_error_status ~msg error status cont =
  let open Message_result in
  match status with
  | Transaction_success -> fail_msg msg
  | Transaction_failure {reason; _} when error = reason -> cont
  | Transaction_failure {reason; _} ->
      let msg =
        Format.asprintf
          "Expected error: %a\nActual error: %a\n"
          Environment.Error_monad.pp
          error
          Environment.Error_monad.pp
          reason
      in
      fail_msg msg

let aggregate_signature_exn : signature list -> signature =
 fun signatures ->
  match Bls12_381.Signature.MinPk.aggregate_signature_opt signatures with
  | Some res -> res
  | None -> raise (Invalid_argument "aggregate_signature_exn")

let (ticket1, ticket2) =
  match gen_n_ticket_hash 2 with [x; y] -> (x, y) | _ -> assert false

let empty_indexes =
  {
    address_indexes = Address_indexes.empty;
    ticket_indexes = Ticket_indexes.empty;
  }

let unexpected_result = fail_msg "Unexpected result operation"

let signer_of_address_index :
    Tx_rollup_l2_address.Indexable.index ->
    Tx_rollup_l2_batch.Signer_indexable.index =
 fun x -> Indexable.(index_exn (to_int32 x))

(** {3. Various Alcotest helpers to check the context. } *)

let eq_qty = Alcotest.of_pp Tx_rollup_l2_qty.pp

let check_balance ctxt name_account name_ticket description tidx aidx
    expected_value =
  let open Syntax in
  let expected_value = Tx_rollup_l2_qty.of_int64_exn expected_value in
  let* res = Ticket_ledger.get ctxt tidx aidx in
  Alcotest.(
    check
      eq_qty
      (Format.sprintf
         "balance for %s of %s (%s)"
         name_account
         name_ticket
         description)
      expected_value
      res) ;
  return ()

let pp_metadata fmt Tx_rollup_l2_context_sig.{counter; public_key} =
  let counter = Int64.to_int counter in
  Format.fprintf
    fmt
    "{counter=%d; public_key=%s}"
    counter
    (Environment.Bls_signature.pk_to_bytes public_key |> Bytes.to_string)

let eq_metadata = Alcotest.of_pp pp_metadata

let check_metadata ctxt name_account description counter pk =
  let open Syntax in
  let addr = Tx_rollup_l2_address.of_bls_pk pk in
  (* We ignore the created [ctxt] because it should be a get only. *)
  let* (_ctxt, _, aidx) = Address_index.get_or_associate_index ctxt addr in
  let* metadata = Address_metadata.get ctxt aidx in
  Alcotest.(
    check
      (option eq_metadata)
      (Format.sprintf "metadata for %s (%s)" name_account description)
      (Some Tx_rollup_l2_context_sig.{counter; public_key = pk})
      metadata) ;
  return ()

let eq_address = Alcotest.of_pp Tx_rollup_l2_address.pp

let eq_ticket = Alcotest.of_pp Ticket_hash.pp

let eq_addr_indexable = Alcotest.of_pp (Indexable.pp (fun _ _ -> ()))

let eq_ticket_indexable = Alcotest.of_pp (Indexable.pp (fun _ _ -> ()))

let pp_withdrawal fmt = function
  | Tx_rollup_withdraw.{claimer; ticket_hash; amount} ->
      Format.fprintf
        fmt
        "{claimer=%a; ticket_hash=%a; amount=%a}"
        Signature.Public_key_hash.pp
        claimer
        Ticket_hash.pp
        ticket_hash
        Tx_rollup_l2_qty.pp
        amount

let eq_withdrawal = Alcotest.of_pp pp_withdrawal

let check_indexes addr_indexes ticket_indexes expected =
  let open Syntax in
  (* This is dirty but it orders the list by their indexes. *)
  let expected_addr_indexes =
    Address_indexes.bindings expected.address_indexes
    |> List.sort (fun (_, idx1) (_, idx2) ->
           Tx_rollup_l2_address.Indexable.compare idx1 idx2)
  in

  let expected_ticket_indexes =
    Ticket_indexes.bindings expected.ticket_indexes
    |> List.sort (fun (_, idx1) (_, idx2) ->
           Tx_rollup_l2_context_sig.Ticket_indexable.compare idx1 idx2)
  in

  (* This is a dirty hack to build [Indexable.either] and then use the
     [Indexable.pp] to build an [Alcotest.testable]. The other solution
     would be to expose [Indexable.pp_index]. *)
  let forget_ticket_indexes = List.map (fun (v, idx) -> (v, forget idx)) in
  let expected_ticket_indexes = forget_ticket_indexes expected_ticket_indexes in
  let ticket_indexes = forget_ticket_indexes ticket_indexes in

  let forget_addr_indexes = List.map (fun (v, idx) -> (v, forget idx)) in
  let expected_addr_indexes = forget_addr_indexes expected_addr_indexes in
  let addr_indexes = forget_addr_indexes addr_indexes in

  Alcotest.(
    check
      (list (pair eq_address eq_addr_indexable))
      "indexables address created"
      expected_addr_indexes
      addr_indexes) ;

  Alcotest.(
    check
      (list (pair eq_ticket eq_ticket_indexable))
      "indexables ticket created"
      expected_ticket_indexes
      ticket_indexes) ;

  return ()

(** {3. Helpers to build apply related values. } *)

let with_initial_setup tickets contracts =
  let open Context_l2.Syntax in
  let ctxt = empty_context in

  let* (ctxt, rev_tidxs) =
    list_fold_left_m
      (fun (ctxt, rev_tidxs) ticket ->
        let* (ctxt, _, tidx) =
          Ticket_index.get_or_associate_index ctxt ticket
        in
        return (ctxt, tidx :: rev_tidxs))
      (ctxt, [])
      tickets
  in
  let tidxs = List.rev rev_tidxs in

  let* (ctxt, rev_contracts) =
    list_fold_left_m
      (fun (ctxt, rev_contracts) balances ->
        let (pkh, _, _) = gen_l1_address () in
        let (sk, pk, addr) = gen_l2_address () in
        let* (ctxt, _, idx) = Address_index.get_or_associate_index ctxt addr in

        let* ctxt =
          list_fold_left_m
            (fun ctxt (ticket, qty) ->
              let qty = Tx_rollup_l2_qty.of_int64_exn qty in
              let* (ctxt, _, tidx) =
                Ticket_index.get_or_associate_index ctxt ticket
              in
              Ticket_ledger.credit ctxt tidx idx qty)
            ctxt
            balances
        in

        return (ctxt, (sk, pk, addr, idx, pkh) :: rev_contracts))
      (ctxt, [])
      contracts
  in
  let rev_contracts = List.rev rev_contracts in

  return (ctxt, tidxs, rev_contracts)

let transfer ?(counter = 1L) ~signer ~dest ~ticket qty =
  let open Tx_rollup_l2_batch.V1 in
  let qty = Tx_rollup_l2_qty.of_int64_exn qty in
  let content = {destination = dest; ticket_hash = from_value ticket; qty} in
  {signer = from_value signer; counter; contents = [content]}

let l1addr pkh = Tx_rollup_l2_batch.Layer1 pkh

let l2addr addr = Tx_rollup_l2_batch.Layer2 (from_value addr)

let transfers =
  List.map (fun (pk_source, dest, ticket, amount, counter) ->
      transfer ~signer:pk_source ~dest ~ticket ?counter amount)

let batch signatures contents =
  let open Tx_rollup_l2_batch.V1 in
  let aggregated_signature = aggregate_signature_exn signatures in
  {aggregated_signature; contents}

let create_batch_v1
    (transactions : ('signer, 'content) Tx_rollup_l2_batch.V1.transaction list)
    sks_l =
  assert (List.(length transactions = length sks_l)) ;
  let signatures =
    List.map2
      ~when_different_lengths:[]
      (fun transaction sks -> sign_transaction sks transaction)
      transactions
      sks_l
    |> function
    | Ok xs -> List.concat xs
    | _ -> assert false
  in
  batch signatures transactions

(** {2. Tests } *)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2461
   A lot of l2-context properties can be property-based tested. *)

(** Test that deposit tickets in the layer2 updates the context. *)
let test_simple_deposit () =
  let open Context_l2.Syntax in
  let ctxt = empty_context in
  let amount = Tx_rollup_l2_qty.of_int64_exn 50L in

  let deposit =
    {sender = pkh; destination = value addr1; ticket_hash = ticket1; amount}
  in
  let* (ctxt, result, withdrawal_opt) = apply_deposit ctxt deposit in

  (* Applying the deposit should create an idx for both [addr1] and [ticket]. *)
  match (result, withdrawal_opt) with
  | (Deposit_success indexes, None) ->
      let* () =
        check_indexes [(addr1, index_exn 0l)] [(ticket1, index_exn 0l)] indexes
      in
      let* aidx_opt = Address_index.get ctxt addr1 in
      let* aidx = get_opt aidx_opt in

      let* tidx_opt = Ticket_index.get ctxt ticket1 in
      let* tidx = get_opt tidx_opt in

      let* amount' = Context_l2.Ticket_ledger.get ctxt tidx aidx in
      assert (amount = amount') ;

      return_unit
  | _ -> unexpected_result

(** Test that deposit tickets in the layer2 does not create new indexes
    if they already existed. *)
let test_deposit_with_existing_indexes () =
  let open Context_l2.Syntax in
  let ctxt = empty_context in

  (* We first associate an index to our address and ticket *)
  let* (ctxt, _, aidx1) = Address_index.get_or_associate_index ctxt addr1 in
  let* (ctxt, _, tidx1) = Ticket_index.get_or_associate_index ctxt ticket1 in

  let deposit =
    {
      sender = pkh;
      destination = value addr1;
      ticket_hash = ticket1;
      amount = Tx_rollup_l2_qty.of_int64_exn 1L;
    }
  in
  let* (ctxt, result, withdrawal_opt) = apply_deposit ctxt deposit in

  (* The indexes should not be considered as created *)
  match (result, withdrawal_opt) with
  | (Deposit_success indexes, None) ->
      assert (indexes.address_indexes = Address_indexes.empty) ;
      assert (indexes.ticket_indexes = Ticket_indexes.empty) ;

      let* () =
        check_balance ctxt "addr1" "ticket1" "deposit 1" tidx1 aidx1 1L
      in

      return_unit
  | _ -> fail_msg "Unexpected operation result"

(** Test that deposit overflow withdraws the amount sent. *)
let test_returned_deposit () =
  let open Context_l2.Syntax in
  let balance = Int64.max_int in
  let* (ctxt, tidxs, accounts) =
    with_initial_setup [ticket1] [[(ticket1, balance)]]
  in
  let tidx1 = nth_exn tidxs 0 in
  let (_sk1, _pk1, addr1, idx1, pkh) = nth_exn accounts 0 in

  (* my cup runneth over *)
  let amount = Tx_rollup_l2_qty.one in
  let deposit =
    {sender = pkh; destination = value addr1; ticket_hash = ticket1; amount}
  in
  let* (ctxt, result, withdrawal_opt) = apply_deposit ctxt deposit in

  (* Applying the deposit will result in a Deposit_failure, an
     unchanged context and a withdrawal of the deposit *)
  match (result, withdrawal_opt) with
  | (Deposit_failure Tx_rollup_l2_context_sig.Balance_overflow, Some withdrawal)
    ->
      (* balance is unchanged *)
      let* balance' = Context_l2.Ticket_ledger.get ctxt tidx1 idx1 in
      Alcotest.(
        check
          eq_qty
          "An overflowing deposit should not modify balance"
          (Tx_rollup_l2_qty.of_int64_exn balance)
          balance') ;
      Alcotest.(
        check
          eq_withdrawal
          "Resulting withdrawal from overflowing L1->L2 deposit"
          withdrawal
          {claimer = pkh; ticket_hash = ticket1; amount}) ;
      return_unit
  | (Deposit_failure reason, _) ->
      let msg =
        Format.asprintf
          "Unexpected failure for overflowing deposit: %a"
          Environment.Error_monad.pp
          reason
      in
      fail_msg msg
  | (Deposit_success _result, _) ->
      fail_msg "Did not expect overflowing deposit to be succesful"

(** Test that all values used in a transaction creates indexes and they are
    packed in the final indexes. *)
let test_indexes_creation () =
  let open Context_l2.Syntax in
  let ctxt = empty_context in
  let contracts = gen_n_address 4 in

  let (sk1, pk1, addr1) = nth_exn contracts 0 in
  let (_, _, addr2) = nth_exn contracts 1 in
  let (_, _, addr3) = nth_exn contracts 2 in
  let (_, _, addr4) = nth_exn contracts 3 in

  (* We begin by deposit 100 tickets to [addr1] which would be
     transfered between the other addresses. *)
  let deposit =
    {
      sender = pkh;
      destination = value addr1;
      ticket_hash = ticket1;
      amount = Tx_rollup_l2_qty.of_int64_exn 100L;
    }
  in
  let* (ctxt, result, withdrawal_opt) = apply_deposit ctxt deposit in

  let* () =
    match (result, withdrawal_opt) with
    | (Deposit_success indexes, None) ->
        check_indexes [(addr1, index_exn 0l)] [(ticket1, index_exn 0l)] indexes
    | _ -> unexpected_result
  in

  (* We create a transaction for each transfer, it makes the test of each
     transaction result easier. *)
  let transaction1 =
    [transfer ~counter:1L ~signer:pk1 ~dest:(l2addr addr2) ~ticket:ticket1 10L]
  in
  let signature1 = sign_transaction [sk1] transaction1 in
  let transaction2 =
    [transfer ~counter:2L ~signer:pk1 ~dest:(l2addr addr3) ~ticket:ticket1 20L]
  in
  let signature2 = sign_transaction [sk1] transaction2 in

  let transaction3 =
    [transfer ~counter:3L ~signer:pk1 ~dest:(l2addr addr4) ~ticket:ticket1 30L]
  in
  let signature3 = sign_transaction [sk1] transaction3 in
  let batch =
    batch
      (List.concat [signature1; signature2; signature3])
      [transaction1; transaction2; transaction3]
  in

  let* (_ctxt, Batch_result {indexes; _}, _withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  let* () =
    check_indexes
      [(addr2, index_exn 1l); (addr3, index_exn 2l); (addr4, index_exn 3l)]
      []
      indexes
  in

  return_unit

let test_indexes_creation_bad () =
  let open Context_l2.Syntax in
  let ctxt = empty_context in
  let contracts = gen_n_address 3 in

  let (sk1, pk1, addr1) = nth_exn contracts 0 in
  let (_, _, addr2) = nth_exn contracts 1 in
  let (_, _, addr3) = nth_exn contracts 2 in

  let deposit =
    {
      sender = pkh;
      destination = value addr1;
      ticket_hash = ticket1;
      amount = Tx_rollup_l2_qty.of_int64_exn 20L;
    }
  in
  let* (ctxt, _, _withdrawal_opt) = apply_deposit ctxt deposit in

  let transaction1 =
    (* This transaction will fail because the number of tickets required is
       more than its own. *)
    [
      transfer
        ~counter:1L
        ~signer:pk1
        ~dest:(l2addr addr2)
        ~ticket:ticket1
        10000L;
    ]
  in
  let signature1 = sign_transaction [sk1] transaction1 in
  let transaction2 =
    (* This is ok *)
    [transfer ~counter:2L ~signer:pk1 ~dest:(l2addr addr3) ~ticket:ticket1 1L]
  in
  let signature2 = sign_transaction [sk1] transaction2 in

  let batch =
    batch (List.concat [signature1; signature2]) [transaction1; transaction2]
  in

  let* (ctxt, Batch_result {results; indexes}, _withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  (* Only the indexes from the second transaction should exist, the first
     should have failed *)
  let* () =
    match results with
    | [(_t1, Transaction_failure _); (_t2, Transaction_success)] -> return_unit
    | _ -> assert false
  in

  let* () = check_indexes [(addr3, index_exn 1l)] [] indexes in

  let* idx = Address_index.get ctxt addr2 in
  assert (idx = None) ;

  let* idx = Address_index.get ctxt addr3 in
  assert (idx = Some (index_exn 1l)) ;

  return_unit

(** The test consists of [addr1] sending [ticket1] to [addr2].
    In exchange [addr2] will send [ticket2] to [addr1]. We check both
    the transaction's status and the balances afterwards. *)
let test_simple_l2_transaction () =
  let open Context_l2.Syntax in
  let* (ctxt, tidxs, accounts) =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  let (sk1, pk1, addr1, idx1, _) = nth_exn accounts 0 in
  let (sk2, pk2, addr2, idx2, _) = nth_exn accounts 1 in

  (* Then, we build a transaction with:
     [addr1] -> [addr2] & [addr2] -> [addr1]. *)
  let transaction =
    transfers
      [
        (pk1, l2addr addr2, ticket1, 10L, None);
        (pk2, l2addr addr1, ticket2, 20L, None);
      ]
  in
  let batch = create_batch_v1 [transaction] [[sk1; sk2]] in

  let* (ctxt, Batch_result {results; _}, _withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  let status = nth_exn results 0 |> snd in

  match (status, _withdrawals) with
  | (Transaction_success, []) ->
      (* Check the balance after the transaction has been applied, we omit
         the check the indexes to not pollute this test. *)
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket1"
          "addr1.ticket1 should be emptied"
          tidx1
          idx1
          0L
      in
      let* () =
        check_balance
          ctxt
          "addr2"
          "ticket1"
          "addr2.ticket1 should be credited"
          tidx1
          idx2
          10L
      in

      let* () =
        check_balance
          ctxt
          "addr2"
          "ticket2"
          "addr2.ticket2 should be emptied"
          tidx2
          idx2
          0L
      in
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket2"
          "addr1.ticket2 should be credited"
          tidx2
          idx1
          20L
      in
      return_unit
  | (Transaction_success, _) -> fail_msg "Did not expect any withdrawals"
  | (Transaction_failure _, _) -> fail_msg "The transaction should be a success"

(** The test consists of [pk1] sending [ticket1] to [pkh2].
    This results in a withdrawal. *)
let test_simple_l1_transaction () =
  let open Context_l2.Syntax in
  let* (ctxt, tidxs, accounts) =
    with_initial_setup [ticket1] [[(ticket1, 10L)]; []]
  in

  let tidx1 = nth_exn tidxs 0 in

  let (sk1, pk1, _addr1, idx1, _pkh1) = nth_exn accounts 0 in
  let (_sk2, _pk2, _addr2, _idx2, pkh2) = nth_exn accounts 1 in

  (* Then, we build a transaction with:
     [addr1] -> [pkh2] *)
  let transaction = transfers [(pk1, l1addr pkh2, ticket1, 10L, None)] in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* (ctxt, Batch_result {results; _}, withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  let status = nth_exn results 0 |> snd in

  match (status, withdrawals) with
  | (Transaction_success, [withdrawal]) ->
      (* Check the balance after the transaction has been applied, we omit
         the check the indexes to not pollute this test. *)
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket1"
          "addr1.ticket1 should be emptied"
          tidx1
          idx1
          0L
      in
      Alcotest.(
        check
          eq_withdrawal
          "Resulting withdrawal from L2->L1 transfer"
          withdrawal
          {
            claimer = pkh2;
            ticket_hash = ticket1;
            amount = Tx_rollup_l2_qty.of_int64_exn 10L;
          }) ;
      return_unit
  | (Transaction_success, _) -> fail_msg "Expected exactly one withdrawal"
  | (Transaction_failure _, _) -> fail_msg "The transaction should be a success"

(** Test that [Missing_ticket] is raised if a transfer is attempted to
    a ticket absent from the rollup. *)
let test_l1_transaction_inexistant_ticket () =
  let open Context_l2.Syntax in
  (* empty context *)
  let* (ctxt, _tidxs, accounts) = with_initial_setup [] [[]; []] in

  let (sk1, pk1, _addr1, _idx1, _pkh1) = nth_exn accounts 0 in
  let (_sk2, _pk2, _addr2, _idx2, pkh2) = nth_exn accounts 1 in

  (* We build an invalid transaction with: [addr1] -> [pkh2] *)
  let transaction = transfers [(pk1, l1addr pkh2, ticket1, 10L, None)] in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* (_ctxt, Batch_result {results; _}, withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  (* Expect no withdrawals *)
  Alcotest.(
    check
      (list eq_withdrawal)
      "Resulting withdrawal from L2->L1 transfer"
      withdrawals
      []) ;

  (* Expect error returned *)
  let status = nth_exn results 0 |> snd in
  expect_error_status
    ~msg:"an invalid transaction must fail"
    (Tx_rollup_l2_apply.Missing_ticket ticket1)
    status
    return_unit

(** If the signer of a L2->L1 transaction does not exist (has no balance),
    then batch application fails with Balance_too_low. *)
let test_l1_transaction_inexistant_signer () =
  let open Context_l2.Syntax in
  let* (ctxt, _tidxs, accounts) =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let (_sk1, _pk1, _addr1, _idx1, _pkh1) = nth_exn accounts 0 in
  let (_sk2, _pk2, _addr2, _idx2, pkh2) = nth_exn accounts 1 in
  let (sk_unknown, pk_unknown, _) = gen_l2_address () in

  (* Then, we build an invalid transaction with:
     [pk_unknown] -> [pkh2] *)
  let transaction = transfers [(pk_unknown, l1addr pkh2, ticket1, 10L, None)] in
  let batch = create_batch_v1 [transaction] [[sk_unknown]] in

  let* (_ctxt, Batch_result {results; _}, withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  (* Expect no withdrawals *)
  Alcotest.(
    check
      (list eq_withdrawal)
      "Resulting withdrawal from L2->L1 transfer"
      withdrawals
      []) ;

  (* Expect error returned *)
  let status = nth_exn results 0 |> snd in
  expect_error_status
    ~msg:"an invalid transaction must fail"
    Tx_rollup_l2_context_sig.Balance_too_low
    status
    return_unit

(** Test that [Balance_too_low] is raised if a transfer is attempted with a
    quantity superior to the senders balance. *)
let test_l1_transaction_overdraft () =
  let open Context_l2.Syntax in
  let initial_balances = [[(ticket1, 10L)]; [(ticket2, 20L)]] in
  let* (ctxt, tidxs, accounts) =
    with_initial_setup [ticket1; ticket2] initial_balances
  in

  let (sk1, pk1, _addr1, idx1, _pkh1) = nth_exn accounts 0 in
  let (_sk2, _pk2, _addr2, idx2, pkh2) = nth_exn accounts 1 in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  (* Then, we build an transaction with: [addr1] -> [pkh2] where addr1 attempts to spend too much*)
  let transaction = transfers [(pk1, l1addr pkh2, ticket1, 30L, None)] in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* (ctxt, Batch_result {results; _}, withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  (* Expect no withdrawals *)
  Alcotest.(
    check
      (list eq_withdrawal)
      "Resulting withdrawal from L2->L1 transfer"
      withdrawals
      []) ;

  (* Expect error returned *)
  let status = nth_exn results 0 |> snd in
  expect_error_status
    ~msg:"an invalid transaction must fail"
    Tx_rollup_l2_context_sig.Balance_too_low
    status
    (let* () =
       check_balance
         ctxt
         "addr1"
         "ticket1"
         "addr1.ticket1 should be unchanged"
         tidx1
         idx1
         10L
     in
     let* () =
       check_balance
         ctxt
         "addr2"
         "ticket1"
         "addr2.ticket1 should be unchanged"
         tidx2
         idx2
         20L
     in

     let* () =
       check_balance
         ctxt
         "addr2"
         "ticket2"
         "addr1.ticket2 should be unchanged (empty)"
         tidx2
         idx1
         0L
     in
     let* () =
       check_balance
         ctxt
         "addr1"
         "ticket2"
         "addr2.ticket1 should be unchanged (empty)"
         tidx1
         idx2
         0L
     in
     return_unit)

(** Test that withdrawals with quantity zero are possible.

    TODO: https://gitlab.com/tezos/tezos/-/issues/2593
    Should they be possible?
 *)
let test_l1_transaction_zero () =
  let open Context_l2.Syntax in
  let initial_balances = [[(ticket1, 10L)]; [(ticket2, 20L)]] in
  let* (ctxt, tidxs, accounts) =
    with_initial_setup [ticket1; ticket2] initial_balances
  in

  let (sk1, pk1, _addr1, idx1, _pkh1) = nth_exn accounts 0 in
  let (_sk2, _pk2, _addr2, idx2, pkh2) = nth_exn accounts 1 in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  (* Then, we build an transaction with: [addr1] -> [pkh2] with amount 0 *)
  let transaction = transfers [(pk1, l1addr pkh2, ticket1, 0L, None)] in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* (ctxt, Batch_result {results; _}, withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  (* Expect one zero-withdrawal *)
  Alcotest.(
    check
      (list eq_withdrawal)
      "Resulting withdrawal from L2->L1 transfer"
      withdrawals
      [{claimer = pkh2; ticket_hash = ticket1; amount = Tx_rollup_l2_qty.zero}]) ;

  match results with
  | [([_], Transaction_success)] ->
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket1"
          "addr1.ticket1 should be unchanged"
          tidx1
          idx1
          10L
      in
      let* () =
        check_balance
          ctxt
          "addr2"
          "ticket1"
          "addr2.ticket2 should be unchanged"
          tidx2
          idx2
          20L
      in

      let* () =
        check_balance
          ctxt
          "addr2"
          "ticket2"
          "addr1.ticket2 should be unchanged (empty)"
          tidx2
          idx1
          0L
      in
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket2"
          "addr2.ticket1 should be unchanged (empty)"
          tidx1
          idx2
          0L
      in
      return_unit
  | _ -> fail_msg "Zero-transactions should be successful"

(** Test partial L2 to L1 transaction. Ensure that a withdrawal is emitted
    for the transferred amount and that the remainder is in the sender's
    account. *)
let test_l1_transaction_partial () =
  let open Context_l2.Syntax in
  let* (ctxt, tidxs, accounts) =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let (sk1, pk1, _addr1, idx1, _pkh1) = nth_exn accounts 0 in
  let (_sk2, _pk2, _addr2, idx2, pkh2) = nth_exn accounts 1 in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  (* Then, we build an transaction with: [addr1] -> [pkh2] , addr1 spending the ticket partially *)
  let transaction = transfers [(pk1, l1addr pkh2, ticket1, 5L, None)] in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* (ctxt, Batch_result {results; _}, withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  (* Expect one partial withdrawal *)
  Alcotest.(
    check
      (list eq_withdrawal)
      "Resulting withdrawal from L2->L1 transfer"
      withdrawals
      [
        {
          claimer = pkh2;
          ticket_hash = ticket1;
          amount = Tx_rollup_l2_qty.of_int64_exn 5L;
        };
      ]) ;

  match results with
  | [([_], Transaction_success)] ->
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket1"
          "addr1.ticket1 should be debited"
          tidx1
          idx1
          5L
      in
      let* () =
        check_balance
          ctxt
          "addr2"
          "ticket1"
          "addr2.ticket2 should be unchanged"
          tidx2
          idx2
          20L
      in

      let* () =
        check_balance
          ctxt
          "addr2"
          "ticket2"
          "addr1.ticket2 should be unchanged (empty)"
          tidx2
          idx1
          0L
      in
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket2"
          "addr2.ticket1 should be unchanged (empty)"
          tidx1
          idx2
          0L
      in
      return_unit
  | _ -> fail_msg "Zero-transactions should be successful"

(** Test that a valid transaction containing both indexes and values is a
    success. *)
let test_transaction_with_unknown_indexable () =
  let open Context_l2.Syntax in
  let open Tx_rollup_l2_batch.V1 in
  let* (ctxt, tidxs, accounts) =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  let (sk1, pk1, addr1, aidx1, _) = nth_exn accounts 0 in
  let (sk2, pk2, addr2, aidx2, _) = nth_exn accounts 1 in

  (* Note that {!with_initial_setup} does not initialize metadatas for the
     public keys. If it was the case, we could not use this function
     to test the pre processing of operations during the application of a
     batch.
  *)
  let* ctxt = Address_metadata.init_with_public_key ctxt aidx1 pk1 in
  let* ctxt = Address_metadata.init_with_public_key ctxt aidx2 pk2 in

  let transfer1 : (Indexable.unknown, Indexable.unknown) operation =
    {
      signer = from_value pk1;
      counter = 1L;
      contents =
        [
          {
            destination = Layer2 (forget aidx2);
            ticket_hash = from_value ticket1;
            qty = Tx_rollup_l2_qty.of_int64_exn 5L;
          };
          {
            destination = Layer2 (from_value addr2);
            ticket_hash = forget tidx1;
            qty = Tx_rollup_l2_qty.of_int64_exn 5L;
          };
        ];
    }
  in
  let transfer2 : (Indexable.unknown, Indexable.unknown) operation =
    {
      signer = signer_of_address_index aidx2 |> Indexable.forget;
      counter = 1L;
      contents =
        [
          {
            destination = Layer2 (forget aidx1);
            ticket_hash = from_value ticket2;
            qty = Tx_rollup_l2_qty.of_int64_exn 10L;
          };
          {
            destination = Layer2 (from_value addr1);
            ticket_hash = forget tidx2;
            qty = Tx_rollup_l2_qty.of_int64_exn 10L;
          };
        ];
    }
  in

  let transaction = [transfer1; transfer2] in
  let signatures = sign_transaction [sk1; sk2] transaction in
  let batch = batch signatures [transaction] in

  let* (ctxt, Batch_result {results; _}, withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  let status = nth_exn results 0 |> snd in

  match (status, withdrawals) with
  | (Transaction_success, []) ->
      (* Check the balance after the transaction has been applied, we omit
         the check the indexes to not pollute this test. *)
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket1"
          "addr1.ticket1 should be emptied"
          tidx1
          aidx1
          0L
      in
      let* () =
        check_balance
          ctxt
          "addr2"
          "ticket1"
          "addr2.ticket1 should be credited"
          tidx1
          aidx2
          10L
      in

      let* () =
        check_balance
          ctxt
          "addr2"
          "ticket2"
          "addr2.ticket2 should be emptied"
          tidx2
          aidx2
          0L
      in
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket2"
          "addr1.ticket2 should be credited"
          tidx2
          aidx1
          20L
      in
      return_unit
  | (Transaction_success, _) -> fail_msg "Did not expect any withdrawals"
  | (Transaction_failure _, _) -> fail_msg "The transaction should be a success"

(** Test that a transaction containing at least one invalid operation
    fails and does not change the context. It is similar to
    {!test_simple_l2_transaction} but the second addr does not
    possess the tickets. *)
let test_invalid_transaction () =
  let open Context_l2.Syntax in
  let* (ctxt, tidxs, accounts) =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; []]
  in

  let tidx1 = nth_exn tidxs 0 in

  let (sk1, pk1, addr1, idx1, _) = nth_exn accounts 0 in
  let (sk2, pk2, addr2, idx2, _) = nth_exn accounts 1 in

  (* Then, we build a transaction with:
     [addr1] -> [addr2] & [addr2] -> [addr1]. *)
  let transaction =
    transfers
      [
        (pk1, l2addr addr2, ticket1, 10L, None);
        (pk2, l2addr addr1, ticket2, 20L, None);
      ]
  in
  let batch = create_batch_v1 [transaction] [[sk1; sk2]] in

  let* (ctxt, Batch_result {results; _}, _withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  let status = nth_exn results 0 |> snd in

  let* () =
    expect_error_status
      ~msg:"an invalid transaction must fail"
      Tx_rollup_l2_context_sig.Balance_too_low
      status
      (let* () =
         check_balance
           ctxt
           "addr1"
           "ticket1"
           "addr1.ticket1 has not changed"
           tidx1
           idx1
           10L
       in
       let* () =
         check_balance
           ctxt
           "addr2"
           "ticket1"
           "addr2.ticket1 has not changed"
           tidx1
           idx2
           0L
       in

       return_unit)
  in
  return_unit

(** Test that submitting an invalid counter fails. *)
let test_invalid_counter () =
  let open Context_l2.Syntax in
  let* (ctxt, _, accounts) = with_initial_setup [ticket1] [[]] in

  let (sk1, pk1, addr1, _idx1, _) = nth_exn accounts 0 in

  let counter = 10L in
  let transaction =
    transfers [(pk1, l2addr addr2, ticket1, 10L, Some counter)]
  in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* (_ctxt, Batch_result {results; _}, _withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  let status = nth_exn results 0 |> snd in

  let* () =
    expect_error_status
      ~msg:"the invalid counter must be detected"
      (Tx_rollup_l2_apply.Counter_mismatch
         {account = addr1; expected = 0L; provided = counter})
      status
      return_unit
  in
  return_unit

(** Test that submitting a transaction updates the counters (expect when
    the batch is incorrectly signed). *)
let test_update_counter () =
  let open Context_l2.Syntax in
  let* (ctxt, _, accounts) = with_initial_setup [ticket1] [[]] in

  let (sk1, pk1, _addr1, _idx1, _) = nth_exn accounts 0 in

  let transactions =
    transfers
      [
        (pk1, l2addr addr2, ticket1, 10L, Some 1L);
        (pk1, l2addr addr2, ticket1, 20L, Some 2L);
        (pk1, l2addr addr2, ticket1, 30L, Some 3L);
        (pk1, l2addr addr2, ticket1, 40L, Some 4L);
        (pk1, l2addr addr2, ticket1, 50L, Some 5L);
      ]
    |> List.map (fun x -> [x])
  in

  let batch =
    create_batch_v1 transactions [[sk1]; [sk1]; [sk1]; [sk1]; [sk1]]
  in

  let* (ctxt, Batch_result {results; _}, withdrawals) =
    Batch_V1.apply_batch ctxt batch
  in

  let status = nth_exn results 0 |> snd in

  match (status, withdrawals) with
  | ( Transaction_failure
        {reason = Tx_rollup_l2_apply.Incorrect_aggregated_signature; _},
      _ ) ->
      fail_msg "This test should not raise [Incorrect_aggregated_signature]"
  | _ ->
      let* () =
        check_metadata
          ctxt
          "addr1"
          "the counter should have been incremented"
          5L
          pk1
      in
      return_unit

let test_pre_apply_batch () =
  let open Context_l2.Syntax in
  let* (ctxt, _tidxs, accounts) =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let (sk1, pk1, addr1, _idx1, _) = nth_exn accounts 0 in
  let (sk2, pk2, addr2, _idx2, _) = nth_exn accounts 1 in

  let transaction =
    transfers
      [
        (pk1, l2addr addr2, ticket1, 10L, None);
        (pk2, l2addr addr1, ticket2, 20L, None);
      ]
  in
  let batch1 = create_batch_v1 [transaction] [[sk1; sk2]] in
  let* (ctxt, _indexes, _) = Batch_V1.check_signature ctxt batch1 in

  let* () =
    check_metadata
      ctxt
      "pk1"
      "check_signature must have created a metadata"
      0L
      pk1
  in
  let* () =
    check_metadata
      ctxt
      "pk1"
      "check_signature must have created a metadata"
      0L
      pk2
  in

  (* We can now produce invalid signatures and expect a failure. *)
  let batch2 = create_batch_v1 [transaction] [[sk1; sk1]] in
  let* () =
    expect_error
      ~msg_if_valid:"The check should fail with an invalid signature"
      (Batch_V1.check_signature ctxt batch2)
      Incorrect_aggregated_signature
  in

  return_unit

(** Deposits and batches are tested individually in the tests above, we now
    test that the toplevel functions correctly calls the subsequent functions.
*)

let test_apply_message_batch () =
  let open Context_l2.Syntax in
  let* (ctxt, _, accounts) =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let (sk1, pk1, addr1, _, _) = nth_exn accounts 0 in
  let (sk2, pk2, addr2, _, _) = nth_exn accounts 1 in

  (* Then, we build a transaction with:
     [addr1] -> [addr2] & [addr2] -> [addr1]. *)
  let transaction =
    transfers
      [
        (pk1, l2addr addr2, ticket1, 10L, None);
        (pk2, l2addr addr1, ticket2, 20L, None);
      ]
  in
  let batch = create_batch_v1 [transaction] [[sk1; sk2]] in
  let (msg, _) =
    Tx_rollup_message.make_batch
      (Data_encoding.Binary.to_string_exn
         Tx_rollup_l2_batch.encoding
         (V1 batch))
  in

  let* (_ctxt, result) = apply_message ctxt msg in

  match result with
  | (Message_result.Batch_V1_result _, []) ->
      (* We do not check the result inside as we consider it is
         covered by other tests. *)
      return_unit
  | _ -> fail_msg "Invalid apply message result"

(** Test a batch of transfers where some of the transfers will emit
   withdrawals. *)
let test_apply_message_batch_withdrawals () =
  let open Context_l2.Syntax in
  let* (ctxt, tidxs, accounts) =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let (sk1, pk1, addr1, idx1, pkh1) = nth_exn accounts 0 in
  let (sk2, pk2, addr2, idx2, pkh2) = nth_exn accounts 1 in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  (* Then, we build a transaction with:
       - [pk1] -> [addr2]
       - [pk1] -> [pkh2] (-> withdrawal)
       - [pk2] -> [addr1]
       - [pk2] -> [pkh1] (-> withdrawal)
  *)
  let transactions =
    [
      transfers [(pk1, l2addr addr2, ticket1, 5L, Some 1L)];
      transfers [(pk1, l1addr pkh2, ticket1, 5L, Some 2L)];
      transfers [(pk2, l2addr addr1, ticket2, 10L, Some 1L)];
      transfers [(pk2, l1addr pkh1, ticket2, 10L, Some 2L)];
    ]
  in
  let batch = create_batch_v1 transactions [[sk1]; [sk1]; [sk2]; [sk2]] in
  let (msg, _) =
    Tx_rollup_message.make_batch
      (Data_encoding.Binary.to_string_exn
         Tx_rollup_l2_batch.encoding
         (V1 batch))
  in

  let* (ctxt, result) = apply_message ctxt msg in

  match result with
  | ( Message_result.Batch_V1_result
        (Message_result.Batch_V1.Batch_result
          {
            results =
              [
                (_, Transaction_success);
                (_, Transaction_success);
                (_, Transaction_success);
                (_, Transaction_success);
              ];
            _;
          }),
      withdrawals ) ->
      Alcotest.(
        check
          (list eq_withdrawal)
          "Resulting withdrawal from L2->L1 batch"
          withdrawals
          [
            {
              claimer = pkh2;
              ticket_hash = ticket1;
              amount = Tx_rollup_l2_qty.of_int64_exn 5L;
            };
            {
              claimer = pkh1;
              ticket_hash = ticket2;
              amount = Tx_rollup_l2_qty.of_int64_exn 10L;
            };
          ]) ;
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket1"
          "addr1.ticket1 should be spent"
          tidx1
          idx1
          0L
      in
      let* () =
        check_balance
          ctxt
          "addr2"
          "ticket1"
          "addr2.ticket1 should be credited"
          tidx1
          idx2
          5L
      in

      let* () =
        check_balance
          ctxt
          "addr2"
          "ticket2"
          "addr1.ticket2 should be credited"
          tidx2
          idx1
          10L
      in
      let* () =
        check_balance
          ctxt
          "addr1"
          "ticket2"
          "addr2.ticket2 should be spent"
          tidx2
          idx2
          0L
      in
      return_unit
  | ( Message_result.Batch_V1_result
        (Message_result.Batch_V1.Batch_result {results; _}),
      _ ) ->
      let* () =
        if List.length results <> 4 then
          fail_msg
            ("Expected 4 results, got " ^ string_of_int @@ List.length results)
        else return_unit
      in
      List.iter_es
        (fun res ->
          match res with
          | (_, Message_result.Transaction_success) -> return_unit
          | (_, Transaction_failure {index; reason}) ->
              let msg =
                Format.asprintf
                  "Result at position %d unexpectedly failed: %a"
                  index
                  Environment.Error_monad.pp
                  reason
              in
              fail_msg msg)
        results
  | _ -> fail_msg "Unexpected apply message result"

let test_apply_message_deposit () =
  let open Context_l2.Syntax in
  let ctxt = empty_context in
  let amount = 50L in

  let (msg, _) =
    Tx_rollup_message.make_deposit
      pkh
      (value addr1)
      ticket1
      (Tx_rollup_l2_qty.of_int64_exn amount)
  in

  let* (_ctxt, result) = apply_message ctxt msg in

  match result with
  | (Message_result.Deposit_result _, []) ->
      (* We do not check the result inside as we consider it is
         covered by other tests. *)
      return_unit
  | _ -> fail_msg "Invalid apply message result"

let tests =
  wrap_tztest_tests
    [
      ("simple transaction", test_simple_deposit);
      ("returned transaction", test_returned_deposit);
      ("deposit with existing indexes", test_deposit_with_existing_indexes);
      ("test simple l1 transaction", test_simple_l1_transaction);
      ( "test simple l1 transaction: inexistant ticket",
        test_l1_transaction_inexistant_ticket );
      ( "test simple l1 transaction: inexistant signer",
        test_l1_transaction_inexistant_signer );
      ("test simple l1 transaction: overdraft", test_l1_transaction_overdraft);
      ("test simple l1 transaction: zero", test_l1_transaction_zero);
      ("test simple l1 transaction: partial", test_l1_transaction_partial);
      ("test simple l2 transaction", test_simple_l2_transaction);
      ( "test simple transaction with indexes and values",
        test_transaction_with_unknown_indexable );
      ("invalid transaction", test_invalid_transaction);
      ("indexes creation", test_indexes_creation);
      ("indexes creation with failing transactions", test_indexes_creation_bad);
      ("invalid counter", test_invalid_counter);
      ("update counter", test_update_counter);
      ("pre apply batch", test_pre_apply_batch);
      ("apply batch from message", test_apply_message_batch);
      ( "apply batch from message with withdrawals",
        test_apply_message_batch_withdrawals );
      ("apply deposit from message", test_apply_message_deposit);
    ]
