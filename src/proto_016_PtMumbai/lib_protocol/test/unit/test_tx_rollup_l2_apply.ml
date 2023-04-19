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
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/unit/main.exe \
                  -- --file test_tx_rollup_l2_apply.ml
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

let pkh = Tezos_crypto.Signature.Public_key_hash.zero

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
  match Tezos_crypto.Signature.Bls.aggregate_signature_opt signatures with
  | Some res -> res
  | None -> raise (Invalid_argument "aggregate_signature_exn")

let ticket1, ticket2 =
  match gen_n_ticket_hash 2 with [x; y] -> (x, y) | _ -> assert false

let empty_indexes = {address_indexes = []; ticket_indexes = []}

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
    "{counter=%d; public_key=%a}"
    counter
    Tezos_crypto.Signature.Bls.Public_key.pp
    public_key

let eq_metadata = Alcotest.of_pp pp_metadata

let check_metadata ctxt name_account description counter addr pk =
  let open Syntax in
  (* We ignore the created [ctxt] because it should be a get only. *)
  let* _ctxt, _, aidx = Address_index.get_or_associate_index ctxt addr in
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

let pp_withdrawal fmt = function
  | Tx_rollup_withdraw.{claimer; ticket_hash; amount} ->
      Format.fprintf
        fmt
        "{claimer=%a; ticket_hash=%a; amount=%a}"
        Tezos_crypto.Signature.Public_key_hash.pp
        claimer
        Ticket_hash.pp
        ticket_hash
        Tx_rollup_l2_qty.pp
        amount

let eq_withdrawal = Alcotest.of_pp pp_withdrawal

let check_indexes expected_addr_indexes expected_ticket_indexes indexes =
  let open Syntax in
  let strip_indexes l =
    List.map (fun (v, idx) -> (v, Indexable.to_int32 idx)) l
    |> List.sort (fun x y -> Int32.compare (snd x) (snd y))
  in
  Alcotest.(
    check
      (list (pair eq_address int32))
      "indexables address created"
      expected_addr_indexes
      (strip_indexes indexes.address_indexes)) ;
  Alcotest.(
    check
      (list (pair eq_ticket int32))
      "indexables ticket created"
      expected_ticket_indexes
      (strip_indexes indexes.ticket_indexes)) ;
  return ()

(** {3. Helpers to build apply related values. } *)

let with_initial_setup tickets contracts =
  let open Context_l2.Syntax in
  let ctxt = empty_context in

  let* ctxt, rev_tidxs =
    list_fold_left_m
      (fun (ctxt, rev_tidxs) ticket ->
        let* ctxt, _, tidx = Ticket_index.get_or_associate_index ctxt ticket in
        return (ctxt, tidx :: rev_tidxs))
      (ctxt, [])
      tickets
  in
  let tidxs = List.rev rev_tidxs in

  let* ctxt, rev_contracts =
    list_fold_left_m
      (fun (ctxt, rev_contracts) balances ->
        let pkh, _, _ = gen_l1_address () in
        let sk, pk, addr = gen_l2_address () in
        let* ctxt, _, idx = Address_index.get_or_associate_index ctxt addr in

        let* ctxt =
          list_fold_left_m
            (fun ctxt (ticket, qty) ->
              let qty = Tx_rollup_l2_qty.of_int64_exn qty in
              let* ctxt, _, tidx =
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

let operation_content ?(counter = 1L) ~signer content =
  let open Tx_rollup_l2_batch.V1 in
  {signer = from_value signer; counter; contents = [content]}

let transfer ?counter ~signer ~dest ~ticket qty =
  let qty = Tx_rollup_l2_qty.of_int64_exn qty in
  let content =
    Transfer
      {destination = from_value dest; ticket_hash = from_value ticket; qty}
  in
  operation_content ?counter ~signer content

let signer_pk x = Tx_rollup_l2_batch.Bls_pk x

let signer_addr x = Tx_rollup_l2_batch.L2_addr x

let withdraw ?counter ~signer ~dest ~ticket qty =
  let qty = Tx_rollup_l2_qty.of_int64_exn qty in
  let content = Withdraw {destination = dest; ticket_hash = ticket; qty} in
  operation_content ?counter ~signer content

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

(** Takes almost the same parameters as {!transfers} but takes also the
    secret keys to sign and create a valid message. *)
let batch_from_transfers inputs =
  let all_sks =
    List.fold_left
      (fun all_sks input ->
        List.fold_left
          (fun acc (sk, _, _, _, _, _) ->
            if
              List.mem ~equal:Tezos_crypto.Signature.Bls.Secret_key.equal sk acc
            then acc
            else sk :: acc)
          []
          input
        :: all_sks)
      []
      inputs
  in
  let transactions =
    List.fold_left
      (fun transactions input ->
        List.map
          (fun (_, pk, dest, ticket, amount, counter) ->
            transfer ~signer:(signer_pk pk) ~dest ~ticket ?counter amount)
          input
        :: transactions)
      []
      inputs
  in
  let batch = create_batch_v1 (List.rev transactions) (List.rev all_sks) in
  let buf =
    Data_encoding.Binary.to_string_exn Tx_rollup_l2_batch.encoding (V1 batch)
  in
  make_batch buf |> fst

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
  let* ctxt, result, withdrawal_opt = apply_deposit ctxt deposit in

  (* Applying the deposit should create an idx for both [addr1] and [ticket]. *)
  match (result, withdrawal_opt) with
  | Deposit_success indexes, None ->
      let* () = check_indexes [(addr1, 0l)] [(ticket1, 0l)] indexes in
      let* aidx_opt = Address_index.get ctxt addr1 in
      let* aidx = get_opt aidx_opt in

      let* tidx_opt = Ticket_index.get ctxt ticket1 in
      let* tidx = get_opt tidx_opt in

      let* amount' = Context_l2.Ticket_ledger.get ctxt tidx aidx in
      assert (amount = amount') ;

      return_unit
  | _ -> unexpected_result

(** Test that deposit overflow withdraws the amount sent. *)
let test_returned_deposit () =
  let open Context_l2.Syntax in
  let balance = Int64.max_int in
  let* ctxt, tidxs, accounts =
    with_initial_setup [ticket1] [[(ticket1, balance)]]
  in
  let tidx1 = nth_exn tidxs 0 in
  let _sk1, _pk1, addr1, idx1, pkh = nth_exn accounts 0 in

  (* my cup runneth over *)
  let amount = Tx_rollup_l2_qty.one in
  let deposit =
    {sender = pkh; destination = value addr1; ticket_hash = ticket1; amount}
  in
  let* ctxt, result, withdrawal_opt = apply_deposit ctxt deposit in

  (* Applying the deposit will result in a Deposit_failure, an
     unchanged context and a withdrawal of the deposit *)
  match (result, withdrawal_opt) with
  | Deposit_failure Tx_rollup_l2_context_sig.Balance_overflow, Some withdrawal
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
  | Deposit_failure reason, _ ->
      let msg =
        Format.asprintf
          "Unexpected failure for overflowing deposit: %a"
          Environment.Error_monad.pp
          reason
      in
      fail_msg msg
  | Deposit_success _result, _ ->
      fail_msg "Did not expect overflowing deposit to be succesful"

let apply_l2_parameters : Protocol.Tx_rollup_l2_apply.parameters =
  {tx_rollup_max_withdrawals_per_batch = 5}

let apply_l2_batch ctxt batch =
  Batch_V1.apply_batch ctxt apply_l2_parameters batch

let apply_l2_message ctxt msg = apply_message ctxt apply_l2_parameters msg

let test_indexes_creation_bad () =
  let open Context_l2.Syntax in
  let ctxt = empty_context in
  let contracts = gen_n_address 3 in

  let sk1, pk1, addr1 = nth_exn contracts 0 in
  let _, _, addr2 = nth_exn contracts 1 in
  let _, _, addr3 = nth_exn contracts 2 in

  let deposit =
    {
      sender = pkh;
      destination = value addr1;
      ticket_hash = ticket1;
      amount = Tx_rollup_l2_qty.of_int64_exn 20L;
    }
  in
  let* ctxt, _, _withdrawal_opt = apply_deposit ctxt deposit in

  let transaction1 =
    (* This transaction will fail because the number of tickets required is
       more than its own. *)
    [
      transfer
        ~counter:1L
        ~signer:(signer_pk pk1)
        ~dest:addr2
        ~ticket:ticket1
        10000L;
    ]
  in
  let signature1 = sign_transaction [sk1] transaction1 in
  let transaction2 =
    (* This is ok *)
    [
      transfer ~counter:2L ~signer:(signer_pk pk1) ~dest:addr3 ~ticket:ticket1 1L;
    ]
  in
  let signature2 = sign_transaction [sk1] transaction2 in

  let batch =
    batch (List.concat [signature1; signature2]) [transaction1; transaction2]
  in

  let* ctxt, Batch_result {results; indexes}, _withdrawals =
    apply_l2_batch ctxt batch
  in

  (* Only the indexes from the second transaction should exist, the first
     should have failed *)
  let* () =
    match results with
    | [(_t1, Transaction_failure _); (_t2, Transaction_success)] -> return_unit
    | _ -> assert false
  in

  let* () = check_indexes [(addr3, 1l)] [] indexes in

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
  let* ctxt, tidxs, accounts =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  let sk1, pk1, addr1, idx1, _ = nth_exn accounts 0 in
  let sk2, pk2, addr2, idx2, _ = nth_exn accounts 1 in

  (* Then, we build a transaction with:
     [addr1] -> [addr2] & [addr2] -> [addr1]. *)
  let transaction =
    transfers
      [
        (signer_pk pk1, addr2, ticket1, 10L, None);
        (signer_pk pk2, addr1, ticket2, 20L, None);
      ]
  in
  let batch = create_batch_v1 [transaction] [[sk1; sk2]] in

  let* ctxt, Batch_result {results; _}, _withdrawals =
    apply_l2_batch ctxt batch
  in

  let status = nth_exn results 0 |> snd in

  match (status, _withdrawals) with
  | Transaction_success, [] ->
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
  | Transaction_success, _ -> fail_msg "Did not expect any withdrawals"
  | Transaction_failure _, _ -> fail_msg "The transaction should be a success"

(** Test that a signer can be layer2 address. *)
let test_l2_transaction_l2_addr_signer_good () =
  let open Context_l2 in
  let open Syntax in
  let* ctxt, _tidxs, accounts = with_initial_setup [] [[(ticket1, 10L)]; []] in
  let sk1, pk1, addr1, idx1, _pkh1 = nth_exn accounts 0 in
  let _sk2, _pk2, addr2, _idx2, _pkh2 = nth_exn accounts 1 in
  let* ctxt = Address_metadata.init_with_public_key ctxt idx1 pk1 in
  let transfer =
    [transfer ~signer:(signer_addr addr1) ~dest:addr2 ~ticket:ticket1 10L]
  in
  let signature = sign_transaction [sk1] transfer in
  let batch = batch signature [transfer] in
  let* _ctxt, Batch_result {results; indexes = _}, _withdrawals =
    apply_l2_batch ctxt batch
  in
  let status = nth_exn results 0 in
  match status with
  | _, Transaction_success -> return_unit
  | _, Transaction_failure _ -> fail_msg "The transaction should be a success"

(** Test that signing with a layer2 address needs a proper context. *)
let test_l2_transaction_l2_addr_signer_bad () =
  let open Context_l2 in
  let open Syntax in
  let ctxt = empty_context in
  let sk1, pk1, addr1 = gen_l2_address () in
  let _sk2, _pk2, addr2 = gen_l2_address () in
  (* The address has no index in the context *)
  let transfer =
    [transfer ~signer:(signer_addr addr1) ~dest:addr2 ~ticket:ticket1 10L]
  in
  let signature = sign_transaction [sk1] transfer in
  let batch = batch signature [transfer] in
  let* () =
    expect_error
      ~msg_if_valid:"The check should fail with an unknown address"
      (apply_l2_batch ctxt batch)
      (Tx_rollup_l2_apply.Unknown_address addr1)
  in
  (* Now we add the index but the metadata is still missing *)
  let* ctxt, _, idx1 = Address_index.get_or_associate_index ctxt addr1 in
  let* () =
    expect_error
      ~msg_if_valid:"The check should fail with unknown metadata"
      (apply_l2_batch ctxt batch)
      (Tx_rollup_l2_apply.Unallocated_metadata 0l)
  in
  (* Finally we add the metadata and the test pass *)
  let* ctxt = Address_metadata.init_with_public_key ctxt idx1 pk1 in
  let* ctxt, _, tidx = Ticket_index.get_or_associate_index ctxt ticket1 in
  let* ctxt =
    Ticket_ledger.credit ctxt tidx idx1 (Tx_rollup_l2_qty.of_int64_exn 100L)
  in
  let* _ctxt, Batch_result {results; indexes = _}, _withdrawals =
    apply_l2_batch ctxt batch
  in
  let status = nth_exn results 0 in
  match status with
  | _, Transaction_success -> return_unit
  | _, Transaction_failure _ -> fail_msg "The transaction should succeed"

(** The test consists of [pk1] sending [ticket1] to [pkh2].
    This results in a withdrawal. *)
let test_simple_l1_transaction () =
  let open Context_l2.Syntax in
  let* ctxt, tidxs, accounts =
    with_initial_setup [ticket1] [[(ticket1, 10L)]; []]
  in

  let tidx1 = nth_exn tidxs 0 in

  let sk1, pk1, _addr1, idx1, _pkh1 = nth_exn accounts 0 in
  let _sk2, _pk2, _addr2, _idx2, pkh2 = nth_exn accounts 1 in

  (* Then, we build a transaction with:
     [addr1] -> [pkh2] *)
  let withdraw =
    withdraw ~signer:(signer_pk pk1) ~dest:pkh2 ~ticket:ticket1 10L
  in
  let transaction = [withdraw] in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* ctxt, Batch_result {results; _}, withdrawals =
    apply_l2_batch ctxt batch
  in

  let status = nth_exn results 0 |> snd in

  match (status, withdrawals) with
  | Transaction_success, [withdrawal] ->
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
  | Transaction_success, _ -> fail_msg "Expected exactly one withdrawal"
  | Transaction_failure _, _ -> fail_msg "The transaction should be a success"

let rec repeat n f acc = if n <= 0 then acc else repeat (n - 1) f (f n acc)

(** This function crafts a batch containing [nb_withdraws]. Then, it applies it
    on an L2 context, and checks the status, depending on the value of
    [should_succeed] *)
let helper_test_withdrawal_limits_per_batch nb_withdraws ~should_succeed =
  let open Context_l2.Syntax in
  (* create sufficiently many accounts *)
  let accounts = repeat nb_withdraws (fun _i l -> [(ticket1, 2L)] :: l) [] in
  let* ctxt, _tidxs, accounts = with_initial_setup [ticket1] ([] :: accounts) in
  (* destination of withdrawals *)
  let _skD, _pkD, _addrD, _idxD, pkhD = nth_exn accounts 0 in
  (* transfer 1 ticket from [nb_withdraws] accounts to the dest *)
  let transactions, sks =
    repeat
      nb_withdraws
      (fun i (transactions, sks) ->
        let sk, pk, _addr, _idx, _pkh = nth_exn accounts i in
        let withdraw =
          withdraw ~signer:(signer_pk pk) ~dest:pkhD ~ticket:ticket1 1L
        in
        (withdraw :: transactions, sk :: sks))
      ([], [])
  in
  let batch = create_batch_v1 [transactions] [sks] in
  (* apply the batch, and handle the success and error cases *)
  Irmin_storage.Syntax.catch
    (apply_l2_batch ctxt batch)
    (fun _success ->
      if should_succeed then return_unit
      else fail_msg "The transaction should fail")
    (fun error ->
      let expected_error = "Maximum tx-rollup withdraws per message exceeded" in
      let ({title = error_title; _} as _error_info) =
        Error_monad.find_info_of_error (Environment.wrap_tzerror error)
      in
      if should_succeed then
        fail_msg
          ("The transaction should be a success, but failed: " ^ error_title)
      else if error_title <> expected_error then
        fail_msg
        @@ Format.sprintf
             "Expected error %s but got %s"
             expected_error
             error_title
      else return_unit)

(* Three tests that use the helper above *)
let nb_withdrawals_per_batch_below_limit () =
  helper_test_withdrawal_limits_per_batch
    (apply_l2_parameters.tx_rollup_max_withdrawals_per_batch - 1)
    ~should_succeed:true

let nb_withdrawals_per_batch_equals_limit () =
  helper_test_withdrawal_limits_per_batch
    apply_l2_parameters.tx_rollup_max_withdrawals_per_batch
    ~should_succeed:true

let nb_withdrawals_per_batch_above_limit () =
  helper_test_withdrawal_limits_per_batch
    (apply_l2_parameters.tx_rollup_max_withdrawals_per_batch + 1)
    ~should_succeed:false

(** Test that [Missing_ticket] is raised if a transfer is attempted to
    a ticket absent from the rollup. *)
let test_l1_transaction_inexistant_ticket () =
  let open Context_l2.Syntax in
  (* empty context *)
  let* ctxt, _tidxs, accounts = with_initial_setup [] [[]; []] in

  let sk1, pk1, _addr1, _idx1, _pkh1 = nth_exn accounts 0 in
  let _sk2, _pk2, _addr2, _idx2, pkh2 = nth_exn accounts 1 in

  (* We build an invalid transaction with: [addr1] -> [pkh2] *)
  let withdraw =
    withdraw ~signer:(signer_pk pk1) ~dest:pkh2 ~ticket:ticket1 10L
  in
  let transaction = [withdraw] in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* _ctxt, Batch_result {results; _}, withdrawals =
    apply_l2_batch ctxt batch
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
  let* ctxt, _tidxs, accounts =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let _sk1, _pk1, _addr1, _idx1, _pkh1 = nth_exn accounts 0 in
  let _sk2, _pk2, _addr2, _idx2, pkh2 = nth_exn accounts 1 in
  let sk_unknown, pk_unknown, _ = gen_l2_address () in

  (* Then, we build an invalid transaction with:
     [pk_unknown] -> [pkh2] *)
  let withdraw =
    withdraw ~signer:(signer_pk pk_unknown) ~dest:pkh2 ~ticket:ticket1 10L
  in
  let transaction = [withdraw] in
  let batch = create_batch_v1 [transaction] [[sk_unknown]] in

  let* _ctxt, Batch_result {results; _}, withdrawals =
    apply_l2_batch ctxt batch
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
  let* ctxt, tidxs, accounts =
    with_initial_setup [ticket1; ticket2] initial_balances
  in

  let sk1, pk1, _addr1, idx1, _pkh1 = nth_exn accounts 0 in
  let _sk2, _pk2, _addr2, idx2, pkh2 = nth_exn accounts 1 in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  (* Then, we build an transaction with: [addr1] -> [pkh2] where addr1 attempts to spend too much*)
  let withdraw =
    withdraw ~signer:(signer_pk pk1) ~dest:pkh2 ~ticket:ticket1 30L
  in
  let transaction = [withdraw] in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* ctxt, Batch_result {results; _}, withdrawals =
    apply_l2_batch ctxt batch
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

(** Test that withdrawals with quantity zero are not possible. *)
let test_l1_transaction_zero () =
  let open Context_l2.Syntax in
  let initial_balances = [[(ticket1, 10L)]; [(ticket2, 20L)]] in
  let* ctxt, tidxs, accounts =
    with_initial_setup [ticket1; ticket2] initial_balances
  in

  let sk1, pk1, _addr1, idx1, _pkh1 = nth_exn accounts 0 in
  let _sk2, _pk2, _addr2, idx2, pkh2 = nth_exn accounts 1 in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  (* Then, we build an transaction with: [addr1] -> [pkh2] with amount 0 *)
  let withdraw =
    withdraw ~signer:(signer_pk pk1) ~dest:pkh2 ~ticket:ticket1 0L
  in
  let transaction = [withdraw] in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* ctxt, Batch_result {results; _}, withdrawals =
    apply_l2_batch ctxt batch
  in

  (* Expect one zero-withdrawal *)
  Alcotest.(
    check
      (list eq_withdrawal)
      "Resulting withdrawal from L2->L1 transfer"
      []
      withdrawals) ;

  match results with
  | [
   ( [_],
     Transaction_failure
       {index = 0; reason = Tx_rollup_l2_apply.Invalid_zero_transfer} );
  ] ->
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
  | _ -> fail_msg "Zero-transactions should be a failure"

(** Test partial L2 to L1 transaction. Ensure that a withdrawal is emitted
    for the transferred amount and that the remainder is in the sender's
    account. *)
let test_l1_transaction_partial () =
  let open Context_l2.Syntax in
  let* ctxt, tidxs, accounts =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let sk1, pk1, _addr1, idx1, _pkh1 = nth_exn accounts 0 in
  let _sk2, _pk2, _addr2, idx2, pkh2 = nth_exn accounts 1 in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  (* Then, we build an transaction with: [addr1] -> [pkh2] , addr1 spending the ticket partially *)
  let withdraw =
    withdraw ~signer:(signer_pk pk1) ~dest:pkh2 ~ticket:ticket1 5L
  in
  let transaction = [withdraw] in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* ctxt, Batch_result {results; _}, withdrawals =
    apply_l2_batch ctxt batch
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
  let* ctxt, tidxs, accounts =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let tidx1 = nth_exn tidxs 0 in
  let tidx2 = nth_exn tidxs 1 in

  let sk1, pk1, addr1, aidx1, _ = nth_exn accounts 0 in
  let sk2, pk2, addr2, aidx2, _ = nth_exn accounts 1 in

  (* Note that {!with_initial_setup} does not initialize metadatas for the
     public keys. If it was the case, we could not use this function
     to test the pre processing of operations during the application of a
     batch.
  *)
  let* ctxt = Address_metadata.init_with_public_key ctxt aidx1 pk1 in
  let* ctxt = Address_metadata.init_with_public_key ctxt aidx2 pk2 in

  let transfer1 : (Indexable.unknown, Indexable.unknown) operation =
    {
      signer = from_value (signer_pk pk1);
      counter = 1L;
      contents =
        [
          Transfer
            {
              destination = forget aidx2;
              ticket_hash = from_value ticket1;
              qty = Tx_rollup_l2_qty.of_int64_exn 5L;
            };
          Transfer
            {
              destination = from_value addr2;
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
          Transfer
            {
              destination = forget aidx1;
              ticket_hash = from_value ticket2;
              qty = Tx_rollup_l2_qty.of_int64_exn 10L;
            };
          Transfer
            {
              destination = from_value addr1;
              ticket_hash = forget tidx2;
              qty = Tx_rollup_l2_qty.of_int64_exn 10L;
            };
        ];
    }
  in

  let transaction = [transfer1; transfer2] in
  let signatures = sign_transaction [sk1; sk2] transaction in
  let batch = batch signatures [transaction] in

  let* ctxt, Batch_result {results; _}, withdrawals =
    apply_l2_batch ctxt batch
  in

  let status = nth_exn results 0 |> snd in

  match (status, withdrawals) with
  | Transaction_success, [] ->
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
  | Transaction_success, _ -> fail_msg "Did not expect any withdrawals"
  | Transaction_failure _, _ -> fail_msg "The transaction should be a success"

(** Test that a transaction containing at least one invalid operation
    fails and does not change the context. It is similar to
    {!test_simple_l2_transaction} but the second addr does not
    possess the tickets. *)
let test_invalid_transaction () =
  let open Context_l2.Syntax in
  let* ctxt, tidxs, accounts =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; []]
  in

  let tidx1 = nth_exn tidxs 0 in

  let sk1, pk1, addr1, idx1, _ = nth_exn accounts 0 in
  let sk2, pk2, addr2, idx2, _ = nth_exn accounts 1 in

  (* Then, we build a transaction with:
     [addr1] -> [addr2] & [addr2] -> [addr1]. *)
  let transaction =
    transfers
      [
        (signer_pk pk1, addr2, ticket1, 10L, None);
        (signer_pk pk2, addr1, ticket2, 20L, None);
      ]
  in
  let batch = create_batch_v1 [transaction] [[sk1; sk2]] in

  let* ctxt, Batch_result {results; _}, _withdrawals =
    apply_l2_batch ctxt batch
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
  let* ctxt, _, accounts = with_initial_setup [ticket1] [[]] in

  let sk1, pk1, addr1, _idx1, _ = nth_exn accounts 0 in

  let counter = 10L in
  let transaction =
    transfers [(signer_pk pk1, addr2, ticket1, 10L, Some counter)]
  in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* _ctxt, Batch_result {results; _}, _withdrawals =
    apply_l2_batch ctxt batch
  in

  let status = nth_exn results 0 |> snd in

  let* () =
    expect_error_status
      ~msg:"the invalid counter must be detected"
      (Tx_rollup_l2_apply.Counter_mismatch
         {account = addr1; expected = 1L; provided = counter})
      status
      return_unit
  in
  return_unit

(** Test that submitting a transaction updates the counters (expect when
    the batch is incorrectly signed). *)
let test_update_counter () =
  let open Context_l2.Syntax in
  let* ctxt, _, accounts = with_initial_setup [ticket1] [[]] in

  let sk1, pk1, addr1, _idx1, _ = nth_exn accounts 0 in

  let transactions =
    transfers
      [
        (signer_pk pk1, addr2, ticket1, 10L, Some 1L);
        (signer_pk pk1, addr2, ticket1, 20L, Some 2L);
        (signer_pk pk1, addr2, ticket1, 30L, Some 3L);
        (signer_pk pk1, addr2, ticket1, 40L, Some 4L);
        (signer_pk pk1, addr2, ticket1, 50L, Some 5L);
      ]
    |> List.map (fun x -> [x])
  in

  let batch =
    create_batch_v1 transactions [[sk1]; [sk1]; [sk1]; [sk1]; [sk1]]
  in

  let* ctxt, Batch_result {results; _}, withdrawals =
    apply_l2_batch ctxt batch
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
          addr1
          pk1
      in
      return_unit

let test_pre_apply_batch () =
  let open Context_l2.Syntax in
  let* ctxt, _tidxs, accounts =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let sk1, pk1, addr1, _idx1, _ = nth_exn accounts 0 in
  let sk2, pk2, addr2, _idx2, _ = nth_exn accounts 1 in

  let transaction =
    transfers
      [
        (signer_pk pk1, addr2, ticket1, 10L, None);
        (signer_pk pk2, addr1, ticket2, 20L, None);
      ]
  in
  let batch1 = create_batch_v1 [transaction] [[sk1; sk2]] in
  let* ctxt, _indexes, _ = Batch_V1.check_signature ctxt batch1 in

  let* () =
    check_metadata
      ctxt
      "pk1"
      "check_signature must have created a metadata"
      0L
      addr1
      pk1
  in
  let* () =
    check_metadata
      ctxt
      "pk1"
      "check_signature must have created a metadata"
      0L
      addr2
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
  let* ctxt, _, accounts =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let sk1, pk1, addr1, _, _ = nth_exn accounts 0 in
  let sk2, pk2, addr2, _, _ = nth_exn accounts 1 in

  (* Then, we build a transaction with:
     [addr1] -> [addr2] & [addr2] -> [addr1]. *)
  let transaction =
    transfers
      [
        (signer_pk pk1, addr2, ticket1, 10L, None);
        (signer_pk pk2, addr1, ticket2, 20L, None);
      ]
  in
  let batch = create_batch_v1 [transaction] [[sk1; sk2]] in
  let msg, _ =
    Tx_rollup_message.make_batch
      (Data_encoding.Binary.to_string_exn
         Tx_rollup_l2_batch.encoding
         (V1 batch))
  in

  let* _ctxt, result = apply_l2_message ctxt msg in

  match result with
  | Message_result.Batch_V1_result _, [] ->
      (* We do not check the result inside as we consider it is
         covered by other tests. *)
      return_unit
  | _ -> fail_msg "Invalid apply message result"

(** Test a batch of transfers where some of the transfers will emit
   withdrawals. *)
let test_apply_message_batch_withdrawals () =
  let open Context_l2.Syntax in
  let* ctxt, tidxs, accounts =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in

  let sk1, pk1, addr1, idx1, pkh1 = nth_exn accounts 0 in
  let sk2, pk2, addr2, idx2, pkh2 = nth_exn accounts 1 in

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
      [
        transfer
          ~signer:(signer_pk pk1)
          ~dest:addr2
          ~ticket:ticket1
          ~counter:1L
          5L;
      ];
      [
        withdraw
          ~signer:(signer_pk pk1)
          ~dest:pkh2
          ~ticket:ticket1
          ~counter:2L
          5L;
      ];
      [
        transfer
          ~signer:(signer_pk pk2)
          ~dest:addr1
          ~ticket:ticket2
          ~counter:1L
          10L;
      ];
      [
        withdraw
          ~signer:(signer_pk pk2)
          ~dest:pkh1
          ~ticket:ticket2
          ~counter:2L
          10L;
      ];
    ]
  in
  let batch = create_batch_v1 transactions [[sk1]; [sk1]; [sk2]; [sk2]] in
  let msg, _ =
    Tx_rollup_message.make_batch
      (Data_encoding.Binary.to_string_exn
         Tx_rollup_l2_batch.encoding
         (V1 batch))
  in

  let* ctxt, result = apply_l2_message ctxt msg in

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
          | _, Message_result.Transaction_success -> return_unit
          | _, Transaction_failure {index; reason} ->
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

  let msg, _ =
    Tx_rollup_message.make_deposit
      pkh
      (value addr1)
      ticket1
      (Tx_rollup_l2_qty.of_int64_exn amount)
  in

  let* _ctxt, result = apply_l2_message ctxt msg in

  match result with
  | Message_result.Deposit_result _, [] ->
      (* We do not check the result inside as we consider it is
         covered by other tests. *)
      return_unit
  | _ -> fail_msg "Invalid apply message result"

(** Test an unparsable message. *)
let test_apply_message_unparsable () =
  let open Context_l2.Syntax in
  let* ctxt, _tidxs, _accounts =
    with_initial_setup [ticket1; ticket2] [[(ticket1, 10L)]; [(ticket2, 20L)]]
  in
  let msg, _ =
    Tx_rollup_message.make_batch
      "Yo, let me bust the funky lyrics (You can't parse this)!"
  in
  apply_l2_message ctxt msg >>= fun result ->
  match result with
  | Error Invalid_batch_encoding -> return_unit
  | _ -> fail_msg "Unexpected apply message result"

let test_transfer_to_self () =
  let open Context_l2.Syntax in
  let* ctxt, _, accounts = with_initial_setup [ticket1] [[(ticket1, 10L)]] in
  let sk1, pk1, addr1, _idx1, _ = nth_exn accounts 0 in
  let transaction =
    [transfer ~signer:(signer_pk pk1) ~dest:addr1 ~ticket:ticket1 1L]
  in
  let batch = create_batch_v1 [transaction] [[sk1]] in

  let* _ctxt, Batch_result {results; _}, _withdrawals =
    apply_l2_batch ctxt batch
  in

  let status = nth_exn results 0 in

  match status with
  | ( _,
      Transaction_failure
        {index = 0; reason = Tx_rollup_l2_apply.Invalid_self_transfer} ) ->
      return_unit
  | _, _ ->
      fail_msg "The transaction should have failed with [Invalid_destination]"

module Indexes = struct
  (** The context should be dropped during an invalid deposit, as the
      indexes should be. *)
  let test_drop_on_wrong_deposit () =
    let open Context_l2.Syntax in
    let deposit, _ =
      make_deposit pkh (value addr1) ticket1 Tx_rollup_l2_qty.one
    in
    (* We make the apply fail with an enormous address count *)
    let* ctxt =
      Address_index.Internal_for_tests.set_count empty_context Int32.max_int
    in
    let* ctxt, _ = apply_l2_message ctxt deposit in
    let* ticket_count = Ticket_index.count ctxt in
    Alcotest.(check int32) "Ticket count should not change" 0l ticket_count ;
    (* We make the apply fail with an enormous ticket count *)
    let* ctxt =
      Ticket_index.Internal_for_tests.set_count empty_context Int32.max_int
    in
    let* ctxt, _ = apply_l2_message ctxt deposit in
    let* address_count = Address_index.count ctxt in
    Alcotest.(check int32) "Address count should not change" 0l address_count ;
    return_unit

  (** A deposit should created if they don't exist an index for both the ticket
      and the destination. *)
  let test_creation_on_deposit () =
    let open Context_l2.Syntax in
    let deposit, _ =
      make_deposit pkh (value addr1) ticket1 Tx_rollup_l2_qty.one
    in
    let* ctxt, (result, _) = apply_l2_message empty_context deposit in
    let* ticket_count = Ticket_index.count ctxt in
    Alcotest.(check int32) "Ticket count should change" 1l ticket_count ;
    let* address_count = Address_index.count ctxt in
    Alcotest.(check int32) "Address count should change" 1l address_count ;
    match result with
    | Deposit_result (Deposit_success indexes) ->
        check_indexes [(addr1, 0l)] [(ticket1, 0l)] indexes
    | _ -> fail_msg "Should be a success"

  (** Deposit tickets in the layer2 does not create new indexes if they already
      existed. *)
  let test_deposit_with_existing_indexes () =
    let open Context_l2.Syntax in
    let* ctxt, _, _ =
      Address_index.get_or_associate_index empty_context addr1
    in
    let* ctxt, _, _ = Ticket_index.get_or_associate_index ctxt ticket1 in
    let deposit, _ =
      make_deposit pkh (value addr1) ticket1 Tx_rollup_l2_qty.one
    in
    let* _, (result, _) = apply_l2_message ctxt deposit in
    match result with
    | Deposit_result (Deposit_success indexes) -> check_indexes [] [] indexes
    | _ -> fail_msg "Should be a success"

  let test_creation_on_valid_batch () =
    let open Context_l2.Syntax in
    let contracts = gen_n_address 3 in
    let sk1, pk1, addr1 = nth_exn contracts 0 in
    let _, _, addr2 = nth_exn contracts 1 in
    let _, _, addr3 = nth_exn contracts 2 in
    let deposit, _ =
      make_deposit
        (Obj.magic pk1)
        (value addr1)
        ticket1
        (Tx_rollup_l2_qty.of_int64_exn 10L)
    in
    let* ctxt, _ = apply_l2_message empty_context deposit in
    let batch =
      batch_from_transfers
        [
          [(sk1, pk1, addr2, ticket1, 1L, Some 1L)];
          [(sk1, pk1, addr3, ticket1, 1L, Some 2L)];
        ]
    in
    let* _, (result, _) = apply_l2_message ctxt batch in
    match result with
    | Batch_V1_result (Batch_result {indexes; _}) ->
        check_indexes [(addr2, 1l); (addr3, 2l)] [] indexes
    | _ -> assert false

  let test_drop_on_wrong_batch () =
    let open Context_l2.Syntax in
    let contracts = gen_n_address 4 in
    let sk1, pk1, addr1 = nth_exn contracts 0 in
    let sk2, pk2, addr2 = nth_exn contracts 1 in
    let _, _, addr3 = nth_exn contracts 2 in
    let _, _, addr4 = nth_exn contracts 3 in
    let deposit, _ =
      make_deposit
        (Obj.magic pk1)
        (value addr1)
        ticket1
        (Tx_rollup_l2_qty.of_int64_exn 10L)
    in
    let* ctxt, _ = apply_l2_message empty_context deposit in
    let batch =
      batch_from_transfers
        [
          (* This will be valid and create an index for [addr2] *)
          [(sk1, pk1, addr2, ticket1, 1L, Some 1L)];
          (* This will be wrong and must not create an index for [addr3] *)
          [(sk1, pk1, addr3, ticket1, 1L, Some 1L)];
          (* This has a valid transfer and an invalid one, both
             indexes must be dropped. *)
          [
            (sk1, pk1, addr4, ticket1, 1L, Some 1L);
            (sk2, pk2, addr3, ticket1, 1L, Some 1L);
          ];
        ]
    in
    let* _ctxt, (result, _) = apply_l2_message ctxt batch in
    match result with
    | Batch_V1_result (Batch_result {indexes; _}) ->
        check_indexes [(addr2, 1l)] [] indexes
    | _ -> assert false

  let tests =
    [
      ("indexes are dropped on deposit failure", test_drop_on_wrong_deposit);
      ("indexes are created on deposit success", test_creation_on_deposit);
      ("deposit with existing indexes", test_deposit_with_existing_indexes);
      ( "indexes are created on valid transfers transaction",
        test_creation_on_valid_batch );
      ( "indexes are dropped on invalid transfers transaction",
        test_drop_on_wrong_batch );
    ]
end

let tests =
  wrap_tztest_tests
    ([
       ("simple transaction", test_simple_deposit);
       ("returned transaction", test_returned_deposit);
       ("simple l1 transaction", test_simple_l1_transaction);
       ( "simple l1 transaction: inexistant ticket",
         test_l1_transaction_inexistant_ticket );
       ( "simple l1 transaction: inexistant signer",
         test_l1_transaction_inexistant_signer );
       ("simple l1 transaction: overdraft", test_l1_transaction_overdraft);
       ("simple l1 transaction: zero", test_l1_transaction_zero);
       ("simple l1 transaction: partial", test_l1_transaction_partial);
       ("simple l2 transaction", test_simple_l2_transaction);
       ( "l2 transaction with l2 addr: good",
         test_l2_transaction_l2_addr_signer_good );
       ( "test l2 transaction with l2 addr: bad",
         test_l2_transaction_l2_addr_signer_bad );
       ( "simple transaction with indexes and values",
         test_transaction_with_unknown_indexable );
       ("invalid transaction", test_invalid_transaction);
       ("invalid counter", test_invalid_counter);
       ("update counter", test_update_counter);
       ("pre apply batch", test_pre_apply_batch);
       ("apply batch from message", test_apply_message_batch);
       ( "apply batch from message with withdrawals",
         test_apply_message_batch_withdrawals );
       ("apply deposit from message", test_apply_message_deposit);
       ("apply unparseable message", test_apply_message_unparsable);
       ("transfer to self fail", test_transfer_to_self);
       ( "nb withdrawals per batch below limit",
         nb_withdrawals_per_batch_below_limit );
       ( "nb withdrawals per batch equals limit",
         nb_withdrawals_per_batch_equals_limit );
       ( "nb withdrawals per batch above limit",
         nb_withdrawals_per_batch_above_limit );
     ]
    @ Indexes.tests)

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("tx rollup l2 apply", tests)]
  |> Lwt_main.run
