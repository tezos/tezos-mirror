(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type l2_message =
  | Ok_deposit of Tx_rollup_message.t * Tx_rollup_l2_apply.indexes
  | Failing_deposit of {
      message : Tx_rollup_message.t;
      reason : Environment.Error_monad.error;
      withdrawal : Tx_rollup_withdraw.t;
    }
  | Ok_batch of {
      transactions_and_results :
        (( Indexable.unknown,
           Indexable.unknown )
         Tx_rollup_l2_batch.V1.transaction
        * Tx_rollup_l2_apply.Message_result.transaction_result)
        list;
      withdrawals : Tx_rollup_withdraw.t list;
      indexes : Tx_rollup_l2_apply.indexes;
      aggregated_signature : Tx_rollup_l2_batch.V1.signature;
    }
  | Failing_batch of {
      transactions :
        (Indexable.unknown, Indexable.unknown) Tx_rollup_l2_batch.V1.transaction
        list;
      reasons : tztrace;
      aggregated_signature : Tx_rollup_l2_batch.V1.signature;
    }
  | Unparsable_batch of string

type fancy_message = {
  message : l2_message;
  l2_context_hash : Inbox.l2_context_hash;
}

type inbox = fancy_message list

type t = inbox L2block.block

let ticket_hash_value ctxt ticket_hash =
  let open Indexable in
  let open Lwt_syntax in
  match destruct ticket_hash with
  | Left index -> (
      let* ticket_opt = Context.get_ticket ctxt index in
      match ticket_opt with
      | Some Ticket.{hash; _} ->
          return
            (Tx_rollup_l2_context_sig.Ticket_indexable.value hash
            |> Indexable.forget)
      | None -> return ticket_hash)
  | Right _ -> return ticket_hash

let address_value ctxt addr =
  let open Indexable in
  let open Lwt_syntax in
  match destruct addr with
  | Left index -> (
      let* addr_opt = Context.get_address ctxt index in
      match addr_opt with
      | Some addr ->
          return (Tx_rollup_l2_address.Indexable.value addr |> Indexable.forget)
      | None -> return addr)
  | Right _ -> return addr

let signer_value ctxt signer =
  (* After interpretation, the signer always has an associated address *)
  let open Indexable in
  let open Lwt_syntax in
  match destruct signer with
  | Left index -> (
      let index = to_int32 index in
      let index = from_index_exn index in
      let* addr = address_value ctxt index in
      match destruct addr with
      | Right addr -> return (value (Tx_rollup_l2_batch.L2_addr addr) |> forget)
      | Left _ -> assert false)
  | Right Tx_rollup_l2_batch.(L2_addr _) -> return signer
  | Right Tx_rollup_l2_batch.(Bls_pk pk) ->
      let addr = Tx_rollup_l2_address.of_bls_pk pk in
      return (value (Tx_rollup_l2_batch.L2_addr addr) |> forget)

let transaction_replace_indexes ctxt transaction =
  let open Tx_rollup_l2_batch.V1 in
  let open Lwt_syntax in
  let operation_content_replace_index = function
    | Transfer {destination; ticket_hash; qty} ->
        let* ticket_hash = ticket_hash_value ctxt ticket_hash in
        let* destination = address_value ctxt destination in
        return (Transfer {destination; ticket_hash; qty})
    | Withdraw _ as x -> return x
  in
  let operation_replace_indexes {signer; counter; contents} =
    let* contents = List.map_s operation_content_replace_index contents in
    let* signer = signer_value ctxt signer in
    return {signer; counter; contents}
  in
  let* operations = List.map_s operation_replace_indexes transaction in
  return operations

let fancy_message_of_message ctxt Inbox.{message; result; l2_context_hash} =
  let open Lwt_syntax in
  let* (l2_message : l2_message) =
    match (message, result) with
    | Batch s, Discarded tztrace -> (
        let batch_opt =
          Data_encoding.Binary.of_string_opt Tx_rollup_l2_batch.encoding s
        in
        match batch_opt with
        | Some (V1 batch) ->
            return
              (Failing_batch
                 {
                   transactions = batch.contents;
                   reasons = tztrace;
                   aggregated_signature = batch.aggregated_signature;
                 })
        | None -> return (Unparsable_batch s))
    | ( Batch s,
        Interpreted
          (Batch_V1_result (Batch_result {results; indexes}), withdrawals) ) ->
        let (V1 batch) =
          Data_encoding.Binary.of_string_exn Tx_rollup_l2_batch.encoding s
        in
        let _, results = List.split results in
        let* transactions =
          List.map_s (transaction_replace_indexes ctxt) batch.contents
        in
        let transactions_and_results =
          Stdlib.List.combine transactions results
        in
        return
          (Ok_batch
             {
               transactions_and_results;
               withdrawals;
               indexes;
               aggregated_signature = batch.aggregated_signature;
             })
    | Deposit _, Interpreted (Deposit_result (Deposit_success indexes), _) ->
        return (Ok_deposit (message, indexes))
    | ( Deposit _,
        Interpreted (Deposit_result (Deposit_failure error), [withdrawal]) ) ->
        return (Failing_deposit {message; reason = error; withdrawal})
    | _ -> assert false
  in
  return {message = l2_message; l2_context_hash}

let of_l2block ctxt L2block.{hash; header; inbox; commitment} =
  let open Lwt_syntax in
  let+ fancy_inbox = Lwt_list.map_s (fancy_message_of_message ctxt) inbox in
  L2block.{hash; header; inbox = fancy_inbox; commitment}

let indexes_encoding =
  let open Data_encoding in
  let open Tx_rollup_l2_apply in
  let indexable value_encoding index_encoding =
    obj2 (req "value" value_encoding) (req "index" index_encoding)
  in
  conv
    (fun {address_indexes; ticket_indexes} -> (address_indexes, ticket_indexes))
    (fun (address_indexes, ticket_indexes) -> {address_indexes; ticket_indexes})
  @@ obj2
       (req
          "address_indexes"
          (list
             (indexable
                Tx_rollup_l2_address.encoding
                Tx_rollup_l2_address.Indexable.index_encoding)))
       (req
          "ticket_indexes"
          (list
             (indexable
                Ticket_hash.encoding
                Tx_rollup_l2_context_sig.Ticket_indexable.index_encoding)))

(** TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2957
    Copy-pasted from the protocol. *)
let transaction_result_encoding =
  let open Data_encoding in
  let open Tx_rollup_l2_apply.Message_result in
  union
    [
      (let kind = "success" in
       case
         ~title:kind
         (Tag 0)
         (constant kind)
         (function Transaction_success -> Some () | _ -> None)
         (fun () -> Transaction_success));
      (let kind = "failure" in
       case
         ~title:kind
         (Tag 1)
         (obj1
            (req
               kind
               (obj2
                  (req "transaction_index" Data_encoding.int31)
                  (req "reason" Environment.Error_monad.error_encoding))))
         (function
           | Transaction_failure {index; reason} -> Some (index, reason)
           | _ -> None)
         (fun (index, reason) -> Transaction_failure {index; reason}));
    ]

(** TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2957
    Copy-pasted from the protocol. *)
let batch_encoding =
  let open Data_encoding in
  let json = conv Bytes.of_string Bytes.to_string bytes in
  splitted ~json ~binary:string

let l2_message_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"ok_deposit"
        (Tag 0)
        (merge_objs
           Tx_rollup_message.encoding
           (obj2
              (req "result" (constant "success"))
              (req "indexes" indexes_encoding)))
        (function
          | Ok_deposit (deposit, indexes) -> Some (deposit, ((), indexes))
          | _ -> None)
        (fun (deposit, ((), indexes)) -> Ok_deposit (deposit, indexes));
      case
        ~title:"failing_deposit"
        (Tag 1)
        (obj4
           (req "deposit" Tx_rollup_message.encoding)
           (req "result" (constant "failed"))
           (req "reason" Environment.Error_monad.error_encoding)
           (req "withdrawal" Tx_rollup_withdraw.encoding))
        (function
          | Failing_deposit {message; reason; withdrawal} ->
              Some (message, (), reason, withdrawal)
          | _ -> None)
        (fun (message, (), reason, withdrawal) ->
          Failing_deposit {message; reason; withdrawal});
      case
        ~title:"ok_batch"
        (Tag 2)
        (obj4
           (req
              "transactions_and_results"
              (list
                 (obj2
                    (req
                       "transaction"
                       Tx_rollup_l2_batch.V1.transaction_encoding)
                    (req "result" transaction_result_encoding))))
           (req "withdrawals" (list Tx_rollup_withdraw.encoding))
           (req "indexes" indexes_encoding)
           (req "aggregated_signature" Bls.encoding))
        (function
          | Ok_batch
              {
                transactions_and_results;
                withdrawals;
                indexes;
                aggregated_signature;
              } ->
              Some
                ( transactions_and_results,
                  withdrawals,
                  indexes,
                  aggregated_signature )
          | _ -> None)
        (fun ( transactions_and_results,
               withdrawals,
               indexes,
               aggregated_signature ) ->
          Ok_batch
            {
              transactions_and_results;
              withdrawals;
              indexes;
              aggregated_signature;
            });
      case
        ~title:"failing_batch"
        (Tag 3)
        (obj3
           (req
              "transactions"
              (list Tx_rollup_l2_batch.V1.transaction_encoding))
           (req "errors" Error_monad.trace_encoding)
           (req "aggregated_signature" Bls.encoding))
        (function
          | Failing_batch {transactions; reasons; aggregated_signature} ->
              Some (transactions, reasons, aggregated_signature)
          | _ -> None)
        (fun (transactions, reasons, aggregated_signature) ->
          Failing_batch {transactions; reasons; aggregated_signature});
      case
        ~title:"unparsable_batch"
        (Tag 4)
        (obj2
           (req "batch" batch_encoding)
           (req "result" (constant "failed to parse")))
        (function Unparsable_batch s -> Some (s, ()) | _ -> None)
        (fun (s, ()) -> Unparsable_batch s);
    ]

let fancy_message_encoding : fancy_message Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {message; l2_context_hash} -> (message, l2_context_hash))
    (fun (message, l2_context_hash) -> {message; l2_context_hash})
    (obj2
       (req "l2_message" l2_message_encoding)
       (req "l2_context_hash" Inbox.l2_context_hash_encoding))

let inbox_encoding = Data_encoding.list fancy_message_encoding

let encoding = L2block.block_encoding inbox_encoding
