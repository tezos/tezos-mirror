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
open Common
module Tx_queue = Hash_queue.Make (L2_transaction.Hash) (L2_transaction)

type state = {
  cctxt : Protocol_client_context.full;
  rollup : Tx_rollup.t;
  parameters : Protocol.Tx_rollup_l2_apply.parameters;
  signer : signer;
  transactions : Tx_queue.t;
  mutable incr_context : Context.t;
  lock : Lwt_mutex.t;
}

(* TODO/TORU Change me to correct value and have a configuration option *)
let max_batch_transactions = 10

(* TODO/TORU Change me with bound on size and have a configuration option *)
let max_number_of_batches = 10

let batcher_context (cctxt : #Client_context.full) =
  let log _channel msg = Logs_lwt.info (fun m -> m "%s" msg) in
  object
    inherit
      Protocol_client_context.wrap_full
        (new Client_context.proxy_context (cctxt :> Client_context.full))

    inherit! Client_context.simple_printer log

    method! exit code =
      Format.ksprintf Stdlib.failwith "Batching client wants to exit %d" code
  end

let encode_batch batch =
  Data_encoding.Binary.to_string Tx_rollup_l2_batch.encoding batch
  |> Result.map_error (fun err -> [Data_encoding_wrapper.Encoding_error err])

let update_incr_context state context = state.incr_context <- context

let find_transaction state tr_hash =
  Tx_queue.find_opt state.transactions tr_hash

let get_queue state = Tx_queue.elements state.transactions

let inject_operations (type kind) state
    (operations : kind manager_operation list) =
  let open Lwt_result_syntax in
  let open Annotated_manager_operation in
  let operations =
    List.map
      (fun operation ->
        Annotated_manager_operation
          (Manager_info
             {
               source = None;
               fee = Limit.unknown;
               gas_limit = Limit.unknown;
               storage_limit = Limit.unknown;
               counter = None;
               operation;
             }))
      operations
  in
  let (Manager_list annot_op) =
    Annotated_manager_operation.manager_of_list operations
  in
  (* TODO maybe use something else (e.g. inject directly with correct limits) *)
  let+ (oph, _, _) =
    Injection.inject_manager_operation
      state.cctxt
      ~chain:state.cctxt#chain
      ~block:(`Head 0)
      ~source:state.signer.pkh
      ~src_pk:state.signer.pk
      ~src_sk:state.signer.sk
      ~successor_level:
        true (* Needed to simulate tx_rollup operations in the next block *)
      ~fee:Limit.unknown
      ~gas_limit:Limit.unknown
      ~storage_limit:Limit.unknown
      ~fee_parameter:
        {
          minimal_fees = Tez.of_mutez_exn 100L;
          minimal_nanotez_per_byte = Q.of_int 1000;
          minimal_nanotez_per_gas_unit = Q.of_int 100;
          force_low_fee = false;
          fee_cap = Tez.one;
          burn_cap = Tez.one;
        }
      annot_op
  in
  oph

let inject_batches state batches =
  let open Lwt_result_syntax in
  let*? operations =
    List.map_e
      (fun batch ->
        let open Result_syntax in
        let+ batch_content = encode_batch batch in
        Tx_rollup_submit_batch
          {tx_rollup = state.rollup; content = batch_content; burn_limit = None})
      batches
  in
  inject_operations state operations

let get_batches state =
  let open Result_syntax in
  let transactions =
    Tx_queue.peek_at_most
      state.transactions
      (max_batch_transactions * max_number_of_batches)
  in
  let rec loop acc = function
    | [] -> ok (List.rev acc)
    | trs ->
        let (trs, rest) = List.split_n max_batch_transactions trs in
        let* batch = L2_transaction.batch trs in
        loop (batch :: acc) rest
  in
  let+ batches = loop [] transactions in
  (batches, transactions)

let batch_and_inject ?(at_least_one_full_batch = false) state =
  let open Lwt_result_syntax in
  let*? (batches, to_remove) = get_batches state in
  match batches with
  | [] -> return_none
  | Tx_rollup_l2_batch.(V1 {V1.contents; _}) :: _
    when at_least_one_full_batch
         && List.compare_length_with contents max_batch_transactions < 0 ->
      (* The first batch is not full, and we requested it to be *)
      return_none
  | _ ->
      let+ oph = inject_batches state batches in
      List.iter
        (fun tr -> Tx_queue.remove state.transactions (L2_transaction.hash tr))
        to_remove ;
      Some oph

let async_batch_and_inject ?at_least_one_full_batch state =
  Lwt.async @@ fun () ->
  let open Lwt_syntax in
  (* let* _ = Lwt_unix.sleep 2. in *)
  let* _ = batch_and_inject ?at_least_one_full_batch state in
  return_unit

let init cctxt ~rollup ~signer index parameters =
  let open Lwt_result_syntax in
  let+ signer = get_signer cctxt signer in
  Option.map
    (fun signer ->
      {
        cctxt = batcher_context cctxt;
        rollup;
        signer;
        parameters;
        transactions = Tx_queue.create 500_000;
        incr_context = Context.empty index;
        lock = Lwt_mutex.create ();
      })
    signer

let register_transaction ?(eager_batch = false) ?(apply = true) state
    (tr : L2_transaction.t) =
  let open Lwt_tzresult_syntax in
  Lwt_mutex.with_lock state.lock @@ fun () ->
  let batch =
    Tx_rollup_l2_batch.V1.
      {contents = [tr.transaction]; aggregated_signature = tr.signature}
  in
  let context = state.incr_context in
  let prev_context = context in
  let+ (context, result) =
    if apply then
      let* (new_context, result, _) =
        L2_apply.Batch_V1.apply_batch context state.parameters batch
      in
      let open Tx_rollup_l2_apply.Message_result in
      let+ context =
        match result with
        | Batch_V1.Batch_result {results = [(_tr, Transaction_success)]; _} ->
            return new_context
        | Batch_V1.Batch_result
            {results = [(_tr, Transaction_failure {reason; _})]; _} ->
            fail (Environment.wrap_tzerror reason)
        | _ -> return context
      in
      context
    else return context
  in
  L2_transaction.Hash_queue.add tr ?result state.transactions ;
  if state.incr_context == prev_context then
    (* Only update internal context if it was not changed due to a head block
       change in the meantime. *)
    state.incr_context <- context ;
  if eager_batch then
    (* TODO/TORU: find better solution as this reduces throughput when we have a
       single key to sign/inject. *)
    async_batch_and_inject ~at_least_one_full_batch:true state ;
  L2_transaction.hash tr
