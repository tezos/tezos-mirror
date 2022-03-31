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
  signer : signer;
  transactions : Tx_queue.t;
  mutable incr_context : Context.t;
  lock : Lwt_mutex.t;
  l1_constants : Protocol.Alpha_context.Constants.parametric;
}

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

(** [is_batch_valid] returns whether the batch is valid or not based on
    two criterias:

    The proof produced by the batch interpretation must be smaller than
    [l1_constants.tx_rollup_rejection_max_proof_size]. Otherwise, the associated
    commitment can be rejected because of the size.

    The batch exceeds the [l1_constants.tx_rollup_hard_size_limit_per_message],
    the submit batch operation will fail.
*)
let is_batch_valid ctxt
    (l1_constants : Protocol.Alpha_context.Constants.parametric) batch =
  let open Lwt_result_syntax in
  (* The batch is ok if:
     1. The proof is small enough
     2. The batch is small enough *)
  let batch_size_ok =
    let size = Data_encoding.Binary.length Tx_rollup_l2_batch.encoding batch in
    size <= l1_constants.tx_rollup_hard_size_limit_per_message
  in
  if batch_size_ok then
    let l2_parameters =
      Tx_rollup_l2_apply.
        {
          tx_rollup_max_withdrawals_per_batch =
            l1_constants.tx_rollup_max_withdrawals_per_batch;
        }
    in
    let*! res_interp =
      Interpreter.interpret_batch
        ctxt
        l2_parameters
        ~rejection_max_proof_size:
          l1_constants.tx_rollup_rejection_max_proof_size
        batch
    in
    let b_proof_size = Result.is_ok res_interp in
    return b_proof_size
  else return_false

let get_batches ctxt l1_constants queue =
  let open Lwt_result_syntax in
  let exception
    Batches_finished of {
      rev_batches :
        (Indexable.unknown, Indexable.unknown) Tx_rollup_l2_batch.t list;
      to_remove : L2_transaction.hash list;
    }
  in
  try
    let* (rev_batches, rev_current_trs, to_remove) =
      Tx_queue.fold_es
        (fun tr_hash tr (batches, rev_current_trs, to_remove) ->
          let new_trs = tr :: rev_current_trs in
          let*? batch = L2_transaction.batch (List.rev new_trs) in
          let* b = is_batch_valid ctxt l1_constants batch in
          if b then return (batches, new_trs, tr_hash :: to_remove)
          else
            match rev_current_trs with
            | [_] ->
                (* If only one transaction makes the batch invalid, we remove it
                   from the current transactions and it'll be removed later. *)
                let*! () = Event.(emit Batcher.invalid_transaction) tr in
                return (batches, [], tr_hash :: to_remove)
            | _ ->
                let*? batch = L2_transaction.batch (List.rev rev_current_trs) in
                let new_batches = batch :: batches in
                if
                  List.compare_length_with new_batches max_number_of_batches
                  >= 0
                then
                  (* We created enough batches, we exit the loop *)
                  raise
                    (Batches_finished {rev_batches = new_batches; to_remove})
                else
                  (* We add the batch to the accumulator and we go on. *)
                  let*? batch = L2_transaction.batch [tr] in
                  let* b = is_batch_valid ctxt l1_constants batch in
                  if b then return (new_batches, [tr], tr_hash :: to_remove)
                  else
                    let*! () = Event.(emit Batcher.invalid_transaction) tr in
                    return (new_batches, [], tr_hash :: to_remove))
        queue
        ([], [], [])
    in
    let*? batches =
      let open Result_syntax in
      if rev_current_trs <> [] then
        let+ last_batch = L2_transaction.batch (List.rev rev_current_trs) in
        List.rev (last_batch :: rev_batches)
      else return (List.rev rev_batches)
    in
    return (batches, to_remove)
  with Batches_finished {rev_batches; to_remove} ->
    return (List.rev rev_batches, to_remove)

let batch_and_inject state =
  let open Lwt_result_syntax in
  let* (batches, to_remove) =
    get_batches state.incr_context state.l1_constants state.transactions
  in
  match batches with
  | [] -> return_none
  | _ ->
      let*! () =
        Event.(emit Batcher.batch) (List.length batches, List.length to_remove)
      in
      let*! () = Event.(emit Batcher.inject) () in
      let* oph = inject_batches state batches in
      let*! () = Event.(emit Batcher.injection_success) oph in
      List.iter
        (fun tr_hash -> Tx_queue.remove state.transactions tr_hash)
        to_remove ;
      return_some oph

let async_batch_and_inject state =
  Lwt.async @@ fun () ->
  let open Lwt_syntax in
  (* let* _ = Lwt_unix.sleep 2. in *)
  let* _ = batch_and_inject state in
  return_unit

let init cctxt ~rollup ~signer index l1_constants =
  let open Lwt_result_syntax in
  let*! incr_context = Context.init_context index in
  let+ signer = get_signer cctxt signer in
  Option.map
    (fun signer ->
      {
        cctxt = batcher_context cctxt;
        rollup;
        signer;
        transactions = Tx_queue.create 500_000;
        incr_context;
        lock = Lwt_mutex.create ();
        l1_constants;
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
  let* context =
    if apply then
      let l2_parameters =
        Tx_rollup_l2_apply.
          {
            tx_rollup_max_withdrawals_per_batch =
              state.l1_constants.tx_rollup_max_withdrawals_per_batch;
          }
      in
      let* (new_context, result, _) =
        L2_apply.Batch_V1.apply_batch context l2_parameters batch
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
  let tr_hash = L2_transaction.hash tr in
  Tx_queue.replace state.transactions tr_hash tr ;
  let*! () = Event.(emit Batcher.queue) tr_hash in
  if state.incr_context == prev_context then
    (* Only update internal context if it was not changed due to a head block
       change in the meantime. *)
    state.incr_context <- context ;
  if eager_batch then
    (* TODO/TORU: find better solution as this reduces throughput when we have a
       single key to sign/inject. *)
    async_batch_and_inject state ;
  return tr_hash
