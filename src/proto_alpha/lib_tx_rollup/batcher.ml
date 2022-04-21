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
open Batcher_worker_types
module Tx_queue = Hash_queue.Make (L2_transaction.Hash) (L2_transaction)

type state = {
  rollup : Tx_rollup.t;
  constants : Constants.t;
  index : Context.index;
  signer : Signature.public_key_hash;
  transactions : Tx_queue.t;
  mutable incr_context : Context.t;
  lock : Lwt_mutex.t;
}

(* TODO/TORU: remove me *)
let max_number_of_batches = 10

let encode_batch batch =
  Data_encoding.Binary.to_string Tx_rollup_l2_batch.encoding batch
  |> Result.map_error (fun err -> [Data_encoding_wrapper.Encoding_error err])

let inject_batches state batches =
  let open Lwt_result_syntax in
  let*? operations =
    List.map_e
      (fun batch ->
        let open Result_syntax in
        let+ batch_content = encode_batch batch in
        let manager_operation =
          Manager
            (Tx_rollup_submit_batch
               {
                 tx_rollup = state.rollup;
                 content = batch_content;
                 burn_limit = None;
               })
        in
        {
          L1_operation.hash =
            L1_operation.hash_manager_operation manager_operation;
          source = state.signer;
          manager_operation;
        })
      batches
  in
  Injector.add_pending_operations operations

(** [is_batch_valid] returns whether the batch is valid or not based on two
    criteria:

    The proof produced by the batch interpretation must be smaller than
    [constants.parametric.tx_rollup_rejection_max_proof_size]. Otherwise, the
    associated commitment can be rejected because of the size.

    The batch exceeds the
    [constants.parametric.tx_rollup_hard_size_limit_per_message], the submit
    batch operation will fail.  *)
let is_batch_valid ctxt (constants : Constants.t) batch =
  let open Lwt_result_syntax in
  (* The batch is ok if:
     1. The proof is small enough
     2. The batch is small enough *)
  let batch_size_ok =
    let size = Data_encoding.Binary.length Tx_rollup_l2_batch.encoding batch in
    size <= constants.parametric.tx_rollup_hard_size_limit_per_message
  in
  if batch_size_ok then
    let parameters =
      Tx_rollup_l2_apply.
        {
          tx_rollup_max_withdrawals_per_batch =
            constants.parametric.tx_rollup_max_withdrawals_per_batch;
        }
    in
    let*! res_interp =
      Interpreter.interpret_batch
        ctxt
        parameters
        ~rejection_max_proof_size:
          constants.parametric.tx_rollup_rejection_max_proof_size
        batch
    in
    let b_proof_size = Result.is_ok res_interp in
    return b_proof_size
  else return_false

let get_batches ctxt constants queue =
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
          let* b = is_batch_valid ctxt constants batch in
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
                  let* b = is_batch_valid ctxt constants batch in
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

let on_batch state =
  let open Lwt_result_syntax in
  let* (batches, to_remove) =
    get_batches state.incr_context state.constants state.transactions
  in
  match batches with
  | [] -> return_unit
  | _ ->
      let*! () =
        Event.(emit Batcher.batch) (List.length batches, List.length to_remove)
      in
      let* () = inject_batches state batches in
      let*! () = Event.(emit Batcher.batch_success) () in
      List.iter
        (fun tr_hash -> Tx_queue.remove state.transactions tr_hash)
        to_remove ;
      return_unit

let on_register state ~apply (tr : L2_transaction.t) =
  let open Lwt_result_syntax in
  Lwt_mutex.with_lock state.lock @@ fun () ->
  let batch =
    Tx_rollup_l2_batch.V1.
      {contents = [tr.transaction]; aggregated_signature = tr.signature}
  in
  let batch_string =
    Data_encoding.Binary.to_string_exn Tx_rollup_l2_batch.encoding (V1 batch)
  in
  let (_msg, msg_size) = Tx_rollup_message.make_batch batch_string in
  let* () =
    fail_when
      (msg_size
     >= state.constants.parametric.tx_rollup_hard_size_limit_per_message)
      (Error.Transaction_too_large
         {
           actual = msg_size;
           limit =
             state.constants.parametric.tx_rollup_hard_size_limit_per_message;
         })
  in
  let context = state.incr_context in
  let prev_context = context in
  let* context =
    if apply then
      let* (new_context, result, _withdrawals) =
        let parameters =
          Tx_rollup_l2_apply.
            {
              tx_rollup_max_withdrawals_per_batch =
                state.constants.parametric.tx_rollup_max_withdrawals_per_batch;
            }
        in
        L2_apply.Batch_V1.apply_batch context parameters batch
      in
      let open Tx_rollup_l2_apply.Message_result in
      let+ context =
        match result with
        | Batch_V1.Batch_result {results = [(_tr, Transaction_success)]; _} ->
            return new_context
        | Batch_V1.Batch_result
            {results = [(_tr, Transaction_failure {reason; _})]; _} ->
            tzfail (Environment.wrap_tzerror reason)
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
  return tr_hash

let on_new_head state head =
  let open Lwt_result_syntax in
  let+ context = Context.checkout state.index head.L2block.header.context in
  (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2816
     Flush and reapply queue *)
  state.incr_context <- context

let init_batcher_state ~rollup ~signer index constants =
  let open Lwt_syntax in
  let+ incr_context = Context.init_context index in
  {
    rollup;
    index;
    signer;
    constants;
    transactions = Tx_queue.create 500_000;
    incr_context;
    lock = Lwt_mutex.create ();
  }

module Types = struct
  type nonrec state = state

  type parameters = {
    signer : Signature.public_key_hash;
    index : Context.index;
    constants : Constants.t;
  }
end

module Worker = Worker.Make (Name) (Dummy_event) (Request) (Types) (Logger)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  let on_request : type r. worker -> r Request.t -> r tzresult Lwt.t =
   fun w request ->
    let open Lwt_result_syntax in
    let state = Worker.state w in
    match request with
    | Request.Register {tr; apply; eager_batch = _} ->
        let* tr_hash = on_register state ~apply tr in
        return tr_hash
    | Request.Batch -> on_batch state
    | Request.New_head head -> on_new_head state head

  let on_request w r = protect @@ fun () -> on_request w r

  let on_launch _w rollup Types.{signer; index; constants} =
    let open Lwt_result_syntax in
    let*! state = init_batcher_state ~rollup ~signer index constants in
    return state

  let on_error _w r st errs =
    let open Lwt_result_syntax in
    let*! () = Event.(emit Batcher.Worker.request_failed) (r, st, errs) in
    return_unit

  let on_completion _w r _ st =
    match Request.view r with
    | Request.View (Register _ | New_head _) ->
        Event.(emit Batcher.Worker.request_completed_debug) (Request.view r, st)
    | View Batch ->
        Event.(emit Batcher.Worker.request_completed_notice) (Request.view r, st)

  let on_no_request _ = return_unit

  let on_close _w = Lwt.return_unit
end

let table = Worker.create_table Queue

let (worker_promise, worker_waker) = Lwt.task ()

let init ~rollup ~signer index constants =
  let open Lwt_result_syntax in
  let+ worker =
    Worker.launch table rollup {signer; index; constants} (module Handlers)
  in
  Lwt.wakeup worker_waker worker

(* This is a batcher worker for a single tx rollup *)
let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> ok worker
    | Lwt.Fail _ | Lwt.Sleep -> error Error.No_batcher)

let active () =
  match Lwt.state worker_promise with
  | Lwt.Return _ -> true
  | Lwt.Fail _ | Lwt.Sleep -> false

let find_transaction tr_hash =
  let open Result_syntax in
  let+ w = Lazy.force worker in
  let state = Worker.state w in
  Tx_queue.find_opt state.transactions tr_hash

let get_queue () =
  let open Result_syntax in
  let+ w = Lazy.force worker in
  let state = Worker.state w in
  Tx_queue.elements state.transactions

let register_transaction ?(eager_batch = false) ?(apply = true) tr =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    w
    (Request.Register {tr; apply; eager_batch})

let batch () =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  Worker.Queue.push_request_and_wait w Request.Batch

let new_head b =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  let*! () = Worker.Queue.push_request w (Request.New_head b) in
  return_unit
