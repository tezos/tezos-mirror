(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_workers

type parameters = {
  relay_endpoint : Uri.t;
  max_transaction_batch_length : int option;
  inclusion_timeout : float;
}

type callback =
  [`Accepted of Ethereum_types.hash | `Confirmed | `Dropped | `Refused] ->
  unit Lwt.t

type request = {payload : Ethereum_types.hex; callback : callback}

type pending = {callback : callback; since : Ptime.t}

module Pending_transactions = struct
  open Ethereum_types
  module S = String.Hashtbl

  type t = pending S.t

  let empty () = S.create 1000

  let add htbl (Hash (Hex hash)) callback =
    S.add htbl hash ({callback; since = Time.System.now ()} : pending)

  let pop htbl (Hash (Hex hash)) =
    match S.find htbl hash with
    | Some pending ->
        S.remove htbl hash ;
        Some pending
    | None -> None

  let drop timeout htbl =
    let now = Time.System.now () in
    let dropped = ref [] in
    S.filter_map_inplace
      (fun _hash pending ->
        let lifespan = Ptime.diff now pending.since in
        if Ptime.Span.compare lifespan timeout > 0 then (
          dropped := pending :: !dropped ;
          None)
        else Some pending)
      htbl ;
    !dropped
end

type state = {
  relay_endpoint : Uri.t;
  mutable queue : request Queue.t;
  pending : Pending_transactions.t;
  max_transaction_batch_length : int option;
  inclusion_timeout : Ptime.Span.t;
}

module Types = struct
  type nonrec state = state

  type nonrec parameters = parameters
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["floodgate"; "injector"]

  let pp _fmt () = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Inject : {
        payload : Ethereum_types.hex;
        callback : callback;
      }
        -> (unit, tztrace) t
    | Confirm : {txn_hash : Ethereum_types.hash} -> (unit, tztrace) t
    | Tick : (unit, tztrace) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    (* This encoding is used to encode only *)
    union
      [
        case
          Json_only
          ~title:"Inject"
          (obj2
             (req "request" (constant "inject"))
             (req "payload" Ethereum_types.hex_encoding))
          (function
            | View (Inject {payload; _}) -> Some ((), payload) | _ -> None)
          (fun _ -> assert false);
        case
          Json_only
          ~title:"Confirm"
          (obj2
             (req "request" (constant "confirm"))
             (req "transaction_hash" Ethereum_types.hash_encoding))
          (function
            | View (Confirm {txn_hash}) -> Some ((), txn_hash) | _ -> None)
          (fun _ -> assert false);
        case
          Json_only
          ~title:"Tick"
          (obj1 (req "request" (constant "flush")))
          (function View Tick -> Some () | _ -> None)
          (fun _ -> assert false);
      ]

  let pp fmt (View r) =
    let open Format in
    match r with
    | Inject {payload = Hex txn; _} -> fprintf fmt "Inject %s" txn
    | Confirm {txn_hash = Hash (Hex txn_hash)} ->
        fprintf fmt "Confirm %s" txn_hash
    | Tick -> fprintf fmt "Tick"
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let uuid_seed = Random.get_state ()

let send_transactions_batch ~relay_endpoint transactions =
  let open Lwt_result_syntax in
  let module M = Map.Make (String) in
  let module Srt = Rpc_encodings.Send_raw_transaction in
  if Seq.is_empty transactions then return_unit
  else
    let rev_batch, callbacks =
      Seq.fold_left
        (fun (rev_batch, callbacks) {payload; callback} ->
          let req_id = Uuidm.(v4_gen uuid_seed () |> to_string ~upper:false) in
          let txn =
            Rpc_encodings.JSONRPC.
              {
                method_ = Srt.method_;
                parameters =
                  Some (Data_encoding.Json.construct Srt.input_encoding payload);
                id = Some (Id_string req_id);
              }
          in

          (txn :: rev_batch, M.add req_id callback callbacks))
        ([], M.empty)
        transactions
    in
    let batch = List.rev rev_batch in

    let*! () = Floodgate_events.injecting_transactions (List.length batch) in

    let* responses =
      Rollup_services.call_service
        ~keep_alive:true
        ~base:relay_endpoint
        ~timeout:Network_info.timeout
        (Batch.dispatch_batch_service ~path:Resto.Path.root)
        ()
        ()
        (Batch batch)
    in

    let responses =
      match responses with Singleton r -> [r] | Batch rs -> rs
    in

    let* missed_callbacks =
      List.fold_left_es
        (fun callbacks (response : Rpc_encodings.JSONRPC.response) ->
          match response with
          | {id = Some (Id_string req); value} -> (
              match (value, M.find_opt req callbacks) with
              | value, Some callback ->
                  let* () =
                    match value with
                    | Ok res ->
                        let hash =
                          Data_encoding.Json.destruct Srt.output_encoding res
                        in
                        Lwt_result.ok (callback (`Accepted hash))
                    | Error error ->
                        let*! () = Floodgate_events.rpc_error error in
                        Lwt_result.ok (callback `Refused)
                  in
                  return (M.remove req callbacks)
              | _ -> return callbacks)
          | _ -> failwith "Inconsistent response from the server")
        callbacks
        responses
    in

    assert (M.is_empty missed_callbacks) ;
    return_unit

module Handlers = struct
  open Request

  type self = worker

  let on_request : type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun self request ->
    let open Lwt_result_syntax in
    let state = Worker.state self in
    match request with
    | Inject {payload; callback} ->
        let instrumented_callback reason =
          (match reason with
          | `Accepted hash ->
              Pending_transactions.add state.pending hash callback
          | _ -> ()) ;
          callback reason
        in
        Queue.add {payload; callback = instrumented_callback} state.queue ;
        return_unit
    | Confirm {txn_hash} -> (
        match Pending_transactions.pop state.pending txn_hash with
        | Some {callback; _} ->
            Lwt.async (fun () -> callback `Confirmed) ;
            return_unit
        | None -> return_unit)
    | Tick ->
        let all_transactions = Queue.to_seq state.queue in
        let* transactions_to_inject, remaining_transactions =
          match state.max_transaction_batch_length with
          | None -> return (all_transactions, Seq.empty)
          | Some max_transaction_batch_length ->
              let when_negative_length =
                TzTrace.make
                  (Exn (Failure "Negative max_transaction_batch_length"))
              in
              let*? transactions_to_inject =
                Seq.take
                  ~when_negative_length
                  max_transaction_batch_length
                  all_transactions
              in
              let*? remaining_transactions =
                Seq.drop
                  ~when_negative_length
                  max_transaction_batch_length
                  all_transactions
              in
              return (transactions_to_inject, remaining_transactions)
        in
        state.queue <- Queue.of_seq remaining_transactions ;

        let+ () =
          send_transactions_batch
            ~relay_endpoint:state.relay_endpoint
            transactions_to_inject
        in

        let txns =
          Pending_transactions.drop state.inclusion_timeout state.pending
        in
        List.iter
          (fun {callback; _} -> Lwt.async (fun () -> callback `Dropped))
          txns

  type launch_error = tztrace

  let on_launch _self ()
      ({relay_endpoint; max_transaction_batch_length; inclusion_timeout} :
        parameters) =
    let open Lwt_result_syntax in
    return
      {
        relay_endpoint;
        queue = Queue.create ();
        pending = Pending_transactions.empty ();
        max_transaction_batch_length;
        inclusion_timeout =
          Ptime.Span.of_float_s inclusion_timeout
          |> WithExceptions.Option.get ~loc:__LOC__;
      }

  let on_error (type a b) _self _status_request (_r : (a, b) Request.t)
      (_errs : b) : [`Continue | `Shutdown] tzresult Lwt.t =
    Lwt_result_syntax.return `Continue

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

type worker_promise = {
  mutable promise : worker Lwt.t;
  mutable resolver : worker Lwt.u;
}

let worker_promise =
  let promise, resolver = Lwt.task () in
  {promise; resolver}

type error += No_worker

let worker () =
  match Lwt.state worker_promise.promise with
  | Lwt.Return worker -> Ok worker
  | Lwt.Fail e -> Result_syntax.tzfail (error_of_exn e)
  | Lwt.Sleep -> Result_syntax.tzfail No_worker

let tick () =
  let open Lwt_result_syntax in
  let*? worker = worker () in
  let*! (_was_pushed : bool) = Worker.Queue.push_request worker Tick in
  return_unit

let rec beacon ~tick_interval =
  let open Lwt_result_syntax in
  let* () = tick () in
  let*! () = Lwt_unix.sleep tick_interval in
  beacon ~tick_interval

let inject ?(callback = fun _ -> Lwt_syntax.return_unit) txn =
  let open Lwt_syntax in
  let* worker = worker_promise.promise in
  let* (_was_pushed : bool) =
    Worker.Queue.push_request worker (Inject {payload = txn; callback})
  in
  return_unit

let confirm txn_hash =
  let open Lwt_result_syntax in
  let*? worker = worker () in
  let*! (was_pushed : bool) =
    Worker.Queue.push_request worker (Confirm {txn_hash})
  in
  assert was_pushed ;
  return_unit

let start ~relay_endpoint ~max_transaction_batch_length
    ?(inclusion_timeout = 2.) () =
  let open Lwt_result_syntax in
  let* worker =
    Worker.launch
      table
      ()
      {relay_endpoint; max_transaction_batch_length; inclusion_timeout}
      (module Handlers)
  in
  Lwt.wakeup worker_promise.resolver worker ;
  let*! () = Floodgate_events.tx_queue_is_ready () in
  return_unit

let transfer ?(callback = fun _ -> Lwt.return_unit) ?to_ ?(value = Z.zero)
    ?nonce ?data ~gas_limit ~infos ~from () =
  let open Lwt_syntax in
  let fees = Z.(gas_limit * infos.Network_info.base_fee_per_gas) in
  let callback reason =
    (match reason with
    | `Confirmed ->
        Account.debit from Z.(value + fees) ;
        Account.increment_nonce from
    | _ -> ()) ;
    callback reason
  in
  let* txn =
    Craft.transfer_exn ?nonce ~infos ~from ?to_ ~gas_limit ~value ?data ()
  in
  inject ~callback txn

let shutdown () =
  let open Lwt_syntax in
  let worker = worker () |> Result.to_option in
  (* Prepare promise for next restart *)
  let promise, resolver = Lwt.task () in
  worker_promise.promise <- promise ;
  worker_promise.resolver <- resolver ;
  let* () = Option.iter_s Worker.shutdown worker in
  return_unit

module Misc = struct
  let send_raw_transaction ~relay_endpoint txn =
    let open Lwt_result_syntax in
    let receipt, receipt_waker = Lwt.task () in

    let* () =
      send_transactions_batch
        ~relay_endpoint
        List.(
          to_seq
            [
              {
                payload = txn;
                callback =
                  (function
                  | `Accepted hash ->
                      Lwt.return @@ Lwt.wakeup receipt_waker (Ok hash)
                  | `Confirmed ->
                      (* [send_transactions_batch] does not call [callback] with
                         [`Confirmed]. *)
                      assert false
                  | `Refused | `Dropped ->
                      Lwt.return
                      @@ Lwt.wakeup
                           receipt_waker
                           (Error [error_of_fmt "Could not send transaction"]));
              };
            ])
    in

    receipt
end
