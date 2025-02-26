(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_workers

type parameters = {evm_node_endpoint : Uri.t; config : Configuration.tx_queue}

type queue_variant = [`Accepted | `Refused]

type pending_variant = [`Confirmed | `Dropped]

type all_variant = [queue_variant | pending_variant]

type 'a variant_callback = 'a -> unit Lwt.t

(** tx is in the queue and wait to be injected into the upstream
    node. *)
type queue_request = {
  payload : Ethereum_types.hex;  (** payload of the transaction *)
  queue_callback : queue_variant variant_callback;
      (** callback to call with the response given by the upstream
          node. *)
}

(** tx have been forwarded to the upstream node, now it's pending until confirmed. *)
type pending_request = {
  since : Time.System.t;
      (** time when the transaction was injected into the upstream node. *)
  pending_callback : pending_variant variant_callback;
      (** callback to call when the pending transaction have been confirmed or is dropped. *)
}

type callback = all_variant variant_callback

type request = {
  payload : Ethereum_types.hex;
  tx_object : Ethereum_types.legacy_transaction_object;
  callback : callback;
}

module Tx_object = struct
  open Ethereum_types
  module S = String.Hashtbl

  type t = Ethereum_types.legacy_transaction_object S.t

  let empty ~start_size = S.create start_size

  let add htbl
      (({hash = Hash (Hex hash); _} : Ethereum_types.legacy_transaction_object)
       as tx_object) =
    S.add htbl hash tx_object

  let find htbl (Hash (Hex hash)) = S.find htbl hash

  let remove htbl (Hash (Hex hash)) = S.remove htbl hash
end

module Pending_transactions = struct
  open Ethereum_types
  module S = String.Hashtbl

  type t = pending_request S.t

  let empty ~start_size = S.create start_size

  let add htbl (Hash (Hex hash)) pending_callback =
    S.add
      htbl
      hash
      ({pending_callback; since = Time.System.now ()} : pending_request)

  let pop htbl (Hash (Hex hash)) =
    match S.find htbl hash with
    | Some pending ->
        S.remove htbl hash ;
        Some pending
    | None -> None

  let drop ~max_lifespan htbl =
    let now = Time.System.now () in
    let dropped = ref [] in
    S.filter_map_inplace
      (fun _hash pending ->
        let lifespan = Ptime.diff now pending.since in
        if Ptime.Span.compare lifespan max_lifespan > 0 then (
          dropped := pending :: !dropped ;
          None)
        else Some pending)
      htbl ;
    !dropped
end

type state = {
  evm_node_endpoint : Uri.t;
  mutable queue : queue_request Queue.t;
  pending : Pending_transactions.t;
  tx_object : Tx_object.t;
  config : Configuration.tx_queue;
}

module Types = struct
  type nonrec state = state

  type nonrec parameters = parameters
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node_worker"; "tx_queue"]

  let pp _fmt () = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Inject : request -> (unit, tztrace) t
    | Confirm : {txn_hash : Ethereum_types.hash} -> (unit, tztrace) t
    | Find : {
        txn_hash : Ethereum_types.hash;
      }
        -> (Ethereum_types.legacy_transaction_object option, tztrace) t
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
          (obj1 (req "request" (constant "tick")))
          (function View Tick -> Some () | _ -> None)
          (fun _ -> assert false);
        case
          Json_only
          ~title:"Find"
          (obj2
             (req "request" (constant "find"))
             (req "transaction_hash" Ethereum_types.hash_encoding))
          (function View (Find {txn_hash}) -> Some ((), txn_hash) | _ -> None)
          (fun _ -> assert false);
      ]

  let pp fmt (View r) =
    let open Format in
    match r with
    | Inject {payload = Hex txn; _} -> fprintf fmt "Inject %s" txn
    | Confirm {txn_hash = Hash (Hex txn_hash)} ->
        fprintf fmt "Confirm %s" txn_hash
    | Find {txn_hash = Hash (Hex txn_hash)} -> fprintf fmt "Find %s" txn_hash
    | Tick -> fprintf fmt "Tick"
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let uuid_seed = Random.get_state ()

let send_transactions_batch ~evm_node_endpoint transactions =
  let open Lwt_result_syntax in
  let module M = Map.Make (String) in
  let module Srt = Rpc_encodings.Send_raw_transaction in
  if Seq.is_empty transactions then return_unit
  else
    let rev_batch, callbacks =
      Seq.fold_left
        (fun (rev_batch, callbacks) {payload; queue_callback} ->
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

          (txn :: rev_batch, M.add req_id queue_callback callbacks))
        ([], M.empty)
        transactions
    in
    let batch = List.rev rev_batch in

    let*! () = Tx_queue_events.injecting_transactions (List.length batch) in

    let* responses =
      Rollup_services.call_service
        ~keep_alive:true
        ~base:evm_node_endpoint
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
                    | Ok _hash_encoded -> Lwt_result.ok (callback `Accepted)
                    | Error error ->
                        let*! () = Tx_queue_events.rpc_error error in
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

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun self request ->
    let open Lwt_result_syntax in
    let state = Worker.state self in
    match request with
    | Inject {payload; tx_object; callback} ->
        let pending_callback (reason : pending_variant) =
          let*! () =
            match reason with
            | `Dropped -> Tx_queue_events.transaction_dropped tx_object.hash
            | `Confirmed -> Tx_queue_events.transaction_confirmed tx_object.hash
          in
          Tx_object.remove state.tx_object tx_object.hash ;
          callback (reason :> all_variant)
        in
        let queue_callback reason =
          (match reason with
          | `Accepted ->
              Pending_transactions.add
                state.pending
                tx_object.hash
                pending_callback
          | `Refused -> Tx_object.remove state.tx_object tx_object.hash) ;
          callback (reason :> all_variant)
        in
        Tx_object.add state.tx_object tx_object ;
        Queue.add {payload; queue_callback} state.queue ;
        return_unit
    | Confirm {txn_hash} -> (
        match Pending_transactions.pop state.pending txn_hash with
        | Some {pending_callback; _} ->
            Lwt.async (fun () -> pending_callback `Confirmed) ;
            return_unit
        | None -> return_unit)
    | Find {txn_hash} -> return @@ Tx_object.find state.tx_object txn_hash
    | Tick ->
        let all_transactions = Queue.to_seq state.queue in
        let* transactions_to_inject, remaining_transactions =
          match state.config.max_transaction_batch_length with
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
            ~evm_node_endpoint:state.evm_node_endpoint
            transactions_to_inject
        in

        let txns =
          Pending_transactions.drop
            ~max_lifespan:(Ptime.Span.of_int_s state.config.max_lifespan_s)
            state.pending
        in
        List.iter
          (fun {pending_callback; _} ->
            Lwt.async (fun () -> pending_callback `Dropped))
          txns

  type launch_error = tztrace

  let on_launch _self () ({evm_node_endpoint; config} : parameters) =
    let open Lwt_result_syntax in
    return
      {
        evm_node_endpoint;
        queue = Queue.create ();
        pending = Pending_transactions.empty ~start_size:(config.max_size / 4);
        (* start with /4 and let it grow if necessary to not allocate
           too much at start. *)
        tx_object = Tx_object.empty ~start_size:(config.max_size / 4);
        config;
      }

  let on_error (type a b) _self _status_request (_r : (a, b) Request.t)
      (_errs : b) : [`Continue | `Shutdown] tzresult Lwt.t =
    Lwt_result_syntax.return `Continue

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_worker

type error += Tx_queue_is_closed

let () =
  register_error_kind
    `Permanent
    ~id:"tx_queue_is_closed"
    ~title:"Tx_queue_is_closed"
    ~description:"Failed to add a request to the Tx queue, it's closed."
    Data_encoding.unit
    (function Tx_queue_is_closed -> Some () | _ -> None)
    (fun () -> Tx_queue_is_closed)

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail e -> Result_syntax.tzfail (error_of_exn e)
    | Lwt.Sleep -> Result_syntax.tzfail No_worker)

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Tx_queue_is_closed]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let bind_worker f =
  let open Lwt_result_syntax in
  let res = Lazy.force worker in
  match res with
  | Error [No_worker] ->
      (* There is no worker, nothing to do *)
      return_unit
  | Error errs -> fail errs
  | Ok w -> f w

let push_request worker request =
  let open Lwt_result_syntax in
  let*! (pushed : bool) = Worker.Queue.push_request worker request in
  if not pushed then tzfail Tx_queue_is_closed else return_unit

let tick () = bind_worker @@ fun w -> push_request w Tick

let rec beacon ~tick_interval =
  let open Lwt_result_syntax in
  let* () = tick () in
  let*! () = Lwt_unix.sleep tick_interval in
  beacon ~tick_interval

let inject ?(callback = fun _ -> Lwt_syntax.return_unit)
    (tx_object : Ethereum_types.legacy_transaction_object) txn =
  let open Lwt_syntax in
  let* () = Tx_queue_events.add_transaction tx_object.hash in
  let* worker = worker_promise in
  push_request worker (Inject {payload = txn; tx_object; callback})

let confirm txn_hash =
  bind_worker @@ fun w -> push_request w (Confirm {txn_hash})

let start ~config ~evm_node_endpoint () =
  let open Lwt_result_syntax in
  let* worker =
    Worker.launch table () {evm_node_endpoint; config} (module Handlers)
  in
  Lwt.wakeup worker_waker worker ;
  let*! () = Tx_queue_events.is_ready () in
  return_unit

let find txn_hash =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  Worker.Queue.push_request_and_wait w (Find {txn_hash}) |> handle_request_error

let shutdown () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! () = Tx_queue_events.shutdown () in
  let*! () = Worker.shutdown w in
  return_unit
