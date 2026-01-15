(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  config : Configuration.tx_queue;
  keep_alive : bool;
  timeout : float;
}

type queue_variant = [`Accepted | `Refused]

type pending_variant = [`Confirmed | `Dropped]

type all_variant = Services_backend_sig.callback_status

type 'a variant_callback = 'a Services_backend_sig.variant_callback

type callback = all_variant variant_callback

(** tx is in the queue and wait to be injected into the upstream
    node. *)
type queue_request = {
  hash : Ethereum_types.hash;
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

module Address_nonce = struct
  module S = String.Hashtbl

  (** [t] contains the nonces of transactions from the tx_queue. If an
      address has no transactions in the tx_queue, it will have no
      value here. In other words, the bitset for that address is
      removed when the last transaction from an address is either
      confirmed or dropped. *)
  type t = Nonce_bitset.t S.t

  let empty ~start_size = S.create start_size

  let find nonces ~addr = S.find nonces addr

  let update nonces addr nonce_bitset =
    if Nonce_bitset.is_empty nonce_bitset then S.remove nonces addr
    else S.replace nonces addr nonce_bitset

  let add nonces ~addr ~next_nonce ~nonce ~add =
    let open Result_syntax in
    let nonce_bitset = S.find nonces addr in
    let* nonce_bitset =
      match nonce_bitset with
      | Some nonce_bitset ->
          (* Only shifts if the next_nonce we want to confirm is
             superior of equal to current next nonce.

             Checking here prevents a possible race condition where a
             transaction is submitted a second time and the
             confirmation of the first try is received while the
             validation of that transaction is processed. In that case
             we could add a transaction in the tx_queue that is
             already confirmed. In such rare case the [bitset_nonce]
             would have a [next_nonce] already superior to the given
             one.

             If [nonce_bitset.next_nonce > next_nonce] then there is no
             need to shift because [next_nonce] is already in the past. *)
          if Z.gt nonce_bitset.Nonce_bitset.next_nonce next_nonce then
            return nonce_bitset
          else Nonce_bitset.shift nonce_bitset ~nonce:next_nonce
      | None -> return @@ Nonce_bitset.create ~next_nonce
    in
    let* nonce_bitset =
      (* [add] should take care of only adding [nonce] if
         [bitset_nonce.next_nonce] is inferior or equal. If
         [nonce_bitset.next_nonce > nonce] then there is no need to
         add because [nonce] is already in the past.

         This is follow-up to the previous comment where we are in a
         rare ce condition of the transaction is being validated while
         a transaction with a superior nonce is being confirmed. In
         such case we simply don't register the nonce, and the
         transaction will be dropped by the upstream node when
         receiving it. *)
      add nonce_bitset nonce
    in
    let () = S.replace nonces addr nonce_bitset in
    return_unit

  let confirm_nonce nonces ~addr ~nonce ~next =
    let open Result_syntax in
    let nonce_bitset = S.find nonces addr in
    match nonce_bitset with
    | Some nonce_bitset ->
        let next_nonce = next nonce in
        if Z.gt nonce_bitset.Nonce_bitset.next_nonce next_nonce then
          (* A tx with a superior nonce was already confirmed, nothing
             to confirm.

             This a an unexpected case but if it occurs it's not a
             problem and the tx_queue is not corrupted. *)
          return_unit
        else
          let* nonce_bitset =
            Nonce_bitset.shift nonce_bitset ~nonce:next_nonce
          in
          update nonces addr nonce_bitset ;
          return_unit
    | None -> return_unit

  let remove nonces ~addr ~nonce ~rm =
    let open Result_syntax in
    let nonce_bitset = S.find nonces addr in
    match nonce_bitset with
    | Some nonce_bitset ->
        let* nonce_bitset = rm nonce_bitset nonce in
        update nonces addr nonce_bitset ;
        return_unit
    | None -> return_unit

  let next_gap nonces ~addr ~next_nonce =
    let open Result_syntax in
    let nonce_bitset = S.find nonces addr in
    match nonce_bitset with
    | Some nonce_bitset ->
        Nonce_bitset.shift_then_next_gap nonce_bitset ~shift_nonce:next_nonce
    | None -> return next_nonce
end

module Tx_container
    (Tx : Tx_queue_types.L2_transaction)
    (ChainName : sig
      val chain_name : string
    end) =
struct
  module Transaction_objects = struct
    open Ethereum_types
    module S = String.Hashtbl

    type t = Tx.t S.t

    let empty ~start_size = S.create start_size

    let add htbl tx_object =
      let (Hash (Hex hash)) = Tx.hash_of_tx_object tx_object in
      S.replace htbl hash tx_object

    let mem htbl (Hash (Hex hash)) = S.mem htbl hash

    let find htbl (Hash (Hex hash)) = S.find htbl hash

    let remove htbl (Hash (Hex hash)) = S.remove htbl hash

    let length s = S.length s
  end

  module Pending_transactions = struct
    open Ethereum_types
    module S = String.Hashtbl

    type t = pending_request S.t

    let empty ~start_size = S.create start_size

    let combine_callback callback1 callback2 reason =
      let open Lwt_syntax in
      let* () = callback1 reason in
      callback2 reason

    let add htbl (Hash (Hex hash)) pending_callback =
      let pending = S.find htbl hash in
      match pending with
      | Some {pending_callback = current_callback; since = _} ->
          S.replace
            htbl
            hash
            ({
               pending_callback =
                 combine_callback pending_callback current_callback;
               since = Time.System.now ();
               (* replace to put the real time of adding tx *)
             }
              : pending_request)
      | None ->
          S.replace
            htbl
            hash
            ({pending_callback; since = Time.System.now ()} : pending_request)

    let add_or_update_callback ~max_lifespan htbl (Hash (Hex hash))
        pending_callback =
      let pending = S.find htbl hash in
      match pending with
      | None ->
          let since =
            let now =
              (* starting in the future to let time to the tx to be
                added as pending, else the added callback could be
                removed before the associated tx is added. *)
              Ptime.add_span (Time.System.now ()) max_lifespan
            in
            match now with Some s -> s | None -> assert false
          in
          S.replace htbl hash ({pending_callback; since} : pending_request)
      | Some {pending_callback = current_callback; since} ->
          S.replace
            htbl
            hash
            ({
               pending_callback =
                 combine_callback current_callback pending_callback;
               since;
             }
              : pending_request)

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

    let to_seq = S.to_seq_values

    let clear = S.clear
  end

  module Transactions_per_addr = struct
    module S = String.Hashtbl

    type t = int64 S.t

    let empty ~start_size = S.create start_size

    let remove s a = S.remove s (Tx.address_to_string a)

    let find s a = S.find s (Tx.address_to_string a)

    let add s a i = S.replace s (Tx.address_to_string a) i

    let decrement s address =
      let current = find s address in
      match current with
      | Some i when i <= 1L -> remove s address
      | Some i -> add s address (Int64.pred i)
      | None -> ()

    let increment s address =
      let current = find s address in
      match current with
      | Some i -> add s address (Int64.succ i)
      | None -> add s address 1L

    let length s = S.length s
  end

  module Inflight_transactions = struct
    include Map.Make (String)

    let of_seq s =
      Seq.map (fun (h, txn) -> (Ethereum_types.hash_to_bytes h, txn)) s
      |> of_seq

    let find_opt h = find_opt (Ethereum_types.hash_to_bytes h)

    let remove h = remove (Ethereum_types.hash_to_bytes h)

    let merge = merge (fun _k before after -> Option.either after before)
  end

  type state = {
    mutable queue : queue_request Queue.t;
    mutable waiting_injection : queue_request Inflight_transactions.t;
    pending : Pending_transactions.t;
    tx_object : Transaction_objects.t;
    tx_per_address : Transactions_per_addr.t;
    address_nonce : Address_nonce.t;
    config : Configuration.tx_queue;
    keep_alive : bool;
    mutable locked : bool;
    timeout : float;
  }

  module Types = struct
    type nonrec state = state

    type nonrec parameters = parameters
  end

  module Name = struct
    type t = unit

    let encoding = Data_encoding.unit

    let base = ["evm_node_worker"; "tx_queue"; ChainName.chain_name]

    let pp _fmt () = ()

    let equal () () = true
  end

  module Request = struct
    type request = {
      next_nonce : Ethereum_types.quantity;
      payload : Ethereum_types.hex;
      tx_object : Tx.t;
      callback : callback;
    }

    type ('a, 'b) t =
      | Inject : request -> ((unit, string) result, tztrace) t
      | Add_pending_callback :
          Ethereum_types.hash * [pending_variant | `Missing] variant_callback
          -> (unit, tztrace) t
      | Find : {txn_hash : Ethereum_types.hash} -> (Tx.t option, tztrace) t
      | Nonce : {
          next_nonce : Ethereum_types.quantity;
          address : Tx.address;
        }
          -> (Ethereum_types.quantity, tztrace) t
      | Tick : {
          evm_node_endpoint : Services_backend_sig.endpoint;
        }
          -> (unit, tztrace) t
      | Injection_confirmation : {
          txn_hash : Ethereum_types.hash;
          status : [`Accepted | `Refused];
        }
          -> (unit, tztrace) t
      | Clear : (unit, tztrace) t
      | Lock_transactions : (unit, tztrace) t
      | Unlock_transactions : (unit, tztrace) t
      | Is_locked : (bool, tztrace) t
      | Content : (Transaction_object.txqueue_content, tztrace) t
      | Size_info : (Metrics.Tx_pool.size_info, tztrace) t
      | Pop_transactions : {
          validation_state : 'a;
          validate_tx :
            'a ->
            string ->
            Tx.t ->
            [`Keep of 'a | `Drop of string | `Stop] tzresult Lwt.t;
        }
          -> ((string * Tx.t) list, tztrace) t
      | Confirm_transactions : {
          confirmed_txs : Ethereum_types.hash Seq.t;
          clear_pending_queue_after : bool;
        }
          -> (unit, tztrace) t
      | Dropped_transaction : {
          dropped_tx : Ethereum_types.hash;
          reason : string;
        }
          -> (unit, tztrace) t
      | Drop_stale_transactions : (unit, tztrace) t

    let name (type a b) (t : (a, b) t) =
      match t with
      | Inject _ -> "Inject"
      | Add_pending_callback _ -> "Add_pending_callback"
      | Find _ -> "Find"
      | Nonce _ -> "Nonce"
      | Tick _ -> "Tick"
      | Injection_confirmation _ -> "Injection_confirmation"
      | Clear -> "Clear"
      | Lock_transactions -> "Lock_transactions"
      | Unlock_transactions -> "Unlock_transactions"
      | Is_locked -> "Is_locked"
      | Content -> "Content"
      | Size_info -> "Size_info"
      | Pop_transactions _ -> "Pop_transactions"
      | Confirm_transactions _ -> "Confirm_transactions"
      | Dropped_transaction _ -> "Dropped_transaction"
      | Drop_stale_transactions -> "Drop_stale_transactions"

    type view = View : _ t -> view

    let view req = View req

    let endpoint_encoding =
      let open Data_encoding in
      conv
        (function
          | Services_backend_sig.Rpc uri -> Uri.to_string uri
          | Websocket _ -> "[websocket]"
          | Block_producer -> "[block_producer]")
        (fun _ -> assert false)
        string

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
            ~title:"Add_pending_callback"
            (obj2
               (req "request" (constant "add_pending_callback"))
               (req "hash" Ethereum_types.hex_encoding))
            (function
              | View (Add_pending_callback (Hash hash, _)) -> Some ((), hash)
              | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Tick"
            (obj2
               (req "request" (constant "tick"))
               (req "evm_node_endpoint" endpoint_encoding))
            (function
              | View (Tick {evm_node_endpoint}) -> Some ((), evm_node_endpoint)
              | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Injection_confirmation"
            (obj3
               (req "request" (constant "injection_confirmation"))
               (req "hash" Ethereum_types.hash_encoding)
               (req
                  "status"
                  (string_enum [("accepted", `Accepted); ("refused", `Refused)])))
            (function
              | View (Injection_confirmation {txn_hash; status}) ->
                  Some ((), txn_hash, status)
              | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Find"
            (obj2
               (req "request" (constant "find"))
               (req "transaction_hash" Ethereum_types.hash_encoding))
            (function
              | View (Find {txn_hash}) -> Some ((), txn_hash) | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Clear"
            (obj1 (req "request" (constant "clear")))
            (function View Clear -> Some () | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Nonce"
            (obj3
               (req "request" (constant "nonce"))
               (req "next_nonce" Ethereum_types.quantity_encoding)
               (req "address" Tx.address_encoding))
            (function
              | View (Nonce {next_nonce; address}) ->
                  Some ((), next_nonce, address)
              | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Lock_transactions"
            (obj1 (req "request" (constant "lock_transactions")))
            (function View Lock_transactions -> Some () | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Unlock_transactions"
            (obj1 (req "request" (constant "unlock_transactions")))
            (function View Unlock_transactions -> Some () | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Is_locked"
            (obj1 (req "request" (constant "is_locked")))
            (function View Is_locked -> Some () | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Content"
            (obj1 (req "request" (constant "content")))
            (function View Content -> Some () | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Size_info"
            (obj1 (req "request" (constant "size_info")))
            (function View Size_info -> Some () | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Pop_transactions"
            (obj1 (req "request" (constant "pop_transactions")))
            (function
              | View (Pop_transactions {validation_state = _; validate_tx = _})
                ->
                  Some ()
              | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Confirm_transactions"
            (obj3
               (req "request" (constant "confirm_transactions"))
               (req "confirmed_txs" (list Ethereum_types.hash_encoding))
               (req "clear_pending_queue_after" bool))
            (function
              | View
                  (Confirm_transactions
                     {confirmed_txs; clear_pending_queue_after}) ->
                  Some ((), List.of_seq confirmed_txs, clear_pending_queue_after)
              | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Dropped_transaction"
            (obj3
               (req "request" (constant "dropped_transaction"))
               (req "dropped_tx" Ethereum_types.hash_encoding)
               (req "reason" string))
            (function
              | View (Dropped_transaction {dropped_tx; reason}) ->
                  Some ((), dropped_tx, reason)
              | _ -> None)
            (fun _ -> assert false);
          case
            Json_only
            ~title:"Drop_stale_transactions"
            (obj1 (req "request" (constant "drop_stale_transactions")))
            (function View Drop_stale_transactions -> Some () | _ -> None)
            (fun _ -> assert false);
        ]

    let pp fmt (View r) =
      let open Format in
      match r with
      | Inject {payload = Hex txn; _} -> fprintf fmt "Inject %s" txn
      | Add_pending_callback (Hash (Hex hash), _) ->
          fprintf fmt "Add_pending_callback %s" hash
      | Find {txn_hash = Hash (Hex txn_hash)} -> fprintf fmt "Find %s" txn_hash
      | Tick _ -> fprintf fmt "Tick"
      | Injection_confirmation {txn_hash; status} ->
          fprintf
            fmt
            "Confirm %a was %s"
            Ethereum_types.pp_hash
            txn_hash
            (match status with
            | `Accepted -> "accepted"
            | `Refused -> "refused")
      | Clear -> fprintf fmt "Clear"
      | Nonce {next_nonce = _; address} ->
          fprintf fmt "Nonce %s" (Tx.address_to_string address)
      | Lock_transactions -> Format.fprintf fmt "Locking the transactions"
      | Unlock_transactions -> Format.fprintf fmt "Unlocking the transactions"
      | Is_locked -> Format.fprintf fmt "Checking if the tx queue is locked"
      | Content -> fprintf fmt "Content"
      | Size_info -> fprintf fmt "Size_info"
      | Pop_transactions {validation_state = _; validate_tx = _} ->
          fprintf fmt "Popping transactions with validation function"
      | Confirm_transactions _ -> fprintf fmt "Confirming transactions"
      | Dropped_transaction _ -> fprintf fmt "Dropping transaction"
      | Drop_stale_transactions -> fprintf fmt "Drop_stale_transactions"
  end

  module Worker = Octez_telemetry.Worker.MakeSingle (Name) (Request) (Types)

  type worker = Worker.infinite Worker.queue Worker.t

  let tx_queue_event ?attrs name =
    Opentelemetry_lwt.Event.make
      ?attrs
      Format.(sprintf "%s/%s" (String.concat "." Name.base) name)

  module Handlers = struct
    open Request

    type self = worker

    let uuid_seed = Random.get_state ()

    let build_batch transactions =
      let module M = Map.Make (String) in
      let module Srt = Rpc_encodings.Send_raw_transaction in
      let rev_batch, hashes =
        Seq.fold_left
          (fun (rev_batch, hashes) {hash; payload; _} ->
            Octez_telemetry.Trace.add_event (fun () ->
                tx_queue_event
                  ~attrs:Telemetry.Attributes.[Transaction.hash hash]
                  "selected_transaction") ;
            let req_id =
              Uuidm.(v4_gen uuid_seed () |> to_string ~upper:false)
            in
            let txn =
              Rpc_encodings.JSONRPC.
                {
                  method_ = Srt.method_;
                  parameters =
                    Some
                      (Data_encoding.Json.construct Srt.input_encoding payload);
                  id = Some (Id_string req_id);
                }
            in

            (txn :: rev_batch, M.add req_id hash hashes))
          ([], M.empty)
          transactions
      in
      (List.rev rev_batch, hashes)

    let check_missed_transactions ~self ~hashes ~responses =
      let open Lwt_result_syntax in
      let module M = Map.Make (String) in
      let* missed_transactions =
        List.fold_left_es
          (fun hashes (response : Rpc_encodings.JSONRPC.response) ->
            match response with
            | {id = Some (Id_string req); value} -> (
                match (value, M.find_opt req hashes) with
                | value, Some txn_hash ->
                    let* () =
                      match value with
                      | Ok _hash_encoded ->
                          let*! (_pushed : bool) =
                            Worker.Queue.push_request
                              self
                              (Injection_confirmation
                                 {txn_hash; status = `Accepted})
                          in
                          return_unit
                      | Error error ->
                          let*! () = Tx_queue_events.rpc_error error in
                          let*! (_pushed : bool) =
                            Worker.Queue.push_request
                              self
                              (Injection_confirmation
                                 {txn_hash; status = `Refused})
                          in
                          return_unit
                    in
                    return (M.remove req hashes)
                | _ -> return hashes)
            | _ -> failwith "Inconsistent response from the server")
          hashes
          responses
      in
      assert (M.is_empty missed_transactions) ;
      return_unit

    let build_sequencer_batch ~(state : Types.state) (seq : queue_request Seq.t)
        =
      let open Lwt_result_syntax in
      let rec select rev_hashes rev_selected seq =
        match seq () with
        | Seq.Nil -> return (rev_hashes, rev_selected)
        | Seq.Cons ({hash; payload; queue_callback}, rest) -> (
            let raw_tx = Ethereum_types.hex_to_bytes payload in
            match Transaction_objects.find state.tx_object hash with
            | None ->
                let*! () = Tx_queue_events.missing_tx_object hash in
                let*! () = queue_callback `Refused in
                select rev_hashes rev_selected rest
            | Some tx_object ->
                select
                  (hash :: rev_hashes)
                  ((raw_tx, Tx.to_transaction_object_t tx_object)
                  :: rev_selected)
                  rest)
      in
      let* rev_hashes, rev_selected = select [] [] seq in
      return (List.rev rev_hashes, List.rev rev_selected)

    let send_transactions_batch ~evm_node_endpoint ~timeout ~keep_alive ~state
        self transactions =
      let open Lwt_result_syntax in
      let module M = Map.Make (String) in
      let module Srt = Rpc_encodings.Send_raw_transaction in
      if Seq.is_empty transactions then return_unit
      else
        match evm_node_endpoint with
        | Services_backend_sig.Rpc base ->
            let batch, hashes = build_batch transactions in
            let*! () =
              Tx_queue_events.injecting_transactions (List.length batch)
            in
            let* batch_response =
              Rollup_services.call_service
                ~keep_alive
                ~base
                ~timeout
                (Batch.dispatch_batch_service ~path:Resto.Path.root)
                ()
                ()
                (Batch batch)
            in
            let responses =
              match batch_response with Singleton r -> [r] | Batch rs -> rs
            in
            check_missed_transactions ~self ~hashes ~responses
        | Websocket ws_client ->
            let timeout =
              Websocket_client.
                {
                  timeout;
                  on_timeout = (if keep_alive then `Retry_forever else `Fail);
                }
            in
            let batch, hashes = build_batch transactions in
            let*! () =
              Tx_queue_events.injecting_transactions (List.length batch)
            in
            let* responses =
              List.map_ep
                (fun req ->
                  let*! response =
                    Websocket_client.send_jsonrpc_request ~timeout ws_client req
                  in
                  return Rpc_encodings.JSONRPC.{value = response; id = req.id})
                batch
            in
            check_missed_transactions ~self ~hashes ~responses
        | Block_producer -> (
            let* hashes, transactions =
              build_sequencer_batch ~state transactions
            in
            let*! hashes_and_status =
              Block_producer.preconfirm_transactions ~transactions
            in
            let inject_status status =
              List.iter_p (fun txn_hash ->
                  let open Lwt_syntax in
                  let*! _added =
                    Worker.Queue.push_request
                      self
                      (Injection_confirmation {txn_hash; status})
                  in
                  return_unit)
            in
            match hashes_and_status with
            | Ok {accepted; refused; dropped} ->
                let*! () =
                  Lwt.join
                    [
                      inject_status `Accepted accepted;
                      inject_status `Refused refused;
                      inject_status `Refused dropped;
                    ]
                in
                return_unit
            | Error [Block_producer.IC_disabled] ->
                (* This case should not happens, it means the
                     block_producer worker has ic deactivated and so
                     we should not call it. *)
                let*! () = inject_status `Refused hashes in
                return_unit
            | Error err -> fail err)

    (** clear values and keep the allocated space *)
    let clear
        ({
           queue;
           pending;
           tx_object;
           tx_per_address;
           address_nonce;
           config = _;
           keep_alive = _;
           locked = _;
           waiting_injection = _;
           timeout = _;
         } as state :
          state) =
      (* full matching so when a new element is added to the state it's not
         forgotten to clear it. *)
      String.Hashtbl.clear pending ;
      String.Hashtbl.clear tx_object ;
      String.Hashtbl.clear tx_per_address ;
      String.Hashtbl.clear address_nonce ;
      Queue.clear queue ;
      state.waiting_injection <- Inflight_transactions.empty ;
      ()

    let lock_transactions state = state.locked <- true

    let unlock_transactions state = state.locked <- false

    let is_locked state = state.locked

    let pop_queue_until state ~validation_state ~validate_tx =
      let open Lwt_result_syntax in
      let rec aux validation_state rev_selected =
        match Queue.peek_opt state.queue with
        | None -> return rev_selected
        | Some {hash; payload; queue_callback} -> (
            let raw_tx = Ethereum_types.hex_to_bytes payload in
            let tx_object = Transaction_objects.find state.tx_object hash in
            match tx_object with
            | None ->
                (* Drop that tx because no tx_object associated. this is
                   an inpossible case, we log it to investigate. *)
                let*! () = Tx_queue_events.missing_tx_object hash in
                let _ = Queue.take state.queue in
                let*! () = queue_callback `Refused in
                aux validation_state rev_selected
            | Some tx_object -> (
                let* is_valid = validate_tx validation_state raw_tx tx_object in
                match is_valid with
                | `Stop -> return rev_selected
                (* `Stop means that we don't pop transaction anymore. We
                   don't remove the last peek tx because it could be valid
                   for another call. *)
                | `Drop _ ->
                    (* `Drop, the current tx was evaluated and was refused
                       by the caller. *)
                    let _ = Queue.take state.queue in
                    let*! () = queue_callback `Refused in
                    aux validation_state rev_selected
                | `Keep validation_state ->
                    (* `Keep, the current tx was evaluated and was validated
                       by the caller. *)
                    let _ = Queue.take state.queue in
                    let*! () = queue_callback `Accepted in
                    aux validation_state ((raw_tx, tx_object) :: rev_selected)))
      in
      let* rev_selected = aux validation_state [] in
      return @@ List.rev rev_selected

    let pending_callback state ~tx_nonce ~addr ~tx_object ~caller_callback =
     fun (reason : pending_variant) ->
      let open Lwt_syntax in
      let* res =
        match reason with
        | `Dropped ->
            let* () =
              Tx_queue_events.transaction_dropped
                (Tx.hash_of_tx_object tx_object)
            in
            return
            @@ Address_nonce.remove
                 state.address_nonce
                 ~addr
                 ~nonce:tx_nonce
                 ~rm:Tx.bitset_remove_nonce
        | `Confirmed ->
            let* () =
              Tx_queue_events.transaction_confirmed
                (Tx.hash_of_tx_object tx_object)
            in
            return
            @@ Address_nonce.confirm_nonce
                 state.address_nonce
                 ~addr
                 ~nonce:tx_nonce
                 ~next:Tx.next_nonce
      in
      let* () =
        match res with
        | Ok () -> return_unit
        | Error errs -> Tx_queue_events.callback_error errs
      in
      Transactions_per_addr.decrement
        state.tx_per_address
        (Tx.from_address_of_tx_object tx_object) ;
      Transaction_objects.remove
        state.tx_object
        (Tx.hash_of_tx_object tx_object) ;
      Lwt.dont_wait
        (fun () -> caller_callback (reason :> all_variant))
        (fun exn ->
          Tx_queue_events.callback_error__dont_wait__use_with_care
            [Error_monad.error_of_exn exn]) ;
      Lwt.return_unit

    let queue_callback state ~tx_nonce ~addr ~tx_object ~pending_callback
        ~caller_callback =
     fun (reason : queue_variant) ->
      let open Lwt_syntax in
      let* res =
        match reason with
        | `Accepted ->
            Pending_transactions.add
              state.pending
              (Tx.hash_of_tx_object tx_object)
              pending_callback ;
            return_ok_unit
        | `Refused ->
            Transactions_per_addr.decrement
              state.tx_per_address
              (Tx.from_address_of_tx_object tx_object) ;
            Transaction_objects.remove
              state.tx_object
              (Tx.hash_of_tx_object tx_object) ;
            return
            @@ Address_nonce.remove
                 state.address_nonce
                 ~addr
                 ~nonce:tx_nonce
                 ~rm:Tx.bitset_remove_nonce
      in
      let* () =
        match res with
        | Ok () -> return_unit
        | Error errs -> Tx_queue_events.callback_error errs
      in
      Lwt.dont_wait
        (fun () -> caller_callback (reason :> all_variant))
        (fun exn ->
          Tx_queue_events.callback_error__dont_wait__use_with_care
            [Error_monad.error_of_exn exn]) ;
      Lwt.return_unit

    let add_tx_to_queue state
        {next_nonce; payload; tx_object; callback = caller_callback} =
      let open Lwt_result_syntax in
      Tx_watcher.notify (Tx.hash_of_tx_object tx_object) ;
      let addr =
        Tx.address_to_string (Tx.from_address_of_tx_object tx_object)
      in
      let tx_nonce = Tx.nonce_of_tx_object tx_object in
      let pending_callback =
        pending_callback state ~tx_nonce ~addr ~tx_object ~caller_callback
      in
      let queue_callback =
        queue_callback
          state
          ~tx_nonce
          ~addr
          ~tx_object
          ~pending_callback
          ~caller_callback
      in
      let*! () =
        Tx_queue_events.add_transaction (Tx.hash_of_tx_object tx_object)
      in
      Transactions_per_addr.increment
        state.tx_per_address
        (Tx.from_address_of_tx_object tx_object) ;
      Transaction_objects.add state.tx_object tx_object ;
      let (Qty next_nonce) = next_nonce in
      let*? () =
        Address_nonce.add
          state.address_nonce
          ~addr
          ~next_nonce
          ~nonce:tx_nonce
          ~add:Tx.bitset_add_nonce
      in
      Queue.add
        {hash = Tx.hash_of_tx_object tx_object; payload; queue_callback}
        state.queue ;
      return_unit

    let inject (state : state)
        {next_nonce; payload; tx_object; callback = caller_callback} =
      let open Lwt_result_syntax in
      let hash = Tx.hash_of_tx_object tx_object in
      if Transaction_objects.mem state.tx_object hash then
        (* The transaction object is already known, we have nothing to do *)
        let*! () = Tx_queue_events.transaction_already_present hash in
        return (Ok ())
      else if Compare.Int.(Queue.length state.queue < state.config.max_size)
      then
        (* Check number of txs by user in tx_queue. *)
        let nb_txs_in_queue =
          Transactions_per_addr.find
            state.tx_per_address
            (Tx.from_address_of_tx_object tx_object)
        in
        match nb_txs_in_queue with
        | Some i when i >= state.config.tx_per_addr_limit ->
            let*! () =
              Tx_pool_events.txs_per_user_threshold_reached
                ~address:
                  (Ethereum_types.hex_to_string
                     (Ethereum_types.Hex
                        (Tx.address_to_string
                           (Tx.from_address_of_tx_object tx_object))))
            in
            return
              (Error
                 "Limit of transaction for a user was reached. Transaction is \
                  rejected.")
        | Some _ | None ->
            let* () =
              add_tx_to_queue
                state
                {next_nonce; payload; tx_object; callback = caller_callback}
            in
            return (Ok ())
      else
        return (Error "Transaction limit was reached. Transaction is rejected.")

    let injection_confirmation state txn_hash status =
      match Inflight_transactions.find_opt txn_hash state.waiting_injection with
      | Some {queue_callback; _} ->
          state.waiting_injection <-
            Inflight_transactions.remove txn_hash state.waiting_injection ;
          queue_callback status
      | None -> Lwt.return_unit

    let on_request : type r request_error.
        worker ->
        (r, request_error) Request.t ->
        (r, request_error) result Lwt.t =
     fun self request ->
      let open Lwt_result_syntax in
      let state = Worker.state self in
      match request with
      | Inject tx_info -> protect @@ fun () -> inject state tx_info
      | Add_pending_callback (hash, callback) ->
          protect @@ fun () ->
          if Transaction_objects.mem state.tx_object hash then
            (* it's ok to add pending callbacks for a transaction in
               `state.pending` that is not yet considered pending by
               the tx_queue i.e. not yet in `state.pending`. This is
               because that state is going to be cleaned regularly by
               the tick.

               The existing queue callback for `hash` existing in
               either `state.queue` or `state.inflight_transactions`
               will be either called with:

                - `Refused: in that case the added callback will be
                   later called by `Dropped by the cleaning logic of the
                   tick.

                - `Accepted: in that case the added callback is
                   combined with the existing pending callback associated
                   to hash.

*)
            return
            @@ Pending_transactions.add_or_update_callback
                 ~max_lifespan:(Ptime.Span.of_int_s state.config.max_lifespan_s)
                 state.pending
                 hash
                 (callback :> pending_variant variant_callback)
          else
            let*! () = callback `Missing in
            return_unit
      | Find {txn_hash} ->
          protect @@ fun () ->
          return @@ Transaction_objects.find state.tx_object txn_hash
      | Tick {evm_node_endpoint} ->
          protect @@ fun () ->
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

          state.waiting_injection <-
            Inflight_transactions.merge
              state.waiting_injection
              (Inflight_transactions.of_seq
                 (Seq.map
                    (fun ({hash; _} as txn : queue_request) -> (hash, txn))
                    transactions_to_inject)) ;
          state.queue <- Queue.of_seq remaining_transactions ;

          Lwt.dont_wait
            (fun () ->
              let open Lwt_syntax in
              let* send_result =
                protect @@ fun () ->
                send_transactions_batch
                  ~keep_alive:state.keep_alive
                  ~timeout:state.timeout
                  ~evm_node_endpoint
                  ~state
                  self
                  transactions_to_inject
              in
              match send_result with
              | Ok () -> return_unit
              | Error error ->
                  let* () =
                    Tx_queue_events.injecting_transactions_failed error
                  in
                  (* Something went wrong, we confirm all transactions as refused *)
                  Seq.S.iter
                    (fun {hash; _} ->
                      let* _pushed =
                        Worker.Queue.push_request
                          self
                          (Injection_confirmation
                             {txn_hash = hash; status = `Refused})
                      in
                      return_unit)
                    transactions_to_inject)
            ignore ;

          return_unit
      | Injection_confirmation {txn_hash; status} ->
          Lwt_result.ok (injection_confirmation state txn_hash status)
      | Size_info ->
          protect @@ fun () ->
          return
            Metrics.Tx_pool.
              {
                number_of_transactions =
                  Transaction_objects.length state.tx_object;
                number_of_addresses =
                  Transactions_per_addr.length state.tx_per_address;
              }
      | Clear ->
          protect @@ fun () ->
          clear state ;
          let*! () = Tx_queue_events.cleared () in
          return_unit
      | Nonce {next_nonce; address} ->
          protect @@ fun () ->
          let (Qty next_nonce) = next_nonce in
          let*? next_gap =
            Address_nonce.next_gap
              state.address_nonce
              ~addr:(Tx.address_to_string address)
              ~next_nonce
          in
          return @@ Ethereum_types.Qty next_gap
      | Lock_transactions ->
          protect @@ fun () -> return (lock_transactions state)
      | Unlock_transactions ->
          protect @@ fun () -> return (unlock_transactions state)
      | Is_locked -> protect @@ fun () -> return (is_locked state)
      | Content ->
          protect @@ fun () ->
          let process_transactions tx_map lookup_fn acc =
            String.Hashtbl.fold
              (fun hash value acc ->
                match lookup_fn hash value with
                | Some (obj : Tx.t) ->
                    let existing_nonce_map =
                      Tx.AddressMap.find_opt
                        (Tx.from_address_of_tx_object obj)
                        acc
                      |> Option.value ~default:Ethereum_types.NonceMap.empty
                    in
                    let tx_nonce_opt =
                      Tx.nonce_of_tx_object obj |> Tx.nonce_to_z_opt
                    in
                    let updated_nonce_map =
                      match tx_nonce_opt with
                      | Some tx_nonce ->
                          Ethereum_types.NonceMap.add
                            tx_nonce
                            obj
                            existing_nonce_map
                      | None -> existing_nonce_map
                    in
                    Tx.AddressMap.add
                      (Tx.from_address_of_tx_object obj)
                      updated_nonce_map
                      acc
                | None -> acc)
              tx_map
              acc
          in

          (* Process pending and collect found transactions *)
          let pending =
            process_transactions
              state.pending
              (fun hash _v -> String.Hashtbl.find_opt state.tx_object hash)
              Tx.AddressMap.empty
          in

          (* Process tx_object separately to collect the queued (unmatched) transactions *)
          let queued =
            process_transactions
              state.tx_object
              (fun hash obj ->
                if String.Hashtbl.mem state.pending hash then None else Some obj)
              Tx.AddressMap.empty
          in

          return (Tx.make_txpool ~pending ~queued)
      | Pop_transactions {validation_state; validate_tx} ->
          protect @@ fun () ->
          if is_locked state then return []
          else pop_queue_until state ~validate_tx ~validation_state
      | Confirm_transactions {confirmed_txs; clear_pending_queue_after} ->
          protect @@ fun () ->
          let*! () =
            Seq.S.iter
              (fun hash ->
                (* Maybe we are receiving the transaction from the stream
                   before the RPC actually returned something. We need to check
                   to be sure. *)
                let*! () = injection_confirmation state hash `Accepted in
                let callback = Pending_transactions.pop state.pending hash in
                match callback with
                | Some {pending_callback; _} -> pending_callback `Confirmed
                | None -> Lwt.return_unit)
              confirmed_txs
          in
          if clear_pending_queue_after then (
            let dropped = Pending_transactions.to_seq state.pending in
            let*! () =
              Seq.S.iter
                (fun {pending_callback; _} -> pending_callback `Dropped)
                dropped
            in
            (* Emptying the pending the dropped transactions *)
            Pending_transactions.clear state.pending ;
            return_unit)
          else return_unit
      | Dropped_transaction {dropped_tx; reason = _} ->
          protect @@ fun () ->
          let callback = Pending_transactions.pop state.pending dropped_tx in
          let*! () =
            match callback with
            | Some {pending_callback; _} -> pending_callback `Dropped
            | None -> Lwt.return_unit
          in
          return_unit
      | Drop_stale_transactions ->
          protect @@ fun () ->
          let txns =
            Pending_transactions.drop
              ~max_lifespan:(Ptime.Span.of_int_s state.config.max_lifespan_s)
              state.pending
          in
          let*! () =
            List.iter_s
              (fun {pending_callback; _} -> pending_callback `Dropped)
              txns
          in
          return_unit

    type launch_error = tztrace

    let on_launch _self () ({config; keep_alive; timeout} : parameters) =
      let open Lwt_result_syntax in
      return
        {
          queue = Queue.create ();
          waiting_injection = Inflight_transactions.empty;
          pending = Pending_transactions.empty ~start_size:(config.max_size / 4);
          (* start with /4 and let it grow if necessary to not allocate
             too much at start. *)
          tx_object = Transaction_objects.empty ~start_size:(config.max_size / 4);
          address_nonce = Address_nonce.empty ~start_size:(config.max_size / 10);
          (* start with /10 and let it grow if necessary to not allocate
             too much at start. It's expected to have less different
             addresses than transactions. *)
          tx_per_address = Transactions_per_addr.empty ~start_size:500;
          (* Provide an arbitrary size for the initial hash tables, to
             be revisited if needs be. *)
          config;
          keep_alive;
          locked = false;
          timeout;
        }

    let on_error (type a b) _self _status_request (_r : (a, b) Request.t)
        (_errs : b) : [`Continue | `Shutdown] tzresult Lwt.t =
      Lwt_result_syntax.return `Continue

    let on_completion _ _ _ _ = Lwt.return_unit

    let on_no_request _ = Lwt.return_unit

    let on_close _ = Lwt.return_unit
  end

  type address = Tx.address

  type transaction_object = Tx.t

  let table = Worker.create_table Queue

  let worker_promise, worker_waker = Lwt.task ()

  type error += No_worker

  type error += Tx_queue_is_closed

  let () =
    register_error_kind
      `Permanent
      ~id:(Format.sprintf "tx_queue_%s_is_closed" ChainName.chain_name)
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

  let nonce ~next_nonce address =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    Worker.Queue.push_request_and_wait w (Nonce {next_nonce; address})
    |> handle_request_error

  let inject ?(callback = fun _ -> Lwt_syntax.return_unit) ~next_nonce tx_object
      txn =
    let open Lwt_syntax in
    let* worker = worker_promise in
    Worker.Queue.push_request_and_wait
      worker
      (Inject {next_nonce; payload = txn; tx_object; callback})
    |> handle_request_error

  let add ?callback ~next_nonce tx_object ~raw_tx =
    let open Lwt_result_syntax in
    let* res = inject ?callback ~next_nonce tx_object raw_tx in
    match res with
    | Ok () -> return (Ok (Tx.hash_of_tx_object tx_object))
    | Error errs -> return (Error errs)

  let add_pending_callback hash ~callback =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    let*! (pushed : bool) =
      Worker.Queue.push_request w (Add_pending_callback (hash, callback))
    in
    if pushed then return_unit else failwith "Failed to ad request in tx_queuea"

  let find txn_hash =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    Worker.Queue.push_request_and_wait w (Find {txn_hash})
    |> handle_request_error

  let content () =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    Worker.Queue.push_request_and_wait w Content |> handle_request_error

  let size_info () =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    Worker.Queue.push_request_and_wait w Size_info |> handle_request_error

  let shutdown () =
    let open Lwt_result_syntax in
    bind_worker @@ fun w ->
    let*! () = Tx_queue_events.shutdown () in
    let*! () = Worker.shutdown w in
    return_unit

  let clear () =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    Worker.Queue.push_request_and_wait w Clear |> handle_request_error

  let tx_queue_tick ~evm_node_endpoint =
    bind_worker @@ fun w -> push_request w (Tick {evm_node_endpoint})

  let drop_stale_transactions () =
    bind_worker @@ fun w -> push_request w Drop_stale_transactions

  let tx_queue_beacon ~evm_node_endpoint ~tick_interval =
    let open Lwt_result_syntax in
    let rec loop () =
      let* () = tx_queue_tick ~evm_node_endpoint in
      let* () = drop_stale_transactions () in
      let*! () = Lwt_unix.sleep tick_interval in
      loop ()
    in
    loop ()

  let confirm_transactions ~clear_pending_queue_after ~confirmed_txs =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    Worker.Queue.push_request_and_wait
      w
      (Confirm_transactions {confirmed_txs; clear_pending_queue_after})
    |> handle_request_error

  let dropped_transaction ~dropped_tx ~reason =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    push_request w (Dropped_transaction {dropped_tx; reason})

  let lock_transactions () =
    bind_worker @@ fun w -> push_request w Lock_transactions

  let unlock_transactions () =
    bind_worker @@ fun w -> push_request w Unlock_transactions

  let is_locked () =
    let open Lwt_result_syntax in
    let*? worker = Lazy.force worker in
    Worker.Queue.push_request_and_wait worker Is_locked |> handle_request_error

  let pop_transactions ~maximum_cumulative_size:_ ~validate_tx
      ~initial_validation_state =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    Worker.Queue.push_request_and_wait
      w
      (Pop_transactions
         {validate_tx; validation_state = initial_validation_state})
    |> handle_request_error

  let start ~config ~keep_alive ~timeout () =
    let open Lwt_result_syntax in
    let timeout = min timeout 5. in
    let* worker =
      Worker.launch table () {config; keep_alive; timeout} (module Handlers)
    in
    Lwt.wakeup worker_waker worker ;
    let*! () = Tx_queue_events.is_ready () in
    return_unit
end

module Eth_tx_container =
  Tx_container
    (Tx_queue_types.Eth_transaction_object)
    (struct
      let chain_name = "etherlink"
    end)

module Tezlink_tx_container =
  Tx_container
    (Tx_queue_types.Tezlink_operation)
    (struct
      let chain_name = "tezlink"
    end)

let tx_container (type f) ~(chain_family : f L2_types.chain_family) :
    _ * f Services_backend_sig.tx_container =
  match chain_family with
  | EVM ->
      ( Eth_tx_container.start,
        Services_backend_sig.Evm_tx_container (module Eth_tx_container) )
  | Michelson ->
      ( Tezlink_tx_container.start,
        Services_backend_sig.Michelson_tx_container
          (module Tezlink_tx_container) )

module Internal_for_tests = struct
  module Nonce_bitset = Nonce_bitset
  module Address_nonce = Address_nonce
end
