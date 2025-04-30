(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_workers

type parameters = {config : Configuration.tx_queue; keep_alive : bool}

type queue_variant = [`Accepted | `Refused]

type pending_variant = [`Confirmed | `Dropped]

type all_variant = [queue_variant | pending_variant]

type 'a variant_callback = 'a -> unit Lwt.t

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

type callback = all_variant variant_callback

type request = {
  next_nonce : Ethereum_types.quantity;
  payload : Ethereum_types.hex;
  tx_object : Ethereum_types.legacy_transaction_object;
  callback : callback;
}

(** Inject transactions with either RPCs or on a websocket connection. *)
type endpoint = Services_backend_sig.endpoint =
  | Rpc of Uri.t
  | Websocket of Websocket_client.t

(** [Nonce_bitset] registers known nonces from transactions that went
    through the tx_queue from a specific sender address. With this
    structure it's easy to do bookkeeping of address' nonce without
    going through all the transactions of the queue.

    The invariants are that for any nonce_bitset [nb]:

    - When creating [nb], [nb.next_nonce] is the next valid nonce for
    the state.

    - When adding a nonce [n] to [nb], [n] must be superior or equal
    to [nb.next_nonce]. This is enforced by the validation in
    {!Validate.is_tx_valid}.

    - [nb.next_nonce] can only increase over time. This is enforced by
    [shift] and [offset].
 *)
module Nonce_bitset = struct
  module Bitset = Tezos_base.Bitset

  (** [t] allows to register for a given address all nonces that are
      currently used by transaction in the tx_queue. *)
  type t = {
    next_nonce : Z.t;
        (** [next_nonce] is the base value for any position found in
            {!field:bitset}. It’s set to be the next expected nonce
            for a given address, which is the nonce found in the
            backend. *)
    bitset : Bitset.t;
  }

  (** [create ~next_nonce] creates a {!t} struct with empty [bitset]. *)
  let create ~next_nonce = {next_nonce; bitset = Bitset.empty}

  (** [offset ~nonce1 ~nonce2] computes the difference between
      [nonce1] and [nonce2].

      Fails if [nonce2 > nonce1] or if the difference between the two is
      more than {!Int.max_int}. *)
  let offset ~nonce1 ~nonce2 =
    let open Result_syntax in
    if Z.gt nonce2 nonce1 then
      error_with
        "Internal: invalid nonce diff. nonce2 %a must be inferior or equal to \
         nonce1 %a."
        Z.pp_print
        nonce2
        Z.pp_print
        nonce1
    else
      let offset = Z.(nonce1 - nonce2) in
      if Z.fits_int offset then return (Z.to_int offset)
      else
        error_with
          "Internal: invalid nonce offset, it's too large to fit in an integer."

  (** [add bitset_nonce ~nonce] adds the nonce [nonce] to [bitset_nonce]. *)
  let add {next_nonce; bitset} ~nonce =
    let open Result_syntax in
    let* offset_position = offset ~nonce1:nonce ~nonce2:next_nonce in
    let* bitset = Bitset.add bitset offset_position in
    return {next_nonce; bitset}

  (** [remove bitset_nonce ~nonce] removes the nonce [nonce] from
      [bitset_nonce].

      If [nonce] is strictly inferior to [bitset_nonce.next_nonce] then
      it's a no-op because nonce can't exist in the bitset. *)
  let remove {next_nonce; bitset} ~nonce =
    let open Result_syntax in
    if Z.lt nonce next_nonce then
      (* no need to remove a nonce that can't exist in the bitset *)
      return {next_nonce; bitset}
    else
      let* offset_position = offset ~nonce1:nonce ~nonce2:next_nonce in
      let* bitset = Bitset.remove bitset offset_position in
      return {next_nonce; bitset}

  (** [shift bitset_nonce ~nonce] shifts the bitset of [bitset_nonce]
      so the next_nonce is now [nonce]. Shifting the bitset means
      that nonces that are inferior to [nonce] are dropped.

      Fails if [nonce] is strictly inferior to
      [bitset_nonce.next_nonce]. *)
  let shift {next_nonce; bitset} ~nonce =
    let open Result_syntax in
    let* offset = offset ~nonce1:nonce ~nonce2:next_nonce in
    let* bitset = Bitset.shift_right bitset ~offset in
    return {next_nonce = nonce; bitset}

  (** [is_empty bitset_nonce] checks if the bitset is empty, i.e. no
      position is at 1. *)
  let is_empty {bitset; _} = Bitset.is_empty bitset

  (** [next_gap bitset_nonce] returns the next available nonce. *)
  let next_gap {next_nonce; bitset} =
    let offset_position = Z.(trailing_zeros @@ lognot @@ Bitset.to_z bitset) in
    Z.(next_nonce + of_int offset_position)

  (** [shift_then_next_gap bitset_nonce ~shift_nonce] calls {!shift
      ~nonce:shift_nonce} then {!next_gap bitset_nonce}. *)
  let shift_then_next_gap bitset_nonce ~shift_nonce =
    let open Result_syntax in
    let* bitset_nonce = shift bitset_nonce ~nonce:shift_nonce in
    return @@ next_gap bitset_nonce
end

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

  let add nonces ~addr ~next_nonce ~nonce =
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
      (* Only adds [nonce] if [bitset_nonce.next_nonce] is inferior or
         equal. If [nonce_bitset.next_nonce > nonce] then there is no
         need to add because [nonce] is already in the past.

         This is follow-up to the previous comment where we are in a
         rare ce condition of the transaction is being validated while
         a transaction with a superior nonce is being confirmed. In
         such case we simply don't register the nonce, and the
         transaction will be dropped by the upstream node when
         receiving it. *)
      if Z.gt nonce_bitset.Nonce_bitset.next_nonce nonce then
        return nonce_bitset
      else Nonce_bitset.add nonce_bitset ~nonce
    in
    let () = S.replace nonces addr nonce_bitset in
    return_unit

  let confirm_nonce nonces ~addr ~nonce =
    let open Result_syntax in
    let nonce_bitset = S.find nonces addr in
    match nonce_bitset with
    | Some nonce_bitset ->
        let next_nonce = Z.succ nonce in
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

  let remove nonces ~addr ~nonce =
    let open Result_syntax in
    let nonce_bitset = S.find nonces addr in
    match nonce_bitset with
    | Some nonce_bitset ->
        let* nonce_bitset = Nonce_bitset.remove nonce_bitset ~nonce in
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

module Transaction_objects = struct
  open Ethereum_types
  module S = String.Hashtbl

  type t = Ethereum_types.legacy_transaction_object S.t

  let empty ~start_size = S.create start_size

  let add htbl
      (({hash = Hash (Hex hash); _} : Ethereum_types.legacy_transaction_object)
       as tx_object) =
    (* Here we uses `add` and not `replace`. If a transaction is
       submitted multiple times then we register it each time in the
       hashtable. Meaning that for `find` to returns None, we must
       call `remove` as many times as the transaction was added. *)
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
    S.replace
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

  let to_seq = S.to_seq_values

  let clear = S.clear
end

module Transactions_per_addr = struct
  module S = String.Hashtbl

  type t = int64 S.t

  let empty ~start_size = S.create start_size

  let remove s (Ethereum_types.Address (Hex h)) = S.remove s h

  let find s (Ethereum_types.Address (Hex h)) = S.find s h

  let add s (Ethereum_types.Address (Hex h)) i = S.replace s h i

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
end

type state = {
  mutable queue : queue_request Queue.t;
  pending : Pending_transactions.t;
  tx_object : Transaction_objects.t;
  tx_per_address : Transactions_per_addr.t;
  address_nonce : Address_nonce.t;
  config : Configuration.tx_queue;
  keep_alive : bool;
  mutable locked : bool;
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
    | Inject : request -> ((unit, string) result, tztrace) t
    | Find : {
        txn_hash : Ethereum_types.hash;
      }
        -> (Ethereum_types.legacy_transaction_object option, tztrace) t
    | Nonce : {
        next_nonce : Ethereum_types.quantity;
        address : Ethereum_types.address;
      }
        -> (Ethereum_types.quantity, tztrace) t
    | Tick : {evm_node_endpoint : endpoint} -> (unit, tztrace) t
    | Clear : (unit, tztrace) t
    | Lock_transactions : (unit, tztrace) t
    | Unlock_transactions : (unit, tztrace) t
    | Is_locked : (bool, tztrace) t
    | Content : (Ethereum_types.txpool, tztrace) t
    | Pop_transactions : {
        validation_state : 'a;
        validate_tx :
          'a ->
          string ->
          Ethereum_types.legacy_transaction_object ->
          [`Keep of 'a | `Drop | `Stop] tzresult Lwt.t;
      }
        -> ((string * Ethereum_types.legacy_transaction_object) list, tztrace) t
    | Confirm_transactions : {
        confirmed_txs : Ethereum_types.hash Seq.t;
        clear_pending_queue_after : bool;
      }
        -> (unit, tztrace) t

  type view = View : _ t -> view

  let view req = View req

  let endpoint_encoding =
    let open Data_encoding in
    conv
      (function Rpc uri -> Uri.to_string uri | Websocket _ -> "[websocket]")
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
          ~title:"Find"
          (obj2
             (req "request" (constant "find"))
             (req "transaction_hash" Ethereum_types.hash_encoding))
          (function View (Find {txn_hash}) -> Some ((), txn_hash) | _ -> None)
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
             (req "address" Ethereum_types.address_encoding))
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
          ~title:"Pop_transactions"
          (obj1 (req "request" (constant "pop_transactions")))
          (function
            | View (Pop_transactions {validation_state = _; validate_tx = _}) ->
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
      ]

  let pp fmt (View r) =
    let open Format in
    match r with
    | Inject {payload = Hex txn; _} -> fprintf fmt "Inject %s" txn
    | Find {txn_hash = Hash (Hex txn_hash)} -> fprintf fmt "Find %s" txn_hash
    | Tick _ -> fprintf fmt "Tick"
    | Clear -> fprintf fmt "Clear"
    | Nonce {next_nonce = _; address = Address (Hex address)} ->
        fprintf fmt "Nonce %s" address
    | Lock_transactions -> Format.fprintf fmt "Locking the transactions"
    | Unlock_transactions -> Format.fprintf fmt "Unlocking the transactions"
    | Is_locked -> Format.fprintf fmt "Checking if the tx queue is locked"
    | Content -> fprintf fmt "Content"
    | Pop_transactions {validation_state = _; validate_tx = _} ->
        fprintf fmt "Popping transactions with validation function"
    | Confirm_transactions _ -> fprintf fmt "Confirming transactions"
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  open Request

  type self = worker

  let uuid_seed = Random.get_state ()

  let send_transactions_batch ~evm_node_endpoint ~keep_alive transactions =
    let open Lwt_result_syntax in
    let module M = Map.Make (String) in
    let module Srt = Rpc_encodings.Send_raw_transaction in
    if Seq.is_empty transactions then return_unit
    else
      let rev_batch, callbacks =
        Seq.fold_left
          (fun (rev_batch, callbacks) {hash = _; payload; queue_callback} ->
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

            (txn :: rev_batch, M.add req_id queue_callback callbacks))
          ([], M.empty)
          transactions
      in
      let batch = List.rev rev_batch in

      let*! () = Tx_queue_events.injecting_transactions (List.length batch) in

      let* responses =
        match evm_node_endpoint with
        | Rpc base ->
            let* batch_response =
              Rollup_services.call_service
                ~keep_alive
                ~base
                (Batch.dispatch_batch_service ~path:Resto.Path.root)
                ()
                ()
                (Batch batch)
            in
            return
              (match batch_response with Singleton r -> [r] | Batch rs -> rs)
        | Websocket ws_client ->
            List.map_es
              (fun req ->
                let+ resp_json =
                  Websocket_client.send_jsonrpc_request ws_client req
                in
                Data_encoding.Json.destruct
                  Rpc_encodings.JSONRPC.response_encoding
                  resp_json)
              batch
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
       } :
        state) =
    (* full matching so when a new element is added to the state it's not
       forgotten to clear it. *)
    String.Hashtbl.clear pending ;
    String.Hashtbl.clear tx_object ;
    String.Hashtbl.clear tx_per_address ;
    String.Hashtbl.clear address_nonce ;
    Queue.clear queue ;
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
              | `Drop ->
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

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun self request ->
    let open Lwt_result_syntax in
    let state = Worker.state self in
    match request with
    | Inject {next_nonce; payload; tx_object; callback} ->
        protect @@ fun () ->
        Tx_watcher.notify tx_object.hash ;
        let (Address (Hex addr)) = tx_object.from in
        let (Qty tx_nonce) = tx_object.nonce in
        let pending_callback (reason : pending_variant) =
          let open Lwt_syntax in
          let* res =
            match reason with
            | `Dropped ->
                let* () = Tx_queue_events.transaction_dropped tx_object.hash in
                return
                @@ Address_nonce.remove
                     state.address_nonce
                     ~addr
                     ~nonce:tx_nonce
            | `Confirmed ->
                let* () =
                  Tx_queue_events.transaction_confirmed tx_object.hash
                in
                return
                @@ Address_nonce.confirm_nonce
                     state.address_nonce
                     ~addr
                     ~nonce:tx_nonce
          in
          let* () =
            match res with
            | Ok () -> return_unit
            | Error errs -> Tx_queue_events.callback_error errs
          in
          Transactions_per_addr.decrement state.tx_per_address tx_object.from ;
          Transaction_objects.remove state.tx_object tx_object.hash ;
          Lwt.dont_wait
            (fun () -> callback (reason :> all_variant))
            (fun exn ->
              Tx_queue_events.callback_error__dont_wait__use_with_care
                [Error_monad.error_of_exn exn]) ;
          Lwt.return_unit
        in
        let queue_callback reason =
          let open Lwt_syntax in
          let* res =
            match reason with
            | `Accepted ->
                Pending_transactions.add
                  state.pending
                  tx_object.hash
                  pending_callback ;
                return_ok_unit
            | `Refused ->
                Transactions_per_addr.decrement
                  state.tx_per_address
                  tx_object.from ;
                Transaction_objects.remove state.tx_object tx_object.hash ;
                return
                @@ Address_nonce.remove
                     state.address_nonce
                     ~addr
                     ~nonce:tx_nonce
          in
          let* () =
            match res with
            | Ok () -> return_unit
            | Error errs -> Tx_queue_events.callback_error errs
          in
          Lwt.dont_wait
            (fun () -> callback (reason :> all_variant))
            (fun exn ->
              Tx_queue_events.callback_error__dont_wait__use_with_care
                [Error_monad.error_of_exn exn]) ;
          Lwt.return_unit
        in
        if Compare.Int.(Queue.length state.queue < state.config.max_size) then (
          (* Check number of txs by user in tx_queue. *)
          let nb_txs_in_queue =
            Transactions_per_addr.find state.tx_per_address tx_object.from
          in
          match nb_txs_in_queue with
          | Some i when i >= state.config.tx_per_addr_limit ->
              let*! () =
                Tx_pool_events.txs_per_user_threshold_reached
                  ~address:(Ethereum_types.Address.to_string tx_object.from)
              in
              return
                (Error
                   "Limit of transaction for a user was reached. Transaction \
                    is rejected.")
          | Some _ | None ->
              let*! () = Tx_queue_events.add_transaction tx_object.hash in
              Transactions_per_addr.increment
                state.tx_per_address
                tx_object.from ;
              Transaction_objects.add state.tx_object tx_object ;
              let Ethereum_types.(Qty next_nonce) = next_nonce in
              let*? () =
                Address_nonce.add
                  state.address_nonce
                  ~addr
                  ~next_nonce
                  ~nonce:tx_nonce
              in
              Queue.add
                {hash = tx_object.hash; payload; queue_callback}
                state.queue ;
              return (Ok ()))
        else
          return
            (Error "Transaction limit was reached. Transaction is rejected.")
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
        state.queue <- Queue.of_seq remaining_transactions ;

        let* () =
          send_transactions_batch
            ~keep_alive:state.keep_alive
            ~evm_node_endpoint
            transactions_to_inject
        in

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
    | Clear ->
        protect @@ fun () ->
        clear state ;
        let*! () = Tx_queue_events.cleared () in
        return_unit
    | Nonce {next_nonce; address = Address (Hex addr)} ->
        protect @@ fun () ->
        let Ethereum_types.(Qty next_nonce) = next_nonce in
        let*? next_gap =
          Address_nonce.next_gap state.address_nonce ~addr ~next_nonce
        in
        return @@ Ethereum_types.Qty next_gap
    | Lock_transactions -> protect @@ fun () -> return (lock_transactions state)
    | Unlock_transactions ->
        protect @@ fun () -> return (unlock_transactions state)
    | Is_locked -> protect @@ fun () -> return (is_locked state)
    | Content ->
        protect @@ fun () ->
        let open Ethereum_types in
        let process_transactions tx_map lookup_fn acc =
          String.Hashtbl.fold
            (fun hash value acc ->
              match lookup_fn hash value with
              | Some (obj : Ethereum_types.legacy_transaction_object) ->
                  let existing_nonce_map =
                    AddressMap.find_opt obj.from acc
                    |> Option.value ~default:NonceMap.empty
                  in
                  let updated_nonce_map =
                    NonceMap.add (Qty.to_z obj.nonce) obj existing_nonce_map
                  in
                  AddressMap.add obj.from updated_nonce_map acc
              | None -> acc)
            tx_map
            acc
        in

        (* Process pending and collect found transactions *)
        let pending =
          process_transactions
            state.pending
            (fun hash _v -> String.Hashtbl.find_opt state.tx_object hash)
            AddressMap.empty
        in

        (* Process tx_object separately to collect the queued (unmatched) transactions *)
        let queued =
          process_transactions
            state.tx_object
            (fun hash obj ->
              if String.Hashtbl.mem state.pending hash then None else Some obj)
            AddressMap.empty
        in

        return {pending; queued}
    | Pop_transactions {validation_state; validate_tx} ->
        protect @@ fun () ->
        if is_locked state then return []
        else pop_queue_until state ~validate_tx ~validation_state
    | Confirm_transactions {confirmed_txs; clear_pending_queue_after} ->
        protect @@ fun () ->
        let*! () =
          Seq.S.iter
            (fun hash ->
              let callback = Pending_transactions.pop state.pending hash in
              match callback with
              | Some {pending_callback; _} -> pending_callback `Confirmed
              | None ->
                  (* delayed transactions hashes are part of confirmed
                     txs *)
                  Lwt.return_unit)
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

  type launch_error = tztrace

  let on_launch _self () ({config; keep_alive} : parameters) =
    let open Lwt_result_syntax in
    return
      {
        queue = Queue.create ();
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
      }

  let on_error (type a b) _self _status_request (_r : (a, b) Request.t)
      (_errs : b) : [`Continue | `Shutdown] tzresult Lwt.t =
    Lwt_result_syntax.return `Continue

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

module Internal_for_tests = struct
  module Nonce_bitset = Nonce_bitset
  module Address_nonce = Address_nonce
end

module Tx_container = struct
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

  let nonce ~next_nonce address =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    Worker.Queue.push_request_and_wait w (Nonce {next_nonce; address})
    |> handle_request_error

  let inject ?(callback = fun _ -> Lwt_syntax.return_unit) ~next_nonce
      (tx_object : Ethereum_types.legacy_transaction_object) txn =
    let open Lwt_syntax in
    let* worker = worker_promise in
    Worker.Queue.push_request_and_wait
      worker
      (Inject {next_nonce; payload = txn; tx_object; callback})
    |> handle_request_error

  let add ~next_nonce tx_object ~raw_tx =
    let open Lwt_result_syntax in
    let* res = inject ~next_nonce tx_object raw_tx in
    match res with
    | Ok () -> return (Ok tx_object.hash)
    | Error errs -> return (Error errs)

  let find txn_hash =
    let open Lwt_result_syntax in
    let* legacy_tx_object =
      let*? w = Lazy.force worker in
      Worker.Queue.push_request_and_wait w (Find {txn_hash})
      |> handle_request_error
    in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7747
       We should instrument the TX queue to return the real
       transaction objects. *)
    return
      (Option.map
         Transaction_object.from_store_transaction_object
         legacy_tx_object)

  let content () =
    let open Lwt_result_syntax in
    let*? w = Lazy.force worker in
    Worker.Queue.push_request_and_wait w Content |> handle_request_error

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

  let tx_queue_beacon ~evm_node_endpoint ~tick_interval =
    let open Lwt_result_syntax in
    let rec loop () =
      let* () = tx_queue_tick ~evm_node_endpoint in
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

  let start ~config ~keep_alive () =
    let open Lwt_result_syntax in
    let* worker =
      Worker.launch table () {config; keep_alive} (module Handlers)
    in
    Lwt.wakeup worker_waker worker ;
    let*! () = Tx_queue_events.is_ready () in
    return_unit
end

let start = Tx_container.start
