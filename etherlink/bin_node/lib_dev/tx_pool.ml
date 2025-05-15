(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Checks that [balance] is enough to pay up to the maximum [gas_limit]
    the sender defined parametrized by the [gas_price]. *)
let can_prepay ~balance ~gas_price ~gas_limit ~value:Ethereum_types.(Qty value)
    =
  balance >= Z.((gas_limit * gas_price) + value)

module Pool = struct
  module Pkey_map = Ethereum_types.AddressMap
  module Nonce_map = Tezos_base.Sized.MakeSizedMap (Ethereum_types.NonceMap)

  (** Transaction stored in the pool. *)
  type transaction = {
    index : int64; (* Global index of the transaction. *)
    raw_tx : string; (* Current transaction. *)
    inclusion_timestamp : Time.Protocol.t;
    (* Time of inclusion in the transaction pool. *)
    transaction_object : Ethereum_types.legacy_transaction_object;
  }

  type t = {
    transactions : transaction Nonce_map.t Pkey_map.t;
    global_index : int64; (* Index to order the transactions. *)
  }

  let empty : t = {transactions = Pkey_map.empty; global_index = Int64.zero}

  let get_pool {transactions; global_index = _} addr_balance_nonce_map =
    let open Result_syntax in
    let find_balance_and_nonce address =
      match Ethereum_types.AddressMap.find address addr_balance_nonce_map with
      | Some nonce -> nonce
      | None -> (Z.zero, Z.zero)
    in
    let is_transaction_pending
        (transaction_object : Ethereum_types.legacy_transaction_object)
        (address_balance : Z.t) (address_nonce : Z.t) : bool =
      let (Qty gas_limit) = transaction_object.gas in
      let (Qty gas_price) = transaction_object.gasPrice in
      transaction_object.nonce == Ethereum_types.quantity_of_z address_nonce
      && can_prepay
           ~balance:address_balance
           ~gas_price
           ~gas_limit
           ~value:transaction_object.value
    in
    let add_transaction_object_to_map nonce transaction_object pending_map
        queued_map address_balance address_nonce =
      if is_transaction_pending transaction_object address_balance address_nonce
      then
        return
          ( Ethereum_types.NonceMap.add nonce transaction_object pending_map,
            queued_map )
      else
        return
          ( pending_map,
            Ethereum_types.NonceMap.add nonce transaction_object queued_map )
    in
    let add_if_non_empty address nonce_map acc_address_map =
      if Ethereum_types.NonceMap.is_empty nonce_map then acc_address_map
      else Ethereum_types.AddressMap.add address nonce_map acc_address_map
    in
    Pkey_map.fold_e
      (fun address nonce_tx_map (acc_address_map_pending, acc_address_map_queued) ->
        let address_balance, address_nonce = find_balance_and_nonce address in
        let+ pending, queued =
          Nonce_map.fold_e
            (fun nonce transaction (pending_map, queued_map) ->
              add_transaction_object_to_map
                nonce
                transaction.transaction_object
                pending_map
                queued_map
                address_balance
                address_nonce)
            nonce_tx_map
            (Ethereum_types.NonceMap.empty, Ethereum_types.NonceMap.empty)
        in
        ( add_if_non_empty address pending acc_address_map_pending,
          add_if_non_empty address queued acc_address_map_queued ))
      transactions
      (Ethereum_types.AddressMap.empty, Ethereum_types.AddressMap.empty)

  (** Add a transaction to the pool. *)
  let add ~must_replace {transactions; global_index} raw_tx
      (transaction_object : Ethereum_types.legacy_transaction_object) =
    let (Qty nonce) = transaction_object.nonce in
    let (Qty gas_price) = transaction_object.gasPrice in
    let inclusion_timestamp = Misc.now () in

    (* Add the transaction to the user's transaction map *)
    let transaction =
      {index = global_index; raw_tx; inclusion_timestamp; transaction_object}
    in
    let add_transaction transactions =
      let replaced = ref false in
      let transactions =
        Pkey_map.update
          transaction_object.from
          (function
            | None ->
                (* User has no transactions in the pool *)
                Some (Nonce_map.singleton nonce transaction)
            | Some user_transactions ->
                Some
                  (Nonce_map.update
                     nonce
                     (function
                       | None -> Some transaction
                       | Some user_transaction ->
                           if
                             gas_price
                             > Ethereum_types.Qty.to_z
                                 user_transaction.transaction_object.gasPrice
                           then (
                             replaced := true ;
                             Some transaction)
                           else Some user_transaction)
                     user_transactions))
          transactions
      in
      (transactions, !replaced)
    in
    match must_replace with
    | `Replace_existing ->
        let transactions, replaced = add_transaction transactions in
        if replaced then
          Ok {transactions; global_index = Int64.(add global_index one)}
        else
          Error
            "Limit of transactions for a user was reached. Transaction is \
             rejected as it did not replace an existing one."
    | `No ->
        let transactions, _replaced = add_transaction transactions in
        Ok {transactions; global_index = Int64.(add global_index one)}
    | `Replace_shift max_nonce ->
        let transactions =
          Pkey_map.update
            transaction_object.from
            (function
              | None ->
                  (* The list of user transactions cannot be empty,
                     otherwise we wouldn't need to replace anything. *)
                  assert false
              | Some user_transactions ->
                  (* Shift by removing the maximum nonce. *)
                  let user_transactions =
                    Nonce_map.remove max_nonce user_transactions
                  in
                  (* Add the new nonce. *)
                  Some (Nonce_map.add nonce transaction user_transactions))
            transactions
        in
        Ok {transactions; global_index = Int64.(add global_index one)}

  (** Returns all the addresses of the pool *)
  let addresses {transactions; _} =
    Pkey_map.bindings transactions |> List.map fst

  (** Returns the transaction matching the predicate.
      And remove them from the pool. *)
  let partition pkey predicate {transactions; global_index} =
    (* Get the sequence of transaction *)
    let selected, remaining =
      transactions |> Pkey_map.find pkey
      |> Option.value ~default:Nonce_map.empty
      |> Nonce_map.partition predicate
    in
    (* Remove transactions from the public key map if empty *)
    let transactions =
      if Nonce_map.is_empty remaining then Pkey_map.remove pkey transactions
      else Pkey_map.add pkey remaining transactions
    in
    (* Convert the sequence to a list *)
    let selected = selected |> Nonce_map.bindings |> List.map snd in
    (selected, {transactions; global_index})

  (** Removes from the pool the transactions matching the predicate
      for the given pkey. *)
  let remove pkey predicate t =
    let _txs, t = partition pkey predicate t in
    t

  (** Returns the next nonce for a given user.
      Returns the given nonce if the user does not have any transactions in the pool. *)
  let next_nonce pkey current_nonce (t : t) =
    let open Ethereum_types in
    let ({transactions; _} : t) = t in
    (* Retrieves the list of transactions for a given user. *)
    let user_transactions =
      Pkey_map.find pkey transactions
      |> Option.value ~default:Nonce_map.empty
      |> Nonce_map.bindings |> List.map fst
    in
    let rec aux current_nonce = function
      | [] -> current_nonce
      | nonce :: txs ->
          if current_nonce > nonce then aux current_nonce txs
          else if current_nonce = nonce then
            (aux [@tailcall]) Z.(add current_nonce one) txs
          else current_nonce
    in
    let (Qty current_nonce) = current_nonce in
    aux current_nonce user_transactions |> Ethereum_types.quantity_of_z

  let find (t : t) tx_hash =
    let transactions =
      Pkey_map.to_seq t.transactions
      |> Seq.concat_map (fun (_, t) -> Nonce_map.to_seq t |> Seq.map snd)
    in
    Seq.find_map
      (fun t ->
        if t.transaction_object.hash = tx_hash then Some t.transaction_object
        else None)
      transactions
end

type mode = Proxy | Sequencer | Relay

type parameters = {
  backend : (module Services_backend_sig.S);
  smart_rollup_address : string;
  mode : mode;
  tx_timeout_limit : int64;
  tx_pool_addr_limit : int;
  tx_pool_tx_per_addr_limit : int;
  chain_family : L2_types.chain_family;
}

module Types = struct
  type state = {
    backend : (module Services_backend_sig.S);
    smart_rollup_address : string;
    mutable pool : Pool.t;
    mutable popped_txs : Pool.transaction list;
    mode : mode;
    tx_timeout_limit : int64;
    tx_pool_addr_limit : int;
    tx_pool_tx_per_addr_limit : int;
    mutable locked : bool;
    chain_family : L2_types.chain_family;
  }

  type nonrec parameters = parameters
end

module Name = struct
  (* We only have a single tx-pool in the evm node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node_worker"; "tx-pool"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Add_transaction :
        Ethereum_types.legacy_transaction_object * string
        -> ((Ethereum_types.hash, string) result, tztrace) t
    | Pop_transactions :
        int
        -> ((string * Ethereum_types.legacy_transaction_object) list, tztrace) t
    | Pop_and_inject_transactions : (unit, tztrace) t
    | Lock_transactions : (unit, tztrace) t
    | Unlock_transactions : (unit, tztrace) t
    | Is_locked : (bool, tztrace) t
    | Size_info : (Metrics.Tx_pool.size_info, tztrace) t
    | Find :
        Ethereum_types.hash
        -> (Ethereum_types.legacy_transaction_object option, tztrace) t
    | Clear_popped_transactions : (unit, unit) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Add_transaction"
          (obj3
             (req "request" (constant "add_transaction"))
             (req
                "transaction_object"
                Ethereum_types.legacy_transaction_object_encoding)
             (req "transaction" string))
          (function
            | View (Add_transaction (transaction, txn)) ->
                Some ((), transaction, txn)
            | _ -> None)
          (fun ((), transaction, txn) ->
            View (Add_transaction (transaction, txn)));
        case
          (Tag 1)
          ~title:"Pop_transactions"
          (obj2
             (req "request" (constant "pop_transactions"))
             (req "maximum_cumulatize_size" int31))
          (function
            | View (Pop_transactions maximum_cumulative_size) ->
                Some ((), maximum_cumulative_size)
            | _ -> None)
          (fun ((), maximum_cumulative_size) ->
            View (Pop_transactions maximum_cumulative_size));
        case
          (Tag 2)
          ~title:"Pop_and_inject_transactions"
          (obj1 (req "request" (constant "pop_and_inject_transactions")))
          (function View Pop_and_inject_transactions -> Some () | _ -> None)
          (fun () -> View Pop_and_inject_transactions);
        case
          (Tag 3)
          ~title:"Lock_transactions"
          (obj1 (req "request" (constant "lock_transactions")))
          (function View Lock_transactions -> Some () | _ -> None)
          (fun () -> View Lock_transactions);
        case
          (Tag 4)
          ~title:"Unlock_transactions"
          (obj1 (req "request" (constant "unlock_transactions")))
          (function View Unlock_transactions -> Some () | _ -> None)
          (fun () -> View Unlock_transactions);
        case
          (Tag 5)
          ~title:"Is_locked"
          (obj1 (req "request" (constant "is_locked")))
          (function View Is_locked -> Some () | _ -> None)
          (fun () -> View Is_locked);
        case
          (Tag 6)
          ~title:"Size_info"
          (obj1 (req "request" (constant "size_info")))
          (function View Size_info -> Some () | _ -> None)
          (fun () -> View Size_info);
        case
          (Tag 7)
          ~title:"Find"
          (obj2
             (req "request" (constant "find"))
             (req "tx_hash" Ethereum_types.hash_encoding))
          (function View (Find tx_hash) -> Some ((), tx_hash) | _ -> None)
          (fun ((), tx_hash) -> View (Find tx_hash));
        case
          (Tag 8)
          ~title:"Clear_popped_transactions"
          (obj1 (req "request" (constant "clear_popped_transactions")))
          (function View Clear_popped_transactions -> Some () | _ -> None)
          (fun () -> View Clear_popped_transactions);
      ]

  let pp ppf (View r) =
    match r with
    | Add_transaction (_, tx_raw) ->
        Format.fprintf
          ppf
          "Add tx [%s] to tx-pool"
          (Hex.of_string tx_raw |> Hex.show)
    | Pop_transactions maximum_cumulative_size ->
        Format.fprintf
          ppf
          "Popping transactions of maximum cumulative size %d bytes"
          maximum_cumulative_size
    | Pop_and_inject_transactions ->
        Format.fprintf ppf "Popping and injecting transactions"
    | Lock_transactions -> Format.fprintf ppf "Locking the transactions"
    | Unlock_transactions -> Format.fprintf ppf "Unlocking the transactions"
    | Is_locked -> Format.fprintf ppf "Checking if the tx pool is locked"
    | Size_info ->
        Format.fprintf ppf "Requesting size information about the tx pool"
    | Find tx_hash ->
        Format.fprintf
          ppf
          "Looking for tx %a in tx pool"
          Ethereum_types.pp_hash
          tx_hash
    | Clear_popped_transactions ->
        Format.fprintf ppf "Clearing popped transactions"
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let check_address_boundaries ~pool ~address ~tx_pool_addr_limit
    ~tx_pool_tx_per_addr_limit =
  let open Lwt_result_syntax in
  match Pool.Pkey_map.find address pool.Pool.transactions with
  | None ->
      if Pool.Pkey_map.cardinal pool.Pool.transactions < tx_pool_addr_limit then
        return_unit
      else
        let*! () = Tx_pool_events.users_threshold_reached () in
        fail `MaxUsers
  | Some txs ->
      if Pool.Nonce_map.cardinal txs < tx_pool_tx_per_addr_limit then
        return_unit
      else
        let*! () =
          Tx_pool_events.txs_per_user_threshold_reached
            ~address:(Ethereum_types.Address.to_string address)
        in
        fail (`MaxPerUser txs)

let insert_valid_transaction state tx_raw
    (transaction_object : Ethereum_types.legacy_transaction_object) =
  let open Lwt_result_syntax in
  let open Types in
  let {
    backend = (module Backend);
    pool;
    tx_pool_addr_limit;
    tx_pool_tx_per_addr_limit;
    _;
  } =
    state
  in
  let add_transaction ~must_replace =
    (* Add the transaction to the pool *)
    match Pool.add ~must_replace pool tx_raw transaction_object with
    | Ok pool ->
        let*! () =
          Tx_pool_events.add_transaction
            ~transaction:(Ethereum_types.hash_to_string transaction_object.hash)
        in
        state.pool <- pool ;
        return (Ok transaction_object.hash)
    | Error msg -> return (Error msg)
  in
  let*! res_boundaries =
    check_address_boundaries
      ~pool
      ~address:transaction_object.from
      ~tx_pool_addr_limit
      ~tx_pool_tx_per_addr_limit
  in
  match res_boundaries with
  | Error `MaxUsers ->
      return
        (Error
           "The transaction pool has reached its maximum threshold for user \
            transactions. Transaction is rejected.")
  | Error (`MaxPerUser transactions) ->
      let (Qty nonce) = transaction_object.nonce in
      let max_nonce, nonce_exists =
        Pool.Nonce_map.fold
          (fun nonce' _tx (max_nonce, nonce_exists) ->
            (Z.max max_nonce nonce', nonce_exists || nonce = nonce'))
          transactions
          (Z.zero, false)
      in
      if nonce_exists then
        (* It must replace an existing one, otherwise it'll be above
           the limit. *)
        add_transaction ~must_replace:`Replace_existing
      else if nonce < max_nonce then
        (* If the nonce is smaller than the max nonce, we must shift the
           list and drop one transaction. *)
        add_transaction ~must_replace:(`Replace_shift max_nonce)
      else
        (* Otherwise we have just reached the limit of transactions. *)
        return
          (Error
             "Limit of transaction for a user was reached. Transaction is \
              rejected.")
  | Ok () -> add_transaction ~must_replace:`No

(** Checks that the transaction can be paid given the [gas_price] that was set
    and the current [base_fee_per_gas]. *)
let can_pay_with_current_base_fee ~gas_price ~base_fee_per_gas =
  gas_price >= base_fee_per_gas

(** Check if a transaction timed out since the moment it was included in the
    transaction pool. *)
let transaction_timed_out ~tx_timeout_limit ~current_timestamp
    ~inclusion_timestamp =
  Time.Protocol.diff current_timestamp inclusion_timestamp >= tx_timeout_limit

let pop_transactions state ~maximum_cumulative_size =
  let open Lwt_result_syntax in
  let Types.
        {
          backend = (module Backend : Services_backend_sig.S);
          pool;
          locked;
          tx_timeout_limit;
          _;
        } =
    state
  in
  if locked || state.chain_family = L2_types.Michelson then return []
  else
    (* Get all the addresses in the tx-pool. *)
    let addresses = Pool.addresses pool in
    (* Get the nonce related to each address. *)
    let*! addr_with_nonces =
      Lwt_list.map_p
        (fun address ->
          let* nonce =
            Backend.Etherlink.nonce
              address
              Ethereum_types.Block_parameter.(Block_parameter Latest)
          in
          let (Qty nonce) = Option.value ~default:(Qty Z.zero) nonce in
          let* (Qty balance) =
            Backend.Etherlink.balance
              address
              Ethereum_types.Block_parameter.(Block_parameter Latest)
          in
          Lwt.return_ok (address, balance, nonce))
        addresses
    in
    let addr_with_nonces = List.filter_ok addr_with_nonces in
    (* Remove transactions with too low nonce, timed-out and the ones that
       can not be prepayed anymore. *)
    let* (Qty base_fee_per_gas) = Backend.Etherlink.base_fee_per_gas () in
    let current_timestamp = Misc.now () in
    let pool =
      List.fold_left
        (fun pool (pkey, balance, current_nonce) ->
          Pool.remove
            pkey
            (fun nonce
                 {
                   inclusion_timestamp;
                   transaction_object =
                     {gasPrice = Qty gas_price; gas = Qty gas_limit; value; _};
                   _;
                 } ->
              nonce < current_nonce
              || (not (can_prepay ~value ~balance ~gas_price ~gas_limit))
              || transaction_timed_out
                   ~current_timestamp
                   ~inclusion_timestamp
                   ~tx_timeout_limit)
            pool)
        pool
        addr_with_nonces
    in
    (* Select transaction with nonce equal to user's nonce, that can be prepaid
       and selects a sum of transactions that wouldn't go above the size limit
       of the blueprint.
       Also removes the transactions from the pool. *)
    let txs, pool, _ =
      List.fold_left
        (fun (txs, pool, cumulative_size) (pkey, _, current_nonce) ->
          (* This mutable counter is purely local and used only for the
             partition. *)
          let accumulated_size = ref cumulative_size in
          let selected, pool =
            Pool.partition
              pkey
              (fun nonce
                   {
                     transaction_object = {gasPrice = Qty gas_price; _};
                     raw_tx;
                     _;
                   } ->
                let check_nonce = nonce = current_nonce in
                let can_fit =
                  !accumulated_size + String.length raw_tx
                  <= maximum_cumulative_size
                in
                let can_pay =
                  can_pay_with_current_base_fee ~gas_price ~base_fee_per_gas
                in
                let selected = check_nonce && can_pay && can_fit in
                (* If the transaction is selected, this means it will fit *)
                if selected then
                  accumulated_size := !accumulated_size + String.length raw_tx ;
                selected)
              pool
          in
          let txs = List.append txs selected in
          (txs, pool, !accumulated_size))
        ([], pool, 0)
        addr_with_nonces
    in
    (* Store popped tx. *)
    let*? () =
      if List.is_empty state.popped_txs then (
        state.popped_txs <- txs ;
        Ok ())
      else
        Error_monad.error_with
          "The current popped transactions have not been fully processed and \
           cleared yet."
    in
    (* Sorting transactions by index.
       First tx in the pool is the first tx to be sent to the batcher. *)
    let txs =
      txs
      |> List.sort (fun Pool.{index = index_a; _} {index = index_b; _} ->
             Int64.compare index_a index_b)
      |> List.map (fun Pool.{raw_tx; transaction_object; _} ->
             (raw_tx, transaction_object))
    in
    (* update the pool *)
    state.pool <- pool ;
    return txs

let clear_popped_transactions state = state.Types.popped_txs <- []

(** [pop_and_inject_transactions state] pop transactions from the pool
    and forward them to the next node. In proxy mode the transaction
    are forwarded to a rollup node, in observer mode to the next evm
    node. The sequencer is not supposed to use this function, using it
    would make transaction disappear from the tx pool. *)
let pop_and_inject_transactions state =
  let open Lwt_result_syntax in
  let open Types in
  (* We over approximate the number of transactions to pop in proxy and
     observer mode to the maximum size an L1 block can hold. If the proxy
     sends more, they won't be applied at the next level. For the observer,
     it prevents spamming the sequencer. *)
  let maximum_cumulative_size =
    Sequencer_blueprint.maximum_usable_space_in_blueprint
      Sequencer_blueprint.maximum_chunks_per_l1_level
  in
  let* txs = pop_transactions state ~maximum_cumulative_size in
  if not (List.is_empty txs) then
    let (module Backend : Services_backend_sig.S) = state.backend in
    let*! hashes =
      Backend.Etherlink.inject_transactions
      (* The timestamp is ignored in observer and proxy mode, it's just for
         compatibility with sequencer mode. *)
        ~timestamp:(Misc.now ())
        ~smart_rollup_address:state.smart_rollup_address
        ~transactions:txs
    in
    let () = clear_popped_transactions state in
    match hashes with
    | Error trace ->
        let*! () = Tx_pool_events.transaction_injection_failed trace in
        return_unit
    | Ok hashes ->
        let*! () =
          List.iter_s
            (fun hash -> Tx_pool_events.transaction_injected ~hash)
            hashes
        in
        return_unit
  else return_unit

let lock_transactions state = state.Types.locked <- true

let unlock_transactions state = state.Types.locked <- false

let is_locked state = state.Types.locked

let size_info (state : Types.state) =
  let pool = state.pool in
  let number_of_addresses, number_of_transactions =
    Pool.Pkey_map.fold
      (fun _addr transactions (number_of_addresses, number_of_transactions) ->
        ( number_of_addresses + 1,
          number_of_transactions + Pool.Nonce_map.cardinal transactions ))
      pool.transactions
      (0, 0)
  in
  Metrics.Tx_pool.{number_of_addresses; number_of_transactions}

let find state tx_hash =
  let res =
    List.find_map
      (fun Pool.{transaction_object; _} ->
        if transaction_object.hash = tx_hash then Some transaction_object
        else None)
      state.Types.popped_txs
  in
  Option.either_f res (fun () -> Pool.find state.Types.pool tx_hash)

module Handlers = struct
  type self = worker

  let relay_self_inject_request w =
    let open Lwt_result_syntax in
    let state = Worker.state w in
    match state.mode with
    | Relay ->
        let*! _ =
          Worker.Queue.push_request w Request.Pop_and_inject_transactions
        in
        return_unit
    | Sequencer | Proxy -> return_unit

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let open Lwt_result_syntax in
    let state = Worker.state w in
    match request with
    | Request.Add_transaction (transaction_object, txn) ->
        protect @@ fun () ->
        Tx_watcher.notify transaction_object.hash ;
        let* res = insert_valid_transaction state txn transaction_object in
        let* () = relay_self_inject_request w in
        return res
    | Request.Pop_transactions maximum_cumulative_size ->
        protect @@ fun () -> pop_transactions state ~maximum_cumulative_size
    | Request.Pop_and_inject_transactions ->
        protect @@ fun () -> pop_and_inject_transactions state
    | Request.Lock_transactions ->
        protect @@ fun () -> return (lock_transactions state)
    | Request.Unlock_transactions -> return (unlock_transactions state)
    | Request.Is_locked -> protect @@ fun () -> return (is_locked state)
    | Request.Size_info -> protect @@ fun () -> return (size_info state)
    | Request.Find tx_hash -> protect @@ fun () -> return (find state tx_hash)
    | Request.Clear_popped_transactions ->
        let () = clear_popped_transactions state in
        return_unit

  type launch_error = error trace

  let on_launch _w ()
      ({
         backend;
         smart_rollup_address;
         mode;
         tx_timeout_limit;
         tx_pool_addr_limit;
         tx_pool_tx_per_addr_limit;
         chain_family;
       } :
        Types.parameters) =
    let state =
      Types.
        {
          backend;
          smart_rollup_address;
          pool = Pool.empty;
          popped_txs = [];
          mode;
          tx_timeout_limit;
          tx_pool_addr_limit;
          tx_pool_tx_per_addr_limit;
          locked = false;
          chain_family;
        }
    in
    Lwt_result_syntax.return state

  let on_error (type a b) _w _st (_r : (a, b) Request.t) (_errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    Lwt_result_syntax.return `Continue

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_worker

type error += Tx_pool_terminated

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail e -> Result_syntax.tzfail (error_of_exn e)
    | Lwt.Sleep -> Result_syntax.tzfail No_worker)

let bind_worker f =
  let open Lwt_result_syntax in
  let res = Lazy.force worker in
  match res with
  | Error [No_worker] ->
      (* There is no worker, nothing to do *)
      return_unit
  | Error errs -> fail errs
  | Ok w -> f w

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Tx_pool_terminated]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let start parameters =
  let open Lwt_result_syntax in
  let+ worker = Worker.launch table () parameters (module Handlers) in
  Lwt.wakeup worker_waker worker

let add transaction_object raw_tx =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    w
    (Request.Add_transaction (transaction_object, raw_tx))
  |> handle_request_error

let nonce pkey =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  let Types.{backend = (module Backend); pool; _} = Worker.state w in
  let+ current_nonce =
    Backend.Etherlink.nonce
      pkey
      Ethereum_types.Block_parameter.(Block_parameter Latest)
  in
  let current_nonce =
    Option.value ~default:Ethereum_types.Qty.zero current_nonce
  in
  Pool.next_nonce pkey current_nonce pool

let pop_transactions ~maximum_cumulative_size =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Pop_transactions maximum_cumulative_size)
  |> handle_request_error

let pop_and_inject_transactions () =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  let state = Worker.state worker in
  match state.mode with
  | Sequencer ->
      (* the sequencer injects blueprint in a rollup node, not
         transaction. *)
      return_unit
  | Proxy | Relay ->
      Worker.Queue.push_request_and_wait
        worker
        Request.Pop_and_inject_transactions
      |> handle_request_error

let pop_and_inject_transactions_lazy () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let state = Worker.state w in
  match state.mode with
  | Sequencer ->
      (* the sequencer injects blueprint in a rollup node, not
         transaction. *)
      return_unit
  | Proxy | Relay ->
      let*! (_pushed : bool) =
        Worker.Queue.push_request w Request.Pop_and_inject_transactions
      in
      return_unit

let size_info () =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait worker Request.Size_info
  |> handle_request_error

let get_tx_pool_content () =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  let Types.{backend = (module Backend); pool; _} = Worker.state w in
  let addresses = Pool.addresses pool in
  let*! addr_with_balance_nonces =
    Lwt_list.map_p
      (fun address ->
        let* nonce =
          Backend.Etherlink.nonce
            address
            Ethereum_types.Block_parameter.(Block_parameter Latest)
        in
        let (Qty nonce) = Option.value ~default:(Qty Z.zero) nonce in
        let* (Qty balance) =
          Backend.Etherlink.balance
            address
            Ethereum_types.Block_parameter.(Block_parameter Latest)
        in
        Lwt.return_ok (address, balance, nonce))
      addresses
  in
  let addr_balance_nonce_map =
    List.fold_left
      (fun acc (address, balance, nonce) ->
        Ethereum_types.AddressMap.add address (balance, nonce) acc)
      Ethereum_types.AddressMap.empty
      (List.filter_ok addr_with_balance_nonces)
  in
  let*? pending, queued = Pool.get_pool pool addr_balance_nonce_map in
  return Ethereum_types.{pending; queued}

let find tx_hash =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait worker (Request.Find tx_hash)
  |> handle_request_error

let clear_popped_transactions () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! (_pushed : bool) =
    Worker.Queue.push_request w Request.Clear_popped_transactions
  in
  return_unit

let mode () =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  let state = Worker.state worker in
  return state.mode

module Tx_container = struct
  let nonce ~next_nonce:_ address = nonce address

  let add ~next_nonce:_ tx_object ~raw_tx =
    let raw_tx_str = Ethereum_types.hex_to_bytes raw_tx in
    add tx_object raw_tx_str

  let find hash =
    let open Lwt_result_syntax in
    let* legacy_tx_object = find hash in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7747
       We should instrument the TX pool to return the real
       transaction objects. *)
    return
      (Option.map
         Transaction_object.from_store_transaction_object
         legacy_tx_object)

  let content = get_tx_pool_content

  let shutdown () =
    let open Lwt_result_syntax in
    bind_worker @@ fun w ->
    let*! () = Tx_pool_events.shutdown () in
    let*! () = Worker.shutdown w in
    return_unit

  let clear () = Lwt_result_syntax.return_unit

  let tx_queue_tick ~evm_node_endpoint:_ = Lwt_result_syntax.return_unit

  let tx_queue_beacon ~evm_node_endpoint:_ ~tick_interval:_ =
    Lwt_result_syntax.return_unit

  let lock_transactions () =
    let open Lwt_result_syntax in
    let*? worker = Lazy.force worker in
    Worker.Queue.push_request_and_wait worker Request.Lock_transactions
    |> handle_request_error

  let unlock_transactions () =
    let open Lwt_result_syntax in
    let*? worker = Lazy.force worker in
    Worker.Queue.push_request_and_wait worker Request.Unlock_transactions
    |> handle_request_error

  let is_locked () =
    let open Lwt_result_syntax in
    let*? worker = Lazy.force worker in
    Worker.Queue.push_request_and_wait worker Request.Is_locked
    |> handle_request_error

  let confirm_transactions ~clear_pending_queue_after:_ ~confirmed_txs:_ =
    clear_popped_transactions ()

  let pop_transactions ~maximum_cumulative_size ~validate_tx:_
      ~initial_validation_state:_ =
    pop_transactions ~maximum_cumulative_size
end
