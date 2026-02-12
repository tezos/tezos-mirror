(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  signer : Signer.map;
  maximum_number_of_chunks : int;
  tx_container : Services_backend_sig.ex_tx_container;
  sequencer_sunset_sec : int64;
  preconfirmation_stream_enabled : bool;
}

(* The size of a delayed transaction is overapproximated to the maximum size
   of an inbox message, as chunks are not supported in the delayed bridge. *)
let maximum_delayed_transaction_size = 4096

(*
   The legacy transactions are as follows:
  -----------------------------
  | Nonce    | Up to 32 bytes |
  -----------------------------
  | GasPrice | Up to 32 bytes |
  -----------------------------
  | GasLimit | Up to 32 bytes |
  -----------------------------
  | To       | 20 bytes addr  |
  -----------------------------
  | Value    | Up to 32 bytes |
  -----------------------------
  | Data     | 0 - unlimited  |
  -----------------------------
  | V        | 1 (usually)    |
  -----------------------------
  | R        | 32 bytes       |
  -----------------------------
  | S        | 32 bytes       |
  -----------------------------

   where `up to` start at 0, and encoded as the empty byte for the 0 value
   according to RLP specification.
*)
let minimum_ethereum_transaction_size =
  Rlp.(
    List
      [
        Value Bytes.empty;
        Value Bytes.empty;
        Value Bytes.empty;
        Value (Bytes.make 20 '\000');
        Value Bytes.empty;
        Value Bytes.empty;
        Value Bytes.empty;
        Value (Bytes.make 32 '\000');
        Value (Bytes.make 32 '\000');
      ]
    |> encode |> Bytes.length)

module Types = struct
  type nonrec parameters = parameters

  type preconfirmation_state =
    | Potential_next_block_timestamp of Time.Protocol.t
    | Selecting_delayed_txs of {
        timestamp : Time.Protocol.t;
        rev_delayed_txs : Ethereum_types.hash list;
        current_size : int;
      }
    | Validating_txs of {
        timestamp : Time.Protocol.t;
        rev_delayed_txs : Ethereum_types.hash list;
        rev_validated_txs : (string * Ethereum_types.hash) list;
        validation_state : Validation_types.validation_state;
      }

  type preconfirmation =
    | Disabled
    | Awaiting_first_timestamp
    | Enabled of preconfirmation_state

  type state = {
    signer : Signer.map;
    maximum_number_of_chunks : int;
    tx_container : Services_backend_sig.ex_tx_container;
    sequencer_sunset_sec : int64;
    mutable sunset : bool;
    mutable locked : bool;
    mutable preconfirmation_state : preconfirmation;
  }
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node_worker"; "block"; "producer"]

  let pp _ _ = ()

  let equal () () = true
end

type force = True | False | With_timestamp of Time.Protocol.t

type preconfirmed_transactions_result = {
  accepted : Ethereum_types.hash list;
  refused : Ethereum_types.hash list;
  dropped : Ethereum_types.hash list;
}

module Request = struct
  type ('a, 'b) t =
    | Produce_genesis :
        (Time.Protocol.t * Ethereum_types.block_hash)
        -> (unit, tztrace) t
    | Produce_block : {
        with_delayed_transactions : bool;
        force : force;
      }
        -> ([`Block_produced of int | `No_block], tztrace) t
    | Propose_next_block_timestamp : Time.Protocol.t -> (unit, tztrace) t
    | Preconfirm_transactions :
        (string * Tx_queue_types.transaction_object_t) list
        -> (preconfirmed_transactions_result, tztrace) t
    | Lock_block_production : (unit, tztrace) t
    | Unlock_block_production : (unit, tztrace) t

  let name : type a b. (a, b) t -> string = function
    | Produce_genesis _ -> "Produce_genesis"
    | Produce_block _ -> "Produce_block"
    | Propose_next_block_timestamp _ -> "Propose_next_block_timestamp"
    | Preconfirm_transactions _ -> "Preconfirm_transactions"
    | Lock_block_production -> "Lock_block_production"
    | Unlock_block_production -> "Unlock_block_production"

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let transaction_object_t_encoding =
    let open Data_encoding in
    let open Tx_queue_types in
    union
      [
        case
          Json_only
          ~title:"EVM"
          Transaction_object.encoding
          (function Evm txn_object -> Some txn_object | _ -> None)
          (fun txn_object -> Evm txn_object);
        case
          Json_only
          ~title:"Michelson"
          Tezos_types.Operation.encoding
          (function Michelson op -> Some op | _ -> None)
          (fun op -> Michelson op);
      ]

  let encoding =
    let open Data_encoding in
    union
      [
        case
          Json_only
          ~title:"Produce_genesis"
          (obj3
             (req "request" (constant "produce_genesis"))
             (req "timestamp" Time.Protocol.encoding)
             (req "hash" Ethereum_types.block_hash_encoding))
          (function
            | View (Produce_genesis (timestamp, hash)) ->
                Some ((), timestamp, hash)
            | _ -> None)
          (fun ((), timestamp, hash) ->
            View (Produce_genesis (timestamp, hash)));
        case
          Json_only
          ~title:"Produce_block"
          (obj4
             (req "request" (constant "produce_block"))
             (req "with_delayed_transactions" bool)
             (opt "timestamp" Time.Protocol.encoding)
             (req "force" bool))
          (function
            | View (Produce_block {with_delayed_transactions; force}) ->
                let timestamp, force =
                  match force with
                  | True -> (None, true)
                  | False -> (None, false)
                  | With_timestamp timestamp -> (Some timestamp, true)
                in
                Some ((), with_delayed_transactions, timestamp, force)
            | _ -> None)
          (fun ((), with_delayed_transactions, timestamp, force) ->
            let force =
              match (timestamp, force) with
              | Some t, true -> With_timestamp t
              | None, true -> True
              | _ -> False
            in
            View (Produce_block {with_delayed_transactions; force}));
        case
          Json_only
          ~title:"Propose_next_block_timestamp"
          (obj2
             (req "request" (constant "propose_next_block_timestamp"))
             (req "timestamp" Time.Protocol.encoding))
          (function
            | View (Propose_next_block_timestamp timestamp) ->
                Some ((), timestamp)
            | _ -> None)
          (fun ((), timestamp) -> View (Propose_next_block_timestamp timestamp));
        case
          Json_only
          ~title:"Preconfirm_transactions"
          (obj2
             (req "request" (constant "preconfirm_transactions"))
             (req
                "transactions"
                (list (tup2 string transaction_object_t_encoding))))
          (function
            | View (Preconfirm_transactions transactions) ->
                Some ((), transactions)
            | _ -> None)
          (fun ((), transactions) ->
            View (Preconfirm_transactions transactions));
        case
          Json_only
          ~title:"Lock_block_production"
          (obj1 (req "request" (constant "lock_block_production")))
          (function View Lock_block_production -> Some () | _ -> None)
          (fun () -> View Lock_block_production);
        case
          Json_only
          ~title:"Unlock_block_production"
          (obj1 (req "request" (constant "unlock_block_production")))
          (function View Unlock_block_production -> Some () | _ -> None)
          (fun () -> View Unlock_block_production);
      ]

  let pp _ppf (View _) = ()
end

module Worker = Octez_telemetry.Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let take_delayed_transactions evm_state maximum_number_of_chunks =
  let open Lwt_result_syntax in
  let maximum_cumulative_size =
    Sequencer_blueprint.maximum_usable_space_in_blueprint
      maximum_number_of_chunks
  in
  let maximum_delayed_transactions =
    maximum_cumulative_size / maximum_delayed_transaction_size
  in
  let*! delayed_transactions = Evm_state.delayed_inbox_hashes evm_state in
  let delayed_transactions =
    List.take_n maximum_delayed_transactions delayed_transactions
  in
  let remaining_cumulative_size =
    maximum_cumulative_size - (List.length delayed_transactions * 4096)
  in
  return (delayed_transactions, remaining_cumulative_size)

let produce_block_with_transactions ~signer ~timestamp ~transactions_and_objects
    ~delayed_hashes head_info =
  let open Lwt_result_syntax in
  let transactions, tx_hashes = List.split transactions_and_objects in
  Misc.with_timing
    (Blueprint_events.blueprint_production
       head_info.Evm_context.next_blueprint_number)
  @@ fun () ->
  let storage_version = head_info.storage_version in
  let tezosx_runtimes = head_info.tezosx_runtimes in
  let blueprint_version : Sequencer_blueprint.blueprint_version =
    if storage_version >= 48 && not (List.is_empty tezosx_runtimes) then V1
    else Legacy
  in
  let chunks =
    Sequencer_blueprint.make_blueprint_chunks
      ~number:head_info.next_blueprint_number
      {
        version = blueprint_version;
        parent_hash = head_info.current_block_hash;
        delayed_transactions = delayed_hashes;
        transactions;
        timestamp;
      }
  in
  (* Resolve the content of delayed transactions. *)
  let* delayed_transactions =
    List.map_es
      (fun delayed_hash ->
        Evm_state.get_delayed_inbox_item head_info.evm_state delayed_hash)
      delayed_hashes
  in
  let* blueprint_chunks, payload, confirmed_txs =
    Evm_context.apply_chunks ~signer timestamp chunks delayed_transactions
  in
  let (Qty number) = head_info.next_blueprint_number in
  let* () =
    Blueprints_publisher.publish
      number
      (Blueprints_publisher_types.Request.Blueprint
         {chunks = blueprint_chunks; inbox_payload = payload})
  in
  let*! () =
    List.iter_p
      (fun hash -> Block_producer_events.transaction_selected ~hash)
      (tx_hashes @ delayed_hashes)
  in
  return confirmed_txs

let produce_empty_block ~signer ~timestamp head_info =
  let open Lwt_result_syntax in
  let* hashes =
    produce_block_with_transactions
      ~signer
      ~timestamp
      ~transactions_and_objects:[]
      ~delayed_hashes:[]
      head_info
  in
  (* (Seq.length hashes) is always zero, this is only to be "future" proof *)
  return (`Block_produced (Seq.length hashes))

let validate_etherlink_tx ~maximum_cumulative_size
    (validation_state : Validation_types.validation_state) raw_tx
    (tx_object : Transaction_object.t) =
  let open Lwt_result_syntax in
  let new_size = validation_state.current_size + String.length raw_tx in
  if new_size > maximum_cumulative_size then return `Stop
  else
    let* validation_state_res =
      Prevalidator.validate_balance_gas_nonce_with_validation_state
        validation_state
        tx_object
    in
    match validation_state_res with
    | Ok validation_state ->
        return (`Keep {validation_state with current_size = new_size})
    | Error msg ->
        let hash = Transaction_object.hash tx_object in
        let*! () = Block_producer_events.transaction_rejected hash msg in
        return (`Drop msg)

let validate_tezlink_op ~maximum_cumulative_size
    (state : Validation_types.validation_state) raw_op
    (operation : Tezos_types.Operation.t) =
  let open Lwt_result_syntax in
  let new_size = state.current_size + String.length raw_op in
  if new_size > maximum_cumulative_size then return `Stop
  else if not (Tezlink_prevalidation.gas_limit_could_fit state operation) then
    return `Stop
  else
    let* res = Tezlink_prevalidation.validate_for_blueprint state operation in
    match res with
    | Ok state -> return (`Keep {state with current_size = new_size})
    | Error msg ->
        let hash = Operation_hash.hash_bytes [operation.raw] in
        let*! () = Block_producer_events.operation_rejected hash msg in
        return (`Drop msg)

let init_validation_state (head_info : Evm_context.head) =
  let open Lwt_result_syntax in
  let read = Evm_state.read head_info.evm_state in
  let michelson_config =
    let get_counter = Tezlink_durable_storage.counter read in
    let get_balance = Tezlink_durable_storage.balance_z read in
    Validation_types.{get_balance; get_counter}
  in
  let* minimum_base_fee_per_gas =
    Etherlink_durable_storage.minimum_base_fee_per_gas_opt read
  in
  let* base_fee_per_gas = Etherlink_durable_storage.base_fee_per_gas_opt read in
  let* maximum_gas_limit =
    Etherlink_durable_storage.maximum_gas_per_transaction read
  in
  let* da_fee_per_byte = Etherlink_durable_storage.da_fee_per_byte read in
  (* TODO #8236 / L2-862
     Using optional values for [minimum_base_fee_per_gas] and [base_fee_per_gas]
     is a temporary work around. In the context of Tezlink, no EVM block
     exists yet so reading these values from the block header raises
     [Invalid_block_structure]. Once the kernel writes an initial EVM block for
     all chain families, this fallback can be removed. *)
  let evm_config =
    Validation_types.
      {
        minimum_base_fee_per_gas =
          Qty
            (Option.value
               ~default:Fees.default_minimum_base_fee_per_gas
               minimum_base_fee_per_gas);
        base_fee_per_gas =
          Option.value
            ~default:(Qty Fees.default_minimum_base_fee_per_gas)
            base_fee_per_gas;
        maximum_gas_limit;
        da_fee_per_byte;
        next_nonce = (fun addr -> Etherlink_durable_storage.nonce read addr);
        balance = (fun addr -> Etherlink_durable_storage.balance read addr);
      }
  in
  return (Validation_types.empty_validation_state ~michelson_config ~evm_config)

let pop_valid_tx (type f) ~(tx_container : f Services_backend_sig.tx_container)
    (head_info : Evm_context.head) ~maximum_cumulative_size =
  let open Lwt_result_syntax in
  (* Skip validation if chain_family is Michelson. *)
  match tx_container with
  | Michelson_tx_container (module Tx_container) ->
      if maximum_cumulative_size <= Tezos_types.Operation.minimum_operation_size
      then return_nil
      else
        let read = Evm_state.read head_info.evm_state in
        let initial_validation_state =
          Tezlink_prevalidation.init_blueprint_validation read ()
        in
        let* l =
          Tx_container.pop_transactions
            ~maximum_cumulative_size
            ~validate_tx:(validate_tezlink_op ~maximum_cumulative_size)
            ~initial_validation_state
        in
        let l =
          List.map
            (fun (raw, tx) -> (raw, Tezos_types.Operation.hash_operation tx))
            l
        in
        return l
  | Evm_tx_container (module Tx_container) ->
      (* Low key optimization to avoid even checking the txpool if there is not
         enough space for the smallest transaction. *)
      if maximum_cumulative_size <= minimum_ethereum_transaction_size then
        return_nil
      else
        let read = Evm_state.read head_info.evm_state in
        let* minimum_base_fee_per_gas =
          Etherlink_durable_storage.minimum_base_fee_per_gas_opt read
        in
        let* base_fee_per_gas =
          Etherlink_durable_storage.base_fee_per_gas_opt read
        in
        let* maximum_gas_limit =
          Etherlink_durable_storage.maximum_gas_per_transaction read
        in
        let* da_fee_per_byte = Etherlink_durable_storage.da_fee_per_byte read in
        (* TODO #8236 / L2-862
           Using optional values for [minimum_base_fee_per_gas] and
           [base_fee_per_gas] is a temporary work around. In the context of
           Tezlink, no EVM block exists yet so reading these values from the
           block header raises [Invalid_block_structure]. Once the kernel writes
           an initial EVM block for all chain families, this fallback can be
           removed. *)
        let evm_config =
          Validation_types.
            {
              minimum_base_fee_per_gas =
                Ethereum_types.Qty
                  (Option.value
                     ~default:Fees.default_minimum_base_fee_per_gas
                     minimum_base_fee_per_gas);
              base_fee_per_gas =
                Option.value
                  ~default:
                    (Ethereum_types.Qty Fees.default_minimum_base_fee_per_gas)
                  base_fee_per_gas;
              maximum_gas_limit;
              da_fee_per_byte;
              next_nonce =
                (fun addr -> Etherlink_durable_storage.nonce read addr);
              balance =
                (fun addr -> Etherlink_durable_storage.balance read addr);
            }
        in
        let initial_validation_state =
          Validation_types.empty_validation_state
            ~michelson_config:Validation_types.dummy_michelson_config
            ~evm_config
        in
        let* l =
          Tx_container.pop_transactions
            ~maximum_cumulative_size
            ~validate_tx:(validate_etherlink_tx ~maximum_cumulative_size)
            ~initial_validation_state
        in
        let l =
          List.map (fun (raw, tx) -> (raw, Transaction_object.hash tx)) l
        in
        return l

(** Produces a block if we find at least one valid transaction in the
    transaction pool. *)
let produce_block_if_needed (type f) ~signer ~timestamp ~delayed_hashes
    ~transactions_and_objects
    ~(tx_container : f Services_backend_sig.tx_container)
    ~clear_pending_queue_after head_info =
  let open Lwt_result_syntax in
  let n = List.length transactions_and_objects + List.length delayed_hashes in
  if n > 0 then
    let* confirmed_txs =
      produce_block_with_transactions
        ~signer
        ~timestamp
        ~transactions_and_objects
        ~delayed_hashes
        head_info
    in
    let (module Tx_container) =
      Services_backend_sig.tx_container_module tx_container
    in
    let* () =
      Tx_container.confirm_transactions
        ~clear_pending_queue_after
        ~confirmed_txs
    in
    return (`Block_produced n)
  else return `No_block

let head_info_and_delayed_transactions ~with_delayed_transactions evm_state
    maximum_number_of_chunks =
  let open Lwt_result_syntax in
  (* We need to first fetch the delayed transactions then requests the head info.
     If the order is swapped, we might face a race condition where the delayed
     transactions are fetched from state more recent than head info. *)
  let* delayed_hashes, remaining_cumulative_size =
    if with_delayed_transactions then
      take_delayed_transactions evm_state maximum_number_of_chunks
    else
      return
        ( [],
          Sequencer_blueprint.maximum_usable_space_in_blueprint
            maximum_number_of_chunks )
  in
  return (delayed_hashes, remaining_cumulative_size)

(* [now] is the timestamp of the previously created block.
   We compute [next] as the maximum between:
   - [now + 500 ms], ensuring a minimal delay between blocks, and
   - [now'], the actual end time of the previous block. *)
let compute_next_block_timestamp ~now =
  let now' = Ptime_clock.now () in

  let t_500 =
    Ptime.Span.of_float_s 0.5 |> WithExceptions.Option.get ~loc:__LOC__
  in
  let now_plus =
    Ptime.add_span now t_500 |> WithExceptions.Option.get ~loc:__LOC__
  in
  Time.System.(max now_plus now') |> Time.System.to_protocol

let preconfirmation_stream_enabled state =
  match state.Types.preconfirmation_state with Disabled -> false | _ -> true

let set_preconfirmation_state (state : Types.state) preconfirmation_state =
  match state.preconfirmation_state with
  | Types.Disabled -> ()
  | Awaiting_first_timestamp | Enabled _ ->
      state.preconfirmation_state <- Enabled preconfirmation_state

let notify_next_block_info ~timestamp ~next_blueprint_number =
  let open Lwt_syntax in
  Broadcast.notify_next_block_info timestamp next_blueprint_number ;
  let* () = Events.sent_next_block_info timestamp next_blueprint_number in
  Evm_context.next_block_info timestamp next_blueprint_number

let notify_delayed_tx ~raw_tx ~tx_hash =
  Broadcast.notify_inclusion (Delayed raw_tx) tx_hash ;
  Events.sent_inclusion tx_hash

let notify_common_tx ~wrapped_raw_tx ~tx_hash =
  let open Lwt_syntax in
  Broadcast.notify_inclusion (Common wrapped_raw_tx) tx_hash ;
  let* () = Events.sent_inclusion tx_hash in
  return_unit

let add_selected_delayed_txs (head_info : Evm_context.head)
    (preconfirmation_state : Types.preconfirmation_state) delayed_hash =
  let open Lwt_result_syntax in
  match preconfirmation_state with
  | Potential_next_block_timestamp timestamp ->
      let* () =
        notify_next_block_info
          ~timestamp
          ~next_blueprint_number:head_info.next_blueprint_number
      in
      let* raw_tx =
        Evm_state.get_delayed_inbox_item head_info.evm_state delayed_hash
      in
      let*! () = notify_delayed_tx ~raw_tx ~tx_hash:delayed_hash in
      return
        (Types.Selecting_delayed_txs
           {timestamp; rev_delayed_txs = [delayed_hash]; current_size = 4096})
  | Selecting_delayed_txs
      {timestamp; rev_delayed_txs = rev_current; current_size} ->
      let* raw_tx =
        Evm_state.get_delayed_inbox_item head_info.evm_state delayed_hash
      in
      let*! () = notify_delayed_tx ~raw_tx ~tx_hash:delayed_hash in
      let rev_delayed_txs = delayed_hash :: rev_current in
      return
        (Types.Selecting_delayed_txs
           {timestamp; rev_delayed_txs; current_size = current_size + 4096})
  | Validating_txs _ ->
      invalid_arg "add_selected_delayed_txs called in invalid state"

let add_validated_tx (head_info : Evm_context.head) ~wrapped_raw_tx ~tx_hash
    validation_state preconfirmation_state =
  let open Lwt_result_syntax in
  let raw_tx =
    match wrapped_raw_tx with
    | Broadcast.Evm raw_tx -> raw_tx
    | Broadcast.Michelson raw_tx -> raw_tx
  in
  match preconfirmation_state with
  | Types.Potential_next_block_timestamp timestamp ->
      let* () =
        notify_next_block_info
          ~timestamp
          ~next_blueprint_number:head_info.next_blueprint_number
      in
      let*! () = notify_common_tx ~wrapped_raw_tx ~tx_hash in
      return
        (Types.Validating_txs
           {
             timestamp;
             rev_delayed_txs = [];
             validation_state;
             rev_validated_txs = [(raw_tx, tx_hash)];
           })
  | Selecting_delayed_txs {timestamp; rev_delayed_txs; current_size = _} ->
      let*! () = notify_common_tx ~wrapped_raw_tx ~tx_hash in
      return
        (Types.Validating_txs
           {
             timestamp;
             rev_delayed_txs;
             validation_state;
             rev_validated_txs = [(raw_tx, tx_hash)];
           })
  | Validating_txs
      {timestamp; rev_delayed_txs; rev_validated_txs; validation_state = _} ->
      let*! () = notify_common_tx ~wrapped_raw_tx ~tx_hash in
      return
        (Types.Validating_txs
           {
             timestamp;
             rev_delayed_txs;
             rev_validated_txs = (raw_tx, tx_hash) :: rev_validated_txs;
             validation_state;
           })

let produce_genesis ~(state : Types.state) ~timestamp ~parent_hash =
  let open Lwt_result_syntax in
  let delayed_transactions = [] in
  (* The genesis blueprint contains no transaction so the version
     field (which versions the format of the "transactions" field) is
     irrelevant. *)
  let chunks =
    Sequencer_blueprint.make_blueprint_chunks
      ~number:Ethereum_types.(Qty Z.zero)
      {
        version = Legacy;
        parent_hash;
        delayed_transactions;
        transactions = [];
        timestamp;
      }
  in
  let* genesis_chunks, genesis_payload, _ =
    Evm_context.apply_chunks
      ~signer:state.signer
      timestamp
      chunks
      delayed_transactions
  in
  set_preconfirmation_state
    state
    (Types.Potential_next_block_timestamp
       (compute_next_block_timestamp
          ~now:(Time.System.of_protocol_exn timestamp))) ;
  Blueprints_publisher.publish
    Z.zero
    (Blueprints_publisher_types.Request.Blueprint
       {chunks = genesis_chunks; inbox_payload = genesis_payload})

let choose_block_timestamp preconfirmation_state (force : force) =
  match (force, preconfirmation_state) with
  | With_timestamp t, _ -> t
  | _, Types.(Disabled | Awaiting_first_timestamp) -> Misc.now ()
  | _, Types.Enabled preconfirmation_state -> (
      match preconfirmation_state with
      | Types.Potential_next_block_timestamp timestamp
      | Selecting_delayed_txs {timestamp; _}
      | Validating_txs {timestamp; _} ->
          timestamp)

let produce_block (state : Types.state) ~force ~with_delayed_transactions =
  let open Lwt_result_syntax in
  let (Ex_tx_container tx_container) = state.tx_container in
  let (module Tx_container) =
    Services_backend_sig.tx_container_module tx_container
  in
  (* now and timestamp serve distinct but complementary purposes:
     - now: taken at the start of the request, used to predict the next timestamp
       and keep it aligned with real time.
     - timestamp: derived from the previously computed `next_block_timestamp`,
       ensuring consistency between preconfirmation and block creation.
  *)
  let now = Ptime_clock.now () in
  let timestamp = choose_block_timestamp state.preconfirmation_state force in
  let*! head_info = Evm_context.head_info () in
  let* () =
    when_ (not state.sunset) @@ fun () ->
    match head_info.pending_sequencer_upgrade with
    | Some Evm_events.Sequencer_upgrade.{timestamp = upgrade_timestamp; _}
      when Time.Protocol.(
             add timestamp state.sequencer_sunset_sec >= upgrade_timestamp
             && timestamp < upgrade_timestamp) ->
        (* We stop producing blocks ahead of the upgrade *)
        let*! () = Block_producer_events.sunset () in
        state.sunset <- true ;
        state.locked <- true ;
        return_unit
    | _ -> return_unit
  in
  let is_locked = state.locked in
  let has_preconfirmed_txs =
    match state.preconfirmation_state with
    | Enabled (Selecting_delayed_txs _ | Validating_txs _) -> true
    | _ -> false
  in
  if is_locked && not has_preconfirmed_txs then (
    let*! () = Block_producer_events.production_locked () in
    set_preconfirmation_state
      state
      (Potential_next_block_timestamp (compute_next_block_timestamp ~now)) ;
    return `No_block)
  else
    let is_going_to_upgrade_kernel =
      match head_info.pending_upgrade with
      | Some Evm_events.Upgrade.{hash = _; timestamp = upgrade_timestamp} ->
          timestamp >= upgrade_timestamp
      | None -> false
    in
    let signer = state.signer in
    if is_going_to_upgrade_kernel then (
      let* result = produce_empty_block ~signer ~timestamp head_info in
      set_preconfirmation_state
        state
        (Potential_next_block_timestamp (compute_next_block_timestamp ~now)) ;
      return result)
    else
      let* delayed_hashes, transactions_and_objects =
        match state.preconfirmation_state with
        | Disabled ->
            let* delayed_hashes, remaining_cumulative_size =
              head_info_and_delayed_transactions
                ~with_delayed_transactions
                head_info.evm_state
                state.maximum_number_of_chunks
            in
            let* transactions_and_hashes =
              pop_valid_tx
                ~tx_container
                head_info
                ~maximum_cumulative_size:remaining_cumulative_size
            in
            return (delayed_hashes, transactions_and_hashes)
        | Enabled (Potential_next_block_timestamp _) | Awaiting_first_timestamp
          ->
            let* delayed_hashes, _rem_size =
              head_info_and_delayed_transactions
                ~with_delayed_transactions
                head_info.evm_state
                state.maximum_number_of_chunks
            in
            return (delayed_hashes, [])
        | Enabled
            (Selecting_delayed_txs
               {rev_delayed_txs; timestamp = _; current_size = _}) ->
            return (List.rev rev_delayed_txs, [])
        | Enabled
            (Validating_txs
               {
                 rev_delayed_txs;
                 rev_validated_txs;
                 timestamp = _;
                 validation_state = _;
               }) ->
            return (List.rev rev_delayed_txs, List.rev rev_validated_txs)
      in
      let* result =
        produce_block_if_needed
          ~signer
          ~timestamp
          ~transactions_and_objects
          ~delayed_hashes
          ~tx_container
          ~clear_pending_queue_after:
            (not (preconfirmation_stream_enabled state))
          head_info
      in
      let* result =
        match (result, force) with
        | `No_block, (True | With_timestamp _) ->
            produce_empty_block ~signer ~timestamp head_info
        | result, _ -> return result
      in
      set_preconfirmation_state
        state
        (Potential_next_block_timestamp (compute_next_block_timestamp ~now)) ;
      return result

let preconfirm_delayed_transactions
    (preconfirmation_state : Types.preconfirmation_state)
    ~maximum_number_of_chunks (head_info : Evm_context.head) =
  let open Lwt_result_syntax in
  let* delayed_hashes, remaining_cumulative_size =
    head_info_and_delayed_transactions
      ~with_delayed_transactions:true
      head_info.evm_state
      maximum_number_of_chunks
  in
  let* preconfirmation_state =
    List.fold_left_es
      (add_selected_delayed_txs head_info)
      preconfirmation_state
      delayed_hashes
  in
  return (remaining_cumulative_size, preconfirmation_state)

let preconfirm_transaction ~maximum_cumulative_size validation_state ~raw_tx
    transaction_object =
  let open Lwt_result_syntax in
  let* res, tx_hash, wrapped_raw_tx =
    match transaction_object with
    | Tx_queue_types.Evm tx_object ->
        let* res =
          validate_etherlink_tx
            ~maximum_cumulative_size
            validation_state
            raw_tx
            tx_object
        in
        return (res, Transaction_object.hash tx_object, Broadcast.Evm raw_tx)
    | Tx_queue_types.Michelson operation ->
        let* res =
          validate_tezlink_op
            ~maximum_cumulative_size
            validation_state
            raw_tx
            operation
        in
        return
          ( res,
            Tezos_types.Operation.hash_operation operation,
            Broadcast.Michelson raw_tx )
  in
  match res with
  | `Drop msg ->
      Broadcast.notify_dropped ~hash:tx_hash ~reason:msg ;
      return (tx_hash, `Dropped)
  | `Keep validation_state ->
      let*! () = Events.inclusion tx_hash in
      return (tx_hash, `Continue (wrapped_raw_tx, validation_state))
  | `Stop -> return (tx_hash, `Stop)

let preconfirm_transactions
    (preconfirmation_state : Types.preconfirmation_state)
    ~maximum_number_of_chunks ~transactions =
  let open Lwt_result_syntax in
  let maximum_cumulative_size =
    Sequencer_blueprint.maximum_usable_space_in_blueprint
      maximum_number_of_chunks
  in
  let*! head_info = Evm_context.head_info () in
  Octez_telemetry.Trace.add_attrs (fun () ->
      [Telemetry.Attributes.Block.number head_info.next_blueprint_number]) ;
  let* validation_state, preconfirmation_state =
    match preconfirmation_state with
    | Potential_next_block_timestamp _ ->
        let* remaining_cumulative_size, preconfirmation_state =
          preconfirm_delayed_transactions
            preconfirmation_state
            (head_info : Evm_context.head)
            ~maximum_number_of_chunks
        in
        let* validation_state = init_validation_state head_info in
        return
          ( {
              validation_state with
              current_size =
                Int.sub maximum_cumulative_size remaining_cumulative_size;
            },
            preconfirmation_state )
    | Selecting_delayed_txs {current_size; _} ->
        let* validation_state = init_validation_state head_info in
        return ({validation_state with current_size}, preconfirmation_state)
    | Validating_txs {validation_state; _} ->
        return (validation_state, preconfirmation_state)
  in
  let aux (validation_state, preconfirmation_state, rev_hashes)
      (raw_tx, transaction_object) =
    let* hash, res =
      preconfirm_transaction
        ~maximum_cumulative_size
        validation_state
        ~raw_tx
        transaction_object
    in
    match res with
    | `Dropped ->
        return
          ( validation_state,
            preconfirmation_state,
            {rev_hashes with refused = hash :: rev_hashes.refused} )
    | `Continue (wrapped_raw_tx, validation_state) ->
        let* preconfirmation_state =
          add_validated_tx
            head_info
            ~wrapped_raw_tx
            ~tx_hash:hash
            validation_state
            preconfirmation_state
        in
        return
          ( validation_state,
            preconfirmation_state,
            {rev_hashes with accepted = hash :: rev_hashes.accepted} )
    | `Stop ->
        return
          ( validation_state,
            preconfirmation_state,
            {rev_hashes with dropped = hash :: rev_hashes.dropped} )
  in
  let* ( _validation_state,
         preconfirmation_state,
         {accepted = rev_accepted; refused = rev_refused; dropped = rev_dropped}
       ) =
    List.fold_left_es
      aux
      ( validation_state,
        preconfirmation_state,
        {accepted = []; refused = []; dropped = []} )
      transactions
  in
  return
    ( preconfirmation_state,
      {
        accepted = List.rev rev_accepted;
        refused = List.rev rev_refused;
        dropped = List.rev rev_dropped;
      } )

type error += IC_disabled

module Handlers = struct
  type self = worker

  let on_request : type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Produce_genesis (timestamp, parent_hash) ->
        produce_genesis ~state ~timestamp ~parent_hash
    | Request.Produce_block {with_delayed_transactions; force} ->
        protect @@ fun () ->
        produce_block state ~force ~with_delayed_transactions
    | Request.Propose_next_block_timestamp timestamp ->
        protect @@ fun () ->
        set_preconfirmation_state
          state
          (Potential_next_block_timestamp timestamp) ;
        Lwt_result_syntax.return_unit
    | Request.Preconfirm_transactions transactions -> (
        let open Lwt_result_syntax in
        protect @@ fun () ->
        (* Refuse preconfirmations when locked, before the first
           created block, or when preconfirmations are explicitly disabled *)
        if state.locked then tzfail IC_disabled
        else
          match state.preconfirmation_state with
          | Disabled -> tzfail IC_disabled
          | Awaiting_first_timestamp -> tzfail IC_disabled
          | Enabled preconfirmation_state ->
              let* preconfirmation_state, selected_txns_hashes =
                preconfirm_transactions
                  ~maximum_number_of_chunks:state.maximum_number_of_chunks
                  ~transactions
                  preconfirmation_state
              in
              set_preconfirmation_state state preconfirmation_state ;
              return selected_txns_hashes)
    | Request.Lock_block_production ->
        protect @@ fun () ->
        state.locked <- true ;
        Lwt_result_syntax.return_unit
    | Request.Unlock_block_production ->
        protect @@ fun () ->
        state.locked <- false ;
        Lwt_result_syntax.return_unit

  type launch_error = error trace

  let on_launch _w ()
      ({
         signer;
         maximum_number_of_chunks;
         tx_container;
         sequencer_sunset_sec;
         preconfirmation_stream_enabled;
       } :
        Types.parameters) =
    Lwt_result_syntax.return
      Types.
        {
          sunset = false;
          locked = false;
          signer;
          maximum_number_of_chunks;
          tx_container;
          sequencer_sunset_sec;
          preconfirmation_state =
            (if preconfirmation_stream_enabled then Awaiting_first_timestamp
             else Disabled);
        }

  let on_error (type a b) _w _st (_r : (a, b) Request.t) (_errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    Lwt_result_syntax.return `Continue

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_block_producer

type error += Block_producer_terminated

let () =
  register_error_kind
    `Permanent
    ~id:"No_block_producer"
    ~title:"No_block_producer"
    ~description:
      "Failed to add a request to the Block producer, it was not started."
    Data_encoding.unit
    (function No_block_producer -> Some () | _ -> None)
    (fun () -> No_block_producer) ;
  register_error_kind
    `Permanent
    ~id:"Block_producer_is_closed"
    ~title:"Block_producer_is_closed"
    ~description:"Failed to add a request to the Block producer, it's closed."
    Data_encoding.unit
    (function No_block_producer -> Some () | _ -> None)
    (fun () -> No_block_producer) ;
  register_error_kind
    `Permanent
    ~id:"Instant_confirmation_is_disabled"
    ~title:"Instant_confirmation_is_disabled"
    ~description:"Instant confirmation is disabled, request can't be traited."
    Data_encoding.unit
    (function IC_disabled -> Some () | _ -> None)
    (fun () -> IC_disabled)

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail e -> Error (TzTrace.make @@ error_of_exn e)
    | Lwt.Sleep -> Error (TzTrace.make No_block_producer))

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Block_producer_terminated]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let start parameters =
  let open Lwt_result_syntax in
  let*! () = Block_producer_events.started () in
  let+ worker = Worker.launch table () parameters (module Handlers) in
  Lwt.wakeup worker_waker worker

let shutdown () =
  let open Lwt_syntax in
  let w = Lazy.force worker in
  match w with
  | Error _ -> Lwt.return_unit
  | Ok w ->
      let* () = Block_producer_events.shutdown () in
      Worker.shutdown w

let produce_genesis ~timestamp ~parent_hash =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Produce_genesis (timestamp, parent_hash))
  |> handle_request_error

let produce_block ~with_delayed_transactions ~force =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Produce_block {with_delayed_transactions; force})
  |> handle_request_error

let preconfirm_transactions ~transactions =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Preconfirm_transactions transactions)
  |> handle_request_error

let lock_block_production () =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  let*! (_pushed : bool) =
    Worker.Queue.push_request worker Request.Lock_block_production
  in
  return_unit

let unlock_block_production () =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  let*! (_pushed : bool) =
    Worker.Queue.push_request worker Request.Unlock_block_production
  in
  return_unit

module Internal_for_tests = struct
  let produce_block ~with_delayed_transactions =
    produce_block ~with_delayed_transactions

  let propose_next_block_timestamp ~next_block_timestamp =
    let open Lwt_result_syntax in
    let*? worker = Lazy.force worker in
    Worker.Queue.push_request_and_wait
      worker
      (Request.Propose_next_block_timestamp next_block_timestamp)
    |> handle_request_error
end

let produce_block = produce_block ~with_delayed_transactions:true
