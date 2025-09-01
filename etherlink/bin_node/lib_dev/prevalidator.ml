(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type error += Gas_limit_too_low of string | Prague_not_enabled

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_gas_limit_too_low"
    ~title:"Gas limit too low"
    ~description:
      "Transaction with a gas limit below the set threshold is not allowed"
    ~pp:(fun ppf gas_limit ->
      Format.fprintf
        ppf
        "Transaction with a gas limit below %s is not allowed"
        gas_limit)
    Data_encoding.(obj1 (req "gas_limit" string))
    (function Gas_limit_too_low gas_limit -> Some gas_limit | _ -> None)
    (fun gas_limit -> Gas_limit_too_low gas_limit) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_prague_not_enabled"
    ~title:"Prague is not enabled"
    ~description:"Prague is not enabled yet on the kernel"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Prague is not enabled yet on the kernel")
    Data_encoding.empty
    (function Prague_not_enabled -> Some () | _ -> None)
    (fun () -> Prague_not_enabled)

type mode = Minimal | Full

module Types = struct
  type 'a inner_session = {
    state : 'a;
    state_backend : (module Services_backend_sig.S with type Reader.state = 'a);
    minimum_base_fee_per_gas : Z.t;
    base_fee_per_gas : Z.t;
    da_fee_per_bytes : Z.t;
    maximum_gas_per_transaction : Z.t;
    storage_version : int;
  }

  type session = Session : 'a inner_session -> session

  let session_of_state (type state) state_backend state =
    let open Lwt_result_syntax in
    let (module Backend_rpc : Services_backend_sig.S
          with type Reader.state = state) =
      state_backend
    in
    let* (Qty base_fee_per_gas) =
      Lwt.catch
        (fun () ->
          Etherlink_durable_storage.base_fee_per_gas
            (Backend_rpc.Reader.read state))
        (function
          | Durable_storage.Invalid_block_structure _ ->
              (* Placeholder value for observer starting from genesis.
                 It will be updated after the first block *)
              return (Qty Z.zero)
          | exn -> Lwt.reraise exn)
    in
    let* minimum_base_fee_per_gas =
      Etherlink_durable_storage.minimum_base_fee_per_gas
        (Backend_rpc.Reader.read state)
    in
    let* (Qty da_fee_per_bytes) =
      Etherlink_durable_storage.da_fee_per_byte (Backend_rpc.Reader.read state)
    in
    let* (Qty maximum_gas_per_transaction) =
      Etherlink_durable_storage.maximum_gas_per_transaction
        (Backend_rpc.Reader.read state)
    in
    let* storage_version =
      Durable_storage.storage_version (Backend_rpc.Reader.read state)
    in
    return
      (Session
         {
           state;
           state_backend;
           storage_version;
           base_fee_per_gas;
           minimum_base_fee_per_gas;
           da_fee_per_bytes;
           maximum_gas_per_transaction;
         })

  type parameters = {
    mode : mode;
    chain_id : L2_types.chain_id;
    max_number_of_chunks : int option;
    session : session;
  }

  type state = {
    mode : mode;
    chain_id : L2_types.chain_id;
    max_number_of_chunks : int option;
    mutable session : session;
  }
end

open Types

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node_worker"; "prevalidator"]

  let pp _fmt () = ()

  let equal () () = true
end

type prevalidation_result = {
  next_nonce : quantity;
  transaction_object : legacy_transaction_object;
}

module Request = struct
  type (_, _) t =
    | Prevalidate_raw_transaction : {
        raw_transaction : string;
      }
        -> ((prevalidation_result, string) result, tztrace) t
    | Refresh_state : (unit, tztrace) t

  let name : type a err. (a, err) t -> string = function
    | Prevalidate_raw_transaction _ -> "Prevalidate_raw_transaction"
    | Refresh_state -> "Refresh_state"

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"Prevalidate_raw_transaction"
          Json_only
          (obj2
             (req "request" (constant "prevalidate_raw_transaction"))
             (req "raw_transaction" (string' Hex)))
          (function
            | View (Prevalidate_raw_transaction {raw_transaction}) ->
                Some ((), raw_transaction)
            | _ -> None)
          (fun ((), _) ->
            (* Only used for logging *)
            assert false);
        case
          ~title:"Refresh_state"
          Json_only
          (obj1 (req "request" (constant "replace_state")))
          (function View Refresh_state -> Some () | _ -> None)
          (fun () ->
            (* Only used for logging *)
            assert false);
      ]

  let pp ppf view =
    Data_encoding.Json.pp ppf @@ Data_encoding.Json.construct encoding view
end

let ( let** ) v f =
  let open Lwt_result_syntax in
  let* r = v in
  match r with Ok v -> f v | Error err -> return (Error err)

let ( let**? ) v f =
  let open Lwt_result_syntax in
  match v with Ok v -> f v | Error err -> return (Error err)

let validate_chain_id ctxt (transaction : Transaction.transaction) :
    (unit, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  match transaction.chain_id with
  | None -> return (Ok ())
  | Some transaction_chain_id ->
      let (Chain_id chain_id) = ctxt.chain_id in
      if Z.equal transaction_chain_id chain_id then return (Ok ())
      else return (Error "Invalid chain id")

let validate_nonce ~next_nonce:(Qty next_nonce)
    (transaction : Transaction.transaction) =
  let open Lwt_result_syntax in
  if transaction.nonce >= next_nonce then return (Ok ())
  else return (Error "Nonce too low")

let validate_gas_limit session (transaction : Transaction.transaction) :
    (unit, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  (* Computing the execution gas limit validates that the gas limit is
     sufficient to cover the inclusion fees. *)
  let**? execution_gas_limit =
    (* since Dionysus, the execution gas limit is always computed from the
       minimum base fee per gas *)
    Fees.execution_gas_limit
      ~da_fee_per_byte:(Qty session.da_fee_per_bytes)
      ~access_list:transaction.access_list
      ~minimum_base_fee_per_gas:session.minimum_base_fee_per_gas
      ~gas_limit:transaction.gas_limit
      transaction.data
  in
  if session.storage_version < 34 then
    if Compare.Z.(execution_gas_limit <= session.maximum_gas_per_transaction)
    then return (Ok ())
    else
      return
        (Error
           (Format.asprintf
              "Gas limit for execution is too high. Maximum limit is %a, \
               transaction has %a"
              Z.pp_print
              session.maximum_gas_per_transaction
              Z.pp_print
              execution_gas_limit))
  else return (Ok ())

let validate_sender_not_a_contract (type state) session caller :
    (unit, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  let (module Backend_rpc : Services_backend_sig.S
        with type Reader.state = state) =
    session.state_backend
  in
  let* (Hex code) =
    Etherlink_durable_storage.code
      (Backend_rpc.Reader.read session.state)
      caller
  in
  if
    (* EOA: *)
    code = ""
    ||
    (* EIP-7702 authorized account: *)
    String.starts_with ~prefix:"ef0100" code
  then return (Ok ())
  else return (Error "Sender is a contract which is not possible")

let validate_max_fee_per_gas ~base_fee_per_gas:(Qty base_fee_per_gas)
    (transaction : Transaction.transaction) =
  let open Lwt_result_syntax in
  if transaction.max_fee_per_gas >= base_fee_per_gas then return (Ok ())
  else return (Error "Max gas fee too low")

let validate_balance_is_enough (transaction : Transaction.transaction) ~balance
    =
  let open Lwt_result_syntax in
  let gas = transaction.gas_limit in
  let gas_price = transaction.max_fee_per_gas in
  let gas_cost = Z.mul gas gas_price in
  let total_cost =
    let value = transaction.value in
    Z.add gas_cost value
  in
  if gas_cost > balance then return (Error "Cannot prepay transaction.")
  else if total_cost > balance then return (Error "Not enough funds")
  else return (Ok total_cost)

let tx_data_size_limit_reached ~max_number_of_chunks ~tx_data =
  let max_number_of_chunks =
    Option.value
      max_number_of_chunks
      ~default:Sequencer_blueprint.maximum_chunks_per_l1_level
  in
  Bytes.length tx_data
  > Sequencer_blueprint.maximum_usable_space_in_blueprint
      (* Minus one so that the "rest" of the raw transaction can
         be contained within one of the chunks. *)
      (max_number_of_chunks - 1)

let validate_tx_data_size ~max_number_of_chunks
    (transaction : Transaction.transaction) =
  let open Lwt_result_syntax in
  let tx_data = transaction.data in
  if tx_data_size_limit_reached ~max_number_of_chunks ~tx_data then
    return @@ Error "Transaction data exceeded the allowed size."
  else return (Ok ())

let minimal_validation ~next_nonce ~max_number_of_chunks ctxt transaction
    ~caller =
  let open Lwt_result_syntax in
  let (Session session) = ctxt.session in
  let minimum_gas_limit =
    let base_intrisic_gas_cost = Z.of_int 21_000 in
    let da_inclusion_fees =
      Fees.da_fees_gas_limit_overhead
        ~da_fee_per_byte:(Qty session.da_fee_per_bytes)
        ~minimum_base_fee_per_gas:session.minimum_base_fee_per_gas
        transaction.Transaction.data
    in
    Z.add da_inclusion_fees base_intrisic_gas_cost
  in
  let* () =
    (* Early exit: transaction with a gas limit below `minimum_gas_limit` is not allowed. *)
    when_ (transaction.Transaction.gas_limit < minimum_gas_limit) @@ fun () ->
    fail [Gas_limit_too_low (Z.to_string minimum_gas_limit)]
  in
  let** () = validate_chain_id ctxt transaction in
  let** () = validate_nonce ~next_nonce transaction in
  let** () = validate_sender_not_a_contract session caller in
  let** () = validate_tx_data_size ~max_number_of_chunks transaction in
  let** () = validate_gas_limit session transaction in
  return (Ok ())

let validate_balance_and_max_fee_per_gas ~base_fee_per_gas ~transaction
    ~from_balance:(Qty from_balance) =
  let open Lwt_result_syntax in
  let** () = validate_max_fee_per_gas ~base_fee_per_gas transaction in
  let** total_cost =
    validate_balance_is_enough transaction ~balance:from_balance
  in
  return (Ok total_cost)

let validate_balance_and_gas_with_backend (type state) ~caller session
    transaction =
  let open Lwt_result_syntax in
  let (module Backend_rpc : Services_backend_sig.S
        with type Reader.state = state) =
    session.state_backend
  in
  let* from_balance =
    Etherlink_durable_storage.balance
      (Backend_rpc.Reader.read session.state)
      caller
  in
  let** _total_cost =
    validate_balance_and_max_fee_per_gas
      ~base_fee_per_gas:(Qty session.base_fee_per_gas)
      ~transaction
      ~from_balance
  in
  return (Ok ())

let full_validation ~next_nonce ~max_number_of_chunks ~caller ctxt transaction =
  let open Lwt_result_syntax in
  let** () =
    minimal_validation
      ~next_nonce
      ~max_number_of_chunks
      ~caller
      ctxt
      transaction
  in
  let (Session session) = ctxt.session in
  let** () =
    validate_balance_and_gas_with_backend ~caller session transaction
  in
  return (Ok ())

let valid_transaction_object (type state) ctxt session mode hash txn =
  let open Lwt_result_syntax in
  let (module Backend_rpc : Services_backend_sig.S
        with type Reader.state = state) =
    session.state_backend
  in
  let**? transaction_object = Transaction.to_transaction_object ~hash txn in
  let caller = transaction_object.from in
  let* next_nonce =
    Etherlink_durable_storage.nonce
      (Backend_rpc.Reader.read session.state)
      caller
  in
  let next_nonce =
    match next_nonce with None -> Qty Z.zero | Some next_nonce -> next_nonce
  in
  let** () =
    match mode with
    | Minimal ->
        minimal_validation
          ~max_number_of_chunks:ctxt.max_number_of_chunks
          ~next_nonce
          ~caller
          ctxt
          txn
    | Full ->
        full_validation
          ~next_nonce
          ~max_number_of_chunks:ctxt.max_number_of_chunks
          ~caller
          ctxt
          txn
  in

  return (Ok {next_nonce; transaction_object})

module Worker = Octez_telemetry.Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  type launch_error = tztrace

  let on_launch _self ()
      ({chain_id; mode; max_number_of_chunks; session} : Types.parameters) =
    let open Lwt_result_syntax in
    return ({chain_id; mode; max_number_of_chunks; session} : Types.state)

  let is_tx_valid (type state) ctxt session raw_transaction :
      (prevalidation_result, string) result tzresult Lwt.t =
    let open Lwt_result_syntax in
    let (module Backend_rpc : Services_backend_sig.S
          with type Reader.state = state) =
      session.state_backend
    in
    let hash = Ethereum_types.hash_raw_tx raw_transaction in
    let**? txn = Transaction.decode raw_transaction in
    let* () =
      when_
        (txn.transaction_type = Transaction.Eip7702
        && session.storage_version < 37)
      @@ fun () -> tzfail Prague_not_enabled
    in
    valid_transaction_object ctxt session ctxt.mode hash txn

  let refresh_state (type state) ctxt session =
    let open Lwt_result_syntax in
    let (module Backend_rpc : Services_backend_sig.S
          with type Reader.state = state) =
      session.state_backend
    in
    let* state = Backend_rpc.Reader.get_state () in
    let* session = Types.session_of_state session.state_backend state in
    ctxt.session <- session ;
    return_unit

  let on_request : type r err.
      self -> (r, err) Request.t -> (r, err) result Lwt.t =
   fun self request ->
    let ctxt = Worker.state self in
    let (Session session) = ctxt.session in
    match request with
    | Prevalidate_raw_transaction {raw_transaction} ->
        is_tx_valid ctxt session raw_transaction
    | Refresh_state -> refresh_state ctxt session

  let on_completion (type a err) _self (_r : (a, err) Request.t) (_res : a) _st
      =
    Lwt_syntax.return_unit

  let on_no_request _self = Lwt.return_unit

  let on_close _self = Lwt.return_unit

  let on_error (type a b) _self _st (_req : (a, b) Request.t) (_errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    let open Lwt_result_syntax in
    return `Continue
end

type worker_status =
  | Not_started  (** {!start} was not called yet *)
  | Starting of unit Lwt.t
      (** {!start} was called, and is currently being executed. The promise
          will be fulfilled once the initialization of the worker is done. *)
  | Started of Worker.infinite Worker.queue Worker.t
      (** {!start} was called successfully, meaning it is now possible to interact with the worker. *)
  | Pending_valid_state : {
      mode : mode;
      max_number_of_chunks : int option;
      state_backend :
        (module Services_backend_sig.S with type Reader.state = 'a);
      chain_family : 'f L2_types.chain_family;
    }
      -> worker_status
      (** {!start} was called, but failed. This means the node does not have
          access to a state with the necessary information yet. The next time a
          request is sent to the worker, we will retry calling {!start}. *)

let worker = ref Not_started

let table = Worker.create_table Queue

type error += No_worker

let start (type state f) ?max_number_of_chunks
    ~(chain_family : f L2_types.chain_family) mode state_backend =
  let open Lwt_result_syntax in
  let starting_promise, starting_waker = Lwt.task () in
  worker := Starting starting_promise ;
  let*! start_result =
    protect @@ fun () ->
    match chain_family with
    | L2_types.EVM ->
        let (module Backend_rpc : Services_backend_sig.S
              with type Reader.state = state) =
          state_backend
        in
        let* state = Backend_rpc.Reader.get_state () in
        let* chain_id =
          Durable_storage.chain_id (Backend_rpc.Reader.read state)
        in
        let* session = Types.session_of_state state_backend state in
        let* w =
          Worker.launch
            table
            ()
            {chain_id; mode; max_number_of_chunks; session}
            (module Handlers)
        in
        worker := Started w ;
        Lwt.wakeup starting_waker () ;
        let*! () = Prevalidator_events.is_ready () in
        return_unit
    | Michelson ->
        (* Tezlink does not use the prevalidator worker *)
        worker := Not_started ;
        Lwt.wakeup starting_waker () ;
        return_unit
  in
  match start_result with
  | Ok () -> return_unit
  | Error _ ->
      let*! () = Prevalidator_events.cannot_start () in
      worker :=
        Pending_valid_state
          {max_number_of_chunks; mode; state_backend; chain_family} ;
      Lwt.wakeup starting_waker () ;
      return_unit

let check_status_and_return worker =
  match Worker.status worker with
  | Closed (_, _, errs) -> (
      match errs with
      | Some errs -> Error errs
      | None -> error_with "Worker failed with no error")
  | _ -> Ok worker

let rec get_worker ?(allow_retry = true) () =
  let open Lwt_result_syntax in
  match !worker with
  | Started worker -> Lwt.return (check_status_and_return worker)
  | Starting p ->
      let*! () = p in
      get_worker ~allow_retry ()
  | Pending_valid_state
      {max_number_of_chunks; mode; state_backend; chain_family}
    when allow_retry ->
      let* () = start ~chain_family ?max_number_of_chunks mode state_backend in
      get_worker ~allow_retry:false ()
  | Pending_valid_state _ | Not_started -> tzfail No_worker

let return_ : (_, _ Worker.message_error) result -> _ =
  let open Lwt_result_syntax in
  function
  | Ok res -> return res
  | Error (Closed (Some trace)) -> Lwt.return (Error trace)
  | Error (Closed None) ->
      failwith
        "Cannot interact with the EVM context worker because it is closed"
  | Error (Request_error err) -> Lwt.return (Error err)
  | Error (Any exn) -> fail_with_exn exn

let worker_wait_for_request req =
  let open Lwt_result_syntax in
  let* w = get_worker () in
  let*! res = Worker.Queue.push_request_and_wait w req in
  return_ res

let bind_worker f =
  let open Lwt_result_syntax in
  let*! res = get_worker () in
  match res with
  | Error [No_worker] ->
      (* There is no worker, nothing to do *)
      return_unit
  | Error errs -> fail errs
  | Ok w -> f w

let worker_add_request ~request =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! (_pushed : bool) = Worker.Queue.push_request w request in
  return_unit

let prevalidate_raw_transaction raw_transaction =
  worker_wait_for_request
    (Request.Prevalidate_raw_transaction {raw_transaction})

let refresh_state () = worker_add_request ~request:Request.Refresh_state

type validation_config = {
  minimum_base_fee_per_gas : Ethereum_types.quantity;
  base_fee_per_gas : Ethereum_types.quantity;
  maximum_gas_limit : Ethereum_types.quantity;
  da_fee_per_byte : Ethereum_types.quantity;
  next_nonce :
    Ethereum_types.address -> Ethereum_types.quantity option tzresult Lwt.t;
  balance : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t;
}

type validation_state = {
  config : validation_config;
  addr_balance : Z.t String.Map.t;
  addr_nonce : Z.t String.Map.t;
}

let validate_balance_gas_nonce_with_validation_state validation_state
    ~(caller : Ethereum_types.address) (transaction : Transaction.transaction) :
    (validation_state, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  let (Address (Hex caller_str)) = caller in
  let* next_nonce =
    let nonce = String.Map.find caller_str validation_state.addr_nonce in
    match nonce with
    | Some nonce -> return nonce
    | None -> (
        let* nonce = validation_state.config.next_nonce caller in
        match nonce with
        | Some (Qty nonce) -> return nonce
        | None -> return Z.zero)
  in
  let** () =
    let tx_nonce = transaction.nonce in
    if Z.equal tx_nonce next_nonce then return (Ok ())
    else return (Error "Transaction nonce is not the expected nonce.")
  in
  let* from_balance =
    let from_balance =
      String.Map.find caller_str validation_state.addr_balance
    in
    match from_balance with
    | Some balance -> return balance
    | None ->
        let* (Qty balance) = validation_state.config.balance caller in
        return balance
  in
  let** total_cost =
    validate_balance_and_max_fee_per_gas
      ~base_fee_per_gas:validation_state.config.base_fee_per_gas
      ~transaction
      ~from_balance:(Qty from_balance)
  in
  let* addr_balance =
    match transaction.to_ with
    | None -> return validation_state.addr_balance
    | Some to_ ->
        let (`Hex to_) = Hex.of_bytes to_ in
        let to_balance = String.Map.find to_ validation_state.addr_balance in
        let* to_balance =
          match to_balance with
          | Some balance -> return balance
          | None ->
              let* (Qty balance) =
                validation_state.config.balance (Address (Hex to_))
              in
              return balance
        in
        let new_to_balance = Z.add transaction.value to_balance in
        let addr_balance =
          String.Map.add to_ new_to_balance validation_state.addr_balance
        in
        return addr_balance
  in
  let validation_state =
    let new_from_balance = Z.sub from_balance total_cost in
    let addr_balance =
      String.Map.add caller_str new_from_balance addr_balance
    in
    let addr_nonce =
      String.Map.add caller_str (Z.succ next_nonce) validation_state.addr_nonce
    in
    {validation_state with addr_balance; addr_nonce}
  in
  return (Ok validation_state)
