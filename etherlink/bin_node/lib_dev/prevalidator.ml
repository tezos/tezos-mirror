(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types
open Validation_types

type error +=
  | Gas_limit_too_low of {gas_limit : Z.t; minimum_gas_limit_required : Z.t}
  | Prague_not_enabled

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_gas_limit_too_low"
    ~title:"Gas limit too low"
    ~description:
      "Transaction with a gas limit below the set threshold is not allowed"
    ~pp:(fun ppf (gas_limit, minimum_gas_limit_required) ->
      Format.fprintf
        ppf
        "The provided gas limit (%a) is insufficient to cover the transaction \
         cost of %a gas. Please increase the gas limit or use eth_estimateGas \
         to get the recommended amount."
        Z.pp_print
        gas_limit
        Z.pp_print
        minimum_gas_limit_required)
    Data_encoding.(
      obj2
        (req "gas_limit" Data_encoding.z)
        (req "minimum_gas_limit_required" Data_encoding.z))
    (function
      | Gas_limit_too_low {gas_limit; minimum_gas_limit_required} ->
          Some (gas_limit, minimum_gas_limit_required)
      | _ -> None)
    (fun (gas_limit, minimum_gas_limit_required) ->
      Gas_limit_too_low {gas_limit; minimum_gas_limit_required}) ;
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

module K = struct
  (* Constants extracted from several EIPs.
     For more details see:
     - https://eips.ethereum.org/EIPS/eip-7623
     - https://eips.ethereum.org/EIPS/eip-3860 *)

  let base_intrisic_gas_cost = 21_000

  let nonzero_bytes_cost = 16

  let standard_token_cost = 4

  let non_zero_byte_multiplier = nonzero_bytes_cost / standard_token_cost

  let total_cost_floor_per_token = 10

  let base_creation_cost = 32_000

  let initcode_word_cost = 2

  let word_size = 32

  let access_list_address = 2_400

  let access_list_storage_key = 1_900

  let eip7702_empty_account_cost = 25_000
end

type mode = Minimal | Full

module Types = struct
  type etherlink_infos = {
    minimum_base_fee_per_gas : Z.t;
    base_fee_per_gas : Z.t;
    da_fee_per_bytes : Z.t;
    maximum_gas_per_transaction : Z.t;
  }

  let etherlink_infos_default =
    {
      minimum_base_fee_per_gas = Z.zero;
      base_fee_per_gas = Z.zero;
      da_fee_per_bytes = Z.zero;
      maximum_gas_per_transaction = Z.zero;
    }

  type 'a inner_session = {
    state : 'a;
    state_backend : (module Services_backend_sig.S with type Reader.state = 'a);
    storage_version : int;
    etherlink_infos : etherlink_infos;
  }

  type session = Session : 'a inner_session -> session

  let etherlink_infos_of_state (type state) state_backend state =
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
    return
      {
        base_fee_per_gas;
        minimum_base_fee_per_gas;
        da_fee_per_bytes;
        maximum_gas_per_transaction;
      }

  let session_of_state (type state) chain_family state_backend state =
    let open Lwt_result_syntax in
    let (module Backend_rpc : Services_backend_sig.S
          with type Reader.state = state) =
      state_backend
    in
    let* storage_version =
      Durable_storage.storage_version (Backend_rpc.Reader.read state)
    in
    match chain_family with
    | L2_types.Ex_chain_family EVM ->
        let* etherlink_infos = etherlink_infos_of_state state_backend state in
        return
          (Session {state; state_backend; storage_version; etherlink_infos})
    | L2_types.Ex_chain_family Michelson ->
        return
          (Session
             {
               state;
               state_backend;
               storage_version;
               etherlink_infos = etherlink_infos_default;
             })

  type parameters = {
    mode : mode;
    chain_id : L2_types.chain_id;
    chain_family : L2_types.ex_chain_family;
    max_number_of_chunks : int option;
    session : session;
  }

  type state = {
    mode : mode;
    chain_id : L2_types.chain_id;
    chain_family : L2_types.ex_chain_family;
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

type 'a prevalidation_result = {next_nonce : quantity; transaction_object : 'a}

module Request = struct
  type (_, _) t =
    | Prevalidate_raw_transaction : {
        raw_transaction : string;
      }
        -> ( (Transaction_object.t prevalidation_result, string) result,
             tztrace )
           t
    | Refresh_state : (unit, tztrace) t
    | Prevalidate_raw_transaction_tezlink : {
        raw_transaction : string;
      }
        -> ( (Tezos_types.Operation.t prevalidation_result, string) result,
             tztrace )
           t

  let name : type a err. (a, err) t -> string = function
    | Prevalidate_raw_transaction _ -> "Prevalidate_raw_transaction"
    | Refresh_state -> "Refresh_state"
    | Prevalidate_raw_transaction_tezlink _ ->
        "Prevalidate_raw_transaction_tezlink"

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
        case
          ~title:"Prevalidate_raw_transaction_tezlink"
          Json_only
          (obj2
             (req "request" (constant "prevalidate_raw_transaction_tezlink"))
             (req "raw_transaction" (string' Hex)))
          (function
            | View (Prevalidate_raw_transaction_tezlink {raw_transaction}) ->
                Some ((), raw_transaction)
            | _ -> None)
          (fun ((), _) ->
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

let validate_chain_id chain_id (transaction : Transaction_object.t) :
    (unit, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  match Transaction_object.chain_id transaction with
  | Ok None -> return (Ok ())
  | Ok (Some (Qty transaction_chain_id)) ->
      if Z.equal transaction_chain_id chain_id then return (Ok ())
      else return (Error "Invalid chain id")
  | Error msg -> Lwt_syntax.return (Error msg)

let validate_nonce ~next_nonce:(Qty next_nonce)
    (transaction : Transaction_object.t) =
  let open Lwt_result_syntax in
  let (Qty nonce) = Transaction_object.nonce transaction in
  if nonce >= next_nonce then return (Ok ()) else return (Error "Nonce too low")

let validate_gas_limit session (transaction : Transaction_object.t) :
    (unit, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  (* Computing the execution gas limit validates that the gas limit is
     sufficient to cover the inclusion fees. *)
  let access_list = Transaction_object.access_list transaction in
  let (Qty gas_limit) = Transaction_object.gas transaction in
  let data =
    Transaction_object.input transaction |> Ethereum_types.encode_hex
  in
  let**? execution_gas_limit =
    (* since Dionysus, the execution gas limit is always computed from the
       minimum base fee per gas *)
    Fees.execution_gas_limit
      ~da_fee_per_byte:(Qty session.etherlink_infos.da_fee_per_bytes)
      ~access_list
      ~minimum_base_fee_per_gas:session.etherlink_infos.minimum_base_fee_per_gas
      ~gas_limit
      data
  in
  if
    Storage_version.gas_limit_validation_enabled
      ~storage_version:session.storage_version
  then
    if
      Compare.Z.(
        execution_gas_limit
        <= session.etherlink_infos.maximum_gas_per_transaction)
    then return (Ok ())
    else
      return
        (Error
           (Format.asprintf
              "Gas limit for execution is too high. Maximum limit is %a, \
               transaction has %a"
              Z.pp_print
              session.etherlink_infos.maximum_gas_per_transaction
              Z.pp_print
              execution_gas_limit))
  else return (Ok ())

let validate_authorizations (type state) ~session ~chain_id ~caller txn =
  let open Lwt_result_syntax in
  let authorization_list = Transaction_object.authorization_list txn in
  if not (Transaction_object.is_eip7702 txn) then return (Ok ())
  else if List.is_empty authorization_list then
    return (Error "Authorization list cannot be empty per EIP-7702.")
  else
    let (module Backend_rpc : Services_backend_sig.S
          with type Reader.state = state) =
      session.state_backend
    in
    let read_nonce address =
      Etherlink_durable_storage.nonce
        (Backend_rpc.Reader.read session.state)
        address
      |> lwt_map_error (fun _ -> "Couldn't retrieve address' nonce")
    in
    let check_auth (item : Transaction_object.authorization_item) =
      let (Qty tx_chain_id) = item.chain_id in
      if not (Z.equal chain_id tx_chain_id) then
        fail "Authorization chain id mismatch"
      else
        let*? signer_address = Transaction_object.authorization_signer item in
        let* current_nonce = read_nonce signer_address in
        let current_nonce =
          match current_nonce with
          | Some (Qty current_nonce) -> current_nonce
          | None -> Z.zero
        in
        let nonce_check =
          (* The authorization list is processed before the execution portion of
             the transaction begins, but after the sender’s nonce is incremented.
             If the sender of the transaction is also the one that signed the
             authorization, the nonce of the signed authorization must be equal
             to its current nonce + 1. *)
          if Address.equal caller signer_address then Z.succ current_nonce
          else current_nonce
        in
        let (Qty nonce) = item.nonce in
        if Z.equal nonce_check nonce then return_unit
        else fail "Authorization nonce mismatch"
    in
    let*! opt_err =
      Lwt_list.map_p check_auth authorization_list
      |> Lwt.map (List.find Result.is_error)
    in
    match opt_err with Some error -> return error | None -> return (Ok ())

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
    (transaction : Transaction_object.t) =
  let open Lwt_result_syntax in
  let (Qty gas_price) = Transaction_object.max_fee_per_gas transaction in
  if gas_price >= base_fee_per_gas then return (Ok ())
  else return (Error "Max gas fee too low")

let validate_balance_is_enough (transaction : Transaction_object.t) ~balance =
  let open Lwt_result_syntax in
  let (Qty gas) = Transaction_object.gas transaction in
  let (Qty gas_price) = Transaction_object.max_fee_per_gas transaction in
  let (Qty value) = Transaction_object.value transaction in
  let gas_cost = Z.mul gas gas_price in
  let total_cost = Z.add gas_cost value in
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

let is_contract_creation calldata to_ authorization_list =
  Bytes.length calldata != 0
  && Option.is_none to_
  && List.is_empty authorization_list

let validate_tx_data_size ~max_number_of_chunks
    (transaction : Transaction_object.t) =
  let open Lwt_result_syntax in
  let tx_data =
    Transaction_object.input transaction |> Ethereum_types.encode_hex
  in
  if tx_data_size_limit_reached ~max_number_of_chunks ~tx_data then
    return @@ Error "Transaction data exceeded the allowed size."
  else return (Ok ())

let initial_tx_gas_computation ~(transaction : Transaction_object.t)
    ~is_prague_enabled =
  let data =
    Transaction_object.input transaction |> Ethereum_types.hex_to_real_bytes
  in
  let zero_bytes, nonzero_bytes =
    Bytes.fold_left
      (fun (zeros, nonzeros) c ->
        if c = '\x00' then (zeros + 1, nonzeros) else (zeros, nonzeros + 1))
      (0, 0)
      data
  in
  let tokens_in_calldata =
    zero_bytes + (nonzero_bytes * K.non_zero_byte_multiplier)
  in
  let base_calldata_cost = tokens_in_calldata * K.standard_token_cost in
  let access_list_accounts, access_list_storages =
    List.fold_left
      (fun (accounts, storages)
           ({address = _; storage_keys} : Transaction_object.access)
         -> (accounts + 1, storages + List.length storage_keys))
      (0, 0)
      (Transaction_object.access_list transaction)
  in
  let access_list_accounts_cost =
    access_list_accounts * K.access_list_address
  in
  let access_list_storages_costs =
    access_list_storages * K.access_list_storage_key
  in
  let authorization_list = Transaction_object.authorization_list transaction in
  let creation_cost =
    if
      is_contract_creation
        data
        (Transaction_object.to_ transaction)
        authorization_list
    then
      let calldata_words =
        let calldata_size = Bytes.length data in
        (* Ensures rounding up when `calldata_size` is not a multiple of `K.word_size`. *)
        (calldata_size + K.word_size - 1) / K.word_size
      in
      K.base_creation_cost + (K.initcode_word_cost * calldata_words)
    else 0
  in
  let prague_init_gas_cost, prague_floor_gas_cost =
    if is_prague_enabled then
      let prague_init_gas_cost =
        List.length authorization_list * K.eip7702_empty_account_cost
      in
      let prague_floor_gas_cost =
        (tokens_in_calldata * K.total_cost_floor_per_token)
        + K.base_intrisic_gas_cost
      in
      (prague_init_gas_cost, prague_floor_gas_cost)
    else (0, 0)
  in
  let initial_gas =
    K.base_intrisic_gas_cost + base_calldata_cost + access_list_accounts_cost
    + access_list_storages_costs + creation_cost + prague_init_gas_cost
  in
  let floor_gas = prague_floor_gas_cost in
  (initial_gas, floor_gas)

(* Validation logic was taken from:
   https://github.com/bluealloy/revm/blob/0ca6564f02004976f533cacf8821fed09d801e0a/crates/handler/src/validation.rs#L221 *)
let validate_minimum_gas_requirement ~session
    ~(transaction : Transaction_object.t) =
  let open Lwt_result_syntax in
  let is_prague_enabled =
    Storage_version.is_prague_enabled ~storage_version:session.storage_version
  in
  let initial_gas, floor_gas =
    initial_tx_gas_computation ~transaction ~is_prague_enabled
  in
  let data =
    Transaction_object.input transaction |> Ethereum_types.encode_hex
  in
  let da_inclusion_fees =
    Fees.da_fees_gas_limit_overhead
      ~da_fee_per_byte:(Qty session.etherlink_infos.da_fee_per_bytes)
      ~minimum_base_fee_per_gas:session.etherlink_infos.minimum_base_fee_per_gas
      data
  in
  let (Qty gas_limit) = Transaction_object.gas transaction in
  let* () =
    let minimum_gas_limit_required =
      Z.(of_int initial_gas + da_inclusion_fees)
    in
    (* Early exit for a transaction with a gas limit that can't cover the minimum
       required. *)
    when_ (gas_limit < minimum_gas_limit_required) @@ fun () ->
    tzfail (Gas_limit_too_low {gas_limit; minimum_gas_limit_required})
  in
  let* () =
    (* Check induced by EIP-7623, see https://eips.ethereum.org/EIPS/eip-7623. *)
    let minimum_gas_limit_required = Z.(of_int floor_gas + da_inclusion_fees) in
    when_ (is_prague_enabled && gas_limit < minimum_gas_limit_required)
    @@ fun () ->
    tzfail (Gas_limit_too_low {gas_limit; minimum_gas_limit_required})
  in
  return (Ok ())

let minimal_validation ~next_nonce ~max_number_of_chunks ctxt
    (transaction : Transaction_object.t) ~caller =
  let open Lwt_result_syntax in
  let (Session session) = ctxt.session in
  let (Chain_id chain_id) = ctxt.chain_id in
  let** () = validate_minimum_gas_requirement ~session ~transaction in
  let** () = validate_chain_id chain_id transaction in
  let** () = validate_nonce ~next_nonce transaction in
  let** () = validate_sender_not_a_contract session caller in
  let** () = validate_authorizations ~session ~chain_id ~caller transaction in
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
      ~base_fee_per_gas:(Qty session.etherlink_infos.base_fee_per_gas)
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

let valid_transaction_object (type state) ctxt session mode txn =
  let open Lwt_result_syntax in
  let (module Backend_rpc : Services_backend_sig.S
        with type Reader.state = state) =
    session.state_backend
  in
  let caller = Transaction_object.sender txn in
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

  return (Ok next_nonce)

module Worker = Octez_telemetry.Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  type launch_error = tztrace

  let on_launch _self ()
      ({chain_id; mode; max_number_of_chunks; chain_family; session} :
        Types.parameters) =
    let open Lwt_result_syntax in
    return
      ({chain_id; mode; max_number_of_chunks; chain_family; session}
        : Types.state)

  let is_tx_valid (type state) ctxt session raw_transaction :
      (Transaction_object.t prevalidation_result, string) result tzresult Lwt.t
      =
    let open Lwt_result_syntax in
    let (module Backend_rpc : Services_backend_sig.S
          with type Reader.state = state) =
      session.state_backend
    in
    let*? transaction_object = Transaction_object.decode raw_transaction in
    let* () =
      when_
        (Transaction_object.is_eip7702 transaction_object
        && not
             (Storage_version.is_prague_enabled
                ~storage_version:session.storage_version))
      @@ fun () -> tzfail Prague_not_enabled
    in
    let** next_nonce =
      valid_transaction_object ctxt session ctxt.mode transaction_object
    in
    return (Ok {next_nonce; transaction_object})

  let refresh_state (type state) ctxt session =
    let open Lwt_result_syntax in
    let (module Backend_rpc : Services_backend_sig.S
          with type Reader.state = state) =
      session.state_backend
    in
    let* state = Backend_rpc.Reader.get_state () in
    let* session =
      Types.session_of_state ctxt.chain_family session.state_backend state
    in
    ctxt.session <- session ;
    return_unit

  let is_tezlink_tx_valid (type state) ~data_model _ctxt session raw_transaction
      :
      (Tezos_types.Operation.t prevalidation_result, string) result tzresult
      Lwt.t =
    let open Lwt_result_syntax in
    (* We build a `read` function from the session. It's the only part of the
       backend we should rely on: the other helpers in the backend rely on the
       internal state, not the state in the session. *)
    let (module Backend_rpc : Services_backend_sig.S
          with type Reader.state = state) =
      session.state_backend
    in
    let read = Backend_rpc.Reader.read session.state in
    let** op =
      Tezlink_prevalidation.parse_and_validate_for_queue
        ~read
        ~data_model
        raw_transaction
    in
    return (Ok {next_nonce = Qty op.first_counter; transaction_object = op})

  let on_request : type r err.
      self -> (r, err) Request.t -> (r, err) result Lwt.t =
   fun self request ->
    let ctxt = Worker.state self in
    let (Session session) = ctxt.session in
    match request with
    | Prevalidate_raw_transaction {raw_transaction} ->
        is_tx_valid ctxt session raw_transaction
    | Refresh_state -> refresh_state ctxt session
    | Prevalidate_raw_transaction_tezlink {raw_transaction} ->
        let data_model =
          match ctxt.chain_family with
          | Ex_chain_family Michelson -> Tezlink_durable_storage.Path
          | Ex_chain_family EVM -> Tezlink_durable_storage.Rlp
        in
        is_tezlink_tx_valid ctxt session raw_transaction ~data_model

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
    let (module Backend_rpc : Services_backend_sig.S
          with type Reader.state = state) =
      state_backend
    in
    let* state = Backend_rpc.Reader.get_state () in
    let* chain_id = Durable_storage.chain_id (Backend_rpc.Reader.read state) in
    let* session =
      Types.session_of_state (Ex_chain_family chain_family) state_backend state
    in
    let* w =
      Worker.launch
        table
        ()
        {
          chain_id;
          mode;
          max_number_of_chunks;
          chain_family = Ex_chain_family chain_family;
          session;
        }
        (module Handlers)
    in
    worker := Started w ;
    Lwt.wakeup starting_waker () ;
    let*! () = Prevalidator_events.is_ready () in
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

let prevalidate_raw_transaction_tezlink raw_transaction =
  worker_wait_for_request
    (Request.Prevalidate_raw_transaction_tezlink {raw_transaction})

let validate_balance_gas_nonce_with_validation_state validation_state
    (transaction : Transaction_object.t) :
    (validation_state, string) result tzresult Lwt.t =
  Octez_telemetry.Trace.with_tzresult
    ~attrs:
      Telemetry.Attributes.
        [Transaction.hash (Transaction_object.hash transaction)]
    ~service_name:"evm_node_worker.prevalidator"
    "validate_balance_gas_nonce_with_validation_state"
  @@ fun _ ->
  let open Lwt_result_syntax in
  let caller = Transaction_object.sender transaction in
  let (Address (Hex caller_str)) = caller in
  let* next_nonce =
    let nonce = String.Map.find caller_str validation_state.addr_nonce in
    match nonce with
    | Some nonce -> return nonce
    | None -> (
        let* nonce = validation_state.evm_config.next_nonce caller in
        match nonce with
        | Some (Qty nonce) -> return nonce
        | None -> return Z.zero)
  in
  let** () =
    let (Qty tx_nonce) = Transaction_object.nonce transaction in
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
        let* (Qty balance) = validation_state.evm_config.balance caller in
        return balance
  in
  let** total_cost =
    validate_balance_and_max_fee_per_gas
      ~base_fee_per_gas:validation_state.evm_config.base_fee_per_gas
      ~transaction
      ~from_balance:(Qty from_balance)
  in
  let* addr_balance =
    match Transaction_object.to_ transaction with
    | None -> return validation_state.addr_balance
    | Some to_ ->
        let (Address (Hex to_)) = to_ in
        let to_balance = String.Map.find to_ validation_state.addr_balance in
        let* to_balance =
          match to_balance with
          | Some balance -> return balance
          | None ->
              let* (Qty balance) =
                validation_state.evm_config.balance (Address (Hex to_))
              in
              return balance
        in
        let (Qty value) = Transaction_object.value transaction in
        let new_balance = Z.add value to_balance in
        let addr_balance =
          String.Map.add to_ new_balance validation_state.addr_balance
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
