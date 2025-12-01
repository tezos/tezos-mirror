(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Internal_event.Simple

let pp_eth fmt balance =
  let one_billion = Z.of_int 1_000_000_000 in
  let eth = Z.(balance / (one_billion * one_billion)) in
  let gwei =
    Z.(balance mod (one_billion * one_billion) / one_billion) |> Z.to_int
  in
  Format.fprintf fmt "%a.%09d" Z.pp_print eth gwei

let section = ["floodgate"]

let is_ready =
  declare_2
    ~alternative_color:Blue
    ~section
    ~name:"is_ready"
    ~msg:
      "Floodgate is ready to spam network {chain_id} (base_fee_per_gas: \
       {base_fee_per_gas})"
    ~level:Notice
    ~pp2:pp_eth
    ("chain_id", L2_types.Chain_id.encoding)
    ("base_fee_per_gas", Data_encoding.n)

let tx_queue_is_ready =
  declare_0
    ~alternative_color:Blue
    ~section
    ~name:"tx_pool_is_ready"
    ~level:Notice
    ~msg:"Tx queue is ready"
    ()

let mainnet_experiment =
  declare_0
    ~section
    ~name:"mainnet_experiment"
    ~msg:"You are about to start a mainnet experiment"
    ~level:Warning
    ()

let received_blueprint =
  declare_1
    ~section
    ~name:"received_blueprint"
    ~msg:"Received blueprint {number}"
    ~level:Info
    ("number", Data_encoding.n)

let spam_started =
  declare_1
    ~section
    ~name:"spam_started"
    ~msg:"Spamming with account {address} has started"
    ~level:Notice
    ("address", Ethereum_types.address_encoding)

let injecting_transactions =
  declare_1
    ~name:"injecting_transaction"
    ~msg:"Injecting {n} transactions"
    ~level:Info
    ("n", Data_encoding.int31)

let transaction_confirmed =
  declare_2
    ~section
    ~name:"confirmation_confirmed"
    ~msg:"Transaction from {address} confirmed in {duration}"
    ~level:Info
    ~pp2:Ptime.Span.pp
    ("address", Ethereum_types.address_encoding)
    ("duration", Time.System.Span.encoding)

let transaction_refused =
  declare_1
    ~section
    ~name:"confirmation_refused"
    ~msg:"Transaction from {address} refused"
    ~level:Warning
    ("address", Ethereum_types.address_encoding)

let transaction_dropped =
  declare_1
    ~section
    ~name:"confirmation_dropped"
    ~msg:"Transaction from {address} dropped"
    ~level:Warning
    ("address", Ethereum_types.address_encoding)

let transaction_retried_confirmed =
  declare_3
    ~section
    ~name:"transaction_retried_confirmed"
    ~msg:
      "Transaction from {address} was retried {attempt} times before being \
       confirmed in {time}."
    ~level:Warning
    ("address", Ethereum_types.address_encoding)
    ("attempt", Data_encoding.int31)
    ("time", Time.System.Span.encoding)

let transaction_retried_failed =
  declare_2
    ~section
    ~name:"transaction_retried_failed"
    ~msg:"Transaction from {address} failed after {attempt} attempts."
    ~level:Warning
    ("address", Ethereum_types.address_encoding)
    ("attempt", Data_encoding.int31)

let setup_completed =
  declare_0
    ~alternative_color:Magenta
    ~section
    ~name:"setup_completed"
    ~level:Notice
    ~msg:"Stress-test ramp-up completed"
    ()

let reimbursed_controller =
  declare_1
    ~section
    ~name:"reimbursed_controller"
    ~msg:"{account} reimbursed its controller"
    ~level:Notice
    ("account", Ethereum_types.address_encoding)

let deploy_erc20 =
  declare_1
    ~section
    ~name:"deploy_erc20"
    ~msg:"ERC20 contract as been deployed at address {address}"
    ~level:Notice
    ("address", Data_encoding.string)

let measured_tps =
  declare_2
    ~alternative_color:Green
    ~section
    ~name:"measured_tps"
    ~msg:"measured {tps} transactions per second over the past {duration}"
    ~level:Notice
    ~pp2:Ptime.Span.pp
    ("tps", Data_encoding.float)
    ("duration", Time.System.Span.encoding)

let measured_dps =
  declare_2
    ~alternative_color:Green
    ~section
    ~name:"measured_dps"
    ~msg:"measured {dps}KB dummy data per second over the past {duration}"
    ~level:Notice
    ~pp2:Ptime.Span.pp
    ("dps", Data_encoding.float)
    ("duration", Time.System.Span.encoding)

let rpc_error =
  declare_2
    ~section
    ~name:"rpc_error"
    ~msg:"An RPC produced the error :\n code:{code},\n message:{message}"
    ~level:Error
    ("code", Data_encoding.int32)
    ("message", Data_encoding.string)

let mainnet_experiment () = emit mainnet_experiment ()

let is_ready chain_id base_fee_per_gas =
  emit is_ready (chain_id, base_fee_per_gas)

let tx_queue_is_ready () = emit tx_queue_is_ready ()

let received_blueprint (Ethereum_types.Qty i) = emit received_blueprint i

let spam_started address = emit spam_started address

let setup_completed () = emit setup_completed ()

let injecting_transactions n = emit injecting_transactions n

let transaction_confirmed account duration =
  emit transaction_confirmed (Account.address_et account, duration)

let transaction_refused account =
  emit transaction_refused (Account.address_et account)

let transaction_retried_confirmed account attempt times =
  emit transaction_retried_confirmed (Account.address_et account, attempt, times)

let transaction_retried_failed account attempt =
  emit transaction_retried_failed (Account.address_et account, attempt)

let transaction_dropped account =
  emit transaction_dropped (Account.address_et account)

let reimbursed_controller account =
  emit reimbursed_controller (Account.address_et account)

let deploy_erc20 address = emit deploy_erc20 address

let rpc_error (error : Rpc_encodings.JSONRPC.error) =
  emit rpc_error (Int32.of_int error.code, error.message)

let measured_tps transactions_count duration =
  let tps = Float.of_int transactions_count /. Ptime.Span.to_float_s duration in
  emit measured_tps (tps, duration)

let measured_dps dummy_data_size_kb duration =
  let tps = Float.of_int dummy_data_size_kb /. Ptime.Span.to_float_s duration in
  emit measured_dps (tps, duration)
