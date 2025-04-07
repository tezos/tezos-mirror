(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Parameters = struct
  type persistent_state = unit

  type session_state = unit

  let base_default_name = "floodgate"

  let default_colors = Log.Color.[|FG.gray; FG.magenta; FG.yellow; FG.green|]
end

include Daemon.Make (Parameters)

let run ?runner ?(path = "./floodgate") ?(scenario = `XTZ) ~rpc_endpoint
    ~controller ?(relay_endpoint = rpc_endpoint) ?max_active_eoa
    ?max_transaction_batch_length ?spawn_interval ?tick_interval
    ?base_fee_factor ?initial_balance () =
  let daemon = create ~path ?runner () in
  let optional_arg ~to_string cli_arg = function
    | Some v -> [cli_arg; to_string v]
    | None -> []
  in
  let scenario = match scenario with `XTZ -> "xtz" | `ERC20 -> "erc20" in
  run ?runner daemon ()
  @@ [
       "run";
       "--verbose";
       "--relay-endpoint";
       relay_endpoint;
       "--rpc-endpoint";
       rpc_endpoint;
       "--controller";
       controller.Eth_account.private_key;
       "--scenario";
       scenario;
     ]
  @ optional_arg ~to_string:string_of_int "--max-active-eoa" max_active_eoa
  @ optional_arg
      ~to_string:string_of_int
      "--max-transaction-batch-length"
      max_transaction_batch_length
  @ optional_arg ~to_string:string_of_float "--spawn_interval" spawn_interval
  @ optional_arg ~to_string:string_of_float "--tick-interval" tick_interval
  @ optional_arg ~to_string:string_of_float "--base-fee-factor" base_fee_factor
  @ optional_arg
      ~to_string:Wei.to_eth_string
      "--initial-balance"
      initial_balance
