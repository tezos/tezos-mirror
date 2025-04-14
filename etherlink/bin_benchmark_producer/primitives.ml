(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let evm_version = Evm_version.Cancun

let general_contracts = "etherlink/bin_benchmark_producer/general_contracts/"

let addr1 = Eth_account.bootstrap_accounts.(1).address

let addr2 = Eth_account.bootstrap_accounts.(2).address

let registered_general_contracts =
  [
    ( Solidity_contracts.compile_contract
        ~source:(general_contracts ^ "erc20tok.sol")
        ~label:"erc20tok"
        ~contract:"ERC20"
        evm_version,
      [
        ("transfer(address,uint256)", [addr1; "1000"]);
        ("approve(address,uint256)", [addr1; "500"]);
        ("transferFrom(address,address,uint256)", [addr1; addr2; "250"]);
        ("mint(uint256)", ["10000"]);
        ("burn(uint256)", ["1000"]);
      ] );
  ]
