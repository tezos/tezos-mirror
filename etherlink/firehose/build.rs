// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Generate the bindings to the ERC20 contract from 'solidity by example'. The compiled artifacts
//! from this contract are defined in the EVM_KERNEL_INPUTS folder.
//!
//! We consume both the abi, and bytecode files - and generate a rust file for interacting with it.

use anyhow::{anyhow, Result};
use ethers::contract::Abigen;

const ABI: &str = include_str!("../tezt/tests/evm_kernel_inputs/erc20tok.abi");
const BIN: &str = include_str!("../tezt/tests/evm_kernel_inputs/erc20tok.bin");

const CONTRACT_DIR: &str = "src/contracts";

fn main() -> Result<()> {
    // Create the contract dir
    std::fs::create_dir_all(CONTRACT_DIR)?;

    // Generate bindings to the erc20 contract
    let erc20_source = format!(r#"{{"abi": {ABI}, "bin": "{BIN}"}}"#);

    Abigen::new("ERC20", erc20_source)
        .map_err(|e| anyhow!("{e}"))?
        .generate()
        .map_err(|e| anyhow!("{e}"))?
        .write_module_in_dir(CONTRACT_DIR)
        .map_err(|e| anyhow!("{e}"))?;

    Ok(())
}
