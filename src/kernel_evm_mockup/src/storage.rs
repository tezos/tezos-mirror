// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT
#![allow(dead_code)]

use host::path::*;
use host::rollup_core::RawRollupCore;
use host::runtime::{load_value_slice, Runtime};

use std::str::from_utf8;

use crate::account::*;
use crate::error::Error;
use crate::wei::Wei;

use primitive_types::U256;

const EVM_ACCOUNTS: RefPath = RefPath::assert_from(b"/eth_accounts");

const EVM_ACCOUNT_BALANCE: RefPath = RefPath::assert_from(b"/balance");
const EVM_ACCOUNT_NONCE: RefPath = RefPath::assert_from(b"/nonce");
const EVM_ACCOUNT_CODE_HASH: RefPath = RefPath::assert_from(b"/code_hash");

const CODE_HASH_SIZE: usize = 32;

/// The size of one 256 bit word. Size in bytes
pub const WORD_SIZE: usize = 32usize;

/// Read a single unsigned 256 bit value from storage at the path given.
fn read_u256(host: &impl Runtime, path: &OwnedPath) -> U256 {
    let bytes = host.store_read(path, 0, WORD_SIZE).unwrap();
    Wei::from_little_endian(&bytes)
}

fn write_u256(host: &mut impl Runtime, path: &OwnedPath, value: U256) -> Result<(), Error> {
    let mut bytes: [u8; WORD_SIZE] = value.into();
    value.to_little_endian(&mut bytes);
    host.store_write(path, &bytes, 0).map_err(Error::from)
}

pub fn address_path(address: Hash) -> Result<OwnedPath, Error> {
    let address: &str = from_utf8(address)?;
    let address_path: Vec<u8> = format!("/{}", &address).into();
    OwnedPath::try_from(address_path).map_err(Error::from)
}

pub fn account_path(address: Hash) -> Result<OwnedPath, Error> {
    let address_hash = address_path(address)?;
    concat(&EVM_ACCOUNTS, &address_hash).map_err(Error::from)
}

pub fn read_account_nonce<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
) -> Result<u64, Error> {
    let path = concat(account_path, &EVM_ACCOUNT_NONCE)?;
    let mut buffer = [0_u8; 8];

    match load_value_slice(host, &path, &mut buffer) {
        Ok(8) => Ok(u64::from_le_bytes(buffer)),
        _ => Err(Error::Generic),
    }
}

pub fn read_account_balance<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
) -> Result<Wei, Error> {
    let path = concat(account_path, &EVM_ACCOUNT_BALANCE)?;
    Ok(read_u256(host, &path))
}

pub fn read_account_code_hash<Host: Runtime>(
    host: &mut Host,
    account_path: &OwnedPath,
) -> Result<Vec<u8>, Error> {
    let path = concat(account_path, &EVM_ACCOUNT_CODE_HASH)?;
    host.store_read(&path, 0, CODE_HASH_SIZE)
        .map_err(Error::from)
}

pub fn read_account<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    address: Hash,
) -> Result<Account, Error> {
    let account_path = account_path(address)?;
    let nonce = read_account_nonce(host, &account_path)?;
    let balance = read_account_balance(host, &account_path)?;
    let code_hash = read_account_code_hash(host, &account_path)?;

    Ok(Account {
        hash: address.to_vec(),
        nonce,
        balance,
        code_hash,
    })
}

pub fn store_nonce<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
    nonce: u64,
) -> Result<(), Error> {
    let path = concat(account_path, &EVM_ACCOUNT_NONCE)?;
    host.store_write(&path, &nonce.to_le_bytes(), 0)
        .map_err(Error::from)
}

pub fn store_balance<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
    balance: Wei,
) -> Result<(), Error> {
    let path = concat(account_path, &EVM_ACCOUNT_BALANCE)?;
    write_u256(host, &path, balance)
}

pub fn store_code_hash<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
    code_hash: Hash,
) -> Result<(), Error> {
    let path = concat(account_path, &EVM_ACCOUNT_CODE_HASH)?;
    host.store_write(&path, code_hash, 0).map_err(Error::from)
}

pub fn store_account<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account: Account,
) -> Result<(), Error> {
    let account_path = account_path(&account.hash)?;
    store_nonce(host, &account_path, account.nonce)?;
    store_balance(host, &account_path, account.balance)?;
    store_code_hash(host, &account_path, &account.code_hash)
}
