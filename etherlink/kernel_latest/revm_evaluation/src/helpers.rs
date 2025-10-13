// SPDX-FileCopyrightText: 2023-2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::cell::RefCell;

use revm::primitives::{alloy_primitives::Keccak256, B256};

use crate::evalhost::EvalHost;

pub fn bytes_hash(bytes: &[u8]) -> B256 {
    let mut keccak = Keccak256::new();
    keccak.update(bytes);
    keccak.finalize()
}

pub fn u256_to_u128(value: primitive_types::U256) -> u128 {
    if value <= primitive_types::U256::from(u128::MAX) {
        value.low_u128()
    } else {
        u128::MAX
    }
}

pub fn extract_brackets(string: &str) -> &str {
    let start = string.find('[').unwrap();
    let end = string.find(']').unwrap();
    &string[start + 1..end]
}

pub fn prepare_host() -> EvalHost {
    let execution_buffer = Vec::new();
    let buffer = RefCell::new(execution_buffer);
    EvalHost::default_with_buffer(buffer)
}

pub fn prepare_host_with_buffer(execution_buffer: Vec<u8>) -> EvalHost {
    let buffer = RefCell::new(execution_buffer);
    EvalHost::default_with_buffer(buffer)
}

#[macro_export]
macro_rules! write_host {
    ($host: expr, $($args: expr),*) => {
        {
            if cfg!(not(feature = "disable-file-logs")) {
                extern crate alloc;
                writeln!(
                    $host.buffer.borrow_mut(),
                    "{}",
                    { &alloc::format!($($args), *) },
                ).unwrap()
            }
        }
    };
}

#[macro_export]
macro_rules! write_out {
    ($output_file: expr, $($args: expr),*) => {
        {
            if cfg!(not(feature = "disable-file-logs")) {
                extern crate alloc;
                if let Some(ref mut output) = $output_file {
                    use std::io::Write;
                    writeln!(
                        output,
                        "{}",
                        { &alloc::format!($($args), *) },
                    ).unwrap()
                } else {
                    println!(
                        "{}",
                        { &alloc::format!($($args), *) }
                    )
                }
            }
        }
    };
}

pub mod pretty {
    use revm::primitives::{B256, KECCAK_EMPTY};

    pub fn code_hash(code_hash: B256) -> String {
        if code_hash == KECCAK_EMPTY {
            "KECCAK EMPTY".to_string()
        } else {
            code_hash.to_string()
        }
    }
}
