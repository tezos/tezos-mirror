// SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::path::{Path, PathBuf};

pub struct OutputOptions {
    pub log: bool,
    pub summary: bool,
    pub result: bool,
    pub diff: bool,
}

pub struct LabelIndexes<'a> {
    pub data_label: Option<&'a String>,
    pub gas_label: Option<&'a String>,
    pub value_label: Option<&'a String>,
}

pub fn string_of_hexa(bytes: &bytes::Bytes) -> String {
    let mut hexa = String::from("0x");
    for byte in bytes.into_iter() {
        hexa.push_str(format!("{:02x?}", byte).as_str());
    }
    hexa
}

pub fn parse_and_get_cmp(data: &str) -> impl Fn(&u8, &u8) -> bool {
    if data.contains('>') {
        u8::gt
    } else if data.contains(">=") {
        u8::ge
    } else if data.contains('<') {
        u8::lt
    } else if data.contains("<=") {
        u8::le
    } else {
        // By default, if there is not cmp prefix then it's supposed
        // to be (strictly) the data itself.
        u8::eq
    }
}

pub fn purify_network(network: &str) -> String {
    let network = network.replace('<', "");
    let network = network.replace('>', "");
    network.replace('=', "")
}

pub fn construct_folder_path(
    base: &str,
    eth_tests: &str,
    sub_dir: &Option<String>,
) -> PathBuf {
    let eth_tests_path = Path::new(eth_tests);
    let base_path = Path::new(base);
    let path_buf = match sub_dir {
        Some(sub_dir) => {
            let sub_dir_path = Path::new(sub_dir);
            eth_tests_path.join(base_path.join(sub_dir_path))
        }
        None => eth_tests_path.join(base_path),
    };

    path_buf
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
