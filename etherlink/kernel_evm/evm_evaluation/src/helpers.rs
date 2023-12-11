// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::path::{Path, PathBuf};

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
            extern crate alloc;
            writeln!(
                $host.buffer.borrow_mut(),
                "{}",
                { &alloc::format!($($args), *) },
            ).unwrap()
        }
    };
}
