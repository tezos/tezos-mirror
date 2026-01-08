// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

#[derive(Debug)]
pub enum Protocol {
    S023,
}

#[cfg(test)]
pub const TARGET_TEZOS_PROTOCOL: Protocol = Protocol::S023;
