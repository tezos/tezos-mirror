// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
// SPDX-FileCopyrightText: [2022-2023] TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

/// Prefix denoting an encoded number.
pub const NUMBER_TAG: u8 = 0x00;
/// Prefix denoting an encoded string.
pub const STRING_TAG: u8 = 0x01;
/// Prefix denoting an encoded sequence.
pub const SEQ_TAG: u8 = 0x02;
/// Prefix denoting an encoded bytes sequence.
pub const BYTES_TAG: u8 = 0x0a;

// Tags for [Michelson::App].
pub const APP_NO_ARGS_NO_ANNOTS_TAG: u8 = 0x03;
pub const APP_NO_ARGS_WITH_ANNOTS_TAG: u8 = 0x04;
pub const APP_ONE_ARG_NO_ANNOTS_TAG: u8 = 0x05;
pub const APP_ONE_ARG_WITH_ANNOTS_TAG: u8 = 0x06;
pub const APP_TWO_ARGS_NO_ANNOTS_TAG: u8 = 0x07;
pub const APP_TWO_ARGS_WITH_ANNOTS_TAG: u8 = 0x08;
pub const APP_GENERIC: u8 = 0x09;
