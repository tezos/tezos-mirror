/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Serialization to and deserialization from bytes. Used for `PACK` and
//! `UNPACK` instructions respectively, but can be used for general-purpose
//! Michelson data serialization as well.
//!
//! Functions are defined as associated functions on [crate::ast::Micheline],
//! see it for more.

mod constants;
mod decode;
mod encode;
mod integration_tests;

pub use {decode::*, encode::*};
