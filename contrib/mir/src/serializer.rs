// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

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

pub use decode::*;

use crate::ast::Micheline;
use typed_arena::Arena;

/// Return the byte size of one canonical Micheline expression starting at
/// `input[0]`.  Walks the binary structure without allocating an output value.
pub fn micheline_expr_size(input: &[u8]) -> Result<usize, DecodeError> {
    let arena = Arena::new();
    let (_, consumed) = Micheline::decode_raw_prefix(&arena, input)?;
    Ok(consumed)
}

/// Wrapper around raw Micheline bytes that uses self-delimiting encoding
/// (matching OCaml's `Script.expr_encoding`).  Unlike a plain `Vec<u8>`,
/// this reads/writes exactly one Micheline expression without a length
/// prefix, relying on the Micheline structure for boundary detection.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct MichelineExpr(pub Vec<u8>);

impl tezos_data_encoding::enc::BinWriter for MichelineExpr {
    fn bin_write(&self, output: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        tezos_data_encoding::enc::bytes(&self.0, output)
    }
}

impl<'a> tezos_data_encoding::nom::NomReader<'a> for MichelineExpr {
    fn nom_read(input: &'a [u8]) -> tezos_data_encoding::nom::NomResult<'a, Self> {
        let consumed = micheline_expr_size(input).map_err(|e| {
            nom::Err::Error(tezos_data_encoding::nom::error::DecodeError::invalid_tag(
                input,
                format!("invalid Micheline expression: {e}"),
            ))
        })?;
        Ok((
            &input[consumed..],
            MichelineExpr(input[..consumed].to_vec()),
        ))
    }
}

impl From<Vec<u8>> for MichelineExpr {
    fn from(v: Vec<u8>) -> Self {
        MichelineExpr(v)
    }
}

#[cfg(test)]
mod tests {
    use super::constants::APP_NO_ARGS_NO_ANNOTS_TAG;
    use crate::ast::Micheline;
    use crate::lexer::Prim;
    use regex::Regex;
    use std::fs::read_to_string;
    use std::str::FromStr;
    use typed_arena::Arena;

    fn read_file_to_string(path: &str) -> String {
        read_to_string(path)
            .unwrap_or_else(|err| panic!("Opening file {path} should have succeeded: {err}"))
    }

    #[test]
    fn test_primitives_serialization_compatibility() {
        let alpha_primitive_list = read_file_to_string(&format!(
            "{}/../../tezt/tests/expected/encoding.ml/Alpha- Michelson primitives regression.out",
            env!("CARGO_MANIFEST_DIR"),
        ));

        let re = Regex::new(r"([0-9]+): ([a-zA-Z0-9_]+)").unwrap();

        for (_, [tag, prim]) in re.captures_iter(&alpha_primitive_list).map(|c| c.extract()) {
            let tag: u8 = tag
                .parse()
                .unwrap_or_else(|err| panic!("tag {tag} should be decodable as u8: {err}"));
            let prim: Prim = Prim::from_str(prim).unwrap_or_else(|err| {
                panic!("string {prim} should be convertible to a primitive: {err}")
            });
            let arena = Arena::new();
            // We don't have access to a function deserializing just a
            // primitive so we use the simplest possible Micheline
            // case involving primitives: application of a primitive
            // to no argument and no annotation
            let serialized = [APP_NO_ARGS_NO_ANNOTS_TAG, tag];
            let micheline = Micheline::decode_raw(&arena, &serialized)
                .unwrap_or_else(|err| panic!("tag {tag} should be decodable as primitive: {err}"));
            assert_eq!(
                micheline,
                Micheline::prim0(prim),
                "tag {tag} is associated to primitive {prim} but we deserialized it as {micheline:?}"
            );
            // While we are at it, we also check that serializing the
            // primitives gives back the expected tag.
            assert_eq!(micheline.encode().unwrap(), serialized,)
        }
    }
}
