/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Tests that cover several components, including Micheline encoding.

/// Covers key points of typed values serialization where ambiguity is possible
/// (e.g. optimized vs readable representations).
/// Full coverage of PACK we expect from the TZT tests.
#[cfg(test)]
mod test_typed_encode {
    use typed_arena::Arena;

    use crate::ast::{byte_repr_trait::*, IntoMicheline, KeyHash, Micheline};
    use crate::ast::{Address, TypedValue};

    // Expected bytes to be produced with
    // octez-client --mode mockup run script "parameter unit; storage bytes; code { DROP; PUSH $ty $val; PACK; NIL operation; PAIR }" on storage 0x and input Unit
    //
    // Or in 19.0-rc1 version of octez-client simply:
    // octez-client --mode mockup run michelson code PACK on stack "{Stack_elt $ty $val}"

    #[track_caller]
    fn check(v: TypedValue, hex_bytes: &str) {
        let hex_bytes: &str = hex_bytes
            .strip_prefix("0x")
            .expect("The `expected` argument must start from 0x");

        let bytes = &hex::decode(hex_bytes).expect("Bad hex string in `expected` argument");

        let arena = Arena::new();
        let micheline = v.into_micheline_optimized_legacy(&arena);
        assert_eq!(&micheline.encode_for_pack(), bytes);
        assert_eq!(Micheline::decode_packed(&arena, bytes), Ok(micheline),);
    }

    #[test]
    fn test_address() {
        check(
            TypedValue::Address(
                Address::from_base58_check("tz1NyAf1KeeFCCPPAZ9ard9YVshVGFibzVKa").unwrap(),
            ),
            "0x050a0000001600002486eda3c7bbbe6be511b46d6deeb1594258a7fd",
        )
    }

    #[test]
    fn test_key_hash() {
        check(
            TypedValue::KeyHash(
                KeyHash::from_base58_check("tz1NyAf1KeeFCCPPAZ9ard9YVshVGFibzVKa").unwrap(),
            ),
            "0x050a00000015002486eda3c7bbbe6be511b46d6deeb1594258a7fd",
        )
    }

    #[test]
    fn test_comb_pair() {
        // Should be encoded as a tree
        check(
            TypedValue::new_pair(
                TypedValue::nat(1),
                TypedValue::new_pair(TypedValue::nat(2), TypedValue::nat(3)),
            ),
            "0x0507070001070700020003",
        )
    }
}
