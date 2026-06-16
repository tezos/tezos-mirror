// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

use super::TypedValue;

// `TypedValue` is partially ordered: two `TypedValue`s are only
// comparable when they have the same type and this common type is
// comparable. Unfortunately, we need `TypedValue` to implement `Ord`
// to build use them as elements in sets and keys in maps. Therefore
// we have an implementation of `Ord` which can panic and a
// non-canonical (in the sense that it can return `None`, despite
// `Ord` being implemented) implementation of `PartialOrd`.

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for TypedValue<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use TypedValue::*;
        match (self, other) {
            (Int(a), Int(b)) => a.partial_cmp(b),
            (Int(..), _) => None,

            (Nat(a), Nat(b)) => a.partial_cmp(b),
            (Nat(..), _) => None,

            (Mutez(a), Mutez(b)) => a.partial_cmp(b),
            (Mutez(..), _) => None,

            (Bool(a), Bool(b)) => a.partial_cmp(b),
            (Bool(..), _) => None,

            (String(a), String(b)) => a.partial_cmp(b),
            (String(..), _) => None,

            (Unit, Unit) => Some(std::cmp::Ordering::Equal),
            (Unit, _) => None,

            (Pair(l1, l2), Pair(r1, r2)) => (l1, l2).partial_cmp(&(r1, r2)),
            (Pair(..), _) => None,

            (Option(x), Option(y)) => x.as_deref().partial_cmp(&y.as_deref()),
            (Option(..), _) => None,

            (Or(x), Or(y)) => x.partial_cmp(y),
            (Or(..), _) => None,

            (Address(l), Address(r)) => l.partial_cmp(r),
            (Address(..), _) => None,

            (ChainId(l), ChainId(r)) => l.partial_cmp(r),
            (ChainId(..), _) => None,

            (Bytes(l), Bytes(r)) => l.partial_cmp(r),
            (Bytes(..), _) => None,

            (Key(l), Key(r)) => l.partial_cmp(r),
            (Key(..), _) => None,

            (Signature(l), Signature(r)) => l.partial_cmp(r),
            (Signature(..), _) => None,

            (KeyHash(l), KeyHash(r)) => l.partial_cmp(r),
            (KeyHash(..), _) => None,

            (Timestamp(a), Timestamp(b)) => a.partial_cmp(b),
            (Timestamp(..), _) => None,

            // non-comparable types
            (
                List(..) | Set(..) | Map(..) | BigMap(..) | Contract(..) | Operation(_)
                | Ticket(..) | Lambda(..),
                _,
            ) => None,
            #[cfg(feature = "bls")]
            (Bls12381Fr(..) | Bls12381G1(..) | Bls12381G2(..), _) => None,
        }
    }
}

impl Ord for TypedValue<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other)
            .expect("Comparing incomparable values in TypedValue")
    }
}

#[cfg(test)]
mod tests {
    use tezos_crypto_rs::hash::HashTrait;

    use crate::ast::ByteReprTrait;

    use super::*;

    #[test]
    fn compare() {
        use TypedValue as V;
        use TypedValue::*;
        macro_rules! assert_cmp {
            ($c:expr; $($l:expr),*; $($r:expr),*; $ord:ident) => {
                assert!($c($($l),*).partial_cmp(&$c($($r),*)) == Some(std::cmp::Ordering::$ord));
                assert!($c($($l),*).cmp(&$c($($r),*)) == std::cmp::Ordering::$ord);
            };
        }

        assert_cmp!(V::int; -1; 0; Less);
        assert_cmp!(V::int; -1; -1; Equal);
        assert_cmp!(V::int; -1; -2; Greater);

        assert_cmp!(V::nat; 3; 4; Less);
        assert_cmp!(V::nat; 4; 4; Equal);
        assert_cmp!(V::nat; 5; 4; Greater);

        assert_cmp!(Mutez; 3; 4; Less);
        assert_cmp!(Mutez; 3; 3; Equal);
        assert_cmp!(Mutez; 32; 4; Greater);

        assert_cmp!(Bool; false; false; Equal);
        assert_cmp!(Bool; false; true; Less);
        assert_cmp!(Bool; true; false; Greater);
        assert_cmp!(Bool; true; true; Equal);

        assert_cmp!(String; "hello".to_owned(); "iello".to_owned(); Less);
        assert_cmp!(String; "a".to_owned(); "bfoo".to_owned(); Less);
        assert_cmp!(String; "afoo".to_owned(); "b".to_owned(); Less);
        assert_cmp!(String; "foo".to_owned(); "foo".to_owned(); Equal);
        assert_cmp!(String; "foo".to_owned(); "bar".to_owned(); Greater);

        assert_cmp!(V::new_option; None; None; Equal);
        assert_cmp!(V::new_option; None; Some(V::int(3)); Less);
        assert_cmp!(V::new_option; Some(V::int(3)); None; Greater);
        assert_cmp!(V::new_option; Some(V::int(3)); Some(V::int(4)); Less);
        assert_cmp!(V::new_option; Some(V::int(4)); Some(V::int(4)); Equal);
        assert_cmp!(V::new_option; Some(V::int(4)); Some(V::int(3)); Greater);

        assert_cmp!(V::new_pair; V::int(3), V::nat(4); V::int(3), V::nat(5); Less);
        assert_cmp!(V::new_pair; V::int(3), V::nat(4); V::int(4), V::nat(4); Less);
        assert_cmp!(V::new_pair; V::int(3), V::nat(4); V::int(3), V::nat(4); Equal);
        assert_cmp!(V::new_pair; V::int(4), V::nat(4); V::int(3), V::nat(4); Greater);
        assert_cmp!(V::new_pair; V::int(3), V::nat(5); V::int(3), V::nat(4); Greater);

        use crate::ast::Or;

        assert_cmp!(V::new_or; Or::Left(V::int(3)); Or::Left(V::int(4)); Less);
        assert_cmp!(V::new_or; Or::Left(V::int(5)); Or::Left(V::int(4)); Greater);
        assert_cmp!(V::new_or; Or::Left(V::int(4)); Or::Left(V::int(4)); Equal);
        assert_cmp!(V::new_or; Or::Right(V::int(3)); Or::Right(V::int(4)); Less);
        assert_cmp!(V::new_or; Or::Right(V::int(5)); Or::Right(V::int(4)); Greater);
        assert_cmp!(V::new_or; Or::Right(V::int(4)); Or::Right(V::int(4)); Equal);
        assert_cmp!(V::new_or; Or::Left(V::int(5)); Or::Right(V::int(3)); Less);
        assert_cmp!(V::new_or; Or::Right(V::int(3)); Or::Left(V::int(5)); Greater);

        // different types don't compare
        assert_eq!(Bool(true).partial_cmp(&V::int(5)), None);
    }

    #[test]
    fn compare_addrs() {
        // ordering was verified against octez-client, see script below
        let ordered_addrs = [
            "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw",
            "tz1SNL5w4RFRbCWRMB4yDWvoRQrPQxZmNzeQ",
            "tz1V8fDHpHzN8RrZqiYCHaJM9EocsYZch5Cy",
            "tz1WPGZjP9eHGqD9DkiRJ1xGRU1wEMY19AAF",
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%bar",
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%defauls",
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j",
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%defaulu",
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%foo",
            "tz1hHGTh6Yk4k7d2PiTcBUeMvw6fJCFikedv",
            "tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH%bar",
            "tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH",
            "tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH%foo",
            "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r%bar",
            "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r",
            "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r%foo",
            "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN%bar",
            "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN",
            "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN%foo",
            "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%bar",
            "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye",
            "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo",
            "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf%bar",
            "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf",
            "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf%foo",
        ]
        .map(|x| TypedValue::Address(crate::ast::Address::from_base58_check(x).unwrap()));

        for (i, addr_i) in ordered_addrs.iter().enumerate() {
            for (j, addr_j) in ordered_addrs.iter().enumerate() {
                assert_eq!(addr_i.partial_cmp(addr_j), i.partial_cmp(&j));
                assert_eq!(addr_i.cmp(addr_j), i.cmp(&j));
            }
        }
    }

    #[test]
    /// checks that an array of chain ids is sorted without a priori assuming
    /// that the comparison operator on chain ids is transitive.
    fn compare_chain_ids() {
        // ordering was verified against octez-client
        let ordered_chain_ids = [
            "00000000", "00000001", "00000002", "00000100", "00000200", "01020304", "a0b0c0d0",
            "a1b2c3d4", "ffffffff",
        ]
        .map(|x| {
            TypedValue::ChainId(
                tezos_crypto_rs::hash::ChainId::try_from_bytes(&hex::decode(x).unwrap()).unwrap(),
            )
        });

        for (i, addr_i) in ordered_chain_ids.iter().enumerate() {
            for (j, addr_j) in ordered_chain_ids.iter().enumerate() {
                assert_eq!(addr_i.partial_cmp(addr_j), i.partial_cmp(&j));
                assert_eq!(addr_i.cmp(addr_j), i.cmp(&j));
            }
        }
    }

    #[test]
    #[should_panic(expected = "Comparing incomparable values in TypedValue")]
    fn compare_different_comparable() {
        // Comparable panics on different types
        use TypedValue as V;
        use TypedValue::*;
        let _ = Bool(true).cmp(&V::int(5)); //panics
    }
}

/*
Script to verify address ordering. Should print "with -1" for all checked address pairs.

```
#!/bin/bash

addrs=(
  "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw"
  "tz1SNL5w4RFRbCWRMB4yDWvoRQrPQxZmNzeQ"
  "tz1V8fDHpHzN8RrZqiYCHaJM9EocsYZch5Cy"
  "tz1WPGZjP9eHGqD9DkiRJ1xGRU1wEMY19AAF"
  "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%bar"
  "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%defauls"
  "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"
  "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%defaulu"
  "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%foo"
  "tz1hHGTh6Yk4k7d2PiTcBUeMvw6fJCFikedv"
  "tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH%bar"
  "tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH"
  "tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH%foo"
  "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r%bar"
  "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r"
  "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r%foo"
  "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN%bar"
  "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN"
  "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN%foo"
  "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%bar"
  "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye"
  "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo"
  "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf%bar"
  "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf"
  "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf%foo"
)

prev=""
for addr in "${addrs[@]}"; do
  if [ -n "$prev" ]; then
    echo $prev $addr
    octez-client --mode mockup run script 'parameter address; storage address; code { UNPAIR; SWAP; COMPARE; FAILWITH }' on storage "\"$prev\"" and input "\"$addr\"" 2>&1 | grep '^with'
  fi
  prev="$addr"
done
```
*/
