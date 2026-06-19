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
        use super::Or as OrEnum;
        use std::cmp::Ordering::Equal;
        use TypedValue::*;

        // Compare two leaf primitives, propagating `None` (incomparable) by
        // returning from the whole comparison -- exactly as the recursive
        // lexicographic version did when a sub-comparison yielded `None`.
        macro_rules! leaf {
            ($a:expr, $b:expr) => {
                match $a.partial_cmp($b) {
                    Some(o) => o,
                    None => return None,
                }
            };
        }

        // Explicit LIFO worklist of sub-value pairs still to compare. For a
        // compound value the children are pushed so the left sibling is popped
        // (compared) first, reproducing the recursive lexicographic order: the
        // first non-`Equal` (or `None`) result wins and short-circuits. This
        // avoids recursing on deep `pair`/`option`/`or` values, which would
        // overflow the kernel's ~1 MiB Rust stack. Consensus-critical: the
        // ordering must stay byte-exact with L1, since it backs `BTreeSet` /
        // `BTreeMap` key ordering (set/map construction, membership, updates).
        // See L2-1449.
        let mut stack: Vec<(&TypedValue, &TypedValue)> = vec![(self, other)];
        while let Some((a, b)) = stack.pop() {
            let ord = match (a, b) {
                (Int(x), Int(y)) => leaf!(x, y),
                (Nat(x), Nat(y)) => leaf!(x, y),
                (Mutez(x), Mutez(y)) => leaf!(x, y),
                (Bool(x), Bool(y)) => leaf!(x, y),
                (String(x), String(y)) => leaf!(x, y),
                (Unit, Unit) => Equal,
                (Address(x), Address(y)) => leaf!(x, y),
                (ChainId(x), ChainId(y)) => leaf!(x, y),
                (Bytes(x), Bytes(y)) => leaf!(x, y),
                (Key(x), Key(y)) => leaf!(x, y),
                (Signature(x), Signature(y)) => leaf!(x, y),
                (KeyHash(x), KeyHash(y)) => leaf!(x, y),
                (Timestamp(x), Timestamp(y)) => leaf!(x, y),

                (Pair(l1, l2), Pair(r1, r2)) => {
                    // Push the right child first so the left child is compared
                    // first (lexicographic order).
                    stack.push((l2.as_ref(), r2.as_ref()));
                    stack.push((l1.as_ref(), r1.as_ref()));
                    Equal
                }

                (Option(x), Option(y)) => match (x.as_deref(), y.as_deref()) {
                    (None, None) => Equal,
                    (None, Some(_)) => std::cmp::Ordering::Less,
                    (Some(_), None) => std::cmp::Ordering::Greater,
                    (Some(xa), Some(yb)) => {
                        stack.push((xa, yb));
                        Equal
                    }
                },

                (Or(x), Or(y)) => match (x, y) {
                    (OrEnum::Left(la), OrEnum::Left(ra)) => {
                        stack.push((la.as_ref(), ra.as_ref()));
                        Equal
                    }
                    (OrEnum::Right(la), OrEnum::Right(ra)) => {
                        stack.push((la.as_ref(), ra.as_ref()));
                        Equal
                    }
                    (OrEnum::Left(_), OrEnum::Right(_)) => std::cmp::Ordering::Less,
                    (OrEnum::Right(_), OrEnum::Left(_)) => std::cmp::Ordering::Greater,
                },

                // Mismatched types and non-comparable types are incomparable,
                // exactly as the per-variant `(X(..), _) => None` arms and the
                // `List | Set | Map | ... ` group did before.
                _ => return None,
            };
            if ord != Equal {
                return Some(ord);
            }
        }
        Some(Equal)
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
            "00000000", "00000001", "00000002", "00000100", "00000200", "01020304",
            "a0b0c0d0", "a1b2c3d4", "ffffffff",
        ]
        .map(|x| {
            TypedValue::ChainId(
                tezos_crypto_rs::hash::ChainId::try_from_bytes(&hex::decode(x).unwrap())
                    .unwrap(),
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

    /// Regression for L2-1449: the previous recursive `partial_cmp`/`cmp`
    /// overflowed the kernel's ~1 MiB Rust stack on a deep `pair`. Build
    /// right-comb pairs that agree on every left element and differ only at the
    /// deepest leaf, so the comparison must walk the full depth.
    #[test]
    fn deep_pair_cmp_does_not_overflow() {
        use std::cmp::Ordering;
        const DEPTH: usize = 100_000;
        let deep = |leaf: TypedValue<'static>| {
            let mut v = leaf;
            for _ in 0..DEPTH {
                v = TypedValue::new_pair(TypedValue::int(0), v);
            }
            v
        };
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(move || {
                let a = deep(TypedValue::int(0));
                let b = deep(TypedValue::int(1));
                assert_eq!(a.partial_cmp(&b), Some(Ordering::Less));
                assert_eq!(b.partial_cmp(&a), Some(Ordering::Greater));
                assert_eq!(a.partial_cmp(&a), Some(Ordering::Equal));
                assert_eq!(a.cmp(&b), Ordering::Less);
                // `TypedValue` has no iterative `Drop` yet (L2-1446); skip the
                // recursive destructor on these deep values.
                std::mem::forget(a);
                std::mem::forget(b);
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    /// Deep comparable values are used as `BTreeSet`/`BTreeMap` keys; insertion
    /// and lookup call `Ord::cmp`, which must not overflow (L2-1449).
    #[test]
    fn deep_set_key_does_not_overflow() {
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let deep = |leaf: TypedValue<'static>| {
                    let mut v = leaf;
                    for _ in 0..DEPTH {
                        v = TypedValue::new_pair(TypedValue::int(0), v);
                    }
                    v
                };
                let mut set = std::collections::BTreeSet::new();
                set.insert(deep(TypedValue::int(0)));
                set.insert(deep(TypedValue::int(1)));
                let probe = deep(TypedValue::int(1));
                assert!(set.contains(&probe));
                assert_eq!(set.len(), 2);
                // Move the keys out and forget them so their recursive
                // destructor does not run on scope exit (L2-1446).
                std::mem::forget(probe);
                for k in std::mem::take(&mut set) {
                    std::mem::forget(k);
                }
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    /// Companion to `deep_pair_cmp_does_not_overflow` (L2-1449): the other two
    /// compound comparable constructors, `option` and `or`, also recursed in
    /// the previous `partial_cmp`. Compare two deep chains that differ only at
    /// the deepest leaf, so the worklist must walk the full depth (None < Some
    /// / inner `Left` ordering) without overflowing.
    #[test]
    fn deep_option_or_cmp_does_not_overflow() {
        use std::cmp::Ordering;
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                // option: Some(Some(... Some(leaf)))
                let deep_opt = |leaf: TypedValue<'static>| {
                    let mut v = leaf;
                    for _ in 0..DEPTH {
                        v = TypedValue::new_option(Some(v));
                    }
                    v
                };
                let a = deep_opt(TypedValue::int(0));
                let b = deep_opt(TypedValue::int(1));
                assert_eq!(a.partial_cmp(&b), Some(Ordering::Less));
                assert_eq!(b.partial_cmp(&a), Some(Ordering::Greater));
                std::mem::forget(a);
                std::mem::forget(b);

                // or: Left(Left(... Left(leaf)))
                let deep_or = |leaf: TypedValue<'static>| {
                    let mut v = leaf;
                    for _ in 0..DEPTH {
                        v = TypedValue::new_or(crate::ast::Or::Left(v));
                    }
                    v
                };
                let c = deep_or(TypedValue::int(0));
                let d = deep_or(TypedValue::int(1));
                assert_eq!(c.partial_cmp(&d), Some(Ordering::Less));
                std::mem::forget(c);
                std::mem::forget(d);
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
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
