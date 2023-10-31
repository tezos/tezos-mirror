use super::TypedValue;

impl PartialOrd for TypedValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use TypedValue::*;
        match (self, other) {
            (Int(a), Int(b)) => a.partial_cmp(b),
            (Nat(a), Nat(b)) => a.partial_cmp(b),
            (Mutez(a), Mutez(b)) => a.partial_cmp(b),
            (Bool(a), Bool(b)) => a.partial_cmp(b),
            (String(a), String(b)) => a.partial_cmp(b),
            (Unit, Unit) => Some(std::cmp::Ordering::Equal),
            (Pair(l), Pair(r)) => l.partial_cmp(r),
            (Option(x), Option(y)) => x.as_deref().partial_cmp(&y.as_deref()),
            (Or(x), Or(y)) => x.as_ref().partial_cmp(y.as_ref()),
            (Address(l), Address(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}

impl Ord for TypedValue {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other)
            .expect("Comparing incomparable values in TypedValue")
    }
}

#[cfg(test)]
mod tests {
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

        assert_cmp!(Int; -1; 0; Less);
        assert_cmp!(Int; -1; -1; Equal);
        assert_cmp!(Int; -1; -2; Greater);

        assert_cmp!(Nat; 3; 4; Less);
        assert_cmp!(Nat; 4; 4; Equal);
        assert_cmp!(Nat; 5; 4; Greater);

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
        assert_cmp!(V::new_option; None; Some(Int(3)); Less);
        assert_cmp!(V::new_option; Some(Int(3)); None; Greater);
        assert_cmp!(V::new_option; Some(Int(3)); Some(Int(4)); Less);
        assert_cmp!(V::new_option; Some(Int(4)); Some(Int(4)); Equal);
        assert_cmp!(V::new_option; Some(Int(4)); Some(Int(3)); Greater);

        assert_cmp!(V::new_pair; Int(3), Nat(4); Int(3), Nat(5); Less);
        assert_cmp!(V::new_pair; Int(3), Nat(4); Int(4), Nat(4); Less);
        assert_cmp!(V::new_pair; Int(3), Nat(4); Int(3), Nat(4); Equal);
        assert_cmp!(V::new_pair; Int(4), Nat(4); Int(3), Nat(4); Greater);
        assert_cmp!(V::new_pair; Int(3), Nat(5); Int(3), Nat(4); Greater);

        use crate::ast::Or;

        assert_cmp!(V::new_or; Or::Left(Int(3)); Or::Left(Int(4)); Less);
        assert_cmp!(V::new_or; Or::Left(Int(5)); Or::Left(Int(4)); Greater);
        assert_cmp!(V::new_or; Or::Left(Int(4)); Or::Left(Int(4)); Equal);
        assert_cmp!(V::new_or; Or::Right(Int(3)); Or::Right(Int(4)); Less);
        assert_cmp!(V::new_or; Or::Right(Int(5)); Or::Right(Int(4)); Greater);
        assert_cmp!(V::new_or; Or::Right(Int(4)); Or::Right(Int(4)); Equal);
        assert_cmp!(V::new_or; Or::Left(Int(5)); Or::Right(Int(3)); Less);
        assert_cmp!(V::new_or; Or::Right(Int(3)); Or::Left(Int(5)); Greater);

        // different types don't compare
        assert_eq!(Bool(true).partial_cmp(&Int(5)), None);
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
    #[should_panic(expected = "Comparing incomparable values in TypedValue")]
    fn compare_different_comparable() {
        // Comparable panics on different types
        use TypedValue::*;
        let _ = Bool(true).cmp(&Int(5)); //panics
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
