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
            (Pair(l1, r1), Pair(l2, r2)) => (l1, r1).partial_cmp(&(l2, r2)),
            (Option(x), Option(y)) => x.as_deref().partial_cmp(&y.as_deref()),
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

        // different types don't compare
        assert!(Bool(true).partial_cmp(&Int(5)) == None);
    }

    #[test]
    #[should_panic(expected = "Comparing incomparable values in TypedValue")]
    fn compare_different_comparable() {
        // Comparable panics on different types
        use TypedValue::*;
        let _ = Bool(true).cmp(&Int(5)); //panics
    }
}
