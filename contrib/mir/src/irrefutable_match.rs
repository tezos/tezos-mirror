/// Helper macro emulating forced irrefutable pattern matches, i.e. matches that
/// panic at runtime if they fail. This helps avoid the awkward
///
/// ```text
/// let x = match y {
///     Something(z) => z
///     _ => panic!(<panic description>)
/// }
/// ```
///
/// when it is known `y` is `Something`.
///
/// It is to an extent equivalent to a let-else expression with a panic with an
/// automatic message in the else branch.
///
/// There are three forms of this macro:
///
/// ```text
/// let x = irrefutable_match!(expr; Enum::Constructor)
/// ```
///
/// will match `expr` with `Enum::Constructor(i)` and return `i`. This works
/// well for single-argument enum constructors, which is a common case.
///
/// this is roughly equivalent to the let-else construct:
///
/// ```text
/// let Enum::Constructor(x) = expr else { panic!(<panic description>) };
/// ```
///
/// ```text
/// irrefutable_match!(expr; Enum::Constructor, ident, ...)
/// ```
///
/// where `...` is optionally more identifiers.
///
/// will match `expr` with `Enum::Constructor(ident, ...)` and _bind_ the
/// variables `ident` &c. This is useful to destructure tuple enum variants with
/// multiple fields.
///
/// this is roughly equivalent to the let-else construct:
///
/// ```text
/// let Enum::Constructor(ident,...) = expr else { panic!(<panic description>) };
/// ```
///
/// In these two forms, `expr`'s type must implement `Debug`.
///
/// A third form,
///
/// ```text
/// irrefutable_match!(expr;)
/// ```
///
/// is equivalent to `expr`, and exists for the sake of convenience of
/// implementing helper macros on top of this.
macro_rules! irrefutable_match {
    ($e:expr $(;)?) => {
        $e
    };
    ($e:expr; $p:path) => {
        match $e {
            #[allow(unused_parens)]
            $p(i) => i,
            i => panic!(
                "assertion failed: `{:?}` doesn't match `{}`",
                i,
                std::stringify!($p(_))
            ),
        }
    };
    ($e:expr; $p:path, $( $a:ident ),+) => {
        #[allow(unused_parens)]
        let ($($a),*) = match $e {
            $p($($a),*) => ($($a),*),
            i => panic!(
                "assertion failed: `{:?}` doesn't match `{}`",
                i,
                std::stringify!($p($($a),*))
            ),
        };
    };
}

pub(crate) use irrefutable_match;

#[cfg(test)]
mod tests {
    #[derive(Debug)]
    enum Tuple {
        Tuple1(&'static str),
        Tuple3(&'static str, &'static str, &'static str),
    }

    #[test]
    fn match_zero() {
        assert_eq!("foo", irrefutable_match!("foo"))
    }

    #[test]
    fn match_one() {
        assert_eq!("foo", irrefutable_match!(Some("foo"); Some))
    }

    #[test]
    #[should_panic(expected = "assertion failed: `None` doesn't match `Some (_)`")]
    fn match_one_fail() {
        irrefutable_match!(None; Some)
    }

    #[test]
    fn match_several() {
        irrefutable_match!(Tuple::Tuple3("foo", "bar", "baz"); Tuple::Tuple3, x, y, z);
        assert_eq!(x, "foo");
        assert_eq!(y, "bar");
        assert_eq!(z, "baz");
    }

    #[test]
    #[should_panic(
        expected = r#"assertion failed: `Tuple1("foo")` doesn't match `Tuple::Tuple3 (_x, _y, _z)`"#
    )]
    fn match_several_fail() {
        irrefutable_match!(Tuple::Tuple1("foo"); Tuple::Tuple3, _x, _y, _z);
    }
}
