// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

/// The `csr!` macro generates a type describing a CSR, its value,
/// associated traits and methods.
#[macro_export]
macro_rules! csr {
    {
        pub struct $group:ident {
            $( $name:ident: $type:ty ),+
            $( , )?
        }
    } => {
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub struct $group(u64);
        $crate::csr_bits!($group;; $( $name: $type ),+);
        $crate::csr_debug!($group;; $( $name ),+);
        $crate::csr_new!($group;; $( $name: $type ),+);
        $crate::csr_fields!(0;; $group;; $( $name: $type ),+);
    };

    {
        struct $group:ident {
            $( $name:ident: $type:ty ),+
        }
    } => {
        #[derive(Clone, Copy, PartialEq, Eq)]
        struct $group(u64);
        $crate::csr_bits!($group;; $( $name: $type ),+);
        $crate::csr_debug!($group;; $( $name ),+);
        $crate::csr_new!($group;; $( $name: $type ),+);
        $crate::csr_fields!(0;; $group;; $( $name: $type ),+);
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! csr_bits {
    ( $group:ident;; $( $name:ident: $type:ty ),+ ) => {
        impl $crate::bits::Bits64 for $group {
            const WIDTH: usize = {{ $crate::csr_width!($($type),+) }};

            #[inline(always)]
            fn from_bits(value: u64) -> Self {
                let mut new_self = Self(0);
                let fake_self = Self(value);

                paste::paste! {
                    $(
                        new_self = new_self.[<with_ $name:lower>](
                            fake_self.[<$name:lower>]()
                        );
                    )+
                }

                new_self
            }

            #[inline(always)]
            fn to_bits(&self) -> u64 {
                self.0
            }
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! csr_width {
    ( $type:ty ) => {
        <$type>::WIDTH
    };

    ( $type0:ty, $( $type1:ty ),+ ) => {
        $crate::csr_width!($type0) + $crate::csr_width!($($type1),+)
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! csr_fields {
    ( $accum:expr;; $group:ident;; $name:ident: $type:ty ) => {
        paste::paste! {
            #[allow(clippy::allow_attributes, reason = "Macro may trigger warnings conditionally in certain contexts")]
            #[allow(dead_code, reason = "Macro may generate unused code")]
            impl $group {
                pub const [<$name:upper _OFFSET>]: usize = { $accum };

                #[inline(always)]
                pub fn [<$name:lower>](self) -> $type {
                    let offset = <$type as $crate::bits::Bits64>::WIDTH.saturating_sub(1);
                    let bits = $crate::bits::u64::bits_subset(self.0, ($accum + offset), ($accum));
                    <$type as $crate::bits::Bits64>::from_bits(bits)
                }

                #[inline(always)]
                pub fn [<with_ $name:lower>](self, value: $type) -> Self {
                    let offset = <$type as $crate::bits::Bits64>::WIDTH.saturating_sub(1);
                    let new_self = $crate::bits::u64::replace_subset(self.0, ($accum + offset), ($accum), $crate::bits::Bits64::to_bits(&value));
                    $group(new_self)
                }

                #[inline(always)]
                pub fn [<set_ $name:lower>](&mut self, value: $type) -> &mut Self {
                    let offset = <$type as $crate::bits::Bits64>::WIDTH.saturating_sub(1);
                    self.0 = $crate::bits::u64::replace_subset(self.0, ($accum + offset), ($accum), $crate::bits::Bits64::to_bits(&value));
                    self
                }
            }
        }
    };

    ( $accum:expr;; $group:ident;; $name:ident: $type:ty, $( $names:ident: $types:ty ),+ ) => {
        $crate::csr_fields!($accum;; $group;; $name: $type);
        $crate::csr_fields!($accum + <$type as $crate::bits::Bits64>::WIDTH;; $group;; $( $names: $types ),+);
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! csr_debug {
    ( $group:ident;; $( $name:ident ),*) => {
        impl std::fmt::Debug for $group {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut str = f.debug_struct(stringify!($group));

                paste::paste! {
                    $(
                        str.field(
                            stringify!($name),
                            &self.[<$name:lower>](),
                        );
                    )*
                }

                str.finish()
            }
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! csr_new {
    ( $group:ident;; $( $name:ident: $type:ty ),* ) => {
        #[allow(clippy::allow_attributes, reason = "Macro may trigger warnings conditionally in certain contexts")]
        #[allow(clippy::too_many_arguments, reason = "Macro may generate too many arguments")]
        #[allow(non_snake_case, reason = "Macro may generate non snake case names")]
        impl $group {
            pub fn new(
                $(
                    $name: $type,
                )*
            ) -> Self {
                let mut myself = <Self as $crate::bits::Bits64>::from_bits(0u64);

                paste::paste! {
                    $(
                        myself = myself.[<with_ $name:lower>]($name);
                    )*
                }

                myself
            }
        }
    };
}

/// Normalise the fields for a Control or State register using WARL/WPRI
pub trait NormaliseFields {
    fn normalise(self) -> Self;
}

#[cfg(test)]
mod tests {
    use crate::bits::Bits64;
    use crate::bits::ConstantBits;

    csr! {
        pub struct Test {
            A: bool,
            reserved: ConstantBits<1, 1>,
            C: bool
        }
    }

    #[test]
    fn reserved_works() {
        let test = Test::from_bits(0u64);
        assert_eq!(test.to_bits(), 0b010);

        let test = Test::new(true, ConstantBits, false);
        assert_eq!(test.to_bits(), 0b011);
    }
}
