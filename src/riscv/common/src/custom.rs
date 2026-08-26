// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Shared wiring for the OCaml custom blocks this API hands out.

/// Implement [`ocaml::Custom`] for a type whose name comes from an associated constant.
///
/// `name` is a [`CStr`], so the identifier handed to OCaml is NUL-terminated by
/// construction. [`ocaml::Custom::NAME`] is derived from it, without the terminator, and is
/// only used for diagnostics.
///
/// ```ignore
/// impl_ocaml_custom! {
///     impl [T: CustomGcResource] ImmutableState<T> {
///         name: T::IMMUTABLE_NAME,
///         used: T::IMMUTABLE_USED,
///         max: T::IMMUTABLE_MAX,
///     }
/// }
/// ```
///
/// Generic parameters and the where-clause are taken as bracketed token lists, since a macro
/// cannot otherwise match a bound such as `KV: WriteableKeyValueStore`.
///
/// [`CStr`]: core::ffi::CStr
#[macro_export]
macro_rules! impl_ocaml_custom {
    (
        impl [$($generics:tt)*] $ty:ty
        $(where [$($bounds:tt)*])?
        {
            name: $name:expr,
            used: $used:expr,
            max: $max:expr $(,)?
        }
    ) => {
        impl<$($generics)*> ocaml::Custom for $ty
        $(where $($bounds)*)?
        {
            const NAME: &'static str = match $name.to_str() {
                Ok(name) => name,
                Err(_) => panic!("an ocaml::Custom name must be valid UTF-8"),
            };

            const OPS: ocaml::custom::CustomOps = ocaml::custom::CustomOps {
                identifier: $name.as_ptr() as *const ocaml::sys::Char,
                ..ocaml::custom::CustomOps {
                    finalize: Some(Self::finalize),
                    ..ocaml::custom::DEFAULT_CUSTOM_OPS
                }
            };

            const USED: usize = $used;

            const MAX: usize = $max;
        }
    };
}
