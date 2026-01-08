// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::Error;
use tezos_protocol::entrypoint;

#[derive(Debug, uniffi::Error, thiserror::Error)]
#[uniffi(flat_error)]
pub enum EntrypointError {
    #[error("Entrypoint format failure: {0:?}")]
    Format(#[from] entrypoint::ByteReprError),
}

#[derive(uniffi::Object, Debug, Clone, PartialEq, Default, Eq)]
#[uniffi::export(Debug, Display, Eq)]
pub struct Entrypoint(pub(crate) entrypoint::Entrypoint);

#[uniffi::export]
impl Entrypoint {
    #[uniffi::constructor]
    pub fn new(name: &str) -> Result<Self, Error> {
        name.try_into()
            .map(Self)
            .map_err(|e| Error::Entrypoint(EntrypointError::Format(e)))
    }
}

impl ::std::fmt::Display for Entrypoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use paste::paste;

    macro_rules! test_conversion {
        ($ep_name:ident) => {
            paste! {

                #[test]
                fn [<build_ $ep_name _entrypoint>]() {
                    let raw_entrypoint = stringify!($ep_name);
                    let entrypoint = Entrypoint::new(raw_entrypoint).expect(&format!(
                        "Building entrypoint `{}` should succeed",
                        raw_entrypoint
                    ));

                    assert_eq!(
                        entrypoint.to_string(),
                        raw_entrypoint,
                        "Entrypoint name must not have changed"
                    );
                }
            }
        };
    }

    test_conversion!(default);
    test_conversion!(root);
    test_conversion!(do);
    test_conversion!(set_delegate);
    test_conversion!(remove_delegate);
    test_conversion!(deposit);
    test_conversion!(stake);
    test_conversion!(unstake);
    test_conversion!(finalize_unstake);
    test_conversion!(set_delegate_parameters);
    test_conversion!(custom);

    #[test]
    fn build_empty_entrypoint() {
        let entrypoint =
            Entrypoint::new("").expect(&format!("Building empty entrypoint should succeed"));

        assert_eq!(
            entrypoint.to_string(),
            entrypoint::DEFAULT_EP_NAME,
            "Empty entrypoint should be converted to default"
        );
    }

    #[test]
    fn build_wrong_formatted_entrypoint() {
        assert!(
            matches!(
                Entrypoint::new("?"),
                Err(Error::Entrypoint(EntrypointError::Format(
                    entrypoint::ByteReprError::WrongFormat(_)
                )))
            ),
            "Wrong formatted entrypoint should fail with a WrongFormat error"
        );
    }
}
