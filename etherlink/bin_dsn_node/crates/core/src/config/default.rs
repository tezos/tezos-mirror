// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Default configuration provider.
//!
//! Returns Default::default() for all queried components.
//! Intended for testing/mocking purposes only.

use super::ConfigProvider;

#[derive(Debug, Clone, Default)]
pub struct DefaultConfigProvider {}

impl ConfigProvider for DefaultConfigProvider {
    fn get_config<T: serde::de::DeserializeOwned + Default>(
        &self,
        _name: &'static str,
    ) -> Result<T, super::ConfigProviderError> {
        Ok(T::default())
    }
}
