// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! DSN config interface.
//!
//! This abstraction allows to share a common configuration for multiple components,
//! and load typed configuration for a specific component with lazy deserialization.
//! Also enables dynamic configuration retrieval (e.g. from a remove server) and
//! generally more complicated strategies compared to a static structure.
//!
//! Configuration provider requires all types to implement Default so that it can
//! be used for the trivial default provider, and Serialize / Deserialize.
//! If you want configuration provider to fall back to defaults in case some fields
//! are missing, specify that using serde attributes:
//!
//! ```ignore
//! #[derive(Serialize, Deserialize)]
//! struct MyConfig {
//!     pub non_default_field: u64,
//!     #[serde(default)]
//!     pub default_field: MySubConfig,
//! }   
//! ```
//!
//! How to use:
//! 1. Instantiate the ConfigProvider implementation on the top level of your application
//! 2. Let your components accept that instance either way:
//!         - `pub fn with_config<CP: ConfigProvider>(config_provider: Arc<ConfigProvider>)`
//!         - `pub fn with_config(config_provider: &impl ConfigProvider)`
//! 3. If some components need to initialize subcomponents, they can pass the config provider
//!    instance down the line.

pub mod default;

use serde::de::DeserializeOwned;

#[derive(Debug, thiserror::Error)]
pub enum ConfigProviderError {
    /// Implement `From<YourErrorType>` for `ConfigProviderError` for convenience.
    #[error("Internal config provider error: {0}")]
    Internal(#[source] Box<dyn std::error::Error + Send + Sync + 'static>),
    // Add provider-agnostic errors that have to be handled in a specific manner,
    // e.g. retriable connection failure (might be AWS secrets or similar service)
}

pub trait ConfigProvider: Send + Sync + 'static {
    /// Get configuration for a particular component given its name.
    fn get_config<T: DeserializeOwned + Default>(
        &self,
        name: &'static str,
    ) -> Result<T, ConfigProviderError>;
}
