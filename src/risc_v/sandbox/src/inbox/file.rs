// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use serde::Deserialize;
use std::{fs, path::Path};

/// Single Inbox message
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Message {
    /// Already serialised inbox message
    Raw(#[serde(with = "hex::serde")] Vec<u8>),

    /// External inbox message
    External {
        #[serde(with = "hex::serde")]
        external: Vec<u8>,
    },
}

/// Inbox contents read from a file grouped by levels.
#[derive(Debug, Clone, Deserialize)]
pub struct InboxFile(pub Vec<Vec<Message>>);

impl InboxFile {
    /// Load the Inbox file.
    pub fn load(path: &Path) -> Result<Self, Box<dyn std::error::Error>> {
        let contents = fs::read(path)?;
        let inbox = serde_json::de::from_slice(contents.as_slice())?;
        Ok(inbox)
    }
}
