// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! `inbox.json` files are consumed as an optional input argument
//! by both *wasm debugger* and *riscv sandbox*.

use serde::{Deserialize, Serialize};
use std::{fs, io::Write, path::Path};

/// Single Inbox message
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Message {
    /// Already serialised inbox message
    Raw(#[serde(with = "hex::serde")] Vec<u8>),

    /// External inbox message
    External {
        /// Payload of the external message - will be tagged with
        /// the 'external message byte tag' in the inbox.
        #[serde(with = "hex::serde")]
        external: Vec<u8>,
    },
}

/// Inbox contents read from a file grouped by levels.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct InboxFile(pub Vec<Vec<Message>>);

impl InboxFile {
    /// Load the Inbox file.
    pub fn load(path: &Path) -> Result<Self, Box<dyn std::error::Error>> {
        let contents = fs::read(path)?;
        let inbox = serde_json::de::from_slice(contents.as_slice())?;
        Ok(inbox)
    }

    /// Write the Inbox file to a file.
    #[allow(unused)]
    pub fn save(&self, path: &Path) -> Result<(), Box<dyn std::error::Error>> {
        let mut file = fs::File::create(path)?;
        serde_json::ser::to_writer_pretty(&mut file, self)?;
        Ok(())
    }

    /// Save the Inbox file as a shell script.
    pub fn save_script(&self, path: &Path) -> Result<(), Box<dyn std::error::Error>> {
        let mut file = fs::File::create(path)?;
        writeln!(file, "#!/bin/sh")?;

        for level in self.0.iter() {
            for msg in level {
                let hex = match msg {
                    Message::Raw(bytes) => bytes.as_slice(),
                    Message::External { external } => external.as_slice(),
                };
                let hex = hex::encode(hex);

                writeln!(file, "octez-client --wait none send smart rollup message 'hex:[\"{hex}\"]' from bootstrap2")?;
                writeln!(file, "octez-client bake for --minimal-timestamp")?;
            }
        }

        Ok(())
    }
}
