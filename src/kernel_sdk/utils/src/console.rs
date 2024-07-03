// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Options for how the debug logs from the kernel are printed.

#![cfg(not(any(
    target_arch = "wasm32",
    all(target_arch = "riscv64", target_os = "hermit")
)))]

use quanta::Instant;
use serde::Serialize;
use std::io::{StdoutLock, Write};
use std::time::Duration;

/// A `Console` is a writeable sink, and is able to modify the output in certain ways,
/// for example by attaching timestamps to each line.
pub struct Console<'a> {
    lock: StdoutLock<'a>,
    options: ConsoleOptions,
}

impl<'a> Console<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            lock: std::io::stdout().lock(),
            options: ConsoleOptions::Passthrough,
        }
    }

    pub fn with_timings() -> Self {
        Self {
            lock: std::io::stdout().lock(),
            options: ConsoleOptions::Timing {
                line: LogLine {
                    message: String::new(),
                    elapsed: Duration::ZERO,
                },
                start: Instant::now(),
            },
        }
    }
}

impl Write for Console<'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match &mut self.options {
            ConsoleOptions::Passthrough => self.lock.write(buf),
            ConsoleOptions::Timing { line, start } => {
                let mut written = 0;

                for c in buf {
                    if line.message.is_empty() {
                        line.elapsed = start.elapsed();
                    }

                    if *c == b'\n' {
                        serde_json::to_writer(&mut self.lock, line)?;
                        self.lock.write_all(&[b'\n'])?;
                        line.message.truncate(0);
                    } else {
                        line.message.push((*c).into());
                    }
                    written += 1;
                }

                Ok(written)
            }
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.lock.flush()
    }
}

impl Drop for Console<'_> {
    fn drop(&mut self) {
        if let ConsoleOptions::Timing { line, .. } = &self.options {
            if !line.message.is_empty() {
                serde_json::to_writer(&mut self.lock, line).unwrap();
            }
        }
        self.lock.flush().unwrap();
    }
}

enum ConsoleOptions {
    Passthrough,
    Timing { line: LogLine, start: Instant },
}

#[derive(Serialize)]
struct LogLine {
    elapsed: Duration,
    message: String,
}
