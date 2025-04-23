// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

mod cli;
mod commands;
mod table;

use std::error::Error;

use octez_riscv::stepper::StepperStatus;

use self::commands::bench;
use self::commands::gdb_server;
use self::commands::run;

fn format_status(result: &StepperStatus) -> String {
    use StepperStatus::*;
    match result {
        Exited {
            success: true,
            status,
            ..
        } => format!("Ok (status = {status})"),
        Exited { status, .. } => format!("Exit with exit code = {}", status),
        Running { .. } => "Timeout".to_string(),
        Errored { cause, message, .. } => format!("{message}\nCaused by: {cause}"),
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = cli::parse();

    #[cfg(feature = "log")]
    {
        use octez_riscv::log::tracing_internal as tracing;

        let subscriber_builder = tracing_subscriber::fmt()
            .without_time()
            .with_max_level(cli.log_level);

        match cli.log_json_file {
            Some(log_json_file) => {
                let file = std::fs::File::create(log_json_file)?;
                let subscriber = subscriber_builder.json().with_writer(file).finish();
                tracing::subscriber::set_global_default(subscriber)?;
            }

            None => {
                let subscriber = subscriber_builder.with_writer(std::io::stderr).finish();
                tracing::subscriber::set_global_default(subscriber)?;
            }
        }
    }

    match cli.command {
        cli::Mode::Run(opts) => run(opts),
        cli::Mode::Bench(opts) => bench(opts),
        cli::Mode::GdbServer(opts) => gdb_server(opts),
    }
}
