use clap::Parser;

/// Structure that encodes the CLI options, flags and commands
#[derive(Debug, Clone, clap::Parser)]
pub struct Cli {
    // Path to the input ELF executable
    #[arg(short, long)]
    pub input: String,
}

/// Parse the command-line arguments.
pub fn parse() -> Cli {
    Cli::parse()
}
