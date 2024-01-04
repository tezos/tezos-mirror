use clap::Parser;

/// Structure that encodes the CLI options, flags and commands
#[derive(Debug, Clone, Parser)]
pub struct Cli {
    /// Path to the input ELF executable
    #[arg(short, long)]
    pub input: String,

    /// Path to the initrd
    #[arg(long)]
    pub initrd: Option<String>,

    /// Support some POSIX-style system calls
    #[arg(long)]
    pub posix: bool,

    /// Rollup address
    #[arg(short, long, default_value = "sr1UNDWPUYVeomgG15wn5jSw689EJ4RNnVQa")]
    pub address: String,

    /// Rollup origination level
    #[arg(short = 'l', long, default_value_t = 0)]
    pub origination_level: u64,
}

/// Parse the command-line arguments.
pub fn parse() -> Cli {
    Cli::parse()
}
