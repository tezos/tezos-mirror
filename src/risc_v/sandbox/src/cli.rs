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
}

/// Parse the command-line arguments.
pub fn parse() -> Cli {
    Cli::parse()
}
