use clap::{Parser, Subcommand};
use finn::lexer;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// File to be used as source code
    file: Option<String>,

    #[clap(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// does testing things
    Test {
        /// lists test values
        #[clap(short, long)]
        list: bool,
    },
}

fn main() {
    let cli = Cli::parse();
}
