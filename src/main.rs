use std::fs::File;
use std::io::prelude::*;

use clap::Parser;
use finn::lexer::Lexer;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// File to be used as source code
    file: Option<String>,
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();
    let content = {
        // TODO: error handling
        let mut reader = File::open(cli.file.expect("File needs to be provided"))?;
        let mut buf = String::new();
        reader.read_to_string(&mut buf)?;
        buf
    };

    let mut lexer = Lexer::new(&content);
    let token = lexer.next_token();
    println!("Token: {token:?}");

    Ok(())
}
