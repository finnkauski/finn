use std::fs::File;
use std::io::prelude::*;

use clap::Parser;
use finn_core::lexer::Lexer;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// File to be used as source code
    file: Option<String>,
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();
    let content = {
        if let Some(filename) = cli.file {
            // TODO: error handling
            let mut reader = File::open(filename)?;
            let mut buf = String::new();
            reader.read_to_string(&mut buf)?;
            buf
        } else {
            // NOTE: example testing string
            String::from("3.1415 .1415 2.2e+92")
        }
    };

    let mut lexer = Lexer::new(&content);
    loop {
        let token = lexer.next_token();
        match token {
            Ok(finn_core::TokenType::EOF) => break Ok(()),
            Ok(token) => println!("Token: {token:?}"),
            Err(error) => println!("Error: {error:?}"),
        }
    }
}
