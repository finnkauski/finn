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
        if let Some(filename) = cli.file {
            // TODO: error handling
            let mut reader = File::open(filename)?;
            let mut buf = String::new();
            reader.read_to_string(&mut buf)?;
            buf
        } else {
            // NOTE: example testing string
            String::from("( () )")
        }
    };

    let mut lexer = Lexer::new(&content);
    loop {
        let token = lexer.next_token();
        match token {
            Ok(finn::lexer::Token::EOF) => break Ok(()),
            Ok(token) => println!("Token: {token:?}"),
            Err(error) => println!("Error: {error:?}"),
        }
    }
}
