use clap::{ArgEnum, Parser, Subcommand};
use finn_core::lexer::Lexer;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// File to be used as source code
    file: String,

    #[clap(subcommand)]
    command: Commands,
}
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ArgEnum)]
enum DebugShow {
    Ast,
    Tokens,
}

#[derive(Subcommand)]
enum Commands {
    Debug {
        #[clap(arg_enum)]
        show: DebugShow,
    },
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();

    let text = std::fs::read_to_string(cli.file)?;

    let mut lexer = Lexer::new(&text);

    match &cli.command {
        Commands::Debug { show } => match show {
            &DebugShow::Tokens => loop {
                let token = lexer.next_token();
                match token {
                    Ok(finn_core::TokenType::EOF) => break,
                    Ok(token) => println!("Token: {token:?}"),
                    Err(error) => println!("Error: {error:?}"),
                }
            },
            &DebugShow::Ast => {
                println!("AST Debug not implemented");
            }
        },
    };
    Ok(())
}
