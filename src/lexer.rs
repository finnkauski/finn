/// Lexer ideas from:
///
/// https://www.youtube.com/watch?v=KZokxZrghCc&t=1079s
pub enum PunctuationKind {
    /// Keeps track of open brackets
    Open(usize),
    /// Keeps track of close brackets
    Close(usize),
    /// Actual punctuation for seperation purposes
    Seperator,
}

pub enum TokenType<'s> {
    /// End of token stream and file
    EOF,

    /// Punctuation like brackets, commas etc
    Punctuation { raw: char, kind: PunctuationKind },

    /// Actions you can take on an entity
    /// *, |, ->
    Operator(&'s str),

    /// Identifiers - Sequences of characters
    Identifier(&'s str),

    /// One Character
    Char(char),

    /// Numeric value
    Numeric { raw: &'s str },
}

pub struct Lexer<'s> {
    /// Human readable number of the line
    /// the lexer is on.
    pub current_line: usize,
    /// Human readable column the lexer is
    /// currently on.
    pub current_col: usize,
    /// Machine readable offset within the
    /// file.
    pub codepoint_offset: usize,

    /// Contents of the code
    pub contents: std::iter::Peekable<std::str::Chars<'s>>,
}

#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    #[error("Failed to operate on file")]
    IOError(#[from] std::io::Error),

    #[error("Missing expected token")]
    MissingExpectedSymbol { expected: String, found: String },
}
