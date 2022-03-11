use std::collections::HashMap;
/// Lexer ideas from:
///
/// https://www.youtube.com/watch?v=KZokxZrghCc&t=1079s

/// The different kinds of punctuation
#[derive(Debug, PartialEq, Clone)]
pub enum PunctuationKind {
    /// Keeps track of open brackets
    Open(BalancingDepth),
    /// Keeps track of close brackets
    Close(BalancingDepth),
    /// Actual punctuation for seperation purposes
    Seperator,
}
pub type BalancingDepth = u32;
enum BalancingUpdate {
    Push,
    Pop,
}

/// The core tokens we can process
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    /// End of token stream and file
    EOF,

    /// Punctuation like brackets, commas etc
    Punctuation { raw: char, kind: PunctuationKind },

    /// Actions you can take on an entity
    /// *, |, ->
    Operator(String),

    /// Identifiers - Sequences of characters
    Identifier(String),

    /// One Character
    Char(char),

    /// Numeric value
    Numeric { raw: String },
}

/// The lexer
///
/// This is the main struct for the Lexer
/// TODO: embed the line and current column updates into the chars
#[derive(Debug)]
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
    chars: std::iter::Peekable<std::str::Chars<'s>>,

    bracket_balancing: HashMap<char, BalancingDepth>,
}

impl<'l> Lexer<'l> {
    /// Initialise a new lexer from a given source string
    pub fn new(chars: &str) -> Lexer {
        Lexer {
            current_line: 1,
            current_col: 1,
            codepoint_offset: 0,

            chars: chars.chars().peekable(),

            bracket_balancing: HashMap::default(),
        }
    }

    fn map_balance(c: &char) -> char {
        match c {
            ')' => '(',
            '(' => ')',
            '}' => '{',
            '{' => '}',
            ']' => '[',
            '[' => ']',
            _ => panic!("Tried balancing a character that doesn't make sense to balance: {c:?}"),
        }
    }

    /// Handles updating the internal state of the lexer to keep track of the state
    /// of the bracket balancing.
    fn update_balancing(&mut self, c: &char, how: BalancingUpdate) -> LResult<BalancingDepth> {
        match how {
            BalancingUpdate::Push => Ok(if let Some(v) = self.bracket_balancing.get_mut(c) {
                *v += 1;
                *v - 1
            } else {
                self.bracket_balancing.insert(*c, 1);
                0
            }),
            BalancingUpdate::Pop => match self.bracket_balancing.get_mut(&Lexer::map_balance(c)) {
                Some(0) => Err(LexerError::MisbalancedSymbol {
                    symbol: *c,
                    open: Lexer::map_balance(c),
                }),
                Some(v) => {
                    *v -= 1;
                    Ok(*v)
                }
                None => Err(LexerError::MisbalancedSymbol {
                    symbol: *c,
                    open: Lexer::map_balance(c),
                }),
            },
        }
    }

    /// Main functio to convert a character to a token type
    fn to_type(&mut self, c: char) -> LResult<Token> {
        match c {
            '(' => Ok(Token::Punctuation {
                raw: c,
                kind: PunctuationKind::Open(self.update_balancing(&c, BalancingUpdate::Push)?),
            }),
            ')' => Ok(Token::Punctuation {
                raw: c,
                // TODO: would this make sense for Close and Open to store an option.
                kind: PunctuationKind::Close(self.update_balancing(&c, BalancingUpdate::Pop)?),
            }),
            _ => Err(LexerError::UnknownSymbolError { symbol: c }),
        }
    }

    /// Consumes the next character and handles updating the state
    /// of the lexers line and column tracking
    pub fn consume_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                if c == '\n' {
                    self.current_line += 1;
                }
                self.current_col += 1;
                // NOTE: this is where the other encoding support can come in handy
                self.codepoint_offset += 1;

                Some(c)
            }
            None => None,
        }
    }

    /// Skips all whitespace
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.consume_char();
        }
    }

    pub fn next_token(&mut self) -> LResult<Token> {
        self.skip_whitespace();

        match self.consume_char() {
            Some(c) => self.to_type(c),
            None => Ok(Token::EOF),
        }
    }
}

impl<'l> Iterator for Lexer<'l> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token::EOF) => None,
            Ok(token) => Some(token),
            Err(_) => None,
        }
    }
}

/// Lexer related errors
///
/// NOTE: unclear if tying the lifetime of this to the
/// input string of a file is a wise decision or a premature
/// optimisation that would lead to more issues down the
/// line.
#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    #[error("Failed to operate on file")]
    IOError(#[from] std::io::Error),

    #[error("Expected {expected:?}, found {found:?}")]
    MissingExpectedSymbol { expected: Token, found: Token },

    #[error("Can't find opening symbol for: {symbol:?}")]
    MisbalancedSymbol { symbol: char, open: char },

    #[error("Unknown token: {symbol:?}")]
    UnknownSymbolError { symbol: char },
}
pub type LResult<'s, T> = Result<T, LexerError>;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn check_bracket_nesting() {
        // TODO: when iterator is implemented this can be refactors
        let lex = Lexer::new("(())");
        let mut v = Vec::new();
        for token in lex {
            v.push(token)
        }
        assert_eq!(
            vec![
                Token::Punctuation {
                    raw: '(',
                    kind: PunctuationKind::Open(0)
                },
                Token::Punctuation {
                    raw: '(',
                    kind: PunctuationKind::Open(1)
                },
                Token::Punctuation {
                    raw: ')',
                    kind: PunctuationKind::Close(1)
                },
                Token::Punctuation {
                    raw: ')',
                    kind: PunctuationKind::Close(0)
                },
            ],
            v
        )
    }
}
